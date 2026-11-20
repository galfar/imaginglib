# ImagingCanvases performance plan

## Context

`Source/ImagingCanvases.pas` has two classes:

- `TImagingCanvas` – works for every non-special data format, but nearly every operation goes
  pixel by pixel through `FFormatInfo.GetPixelFP` / `SetPixelFP` function pointers (a 16-byte
  float record per pixel, each direction), plus a second indirect call (`TPixelWriteProc`) for blending.
- `TFastARGB32Canvas` – overrides only `GetPixel32`, `SetPixel32`, `DrawAlpha`, `StretchDrawAlpha`
  (nearest + bilinear only), `InvertColors`. Everything else (bicubic/Lanczos stretch, blend, add,
  convolution, filters, point transforms, flood fill, lines…) falls back to the slow float path.

Goal: make the generic canvas substantially faster through one architectural change (row/span
processing instead of per-pixel callbacks), let the ARGB32 canvas reuse the same kernels with zero
copies, and add an optimized fixed-point separable resampler (Lanczos, bicubic, proper bilinear
minification) for `TFastARGB32Canvas.StretchDrawAlpha`.

This file is a hand-off: it is meant to be executed phase by phase in a later session by a
different model. Each phase is independently shippable (tests green after each one).

## Ground rules for the implementer

- Pascal must compile with **FPC and Delphi** (old Delphi too): no generics, no inline `var`,
  no anonymous methods, no advanced-record methods. Follow the unit's style
  (`{$IFDEF USE_INLINE}inline;{$ENDIF}`, `PColor32RecArray`-style typed pointers, `PByte` + `Inc`).
- **No assembler / SIMD.** Pure Pascal only (library targets ARM, WASM, etc.).
- Tests are built with `-CroiR` (range + overflow checks). Fixed-point code must not rely on
  wraparound. **`shr` on a negative Integer is a logical shift in Pascal** – never shift a
  possibly-negative accumulator; add the rounding bias and use `div`, or clamp/offset first.
- Public API stays source compatible. New virtual methods / protected helpers are fine.
  `TPixelWriteProc`, `TPointTransformFunction`, `TSelectPixelFunction` remain supported.
- Results may differ from today by at most ±1 per 8-bit channel (rounding), never more; operations
  that are exact today (`bfOne/bfZero` copy, flat-colour resample, convolution test values) stay exact.
- FPC is **not installed** in the cloud container by default; install it (`apt-get install -y fpc`)
  before starting. Build/run: `cd Scripts && ./BuildTestsFPC.sh && ../Bin/ImagingTests`.

## Findings (current hot spots, `Source/ImagingCanvases.pas`)

| Area | Lines | Problem |
|---|---|---|
| `DrawInternal` | 1341–1376 | per pixel: indirect `GetPixelFP` + indirect `PixelWriteProc` → which calls `GetPixelFP`+`SetPixelFP` on dest |
| `PixelBlendProc` | 574–613 | two `case` on blend factors evaluated per pixel; builds 2 temp float colours |
| `StretchDrawInternal` | 1396–1507 | vertical pass calls `GetPixelFP` once per *tap* (source pixel converted to float K times per dest row), column-major access; same code duplicated in `ImagingFormats.StretchResample` (1869–1979) |
| `ApplyConvolution` | 1528–1598 | `CloneImage` of the whole image even for a small clip rect; per tap: 2×`ClampInt`, multiply for address, `GetPixelFP`; zero kernel entries not skipped |
| `ApplyNonLinearFilter` | 1610–1662 | same clone + per-tap conversion; `MedianSelect` quicksorts 16-byte records recomputing luminance in every compare |
| `PointTransform` + users | 1679–1738 | get/transform/set through 3 indirect calls per pixel; `Power()` per channel per pixel for gamma/levels |
| `FloodFill` | 1249–1339 | walks **columns** (cache hostile), every compare is a virtual `GetPixel32` → `Imaging.GetPixel32` → `GetPixel32Generic` |
| `Line` | 1024–1123 | no real clipping – per-pixel clip test + `GetPixelPointer` multiply per pixel |
| `FillRectBlend` | 1150–1170 | constant source colour still goes through `PixelBlendProc` per pixel |
| `FillChannel` | 1740–1760 | converts whole pixel through 8-bit ARGB (also **lossy** for 16-bit/float formats) |
| `GetPixel32/FP`, `SetPixel32/FP` | 823–854 | use `Imaging.GetPixel*` (→ `*Generic`) instead of the faster `FFormatInfo.GetPixel*` pointers |
| `TFastARGB32Canvas.AlphaBlendPixels` | 1797–1812 | a `div` per pixel, no fast path for A=0 / A=255, inexact (opaque 255 over transparent gives 254 – tests carry `Tolerance = 2` because of it) |
| `TFastARGB32Canvas.StretchDrawAlpha` | 1860–2003 | bicubic/Lanczos → float fallback; bilinear uses only 2×2 taps so **minification aliases** (base class uses a proper widened triangle filter); Int64 position math + 4 weights recomputed per pixel |

Existing helpers to reuse (do not reinvent):

- `ImagingFormats.BuildMappingTable` (1720) / `FindExtremes` (1841) – normalized float weights,
  honours `DrawnRect`/clamp ranges from `ImagingUtility.ClipStretchRects` (decl. 390). Keep it as the
  single source of filter geometry so clipped and unclipped draws stay identical.
- `SamplingFilterFunctions` / `SamplingFilterRadii`, `DefaultCubicFilter` (ImagingFormats).
- `ImagingFormats.ConvertToPixel32` (2114) – has the per-format fast cases to hoist into row readers.
- `GetPixel32ifA8R8G8B8`, `*Channel8Bit`, `*Float32` accessors (ImagingFormats 4455–4605).
- `ImagingUtility.ClipCopyBounds`, `ClipStretchRects`, `ClampToByte`, `FillMemoryByte/Word/UInt32`.
- `Imaging.FillRect` (already fast; used by `Clear`, `FillRect`, `HorzLine`, `VertLine`).
- Test helpers: `Tests/UnitTests/ImagingTestPixels.pas` (`FillTestImage`), `TestImagingCanvases.pas`.

Note in `ImagingFormats.pas` history (0.26.5): an earlier per-format fast path in `StretchResample`
was removed as "quite buggy". The new one must land together with parity tests (Phase 0).

## Architecture: rows and spans in a "working format"

Replace *per-pixel function pointers* with *per-row conversion + tight span kernels*.

1. **Working formats** (decided once per operation, not per pixel):
   - `wf32` – row of `TColor32Rec`. Used when every involved image has ≤ 8 bits per channel and is
     not floating point (Gray8, A8Gray8, R8G8B8, X8R8G8B8, A8R8G8B8, packed 16-bit formats,
     Index8 as a *source*).
   - `wfFP` – row of `TColorFPRec`. Used for 16-bit-per-channel, float, half-float and everything else.
2. **Row readers/writers** (new, in `ImagingFormats.pas`, interface section):
   ```pascal
   procedure ReadRow32(Src: PByte; Dst: PColor32Rec; Count: Integer; Info: PImageFormatInfo; Pal: PPalette32);
   procedure WriteRow32(Src: PColor32Rec; Dst: PByte; Count: Integer; Info: PImageFormatInfo; Pal: PPalette32);
   procedure ReadRowFP (Src: PByte; Dst: PColorFPRec; Count: Integer; Info: PImageFormatInfo; Pal: PPalette32);
   procedure WriteRowFP(Src: PColorFPRec; Dst: PByte; Count: Integer; Info: PImageFormatInfo; Pal: PPalette32);
   ```
   One `case Info.Format` **outside** the loop with tight loops for: A8R8G8B8 (`Move`), X8R8G8B8,
   R8G8B8, Gray8, A8Gray8, Index8 (read only), A16R16G16B16/R16G16B16 (+ RB-swapped), Gray16,
   A32R32G32B32F (`Move`)/R32G32B32F (+ swapped). `else` branch: loop calling
   `Info.GetPixel32/GetPixelFP/SetPixel32/SetPixelFP` – guarantees every format still works.
   Each fast case must produce **bit-identical** values to the corresponding `Info.Get/SetPixel*`
   (unit-tested against it for all formats, see Phase 0).
3. **Span kernels** (implementation section of `ImagingCanvases.pas`, plain procedures):
   - `BlendSpan32(Src, Dst: PColor32Rec; Count; SrcF, DstF)` / `BlendSpanFP(...)` – factor `case`
     hoisted out of the loop; dedicated loops for alpha-over, add, `bfOne/bfZero` (copy),
     `bfSrcAlpha/bfOneMinusSrcAlpha`; generic loop for remaining combos.
   - `AlphaSpan32`, `AddSpan32`, `AlphaSpanFP`, `AddSpanFP`, and constant-source variants for
     `FillRectBlend`.
   - Integer alpha-over must be exact: skip when `Src.A = 0`, plain store when `Src.A = 255`,
     dest-opaque path `(S*a + D*(255-a) + 127) div 255` (use the `(x + 128 + (x+128) shr 8) shr 8`
     trick on non-negative x), general path via a 256-entry reciprocal table for the final alpha.
     Formula to match: `PixelAlphaProc` (615–634).
4. **Canvas plumbing** (`TImagingCanvas`):
   - `UpdateCanvasState` computes `FWorkFormat` (`wf32`/`wfFP`) and `FNativeIs32: Boolean`
     (format = ifA8R8G8B8 → rows can be used in place, no read/write step).
   - Protected helpers: `BeginRow32(Y, X, Count): PColor32Rec` (returns pointer into image when
     `FNativeIs32`, else reads into a reusable `FRowBuf`) and `EndRow32(...)` (no-op or
     `WriteRow32`), same pair for FP. All refactored operations use these.
   - Operation working format = `wf32` only if **both** source and dest canvases are `wf32`.
   - `TPixelWriteProc` stays as the fallback path for third-party callers of
     `DrawInternal/StretchDrawInternal` passing a custom proc; the three built-in procs are detected
     (`@PixelWriteProc = @PixelAlphaProc` etc.) and routed to span kernels.

Consequence: `TFastARGB32Canvas` needs few overrides – it is simply the zero-copy case of the base
class. Overrides remain only where the algorithm itself differs.

5. **Shared separable resampler** (new, `ImagingFormats.pas`), used by both canvases *and* by
   `StretchResample` (so `Imaging.StretchRect` / `ResizeImage` / mipmaps speed up too):
   ```pascal
   type
     TRowResampler = class
       constructor Create(const Src: TImageData; const SrcRect, DstRect, DrawnRect, SrcValidRect: TRect;
         Filter: TFilterFunction; Radius: Single; WrapEdges: Boolean; Use32: Boolean);
       // Returns DrawnRect.Width resampled pixels of destination row DrawnRect.Top + J,
       // PColor32Rec (Use32) or PColorFPRec. Rows must be requested in increasing J.
       function GetRow(J: Integer): Pointer;
     end;
   ```
   Details in Phase 3.

## Phases

### Phase 0 – safety net (tests + benchmark) before touching behaviour

Files: `Tests/UnitTests/TestImagingCanvases.pas`, `Tests/UnitTests/TestImagingFormats.pas`,
`Demos/ObjectPascal/Benchmark/DemoUnit.pas`.

1. Add a **reference-parity** helper to the canvas tests: run an operation on a
   `FillTestImage`-filled image with the current code, store result, compare against expected
   values computed independently in the test with plain float math via `GetPixelFP`
   (not by calling the canvas). Cover per format group `ifGray8, ifA8Gray8, ifR8G8B8, ifX8R8G8B8,
   ifA8R8G8B8, ifR5G6B5, ifA16R16G16B16, ifA32R32G32B32F, ifIndex8 (as source)`:
   `DrawAlpha`, `DrawAdd`, `DrawBlend` (several factor pairs), `FillRectBlend`,
   `StretchDrawAlpha/Add/Blend` × 4 filters × up/down scale, `ApplyConvolution3x3/5x5`
   (several kernels incl. bias), min/max/median, each point transform, `FillChannel(FP)`,
   `FloodFill` (both modes, with clip rect), `Line` (all octants, clipped, pen width 1 and 3),
   `Ellipse`. Tolerance: 0 where exact today, else ±1 of 255 (or 1e-5 for float formats).
2. Add fast-vs-generic parity test: same op through `TFastARGB32Canvas` and through
   `TImagingCanvas.CreateForData` on an identical ifA8R8G8B8 image, ±1.
3. Benchmark: add a "Canvas" section to the Benchmark demo timing each of the above on
   e.g. 2000×2000 for ifA8R8G8B8, ifR8G8B8, ifA16R16G16B16; record baseline numbers in the PR text.

### Phase 1 – row readers/writers + cheap wins (no semantic change)

1. Implement `ReadRow32/WriteRow32/ReadRowFP/WriteRowFP` (ImagingFormats). Test: for every regular
   format compare against per-pixel `Info.Get/SetPixel*` on `FillTestImage` data (bit-exact).
2. `TImagingCanvas.GetPixel32/GetPixelFP/SetPixel32/SetPixelFP`: compute pointer with
   `GetPixelPointer` and call `FFormatInfo.GetPixel32(...)` etc. directly.
3. Add `FWorkFormat`, `FNativeIs32`, row buffer + `BeginRow*/EndRow*` helpers.
4. `PointTransform`: row based (`ReadRowFP` → loop calling `Transform` → `WriteRowFP`).
5. `FillChannel/FillChannelFP`: for formats with plain per-channel layout (8-bit, 16-bit, float32
   channel formats; respect `IsRBSwapped`, no-op when the channel does not exist, gray formats keep
   the old path) write the channel bytes directly; otherwise old path. Fixes 16-bit precision loss.

### Phase 2 – blending and drawing on spans

1. Span kernels from Architecture §3 (+ unit tests against `PixelBlendProc/AlphaProc/AddProc`).
2. `DrawInternal`: clip once (`ClipCopyBounds`), per row `BeginRow`(src) → blend into dest row →
   `EndRow`(dest). `bfOne/bfZero` skips reading dest. Keep generic-proc fallback.
3. `FillRectBlend`: translate fill colour once, constant-source span kernel.
4. `TFastARGB32Canvas.DrawAlpha`: body becomes a call to the shared `AlphaSpan32` on scanlines
   (keep the override only if it still saves anything; otherwise delete it and `AlphaBlendPixels`).
   Replace `DestCanvas.ClassType <> Self.ClassType` checks by the base-class `FNativeIs32` logic.
5. Now that blending is exact, lower `Tolerance` in `TestStretchDrawAlphaLongSource` to 1 and drop
   the "254 of opaque 255" workaround comment/setup in `TestStretchDrawFlat` if it passes without.

### Phase 3 – resampler (the Lanczos work)

Implement `TRowResampler` in `ImagingFormats.pas`:

1. **Tables**: call `BuildMappingTable` for X and Y exactly as `StretchDrawInternal` does today
   (lines 1438–1443). Convert each cluster to a dense run: `Start = min Pos`, `Count = max Pos -
   Start + 1`, weights summed per position (clamping at edges creates duplicate positions; zero
   weights are skipped by the builder, so fill gaps with 0). Store flat:
   `Starts[]`, `Counts[]`, `Weights[]` (stride = max count).
2. **Fixed point (Use32)**: weights → Integer with 14 fractional bits (`Round(w * 16384)`), then add
   the residual `16384 - Sum` to the largest weight of the run so every run sums to exactly 16384
   (keeps flat areas exact – `TestStretchDrawFlat`). FP mode keeps Single weights.
3. **Horizontal pass, cached per source row**: needed source rows for dest row J are
   `StartsY[J] .. StartsY[J]+CountsY[J]-1`, monotonically non-decreasing in J. Keep a ring of
   `MaxCountY` intermediate rows indexed `SrcRow mod RingSize` with a `CachedRowIndex[]` array;
   compute a row only when missing. A source row is thus filtered once, not once per dest row.
   - Use32: read source pixels `XMinimum..XMaximum` directly (`ifA8R8G8B8`: in place; other wf32
     formats: `ReadRow32` into a temp row). For each drawn X accumulate 4 channels
     `Acc += Chan * W` (Int32; 255·16384·ΣǀWǀ ≪ 2³¹), store intermediate as Int32 with 6 fractional
     bits: `(Acc + 128) div 256` – **not clamped** (keeps Lanczos overshoot for the second pass).
   - FP: `ReadRowFP` once per source row, Single accumulators.
4. **Vertical pass** for dest row J: zero an Int32 accumulator row (4·Width); loop taps outer, pixels
   inner (`Acc[i] += Ring[tap][i] * WY[tap]`, sequential memory); finalize
   `ClampToByte((Acc + 1 shl 19) div (1 shl 20))` into the output `TColor32Rec` row. Handle negative
   `Acc` without `shr` (see ground rules). Overflow bound: ǀintermediateǀ ≤ ~255·64·1.5,
   ·16384·1.5 ≈ 6·10⁸ < 2³¹ – assert `ΣǀWǀ` < 2.5 per run when building tables, else use FP mode.
5. Special cases: `SrcWidth = 1`/single-tap runs fall out naturally; `Count = 1` rows with weight
   16384 may short-circuit to a copy.
6. Rewrite `ImagingFormats.StretchResample` (1869) on top of `TRowResampler` (`Use32` when the
   format is wf32-capable, then `WriteRow32`; else FP + `WriteRowFP`). Existing resize tests in
   `TestImagingManipulation.pas` must stay green (±1 where they compare exact values – check and
   adjust only with justification).
7. `TImagingCanvas.StretchDrawInternal`: `ClipStretchRects` → `TRowResampler` → per dest row blend
   the returned row with the span kernel (Phase 2). Removes the duplicated resample loop.
8. `TFastARGB32Canvas.StretchDrawAlpha`:
   - `rfBicubic`, `rfLanczos`: resampler (`Use32`, source rows read in place via `FScanlines`) +
     `AlphaSpan32`. No more float fallback.
   - `rfBilinear`: keep the 2×2 fast loop for magnification and scale ≥ 0.5; for stronger
     minification (either axis) use the resampler with `sfLinear` so it no longer aliases and
     matches the base class. Speed up the 2×2 loop by precomputing per-X `(T1, T2, FracX)` tables
     once per call instead of Int64 math per pixel per row.
   - `rfNearest`: precompute per-X source index table once per call.
   - Destination of another class: base implementation (already span based after step 7).
9. Tests: extend `TestStretchDrawClipped` and `TestStretchDrawFlat` filter lists with `rfLanczos`
   (and rfBicubic for flat on the fast canvas), add fast-vs-generic parity (±1) for Lanczos/bicubic
   up- and down-scaling incl. alpha gradients, add a 40000-px-wide source case for Lanczos
   (position math must not overflow 32 bits – `BuildMappingTable` uses Single for `Center`; verify
   precision for sources > 2²⁴/… pixels is no worse than today).

**Decision kept as today:** filtering is done on straight (non-premultiplied) ARGB, like the
existing generic and bilinear paths. Premultiplied-weighted filtering (avoids colour bleeding from
fully transparent pixels under Lanczos ringing) is a possible follow-up, not part of this plan.

### Phase 4 – filters and point transforms

1. `ApplyConvolution`: no `CloneImage`. Ring of `KernelSize` source rows in working format, each
   row copied from the clip-rect span and **edge-padded by `KernelSize div 2` replicated pixels**
   on both sides (same clamp-to-clip-rect semantics as lines 1561/1565, but no per-tap clamp).
   Rows above/below the clip rect reuse the first/last row pointer. Precompute list of non-zero
   kernel taps. wf32: Int32 accumulation of `Chan * KernelValue` for R,G,B, then
   `Round(Acc * DivFloat + Bias * 255)` + clamp when `ClampChannels` (if not clamping on wf32 the
   byte store clamps anyway – keep identical to today's `SetPixelFP` behaviour); use Int64/FP path
   when `ΣǀKǀ·255` could overflow. Alpha untouched (as today). Output written straight to the image
   because the ring holds the originals.
2. `ApplyNonLinearFilter`: same ring; keep `TSelectPixelFunction` API for custom functions
   (fill `NeighPixels` from the FP ring). Built-in min/max/median get dedicated kernels: precompute
   a luminance key per pixel per ring row (Integer for wf32, weights matching `GrayConv` ordering),
   min/max = scan keys, median = selection on a small key+index array (insertion sort for ≤ 25,
   quickselect above). Ties must resolve like today or tests must prove pixel values equal.
3. Built-in point transforms on wf32 non-gray formats: build three 256-entry byte LUTs by running
   the existing `Transform*` function + the same rounding as `SetPixelFP` on `i/255`, apply in a
   tight loop (in place when `FNativeIs32`). Applies to contrast/brightness, gamma, invert,
   threshold, levels. Gray formats: LUT only when R/G/B parameters are equal, else row-FP path.
   16-bit channel formats: optional 65536-entry Word LUT. `PremultiplyAlpha` /
   `UnPremultiplyAlpha`: integer span kernels on wf32 (must equal rounding of the float formula).
   Implement by making `PointTransform` accept an optional internal "LUT builder" path; the public
   signature is unchanged. `TFastARGB32Canvas.InvertColors` → `Color xor $00FFFFFF` per pixel.

### Phase 5 – primitives

1. `FloodFill`: horizontal span scanline fill (seed stack of spans), fills with
   `HorzLineInternal`. Comparison stays "equal as 32-bit ARGB" (current semantics): read the row
   through a small inline accessor – `FFormatInfo.GetPixel32` in the base class, direct `UInt32`
   compare via `FScanlines` in `TFastARGB32Canvas` (make the compare/fill-span steps protected
   virtuals, or override `FloodFill`). Must honour `FClipRect` and both modes.
2. `Line`: real clipping to `FClipRect` (Cohen–Sutherland or Liang–Barsky, integer, keeping the
   exact Bresenham pixel sequence – clip by advancing the error term, do not re-derive the slope),
   then an unchecked loop stepping a pointer by `±Bpp` / `±Width*Bpp` with `CopyPixel` specialised
   for Bpp 1/2/3/4. Fix the off-by-one in the outcodes (`> Right`/`> Bottom` → `>=`).
3. `Ellipse`: outline pixels via pointer/`CopyPixelInternal` unchanged; only verify with tests.
4. `HorzLineInternal`: already fine (fill helpers); nothing to do.

### Phase 6 – wrap-up

- Update unit header comment of `TImagingCanvas` (no longer "always works in ifA32R32G32B32F") and
  the "File Notes" changelog blocks in `ImagingCanvases.pas` and `ImagingFormats.pas`.
- Re-run the benchmark, put before/after table in the commit/PR description.
- Check demos that use canvases still build: `Demos/ObjectPascal/ImageBrowser`, `LCLImager`,
  `Extras/Demos/ClippingTest` (`Scripts/BuildDemosFPC.sh`), and `Source/ImagingNetworkGraphics.pas`
  (uses canvases for MNG/APNG frame composition – run `TestImagingFileFormats`).

## Suggested order / commits

Work on branch **`canvas-opt`** (create it from the current default branch:
`git checkout -b canvas-opt`, push with `git push -u origin canvas-opt`).
One commit per phase on that branch: 0 tests+bench → 1 rows →
2 spans → 3 resampler → 4 filters → 5 primitives → 6 docs. Phases 4 and 5 are independent of 3.
If time is limited, 0→1→2→3 delivers the Lanczos goal and most of the generic speedup.

## Verification

```bash
apt-get install -y fpc            # once per container
cd Scripts && ./BuildTestsFPC.sh  # builds Bin/ImagingTests with range/overflow checks + heaptrc
cd ../Bin && ./ImagingTests       # must be all green, no heaptrc leak report
```

- After every phase: full unit test run (not only canvas tests – `StretchResample` is shared with
  resize, mipmaps, file-format savers).
- Phase 0 parity tests are the acceptance criteria for phases 1–5 (tolerances stated there).
- Visual check for Phase 3: stretch `Demos/Data/*` test images with Lanczos through both canvases,
  save PNGs to the scratchpad, diff them (max channel difference ≤ 1) and look at edges/ringing.
- Benchmark demo: `Scripts/BuildDemosFPC.sh`, run `Bin/Bench`; expect order-of-magnitude gains
  for 8-bit formats on blend/stretch/convolution/point transforms, ~2–4× for FP formats.
- Delphi cannot be compiled here: keep to constructs already used in these units and say so in
  the hand-back summary so the maintainer runs `Scripts/BuildTestsDelphi.bat`.
