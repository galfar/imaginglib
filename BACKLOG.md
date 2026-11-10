# Backlog

Problems noticed during other work and not fixed there.
Format: file/unit, description, severity, date.

| File/unit | Description | Severity | Date |
|---|---|---|---|
| Source/Imaging.pas (`TImageFileFormat.SaveToMemory`) | Returns True with silently truncated data when the buffer is too small: `MemoryWrite` writes what fits and savers do not check write counts. | medium | 2026-11-10 |
| Source/Imaging.pas (`RotateImage`, `Rotate45`) | Shearing blends pixels byte by byte, so formats with 16-bit channels (Gray16, A16Gray16, Gray32) get slightly wrong antialiased edge pixels. | low | 2026-11-10 |
| Source/Imaging.pas (`RotateImageMul90`) | Does not handle special (compressed) data formats, pixels are copied with zero size; `RotateImage` converts them first. | low | 2026-11-10 |
| Source/ImagingUtility.pas (`NextPow2`) | Never ends for inputs above 2^30 (the shifted value overflows to zero). | low | 2026-11-10 |
| Source/ImagingUtility.pas (`FillMemoryUInt32`) | For sizes not divisible by 4 the third byte of the rest gets byte 0 of the value instead of byte 2 (Pascal and assembler version). | low | 2026-11-10 |
| Source/ImagingIO.pas (`TBufferedStream.Write`) | Writing after a seek past the end of file puts uninitialized bytes of the buffer into the gap. | low | 2026-11-10 |
| Source/ImagingIO.pas (`TReadMemoryStream.CreateFromIOHandle`) | Allocated memory is never freed, size is the size of the whole input even when reading starts in the middle. Unused in the library. | low | 2026-11-10 |
| Source/ImagingIO.pas (`ReadLine`) | With `FailOnControlChars` the line delimiter itself is a control character, so the call can never succeed. Unused in the library. | low | 2026-11-10 |
| Source/ImagingFormats.pas (`ReduceColorsMedianCut`) | Histogram and color boxes are global variables: color reduction is not thread safe (like the global IO functions and options). | low | 2026-11-10 |
| Source/ImagingFormats.pas (`PixelFormatMask`) | `GetBitCount` shifts by 32 for masks with the top bit set; result depends on the compiler/CPU. Unused in the library. | low | 2026-11-10 |
| Source/ImagingNetworkGraphics.pas (`LoadImageFromPNGFrame`) | `CheckBinaryPalette` looks at the palette of the last frame of the file, not of the frame being decoded (matters for MNG with frames of different palettes). | low | 2026-11-10 |
| Source/ImagingNetworkGraphics.pas (JNG alpha) | Alpha image is leaked when its conversion or resizing raises. | low | 2026-11-10 |
| Source/ImagingPortableMaps.pas | PFM rows are read and written top to bottom (specification says bottom to top, kept as Photoshop does it); PAM header fields must be in the usual order; pixels of binary RGB files are written one `Write` call per pixel (slow with unbuffered streams). | low | 2026-11-10 |
| Source/ImagingTarga.pas | `ColorMapOff` (index of the first palette entry) is ignored. | low | 2026-11-10 |
| Source/ImagingBitmap.pas | 32-bit `BI_BITFIELDS` images with masks other than A8R8G8B8 are loaded as A8R8G8B8. | low | 2026-11-10 |
| Source/ImagingCanvases.pas | `AdjustColorLevels` divides by zero for `MidPoint = 0` or `WhitePoint = BlackPoint`, `GammaCorrection` for zero gamma; `FillChannel` does not check `ChannelId`. | low | 2026-11-10 |
| Source/Libs/ImPasJpeg.pas | Inverse DCT raises range check errors on corrupted coefficient data in builds with range checks (wrapping arithmetic of the C original); harmless without them. | low | 2026-11-10 |
| Extensions/ElderImageryImg.pas | `LoadData` passes unchecked dimensions to `NewImage` (assertion in debug builds) for arbitrary data picked by content detection. Extensions loaders were not reviewed or fuzzed. | medium | 2026-11-10 |
