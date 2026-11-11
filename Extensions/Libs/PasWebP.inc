{
  PasWebP - pure Pascal WebP codec
  Compiler settings shared by all PasWebP units.

  This include is independent of Imaging's ImagingOptions.inc, the units
  use nothing of Imaging.

  Supported compilers: Free Pascal 3.2.2+ and Delphi 2009+.
  Delphi 7 is not supported (POINTERMATH is used).
  Little-endian CPUs only (as Vampyre Imaging Library).
}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$INLINE ON}
  // Byte order of pixels and of words read from files is assumed
  {$IFDEF ENDIAN_BIG}
    {$FATAL PasWebP supports little-endian CPUs only}
  {$ENDIF}
{$ELSE}
  {$IF CompilerVersion < 20.0}
    {$MESSAGE FATAL 'PasWebP needs Delphi 2009 or newer'}
  {$IFEND}
  {$IF CompilerVersion >= 25.0}
    // Allow closing $IF with $IFEND in XE4+ as in older versions
    {$LEGACYIFEND ON}
  {$IFEND}
{$ENDIF}

{$LONGSTRINGS ON}
{$BOOLEVAL OFF}
// Typed pointers can be indexed like arrays: P[I]. All units rely on it.
{$POINTERMATH ON}
// Hashes, bit windows and arithmetic of pixel channels wrap around on
// purpose: the units do not work with range and overflow checks.
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
