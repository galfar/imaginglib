{ -------------------------------------------------------------------- }

{$DEFINE MAX_MATCH_IS_258}

{ Compile with -DMAXSEG_64K if the alloc function cannot allocate more
  than 64k bytes at a time (needed on systems with 16-bit int). }

{$UNDEF MAXSEG_64K}
{$DEFINE UNALIGNED_OK}    { requires SizeOf(ush) = 2 ! }
{$UNDEF DYNAMIC_CRC_TABLE}
{$UNDEF FASTEST}
{$DEFINE Use32}
{$DEFINE patch112}        { apply patch from the zlib home page }

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

// Undef DEBUG mode for ImPasZLib. DEBUG is off by default, since it's ~5x slower
// Comment this out if you want to debug ZLib.
{$UNDEF DEBUG}

{$IFNDEF DEBUG}
  {$RANGECHECKS OFF}
  {$OVERFLOWCHECKS OFF}
{$ENDIF}

{ -------------------------------------------------------------------- }


