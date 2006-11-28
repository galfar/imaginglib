{ $Id$ }

{ Imaging options }

{$DEFINE USE_INLINE}          // use function inlining for some functions
                              // works in Free Pascal and Delphi 9+
{$DEFINE USE_ASM}             // if defined, assembler versions of some
                              // functions will be used (only for x86)
{$DEFINE DEBUG}               // if defined, debug info and range/IO/overflow
                              // checking and stack frames will be turned on

// define component set used in ImagignComponents.pas unit
{$DEFINE COMPONENT_SET_VCL}   // use Borland's VCL
{ $DEFINE COMPONENT_SET_CLX}   // use Borland's CLX (set automatically when using Kylix, must be se manually when using Delphi 6/7)
{ $DEFINE COMPONENT_SET_LCL}   // use Lazarus' LCL (set automatically when using FPC)


{ Compiler options }

{$ALIGN 8}                // Field alignment: 8 B
{$ASSERTIONS ON}          // Assertions: on
{$BOOLEVAL OFF}           // Boolean eval: off
{$EXTENDEDSYNTAX ON}      // Extended syntax: on
{$LONGSTRINGS ON}         // string = AnsiString: on
{$MINENUMSIZE 4}          // Min enum size: 4 B
{$TYPEDADDRESS OFF}       // Typed pointers: off
{$WRITEABLECONST OFF}     // Writeable constants: off

{$IFNDEF FPC}
  {$DEFINE DCC}           // if not using FPC then DCC compiler is used (Delphi/Kylix)
                          // others are not supported
{$ENDIF}

{$IFNDEF FPC}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}       // using Kylix
  {$ENDIF}
{$ENDIF}

{$IFDEF DCC}
  {$IFNDEF KYLIX}
    {$DEFINE DELPHI}      // using Delphi
  {$ENDIF}
{$ENDIF}

{$IFDEF DEBUG}
  {$DEBUGINFO ON}
  {$LOCALSYMBOLS ON}
  {$RANGECHECKS ON}
  {$IOCHECKS ON}
  {$OVERFLOWCHECKS ON}
  {$IFDEF DCC}
    {$OPTIMIZATION OFF}
    {$STACKFRAMES ON}
    { $DEFINE MEMCHECK}
  {$ENDIF}
  {$IFDEF FPC}
    {$S+}
  {$ENDIF}
{$ELSE}
  {$DEBUGINFO OFF}
  {$LOCALSYMBOLS OFF}
  {$RANGECHECKS OFF}
  {$IOCHECKS OFF}
  {$OVERFLOWCHECKS OFF}
  {$IFDEF DCC}
    {$OPTIMIZATION ON}
    {$STACKFRAMES OFF}
  {$ENDIF}
  {$IFDEF FPC}
    {$S-}
  {$ENDIF}
{$ENDIF}

{ Compiler capabilities }

// Define if compiler supports inlining of functions and procedures
// FPC inline support is now disable because it crashes
{$IF (Defined(DCC) and (CompilerVersion >= 17)) }// or Defined(FPC)}
  {$DEFINE HAS_INLINE}
{$IFEND}

// Define if compiler supports advanced records with methods
{$IF (Defined(DCC) and (CompilerVersion >= 18)) }
  {$DEFINE HAS_ADVANCED_RECORDS}
{$IFEND}

// Define if compiler supports operator overloading (unfortunately Delphi and FPC operator overloaing is not compatible)
{$IF (Defined(DCC) and (CompilerVersion >= 18)) or Defined(FPC)}
  {$DEFINE HAS_OPERATOR_OVERLOADING}
{$IFEND}

{ Imaging options check}

{$IFNDEF HAS_INLINE}
  {$UNDEF USE_INLINE}
{$ENDIF}

{$IFDEF FPC}
  {$IFNDEF CPU86}
    {$UNDEF USE_ASM}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE COMPONENT_SET_LCL}
  {$UNDEF COMPONENT_SET_VCL}
  {$UNDEF COMPONENT_SET_CLX}
{$ENDIF}

{$IFDEF KYLIX}
  {$DEFINE COMPONENT_SET_CLX}
  {$UNDEF COMPONENT_SET_VCL}
  {$UNDEF COMPONENT_SET_LCL}
{$ENDIF}

{$IFDEF DELPHI}
  {$UNDEF COMPONENT_SET_LCL}
  {$IF CompilerVersion >= 17}
    {$UNDEF COMPONENT_SET_CLX}     // Delphi 9+ has no CLX
  {$IFEND}
  {$IFNDEF COMPONENT_SET_VCL}
    {$IFNDEF COMPONENT_SET_CLX}
      {$DEFINE COMPONENT_SET_VCL}  // use VCL as default if not set
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF COMPONENT_SET_VCL}
  {$UNDEF COMPONENT_SET_CLX}
  {$UNDEF COMPONENT_SET_LCL}
{$ENDIF}

{$IFDEF COMPONENT_SET_CLX}
  {$UNDEF COMPONENT_SET_VCL}
  {$UNDEF COMPONENT_SET_LCL}
{$ENDIF}

{$IFDEF COMPONENT_SET_LCL}
  {$UNDEF COMPONENT_SET_VCL}
  {$UNDEF COMPONENT_SET_CLX}
{$ENDIF}

{ Platform options }

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF DPMI}
  {$DEFINE MSDOS}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE UNIX}
{$ENDIF}

{ More compiler options }

{$IFDEF FPC}               // Free Pascal options
  {$MODE DELPHI}           // compatible with delphi
  {$GOTO ON}               // alow goto
  {$PACKRECORDS 8}         // same as ALING 8 for Delphi
  {$CALLING REGISTER}      // default calling convention is register
  {$IFDEF CPU86}
    {$IFNDEF DYN_LIBRARY}
      {$SMARTLINK ON}      // smartlinking on, but not for dll/so -
                           // nothing gets exported from library when it is on
                           // in FPC 1.9.8
    {$ENDIF}
    {$ASMMODE INTEL}       // intel assembler mode
  {$ENDIF}
{$ENDIF}

{$IFDEF HAS_INLINE}        
  {$INLINE ON}             // turns inlining on for compilers that support it
{$ENDIF}


{ Extensions linking options }

{$DEFINE LINK_JPEG}        // link support for Jpeg images
{$DEFINE LINK_PNG}         // link support for PNG images
{$DEFINE LINK_TARGA}       // link support for Targa images
{$DEFINE LINK_BITMAP}      // link support for Windows Bitmap images
{$DEFINE LINK_DDS}         // link support for DDS images
{$DEFINE LINK_MNG}         // link support for MNG images
{$DEFINE LINK_JNG}         // link support for JNG images

{$DEFINE LINK_OPENGL}      // link support for OpenGL
{$DEFINE LINK_SDL}         // link support for SDL
{$DEFINE LINK_D3D9}        // link support for D3D9

//{$DEFINE LINK_DRAWING}     // link drawing function
//{$DEFINE LINK_EFFECTS}     // link effect functions


{ Extension dependencies check }

{$IFDEF LINK_MNG}          // MNG uses internaly both PNG and JNG
  {$DEFINE LINK_JNG}
  {$DEFINE LINK_PNG}
{$ENDIF}

{$IFDEF LINK_JNG}          // JNG uses internaly both PNG and JPEG
  {$DEFINE LINK_PNG}
  {$DEFINE LINK_JPEG}
{$ENDIF}


