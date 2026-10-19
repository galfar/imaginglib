{
  Vampyre Imaging Library Demo
  Benchmark (core low level API)

  Simple program which measures time taken by the main Imaging functions
  (loading, manipulation, saving) in microsecond resolution.
  You can use it to compare the speeds of executables created by the supported
  compilers (you can find results for my machine somewhere in Demos directory).

  Results are written to the console and to Results.log file, images created
  in the saving test are written to disk too (time is measured for saving to
  memory). Files go to _BenchOut subdirectory of the executable's directory,
  executables not named Bench (Bench32, Bench-Debug, ...) use their own
  subdirectory in it, so several builds can be run one after another.
  Command line option -nofiles disables writing of the files (images and the log).
}
unit DemoUnit;

{$I ImagingOptions.inc}

interface

procedure RunDemo;

implementation

uses
  SysUtils,
  Classes,
  ImagingTypes,
  Imaging,
  ImagingUtility,
  DemoUtils;

type
  TManipulation = (maResize3k, maResize1k, maFlip, maMirror, maSwapChannels,
    maConvARGB64, maConvARGBF, maConvARGB16, maConvRGB24, maConvARGB32,
    maCompressDXT, maDecompressDXT, maReduceColors, maClone, maMipMaps,
    maCopyRect, maMapImage, maFill, maSplit, maMakePal, maReplace,
    maRotate5, maRotate90, maStretchRect);

  TFileFormatInfo = record
    Name: string;
    Ext: string;
    Masks: string;
    CanSave: Boolean;
    IsMulti: Boolean;
  end;

const
  SDataDir = 'Data';
  SImageName = 'Tigers';
  SOutDir = '_BenchOut';
  SDefaultRunName = 'Bench';
  SLogFileName = 'Results.log';
  SNoFilesOption = '-nofiles';

var
  Time: Int64;
  Img: TImageData;
  WriteFiles: Boolean = True;
  OutDir: string;
  LogFile: TextFile;
  LogFileOpen: Boolean = False;

procedure Log(const S: string);
begin
  WriteLn(S);
  if LogFileOpen then
    WriteLn(LogFile, S);
end;

procedure WriteTimeDiff(const Msg: string; const OldTime: Int64);
var
  Diff: Double;
  S: string;
begin
  Diff := (GetTimeMicroseconds - OldTime) * 1.0;
  S := Format('%-58s %16.0n us', [Msg, Diff], GetFormatSettingsForFloats);
  Log(S);
end;

function GetImageName(const Ext: string): string;
begin
  Result := GetDataDir + PathDelim + SImageName + '.' + Ext;
end;

procedure LoadImage(const Name: string);
var
  Mem: TMemoryStream;
begin
  if FileExists(Name) then
  begin
    Mem := TMemoryStream.Create;
    try
      Log('Loading image: ' + ExtractFileName(Name));
      Mem.LoadFromFile(Name);
      Time := GetTimeMicroseconds;
      // We are loading from memory stream so there is no file system
      // overhead measured.
      Imaging.LoadImageFromStream(Mem, Img);
      WriteTimeDiff('Image loaded in:', Time);
    finally
      Mem.Free;
    end;
  end;
end;

procedure SaveImage(const Ext: string);
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  Log('Saving image to format: ' + Ext);
  try
    Time := GetTimeMicroseconds;
    // We are saving to memory stream so there is no file system
    // overhead measured. But if image is in data format which is not
    // supported by this file format the measured time will include conversion
    // time.
    Imaging.SaveImageToStream(Ext, Mem, Img);
    WriteTimeDiff('Image saved in:', Time);
    if WriteFiles then
      Mem.SaveToFile(OutDir + PathDelim + SImageName + '.' + Ext);
  finally
    Mem.Free;
  end;
end;

const
  FillColor: TColor32Rec = (Color: $FFFF0000);
  NewColor: TColor32Rec = (Color: $FF00CCFF);
var
  ImgClone: TImageData;
  Subs: TDynImageDataArray;
  XCount, YCount: LongInt;
  Pal: PPalette32;
  Formats: array of TFileFormatInfo;

procedure ManipulateImage(Man: TManipulation);
begin
  // According to the enum value image manipulation functions are
  // called and measured.
  case Man of
    maResize3k:
      begin
        Log('Resizing image to 3000x3000 (bilinear) ... ');
        Time := GetTimeMicroseconds;
        Imaging.ResizeImage(Img, 3000, 3000, rfBilinear);
        WriteTimeDiff('Image resized in: ', Time);
      end;
    maResize1k:
      begin
        Log('Resizing image to 1000x1000 (bicubic) ... ');
        Time := GetTimeMicroseconds;
        Imaging.ResizeImage(Img, 1000, 1000, rfBicubic);
        WriteTimeDiff('Image resized in: ', Time);
      end;
    maFlip:
      begin
        Log('Flipping image ... ');
        Time := GetTimeMicroseconds;
        Imaging.FlipImage(Img);
        WriteTimeDiff('Image flipped in: ', Time);
      end;
    maMirror:
      begin
        Log('Mirroring image ... ');
        Time := GetTimeMicroseconds;
        Imaging.MirrorImage(Img);
        WriteTimeDiff('Image mirrored in:', Time);
      end;
    maSwapChannels:
      begin
        Log('Swapping channels of image ... ');
        Time := GetTimeMicroseconds;
        Imaging.SwapChannels(Img, ChannelRed, ChannelGreen);
        WriteTimeDiff('Channels swapped in: ', Time);
      end;
    maConvARGB64:
      begin
        Log('Converting image to A16R16G16B16 64bit format ... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA16R16G16B16);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maConvARGBF:
      begin
        Log('Converting image to A32B32G32R32F 128bit FP format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA32B32G32R32F);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maConvARGB16:
      begin
        Log('Converting image to A4R4G4B4 16bit format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA4R4G4B4);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maConvRGB24:
      begin
        Log('Converting image to R8G8B8 24bit format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifR8G8B8);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maConvARGB32:
      begin
        Log('Converting image to A8R8G8B8 32bit format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA8R8G8B8);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maCompressDXT:
      begin
        Log('Compressing image to DXT1 format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifDXT1);
        WriteTimeDiff('Image compressed in: ', Time);
      end;
    maDecompressDXT:
      begin
        Log('Decompressing image from DXT1 format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA8R8G8B8);
        WriteTimeDiff('Image decompressed in: ', Time);
      end;
    maReduceColors:
      begin
        Log('Reducing colors count to 1024... ');
        Time := GetTimeMicroseconds;
        Imaging.ReduceColors(Img, 1024);
        WriteTimeDiff('Colors reduced in: ', Time);
      end;
    maMipMaps:
      begin
        Log('Creating mipmaps ... ');
        SetLength(Subs, 0);
        Time := GetTimeMicroseconds;
        Imaging.GenerateMipMaps(Img, 0, Subs);
        WriteTimeDiff('Mipmaps created in: ', Time);
        Imaging.FreeImagesInArray(Subs);
      end;
    maClone:
      begin
        Log('Cloning image ... ');
        Imaging.InitImage(ImgClone);
        Time := GetTimeMicroseconds;
        Imaging.CloneImage(Img, ImgClone);
        WriteTimeDiff('Image cloned in: ', Time);
      end;
    maCopyRect:
      begin
        Log('Copying rectangle ... ');
        Time := GetTimeMicroseconds;
        Imaging.CopyRect(ImgClone, 0, 1500, 1500, 1500, Img, 0, 0);
        WriteTimeDiff('Rectangle copied in: ', Time);
      end;
    maStretchRect:
      begin
        Log('Stretching rectangle (bicubic) ... ');
        Time := GetTimeMicroseconds;
        Imaging.StretchRect(ImgClone, 0, 1500, 1500, 1500, Img, 500, 500, 2000, 2000, rfBicubic);
        WriteTimeDiff('Rectangle stretched in: ', Time);
        Imaging.FreeImage(ImgClone);
      end;
    maMapImage:
      begin
        Log('Mapping image to existing palette ... ');
        Time := GetTimeMicroseconds;
        Imaging.MapImageToPalette(Img, Pal, 256);
        WriteTimeDiff('Image mapped in: ', Time);
        Imaging.FreePalette(Pal);
      end;
    maFill:
      begin
        Log('Filling rectangle ... ');
        Time := GetTimeMicroseconds;
        Imaging.FillRect(Img, 1500, 0, 1500, 1500, @FillColor);
        WriteTimeDiff('Rectangle filled in: ', Time);
      end;
    maReplace:
      begin
        Log('Replacing colors in rectangle ... ');
        Time := GetTimeMicroseconds;
        Imaging.ReplaceColor(Img, 0, 0, Img.Width, Img.Height, @FillColor, @NewColor);
        WriteTimeDiff('Colors replaced in: ', Time);
      end;
    maSplit:
      begin
        Log('Splitting image ... ');
        SetLength(Subs, 0);
        Time := GetTimeMicroseconds;
        Imaging.SplitImage(Img, Subs, 300, 300, XCount, YCount, True, @FillColor);
        WriteTimeDiff('Image split in: ', Time);
        Imaging.FreeImagesInArray(Subs);
      end;
    maMakePal:
      begin
        Log('Making palette for images ... ');
        Imaging.NewPalette(256, Pal);
        SetLength(Subs, 1);
        Subs[0] := Img;
        Time := GetTimeMicroseconds;
        Imaging.MakePaletteForImages(Subs, Pal, 256, False);
        WriteTimeDiff('Palette made in: ', Time);
        Img := Subs[0];
      end;
    maRotate5:
      begin
        Log('Rotating image 5 degrees CCW ... ');
        Time := GetTimeMicroseconds;
        Imaging.RotateImage(Img, 5);
        WriteTimeDiff('Image rotated in: ', Time);
      end;
    maRotate90:
      begin
        Log('Rotating image 90 degrees CCW ... ');
        Time := GetTimeMicroseconds;
        Imaging.RotateImage(Img, 90);
        WriteTimeDiff('Image rotated in: ', Time);
      end;
  end;
end;

function ParseOptions: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to ParamCount do
  begin
    if SameText(ParamStr(I), SNoFilesOption) then
      WriteFiles := False
    else
    begin
      WriteLn('Unknown option: ' + ParamStr(I));
      WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' [' + SNoFilesOption + ']');
      Result := False;
    end;
  end;
end;

function GetOutDir: string;
var
  RunName: string;
begin
  // Default executable writes directly to _BenchOut, other builds
  // (Bench32, Bench-Debug, ...) to their own subdirectory so that they
  // don't overwrite results of each other (CI runs multiple benches).
  RunName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  Result := GetAppDir + PathDelim + SOutDir;
  if not SameText(RunName, SDefaultRunName) then
    Result := Result + PathDelim + RunName;
end;

function OpenLogFile: Boolean;
begin
  Result := False;
  try
    if not ForceDirectories(OutDir) then
      raise Exception.Create('Cannot create directory ' + OutDir);
    AssignFile(LogFile, OutDir + PathDelim + SLogFileName);
    Rewrite(LogFile);
    LogFileOpen := True;
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Exception raised during opening log file for writing: ' +
        OutDir + PathDelim + SLogFileName);
      WriteLn(E.Message);
    end;
  end;
end;

procedure WaitForReturnInDebug;
begin
{$IFDEF DEBUG}
  // Can be skipped by "./Bench < /dev/null" for e.g. CI runs
  WriteLn;
  WriteLn('Press RETURN key to exit');
  ReadLn;
{$ENDIF}
end;

procedure RunDemo;
var
  I: LongInt;
  Failed: Boolean;
  Title, Libs: string;
begin
  Title := 'Bench Demo - Vampyre Imaging Library v' + Imaging.GetVersionStr +
    ' (' + GetBuildInfo + ')';
  Libs := 'Libraries: ' + GetLibsInfo;
  WriteLn(Title);
  WriteLn(Libs);
  WriteLn;

  if not ParseOptions then
  begin
    ExitCode := 1;
    Exit;
  end;

  if not DirectoryExists(GetDataDir) then
  begin
    // If required testing data is not found program ends.
    WriteLn('Error!' + sLineBreak + '"Data" directory with ' +
      'required "Tigers.*" images not found.');
    WaitForReturnInDebug;
    ExitCode := 1;
    Exit;
  end;

  if WriteFiles then
  begin
    // Results are written to the console and also to the log file.
    OutDir := GetOutDir;
    if not OpenLogFile then
    begin
      ExitCode := 1;
      Exit;
    end;
    WriteLn(LogFile, Title);
    WriteLn(LogFile, Libs);
    WriteLn(LogFile);
  end;

  Failed := False;
  // Call this before any manipulation with TImageData record.
  Imaging.InitImage(Img);
  try
    try
      I := 0;
      SetLength(Formats, 1);
      // Enumerate all supported file formats and store their properties
      // to dyn array. After each iteration dyn array's size is increased by one
      // so next call to EnumFileFormats will have free space for results.
      // After enumerating last array item should be deleted because its empty.
      while Imaging.EnumFileFormats(I, Formats[I].Name, Formats[I].Ext,
        Formats[I].Masks, Formats[I].CanSave, Formats[I].IsMulti) do
      begin
        SetLength(Formats, I + 1);
      end;
      SetLength(Formats, I);

      // Test image loading functions for all supported image file formats
      // note that image loaded in one LoadImage is automatically
      // freed in then next LoadImage call so no leaks (should) occur.
      Log('-------------  Loading Images -------------');
      for I := Low(Formats) to High(Formats) do
        LoadImage(GetImageName(Formats[I].Ext));

      // Test image manipulation functions like conversions, resizing and other.
      Log(sLineBreak + '-----------  Image Manipulation -----------');
      ManipulateImage(maResize3k);
      ManipulateImage(maConvARGB64);
      ManipulateImage(maFlip);
      ManipulateImage(maMirror);
      ManipulateImage(maSwapChannels);
      ManipulateImage(maConvARGBF);
      ManipulateImage(maConvARGB16);
      ManipulateImage(maConvARGB32);
      ManipulateImage(maClone);
      ManipulateImage(maCopyRect);
      ManipulateImage(maFill);
      ManipulateImage(maStretchRect);
      ManipulateImage(maReplace);
      ManipulateImage(maMipMaps);
      ManipulateImage(maSplit);
      ManipulateImage(maResize1k);
      ManipulateImage(maRotate5);
      ManipulateImage(maRotate90);
      ManipulateImage(maReduceColors);
      ManipulateImage(maMakePal);
      ManipulateImage(maMapImage);
      ManipulateImage(maCompressDXT);
      ManipulateImage(maDecompressDXT);
      ManipulateImage(maConvRGB24);

      // Test image saving functions. Image is now in R8G8B8 format. Note that
      // some supported file formats cannot save images in R8G8B8 so their
      // time includes conversions.
      Log(sLineBreak + '-------------  Saving Images --------------');
      for I := Low(Formats) to High(Formats) do
      begin
        if Formats[I].CanSave then
          SaveImage(Formats[I].Ext);
      end;

    except
      on E: Exception do
      begin
        Log('Exception Raised!');
        Log(E.Message);
        Failed := True;
      end;
    end;
  finally
    // Image must be freed in the end.
    Imaging.FreeImage(Img);
    if LogFileOpen then
    begin
      CloseFile(LogFile);
      LogFileOpen := False;
      WriteLn;
      WriteLn('Results written to "' + ExtractRelativePath(GetAppDir + PathDelim,
        OutDir + PathDelim + SLogFileName) + '" file.');
    end;
  end;

  WaitForReturnInDebug;
  if Failed then
    ExitCode := 1;
end;

end.
