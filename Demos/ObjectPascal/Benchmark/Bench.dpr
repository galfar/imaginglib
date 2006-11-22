{
  Vampyre Imaging Library Demo
  Benchmark (ObjectPascal, low level, Win32/Linux/DOS)
  tested in Delphi 7/10, Kylix 3, Free Pascal 2.0.4 (Win32/Linux/DOS)
  written by Marek Mauder

  Simple program which measures time taken by the main Imaging functions
  (loading, manipulation, saving) in microsecond resolution.
  You can use it to compare the speeds of executables created by the supported
  compilers (you can find results for my machine somewhere in Demos directory).

  Important:
    1) During the test large amounts of memory can be allocated by
       the program (e.g. conversion from 3000x3000x64 bit image to 128 bit requires
       over 200 MB of memory).
    2) Program's executable must be located in Demos,
       Demos\SomeDir or Demos\SomeDir1\SomeDir2 to be able to find used data
       files.
}

program Bench;

{$I ImagingOptions.inc}

{ Define this to write results to log file or undef it to
  display them on screen.}
{$DEFINE LOG_TO_FILE}
{ Define this to write images created in saving test on disk.
  They are saved only to memory when testing.}
{ $DEFINE SAVE_IMAGES_TO_FILES}

{$APPTYPE CONSOLE}

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
    maRotate180, maRotate90, maStretchRect);

const
  SDataDir = 'Data';
  SLoadImageJpg =  'Tigers.jpg';
  SLoadImageBmp =  'Tigers.bmp';
  SLoadImagePng =  'Tigers.png';
  SLoadImageDds =  'Tigers.dds';
  SLoadImageTga =  'Tigers.tga';
  SLoadImageMng =  'Drake.mng';
  SLoadImageJng =  'Tigers.jng';
  SSaveImage = '_out';

var
  Time: Int64;
  Img: TImageData;
{$IFDEF LOG_TO_FILE}
  Output: TextFile;
{$ENDIF}

procedure WriteTimeDiff(const Msg: string; const OldTime: Int64);
begin
  WriteLn(Output, Format('%-60s %16.0n us', [Msg, GetTimeMicroseconds - OldTime * 1.0]));
end;

procedure LoadImage(const Name: string);
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  WriteLn(Output, 'Loading image: ' + Name);
  Mem.LoadFromFile(GetDataDir + PathDelim + Name);
  Time := GetTimeMicroseconds;
  // we are loading from memory stream so there is no file system
  // overhead measured
  Imaging.LoadImageFromStream(Mem, Img);
  WriteTimeDiff('Image loaded in:', Time);
  Mem.Free;
end;

procedure SaveImage(const Ext: string);
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  WriteLn(Output, 'Saving image to format: ' + Ext);
  Time := GetTimeMicroseconds;
  // we are saving to memory stream so there is no file system
  // overhead measured
  Imaging.SaveImageToStream(Ext, Mem, Img);
  WriteTimeDiff('Image saved in:', Time);
{$IFDEF SAVE_IMAGES_TO_FILES}
  Mem.SaveToFile(GetAppDir +  PathDelim + sSaveImage + '.' + Ext);
{$ENDIF}
  Mem.Free;
end;

var
  ImgClone: TImageData;
  Subs: TDynImageDataArray;
  FillColor: TColor32Rec = (Color: $FFFF0000);
  NewColor: TColor32Rec = (Color: $FF00CCFF);
  XCount, YCount: LongInt;
  Pal: PPalette32;

procedure ManipulateImage(Man: TManipulation);
begin
  // According to the enum value image manipulation functions are
  // called and measured.
  case Man of
    maResize3k:
      begin
        WriteLn(Output, 'Resizing image to 3000x3000 (bilinear) ... ');
        Time := GetTimeMicroseconds;
        Imaging.ResizeImage(Img, 3000, 3000, rfBilinear);
        WriteTimeDiff('Image resized in: ', Time);
      end;
    maResize1k:
      begin
        WriteLn(Output, 'Resizing image to 1000x1000 (bicubic) ... ');
        Time := GetTimeMicroseconds;
        Imaging.ResizeImage(Img, 1000, 1000, rfBicubic);
        WriteTimeDiff('Image resized in: ', Time);
      end;
    maFlip:
      begin
        WriteLn(Output, 'Flipping image ... ');
        Time := GetTimeMicroseconds;
        Imaging.FlipImage(Img);
        WriteTimeDiff('Image flipped in: ', Time);
      end;
    maMirror:
      begin
        WriteLn(Output, 'Mirroring image ... ');
        Time := GetTimeMicroseconds;
        Imaging.MirrorImage(Img);
        WriteTimeDiff('Image mirrored in:', Time);
      end;
    maSwapChannels:
      begin
        WriteLn(Output, 'Swapping channels of image ... ');
        Time := GetTimeMicroseconds;
        Imaging.SwapChannels(Img, ChannelRed, ChannelGreen);
        WriteTimeDiff('Channels swapped in: ', Time);
      end;
    maConvARGB64:
      begin
        WriteLn(Output, 'Converting image to A16R16G16B16 64bit format ... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA16R16G16B16);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maConvARGBF:
      begin
        WriteLn(Output, 'Converting image to A32B32G32R32F 128bit floating ' +
          'point format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA32B32G32R32F);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maConvARGB16:
      begin
        WriteLn(Output, 'Converting image to A4R4G4B4 16bit format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA4R4G4B4);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maConvRGB24:
      begin
        WriteLn(Output, 'Converting image to R8G8B8 24bit format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifR8G8B8);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maConvARGB32:
      begin
        WriteLn(Output, 'Converting image to A8R8G8B8 32bit format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA8R8G8B8);
        WriteTimeDiff('Image converted in: ', Time);
      end;
    maCompressDXT:
      begin
        WriteLn(Output, 'Compressing image to DXT1 format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifDXT1);
        WriteTimeDiff('Image compressed in: ', Time);
      end;
    maDecompressDXT:
      begin
        WriteLn(Output, 'Decompressing image from DXT1 format... ');
        Time := GetTimeMicroseconds;
        Imaging.ConvertImage(Img, ifA8R8G8B8);
        WriteTimeDiff('Image decompressed in: ', Time);
      end;
    maReduceColors:
      begin
        WriteLn(Output, 'Reducing colors count to 1024... ');
        Time := GetTimeMicroseconds;
        Imaging.ReduceColors(Img, 1024);
        WriteTimeDiff('Colors reduced in: ', Time);
      end;
    maMipMaps:
      begin
        WriteLn(Output, 'Creating mipmaps ... ');
        SetLength(Subs, 0);
        Time := GetTimeMicroseconds;
        Imaging.GenerateMipMaps(Img, 0, Subs);
        WriteTimeDiff('Mipmaps created in: ', Time);
        Imaging.FreeImagesInArray(Subs);
      end;
    maClone:
      begin
        WriteLn(Output, 'Cloning image ... ');
        Imaging.InitImage(ImgClone);
        Time := GetTimeMicroseconds;
        Imaging.CloneImage(Img, ImgClone);
        WriteTimeDiff('Image cloned in: ', Time);
      end;
    maCopyRect:
      begin
        WriteLn(Output, 'Copying rectangle ... ');
        Time := GetTimeMicroseconds;
        Imaging.CopyRect(ImgClone, 0, 1500, 1500, 1500, Img, 0, 0);
        WriteTimeDiff('Rectangle copied in: ', Time);
      end;
    maStretchRect:
      begin
        WriteLn(Output, 'Stretching rectangle (bicubic) ... ');
        Time := GetTimeMicroseconds;
        Imaging.StretchRect(ImgClone, 0, 1500, 1500, 1500, Img, 500, 500, 2000, 2000, rfBicubic);
        WriteTimeDiff('Rectangle stretched in: ', Time);
        Imaging.FreeImage(ImgClone);
      end;
    maMapImage:
      begin
        WriteLn(Output, 'Mapping image to existing palette ... ');
        Time := GetTimeMicroseconds;
        Imaging.MapImageToPalette(Img, Pal, 256);
        WriteTimeDiff('Image mapped in: ', Time);
        Imaging.FreePalette(Pal);
      end;
    maFill:
      begin
        WriteLn(Output, 'Filling rectangle ... ');
        Time := GetTimeMicroseconds;
        Imaging.FillRect(Img, 1500, 0, 1500, 1500, @FillColor);
        WriteTimeDiff('Rectangle filled in: ', Time);
      end;
    maReplace:
      begin
        WriteLn(Output, 'Replacing colors in rectangle ... ');
        Time := GetTimeMicroseconds;
        Imaging.ReplaceColor(Img, 0, 0, Img.Width, Img.Height, @FillColor, @NewColor);
        WriteTimeDiff('Colors replaced in: ', Time);
      end;
    maSplit:
      begin
        WriteLn(Output, 'Splitting image ... ');
        SetLength(Subs, 0);
        Time := GetTimeMicroseconds;
        Imaging.SplitImage(Img, Subs, 300, 300, XCount, YCount, True, @FillColor);
        WriteTimeDiff('Image split in: ', Time);
        Imaging.FreeImagesInArray(Subs);
      end;
    maMakePal:
      begin
        WriteLn(Output, 'Making palette for images ... ');
        Imaging.NewPalette(256, Pal);
        SetLength(Subs, 1);
        Subs[0] := Img;
        Time := GetTimeMicroseconds;
        Imaging.MakePaletteForImages(Subs, Pal, 256, False);
        WriteTimeDiff('Palette made in: ', Time);
        Img := Subs[0];
      end;
    maRotate180:
      begin
        WriteLn(Output, 'Rotating image 180 degrees CCW ... ');
        Time := GetTimeMicroseconds;
        Imaging.RotateImage(Img, 180);
        WriteTimeDiff('Image rotated in: ', Time);
      end;
    maRotate90:
      begin
        WriteLn(Output, 'Rotating image 90 degrees CCW ... ');
        Time := GetTimeMicroseconds;
        Imaging.RotateImage(Img, 90);
        WriteTimeDiff('Image rotated in: ', Time);
      end;
  end;
end;

begin
{$IFDEF LOG_TO_FILE}
  // if logging to file is defined new output file is created
  // and all messages are written into it
  AssignFile(Output, GetAppDir + PathDelim + 'ResultsPas.log');
  Rewrite(Output);
  WriteLn('Benchmarking ...');
{$ELSE}
  // else standard System.Output file is used
{$ENDIF}

  WriteLn(Output, 'Vampyre Imaging Library Benchmark Demo version ',
    Imaging.GetVersionStr);
  WriteLn(Output);
  SysUtils.ThousandSeparator := ' ';

  if not DirectoryExists(GetDataDir) then
  begin
    // if required testing data is not found, program halts
    WriteLn(Output, 'Error!' + sLineBreak + '"Data" directory with ' +
      'required "Tigers.*" images not found.');
    WriteLn;
    WriteLn('Press RETURN key to exit');
    ReadLn;
    Halt(1);
  end;

  // call this before any manipulation with TImageData record
  Imaging.InitImage(Img);

  try
    // test image loading functions for all supported image file formats
    // note that image loaded in one LoadImage is automaticaly
    // freed in then next LoadImage call
    WriteLn(Output, '-------------  Loading Images -------------');
    LoadImage(SLoadImageJpg);
    LoadImage(SLoadImageBmp);
    LoadImage(SLoadImagePng);
    LoadImage(SLoadImageTga);
    LoadImage(SLoadImageMng);
    LoadImage(SLoadImageJng);
    LoadImage(SLoadImageDds);

    // test image manipulation functions like conversions, resizing and other
    WriteLn(Output, sLineBreak + '-----------  Image Manipulation -----------');
    ManipulateImage(maDecompressDXT);
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
    ManipulateImage(maRotate180);
    ManipulateImage(maRotate90);
    ManipulateImage(maReduceColors);
    ManipulateImage(maMakePal);
    ManipulateImage(maMapImage);
    ManipulateImage(maCompressDXT);

    // test image saving functions. Image is converted to R8G8B8 after DDS file
    // saving, otherwise image would have to be decompressed from DXT1 by
    // the other image savers every time.
    WriteLn(Output, sLineBreak + '-------------  Saving Images --------------');
    SaveImage('dds');
    ManipulateImage(maConvRGB24);
    SaveImage('jpg');
    SaveImage('bmp');
    SaveImage('png');
    SaveImage('tga');
    SaveImage('mng');
    SaveImage('jng');
  except
    on E: Exception do
    begin
      WriteLn('Exception Raised!');
      WriteLn(E.Message);
    end;
  end;

  // image must be freed in the end
  Imaging.FreeImage(Img);
{$IFDEF LOG_TO_FILE}
  CloseFile(Output);
  WriteLn('Results written to "ResultsPas.log" file.');
{$ENDIF}
  WriteLn;
  WriteLn('Press RETURN key to exit');
  ReadLn;

{
  File Notes:

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added thousand separators to output times

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - added filtered image resizing and rectangle stretching
    - added MNG and JNG file saving and loading and exception catcher
}
end.
