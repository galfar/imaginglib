// Vampyre Imaging Library Demo
// Benchmark (C++, dll library usage, Win32)
// tested in MSVC 7.1, BC++ 5.6.4, GCC 3.2
// written by Marek Mauder
//
// This is not actually benchmark like ObjectPascal version because
// all measured functions are called from external library, but it
// shows how to use Imaging dll from C/C++ at least.
//
// Important:
//   1) During the test large amounts of memory can be allocated by
//      the program (e.g. conversion from 3000x3000x64 bit image to 128 bit
//      requires over 200 MB of memory).
//   2) Program's executable must be located in Demos\Bin,
//      or anywhere in the subdirectories of Demos\Cpp dir to be able to find
//      used data files.
//
//   Compiled Imaging library must be located somewhere on system's
//   search path for this sample to work (usually VampyreImaging.dll
//   in C:\Windows or libVampyreImaging.so in /lib).

#include <iostream>
#include <string>
#include <fstream>
#include "..\..\..\Source\Wrappers\Cpp\ImagingImport.h"

using namespace Imaging;
using namespace std;

// Define this to write results to log file or undef it to
// display them on screen.
#define LOG_TO_FILE
// Define this to write images created in saving test on disk.
// They are saved only to memory when testing.
//#define SAVE_IMAGES_TO_FILES

LARGE_INTEGER PerfFrequency;
float InvPerfFrequency;

const string sDataDir = "Data\\";
const string sLoadImageJpg = "Tigers.jpg";
const string sLoadImageBmp = "Tigers.bmp";
const string sLoadImagePng = "Tigers.png";
const string sLoadImageDds = "Tigers.dds";
const string sLoadImageTga = "Tigers.tga";
const string sLoadImageMng = "Drake.mng";
const string sLoadImageJng = "Tigers.jng";
const string sSaveImage = "_out";
const int BufSize = 8 * 1024 * 1024;

enum TManipulation {maResize3k, maResize1k, maFlip, maMirror, maSwapChannels,
  maConvARGB64, maConvARGBF, maConvARGB16, maConvRGB24, maConvARGB32,
  maCompressDXT, maDecompressDXT, maReduceColors, maClone, maMipMaps,
  maCopyRect, maMapImage, maFill, maSplit, maMakePal, maReplace, maRotate180, 
  maRotate90, maStretchRect};

#ifdef LOG_TO_FILE
fstream Out;
#else
// if logging to files is not defined standard cout is used
ostream &Out = cout;
#endif
TImageData Img, ImgClone;
LONGLONG Time;
string Dir;
char SaveBuf[BufSize];
TImageDataList Subs = 0;
TColor32Rec FillColor, NewColor;
int XCount, YCount;
PPalette32 Pal = NULL;

string GetAppDir(void);
string GetDataDir(void);
LONGLONG GetTimeMicroseconds(void);
void LoadImage(const string Name);
void SaveImage(const string Ext);
void ManipulateImage(TManipulation Man);

int main(int argc, char ** argv)
{
  char tmp;
  int Major, Minor, Patch;
  Dir = argv[0];
  FillColor.Color = 0xFFFF0000;
  NewColor.Color = 0xFF00CCFF;

  QueryPerformanceFrequency(&PerfFrequency);
  InvPerfFrequency = (float)1.0 / PerfFrequency.QuadPart;

#ifdef LOG_TO_FILE
  // if logging to file is defined new output file is created
  // and all messages are written into it
  Out.open("ResultsCpp.log", ios::out);
  cout << "Benchmarking..." << endl;
#endif
  // this call must be made before any atempt to use any Imaging function.
  // everything is imported from dll here
  ImLoadLibrary();
  // call this before any manipulation with TImageData record
  ImInitImage(&Img);
  
  ImGetVersion(&Major, &Minor, &Patch);
  Out << "Vampyre Imaging Library Benchmark Demo version " << 
	  Major << '.' << Minor << '.' << Patch << endl << endl;

  // test image loading functions for all supported image file formats
  // note that image loaded in one LoadImage is automaticaly
  // freed in then next LoadImage call
  Out << "-------------  Loading Images -------------" << endl;
  LoadImage(sLoadImageJpg);
  LoadImage(sLoadImageBmp);
  LoadImage(sLoadImagePng);
  LoadImage(sLoadImageTga);
  LoadImage(sLoadImageMng);
  LoadImage(sLoadImageJng);
  LoadImage(sLoadImageDds);

  // test image manipulation functions like conversions, resizing and other
  Out << endl << "-----------  Image Manipulation -----------" << endl;
  
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
  Out << endl << "-------------  Saving Images --------------" << endl;
  SaveImage("dds");
  ManipulateImage(maConvRGB24);
  SaveImage("jpg");
  SaveImage("bmp");
  SaveImage("png");
  SaveImage("tga");
  SaveImage("mng");
  SaveImage("jng");

  // image must be freed in the end
  ImFreeImage(&Img);
  // call this if you no longer need to use Imaging functions
  // Imaging dll is unloaded here
  ImFreeLibrary();
#ifdef LOG_TO_FILE
  Out.close();
  cout << "Results written to 'ResultsCpp.log' file." << endl;
#endif
  cout << "Press any key to exit" << endl;
  cin.get(tmp);
  return 0;
}

string GetAppDir(void)
{
  int Idx;
  string Res = Dir;

  Idx = static_cast<int>(Res.rfind("\\"));
  if (Idx > 0)
    Res.erase(Idx + 1);

  return Res;
}

string GetDataDir(void)
{
  int Idx;
  string Res = Dir;

  Idx = static_cast<int>(Res.find("Bin\\"));
  if (Idx >= 0)
    return Res.erase(Idx) + sDataDir;

  Idx = static_cast<int>(Res.find("Cpp\\"));
  if (Idx >= 0)
    return Res.erase(Idx) + sDataDir;

  return Res;
}

LONGLONG GetTimeMicroseconds(void)
{
  LARGE_INTEGER Time;

  QueryPerformanceCounter(&Time);
  return static_cast<LONGLONG>(1000000 * InvPerfFrequency * Time.QuadPart);
}

void LoadImage(const string Name)
{
  Out << "Loading image: " << Name << endl;
  Time = GetTimeMicroseconds();
  ImFreeImage(&Img);
  ImLoadImageFromFile((GetDataDir() + Name).c_str(), &Img);
  Out << "Image loaded in: " << GetTimeMicroseconds() - Time << " us" << endl;
}

void SaveImage(const string Ext)
{
  int Size = BufSize;
  Out << "Saving image to format: " << Ext << endl;
  Time = GetTimeMicroseconds();
  ImSaveImageToMemory(Ext.c_str(), (void*)SaveBuf, &Size, &Img);
  Out << "Image saved in: " << GetTimeMicroseconds() - Time << " us" << endl;
#ifdef SAVE_IMAGES_TO_FILES
  ImSaveImageToFile((GetAppDir() + sSaveImage + '.' + Ext).c_str(), &Img);
#endif
}

void ManipulateImage(TManipulation Man)
{
  // According to the enum value image manipulation functions are
  // called and measured.
  switch (Man)
  {
    case maResize3k:
      Out << "Resizing image to 3000x3000 (bilinear) ..." << endl;
      Time = GetTimeMicroseconds();
      ImResizeImage(&Img, 3000, 3000, rfBilinear);
      Out << "Image resized in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maResize1k:
      Out << "Resizing image to 1000x1000 (bicubic) ..." << endl;
      Time = GetTimeMicroseconds();
      ImResizeImage(&Img, 1000, 1000, rfBicubic);
      Out << "Image resized in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maFlip:
      Out << "Flipping image ..." << endl;
      Time = GetTimeMicroseconds();
      ImFlipImage(&Img);
      Out << "Image flipped in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maMirror:
      Out << "Mirroring image ..." << endl;
      Time = GetTimeMicroseconds();
      ImMirrorImage(&Img);
      Out << "Image mirrored in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maSwapChannels:
      Out << "Swapping channels of image ..." << endl;
      Time = GetTimeMicroseconds();
      ImSwapChannels(&Img, ChannelRed, ChannelGreen);
      Out << "Channels swapped in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maConvARGB64:
      Out << "Converting image to A16R16G16B16 64bit format ..." << endl;
      Time = GetTimeMicroseconds();
      ImConvertImage(&Img, ifA16R16G16B16);
      Out << "Image converted in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maConvARGBF:
      Out << "Converting image to A32B32G32R32F 128bit floating point format ..." << endl;
      Time = GetTimeMicroseconds();
      ImConvertImage(&Img, ifA32B32G32R32F);
      Out << "Image converted in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maConvARGB16:
      Out << "Converting image to A4R4G4B4 16bit format ..." << endl;
      Time = GetTimeMicroseconds();
      ImConvertImage(&Img, ifA4R4G4B4);
      Out << "Image converted in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maConvRGB24:
      Out << "Converting image to R8G8B8 24bit format ..." << endl;
      Time = GetTimeMicroseconds();
      ImConvertImage(&Img, ifR8G8B8);
      Out << "Image converted in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maConvARGB32:
      Out << "Converting image to A8R8G8B8 32bit format ..." << endl;
      Time = GetTimeMicroseconds();
      ImConvertImage(&Img, ifA8R8G8B8);
      Out << "Image converted in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maCompressDXT:
      Out << "Compressing image to DXT1 format ..." << endl;
      Time = GetTimeMicroseconds();
      ImConvertImage(&Img, ifDXT1);
      Out << "Image compressed in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maDecompressDXT:
      Out << "Decompressing image from DXT1 format ..." << endl;
      Time = GetTimeMicroseconds();
      ImConvertImage(&Img, ifA8R8G8B8);
      Out << "Image decompressed in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maReduceColors:
      Out << "Reducing colors count to 1024 ..." << endl;
      Time = GetTimeMicroseconds();
      ImReduceColors(&Img, 1024);
      Out << "Colors reduced in in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maMipMaps:
	  Out << "Creating mipmaps ..." << endl;
	  Time = GetTimeMicroseconds();
	  ImGenerateMipMaps(&Img, 0, &Subs);
      Out << "Mipmaps created in: " << GetTimeMicroseconds() - Time << " us" << endl;
	  ImFreeImageList(&Subs);
    break;
	case maClone:
	  Out << "Cloning image ..." << endl;
      ImInitImage(&ImgClone);
	  Time = GetTimeMicroseconds();
	  ImCloneImage(&Img, &ImgClone);
      Out << "Image cloned in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
	case maCopyRect:
	  Out << "Copying rectangle ..." << endl;
	  Time = GetTimeMicroseconds();
	  ImCopyRect(&ImgClone, 0, 1500, 1500, 1500, &Img, 0, 0);
      Out << "Rectangle copied in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;	
	case maStretchRect:
	  Out << "Stretching rectangle (bicubic) ... " << endl;
	  Time = GetTimeMicroseconds();
	  ImStretchRect(&ImgClone, 0, 1500, 1500, 1500, &Img, 500, 500, 2000, 2000, rfBicubic);
      Out << "Rectangle stretched in: " << GetTimeMicroseconds() - Time << " us" << endl;
	  ImFreeImage(&ImgClone);
    break;
	case maMapImage:
	  Out << "Mapping image to existing palette ..." << endl;
	  Time = GetTimeMicroseconds();
	  ImMapImageToPalette(&Img, Pal, 256);
      Out << "Image mapped in: " << GetTimeMicroseconds() - Time << " us" << endl;
	  ImFreePalette(&Pal);
    break;
	case maFill:
	  Out << "Filling rectangle ..." << endl;
	  Time = GetTimeMicroseconds();
	  ImFillRect(&Img, 1500, 0, 1500, 1500, &FillColor);
      Out << "Rectangle filled in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
	case maReplace:
	  Out << "Replacing colors in rectagle ..." << endl;
	  Time = GetTimeMicroseconds();
	  ImReplaceColor(&Img, 0, 0, Img.Width, Img.Height, &FillColor, &NewColor);
      Out << "Colors replaced in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
    case maSplit:
	  Out << "Splitting image ..." << endl;
	  Time = GetTimeMicroseconds();
	  Subs = 0;
	  ImSplitImage(&Img, &Subs, 300, 300, &XCount, &YCount, True, &FillColor);
      Out << "Image split in: " << GetTimeMicroseconds() - Time << " us" << endl;
	  ImFreeImageList(&Subs);
    break;   
	case maMakePal:
	  Out << "Making palette for images ..." << endl;
	  ImNewPalette(256, &Pal);
	  ImInitImageList(1, &Subs);
	  ImSetImageListElement(Subs, 0, &Img);
	  Time = GetTimeMicroseconds();
	  ImMakePaletteForImages(Subs, Pal, 256, False);
      Out << "Palette made in: " << GetTimeMicroseconds() - Time << " us" << endl;
	  ImFreeImage(&Img);
	  ImGetImageListElement(Subs, 0, &Img);
	  ImFreeImageList(&Subs);
    break;  
	case maRotate180:
      Out << "Rotating image 180 degrees CCW ... " << endl;
      Time = GetTimeMicroseconds();
      ImRotateImage(&Img, 180);
      Out << "Image rotated in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
	case maRotate90:
      Out << "Rotating image 90 degrees CCW ... " << endl;
      Time = GetTimeMicroseconds();
      ImRotateImage(&Img, 90);
      Out << "Image rotated in: " << GetTimeMicroseconds() - Time << " us" << endl;
    break;
  }
}

/*
  Changes/Bug Fixes:

  -- 0.17 -----------------------------------------------------
    - made changes to be up to date with Pascal version
	  (new tests, filtered resizing, ...)

  -- 0.15 -----------------------------------------------------
    - changed working with ImageDataList types because of change 
	  in header

*/

