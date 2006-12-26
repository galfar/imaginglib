{
  Vampyre Imaging Library Demo
  Demo01 (Delphi.NET, dll library usage, dotNET)
  tested in BDS 2006
  written by Marek Mauder

  Simple image viewer program. It loads image stored in one of file formats
  supported by Imaging library and assigns it to PictureBox component.
  You can get some information about loaded image by clicking on PictureBox.
}

unit WinForm;

interface

uses
  System.Drawing, System.Drawing.Imaging, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data, System.Diagnostics, System.IO, System.Resources,
  // Imaging dotNET wrapper
  ImagingNET;

type
  TWinForm = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    components: System.ComponentModel.IContainer;
    Button: System.Windows.Forms.Button;
    Picture: System.Windows.Forms.PictureBox;
    OpenFile: System.Windows.Forms.OpenFileDialog;
    Panel1: System.Windows.Forms.Panel;
    Link: System.Windows.Forms.LinkLabel;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure Button_Click(sender: System.Object; e: System.EventArgs);
    procedure Picture_Click(sender: System.Object; e: System.EventArgs);
    procedure Link_LinkClicked(sender: System.Object; e: System.Windows.Forms.LinkLabelLinkClickedEventArgs);
  {$ENDREGION}
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  private
    Info: TImageFormatInfo;
    ImgFile: string;
    ImgWidth: Int32;
    ImgHeight: Int32;
    Img: TImageData;
  public
    constructor Create;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TWinForm))]

implementation

{$AUTOBOX ON}

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support -- do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TWinForm.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TWinForm));
  Self.Picture := System.Windows.Forms.PictureBox.Create;
  Self.OpenFile := System.Windows.Forms.OpenFileDialog.Create;
  Self.Panel1 := System.Windows.Forms.Panel.Create;
  Self.Link := System.Windows.Forms.LinkLabel.Create;
  Self.Button := System.Windows.Forms.Button.Create;
  Self.Panel1.SuspendLayout;
  Self.SuspendLayout;
  // 
  // Picture
  // 
  Self.Picture.Anchor := (System.Windows.Forms.AnchorStyles((((System.Windows.Forms.AnchorStyles.Top 
    or System.Windows.Forms.AnchorStyles.Bottom) or System.Windows.Forms.AnchorStyles.Left) 
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.Picture.BackColor := System.Drawing.Color.FromArgb((Byte(192)), (Byte(255)), 
      (Byte(255)));
  Self.Picture.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.Picture.Cursor := System.Windows.Forms.Cursors.Hand;
  Self.Picture.Location := System.Drawing.Point.Create(0, 35);
  Self.Picture.Name := 'Picture';
  Self.Picture.Size := System.Drawing.Size.Create(500, 265);
  Self.Picture.SizeMode := System.Windows.Forms.PictureBoxSizeMode.CenterImage;
  Self.Picture.TabIndex := 1;
  Self.Picture.TabStop := False;
  Include(Self.Picture.Click, Self.Picture_Click);
  // 
  // OpenFile
  // 
  Self.OpenFile.Filter := 'Imaging Files (*.jpg;*.png;*.dds;*.mng;*.jng;*.bm' +
  'p;*.tga)|*.jpg;*.png;*.dds;*.mng;*.jng;*.bmp;*.tga|All files (*.*)|*.*';
  // 
  // Panel1
  // 
  Self.Panel1.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.Panel1.Controls.Add(Self.Link);
  Self.Panel1.Controls.Add(Self.Button);
  Self.Panel1.Dock := System.Windows.Forms.DockStyle.Top;
  Self.Panel1.Location := System.Drawing.Point.Create(0, 0);
  Self.Panel1.Name := 'Panel1';
  Self.Panel1.Size := System.Drawing.Size.Create(492, 35);
  Self.Panel1.TabIndex := 2;
  // 
  // Link
  // 
  Self.Link.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Top 
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.Link.LinkArea := System.Windows.Forms.LinkArea.Create(0, 522);
  Self.Link.Location := System.Drawing.Point.Create(305, 5);
  Self.Link.Name := 'Link';
  Self.Link.Size := System.Drawing.Size.Create(180, 25);
  Self.Link.TabIndex := 2;
  Self.Link.TabStop := True;
  Self.Link.Text := 'http://imaginglib.sourceforge.net';
  Self.Link.TextAlign := System.Drawing.ContentAlignment.MiddleCenter;
  Include(Self.Link.LinkClicked, Self.Link_LinkClicked);
  // 
  // Button
  // 
  Self.Button.Location := System.Drawing.Point.Create(5, 5);
  Self.Button.Name := 'Button';
  Self.Button.Size := System.Drawing.Size.Create(75, 25);
  Self.Button.TabIndex := 1;
  Self.Button.Text := 'Load Image';
  Include(Self.Button.Click, Self.Button_Click);
  // 
  // TWinForm
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.ClientSize := System.Drawing.Size.Create(492, 296);
  Self.Controls.Add(Self.Panel1);
  Self.Controls.Add(Self.Picture);
  Self.Icon := (System.Drawing.Icon(resources.GetObject('$this.Icon')));
  Self.Name := 'TWinForm';
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Imaging.NET Demo 01';
  Self.Panel1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TWinForm.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  // Free source image
  Imaging.FreeImage(Img);
  inherited Dispose(Disposing);
end;

constructor TWinForm.Create;
var
  Major, Minor, Patch: Int32;
begin
  inherited Create;
  InitializeComponent;
  // Get imaging version
  Imaging.GetVersion(Major, Minor, Patch);
  Self.Text := Self.Text + System.&String.Format(' (Imaging library version {0}.{1}.{2})',
    [Major, Minor, Patch]);
end;

procedure TWinForm.Link_LinkClicked(sender: System.Object; e: System.Windows.Forms.LinkLabelLinkClickedEventArgs);
begin
  System.Diagnostics.Process.Start(Link.Text);
end;

procedure TWinForm.Picture_Click(sender: System.Object; e: System.EventArgs);
const
  NewLine = #13#10;
var
  Msg: System.&String;
begin
  if System.IO.File.Exists(ImgFile) then
    Msg := 'File Name: ' + ImgFile + NewLine +
      'Width: ' + Img.Width.ToString + NewLine +
      'Height: ' + Img.Height.ToString + NewLine+
      'Format: ' + Info.Name + NewLine +
      'Size: ' + Single(Imaging.GetPixelsSize(Info.Format, Img.Width, Img.Height) / 1024).ToString +
      ' KiB ' + NewLine
   else
     Msg := 'No image loaded';

  MessageBox.Show(Msg, 'Image Info',
    MessageBoxButtons.OK, MessageBoxIcon.Information);
end;

procedure TWinForm.Button_Click(sender: System.Object; e: System.EventArgs);
var
  Stride: LongInt;
begin
  OpenFile.Filter := Imaging.GetImageFileFormatsFilter(True);

  if (OpenFile.ShowDialog = System.Windows.Forms.DialogResult.OK) then
  begin
    // Free old image
    Imaging.FreeImage(Img);
    // Load and test image
    if Imaging.LoadImageFromFile(OpenFile.FileName, Img) and Imaging.TestImage(Img) then
    begin
      // Get original image frmat info
      Imaging.GetImageFormatInfo(Img.Format, Info);
      ImgWidth := Img.Width;
      ImgHeight := Img.Height;
      ImgFile := OpenFile.FileName;
      // Convert image to 32bit ARGB format
      Imaging.ConvertImage(Img, ifA8R8G8B8);
      // Get number of bytes per one line
      Stride := Imaging.GetPixelsSize(Img.Format, Img.Width, 1);
      // Create dotNET bitmap and assign it to PictureBox
      if Picture.Image <> nil then
        Picture.Image.Dispose;
      Picture.Image := Bitmap.Create(Img.Width, Img.Height, Stride,
        PixelFormat.Format32bppArgb, Img.Bits);
      // We cannot free source image even though we will not
      // use it any more. It looks like Bitmap uses Img.Bits directly
      // without making its own copy
    end
    else
    begin
      MessageBox.Show('Error when loading selected image', 'Imaging Error',
         MessageBoxButtons.OK, MessageBoxIcon.Error);
    end;
  end;
end;

{
  File Notes:

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - OpenDialog has now proper filters.
 }

end.
