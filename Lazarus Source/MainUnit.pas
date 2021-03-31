unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Controls,SysUtils,ExtCtrls,StdCtrls,Forms,Buttons,LCLType,Dialogs,
  Classes,Graphics,FPImage,IntfGraphics,LazCanvas, ComCtrls,SpriteFile;

type

  { TMainForm }

  TMainForm = class(TForm)
   CheckBox1: TCheckBox;
    OpenDialog: TOpenDialog;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    sb_OpenFile: TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    sb_Save: TSpeedButton;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure sb_OpenFileClick(Sender: TObject);
    procedure LoadAFile(filename: String);
    function createImage(cparent: TObject;ctop,cleft,cwidth,cheight: Integer): TImage;
    procedure FormCreate(Sender: TObject);
    function validateFilename(f: String): String;
    procedure Image1DblClick(Sender: TObject);
    procedure sb_SaveClick(Sender: TObject);
    procedure SaveAsPng(screen:TBitmap;AFileName:String;Mask:Boolean;BGColour:Cardinal);
    procedure UpdateProgress(p: Integer);
  private
   image: array of TImage;
  public
   SpriteList: TSpriteFile;
   const
    AppTitle = 'Sprite Converter';
    AppVersion = '0.01.5';
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses BigImageUnit;

procedure TMainForm.FormCreate(Sender: TObject);
begin
 Application.Title:=AppTitle;
end;

procedure TMainForm.Image1DblClick(Sender: TObject);
var
 x: Integer;
begin
 if Sender is TImage then
 begin
  x:=TImage(Sender).Tag;
  BigImageForm.ZoomedImage.Picture.Clear;
  BigImageForm.ZoomedImage.Align:=alNone;
  BigImageForm.ZoomedImage.Width:=SpriteList.SpriteList[x].Image.Width;
  BigImageForm.ZoomedImage.Height:=SpriteList.SpriteList[x].Image.Height;
  BigImageForm.ZoomedImage.Canvas.AntialiasingMode:=amOff;
  BigImageForm.ZoomedImage.Stretch:=True;
  BigImageForm.ZoomedImage.Proportional:=True;
  BigImageForm.ZoomedImage.Canvas.Pen.Color:=BigImageForm.Color;
  BigImageForm.ZoomedImage.Canvas.Brush.Color:=BigImageForm.Color;
  BigImageForm.ZoomedImage.Canvas.Rectangle(0,0,BigImageForm.ZoomedImage.Width,BigImageForm.ZoomedImage.Height);
  BigImageForm.ZoomedImage.Canvas.Draw(0,0,SpriteList.SpriteList[x].Image);
  BigImageForm.ZoomedImage.Align:=alClient;
  BigImageForm.Caption:=SpriteList.SpriteList[x].Name;
  BigImageForm.lb_size.Caption:='Size:'+IntToStr(SpriteList.SpriteList[x].PixWidth)+'x'
                                       +IntToStr(SpriteList.SpriteList[x].ScanLines+1)
                               +' BPP:'+IntToStr(SpriteList.SpriteList[x].BPP);
  if SpriteList.SpriteList[x].BPP<>SpriteList.SpriteList[x].BPPOriginal then
   BigImageForm.lb_size.Caption:=BigImageForm.lb_size.Caption
                                +' (converted from '
                                +IntToStr(SpriteList.SpriteList[x].BPPOriginal)+'bpp)';
  BigImageForm.ShowModal;
 end;
end;

procedure TMainForm.sb_OpenFileClick(Sender: TObject);
begin
 if OpenDialog.Execute then
  LoadAFile(OpenDialog.Filename);
end;

procedure TMainForm.LoadAFile(filename: String);
var
  x    : Integer;
  ix,iy: Cardinal;
  bigH : Integer;
begin
 //First, we'll destroy any images that have been opened before
 if Length(image)>0 then
  for x:=0 to Length(image)-1 do
   image[x].Free;
 SetLength(image,0);
 if SpriteList.SpriteFile<>'' then
 begin
  SpriteList.Free;
  SpriteList:=TSpriteFile.Create;
 end;
 SpriteList.ProgressIndicator:=@UpdateProgress;
 SpriteList.LoadSpriteFile(filename);
 if Length(SpriteList.SpriteList)>0 then
 begin
  //Set up the arrays for the sprites
  SetLength(image,Length(SpriteList.SpriteList));
  //Image position on the form
  ix:=4;
  iy:=4;
  bigH:=0;
  for x:=0 to Length(SpriteList.SpriteList)-1 do
  begin
   //Display it
   if SpriteList.SpriteList[x].Image<>nil then
   begin
    image[x]:=TImage.Create(Scrollbox1 as TComponent);
    image[x].Parent:=ScrollBox1 as TWinControl;
    image[x].Width:=SpriteList.SpriteList[x].Image.Width;
    image[x].Height:=SpriteList.SpriteList[x].Image.Height;
    image[x].OnDblClick:=@Image1DblClick;
    image[x].Tag:=x;
    image[x].Visible:=True;
    image[x].Canvas.AntialiasingMode:=amOff;
    image[x].Stretch:=True;
    image[x].Proportional:=True;
    if (ix+32>=ScrollBox1.ClientWidth) then
    begin
     ix:=4;
     inc(iy,bigH+4);
     bigH:=0;
    end;
    image[x].Top:=iy;
    image[x].Left:=ix;
    image[x].Canvas.Pen.Color:=MainForm.Color;//BGColour mod $1000000;
    image[x].Canvas.Brush.Color:=MainForm.Color;//BGColour mod $1000000;
    image[x].Canvas.Rectangle(0,0,image[x].Width,image[x].Height);
    //image[x].Picture.Bitmap.Transparent:=True;
    //image[x].Picture.Bitmap.TransparentColor:=MainForm.Color;//BGColour mod $1000000;
    image[x].Canvas.Draw(0,0,SpriteList.SpriteList[x].Image);
    image[x].Width:=32;
    image[x].Height:=32; // <- this does not work}
    //image[x].Transparent:=True;
    //Adjust for the next sprite position
    ix:=ix+image[x].Width+4;
    if image[x].Height>bigH then bigH:=image[x].Height;
    image[x].Hint:=SpriteList.SpriteList[x].Name;
   end;
   //Update the window
   ProgressBar1.Position:=100+Round((x/Length(SpriteList.SpriteList))*100);
   Application.ProcessMessages;
  end;
 end;
 if CheckBox1.Checked then
  Memo1.Lines:=SpriteList.LogFile;
 ProgressBar1.Position:=ProgressBar1.Max;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
begin
 LoadAFile(FileNames[0]);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
 Caption:=AppTitle+' '+AppVersion+' by Gerald J Holdsworth';
 SpriteList:=TSpriteFile.Create;
end;

procedure TMainForm.sb_SaveClick(Sender: TObject);
var
 Dir: String;
 x: Integer;
begin
 if Length(SpriteList.SpriteList)>0 then
 begin
  if SelectDirectoryDialog1.Execute then
  begin
   Dir:=SelectDirectoryDialog1.FileName;
   if Dir[Length(dir)]<>PathDelim then Dir:=Dir+PathDelim;
   for x:=0 to Length(SpriteList.SpriteList)-1 do
   begin
    //PNG
    SaveAsPng(SpriteList.SpriteList[x].Image,Dir+PathDelim
                                 +validateFilename(SpriteList.SpriteList[x].Name)+'.png',
                                 SpriteList.SpriteList[x].TransFormat>0,
                                 SpriteList.SpriteList[x].BGColour);
    //Save BMP
    SpriteList.SpriteList[x].Image.SaveToFile(Dir+PathDelim
                                 +validateFilename(SpriteList.SpriteList[x].Name)+'.bmp');
   end;
  end;
 end;
end;

procedure TMainForm.SaveAsPng(screen:TBitmap;AFileName:String;Mask:Boolean;BGColour:Cardinal);
var
 png: TPortableNetworkGraphic;
 img: TLazIntfImage;
 cnv: TLazCanvas;
 col: TFPColor;
 x,y: Integer;
begin
 col.Alpha:=(BGColour and $FF000000)>>24;
 col.Red  :=BGColour and $FF;
 col.Green:=(BGColour and $FF00)>>8;
 col.Blue :=(BGColour and $FF0000)>>16;
 col.Alpha:=col.Alpha or col.Alpha<<8;
 col.Red  :=col.Red or col.Red<<8;
 col.Green:=col.Green or col.Green<<8;
 col.Blue :=col.Blue or col.Blue<<8;
 png:=TPortableNetworkGraphic.Create;
 png.PixelFormat:=pf32Bit;  // 32 bit --> with alpha channel
 png.SetSize(screen.Width,screen.Height);
 if Mask then png.Transparent:=True;
 img:=png.CreateIntfImage;
 cnv:=TLazCanvas.Create(img);
 cnv.Draw(0,0,screen.CreateIntfImage);
 if Mask then
  for y:=0 to img.Height-1 do
   for x:=0 to img.Width-1 do
    if cnv.Colors[x,y]=col then
     cnv.Colors[x,y]:=colTransparent;
 png.LoadFromIntfImage(img);
 png.SaveToFile(AFileName);
 cnv.Free;
 img.Free;
 png.Free;
end;

function TMainForm.createImage(cparent: TObject;ctop,cleft,cwidth,cheight: Integer): TImage;
var
 i: TImage;
begin
  i:=TImage.Create(cparent as TComponent);
  i.Parent:=cparent as TWinControl;
  i.Visible:=true;
  i.Top:=ctop;
  i.Left:=cleft;
  i.Width:=cwidth;
  i.Height:=cheight;
  i.Stretch:=true;
  i.Proportional:=True;
  i.Center:=True;
  createImage:=i;
end;

function TMainForm.validateFilename(f: String): String;
var
 i: Integer;
begin
 for i:=1 to Length(f) do
  if (f[i]='\')
  or (f[i]='/')
  or (f[i]=':')
  or (f[i]='*')
  or (f[i]='?')
  or (f[i]='"')
  or (f[i]='<')
  or (f[i]='>')
  or (f[i]='|') then
   f[i]:=' ';
 Result:=f;
end;

procedure TMainForm.UpdateProgress(p: Integer);
begin
 ProgressBar1.Position:=p;
 Application.ProcessMessages;
end;

end.
