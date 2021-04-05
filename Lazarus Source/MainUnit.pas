unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Controls,SysUtils,ExtCtrls,StdCtrls,Forms,Buttons,LCLType,Dialogs,
  Classes,Graphics,FPImage,IntfGraphics,LazCanvas, ComCtrls,SpriteFile;

type

  { TMainForm }

  TMainForm = class(TForm)
   MainToolBarImages: TImageList;
    OpenDialog: TOpenDialog;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    ScrollBox1: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    MainToolBar: TToolBar;
    btnOpenSprite: TToolButton;
    btnSaveAsBMPPNG: TToolButton;
    btnSaveAsBMP: TToolButton;
    btnSaveAsPNG: TToolButton;
    btnSaveSprite: TToolButton;
    btnSavePalette: TToolButton;
    StatusBar1: TStatusBar;
    ToolButton1: TToolButton;
    btnAbout: TToolButton;
    btnSaveText: TToolButton;
    procedure btnAboutClick(Sender: TObject);
    procedure btnSaveTextClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure sb_OpenFileClick(Sender: TObject);
    procedure LoadAFile(filename: String);
    function createLabel(cparent: TObject;ctop,cleft: Integer;spritename: String): TLabel;
    procedure FormCreate(Sender: TObject);
    function validateFilename(f: String): String;
    procedure Image1DblClick(Sender: TObject);
    procedure sb_SaveClick(Sender: TObject);
    procedure SaveAsPng(screen:TBitmap;AFileName:String;Mask:Boolean;BGColour:Cardinal);
    procedure UpdateProgress(p: Integer);
  private
   image: array of TImage;
   spritename: array of TLabel;
  public
   SpriteList: TSpriteFile;
   const
    AppTitle = 'Sprite Converter';
    AppVersion = '1.00';
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses BigImageUnit,AboutUnit;

procedure TMainForm.FormCreate(Sender: TObject);
begin
 Application.Title:=AppTitle;
end;

procedure TMainForm.Image1DblClick(Sender: TObject);
var
 x: Integer;
 t: String;
begin
 if Sender is TImage then
 begin
  //Set up the image
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
  BigImageForm.ZoomedImage.Tag:=x;
  //Sprite name in the dialogue's title bar
  BigImageForm.Caption:=SpriteList.SpriteList[x].Name;
  //Sprite information
  //Size
  BigImageForm.lb_size.Caption:=IntToStr(SpriteList.SpriteList[x].PixWidth)+'x'
                               +IntToStr(SpriteList.SpriteList[x].ScanLines+1);
  BigImageForm.SizePanel.Top:=0;
  //BPP
  BigImageForm.lb_bpp.Caption:=IntToStr(SpriteList.SpriteList[x].BPP)+'bpp';
  if SpriteList.SpriteList[x].BPP<>SpriteList.SpriteList[x].BPPOriginal then
   BigImageForm.lb_bpp.Caption:=BigImageForm.lb_bpp.Caption
                               +' (originally '
                               +IntToStr(SpriteList.SpriteList[x].BPPOriginal)+'bpp)';
  BigImageForm.BPPPanel.Top:=BigImageForm.SizePanel.Top+BigImageForm.SizePanel.Height;
  //Mask
  BigImageForm.lb_mask.Caption:=SpriteList.MaskFormat(x);
  BigImageForm.MaskPanel.Top:=BigImageForm.BPPPanel.Top+BigImageForm.BPPPanel.Height;
  //Palette
  BigImageForm.lb_palette.Caption:=SpriteList.SpriteList[x].PaletteType;
  BigImageForm.PalettePanel.Top:=BigImageForm.MaskPanel.Top+BigImageForm.MaskPanel.Height;
  //Colours
  if SpriteList.SpriteList[x].PaletteColours>0 then
  begin
   BigImageForm.ColoursPanel.Visible:=True;
   BigImageForm.lb_colours.Caption:=IntToStr(SpriteList.SpriteList[x].PaletteColours);
   BigImageForm.ColoursPanel.Top:=BigImageForm.ColoursPanel.Top+BigImageForm.BPPPanel.Height;
  end
  else BigImageForm.ColoursPanel.Visible:=False;
  //Sprite Type 
  BigImageForm.lb_spritetype.Caption:=SpriteList.SpriteType(x);
  BigImageForm.SpriteTypePanel.Top:=BigImageForm.ColoursPanel.Top+BigImageForm.ColoursPanel.Height;
  //Mode Data
  if SpriteList.SpriteList[x].ModeData>54 then
   BigImageForm.lb_mode.Caption:='0x'+IntToHex(SpriteList.SpriteList[x].ModeData,8)
  else
   BigImageForm.lb_mode.Caption:=IntToStr(SpriteList.SpriteList[x].ModeData);
  BigImageForm.ModePanel.Top:=BigImageForm.SpriteTypePanel.Top+BigImageForm.SpriteTypePanel.Height;
  //Mode Flags
  t:=SpriteList.ModeFlag(x);
  if t<>'' then
  begin
   BigImageForm.ModeFlagsPanel.Visible:=True;
   BigImageForm.lb_modeflags.Caption:=SpriteList.ModeFlag(x);
   BigImageForm.ModeFlagsPanel.Top:=BigImageForm.ModePanel.Top+BigImageForm.ModePanel.Height;
  end
  else BigImageForm.ModeFlagsPanel.Visible:=False;
  //OS Compatibility
  BigImageForm.lb_OS.Caption:=SpriteList.OS(x);
  BigImageForm.OSPanel.Top:=BigImageForm.ModeFlagsPanel.Top+BigImageForm.ModeFlagsPanel.Height;
  //Show the form
  BigImageForm.Height:=306;
  BigImageForm.Width :=653;
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
const
  imgsizex = 32;
  imgsizey = 32;
begin
 //Disable the controls
 btnSaveAsBMPPNG.Enabled:=False;
 btnSaveAsBMP.Enabled   :=False;
 btnSaveAsPNG.Enabled   :=False;
 btnSaveSprite.Enabled  :=False;
 btnSavePalette.Enabled :=False;
 btnSaveText.Enabled    :=False;
 //First, we'll destroy any images that have been opened before
 if Length(image)>0 then
  for x:=0 to Length(image)-1 do
  begin
   image[x].Free;
   spritename[x].Free;
  end;
 SetLength(image,0);
 SetLength(spritename,0);
 if SpriteList.SpriteFile<>'' then
 begin
  SpriteList.Free;
  SpriteList:=TSpriteFile.Create;
 end;
 ProgressBar1.Visible:=True;
 SpriteList.ProgressIndicator:=@UpdateProgress;
 SpriteList.LoadSpriteFile(filename);
 if Length(SpriteList.SpriteList)>0 then
 begin
  //Enable the controls
  btnSaveText.Enabled    :=True;
  btnSaveAsBMPPNG.Enabled:=True;
  //Display the filename
  StatusBar1.Panels[0].Text:=filename;
  //Set up the arrays for the sprites
  SetLength(image,Length(SpriteList.SpriteList));
  SetLength(spritename,Length(SpriteList.SpriteList));
  //Image position on the form
  ix:=4;
  iy:=4;
  bigH:=0;
  for x:=0 to Length(SpriteList.SpriteList)-1 do
  begin
   //Display it
   if SpriteList.SpriteList[x].Image<>nil then
   begin
    if x>0 then
    begin
     //Adjust for the next sprite position
     if ix+((imgsizex*2)+4)*2>=ScrollBox1.ClientWidth then
     begin
      ix:=4;
      inc(iy,bigH+4);
      bigH:=0;
     end
     else
      inc(ix,(imgsizex*2)+4);
    end;
    spritename[x]:=createLabel(Scrollbox1,iy+imgsizey+4,ix,SpriteList.SpriteList[x].Name);
    spritename[x].Left:=ix+(((imgsizex*2)-spritename[x].Width)div 2);
    if spritename[x].Height+(spritename[x].Top-iy)>bigH then
     bigH:=spritename[x].Height+(spritename[x].Top-iy);
    image[x]:=TImage.Create(Scrollbox1 as TComponent);
    image[x].Parent:=Scrollbox1 as TWinControl;
    image[x].Width:=SpriteList.SpriteList[x].Image.Width;
    image[x].Height:=SpriteList.SpriteList[x].Image.Height;
    image[x].OnDblClick:=@Image1DblClick;
    image[x].Tag:=x;
    image[x].Visible:=True;
    image[x].Canvas.AntialiasingMode:=amOff;
    image[x].Stretch:=True;
    image[x].Proportional:=True;
    image[x].Top:=iy;
    image[x].Left:=ix+(imgsizex div 2);
    image[x].Canvas.Pen.Color:=MainForm.Color;//BGColour mod $1000000;
    image[x].Canvas.Brush.Color:=MainForm.Color;//BGColour mod $1000000;
    image[x].Canvas.Rectangle(0,0,image[x].Width,image[x].Height);
    //image[x].Picture.Bitmap.Transparent:=True;
    //image[x].Picture.Bitmap.TransparentColor:=MainForm.Color;//BGColour mod $1000000;
    image[x].Canvas.Draw(0,0,SpriteList.SpriteList[x].Image);
    image[x].Width:=imgsizex;
    image[x].Height:=imgsizey; // <- this does not work with transparent above
    //image[x].Transparent:=True;
    image[x].Hint:=SpriteList.SpriteList[x].Name;
   end;
   //Update the window
   ProgressBar1.Position:=100+Round((x/Length(SpriteList.SpriteList))*100);
   if Round((x/Length(SpriteList.SpriteList))*100)mod 10=0 then
    Application.ProcessMessages;
  end;
 end
 else
  StatusBar1.Panels[0].Text:='Invalid Sprite File: "'+filename+'"';
 ProgressBar1.Position:=ProgressBar1.Max;
 ProgressBar1.Visible:=False;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
begin
 LoadAFile(FileNames[0]);
end;

procedure TMainForm.btnAboutClick(Sender: TObject);
var
 platform: String;
begin
 //Determine the current platform (compile time directive)
 platform:='';
 {$IFDEF Darwin}
 platform:=' macOS';            //Apple Mac OS X
 {$ENDIF}
 {$IFDEF Windows}
 platform:=' Windows';          //Microsoft Windows
 {$ENDIF}
 {$IFDEF Linux}
 platform:=' Linux';            //Linux
 {$ENDIF}
 {$IFDEF CPU32}
 platform:=platform+' 32 bit';  //32 bit CPU
 {$ENDIF}
 {$IFDEF CPU64}
 platform:=platform+' 64 bit';  //64 bit CPU
 {$ENDIF}
 {$IFDEF CPUARM}
 platform:=platform+' ARM';     //ARM CPU
 {$ENDIF}
 AboutForm.IconImage.Visible:=True;
 AboutForm.lb_Title.Caption:=AppTitle;
 AboutForm.lb_Version.Caption:='Version '+AppVersion+platform;
 AboutForm.ShowModal;
end;

procedure TMainForm.btnSaveTextClick(Sender: TObject);
begin
 if SaveDialog1.Execute then
  SpriteList.LogFile.SaveToFile(SaveDialog1.Filename);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
 Caption:=AppTitle+' '+AppVersion+' by Gerald J Holdsworth';
 SpriteList:=TSpriteFile.Create;
 //Disable the controls
 btnSaveAsBMPPNG.Enabled:=False;
 btnSaveAsBMP.Enabled   :=False;
 btnSaveAsPNG.Enabled   :=False;
 btnSaveSprite.Enabled  :=False;
 btnSavePalette.Enabled :=False;
 btnSaveText.Enabled    :=False;
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

function TMainForm.createLabel(cparent: TObject;ctop,cleft: Integer;spritename: String): TLabel;
begin
 while(spritename[Length(spritename)]=' ')and(Length(spritename)>1)do
  spritename:=LeftStr(spritename,Length(spritename)-1);
 while(spritename[1]=' ')and(Length(spritename)>1)do
  spritename:=RightStr(spritename,Length(spritename)-1);
 Result:=TLabel.Create(cparent as TComponent);
 Result.Parent:=cparent as TWinControl;
 Result.Top:=ctop;
 Result.Left:=cleft;
 Result.Caption:=spritename;
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
