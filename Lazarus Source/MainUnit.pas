unit MainUnit;

{
Sprite Converter written by Gerald Holdsworth
Windows/macOS/Linux application to load and convert RISC OS sprites into Windows
Bitmap and PNG.

Copyright (C) 2018-2021 Gerald Holdsworth gerald@hollypops.co.uk

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Controls,SysUtils,ExtCtrls,StdCtrls,Forms,Buttons,LCLType,Dialogs,
  Classes,Graphics,FPImage,IntfGraphics,ComCtrls, ExtDlgs,SpriteFile,Global;

type

  { TMainForm }

  TMainForm = class(TForm)
   OpenImageDialog: TOpenPictureDialog;
   StatusBarImages: TImageList;
   Texture: TImage;
   MainToolBarImages: TImageList;
    OpenDialog: TOpenDialog;
    ProgressInd: TProgressBar;
    SavePaletteFile: TSaveDialog;
    SaveSpriteFile: TSaveDialog;
    SaveTextFile: TSaveDialog;
    MainSpriteArea: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    MainToolBar: TToolBar;
    btnOpenSprite: TToolButton;
    btnSaveAsBMPPNG: TToolButton;
    btnSaveAsBMP: TToolButton;
    btnSaveAsPNG: TToolButton;
    btnSaveSprite: TToolButton;
    btnSavePalette: TToolButton;
    MainStatusBar: TStatusBar;
    ToolButton1: TToolButton;
    btnAbout: TToolButton;
    btnSaveText: TToolButton;
    btnOptions: TToolButton;
    btnCloseFile: TToolButton;
    btnImportImage: TToolButton;
    ToolButton3: TToolButton;
    procedure btnAboutClick(Sender: TObject);
    procedure btnCloseFileClick(Sender: TObject);
    procedure btnImportImageClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnSavePaletteClick(Sender: TObject);
    procedure btnSaveSpriteClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
//    procedure btnSaveTextClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure MainStatusBarDrawPanel(StatusBar: TStatusBar;
     Panel: TStatusPanel; const Rect: TRect);
    procedure sb_OpenFileClick(Sender: TObject);
    procedure CloseSpriteFile;
    procedure LoadAFile(filename: String);
    procedure ArrangeSprites;
    procedure FormCreate(Sender: TObject);
    procedure MainSpriteAreaPaint(Sender: TObject);
    function validateFilename(f: String): String;
    procedure Image1DblClick(Sender: TObject);
    procedure sb_SaveClick(Sender: TObject);
//    procedure SaveAsPng(screen:TBitmap;AFileName:String;Mask:Boolean;BGColour:Cardinal);
    procedure CreateSpriteControl(sprite,x,y: Integer);
    procedure UpdateProgress(p: Integer);
    procedure TileCanvas(c: TCanvas);
    procedure TileCanvas(c: TCanvas;rc: TRect); overload;
    function QueryUnsaved: Boolean;
  private
   image     : array of TImage;
   spritename: array of TLabel;
   select,
   wide,
   high      : Integer;
   sprload,
   arthuros,
   HasChanged: Boolean;
  public
   SpriteList: TSpriteFile;
   const
    AppTitle = 'Sprite Converter';
    AppVersion = '1.02';
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses BigImageUnit,AboutUnit,SettingsUnit;

procedure TMainForm.FormCreate(Sender: TObject);
begin
 Application.Title:=AppTitle;
end;

procedure TMainForm.MainSpriteAreaPaint(Sender: TObject);
begin
if Sender is TScrollBox then
 TileCanvas(TScrollBox(Sender).Canvas);
end;

procedure TMainForm.Image1DblClick(Sender: TObject);
var
 x: Integer;
 t: String;
begin
 if(Sender is TImage)or(Sender is TLabel)then
 begin
  //Set up the image
  if Sender is TImage then
   x:=TImage(Sender).Tag;
  if Sender is TLabel then
   x:=TLabel(Sender).Tag;
  BigImageForm.ZoomedImage.Picture.Clear;
  BigImageForm.ZoomedImage.Align:=alNone;
  BigImageForm.ZoomedImage.Width:=SpriteList.SpriteList[x].Image.Width;
  BigImageForm.ZoomedImage.Height:=SpriteList.SpriteList[x].Image.Height;
  BigImageForm.ZoomedImage.Canvas.AntialiasingMode:=amOff;
  BigImageForm.ZoomedImage.Stretch:=True;
  BigImageForm.ZoomedImage.Proportional:=True;
  BigImageForm.ZoomedImage.Picture.Bitmap.Assign(SpriteList.SpriteList[x].PNG);
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
  BigImageForm.BPPPanel.Top:=BigImageForm.SizePanel.Top+BigImageForm.SizePanel.Height;
  //Mask
  BigImageForm.lb_mask.Caption:=SpriteList.MaskFormat(x);
  BigImageForm.MaskPanel.Top:=BigImageForm.BPPPanel.Top+BigImageForm.BPPPanel.Height;
  //Palette
  BigImageForm.lb_palette.Caption:=SpriteList.PaletteType(x);
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

procedure TMainForm.CloseSpriteFile;
var
 x: Integer;
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
 SpriteList.CloseCurrentFile;
 HasChanged:=False;
end;

procedure TMainForm.LoadAFile(filename: String);
var
  x  : Integer;
  ix : Cardinal;
  txt: String;
begin
 sprload:=True;
 select:=-1;
 //Disable the controls
 btnSaveAsBMPPNG.Enabled:=False;
 btnSaveAsBMP.Enabled   :=False;
 btnSaveAsPNG.Enabled   :=False;
 btnSaveSprite.Enabled  :=False;
 btnSavePalette.Enabled :=False;
 btnSaveText.Enabled    :=False;
 if SpriteList.ImportImage(filename,arthuros) then
 begin
  //Set up the arrays for the sprites
  SetLength(image,SpriteList.SpriteCount);
  SetLength(spritename,SpriteList.SpriteCount);
  if SpriteList.SpriteList[SpriteList.SpriteCount-1].Image<>nil then
   CreateSpriteControl(SpriteList.SpriteCount-1,4,4);
  ArrangeSprites;
  HasChanged:=True;
 end
 else
  if QueryUnsaved then
  begin
   CloseSpriteFile;
   ProgressInd.Visible:=True;
   SpriteList.ProgressIndicator:=@UpdateProgress;
   if SpriteList.LoadSpriteFile(filename)=0 then
   begin
    if SpriteList.SpriteCount>0 then
    begin
     //Set up the arrays for the sprites
     SetLength(image,SpriteList.SpriteCount);
     SetLength(spritename,SpriteList.SpriteCount);
     //Initial image position on the form
     ix:=4;
     wide:=0; //We need to work out the widest sprite
     high:=0; //And the tallest
     for x:=0 to SpriteList.SpriteCount-1 do
     begin
      //Display it
      if SpriteList.SpriteList[x].Image<>nil then
      begin
       CreateSpriteControl(x,ix,4);
       inc(ix,4);
      end;
      //Update the window
      ProgressInd.Position:=100+Round((x/SpriteList.SpriteCount)*100);
      if Round((x/SpriteList.SpriteCount)*100)mod 10=0 then
       Application.ProcessMessages;
     end;
    end;
   end
   else
    ShowMessage('Invalid Sprite File: "'+filename+'"');
   Application.ProcessMessages;
   ProgressInd.Position:=ProgressInd.Max;
   ProgressInd.Visible:=False;
   Application.ProcessMessages;
   ArrangeSprites;
  end;
 if SpriteList.SpriteCount>0 then
 begin
  //Display the filename
  txt:=IntToStr(SpriteList.SpriteCount)+' sprite';
  if SpriteList.SpriteCount>1 then
   txt:=txt+'s';
  //Enable the controls
  btnSaveText.Enabled    :=True;
  btnSaveAsBMPPNG.Enabled:=True;
  btnSaveAsBMP.Enabled   :=True;
  btnSaveAsPNG.Enabled   :=True;
  btnSaveSprite.Enabled  :=True;
  MainStatusBar.Panels[1].Text:=txt;
  MainStatusBar.Panels[2].Text:=SpriteList.SpriteFile;
  sprload:=False;
 end;
end;

procedure TMainForm.ArrangeSprites;
var
 s,
 x,y,
 max : Integer;
begin
 if Length(image)>0 then
 begin
  //The maximum width
  max:=image[0].Parent.ClientWidth;
  //Now we can arrange them nicely in columns and rows
  x:=12;
  y:=12;
  for s:=0 to Length(image)-1 do
  begin
   image[s].Visible:=True;
   spritename[s].Visible:=True;
   //Ensure we don't go too far to the right
   if x+wide>max then
   begin
    //So, next row
    x:=12;
    inc(y,high+20);
   end;
   //Position the image, top and centre
   image[s].Top:=y;
   image[s].Left:=x+((wide-image[s].Width)div 2);
   //Position the name, below and centre
   spritename[s].Top:=image[s].Top+image[s].Height+8;
   spritename[s].Left:=x+((wide-spritename[s].Width)div 2);
   //Next column
   inc(x,wide+12);
  end;
 end;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
begin
 LoadAFile(FileNames[0]);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
 if not sprload then ArrangeSprites;
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

procedure TMainForm.btnCloseFileClick(Sender: TObject);
begin
 If QueryUnSaved then CloseSpriteFile;
end;

procedure TMainForm.btnImportImageClick(Sender: TObject);
begin
 If OpenImageDialog.Execute then
  LoadAFile(OpenImageDialog.Filename);
end;

procedure TMainForm.btnOptionsClick(Sender: TObject);
begin
 SettingsForm.ArthurOption.Checked:=arthuros;
 SettingsForm.RO35Option.Checked:=Not arthuros;
 SettingsForm.ShowModal;
 if SettingsForm.ModalResult=mrOK then
 begin
  arthuros:=SettingsForm.ArthurOption.Checked;
  SetRegValB('ArthurCompatible',arthuros);
 end;
end;

procedure TMainForm.btnSavePaletteClick(Sender: TObject);
begin
 if(select>=0)and(select<SpriteList.SpriteCount)then
 begin
  SavePaletteFile.Filename:=SpriteList.SpriteFile+'.pal';
  if SavePaletteFile.Execute then
   SpriteList.SavePaletteFile(SavePaletteFile.Filename,select);
 end;
end;

procedure TMainForm.btnSaveSpriteClick(Sender: TObject);
begin
 SaveSpriteFile.Filename:=SpriteList.SpriteFile;
 if SaveSpriteFile.Execute then
 begin
  SpriteList.SaveSpriteFile(SaveSpriteFile.Filename);
  HasChanged:=False;
 end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 CanClose:=QueryUnsaved;
end;

{procedure TMainForm.btnSaveTextClick(Sender: TObject);
begin
 SaveTextFile.Filename:=SpriteList.SpriteFile+'.txt';
 if SaveTextFile.Execute then
  SpriteList.LogFile.SaveToFile(SaveTextFile.Filename);
end;}

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
 //Loading sprites flag
 sprload:=False;
 //Has Changed flag
 HasChanged:=False;
 //Get the user options
 arthuros:=GetRegValB('ArthurCompatible',False);
end;

procedure TMainForm.Image1Click(Sender: TObject);
var
 x: Integer;
begin
 if(Sender is TImage)or(Sender is TLabel)then
 begin
  if Sender is TImage then x:=TImage(Sender).Tag;
  if Sender is TLabel then x:=TLabel(Sender).Tag;
  if select>=0 then
  begin
   spritename[select].Color:=clNone;
   spritename[select].Font.Color:=clDefault;
  end;
  if x<>select then
  begin
   spritename[x].Color:=clBlue;
   spritename[x].Font.Color:=clWhite;
   select:=x;
  end
  else select:=-1;
  if select=-1 then btnSavePalette.Enabled:=False;
  if select>=0 then
   if SpriteList.SpriteList[select].HasPalette then
    btnSavePalette.Enabled:=True
   else
    btnSavePalette.Enabled:=False;
 end;
end;

procedure TMainForm.MainStatusBarDrawPanel(StatusBar: TStatusBar;
 Panel: TStatusPanel; const Rect: TRect);
var
 imgRect: TRect;
begin
 //Set up the rectangle for the image - giving it 2px border
 imgRect.Top:=Rect.Top+3;
 imgRect.Left:=Rect.Left+3;
 imgRect.Height:=Rect.Height-6;
 imgRect.Width:=imgRect.Height;
 //First panel - we want to put the 'not saved' indicator here
 if (Panel.Index=0) and (HasChanged) then
  StatusBarImages.StretchDraw(StatusBar.Canvas,0,imgRect);
end;

procedure TMainForm.sb_SaveClick(Sender: TObject);
var
 Dir   : String;
 x,
 svsrt,
 svend : Integer;
 png,
 bmp   : Boolean;
begin
 //Set as default
 png:=False;
 bmp:=False;
 //Work out which button was clicked to set the appropriate flag
 if Sender is TToolButton then
 begin
  if TToolButton(Sender).Name='btnSaveAsBMPPNG' then
  begin
   png:=True;
   bmp:=True;
  end;
  if TToolButton(Sender).Name='btnSaveAsBMP' then bmp:=True;
  if TToolButton(Sender).Name='btnSaveAsPNG' then png:=True;
 end;
 //Are there actually any sprites?
 if SpriteList.SpriteCount>0 then
 begin
  svsrt:=0;
  svend:=SpriteList.SpriteCount-1;
  //Open the Save As Dialog box to select a directory
  if SelectDirectoryDialog1.Execute then
  begin
   //Get the directory name
   Dir:=SelectDirectoryDialog1.FileName;
   //Make sure it has a path delimiter at the end
   if Dir[Length(dir)]<>PathDelim then Dir:=Dir+PathDelim;
   //Is there a sprite selected, then just save this one
   if select>=0 then
   begin
    svsrt:=select;
    svend :=select;
   end;
   for x:=svsrt to svend do
   begin
    //PNG
    if png then
     {SaveAsPng(SpriteList.SpriteList[x].Image,Dir+PathDelim
                        +validateFilename(SpriteList.SpriteList[x].Name)+'.png',
                        SpriteList.SpriteList[x].TransFormat>0,
                        SpriteList.SpriteList[x].BGColour);}
     SpriteList.SpriteList[x].PNG.SaveToFile(Dir+PathDelim
                       +validateFilename(SpriteList.SpriteList[x].Name)+'.png');
    //Save BMP
    if bmp then
     try
      SpriteList.SpriteList[x].Image.SaveToFile(Dir+PathDelim
                       +validateFilename(SpriteList.SpriteList[x].Name)+'.bmp');
     finally
     end;
   end;
  end;
 end;
end;

{procedure TMainForm.SaveAsPng(screen:TBitmap;AFileName:String;Mask:Boolean;
                                                             BGColour:Cardinal);
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
 try
  png.SaveToFile(AFileName);
 finally
 end;
 cnv.Free;
 img.Free;
 png.Free;
end;}

procedure TMainForm.CreateSpriteControl(sprite,x,y: Integer);
const
  imgsizex = 32;
  imgsizey = 32;
begin
 spritename[sprite]:=TLabel.Create(MainSpriteArea as TComponent);
 spritename[sprite].Parent:=MainSpriteArea as TWinControl;
 spritename[sprite].Top:=y+imgsizey;
 spritename[sprite].Left:=x;
 spritename[sprite].Visible:=True;
 spritename[sprite].Caption:=SpriteList.SpriteList[sprite].Name;
 spritename[sprite].Left:=x+(((imgsizex*2)-spritename[sprite].Width)div 2);
 spritename[sprite].OnDblClick:=@Image1DblClick;
 spritename[sprite].OnClick:=@Image1Click;
 spritename[sprite].Tag:=sprite;
 image[sprite]:=TImage.Create(MainSpriteArea as TComponent);
 image[sprite].Parent:=MainSpriteArea as TWinControl;
 //image[sprite].Width:=SpriteList.SpriteList[sprite].PNG.Width;
 //image[sprite].Height:=SpriteList.SpriteList[sprite].PNG.Height;
 image[sprite].OnDblClick:=@Image1DblClick;
 image[sprite].OnClick:=@Image1Click;
 image[sprite].Tag:=sprite;
 image[sprite].Visible:=True;
 //image[sprite].Canvas.AntialiasingMode:=amOff;
 image[sprite].Stretch:=True;
 image[sprite].Proportional:=True;
 image[sprite].Top:=y;
 image[sprite].Left:=x+(imgsizex div 2);
 {image[sprite].Canvas.Pen.Color:=MainForm.Color;
 image[sprite].Canvas.Brush.Color:=MainForm.Color;
 image[sprite].Canvas.Rectangle(0,0,image[sprite].Width,image[sprite].Height);}
 image[sprite].Picture.Bitmap.Assign(SpriteList.SpriteList[sprite].PNG);
 //image[sprite].Canvas.Draw(0,0,SpriteList.SpriteList[sprite].Image);
 image[sprite].Width:=imgsizex;
 image[sprite].Height:=imgsizey;
 image[sprite].Hint:=SpriteList.SpriteList[sprite].Name;
 if spritename[sprite].Width>wide then wide:=spritename[sprite].Width;
 if image[sprite].Width>wide then wide:=image[sprite].Width;
 //They're all the same height
 if high=0 then
  high:=((spritename[sprite].Top+spritename[sprite].Height)-image[sprite].Top);
 spritename[sprite].Visible:=False;
 image[sprite].Visible:=False;
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
 ProgressInd.Position:=p;
 Application.ProcessMessages;
end;

procedure TMainForm.TileCanvas(c: TCanvas);
var
 rc: TRect;
begin
 rc:=Rect(0,0,c.Width,c.Height);
 TileCanvas(c,rc);
end;
procedure TMainForm.TileCanvas(c: TCanvas;rc: TRect);
var
 b : TBrush;
begin
 b := Tbrush.Create;
 b.Bitmap := Texture.Picture.Bitmap;
 c.Brush := b;
 c.FillRect(rc);
 b.Free;
end;

{------------------------------------------------------------------------------}
//Query about unsaved changes
{------------------------------------------------------------------------------}
function TMainForm.QueryUnsaved: Boolean;
begin
 Result:=True;
 if HasChanged then
  Result:=MessageDlg('You have unsaved changes. Do you wish to continue?',
                mtInformation,[mbYes,mbNo],0)=mrYes;
end;

end.
