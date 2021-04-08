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
    SavePaletteFile: TSaveDialog;
    SaveSpriteFile: TSaveDialog;
    SaveTextFile: TSaveDialog;
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
    procedure btnSavePaletteClick(Sender: TObject);
    procedure btnSaveSpriteClick(Sender: TObject);
    procedure btnSaveTextClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure sb_OpenFileClick(Sender: TObject);
    procedure LoadAFile(filename: String);
    procedure ArrangeSprites;
    procedure FormCreate(Sender: TObject);
    function validateFilename(f: String): String;
    procedure Image1DblClick(Sender: TObject);
    procedure sb_SaveClick(Sender: TObject);
    procedure SaveAsPng(screen:TBitmap;AFileName:String;Mask:Boolean;BGColour:Cardinal);
    procedure CreateSpriteControl(sprite,x,y: Integer);
    procedure UpdateProgress(p: Integer);
  private
   image     : array of TImage;
   spritename: array of TLabel;
   select    : Integer;
  public
   SpriteList: TSpriteFile;
   const
    AppTitle = 'Sprite Converter';
    AppVersion = '1.01';
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

procedure TMainForm.LoadAFile(filename: String);
var
  x  : Integer;
  ix : Cardinal;
  txt: String;
begin
 select:=-1;
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
 if SpriteList.LoadSpriteFile(filename)=0 then
 begin
  if Length(SpriteList.SpriteList)>0 then
  begin
   //Enable the controls
   btnSaveText.Enabled    :=True;
   btnSaveAsBMPPNG.Enabled:=True;
   btnSaveAsBMP.Enabled   :=True;
   btnSaveAsPNG.Enabled   :=True;
   //Display the filename
   txt:=IntToStr(Length(SpriteList.SpriteList))+' sprite';
   if Length(SpriteList.SpriteList)>1 then txt:=txt+'s';
   StatusBar1.Panels[0].Text:=txt;
   StatusBar1.Panels[1].Text:=filename;
   //Set up the arrays for the sprites
   SetLength(image,Length(SpriteList.SpriteList));
   SetLength(spritename,Length(SpriteList.SpriteList));
   //Initial image position on the form
   ix:=4;
   for x:=0 to Length(SpriteList.SpriteList)-1 do
   begin
    //Display it
    if SpriteList.SpriteList[x].Image<>nil then
    begin
     CreateSpriteControl(x,ix,4);
     inc(ix,4);
    end;
    //Update the window
    ProgressBar1.Position:=100+Round((x/Length(SpriteList.SpriteList))*100);
    if Round((x/Length(SpriteList.SpriteList))*100)mod 10=0 then
     Application.ProcessMessages;
   end;
  end;
 end
 else
  StatusBar1.Panels[0].Text:='Invalid Sprite File: "'+filename+'"';
 ProgressBar1.Position:=ProgressBar1.Max;
 ProgressBar1.Visible:=False;
 ArrangeSprites;
end;

procedure TMainForm.ArrangeSprites;
var
 s,
 x,y,
 max,
 wide,
 high : Integer;
begin
 if Length(image)>0 then
 begin
  //The maximum width
  max:=image[0].Parent.ClientWidth;
  wide:=0;
  //We'll first go through them to find the widest
  for s:=0 to Length(image)-1 do
  begin
   //Dimensions will only get reported when visible
   image[s].Visible:=True;
   spritename[s].Visible:=True;
   //Find the widest of the two
   if spritename[s].Width>wide then wide:=spritename[s].Width;
   if image[s].Width>wide then wide:=image[s].Width;
  end;
  //Height of each one (will be the same)
  high:=(spritename[0].Top+spritename[0].Height)-image[0].Top;
  //Now we can arrange them nicely in columns and rows
  x:=4;
  y:=4;
  inc(wide,4); //4px gap between sprites side to side
  inc(high,8); //8px gap between sprites top to bottom
  for s:=0 to Length(image)-1 do
  begin
   //Ensure we don't go too far to the right
   if x+wide>max then
   begin
    //So, next row
    x:=4;
    inc(y,high);
   end;
   //Position the image, top and centre
   image[s].Top:=y;
   image[s].Left:=x+((wide-image[s].Width)div 2);
   //Position the name, below and centre
   spritename[s].Top:=image[s].Top+image[s].Height+4;
   spritename[s].Left:=x+((wide-spritename[s].Width)div 2);
   //Next column
   inc(x,wide);
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
 ArrangeSprites;
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

procedure TMainForm.btnSavePaletteClick(Sender: TObject);
begin
 if(select>=0)and(select<Length(SpriteList.SpriteList))then
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
  SpriteList.SaveSpriteFile(SaveSpriteFile.Filename);
end;

procedure TMainForm.btnSaveTextClick(Sender: TObject);
begin
 SaveTextFile.Filename:=SpriteList.SpriteFile+'.txt';
 if SaveTextFile.Execute then
  SpriteList.LogFile.SaveToFile(SaveTextFile.Filename);
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
 if Length(SpriteList.SpriteList)>0 then
 begin
  svsrt:=0;
  svend:=Length(SpriteList.SpriteList)-1;
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
     SaveAsPng(SpriteList.SpriteList[x].Image,Dir+PathDelim
                        +validateFilename(SpriteList.SpriteList[x].Name)+'.png',
                        SpriteList.SpriteList[x].TransFormat>0,
                        SpriteList.SpriteList[x].BGColour);
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

procedure TMainForm.SaveAsPng(screen:TBitmap;AFileName:String;Mask:Boolean;
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
end;

procedure TMainForm.CreateSpriteControl(sprite,x,y: Integer);
const
  imgsizex = 32;
  imgsizey = 32;
begin
 spritename[sprite]:=TLabel.Create(Scrollbox1 as TComponent);
 spritename[sprite].Parent:=Scrollbox1 as TWinControl;
 spritename[sprite].Top:=y+imgsizey;
 spritename[sprite].Left:=x;
 spritename[sprite].Visible:=False;
 spritename[sprite].Caption:=SpriteList.SpriteList[sprite].Name;
 spritename[sprite].Left:=x+(((imgsizex*2)-spritename[sprite].Width)div 2);
 spritename[sprite].OnDblClick:=@Image1DblClick;
 spritename[sprite].OnClick:=@Image1Click;
 spritename[sprite].Tag:=sprite;
 image[sprite]:=TImage.Create(Scrollbox1 as TComponent);
 image[sprite].Parent:=Scrollbox1 as TWinControl;
 image[sprite].Width:=SpriteList.SpriteList[sprite].Image.Width;
 image[sprite].Height:=SpriteList.SpriteList[sprite].Image.Height;
 image[sprite].OnDblClick:=@Image1DblClick;
 image[sprite].OnClick:=@Image1Click;
 image[sprite].Tag:=sprite;
 image[sprite].Visible:=False;
 image[sprite].Canvas.AntialiasingMode:=amOff;
 image[sprite].Stretch:=True;
 image[sprite].Proportional:=True;
 image[sprite].Top:=y;
 image[sprite].Left:=x+(imgsizex div 2);
 image[sprite].Canvas.Pen.Color:=MainForm.Color;
 image[sprite].Canvas.Brush.Color:=MainForm.Color;
 image[sprite].Canvas.Rectangle(0,0,image[sprite].Width,image[sprite].Height);
 image[sprite].Canvas.Draw(0,0,SpriteList.SpriteList[sprite].Image);
 image[sprite].Width:=imgsizex;
 image[sprite].Height:=imgsizey;
 image[sprite].Hint:=SpriteList.SpriteList[sprite].Name;
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
