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
  Classes,Graphics,FPImage,IntfGraphics,ComCtrls, Menus,SpriteFile,Global, Types;

type

  { TMainForm }

  TMainForm = class(TForm)
   edRenameSprite: TEdit;
   AppMenu: TMainMenu;
   MainSpritePanel: TPanel;
   FileMenu: TMenuItem;
   menuClosePool: TMenuItem;
   menuImportImage: TMenuItem;
   menuDeleteSprite: TMenuItem;
   MenuItem1: TMenuItem;
   menuAbout: TMenuItem;
   contRenameSprite: TMenuItem;
   contDeleteSprite: TMenuItem;
   contSavePalette: TMenuItem;
   MenuItem3: TMenuItem;
   contConvertSprite: TMenuItem;
   menuConvertSprite: TMenuItem;
   menuOptions: TMenuItem;
   ContextMenu: TPopupMenu;
   btnConvertSprite: TToolButton;
   ToolMenu: TMenuItem;
   menuSavePalette: TMenuItem;
   menuSaveSprite: TMenuItem;
   menuSaveAsPNG: TMenuItem;
   menuSaveAsBMP: TMenuItem;
   menuSaveAsBMPPNG: TMenuItem;
   MenuItem2: TMenuItem;
   menuRenameSprite: TMenuItem;
   SpriteMenu: TMenuItem;
   menuOpenFile: TMenuItem;
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
   btnOptions: TToolButton;
   btnCloseFile: TToolButton;
   btnImportImage: TToolButton;
   ToolButton2: TToolButton;
   ToolButton3: TToolButton;
   btnRenameSprite: TToolButton;
   btnDeleteSprite: TToolButton;
   procedure btnAboutClick(Sender: TObject);
   procedure btnCloseFileClick(Sender: TObject);
   procedure btnConvertSpriteClick(Sender: TObject);
   procedure btnDeleteSpriteClick(Sender: TObject);
   procedure btnImportImageClick(Sender: TObject);
   procedure btnOptionsClick(Sender: TObject);
   procedure btnRenameSpriteClick(Sender: TObject);
   procedure btnSavePaletteClick(Sender: TObject);
   procedure btnSaveSpriteClick(Sender: TObject);
   procedure edRenameSpriteEditingDone(Sender: TObject);
   procedure edRenameSpriteKeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
   procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
   procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
   procedure DisableButtons;
   procedure FormResize(Sender: TObject);
   procedure FormShow(Sender: TObject);
   procedure Image1Click(Sender: TObject);
   procedure MainStatusBarDrawPanel(StatusBar: TStatusBar;
    Panel: TStatusPanel; const Rect: TRect);
   procedure sb_OpenFileClick(Sender: TObject);
   procedure CloseSpriteFile;
   procedure LoadAFile(filename: String);
   procedure ContextPopup(Sender: TObject; MousePos: TPoint;var Handled: Boolean);
   procedure UpdateStatusBar;
   procedure ArrangeSprites;
   procedure FormCreate(Sender: TObject);
   procedure MainSpriteAreaPaint(Sender: TObject);
   function validateFilename(f: String): String;
   procedure Image1DblClick(Sender: TObject);
   procedure sb_SaveClick(Sender: TObject);
   procedure CreateSpriteControl(sprite,x,y: Integer);
   procedure UpdateProgress(p: Integer);
   procedure TileCanvas(c: TCanvas);
   procedure TileCanvas(c: TCanvas;rc: TRect); overload;
   function QueryUnsaved: Boolean;
   function ConvertToKMG(size: Int64): String;
   private
    image     : array of TImage; //Containers for the images shown
    spritename: array of TLabel; //Containers for the name underneath
    spritepal : array of Boolean;//Whether it has a palette or not
    select,                      //Currently selected sprite
    editing,                     //Sprite currently being edited
    thumbnailwidth,              //Sprite thumbnail width
    thumbnailheight,             //Sprite thumbnail height
    wide,                        //Widest and
    tall      : Integer;         //Tallest sprite+name
    sprload,                     //Prevents re-arrangement of the sprites
    arthuros,                    //Arthur OS Compatible?
    HasChanged: Boolean;         //Has Changed flag
   public
    SpriteList: TSpriteFile;     //The container for the sprites
   const
    AppTitle = 'Sprite Converter';
    AppVersion = '1.04a';
   procedure AfterConstruction; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses BigImageUnit,AboutUnit,SettingsUnit;

{------------------------------------------------------------------------------}
//Rescale the form
{------------------------------------------------------------------------------}
procedure TMainForm.AfterConstruction;
var
 i: Integer;
begin
{
See: http://zarko-gajic.iz.hr/delphi-high-dpi-road-ensuring-your-ui-looks-correctly/
and https://wiki.lazarus.freepascal.org/High_DPI
}
 inherited;
 if Screen.PixelsPerInch<>96 then //as I’m designing at 96 DPI
 begin
  //Status Bar
  MainStatusBar.Height:=Round(MainStatusBar.Height*Screen.PixelsPerInch/96);
  MainStatusBar.Panels[0].Width:=MainStatusBar.Height;
  for i:=0 to -1+MainStatusBar.Panels.Count do
   MainStatusBar.Panels[i].Width:=Round(MainStatusBar.Panels[i].Width*Screen.PixelsPerInch/96);
  MainToolBar.ImagesWidth:=Round(MainToolBar.ImagesWidth*Screen.PixelsPerInch/96);
  //Can use TMonitor.PixelsPerInch to scale to a big monitor
 end;
end;

{------------------------------------------------------------------------------}
//Create the form
{------------------------------------------------------------------------------}
procedure TMainForm.FormCreate(Sender: TObject);
begin
 Application.Title:=AppTitle;
end;

{------------------------------------------------------------------------------}
//Tile various areas
{------------------------------------------------------------------------------}
procedure TMainForm.MainSpriteAreaPaint(Sender: TObject);
begin
if Sender is TScrollBox then
 TileCanvas(TScrollBox(Sender).Canvas);
if Sender is TPanel then
 TileCanvas(TPanel(Sender).Canvas);
end;

{------------------------------------------------------------------------------}
//Respond to a sprite image being double clicked
{------------------------------------------------------------------------------}
procedure TMainForm.Image1DblClick(Sender: TObject);
var
 x: Integer;
 t: String;
 sprite: TSprite;
begin
 x:=0;
 //Make sure it is a component we can deal with
 if(Sender is TImage)or(Sender is TLabel)then
 begin
  //Set up the image
  if Sender is TImage then
   x:=TImage(Sender).Tag;
  if Sender is TLabel then
   x:=TLabel(Sender).Tag;
  //Deselect any that are selected
  if select=x then Image1Click(image[x]);
  //Read the sprite
  sprite:=SpriteList.ReadSprite(x);
  //Now we can populate the big image in the dialogue
  BigImageForm.ZoomedImage.Picture.Clear;
  BigImageForm.ZoomedImage.Align:=alNone;
  BigImageForm.ZoomedImage.Width:=sprite.Image.Width;
  BigImageForm.ZoomedImage.Height:=sprite.Image.Height;
  BigImageForm.ZoomedImage.Canvas.AntialiasingMode:=amOff;
  BigImageForm.ZoomedImage.Stretch:=True;
  BigImageForm.ZoomedImage.Proportional:=True;
  BigImageForm.ZoomedImage.Picture.Bitmap.Assign(sprite.PNG);
  BigImageForm.ZoomedImage.Align:=alClient;
  BigImageForm.ZoomedImage.Tag:=x;
  //Sprite name in the dialogue's title bar
  BigImageForm.Caption:=sprite.Name;
  //Sprite information
  //Size
  BigImageForm.lb_size.Caption:=IntToStr(sprite.PixWidth)+'x'
                               +IntToStr(sprite.ScanLines+1);
  BigImageForm.SizePanel.Top:=0;
  //BPP
  BigImageForm.lb_bpp.Caption:=IntToStr(sprite.BPP)+'bpp';
  BigImageForm.BPPPanel.Top:=BigImageForm.SizePanel.Top+BigImageForm.SizePanel.Height;
  //Mask
  BigImageForm.lb_mask.Caption:=SpriteList.MaskFormat(x);
  BigImageForm.MaskPanel.Top:=BigImageForm.BPPPanel.Top+BigImageForm.BPPPanel.Height;
  //Palette
  BigImageForm.lb_palette.Caption:=SpriteList.PaletteType(x);
  BigImageForm.PalettePanel.Top:=BigImageForm.MaskPanel.Top+BigImageForm.MaskPanel.Height;
  //Colours
  if sprite.PaletteColours>0 then
  begin
   BigImageForm.ColoursPanel.Visible:=True;
   BigImageForm.lb_colours.Caption:=IntToStr(sprite.PaletteColours);
   BigImageForm.ColoursPanel.Top:=BigImageForm.ColoursPanel.Top+BigImageForm.BPPPanel.Height;
  end
  else BigImageForm.ColoursPanel.Visible:=False;
  //Sprite Type 
  BigImageForm.lb_spritetype.Caption:=SpriteList.SpriteType(x);
  BigImageForm.SpriteTypePanel.Top:=BigImageForm.ColoursPanel.Top+BigImageForm.ColoursPanel.Height;
  //Mode Data
  if sprite.ModeData>54 then
   BigImageForm.lb_mode.Caption:='0x'+IntToHex(sprite.ModeData,8)
  else
   BigImageForm.lb_mode.Caption:=IntToStr(sprite.ModeData);
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

{------------------------------------------------------------------------------}
//Open a new file, closing the old one first
{------------------------------------------------------------------------------}
procedure TMainForm.sb_OpenFileClick(Sender: TObject);
begin
 if QueryUnsaved then
 begin
  //Set the options - not multiselect
  OpenDialog.Options := [ofEnableSizing,ofViewDetail];
  if OpenDialog.Execute then
  begin
   //We're not appending to the current one, so close it
   CloseSpriteFile;
   LoadAFile(OpenDialog.Filename);
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Close a sprite file and reset everything
{------------------------------------------------------------------------------}
procedure TMainForm.CloseSpriteFile;
var
 x: Integer;
begin
 //Disable the controls
 DisableButtons;
 //Destroy any images that have been opened before
 if Length(image)>0 then
  for x:=0 to Length(image)-1 do
  begin
   image[x].Free;
   spritename[x].Free;
  end;
 //Empty the arrays
 SetLength(image,0);
 SetLength(spritename,0);
 SpriteList.CloseCurrentFile;
 //Reset Has Changed
 HasChanged:=False;
 //Clear the status bar
 MainStatusBar.Panels[1].Text:='';
 MainStatusBar.Panels[2].Text:='';
 MainStatusBar.Panels[3].Text:='';
 MainStatusBar.Repaint;
 //Reset the selectors
 select:=-1;
 editing:=-1;
end;

{------------------------------------------------------------------------------}
//Load a file in
{------------------------------------------------------------------------------}
procedure TMainForm.LoadAFile(filename: String);
var
  x,
  oldCount: Integer;
  ix      : Cardinal;
begin
 //If the window is re-sized while sprites are being imported, it can crash
 sprload:=True;
 //Reset the selected sprite to nothing
 select:=-1;
 editing:=-1;
 //Disable the controls
 DisableButtons;
 //Try and import it, if it is a 'foreign' image
 if SpriteList.ImportImage(filename,arthuros) then
 begin
  //Set up the arrays for the sprites
  SetLength(image,SpriteList.SpriteCount);
  SetLength(spritename,SpriteList.SpriteCount);
  SetLength(spritepal,SpriteList.SpriteCount);
  //Create the controls to display
  CreateSpriteControl(SpriteList.SpriteCount-1,4,4);
  //Arrange the sprites
  ArrangeSprites;
  //And mark as changed
  HasChanged:=True;
 end
 else //Not a 'foreign' image, so try for a sprite file
 begin
  //Make the progress bar show
  ProgressInd.Visible:=True;
  //And tell the class where to find it
  SpriteList.ProgressIndicator:=@UpdateProgress;
  //So we can keep track if anything has been added
  oldCount:=SpriteList.SpriteCount;
  //Now try and open a sprite file
  if SpriteList.LoadSpriteFile(filename)=0 then
  begin
   //If this is bigger than before, then something has been imported
   if SpriteList.SpriteCount>oldCount then
   begin
    //Set up or increase the arrays for the sprites
    SetLength(image,SpriteList.SpriteCount);
    SetLength(spritename,SpriteList.SpriteCount);
    SetLength(spritepal,SpriteList.SpriteCount);
    //Initial image position on the form - this will change
    ix:=4;
//    wide:=0; //We need to work out the widest sprite
//    tall:=0; //And the tallest
    for x:=oldCount to SpriteList.SpriteCount-1 do
    begin
     //Display it
     CreateSpriteControl(x,ix,4);
     inc(ix,4);
     //Update the window
     ProgressInd.Position:=100+Round((x/SpriteList.SpriteCount)*100);
     //We need to process the messages for this to show, but this also slows
     //down the progress, so we'll only do it every 10%
     if Round((x/SpriteList.SpriteCount)*100)mod 10=0 then
      Application.ProcessMessages;
    end;
    //If we have added some more sprites to an already open file, mark as changed
    if oldCount<>0 then HasChanged:=True;
   end;
  end
  else //We can't recognise the file, so report an error
   ShowMessage('Invalid Sprite File: "'+filename+'"');
  //Update the windows
  Application.ProcessMessages;
  ProgressInd.Position:=ProgressInd.Max;
  ProgressInd.Visible:=False;
  Application.ProcessMessages;
  //Now arrange the sprites so they look nice, and spaced out
  ArrangeSprites;
 end;
 //If something has been opened, then update the status bar and enable the toolbar buttons
 if SpriteList.SpriteCount>0 then
 begin
  //Enable the controls
  btnSaveAsBMPPNG.Enabled :=True;
  menuSaveAsBMPPNG.Enabled:=True;
  btnSaveAsBMP.Enabled    :=True;
  menuSaveAsBMP.Enabled   :=True;
  btnSaveAsPNG.Enabled    :=True;
  menuSaveAsPNG.Enabled   :=True;
  btnSaveSprite.Enabled   :=True;
  menuSaveSprite.Enabled  :=True;
  //Update the status bar
  UpdateStatusBar;
  //Allow re-arranging
  sprload:=False;
 end;
end;

{------------------------------------------------------------------------------}
//Middle button (two finger on mac touchpad) has been clicked
{------------------------------------------------------------------------------}
procedure TMainForm.ContextPopup(Sender: TObject; MousePos: TPoint;
 var Handled: Boolean);
begin
 Image1Click(Sender);
end;

{------------------------------------------------------------------------------}
//Update the status bar
{------------------------------------------------------------------------------}
procedure TMainForm.UpdateStatusBar;
var
 txt: String;
begin
 txt:=IntToStr(SpriteList.SpriteCount)+' sprite';
 //Plurise if necessary
 if SpriteList.SpriteCount>1 then
  txt:=txt+'s';
 MainStatusBar.Panels[1].Text:=txt;
 MainStatusBar.Panels[2].Text:=ConvertToKMG(SpriteList.LastFreeWord);
 MainStatusBar.Panels[3].Text:=SpriteList.SpriteFile;
 MainStatusBar.Repaint;
end;

{------------------------------------------------------------------------------}
//Arrange the sprites so they are evenly spaced out
{------------------------------------------------------------------------------}
procedure TMainForm.ArrangeSprites;
var
 s,
 x,y,
 max : Integer;
begin
 //Only if there is something to arrange
 if Length(image)>0 then
 begin
  //The maximum width
  max:=image[0].Parent.ClientWidth;
  //Now we can arrange them nicely in columns and rows
  x:=12;
  y:=12;
  s:=Length(image);
  for s:=0 to Length(image)-1 do
  begin
   //We need to make the components visible so that they show up dimensions
   image[s].Visible:=True;
   spritename[s].Visible:=True;
   //Ensure we don't go too far to the right
   if x+wide>max then
   begin
    //So, next row
    x:=12;
    inc(y,tall+20);
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
  MainSpritePanel.Height:=y+tall+20;
 end;
end;

{------------------------------------------------------------------------------}
//Accept files dropped onto the form
{------------------------------------------------------------------------------}
procedure TMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
var
 f: Integer;
begin
 for f:=0 to Length(FileNames)-1 do
  LoadAFile(FileNames[f]);
end;

{------------------------------------------------------------------------------}
//Resize the form
{------------------------------------------------------------------------------}
procedure TMainForm.FormResize(Sender: TObject);
begin
 MainSpritePanel.Top:=0;
 MainSpritePanel.Left:=0;
 MainSpritePanel.Width:=MainSpriteArea.ClientWidth;
 MainSpritePanel.Height:=MainSpriteArea.ClientHeight;
 //Only if the flag is not set
 if not sprload then ArrangeSprites;
end;

{------------------------------------------------------------------------------}
//Display the About dialogue
{------------------------------------------------------------------------------}
procedure TMainForm.btnAboutClick(Sender: TObject);
var
 TargetOS,
 TargetCPU : String;
begin
 //Determine the current platform (compile time directives)
 TargetOS:=LowerCase({$I %FPCTARGETOS%});
 if TargetOS='darwin'        then TargetOS:='macOS';
 if TargetOS='linux'         then TargetOS:='Linux';
 if Copy(TargetOS,1,3)='win' then TargetOS:='Windows';
 TargetCPU:=LowerCase({$I %FPCTARGETCPU%});
 if(TargetCPU='arm')
 or(TargetCPU='aarch64')then TargetCPU:='ARM';
 if TargetCPU='i386'    then TargetCPU:='32 bit';
 if TargetCPU='x86_64'  then TargetCPU:='64 bit';
 AboutForm.IconImage.Visible:=True;
 AboutForm.lb_Title.Caption:=AppTitle;
 AboutForm.lb_Version.Caption:='Version '+AppVersion+' '+TargetOS+' '+TargetCPU;
 AboutForm.ShowModal;
end;

{------------------------------------------------------------------------------}
//User is closing application, so check if the file is unsaved
{------------------------------------------------------------------------------}
procedure TMainForm.btnCloseFileClick(Sender: TObject);
begin
 If QueryUnSaved then CloseSpriteFile;
end;

{------------------------------------------------------------------------------}
//Convert a sprite
{------------------------------------------------------------------------------}
procedure TMainForm.btnConvertSpriteClick(Sender: TObject);
begin
 //Convert a sprite: colour depth; OS compatibility; mask type; mode data
end;

{------------------------------------------------------------------------------}
//Delete a sprite
{------------------------------------------------------------------------------}
procedure TMainForm.btnDeleteSpriteClick(Sender: TObject);
var
 x: Integer;
begin
 if select>=0 then
  if MessageDlg('Delete sprite'
               ,'Are you sure you want to delete sprite "'
                +spritename[select].Caption+'"?'
               ,mtConfirmation
               ,[mbYes,mbNo],0)=mrYes then
   if SpriteList.DeleteSprite(select) then
   begin
    //Delete the control
    if Length(image)>1 then //If more than one, otherwise we can just close the file
    begin
     //Is it not the last one?
     if select<Length(image)-2 then
     begin
      for x:=select to Length(image)-2 do
      begin
       image[x].Picture.Assign(image[x+1].Picture);
       spritename[x].Caption:=spritename[x+1].Caption;
       spritepal[x]:=spritepal[x+1];
      end;
     end;
     //Free up the contols
     image[Length(image)-1].Free;
     spritename[Length(spritename)-1].Free;
     //Reduce the array lengths
     SetLength(image,Length(image)-1);
     SetLength(spritename,Length(spritename)-1);
     SetLength(spritepal,Length(spritepal)-1);
     //Mark as changed
     HasChanged:=True;
     //Re-arrange the controls
     ArrangeSprites;
     //Update the status bar
     UpdateStatusBar;
     //Deselect the deleted sprite
     if select<Length(spritename) then
     begin
      spritename[select].Color:=clNone;
      spritename[select].Font.Color:=clDefault;
     end;
     select:=-1;
     editing:=-1;
    end
    else CloseSpriteFile;
   end;
end;

{------------------------------------------------------------------------------}
//Load/import files to append to the current open one
{------------------------------------------------------------------------------}
procedure TMainForm.btnImportImageClick(Sender: TObject);
var
 x: Integer;
begin
 //Set the dialogue options
 OpenDialog.Options := [ofEnableSizing,ofViewDetail,ofAllowMultiSelect];
 //If user has clicked OK, open all selected
 If OpenDialog.Execute then
  for x:=0 to OpenDialog.Files.Count-1 do
   LoadAFile(OpenDialog.Files[x]);
end;

{------------------------------------------------------------------------------}
//Options dialogue box is being opened
{------------------------------------------------------------------------------}
procedure TMainForm.btnOptionsClick(Sender: TObject);
begin
 //Set the settings on the control
 SettingsForm.ArthurOption.Checked:=arthuros;
 SettingsForm.RO35Option.Checked:=Not arthuros;
 //Show it
 SettingsForm.ShowModal;
 //Now respond to any change, if OK is clicked
 if SettingsForm.ModalResult=mrOK then
 begin
  arthuros:=SettingsForm.ArthurOption.Checked;
  SetRegValB('ArthurCompatible',arthuros);
 end;
end;

{------------------------------------------------------------------------------}
//Rename a sprite - show the edit control
{------------------------------------------------------------------------------}
procedure TMainForm.btnRenameSpriteClick(Sender: TObject);
begin
 if(select>=0)and(select<SpriteList.SpriteCount)then
 begin
  editing:=select;
  //Make the edit box visible
  edRenameSprite.Visible:=True;
  //Position so it takes up the whole width
  edRenameSprite.Left:=(spritename[select].Left
                      +(spritename[select].Width div 2))-(wide div 2);
  edRenameSprite.Width:=wide;
  //And is the same place as the label
  edRenameSprite.Top :=spritename[select].Top;
  //Put the label's text into the edit box
  edRenameSprite.Text:=spritename[select].Caption;
  //Hide the label
  spritename[select].Visible:=False;
  //And give the edit box focus
  edRenameSprite.SetFocus;
 end;
end;

{------------------------------------------------------------------------------}
//Save the palette of the current selected sprite
{------------------------------------------------------------------------------}
procedure TMainForm.btnSavePaletteClick(Sender: TObject);
begin
 //Only if something is selected and is within bounds
 if(select>=0)and(select<SpriteList.SpriteCount)then
 begin
  SavePaletteFile.Filename:=SpriteList.SpriteFile+'.pal';
  if SavePaletteFile.Execute then
   SpriteList.SavePaletteFile(SavePaletteFile.Filename,select);
 end;
end;

{------------------------------------------------------------------------------}
//Save the current sprite pool as a sprite file
{------------------------------------------------------------------------------}
procedure TMainForm.btnSaveSpriteClick(Sender: TObject);
begin
 SaveSpriteFile.Filename:=SpriteList.SpriteFile;
 if SaveSpriteFile.Execute then
 begin
  SpriteList.SaveSpriteFile(SaveSpriteFile.Filename);
  HasChanged:=False;
  MainStatusBar.Repaint;
 end;
end;

{------------------------------------------------------------------------------}
//User has done editing the sprite
{------------------------------------------------------------------------------}
procedure TMainForm.edRenameSpriteEditingDone(Sender: TObject);
begin
 if editing>=0 then
 begin
  edRenameSprite.Visible:=False;
  spritename[editing].Visible:=True;
  editing:=-1;
 end;
end;

{------------------------------------------------------------------------------}
//Query if we can close the form
{------------------------------------------------------------------------------}
procedure TMainForm.edRenameSpriteKeyUp(Sender: TObject; var Key: Word;
 Shift: TShiftState);
begin
 if editing>=0 then
 begin
  //Check for enter being pressed
  if Key=13 then
  begin
   edRenameSpriteEditingDone(Sender);
   if SpriteList.RenameSprite(spritename[editing].Caption,edRenameSprite.Text) then
   begin
    spritename[editing].Caption:=edRenameSprite.Text;
    HasChanged:=True;
    ArrangeSprites;
    editing:=-1;
   end;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Query if we can close the form
{------------------------------------------------------------------------------}
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 CanClose:=QueryUnsaved;
end;

{------------------------------------------------------------------------------}
//Disable the toolbar buttons
{------------------------------------------------------------------------------}
procedure TMainForm.DisableButtons;
begin
 btnSaveAsBMPPNG.Enabled:=False;
 btnSaveAsBMP.Enabled   :=False;
 btnSaveAsPNG.Enabled   :=False;
 btnSaveSprite.Enabled  :=False;
 btnSavePalette.Enabled :=False;
 btnRenameSprite.Enabled:=False;
 btnDeleteSprite.Enabled:=False;
 menuSaveAsBMPPNG.Enabled:=False;
 menuSaveAsBMP.Enabled   :=False;
 menuSaveAsPNG.Enabled   :=False;
 menuSaveSprite.Enabled  :=False;
 menuSavePalette.Enabled :=False;
 menuRenameSprite.Enabled:=False;
 menuDeleteSprite.Enabled:=False;
 contSavePalette.Enabled :=False;
 contRenameSprite.Enabled:=False;
 contDeleteSprite.Enabled:=False;
end;

{------------------------------------------------------------------------------}
//Show the form - and set up the application
{------------------------------------------------------------------------------}
procedure TMainForm.FormShow(Sender: TObject);
begin
 Caption:=AppTitle+' '+AppVersion+' by Gerald J Holdsworth';
 SpriteList:=TSpriteFile.Create;
 //Disable the controls
 DisableButtons;
 //Loading sprites flag
 sprload:=False;
 //Has Changed flag
 HasChanged:=False;
 //Get the user options
 arthuros:=GetRegValB('ArthurCompatible',False);
 //Set the selector to nothing
 select:=-1;
 editing:=-1;
 //Sprite thumbnails
 thumbnailwidth :=Round(32*Screen.PixelsPerInch/96);
 thumbnailheight:=Round(32*Screen.PixelsPerInch/96);
 //widest and tallest
 wide:=Round(thumbnailwidth*2.5);
 tall:=Round(thumbnailheight*2);
end;

{------------------------------------------------------------------------------}
//Single click on a sprite
{------------------------------------------------------------------------------}
procedure TMainForm.Image1Click(Sender: TObject);
var
 x: Integer;
 edit: Boolean;
begin
 x:=-1;
 edit:=False;
 //Make sure it is an image or label
 if(Sender is TImage)or(Sender is TLabel)then
 begin
  //Get the sprite number
  if Sender is TImage then x:=TImage(Sender).Tag;
  if Sender is TLabel then
  begin
   x:=TLabel(Sender).Tag;
   edit:=True;
  end;
 end;
 //Just to be sure
 if(select>=0)and(not edit)then
 begin
  //Clear the current selected label
  spritename[select].Color:=clNone;
  spritename[select].Font.Color:=clDefault;
 end;
 //Now colour the background of the new one (unless it is the scroll box has been clicked).
 if(x<>select)and(x>=0)then
 begin
  spritename[x].Color:=clBlue;
  spritename[x].Font.Color:=clWhite;
  select:=x;
  btnSavePalette.Enabled  :=spritepal[select];
  menuSavePalette.Enabled :=spritepal[select];
  contSavePalette.Enabled :=spritepal[select];
  btnRenameSprite.Enabled :=True;
  btnDeleteSprite.Enabled :=True;
  menuRenameSprite.Enabled:=True;
  menuDeleteSprite.Enabled:=True;
  contRenameSprite.Enabled:=True;
  contDeleteSprite.Enabled:=True;
 end
 else
  if(x=select)and(x>=0)and(edit)then btnRenameSpriteClick(Sender)
  else
  begin
   select:=-1; //Or mark as deselected
   btnSavePalette.Enabled  :=False;
   btnRenameSprite.Enabled :=False;
   btnDeleteSprite.Enabled :=False;
   menuSavePalette.Enabled :=False;
   menuRenameSprite.Enabled:=False;
   menuDeleteSprite.Enabled:=False;
   contSavePalette.Enabled :=False;
   contRenameSprite.Enabled:=False;
   contDeleteSprite.Enabled:=False;
  end;
end;

{------------------------------------------------------------------------------}
//Redraw the status bar - currently only the Has Changed part
{------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------}
//Save the current selection, or all, as bitmap or PNG
{------------------------------------------------------------------------------}
procedure TMainForm.sb_SaveClick(Sender: TObject);
var
 Dir   : String;
 x,
 svsrt,
 svend : Integer;
 png,
 bmp   : Boolean;
 sprite: TSprite;
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
 if Sender is TMenuItem then
 begin
  if TMenuItem(Sender).Name='menuSaveAsBMPPNG' then
  begin
   png:=True;
   bmp:=True;
  end;
  if TMenuItem(Sender).Name='menuSaveAsBMP' then bmp:=True;
  if TMenuItem(Sender).Name='menuSaveAsPNG' then png:=True;
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
    sprite:=SpriteList.ReadSprite(x);
    //PNG
    if png then
     try
      sprite.PNG.SaveToFile(Dir+PathDelim+validateFilename(sprite.Name)+'.png');
     finally
     end;
    //Save BMP
    if bmp then
     try
      sprite.Image.SaveToFile(Dir+PathDelim+validateFilename(sprite.Name)+'.bmp');
     finally
     end;
   end;
  end;
 end;
end;

{------------------------------------------------------------------------------}
//Create the image and label controls for the specified sprite
{------------------------------------------------------------------------------}
procedure TMainForm.CreateSpriteControl(sprite,x,y: Integer);
var
  spritedata: TSprite;
begin
 //Read the specified sprite in
 spritedata:=SpriteList.ReadSprite(sprite);
 //We'll take note of whether it has a palette, to speed up access later on
 spritepal[sprite]:=spritedata.HasPalette;
 //Create and populate the sprite name label
 spritename[sprite]:=TLabel.Create(MainSpritePanel as TComponent);
 spritename[sprite].Parent:=MainSpritePanel as TWinControl;
 spritename[sprite].Top:=y+thumbnailheight;
 spritename[sprite].Left:=x;
 spritename[sprite].Visible:=True;
 spritename[sprite].Caption:=spritedata.Name;
 spritename[sprite].Left:=x+(((thumbnailwidth*2)-spritename[sprite].Width)div 2);
 spritename[sprite].OnDblClick:=@Image1DblClick;
 spritename[sprite].OnClick:=@Image1Click;
 spritename[sprite].OnContextPopup:=@ContextPopup;
 spritename[sprite].PopupMenu:=ContextMenu;
 spritename[sprite].Tag:=sprite; //For reference later
 //Create and populate the sprite image control
 image[sprite]:=TImage.Create(MainSpritePanel as TComponent);
 image[sprite].Parent:=MainSpritePanel as TWinControl;
 image[sprite].OnDblClick:=@Image1DblClick;
 image[sprite].OnClick:=@Image1Click;
 image[sprite].OnContextPopup:=@ContextPopup;
 image[sprite].PopupMenu:=ContextMenu;
 image[sprite].Tag:=sprite;
 image[sprite].Visible:=True;
 image[sprite].Stretch:=True;
 image[sprite].Proportional:=True;
 image[sprite].Top:=y;
 image[sprite].Left:=x+(thumbnailwidth div 2);
 image[sprite].Picture.Bitmap.Assign(spritedata.PNG);
 image[sprite].Width:=thumbnailwidth;
 image[sprite].Height:=thumbnailheight;
 image[sprite].Hint:=spritedata.Name;
 //Take note if it is the widest so far seen
{ if spritename[sprite].Width>wide then wide:=spritename[sprite].Width;
 if image[sprite].Width>wide then wide:=image[sprite].Width;}
 //They're all the same height
{ if tall=0 then
  tall:=((spritename[sprite].Top+spritename[sprite].Height)-image[sprite].Top);}
 //Hide them, until later
 spritename[sprite].Visible:=False;
 image[sprite].Visible:=False;
end;

{------------------------------------------------------------------------------}
//Validate a filename
{------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------}
//Update the progress bar
{------------------------------------------------------------------------------}
procedure TMainForm.UpdateProgress(p: Integer);
begin
 ProgressInd.Position:=p;
 Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}
//Tile the window and controls with the texture
{------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------}
//Converts a number into a string with trailing 'Bytes', 'KB', etc.
{------------------------------------------------------------------------------}
function TMainForm.ConvertToKMG(size: Int64): String;
var
 new_size_int : Int64; //Int64 will allow for huge sizes
 new_size_dec,
 level,
 multiplier   : Integer;
const
 sizes: array[1..6] of String = ('Bytes','KB','MB','GB','TB','EB');
begin
 //Default is in bytes
 Result:=IntToStr(size)+' '+sizes[1];
 //How far through the array
 level:=0;
 //Current multiplier - KB is 1024 bytes, but MB is 1000 KB...odd, I know!
 multiplier:=1024;
 //We just do this as the first thing we do is divide
 new_size_int:=size*multiplier;
 repeat //we could use a While Do loop and avoid the pre-multiplying above
  inc(level); //Next level, or entry into the array
  //There are 1000KB per MB, 1000MB per GB, etc. but 1024 bytes per KB, etc.
  if level>2 then multiplier:=1000;
  //Work out the remainder, as a decimal to three decimal places
  new_size_dec:=Round(((new_size_int mod multiplier)/multiplier)*1000);
  //Reduce the size to the next smaller multiple
  new_size_int:=new_size_int div multiplier;
  //and repeat until we are smaller than the current multiple, or out of choices
 until (new_size_int<multiplier) or (level=High(sizes));
 //If level is valid
 if level<=High(sizes) then
 begin
  //First the major, whole number, portion
  Result:=IntToStr(new_size_int);
  //Then the decimal portion, but only if there is one
  if new_size_dec>0 then
  begin
   //Add an extra zero, which will be removed
   Result:=Result+'.'+IntToStr(new_size_dec)+'0';
   //Now remove all the surplus zeros
   repeat
    Result:=Copy(Result,1,Length(Result)-1);
   until Result[Length(Result)]<>'0';
  end;
  //And finally return the result, with the unit
  Result:=Result+' '+sizes[level];
 end;
end;

end.
