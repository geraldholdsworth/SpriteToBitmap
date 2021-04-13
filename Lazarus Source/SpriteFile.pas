unit SpriteFile;

{
TSpriteFile class written by Gerald Holdsworth
Class to load and convert RISC OS sprites into Windows Bitmap and PNG.

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
 Classes,SysUtils,Graphics,Math,Dialogs,FPImage,IntfGraphics,GraphType,StrUtils;

{$M+}

type
 TDynByteArray = array of Byte;
 TSpriteFile = class
 private
 type
  TSprite = record
   Name          : String;                   // Name of the sprite
   Offset,                                   // Offset of this sprite
   Next,                                     // Offset to next sprite
   WidthWord,                                // Width of sprite in words - 1
   ScanLines,                                // Height of sprite in scan lines -1
   LeftBit,                                  // Left hand wastage (zero in new format)
   RightBit,                                 // Right hand wastage
   ModeData,                                 // Raw mode data
   PixelData,                                // Offset to pixel data
   Transparency,                             // Offset to mask (or pixel data if none)
   PixWidth,                                 // Width of sprite in pixels (calculated)
   PaletteColours{,                           // Number of palette entries (calculated) <16bpp
   BGColour      }: Cardinal;                 // Mask colour
   HasPalette    : Boolean;                  // Does sprite have a palette?
//   ColoursUsed   : array of Integer;         // Tally of each colour used (calculated)
//   ColourCount   : Integer;                  // Keep track of how many colours are used (calculated)
   ModeFlag,                                 // Mode flag (calculated)
   SpriteType,                               // Sprite type (calculated)
   BPP,                                      // Bits per pixel (calculated)
//   BPPOriginal,                              // Original BPP
   OS,                                       // Risc OS compatibility
   PaletteType,                              // What type of palette? Yes (1), No (0) or Partial (2)
   TransFormat   : Byte;                     // Old (1), New (2), Wide (3) or no (0) mask format (calculated)
   Palette       : array of Byte;            // Palette (if not in file, loaded from resource or built from pixel data)
   Image         : TBitmap;                  // Actual bitmap image
   PNG           : TPortableNetworkGraphic;  // PNG of image
   Mask          : array of array of Byte;   // Transparency Mask
  end;
  TSprites = array of TSprite;
  //Provides feedback
  TProgressProc = procedure(Fupdate: Integer) of Object;
 private
  Fdata : array of Byte;       //The actual sprite file data
  FSpriteList  : TSprites;     //The sprites
  FSpriteFile  : String;       //Filename of the sprite file
  //FDiagnostic  : TStringList;  //Diagnostic info
  FProgress    : TProgressProc;//Used for feedback
  FSpriteCount,                //Number of sprites
  FFirstSprite,                //Pointer to first sprite
  FLastWord    : Cardinal;     //Pointer to last free word in file
  function ReadSpriteFile(var error: Byte): TSprites;
  function ReadSprite(var ptr: Cardinal): TSprite;
//  function bitmapHeader(sizex,sizey,bpp,cols:Integer;var bmp:array of Byte):Integer;
//  procedure ExpandBPP(var OldBPP: Byte;var buffer:TDynByteArray);
//  procedure AddToPalette(r,g,b: Byte;Sprite: TSprite);
  procedure UpdateProgress(Fupdate: Integer);
  function DecodeModeFlags(modeflag: Byte): String;
  function DecodeSpriteType(spritetype: Byte): String;
  function DecodeMaskType(transformat: Byte): String;
  function DecodeOS(os: Byte): String;
  function DecodePaletteType(pal: Byte): String;
  function IsValidImageFile(filename: String):Boolean;
  {$INCLUDE 'SpriteFilePalettes.pas'}
 published
  //Methods
  constructor Create;
  function LoadSpriteFile(Afilename: String):Byte;
  function SaveSpriteFile(Afilename: String):Boolean;
  function SavePaletteFile(Afilename: String;sprite: Integer):Boolean;
  function ModeFlag(spritenumber: Integer): String;
  function SpriteType(spritenumber: Integer): String;
  function MaskFormat(spritenumber: Integer): String;
  function OS(spritenumber: Integer): String;
  function PaletteType(spritenumber: Integer): String;
  function ImportImage(filename: String;arthur: Boolean=False): Boolean;
  function IsValidSpriteName(spritename: String): Boolean;
  procedure CloseCurrentFile;
  //Properties
  property SpriteList       : TSprites read FSpriteList;
  property SpriteFile       : String   read FSpriteFile;
//  property LogFile          : TStringList read FDiagnostic;
  property ProgressIndicator: TProgressProc write FProgress;
  property SpriteCount      : Cardinal read FSpriteCount;
  property FirstSprite      : Cardinal read FFirstSprite;
  property LastFreeWord     : Cardinal read FLastWord;
 public
  destructor Destroy; override;
 end;

implementation

function TSpriteFile.ReadSpriteFile(var error:Byte): TSprites;
var
  x{,sprites,
  amt,ctr    }: Integer;
  ptr,size{,
  endoffile  }: Cardinal;
//  ms         : TMemoryStream; //Is this actually used now?
{const
 BGColour = $FFFF00FF; }
begin
 //Set up the variables
 Result:=nil;
 SetLength(Result,0);
// FDiagnostic.Clear;
 error:=0;
 //Create the memory stream
 //ms:=TMemoryStream.Create;
 //Sprite file header ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 //Size of sprite area - not in file, but in Sprite Area
 size:=Length(Fdata)-4;
 Fdata[$00]:= size      MOD $100;
 Fdata[$01]:=(size>> 8) MOD $100;
 Fdata[$02]:=(size>>16) MOD $100;
 Fdata[$03]:=(size>>24) MOD $100;
 //Get the number of sprites
 FSpriteCount:=(Fdata[$07]<<24)
              +(Fdata[$06]<<16)
              +(Fdata[$05]<<8)
              + Fdata[$04];
// FDiagnostic.Add('Number of sprites in file: '+IntToStr(FSpriteCount));
 //Get the position of the first sprite - this will act as our pointer into the data
 ptr:=(Fdata[$0B]<<24)
     +(Fdata[$0A]<<16)
     +(Fdata[$09]<<8)
     + Fdata[$08];
 FFirstSprite:=ptr;
// FDiagnostic.Add('Pointer to first sprite  : 0x'+IntToHex(ptr,8));
 //Get the position of the last free word - this is the end of the file
 FLastWord:=(Fdata[$0F]<<24)
           +(Fdata[$0E]<<16)
           +(Fdata[$0D]<<8)
           + Fdata[$0C];
// FDiagnostic.Add('Pointer to last free word: 0x'+IntToHex(FLastWord,8));
 if (ptr>size) or (FLastWord>size+4) then //Invalid Sprite File
 begin
  SetLength(Result,0);
//  FDiagnostic.Clear;
//  FDiagnostic.Add('Invalid Sprite File - pointer to end of file or first sprite is bigger than file size.');
  error:=1;
  exit;
 end;
 //Set up the structure
 SetLength(Result,FSpriteCount);
 //Read in all the sprites +++++++++++++++++++++++++++++++++++++++++++++++++++++
 for x:=0 to FSpriteCount-1 do
 begin
  Result[x]:=ReadSprite(ptr);
  //ptr has been returned in the header, meaning there was an error
  if ptr<FFirstSprite then
  begin
   SetLength(Result,0);
   error:=ptr;
   exit;
  end;
  UpdateProgress(Round((x/FSpriteCount*100)));
 end;
 //Free up the memory stream
 //ms.Free;
end;

function TSpriteFile.ReadSprite(var ptr: Cardinal): TSprite;
var
 buffer     : array of Byte;
 maskbpp,
 r,g,b,a,
 swap,
 c,m,y,k,
//  y,u,v,
 BPPOriginal: Byte;
 sx,sy,
 bx,p,{p2,}t,
 t2,tp      : Cardinal;
 mask       : Boolean;
 img        : TLazIntfImage;
 col        : TFPColor;
const
 //BPP colour depth of the Arthur/RISC OS 2/RISC OS 3.1 modes
 modes: array[0..53] of Byte = (1,2,3,2,1,2,1,3,2,3,
                                4,2,3,4,3,4,3,3,1,2,
                                3,4,3,1,4,1,2,3,4,1,
                                2,3,4,1,2,3,4,1,2,3,
                                4,1,2,4,1,2,3,4,3,4,
                                1,2,3,4);
begin
 maskbpp:=0;
{  FDiagnostic.Add('-------------------------------------');
  FDiagnostic.Add('Memory Pointer           : '+IntToHex(ptr,8));
  FDiagnostic.Add('-------------------------------------');
  FDiagnostic.Add('Sprite                   : '+IntToStr(x)); }
  //Sprite header ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  Result.Offset:=ptr;
  //Pointer to next sprite
  Result.Next:=(Fdata[ptr+$03]<<24)
              +(Fdata[ptr+$02]<<16)
              +(Fdata[ptr+$01]<<8)
              + Fdata[ptr+$00];
//  FDiagnostic.Add('Pointer to next sprite   : 0x'+IntToHex(Result.Next,8));
  if ptr+Result.Next>FLastWord then //Invalid Sprite File
  begin
{   FDiagnostic.Clear;
   FDiagnostic.Add('Invalid Sprite File - pointer to next sprite is beyond the end of the file.');}
   ptr:=2;
   exit;
  end;
  //Sprite name
  Result.Name:='';
  for sx:=0 to 11 do
  begin
   t:=Fdata[ptr+$04+sx]AND$7F;//macOS doesn't like top bit set characters
   if(t>0)and(t<32)then t:=t OR $20;//Remove any control characters
   if t>0 then Result.Name:=Result.Name+chr(t);
  end;
  //Still no spritename? Can't save it to disc.
  if Result.Name='' then Result.Name:='Sprite'+IntToHex(ptr,6);
  //Remove any extraneous spaces from the beginning or end
  while(Result.Name[Length(Result.Name)]=' ')and(Length(Result.Name)>1)do
   Result.Name:=LeftStr(Result.Name,Length(Result.Name)-1);
  while(Result.Name[1]=' ')and(Length(Result.Name)>1)do
   Result.Name:=RightStr(Result.Name,Length(Result.Name)-1);
//  FDiagnostic.Add('Sprite Name              : '+Result.Name);
  //Width of sprite (in words) minus 1
  Result.WidthWord :=(Fdata[ptr+$13]<<24)
                    +(Fdata[ptr+$12]<<16)
                    +(Fdata[ptr+$11]<<8)
                    + Fdata[ptr+$10];
//  FDiagnostic.Add('Width in words           : '+IntToStr(Result.WidthWord+1));
  //Height of sprite (in scanlines) minus 1
  Result.ScanLines:=(Fdata[ptr+$17]<<24)
                   +(Fdata[ptr+$16]<<16)
                   +(Fdata[ptr+$15]<<8)
                   + Fdata[ptr+$14];
//  FDiagnostic.Add('Height in scan lines     : '+IntToStr(Result.ScanLines+1));
  //First bit used (aka left hand wastage)
  Result.LeftBit:=(Fdata[ptr+$1B]<<24)
                 +(Fdata[ptr+$1A]<<16)
                 +(Fdata[ptr+$19]<<8)
                 + Fdata[ptr+$18];
//  FDiagnostic.Add('Left bit                 : 0x'+IntToHex(Result.LeftBit,8));
  //Last bit used (aka right hand wastage)
  Result.RightBit :=(Fdata[ptr+$1F]<<24)
                   +(Fdata[ptr+$1E]<<16)
                   +(Fdata[ptr+$1D]<<8)
                   + Fdata[ptr+$1C];
//  FDiagnostic.Add('Right bit                : 0x'+IntToHex(Result.RightBit,8));
  //Pointer to first pixel of sprite
  Result.PixelData:=(Fdata[ptr+$23]<<24)
                   +(Fdata[ptr+$22]<<16)
                   +(Fdata[ptr+$21]<<8)
                   +(Fdata[ptr+$20]);
//  FDiagnostic.Add('Pixel Data offset        : 0x'+IntToHex(Result.PixelData,8));
  //Pointer to transparent mask
  Result.Transparency:=(Fdata[ptr+$27]<<24)
                      +(Fdata[ptr+$26]<<16)
                      +(Fdata[ptr+$25]<<8)
                      +(Fdata[ptr+$24]);
//  FDiagnostic.Add('Transparent Data offset  : 0x'+IntToHex(Result.Transparency,8));
  //Mode data
  Result.ModeData:= (Fdata[ptr+$2B]<<24)
                   +(Fdata[ptr+$2A]<<16)
                   +(Fdata[ptr+$29]<<8)
                   + Fdata[ptr+$28];
//  FDiagnostic.Add('Mode data                : 0x'+IntToHex(Result.ModeData,8));
  //Header read in, now the calculations +++++++++++++++++++++++++++++++++++++++
//  FDiagnostic.Add('Calculations:');
  //Mode flag
  if Result.ModeData<128 then                //Old format
  begin
   //Not sprite type, but the actual screen mode, as old format
   Result.SpriteType:=modes[Result.ModeData mod 54];   //no modes 54+
   Result.ModeFlag:=0;
   Result.OS:=0;
  end
  else
   if (Result.ModeData>>27)AND$0F=$0F then   //RISC OS 5
   begin
    Result.SpriteType:=(Result.ModeData>>20)AND$7F;
    Result.ModeFlag:=(Result.ModeData>>8)AND$FF;
    Result.OS:=2;
   end
   else                                         //RISC OS 3.5
   begin
    Result.SpriteType:=(Result.ModeData>>27)AND$0F;
    Result.ModeFlag:=0;
    Result.OS:=1;
   end;
  //Bits per pixel
  Result.BPP:=0;
  case Result.SpriteType of
    0: ;//Invalid - used to id Arthur modes
    1: Result.BPP:= 1; //1bpp palletised
    2: Result.BPP:= 2; //2bpp palletised
    3: Result.BPP:= 4; //4bpp palletised
    4: Result.BPP:= 8; //8bpp palletised
    5: Result.BPP:=16; //16bpp 1:5:5:5 TBGR
    6: Result.BPP:=32; //32bpp 8:8:8:8 TBGR
    7: Result.BPP:=32; //CMYK
    8: ;//24bpp
    9: ;//JPEG
   10: Result.BPP:=16; //16bpp 5:6:5 TBGR
   15: ;//Invalid - used for id RISC OS 5 sprite mode words
   16: Result.BPP:=16; //16bpp 4:4:4:4
   17: ;//4:2:0 YCbCr
   18: ;//4:2:4 YCbCr
   //11-14 and 19-127 Reserved
  end;
  {Result.}BPPOriginal:=Result.BPP;
  if Result.BPP=2 then Result.BPP:=4; //Bitmaps don't have 2bpp
//  FDiagnostic.Add('Sprite Type              : '+IntToStr(Result.SpriteType));
//  FDiagnostic.Add('Sprite Type meaning      : '+DecodeSpriteType(Result.SpriteType));
  if Result.SpriteType=7 then Result.OS:=3;
  //Expand on RISC OS 5 details
{  if Result.OS=2 then
  begin
   FDiagnostic.Add('Mode Flags               : 0x'+IntToHex(Result.ModeFlag,2));
   FDiagnostic.Add('Mode Flags meaning       : '+DecodeModeFlags(Result.ModeFlag));
  end;
  FDiagnostic.Add('Bits per pixel           : '+IntToStr(Result.BPPOriginal));
  FDiagnostic.Add('Compatibility            : '+DecodeOS(Result.OS));}
  //Pixel Width
  if {Result.}BPPOriginal>0 then //BPP of 0 means that we can't handle it, currently
  begin
   Result.PixWidth:=((Result.WidthWord*32)
                        +Result.RightBit+1
                        -Result.LeftBit)
                        div {Result.}BPPOriginal;
//   FDiagnostic.Add('Width in pixels          : '+IntToStr(Result.PixWidth));
   //Now we read in the data +++++++++++++++++++++++++++++++++++++++++++++++++++
   //Have we a palette?
   if Result.PixelData>Result.Transparency then
    p:=Result.Transparency
   else
    p:=Result.PixelData;
   //By default
   Result.PaletteType:=0;
   Result.HasPalette:=False;
   Result.PaletteColours:=0;
   if {Result.}BPPOriginal<16 then //sprites with bpp of 16 or more have no palette
   begin
    if p>$2C then //Yes, palette is in the file
    begin
     Result.PaletteType:=1;
     Result.HasPalette:=True;
     //Number of entries in palette
     Result.PaletteColours:=(p-$2C) div 8;
     //Partial palette? 8bpp only
     if({Result.}BPPOriginal=8)AND(Result.PaletteColours<256)then
      Result.PaletteType:=2;
    end;
    //Assign enough memory for the palette
    SetLength(Result.Palette,(1<<{Result.}BPPOriginal)*4);
    //We don't have a palette in file, or it is partial
    if(not Result.HasPalette)or(Result.PaletteType=2)then
    begin
     //Use standard palette (constants in file SpriteFilePalettes.pas)
     SetLength(buffer,0);
     case {Result.}BPPOriginal of
      1: buffer:=ColourPalette2;    //2 colour palette
      2: buffer:=ColourPalette4;    //4 colour palette
      4: buffer:=ColourPalette16;   //16 colour palette
      8: buffer:=ColourPalette256;  //256 colour palette
     end;
     //Extract the palette entries, discarding the VDU19,cn,16
     for t:=0 to (1<<{Result.}BPPOriginal)-1 do
     begin
      Result.Palette[ t*4   ]:=buffer[(t*6)+5];//Blue
      Result.Palette[(t*4)+1]:=buffer[(t*6)+4];//Green
      Result.Palette[(t*4)+2]:=buffer[(t*6)+3];//Red
      Result.Palette[(t*4)+3]:=$00;            //Alpha
     end;
    end;
    //We do have a palette in file - if partial, we will overwrite the standard one
    if Result.HasPalette then
    begin
     sx:=0;  //Pointer into our palette
     t:=$2C; //Pointer into the data
     repeat
      Result.Palette[sx  ]:=Fdata[ptr+t+3]; //Blue
      Result.Palette[sx+1]:=Fdata[ptr+t+2]; //Green
      Result.Palette[sx+2]:=Fdata[ptr+t+1]; //Red
      Result.Palette[sx+3]:=$00;           //Alpha (0x00)
      inc(sx,4);
      inc(t,8);
     until sx>=Result.PaletteColours*4; //t>=p-1;
    end;
   end;
//   FDiagnostic.Add('Palette                  : '+DecodePaletteType(Result.PaletteType));
//   FDiagnostic.Add('Number of palette colours: '+IntToStr(Result.PaletteColours));
   //Assign the background colour, which will be transparent
//   Result.BGColour:=BGColour;
   //Check to see that our BGColour is not used
{   if Length(Result.Palette)>0 then
   begin
    t:=0;
    //Go through the palette
    while(t<Length(Result.Palette))and(Result.BGColour<>0)do
    begin
     //Check each colour
     if (Result.Palette[ t*4   ]= Result.BGColour mod $100)//Red
     and(Result.Palette[(t*4)+1]=(Result.BGColour>>8) mod $100)//Green
     and(Result.Palette[(t*4)+2]=(Result.BGColour>>16) mod $100) then//Blue
     begin
      //If we have a match, start back at the beginning of the palette
      t:=0;
      //Move onto the next colour
      if Result.BGColour>$FF000000 then
      begin
       dec(Result.BGColour,$100);
       //If we are back at BGColour, we have failed, so reset to zero
       if Result.BGColour=BGColour then Result.BGColour:=0;
      end
      else
       //Loop round
       Result.BGColour:=$FFFFFFFF;
     end
     else inc(t,4); //Otherwise, check next colour
    end;
   end;}
   //Set up the mask array
   SetLength(Result.Mask,Result.PixWidth,Result.ScanLines+1);
   for sx:=0 to Length(Result.Mask)-1 do
    for sy:=0 to Length(Result.Mask[sx])-1 do
     Result.Mask[sx,sy]:=$00;
   //Is there a transparent mask?
   if Result.Transparency<>Result.PixelData then
   begin
//    FDiagnostic.Add('Transparency             : Yes');
//    FDiagnostic.Add('Background Colour        : 0x'+IntToHex(Result.BGColour,8));
    //We do, so read in transparent mask
    if Result.Transparency>Result.PixelData then
    begin //Transparency data is after pixel data
     t:=Result.Next-Result.Transparency; //Size of transparency
     t2:=Result.Transparency-Result.PixelData; //Size of pixel data
    end
    else
    begin //Transparency data is before pixel data
     t2:=Result.Next-Result.PixelData; //Size of Pixel Data
     t:=Result.PixelData-Result.Transparency; //Size of Transparency
    end;
    //Work out if it is old format or new format
    //If new format, the sizes will be different
    if t<t2 then
     //Look at bit 31 of the mode data - if set, it is a 'wide mask'.
     //This means that the mask will be 8bpp, whatever. Therefore, the size of
     //the pixel data and the mask data will be different.
     if Result.ModeData>>31=0 then
     begin
      Result.TransFormat:=2;
      maskbpp:=1; //Mask is 1bpp
     end
     else
     begin
      Result.TransFormat:=3;
      maskbpp:=8; //Mask is 8bpp Alpha
     end
    else //If the sizes are the same, it can still be a new mask
    begin//Old masks are used when an old style mode is specified
     if Result.ModeData>127 then
     begin
      Result.TransFormat:=2;
      maskbpp:=1; //Mask is 1bpp
     end
     else
     begin
      Result.TransFormat:=1;
      maskbpp:={Result.}BPPOriginal;
     end;
    end;
//    FDiagnostic.Add('Format of Transparency   : '+DecodeMaskType(Result.TransFormat));
   end
   else
   begin //No mask
    Result.TransFormat:=0;
//    FDiagnostic.Add('Transparency             : No');
   end;
{   if(Result.BGColour=0)and(Result.TransFormat>0)then
   begin //No mask - no free colour found
    Result.TransFormat:=0;
//    FDiagnostic.Add('Transparency             : No free colours to use');
   end;}
   //All information now gathered from the file, for this sprite, so now create
   //the image. We still need to read in the mask and sprite data.
   //
   //Create the bitmap container in the array
   Result.Image:=TBitmap.Create;
   Result.PNG:=TPortableNetworkGraphic.Create;
   Result.PNG.PixelFormat:=pf32bit;
{   //Setup buffer to create bitmap data in
   SetLength(buffer,$36);
   amt:=bitmapHeader(Result.PixWidth,
                     Result.ScanLines+1,
                     Result.BPP,
                     0,
                     buffer);
   SetLength(buffer,amt);
   for t:=$36 to amt-1 do buffer[t]:=0;}
   //And PNG container
   img:=TLazIntfImage.Create(0,0,[riqfRGB,riqfAlpha]);
   img.SetSize(Result.PixWidth,Result.ScanLines+1);
{   //Copy the palette across
   if Result.BPP<16 then
    for t:=0 to Length(Result.Palette)-1 do
     buffer[$36+t]:=Result.Palette[t];
   //Pointer to Pixel data in bitmap
   p2:=buffer[$0A]
     +(buffer[$0B]<<8)
     +(buffer[$0C]<<16)
     +(buffer[$0D]<<24);
   //Set and empty the Colours Used counter
   if Result.BPPOriginal<16 then
   begin
    //As we read the sprite data in, we will count the colours used
    SetLength(Result.ColoursUsed,1<<Result.BPPOriginal);
    for t:=0 to Length(Result.ColoursUsed)-1 do
     Result.ColoursUsed[t]:=0;
   end;}
   //Extract the sprite data +++++++++++++++++++++++++++++++++++++++++++++++++++
   for sy:=0 to Result.ScanLines do
   begin
    //Loops through each pixel
    for sx:=0 to Result.PixWidth-1 do
    begin
     col.Alpha:=$FF;
     col.Red:=0;
     col.Green:=0;
     col.Blue:=0;
     //We will read in the mask data first +++++++++++++++++++++++++++++++++++++
     if Result.TransFormat>0 then //Check we have a mask
      if sx<Length(Result.Mask) then //And the x and y are within bounds
       if sy<Length(Result.Mask[sx]) then
       begin
        //Pointer to mask pixel in sprite
        tp:=Result.Transparency+ptr+
            (((((Result.PixWidth*maskbpp)+7)div 8)+3)div 4)*4*sy+
            Floor(sx*(maskbpp/8))+
            (Result.LeftBit div 8);
        //Byte(s) containing the pixel
        t:=0;
        for bx:=0 to Ceil(maskbpp/8)-1 do t:=t+Fdata[tp+bx]<<(bx*8);
        //Take account of the left hand wastage
        t:=t>>(Result.LeftBit mod 8);
        mask:=False;
        //The PRM says zero is invisible, anything else is visible
        case maskbpp of
         1: mask:=t and($1<<(sx mod 8))=0;    //1bpp
         2: mask:=t and($3<<((sx mod 4)*2))=0;//2bpp
         4: mask:=t AND($F<<((sx mod 2)*4))=0;//4bpp
         8,                                   //8bpp
         16,                                  //16bpp
         24,                                  //24bpp
         32: mask:=t and $FF=0;               //32bpp
        end;
        if mask then Result.Mask[sx,sy]:=$FF else Result.Mask[sx,sy]:=$00;
        if Result.TransFormat=3 then //Wide mask, so this is the alpha value
         Result.Mask[sx,sy]:=t and $FF;//Result.Mask[sx,sy] XOR $FF;
        col.Alpha:=$FF-Result.Mask[sx,sy];
       end;
     //Pointer to pixel in sprite
     p:=Result.PixelData+ptr+
            (((((Result.PixWidth*{Result.}BPPOriginal)+7)div 8)+3)div 4)*4*sy+
            Floor(sx*({Result.}BPPOriginal/8))+
            (Result.LeftBit div 8);
     //Byte(s) containing the pixel
     t:=0;
     for bx:=0 to Ceil({Result.}BPPOriginal/8)-1 do t:=t+Fdata[p+bx]<<(bx*8);
     //Take account of the left hand wastage
     t:=t>>(Result.LeftBit mod 8);
{     //Pointer to pixel in bitmap
     amt:=p2+((Ceil((Result.PixWidth*Result.BPP)/32)*4)*(Result.ScanLines-sy))
            +Floor(sx*(Result.BPP/8));}
{     //Put it in the buffer, top down
     if amt>0 then
     begin}
      r:=0;
      g:=0;
      b:=0;
      a:=$FF;
      case {Result.}BPPOriginal of
       1:
       begin
{        buffer[amt]:=buffer[amt]OR(t and($1<<(sx mod 8)));
        inc(Result.ColoursUsed[(t>>(sx mod 8))AND 1]);}
        r:=Result.Palette[((t>>(sx mod 8))AND 1)*4+2];
        g:=Result.Palette[((t>>(sx mod 8))AND 1)*4+1];
        b:=Result.Palette[((t>>(sx mod 8))AND 1)*4+0];
       end;
       2:
       begin
        r:=Result.Palette[((t>>((sx mod 4)*2))AND 3)*4+2];
        g:=Result.Palette[((t>>((sx mod 4)*2))AND 3)*4+1];
        b:=Result.Palette[((t>>((sx mod 4)*2))AND 3)*4+0];
       end;
       4:
       begin
 {       //Expand 2bpp to 4bpp, swapping half nibbles
        if {Result.}BPPOriginal=2 then
        begin
         t:=(t>>((sx mod 4)*2))AND$3;//Isolate the pixel and shift it right
         t:=t<<((sx mod 2)*4);//Shift it left to make it 4bpp
        end;}
        r:=Result.Palette[((t>>((sx mod 2)*4))and$F)*4+2];
        g:=Result.Palette[((t>>((sx mod 2)*4))and$F)*4+1];
        b:=Result.Palette[((t>>((sx mod 2)*4))and$F)*4+0];
{        //Swap nibbles round
        t:=((t AND$F)<<4)OR((t AND$F0)>>4);
        buffer[amt]:=buffer[amt]OR(t and($F0>>((sx mod 2)*4)));
        inc(Result.ColoursUsed[(t>>((sx mod 2)*4))and$F]);}
       end;
       8:
       begin
{        buffer[amt]:=t;
        inc(Result.ColoursUsed[t]);}
        r:=Result.Palette[t*4+2];
        g:=Result.Palette[t*4+1];
        b:=Result.Palette[t*4+0];
       end;
       16:
       begin
        //1:5:5:5 - Sprite Type 5
        if Result.SpriteType=5 then
        begin
         b:=(t AND $7C00)>>7;
         g:=(t AND $3E0)>>2;
         r:=(t AND $1F)<<3;
         a:=(t AND $8000)>>8;
        end;
        //5:6:5 - Sprite Type 10
        if Result.SpriteType=10 then
        begin
         b:=(t AND $F800)>>8;
         g:=(t AND $7E0)>>3;
         r:=(t AND $1F)<<3;
         a:=0;
        end;
        //4:4:4:4 - Sprite Type 16
        if Result.SpriteType=16 then
        begin
         b:=(t AND $F000)>>8;
         g:=(t AND $F00)>>4;
         r:= t AND $F0;
         a:=(t AND $F)<<4;
        end;
       end;
       32:
       begin
        //BGR
        r:=t and $FF;
        g:=(t>>8)and$FF;
        b:=(t>>16)and$FF;
        a:=(t>>24)and$FF;
        if Result.TransFormat=3 then
         a:=Result.Mask[sx,sy]; //Alpha
{        //Write the values
        buffer[amt+0]:=b;
        buffer[amt+1]:=g;
        buffer[amt+2]:=r;
        buffer[amt+3]:=a;}
       end;
      end;
      //RGB
      if ((Result.ModeFlag>>6)AND$1=1)     //Modeflag bit 6 is set
      and((Result.ModeFlag>>4)AND$3=0)then //bits 4 and 5 are clear
      begin
       swap:=b;
       b:=r;
       r:=swap;
      end;
      //CMYK
      if(Result.SpriteType=7) //Sprite Type 7 (RISC OS SIX)
      or (((Result.ModeFlag>>6)AND$3=0) //Mode flag bits 6 & 7 clear
      and((Result.ModeFlag>>4)AND$3=1))then //Mode flag bit 4 set & bit 5 clear
      begin
       //CMYK is stored KKYYMMCC, so k is a, y is b, m is g and r is c
       c:=r;
       m:=g;
       y:=b;
       k:=a;
       //Convert CMYK to RGB
       r:=Round(255*(1-c/255)*(1-k/255));
       g:=Round(255*(1-m/255)*(1-k/255));
       b:=Round(255*(1-y/255)*(1-k/255));
       //And blank off the alpha
       a:=0;
      end;
      col.Red:=r;
      col.Green:=g;
      col.Blue:=b;
      //Alpha sprite? Only if bit 7 of the Modeflag is set, and bits 4 and 5 are not
      if (Result.TransFormat=0)
      and((Result.ModeFlag>>4)AND$3=0)
      and((Result.ModeFlag>>6)AND$2<>$2)then
       a:=$FF;
      //Old and New masks have been applied already
      if(Result.TransFormat=0)or(Result.TransFormat=3)then
       col.Alpha:=a;
      //TFPColor is 16 bit per colour
      col.Alpha:=col.Alpha or col.Alpha<<8;
      col.Red  :=col.Red   or col.Red<<8;
      col.Green:=col.Green or col.Green<<8;
      col.Blue :=col.Blue  or col.Blue<<8;
//     end;
     //Write the pixel for PNG
     img.Colors[sx,sy]:=col;
    end;
   end;
   Result.PNG.LoadFromIntfImage(img);
   img.Free;
{   //Count the number of colours used
   if Result.BPP<16 then
   begin
    Result.ColourCount:=0;
    for t:=0 to Length(Result.ColoursUsed)-1 do
     if Result.ColoursUsed[t]<>0 then
      inc(Result.ColourCount);
    FDiagnostic.Add('Number of colours used   : '+IntToStr(Result.ColourCount));
   end;
   //Apply the mask
   if(Result.TransFormat=1)or(Result.TransFormat=2)then
   begin
    ctr:=-1;
    //Change the pixel to the BGColour, depending on the BPP
    //Can only do this if there is a free colour in the palette
    if Result.BPP<9 then
    begin
     if Result.ColourCount<1<<Result.BPP then
     begin
      //Find a free colour
      repeat
       inc(ctr);
      until (Result.ColoursUsed[ctr]=0) or (ctr=1<<Result.BPP-1);
      //Have we found one?
      if Result.ColoursUsed[ctr]<>0 then ctr:=-1; //No - mark as not found
     end;
     //No free colours, so we'll need to expand the bpp to add the extra colour
     if ctr=-1 then
     begin
      ExpandBPP(Result.BPP,buffer);
      if Result.BPP<16 then ctr:=(1<<Result.BPPOriginal)+1;
     end;
     //If we have any spare colours, add the mask colour to the palette
     if ctr<>-1 then
     begin //TColor format is ABGR, whereas BMP is ARGB
      buffer[$38+(ctr*4)]:= Result.BGColour      mod $100;//R
      buffer[$37+(ctr*4)]:=(Result.BGColour>>8)  mod $100;//G
      buffer[$36+(ctr*4)]:=(Result.BGColour>>16) mod $100;//B
      buffer[$39+(ctr*4)]:=(Result.BGColour>>24) mod $100;//A
     end;
    end;
    //Overlay the mask, overwriting whatever pixels are there with BGColour
    if(ctr>=0)or(Result.BPP>8)then
     for sy:=0 to Result.ScanLines do
      for sx:=0 to Result.PixWidth-1 do
      begin
       //Is pixel transparent?
       if Result.Mask[sx,sy]=$FF then
       begin
        //Pointer to pixel data
        p2:=buffer[$0A]
          +(buffer[$0B]<<8)
          +(buffer[$0C]<<16)
          +(buffer[$0D]<<24);
        //Pointer to start of row in bitmap
        p2:=p2+((Ceil((Result.PixWidth*Result.BPP)/32)*4)*(Result.ScanLines-sy));
        //Put the mask pixel in
        case Result.BPP of
         4:
          if ctr>=0 then
          begin
           if sx mod 2=0 then
            buffer[p2+(sx div 2)]:=(buffer[p2+(sx div 2)]AND$0F)or(ctr<<4)
           else
            buffer[p2+(sx div 2)]:=(buffer[p2+(sx div 2)]AND$F0)or ctr;
          end;
         8: if ctr>=0 then buffer[p2+sx]:=ctr;
         16: //TColor format is ABGR, whereas BMP is ARGB
         begin
          buffer[p2+(sx*2)]  :=((Result.BGColour AND$F80000)>>19)   //B
                              +((Result.BGColour AND$3800)>>6);     //G
          buffer[p2+(sx*2)+1]:=((Result.BGColour AND$C000)>>14)     //G
                              +((Result.BGColour AND$F8)>>1)        //R
                              +((Result.BGColour AND$80000000)>>24);//A
         end;
         32:
         begin //TColor format is ABGR, whereas BMP is ARGB
          buffer[p2+(sx*4)+2]:= Result.BGColour mod $100;     //Red
          buffer[p2+(sx*4)+1]:=(Result.BGColour>>8) mod $100; //Green
          buffer[p2+(sx*4)+0]:=(Result.BGColour>>16) mod $100;//Blue
          //Not sure why, but the next line will make the entire image transparent
//          buffer[p2+(sx*4)+3]:=(Result.BGColour>>24) mod $100;//Alpha
         end;
        end;
       end;
     end;
   end;
   if Result.BPP<>Result.BPPOriginal then
    FDiagnostic.Add('New bits per pixel       : '+IntToStr(Result.BPP));
   //Set the bitmap to be transparent, if needed
   if(Result.TransFormat<>0){and(Result.BPP<>32)}then
   begin
    Result.Image.Transparent:=True;
    if Result.BPP=16 then
     Result.Image.TransparentColor:=Result.BGColour and $00FFFFF7
    else// if Result.BPP<>32 then
     Result.Image.TransparentColor:=Result.BGColour mod $1000000;
   end
   else
    Result.Image.Transparent:=False;
   //Load the buffer into the bitmap
   ms.Position:=0;
   ms.WriteBuffer(buffer[0],Length(buffer));
   ms.Position:=0;
   Result.Image.LoadFromStream(ms);}
   Result.Image.Assign(Result.PNG);
   Result.BPP:=BPPOriginal;
  end;
  //Move onto the next sprite
  ptr:=ptr+Result.Next;
end;

function TSpriteFile.DecodeModeFlags(modeflag: Byte): String;
begin
 Result:='';
 if modeflag AND $1=$1 then
  Result:=Result+' Full res interlace';
 if modeflag AND $2=$2 then
  Result:=Result+' Greyscale';
 case (modeflag>>4)AND$3 of //bits 4 & 5
  0: //RGB
   case modeflag>>6 of
    0: Result:=Result+' RGB - TBGR';//bits 6 & 7 clear
    1: Result:=Result+' RGB - TRGB';//bit 7 clear, 6 set
    2: Result:=Result+' RGB - ABGR';//bit 7 set, 6 clear
    3: Result:=Result+' RGB - ARGB';//bits 6 & 7 set
   end;
  1: //Misc
   case modeflag>>6 of
    0: Result:=Result+' KYMC';    //bits 6 & 7 clear
    1: Result:=Result+' Reserved';
    2: Result:=Result+' Reserved';
    3: Result:=Result+' Reserved';
   end;
  2:
   case modeflag>>6 of
    0: Result:=Result+' YCbCr - ITU-R BT.601 Full'; //bits 6 & 7 clear
    1: Result:=Result+' YCbCr - ITU-R BT.601 Video';//bit 7 clear, 6 set
    2: Result:=Result+' YCbCr - ITU-R BT.709 Full'; //bit 7 set, 6 clear
    3: Result:=Result+' YCbCr - ITU-R BT.709 Video';//bits 6 & 7 set
   end;
  3: Result:=Result+' Reserved';
 end;
 if Result[1]=' ' then Result:=Copy(Result,2);
end;

function TSpriteFile.DecodeSpriteType(spritetype: Byte): String;
const
 //Sprite type string
 ModeStr: array[0..18] of String = ('Arthur mode','1bpp','2bpp','4bpp','8bpp',
                                    '16bpp 1:5:5:5 TBGR','32bpp 8:8:8:8 TBGR',
                                    '32bpp CMYK','24bpp','JPEG data',
                                    '16bpp 5:6:5 TBGR','Reserved','Reserved',
                                    'Reserved','Reserved','RISC OS 5',
                                    '16bpp 4:4:4:4','4:2:0 YCbCr','4:2:2 YCbCr');
begin
 if spritetype<=High(ModeStr) then
  Result:=ModeStr[spritetype]
 else
  Result:='undefined';
end;

function TSpriteFile.DecodeMaskType(transformat: Byte): String;
const
 masktype : array[0..3] of String = ('None','Old','New','Alpha');
begin
 if transformat<=High(masktype) then
  Result:=masktype[transformat]
 else
  Result:='undefined';
end;

function TSpriteFile.DecodeOS(os: Byte): String;
const
 //OS Compatibility string
 OSstr  : array[0..3]  of String = ('Arthur','RISC OS 3.5','RISC OS 5.21','RISC OS SIX');
begin
 if os<=High(OSstr) then
  Result:=OSstr[os]
 else
  Result:='undefined';
end;

function TSpriteFile.DecodePaletteType(pal: Byte): String;
const
 //Palette Types
 palStr : array[0..2] of String = ('No','Yes','Yes - Partial');
begin
 if pal<=High(palStr) then
  Result:=palStr[pal]
 else
  Result:='undefined';
end;

{function TSpriteFile.bitmapHeader(sizex,sizey,bpp,cols:Integer;var bmp:array of Byte):Integer;
var
 rowwidth,pal,pxd,amt: Integer;
begin
 //If BPP is not supported in bitmap, change to the next highest that is
 while(bpp<>1)and(bpp<>4)and(bpp<>8)and(bpp<>16)and(bpp<>24)and(bpp<>32)do
  inc(bpp);
 //Work out number of colours, under 16bpp
 if bpp<16 then
  if(cols=0)or(cols>1<<bpp)then
   cols:=1<<bpp;
 {Work out the row width, with padding to 4 bytes}
 rowwidth:=Ceil((sizex*bpp)/32)*4;
 if bpp<16 then
  pal:=cols*4 {Size of palette - colours * 4}
 else
  pal:=0; {BPP of 16,24 and 32 do not have a palette}
 pxd:=rowwidth*sizey; {Size of pixel data}
 amt:=pxd+$36+pal;{Size of file}
 {File header}
 //0x00	2 bytes	'BM' Identifies it as a BMP
 bmp[$00]:=ord('B');
 bmp[$01]:=ord('M');
 //0x02	4 bytes	Size of the file in bytes
 bmp[$02]:=  amt                    AND $FF;
 bmp[$03]:=( amt      div     $100) AND $FF;
 bmp[$04]:=( amt      div   $10000) AND $FF;
 bmp[$05]:=( amt      div $1000000) AND $FF;
 //0x06 4 bytes Reserved
 bmp[$06]:=$00;
 bmp[$07]:=$00;
 bmp[$08]:=$00;
 bmp[$09]:=$00;
 //0x0A	4 bytes	Offset to pixel data
 bmp[$0A]:= ($36+pal)               AND $FF;
 bmp[$0B]:=(($36+pal) div     $100) AND $FF;
 bmp[$0C]:=(($36+pal) div   $10000) AND $FF;
 bmp[$0D]:=(($36+pal) div $1000000) AND $FF;
 {DIB Header}
 //0x0E	4 bytes	Size of DIB header (0x00000028, or 40 bytes)
 bmp[$0E]:=$28;
 bmp[$0F]:=$00;
 bmp[$10]:=$00;
 bmp[$11]:=$00;
 //0x0012	4	Bitmap width
 bmp[$12]:=  sizex               AND $FF;
 bmp[$13]:=( sizex div     $100) AND $FF;
 bmp[$14]:=( sizex div   $10000) AND $FF;
 bmp[$15]:=( sizex div $1000000) AND $FF;
 //0x16	4 bytes	Bitmap height
 bmp[$16]:=  sizey                  AND $FF;
 bmp[$17]:=( sizey    div     $100) AND $FF;
 bmp[$18]:=( sizey    div   $10000) AND $FF;
 bmp[$19]:=( sizey    div $1000000) AND $FF;
 //0x1A	2 bytes	Colour planes (should be 0x01)
 bmp[$1A]:=$01;
 bmp[$1B]:=$00;
 //0x1C	2 bytes	Colour depth/bits per pixel
 bmp[$1C]:= bpp           AND $FF;
 bmp[$1D]:=(bpp div $100) AND $FF;
 //0x1E 4 bytes Compression Method
 bmp[$1E]:=$00;
 bmp[$1F]:=$00;
 bmp[$20]:=$00;
 bmp[$21]:=$00;
 //0x22	4 bytes	Size of the raw bitmap data (i.e. [0x02]-[0x0A])
 bmp[$22]:=  pxd                    AND $FF;
 bmp[$23]:=( pxd      div     $100) AND $FF;
 bmp[$24]:=( pxd      div   $10000) AND $FF;
 bmp[$25]:=( pxd      div $1000000) AND $FF;
 //0x26	4 bytes	Horizontal resolution (pixel per metre)
 bmp[$26]:=$12;
 bmp[$27]:=$0B;
 bmp[$28]:=$00;
 bmp[$29]:=$00;
 //0x2A	4 bytes	Vertical resolution (pixel per metre)
 bmp[$2A]:=$12;
 bmp[$2B]:=$0B;
 bmp[$2C]:=$00;
 bmp[$2D]:=$00;
 //0x2E	4 bytes	Number of colours in the palette (0=2^[0x1C])
 bmp[$2E]:=  cols                   AND $FF;
 bmp[$2F]:=( cols     div     $100) AND $FF;
 bmp[$30]:=( cols     div   $10000) AND $FF;
 bmp[$31]:=( cols     div $1000000) AND $FF;
 //0x32 4 bytes Number of important colours
 bmp[$32]:=$00;
 bmp[$33]:=$00;
 bmp[$34]:=$00;
 bmp[$35]:=$00;
 //0x36 Palette data (if any)
 //Then follows raw pixel data
 Result:=amt;
end;}

{procedure TSpriteFile.ExpandBPP(var OldBPP:Byte;var buffer:TDynByteArray);
var
 newbmp: array of Byte;
 bpp   : Byte;
 sizex,
 sizey,
 amt,
 Oldpxd,
 pxd,
 x,y,
 OldPX,
 NewPX  :Integer;
begin
 //Move up
 bpp:=OldBPP+1;
 //Get the old sizex
 sizex:=buffer[$12]+buffer[$13]<<8+buffer[$14]<<16+buffer[$15]<<24;
 //Get the old sizey
 sizey:=buffer[$16]+buffer[$17]<<8+buffer[$18]<<16+buffer[$19]<<24;
 //Create a new header
 SetLength(newbmp,$36);
 amt:=bitmapHeader(sizex,sizey,bpp,0,newbmp);
 SetLength(newbmp,amt);
 //Get the new BPP
 bpp:=newbmp[$1C]+newbmp[$1D]<<8;
 //Old location of the pixel data
 Oldpxd:=buffer[$0A]+buffer[$0B]<<8+buffer[$0C]<<16+buffer[$0D]<<24;
 //New location of the pixel data
 pxd:=newbmp[$0A]+newbmp[$0B]<<8+newbmp[$0C]<<16+newbmp[$0D]<<24;
 //If old BPP and new BPP is less than 16, just copy the palette across
 if(OldBPP<16)and(bpp<16)then
  for amt:=$36 to pxd-1 do
   if amt<Oldpxd then
    newbmp[amt]:=buffer[amt]
   else
    newbmp[amt]:=0;
 //Go through the old pixel data and write it to the new location, upscaling it
 for y:=0 to sizey-1 do
  for x:=0 to sizex-1 do
  begin
   //Pointer to the old pixel
   OldPX:=Oldpxd+((Ceil((sizex*OldBPP)/32)*4)*y)+Floor(x*(OldBPP/8));
   //Pointer to the new pixel
   NewPX:=pxd+((Ceil((sizex*bpp)/32)*4)*y)+Floor(x*(bpp/8));
   //Read the pixel
   amt:=buffer[OldPX];
   if OldBPP=1 then amt:=(amt>>(x mod 8))and 1;
   if OldBPP=4 then
   begin
    if x mod 2=0 then amt:=amt>>4;
    if x mod 2=1 then amt:=amt and $F;
   end;
   //Chances are we are going from 1 to 4, 4 to 8, or 8 to 16.
   //Unlikely to be going 16 to 24 or 24 to 32.
   //16bpp is ARGB
   //Write the pixel
   if bpp=4 then
   begin
    if x mod 2=0 then newbmp[NewPX]:=(newbmp[NewPX]AND$0F)OR(amt<<4);
    if x mod 2=1 then newbmp[NewPX]:=(newbmp[NewPX]AND$F0)OR amt;
   end;
   if bpp=8 then newbmp[NewPX]:=amt;
   if bpp=16 then
   begin //BMP 16bpp are stored 1:5:5:5
    newbmp[NewPX]  :=(newbmp[NewPX  ]AND$E0)
                  OR((buffer[$36+amt*4]AND$F8)>>3);//Blue bits 0-7 to 0-4
    newbmp[NewPX]  :=(newbmp[NewPX  ]AND$1F)
                  OR((buffer[$37+amt*4]AND$38)<<2);//Green bits 8-15 to 5-9
    newbmp[NewPX+1]:=(newbmp[NewPX+1]AND$FC)
                  OR((buffer[$37+amt*4]AND$E0)>>6);//Green bits 8-15 to 5-9
    newbmp[NewPX+1]:=(newbmp[NewPX+1]AND$03)
                  OR((buffer[$38+amt*4]and$F8)>>1);//Red bits 16-23 to 10-14
    newbmp[NewPX+1]:=(newbmp[NewPX+1]AND$7F)
                  OR (buffer[$39+amt*4]and$80);    //Alpha bits 24-31 to 15
   end;
  end;
 //Return the resultant bpp
 OldBPP:=bpp;
 //And the new bitmap
 SetLength(buffer,Length(newbmp));
 for amt:=0 to Length(newbmp)-1 do
  buffer[amt]:=newbmp[amt];
end;}

{procedure TSpriteFile.AddToPalette(r,g,b: Byte;Sprite: TSprite);
var
 ctr,found: Integer;
begin
 found:=-1;
 if Length(Sprite.Palette)>0 then
  for ctr:=0 to (Length(Sprite.Palette)div 3)-1 do
   if  (Sprite.Palette[ ctr*3   ]=r)
   and (Sprite.Palette[(ctr*3)+1]=g)
   and (Sprite.Palette[(ctr*3)+2]=b) then
    found:=ctr;
 if found=-1 then
 begin
  SetLength(Sprite.Palette,Length(Sprite.Palette)+3);
  ctr:=Length(Sprite.Palette)-3;
  Sprite.Palette[ctr  ]:=r;
  Sprite.Palette[ctr+1]:=g;
  Sprite.Palette[ctr+2]:=b;
 end;
end;}

procedure TSpriteFile.UpdateProgress(Fupdate: Integer);
begin
 //If the main program has defined a procedure then call it
 if Assigned(FProgress) then FProgress(Fupdate);
end;

function TSpriteFile.IsValidImageFile(filename: String):Boolean;
var
 buffer : array of Byte;
 F      : TFileStream;
 bmp,
 png,
 jpg    : Boolean;
 j      : Integer;
 size   : Cardinal;
 const
  pngsig: array[0..$F] of Byte=
  ($89,$50,$4E,$47,$0D,$0A,$1A,$0A,$00,$00,$00,$0D,$49,$48,$44,$52);
begin
 buffer:=nil;
 png:=True;
 bmp:=False;
 jpg:=False;
 F:=TFileStream.Create(filename,fmOpenRead or fmShareDenyNone);
 size:=F.Size;
 SetLength(buffer,16);
 F.Read(buffer[0],16);
 F.Free;
 bmp:=(buffer[0]=ord('B'))and(buffer[1]=ord('M'))
       and(buffer[2]+buffer[3]<<8+buffer[4]<<16+buffer[5]<<24=size);
 for j:=0 to 15 do
  if buffer[j]<>pngsig[j] then png:=False;
 jpg:=(buffer[0]=$FF)and(buffer[1]=$D8);
 Result:=png or bmp or jpg;
end;

{Published methods ************************************************************}

constructor TSpriteFile.Create;
begin
 inherited Create;
 FSpriteFile:='';
 SetLength(FSpriteList,0);
// FDiagnostic:=TStringList.Create;
end;

function TSpriteFile.LoadSpriteFile(Afilename: String):Byte;
var
 F    : TFileStream;
 error: Byte;
begin
 error:=3;
 CloseCurrentFile;
 //Open the file and read the data in
 try
  F:=TFileStream.Create(Afilename,fmOpenRead or fmShareDenyNone);
  F.Position:=0;
  SetLength(Fdata,F.Size+4);
  F.Read(Fdata[$04],F.Size);
  F.Free;
  FSpriteFile:=Afilename;
  FSpriteList:=ReadSpriteFile(error);
 finally
  Result:=error;
 end;
end;

function TSpriteFile.SaveSpriteFile(Afilename: String):Boolean;
var
 F    : TFileStream;
begin
 if Length(Fdata)>4 then
 begin
  //Create the file and write the data out
  try
   F:=TFileStream.Create(Afilename,fmCreate or fmShareDenyNone);
   F.Write(Fdata[$04],Length(Fdata)-4);
   F.Free;
   Result:=True;
  except
   Result:=False;
  end;
 end;
end;

function TSpriteFile.SavePaletteFile(Afilename: String;sprite: Integer):Boolean;
var
 F     : TFileStream;
 buffer: array of Byte;
 x     : Integer;
begin
 if sprite<Length(FSpriteList) then
  if FSpriteList[sprite].HasPalette then
  begin
   //Create the data to save - num of colours * 6 bytes
   SetLength(buffer,(Length(FSpriteList[sprite].Palette)div 4)*6);
   for x:=0 to (Length(FSpriteList[sprite].Palette)div 4)-1 do
   begin
    buffer[x*6+0]:=19; //VDU19,col,16,R,G,B
    buffer[x*6+1]:=x;
    buffer[x*6+2]:=16;
    buffer[x*6+3]:=FSpriteList[sprite].Palette[x*4+2];//Red
    buffer[x*6+4]:=FSpriteList[sprite].Palette[x*4+1];//Green
    buffer[x*6+5]:=FSpriteList[sprite].Palette[x*4+0];//Blue
   end;
   //Create the file and write the data out
   try
    F:=TFileStream.Create(Afilename,fmCreate or fmShareDenyNone);
    F.Write(buffer[0],Length(buffer));
    F.Free;
    Result:=True;
   except
    Result:=False;
   end;
  end;
end;

destructor TSpriteFile.Destroy;
begin
 inherited;
 CloseCurrentFile;
end;

function TSpriteFile.ModeFlag(spritenumber: Integer): String;
begin
 Result:='';
 if(spritenumber>=0)and(spritenumber<Length(FSpriteList))then
  if FSpriteList[spritenumber].OS=2 then
   Result:=DecodeModeFlags(FSpriteList[spritenumber].ModeFlag);
end;

function TSpriteFile.SpriteType(spritenumber: Integer): String;
begin
 Result:=DecodeSpriteType(FSpriteList[spritenumber].SpriteType);
end;

function TSpriteFile.MaskFormat(spritenumber: Integer): String;
begin
 Result:=DecodeMaskType(FSpriteList[spritenumber].TransFormat);
end;

function TSpriteFile.OS(spritenumber: Integer): String;
begin
 Result:=DecodeOS(FSpriteList[spritenumber].OS);
end;

function TSpriteFile.PaletteType(spritenumber: Integer): String;
begin
 Result:=DecodePaletteType(FSpriteList[spritenumber].PaletteType);
end;

function TSpriteFile.ImportImage(filename: String;arthur: Boolean=False): Boolean;
var
 buffer  : array of Byte;
 img     : TLazIntfImage;
 col     : TFPColor;
 x,y,
 sprite,
 numcols : Integer;
 colours : array of String;
 mask,
 BPP,
 maskBPP : Byte;
 colsrc,
 newname : String;
 pixsize,
 ptr,p,t : Cardinal;
begin
 Result:=False;
 colours:=nil;
 if IsValidImageFile(filename) then
 begin
  numcols:=0;
  SetLength(colours,257);
  img:=TLazIntfImage.Create(0,0,[riqfRGB, riqfAlpha]);
  img.LoadFromFile(filename);
  SetLength(buffer,img.Height*img.Width*4);
  mask:=0;//No mask
  for y:=0 to img.Height-1 do
   for x:=0 to img.Width-1 do
   begin
    col:=img.Colors[x,y];
    buffer[(y*img.Width*4)+(x*4)+3]:=col.Alpha>>8;
    if(col.Alpha>>8=$00)and(mask=0)then mask:=1;// Binary mask
    if(col.Alpha>>8>$00)and(col.Alpha>>8<$FF)then mask:=2; //Alpha mask
    buffer[(y*img.Width*4)+(x*4)+2]:=col.Red  >>8;
    buffer[(y*img.Width*4)+(x*4)+1]:=col.Green>>8;
    buffer[(y*img.Width*4)+(x*4)+0]:=col.Blue >>8;
    if numcols<257 then
    begin
     colsrc:=IntToHex(col.Blue>>8,2)+IntToHex(col.Green>>8,2)+IntToHex(col.Red>>8,2);
     if not AnsiMatchStr(colsrc,colours) then
     begin
      colours[numcols]:=colsrc;
      inc(numcols);
     end;
    end;
   end;
  BPP:=0;
  repeat
   if BPP=0 then inc(BPP) else BPP:=BPP*2;
  until numcols<1<<BPP;
  if BPP=16 then BPP:=32;
  SetLength(FSpriteList,Length(FSpriteList)+1);
  inc(FSpriteCount);
  if FSpriteCount=1 then //First sprite?
  begin
   //Create the sprite file header
   FSpriteFile:='Untitled,ff9';
   FFirstSprite:=$10;
   FLastWord:=$10;
  end;
  sprite:=FSpriteCount-1;
  colsrc:=ExtractFilename(filename);
  colsrc:=LowerCase(LeftStr(colsrc,Length(colsrc)-Length(ExtractFileExt(filename))));
  colsrc:=LeftStr(colsrc,12);
  x:=0;
  newname:=colsrc;
  while not IsValidSpriteName(newname) do
  begin
   inc(x);
   newname:=RightStr(colsrc+IntToStr(x),12);
  end;
  FSpriteList[sprite].Name:=newname;
  FSpriteList[sprite].BPP:=BPP;
{  FSpriteList[sprite].PNG:=TPortableNetworkGraphic.Create;
  FSpriteList[sprite].PNG.LoadFromIntfImage(img);
  FSpriteList[sprite].Image:=TBitmap.Create;
  FSpriteList[sprite].Image.LoadFromIntfImage(img);}
  FSpriteList[sprite].PixWidth:=img.Width;
  FSpriteList[sprite].ScanLines:=img.Height-1;
  FSpriteList[sprite].LeftBit:=0;
  FSpriteList[sprite].WidthWord:=Ceil((img.Width*BPP)/32)-1;
  FSpriteList[sprite].RightBit:=(img.Width*BPP)-(FSpriteList[sprite].WidthWord*32)-1;
  pixsize:=((FSpriteList[sprite].WidthWord+1)*4)*img.Height;
  if BPP<16 then //Point to pixel data, after the palette
  begin
   FSpriteList[sprite].PixelData:=$2C+8*(1<<BPP); //Two copies of each colour
   FSpriteList[sprite].HasPalette:=True;
   FSpriteList[sprite].PaletteColours:=1<<BPP;
   FSpriteList[sprite].PaletteType:=1;
  end
  else
  begin
   FSpriteList[sprite].PixelData:=$2C;
   FSpriteList[sprite].HasPalette:=False;
   FSpriteList[sprite].PaletteColours:=0;
   FSpriteList[sprite].PaletteType:=0;
   //Arthur did not have 16 or 32 bpp sprites
   arthur:=False;
  end;
  if mask>0 then
  begin
   FSpriteList[sprite].Transparency:=FSpriteList[sprite].PixelData+pixsize;
   if mask=1 then
    maskBPP:=1
   else
    maskBPP:=8;
   //Old masks are same BPP as main sprite
   if arthur then maskBPP:=BPP;
   pixsize:=Ceil((img.Width*maskBPP)/32)*4*img.Height;
   SetLength(FSpriteList[sprite].Mask,img.Width,img.Height);
   for y:=0 to img.Height-1 do
    for x:=0 to img.Width-1 do
    begin
     //Mask is stored $00 for transparent up to $FF for full pixel
     FSpriteList[sprite].Mask[x,y]:=buffer[y*img.Width*4+(x*4)+3];
     //Arthur masks are binary, either $00 for transparent or $FF for pixel
     if arthur then
      if FSpriteList[sprite].Mask[x,y]>0 then
       FSpriteList[sprite].Mask[x,y]:=$FF;
    end;
  end
  else
  begin
   FSpriteList[sprite].Transparency:=FSpriteList[sprite].PixelData;
   maskBPP:=0;
  end;
  FSpriteList[sprite].Next:=FSpriteList[sprite].Transparency+pixsize;
  ptr:=FLastWord;
  inc(FLastWord,FSpriteList[sprite].Next);
  if mask>0 then
   FSpriteList[sprite].TransFormat:=mask+1
  else
   FSpriteList[sprite].TransFormat:=0;
  FSpriteList[sprite].ModeFlag:=0;
  case BPP of
   1 : FSpriteList[sprite].SpriteType:=1;
   2 : FSpriteList[sprite].SpriteType:=2;
   4 : FSpriteList[sprite].SpriteType:=3;
   8 : FSpriteList[sprite].SpriteType:=4;
   16: FSpriteList[sprite].SpriteType:=5;
   32: FSpriteList[sprite].SpriteType:=6;
  end;
  //For Arthur, use Mode 19 (2bpp), 20 (4bpp) or 21 (8bpp)
  if arthur then
  begin
   FSpriteList[sprite].OS:=0;//Arthur
   case BPP of
    1: FSpriteList[sprite].ModeData:=18;
    2: FSpriteList[sprite].ModeData:=19;
    4: FSpriteList[sprite].ModeData:=20;
    8: FSpriteList[sprite].ModeData:=21;
   end;
  end
  else
  begin
   FSpriteList[sprite].OS:=1;//RISC OS 3.50
   FSpriteList[sprite].ModeData:=((mask AND 2)<<30)                 //Wide mask?
                              OR(FSpriteList[sprite].SpriteType<<27)//Sprite Type
                              OR(180<<14)                           //V DPI
                              OR(180<<1)                            //H DPI
                              OR 1;                                 //Set
  end;
  if BPP<16 then
  begin
   SetLength(FSpriteList[sprite].Palette,(1<<BPP)*4);
   for x:=0 to Length(FSpriteList[sprite].Palette)-1 do
    FSpriteList[sprite].Palette[x]:=0;
   for x:=0 to numcols-1 do
   begin //BGRA
    p:=StrToInt('$'+colours[x]);
    FSpriteList[sprite].Palette[x*4+0]:=p>>16;
    FSpriteList[sprite].Palette[x*4+1]:=(p>>8)AND$FF;
    FSpriteList[sprite].Palette[x*4+2]:=p AND$FF;
    FSpriteList[sprite].Palette[x*4+3]:=$00;
   end;
  end;
  img.Free;
  SetLength(Fdata,FLastWord);
  for x:=ptr to FLastWord-1 do Fdata[x]:=0;
  //Sprite file header
  Fdata[$00]:=FLastWord AND $FF;
  Fdata[$01]:=(FLastWord>>8)AND$FF;
  Fdata[$02]:=(FLastWord>>16)AND$FF;
  Fdata[$03]:=(FLastWord>>24)AND$FF;
  Fdata[$04]:=FSpriteCount AND $FF;
  Fdata[$05]:=(FSpriteCount>>8)AND$FF;
  Fdata[$06]:=(FSpriteCount>>16)AND$FF;
  Fdata[$07]:=(FSpriteCount>>24)AND$FF;
  Fdata[$08]:=$10;
  Fdata[$09]:=$00;
  Fdata[$0A]:=$00;
  Fdata[$0B]:=$00;
  Fdata[$0C]:=FLastWord AND $FF;
  Fdata[$0D]:=(FLastWord>>8)AND$FF;
  Fdata[$0E]:=(FLastWord>>16)AND$FF;
  Fdata[$0F]:=(FLastWord>>24)AND$FF;
  //New sprite header
  Fdata[ptr+$00]:=FSpriteList[sprite].Next AND $FF;
  Fdata[ptr+$01]:=(FSpriteList[sprite].Next>>8)AND$FF;
  Fdata[ptr+$02]:=(FSpriteList[sprite].Next>>16)AND$FF;
  Fdata[ptr+$03]:=(FSpriteList[sprite].Next>>24)AND$FF;
  for x:=0 to 11 do
  begin
   y:=0;
   if x<Length(FSpriteList[sprite].Name) then
    y:=Ord(FSpriteList[sprite].Name[x+1]);
   Fdata[ptr+$04+x]:=y;
  end;
  Fdata[ptr+$10]:=FSpriteList[sprite].WidthWord AND $FF;
  Fdata[ptr+$11]:=(FSpriteList[sprite].WidthWord>>8)AND$FF;
  Fdata[ptr+$12]:=(FSpriteList[sprite].WidthWord>>16)AND$FF;
  Fdata[ptr+$13]:=(FSpriteList[sprite].WidthWord>>24)AND$FF;
  Fdata[ptr+$14]:=FSpriteList[sprite].ScanLines AND $FF;
  Fdata[ptr+$15]:=(FSpriteList[sprite].ScanLines>>8)AND$FF;
  Fdata[ptr+$16]:=(FSpriteList[sprite].ScanLines>>16)AND$FF;
  Fdata[ptr+$17]:=(FSpriteList[sprite].ScanLines>>24)AND$FF;
  Fdata[ptr+$18]:=FSpriteList[sprite].LeftBit AND $FF;
  Fdata[ptr+$19]:=(FSpriteList[sprite].LeftBit>>8)AND$FF;
  Fdata[ptr+$1A]:=(FSpriteList[sprite].LeftBit>>16)AND$FF;
  Fdata[ptr+$1B]:=(FSpriteList[sprite].LeftBit>>24)AND$FF;
  Fdata[ptr+$1C]:=FSpriteList[sprite].RightBit AND $FF;
  Fdata[ptr+$1D]:=(FSpriteList[sprite].RightBit>>8)AND$FF;
  Fdata[ptr+$1E]:=(FSpriteList[sprite].RightBit>>16)AND$FF;
  Fdata[ptr+$1F]:=(FSpriteList[sprite].RightBit>>24)AND$FF;
  Fdata[ptr+$20]:=FSpriteList[sprite].PixelData AND$FF;
  Fdata[ptr+$21]:=(FSpriteList[sprite].PixelData>>8)AND$FF;
  Fdata[ptr+$22]:=(FSpriteList[sprite].PixelData>>16)AND$FF;
  Fdata[ptr+$23]:=(FSpriteList[sprite].PixelData>>24)AND$FF;
  Fdata[ptr+$24]:=FSpriteList[sprite].Transparency AND $FF;
  Fdata[ptr+$25]:=(FSpriteList[sprite].Transparency>>8)AND$FF;
  Fdata[ptr+$26]:=(FSpriteList[sprite].Transparency>>16)AND$FF;
  Fdata[ptr+$27]:=(FSpriteList[sprite].Transparency>>24)AND$FF;
  Fdata[ptr+$28]:=FSpriteList[sprite].ModeData AND$FF;
  Fdata[ptr+$29]:=(FSpriteList[sprite].ModeData>>8)AND$FF;
  Fdata[ptr+$2A]:=(FSpriteList[sprite].ModeData>>16)AND$FF;
  Fdata[ptr+$2B]:=(FSpriteList[sprite].ModeData>>24)AND$FF;
  //Write the palette data
  if BPP<16 then
   for x:=0 to (Length(FSpriteList[sprite].Palette)div 4)-1 do
   begin
    Fdata[ptr+$2C+x*8+0]:=$00;
    Fdata[ptr+$2C+x*8+1]:=FSpriteList[sprite].Palette[x*4+2];
    Fdata[ptr+$2C+x*8+2]:=FSpriteList[sprite].Palette[x*4+1];
    Fdata[ptr+$2C+x*8+3]:=FSpriteList[sprite].Palette[x*4+0];
    Fdata[ptr+$2C+x*8+4]:=Fdata[ptr+$2C+x*8+0];
    Fdata[ptr+$2C+x*8+5]:=Fdata[ptr+$2C+x*8+1];
    Fdata[ptr+$2C+x*8+6]:=Fdata[ptr+$2C+x*8+2];
    Fdata[ptr+$2C+x*8+7]:=Fdata[ptr+$2C+x*8+3];
   end;
  //Write the pixel and mask data
  for y:=0 to FSpriteList[sprite].ScanLines do
   for x:=0 to FSpriteList[sprite].PixWidth-1 do
   begin
    p:=FSpriteList[sprite].PixelData+ptr+
       (((((FSpriteList[sprite].PixWidth*BPP)+7)div 8)+3)div 4)*4*y+Floor(x*(BPP/8));
    colsrc:=IntToHex(buffer[(y*FSpriteList[sprite].PixWidth+x)*4+0],2)
           +IntToHex(buffer[(y*FSpriteList[sprite].PixWidth+x)*4+1],2)
           +IntToHex(buffer[(y*FSpriteList[sprite].PixWidth+x)*4+2],2);
    case BPP of
     1 : Fdata[p]:=Fdata[p]OR(AnsiIndexStr(colsrc,colours)AND$1)<<(x mod 8);
     2 : Fdata[p]:=Fdata[p]OR(AnsiIndexStr(colsrc,colours)AND$3)<<((x mod 4)*2);
     4 : Fdata[p]:=Fdata[p]OR(AnsiIndexStr(colsrc,colours)AND$7)<<((x mod 2)*4);
     8 : Fdata[p]:=AnsiIndexStr(colsrc,colours);
     16:
      begin
       Fdata[p+0]:=(buffer[(y*FSpriteList[sprite].PixWidth+x)*4+0]AND$F8)>>1
                 OR(buffer[(y*FSpriteList[sprite].PixWidth+x)*4+1]AND$C0)>>6;
       Fdata[p+1]:=(buffer[(y*FSpriteList[sprite].PixWidth+x)*4+1]AND$38)<<2
                 OR(buffer[(y*FSpriteList[sprite].PixWidth+x)*4+2]AND$F8)>>3;
      end;
     32:
      begin
       Fdata[p+0]:=buffer[(y*FSpriteList[sprite].PixWidth+x)*4+0];
       Fdata[p+1]:=buffer[(y*FSpriteList[sprite].PixWidth+x)*4+1];
       Fdata[p+2]:=buffer[(y*FSpriteList[sprite].PixWidth+x)*4+2];
       Fdata[p+3]:=buffer[(y*FSpriteList[sprite].PixWidth+x)*4+3];
      end;
    end;
    if mask>0 then
    begin
     t:=FSpriteList[sprite].Transparency+ptr+
        (((((FSpriteList[sprite].PixWidth*maskBPP)+7)div 8)+3)div 4)*4*y+Floor(x*(maskBPP/8));
     case maskBPP of
      1 : Fdata[t]:=Fdata[t]OR((FSpriteList[sprite].Mask[x,y])>>7)<<(x mod 8);
      2 : Fdata[t]:=Fdata[t]OR((FSpriteList[sprite].Mask[x,y])>>6)<<((x mod 4)*2);
      4 : Fdata[t]:=Fdata[t]OR((FSpriteList[sprite].Mask[x,y])>>4)<<((x mod 2)*4);
      8 : Fdata[t]:=FSpriteList[sprite].Mask[x,y];
      16:
      begin
       Fdata[t+0]:=FSpriteList[sprite].Mask[x,y];
       Fdata[t+1]:=FSpriteList[sprite].Mask[x,y];
      end;
      32:
      begin
       Fdata[t+0]:=FSpriteList[sprite].Mask[x,y];
       Fdata[t+1]:=FSpriteList[sprite].Mask[x,y];
       Fdata[t+2]:=FSpriteList[sprite].Mask[x,y];
       Fdata[t+3]:=FSpriteList[sprite].Mask[x,y];
      end;
     end;
    end;
   end;
  //Now we read it back in, for confidence, and to fill in any gaps we might have
  FSpriteList[sprite]:=ReadSprite(ptr);
  Result:=True;
 end;
end;

function TSpriteFile.IsValidSpriteName(spritename: String): Boolean;
var
 x: Integer;
begin
 Result:=Length(spritename)<13;
 if FSpriteCount>0 then
  for x:=0 to FSpriteCount-1 do
   if spritename=FSpriteList[x].Name then Result:=False;
end;

procedure TSpriteFile.CloseCurrentFile;
begin
 while FSpriteCount>0 do
 begin
  FSpriteList[FSpriteCount-1].Image.Free;
  FSpriteList[FSpriteCount-1].PNG.Free;
  dec(FSpriteCount);
 end;
 SetLength(Fdata,0);
 SetLength(FSpriteList,0);
 FSpriteFile:='';
 FFirstSprite:=0;
 FLastWord:=0;
end;

end.
