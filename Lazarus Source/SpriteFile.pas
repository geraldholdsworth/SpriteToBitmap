unit SpriteFile;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,Graphics,Math,Dialogs;

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
   PaletteColours,                           // Number of palette entries (calculated) <16bpp
   BGColour      : Cardinal;                 // Mask colour
   HasPalette    : Boolean;                  // Does sprite have a palette?
   ColoursUsed   : array of Integer;         // Tally of each colour used (calculated)
   ColourCount   : Integer;                  // Keep track of how many colours are used (calculated)
   ModeFlag,                                 // Mode flag (calculated)
   SpriteType,                               // Sprite type (calculated)
   BPP,                                      // Bits per pixel (calculated)
   BPPOriginal,                              // Original BPP
   OS,                                       // Risc OS compatibility
   PaletteType,                              // What type of palette? Yes (1), No (0) or Partial (2)
   TransFormat   : Byte;                     // Old (1), New (2) or no (0) mask format (calculated)
   Palette       : array of Byte;            // Palette (if not in file, loaded from resource or built from pixel data)
   Image         : TBitmap;                  // Actual bitmap image
   Mask          : array of array of Byte    // Transparency Mask (converted to true/false)
   //Change the Mask property to a cardinal, so that a full alpha channel value
   //can be stored, then OR-ed with the pixel value.
  end;
  TSprites = array of TSprite;
  //Provides feedback
  TProgressProc = procedure(Fupdate: Integer) of Object;
 private
  Fdata : array of Byte; //The actual sprite file
  FSpriteList: TSprites;
  FSpriteFile: String;
  FDiagnostic: TStringList;
  FProgress  : TProgressProc;//Used for feedback
  function ReadSpriteFile(data: array of Byte;var error: Byte): TSprites;
  function bitmapHeader(sizex,sizey,bpp,cols:Integer;var bmp:array of Byte):Integer;
  procedure ExpandBPP(var OldBPP: Byte;var buffer:TDynByteArray);
  procedure AddToPalette(r,g,b: Byte;Sprite: TSprite);
  procedure UpdateProgress(Fupdate: Integer);
  function DecodeModeFlags(modeflag: Byte): String;
  function DecodeSpriteType(spritetype: Byte): String;
  function DecodeMaskType(transformat: Byte): String;
  function DecodeOS(os: Byte): String;
  function DecodePaletteType(pal: Byte): String;
  {$INCLUDE 'SpriteFilePalettes.pas'}
 published
  constructor Create;
  function LoadSpriteFile(Afilename: String):Byte;
  function SaveSpriteFile(Afilename: String):Boolean;
  function SavePaletteFile(Afilename: String;sprite: Integer):Boolean;
  function ModeFlag(spritenumber: Integer): String;
  function SpriteType(spritenumber: Integer): String;
  function MaskFormat(spritenumber: Integer): String;
  function OS(spritenumber: Integer): String;
  function PaletteType(spritenumber: Integer): String;
  property SpriteList: TSprites read FSpriteList;
  property SpriteFile: String   read FSpriteFile;
  property LogFile: TStringList read FDiagnostic;
  property ProgressIndicator: TProgressProc write FProgress;
 public
  destructor Destroy; override;
 end;

implementation

constructor TSpriteFile.Create;
begin
 inherited Create;
 FSpriteFile:='';
 SetLength(FSpriteList,0);
 FDiagnostic:=TStringList.Create;
end;

function TSpriteFile.LoadSpriteFile(Afilename: String):Byte;
var
 F    : TFileStream;
 error: Byte;
begin
 error:=3;
 //Open the file and read the data in
 try
  F:=TFileStream.Create(Afilename,fmOpenRead or fmShareDenyNone);
  F.Position:=0;
  SetLength(Fdata,F.Size+4);
  F.Read(Fdata[$04],F.Size);
  F.Free;
  FSpriteFile:=Afilename;
  FSpriteList:=ReadSpriteFile(Fdata,error);
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
end;

function TSpriteFile.ReadSpriteFile(data: array of Byte;var error:Byte): TSprites;
var
  buffer     : array of Byte;
  maskbpp,
  r,g,b,a,
  swap,
  c,m,y,k    : Byte;
  x,sprites,
  amt,ctr    : Integer;
  ptr,sx,sy,
  bx,p,p2,t,
  t2,tp,size,
  endoffile  : Cardinal;
  ms         : TMemoryStream;
  mask       : Boolean;
const
 BGColour = $FFFF00FF;
 //BPP colour depth of the Arthur/RISC OS 2/RISC OS 3.1 modes
 modes: array[0..53] of Byte = (1,2,3,2,1,2,1,3,2,3,
                                4,2,3,4,3,4,3,3,1,2,
                                3,4,3,1,4,1,2,3,4,1,
                                2,3,4,1,2,3,4,1,2,3,
                                4,1,2,4,1,2,3,4,3,4,
                                1,2,3,4);
begin
 //Set up the variables
 Result:=nil;
 SetLength(Result,0);
 FDiagnostic.Clear;
 maskbpp:=0;
 error:=0;
 //Create the memory stream
 ms:=TMemoryStream.Create;
 //Sprite file header ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 //Size of sprite area - not in file, but in Sprite Area
 size:=Length(data)-4;
 data[$00]:= size      MOD $100;
 data[$01]:=(size>> 8) MOD $100;
 data[$02]:=(size>>16) MOD $100;
 data[$03]:=(size>>24) MOD $100;
 //Get the number of sprites
 sprites:=(data[$07]<<24)
         +(data[$06]<<16)
         +(data[$05]<<8)
         + data[$04];
 FDiagnostic.Add('Number of sprites in file: '+IntToStr(sprites));
 //Get the position of the first sprite - this will act as our pointer into the data
 ptr:=(data[$0B]<<24)
     +(data[$0A]<<16)
     +(data[$09]<<8)
     + data[$08];
 FDiagnostic.Add('Pointer to first sprite  : 0x'+IntToHex(ptr,8));
 //Get the position of the last free word - this is the end of the file
 endoffile:=(data[$0F]<<24)
           +(data[$0E]<<16)
           +(data[$0D]<<8)
           + data[$0C];
 FDiagnostic.Add('Pointer to last free word: 0x'+IntToHex(endoffile,8));
 if (ptr>size) or (endoffile>size+4) then //Invalid Sprite File
 begin
  SetLength(Result,0);
  FDiagnostic.Clear;
  FDiagnostic.Add('Invalid Sprite File - pointer to end of file or first sprite is bigger than file size.');
  error:=1;
  exit;
 end;
 //Set up the structure
 SetLength(Result,sprites);
 //Read in all the sprites +++++++++++++++++++++++++++++++++++++++++++++++++++++
 for x:=0 to sprites-1 do
 begin
  FDiagnostic.Add('-------------------------------------');
  FDiagnostic.Add('Memory Pointer           : '+IntToHex(ptr,8));
  FDiagnostic.Add('-------------------------------------');
  FDiagnostic.Add('Sprite                   : '+IntToStr(x));
  //Sprite header ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  Result[x].Offset:=ptr;
  //Pointer to next sprite
  Result[x].Next:=(data[ptr+$03]<<24)
                 +(data[ptr+$02]<<16)
                 +(data[ptr+$01]<<8)
                 + data[ptr+$00];
  FDiagnostic.Add('Pointer to next sprite   : 0x'+IntToHex(Result[x].Next,8));
  if ptr+Result[x].Next>endoffile then //Invalid Sprite File
  begin
   SetLength(Result,0);
   FDiagnostic.Clear;
   FDiagnostic.Add('Invalid Sprite File - pointer to next sprite is beyond the end of the file.');
   error:=2;
   exit;
  end;
  //Sprite name
  Result[x].Name:='';
  for sx:=0 to 11 do
  begin
   t:=data[ptr+$04+sx]AND$7F;//macOS doesn't like top bit set characters
   if t<32 then t:=t OR $20;//Remove any control characters
   Result[x].Name:=Result[x].Name+chr(t);
  end;
  //Still no spritename? Can't save it to disc.
  if Result[x].Name='' then Result[x].Name:='Sprite'+IntToStr(x);
  //Remove any extraneous spaces from the beginning or end
  while(Result[x].Name[Length(Result[x].Name)]=' ')and(Length(Result[x].Name)>1)do
   Result[x].Name:=LeftStr(Result[x].Name,Length(Result[x].Name)-1);
  while(Result[x].Name[1]=' ')and(Length(Result[x].Name)>1)do
   Result[x].Name:=RightStr(Result[x].Name,Length(Result[x].Name)-1);
  FDiagnostic.Add('Sprite Name              : '+Result[x].Name);
  //Width of sprite (in words) minus 1
  Result[x].WidthWord :=(data[ptr+$13]<<24)
                       +(data[ptr+$12]<<16)
                       +(data[ptr+$11]<<8)
                       + data[ptr+$10];
  FDiagnostic.Add('Width in words           : '+IntToStr(Result[x].WidthWord+1));
  //Height of sprite (in scanlines) minus 1
  Result[x].ScanLines:=(data[ptr+$17]<<24)
                      +(data[ptr+$16]<<16)
                      +(data[ptr+$15]<<8)
                      + data[ptr+$14];
  FDiagnostic.Add('Height in scan lines     : '+IntToStr(Result[x].ScanLines+1));
  //First bit used (aka left hand wastage)
  Result[x].LeftBit:=(data[ptr+$1B]<<24)
                    +(data[ptr+$1A]<<16)
                    +(data[ptr+$19]<<8)
                    + data[ptr+$18];
  FDiagnostic.Add('Left bit                 : 0x'+IntToHex(Result[x].LeftBit,8));
  //Last bit used (aka right hand wastage)
  Result[x].RightBit :=(data[ptr+$1F]<<24)
                      +(data[ptr+$1E]<<16)
                      +(data[ptr+$1D]<<8)
                      + data[ptr+$1C];
  FDiagnostic.Add('Right bit                : 0x'+IntToHex(Result[x].RightBit,8));
  //Pointer to first pixel of sprite
  Result[x].PixelData:=(data[ptr+$23]<<24)
                      +(data[ptr+$22]<<16)
                      +(data[ptr+$21]<<8)
                      +(data[ptr+$20]);
  FDiagnostic.Add('Pixel Data offset        : 0x'+IntToHex(Result[x].PixelData,8));
  //Pointer to transparent mask
  Result[x].Transparency:=(data[ptr+$27]<<24)
                         +(data[ptr+$26]<<16)
                         +(data[ptr+$25]<<8)
                         +(data[ptr+$24]);
  FDiagnostic.Add('Transparent Data offset  : 0x'+IntToHex(Result[x].Transparency,8));
  //Mode data
  Result[x].ModeData:= (data[ptr+$2B]<<24)
                      +(data[ptr+$2A]<<16)
                      +(data[ptr+$29]<<8)
                      + data[ptr+$28];
  FDiagnostic.Add('Mode data                : 0x'+IntToHex(Result[x].ModeData,8));
  //Header read in, now the calculations +++++++++++++++++++++++++++++++++++++++
  FDiagnostic.Add('Calculations:');
  //Mode flag
  if Result[x].ModeData<256 then                //Old format
  begin
   //Not sprite type, but the actual screen mode, as old format
   Result[x].SpriteType:=modes[Result[x].ModeData mod 54];   //no modes 54+
   Result[x].ModeFlag:=0;
   Result[x].OS:=0;
  end
  else
   if (Result[x].ModeData>>27)AND$0F=$0F then   //RISC OS 5
   begin
    Result[x].SpriteType:=(Result[x].ModeData>>20)AND$7F;
    Result[x].ModeFlag:=(Result[x].ModeData>>8)AND$FF;
    Result[x].OS:=2;
   end
   else                                         //RISC OS 3.5
   begin
    Result[x].SpriteType:=(Result[x].ModeData>>27)AND$0F;
    Result[x].ModeFlag:=0;
    Result[x].OS:=1;
   end;
  //Bits per pixel
  Result[x].BPP:=0;
  case Result[x].SpriteType of
    0: ;//Invalid - used to id Arthur modes
    1: Result[x].BPP:= 1; //1bpp palletised
    2: Result[x].BPP:= 2; //2bpp palletised
    3: Result[x].BPP:= 4; //4bpp palletised
    4: Result[x].BPP:= 8; //8bpp palletised
    5: Result[x].BPP:=16; //16bpp 1:5:5:5 TBGR
    6: Result[x].BPP:=32; //32bpp 8:8:8:8 TBGR
    7: Result[x].BPP:=32; //CMYK
    8: Result[x].BPP:=24;
    9: ;//JPEG
   10: Result[x].BPP:=16; //16bpp 5:6:5 TBGR
   15: ;//Invalid - used for id RISC OS 5 sprite mode words
   16: Result[x].BPP:=16; //16bpp 4:4:4:4
   17: ;//4:2:0 YCbCr
   18: ;//4:2:4 YCbCr
   //11-14 and 19-127 Reserved
  end;
  Result[x].BPPOriginal:=Result[x].BPP;
  if Result[x].BPP=2 then Result[x].BPP:=4; //Bitmaps don't have 2bpp
  FDiagnostic.Add('Sprite Type              : '+IntToStr(Result[x].SpriteType));
  FDiagnostic.Add('Sprite Type meaning      : '+DecodeSpriteType(Result[x].SpriteType));
  if Result[x].SpriteType=7 then Result[x].OS:=3;
  //Expand on RISC OS 5 details
  if Result[x].OS=2 then
  begin
   FDiagnostic.Add('Mode Flags               : 0x'+IntToHex(Result[x].ModeFlag,2));
   FDiagnostic.Add('Mode Flags meaning       : '+DecodeModeFlags(Result[x].ModeFlag));
  end;
  FDiagnostic.Add('Bits per pixel           : '+IntToStr(Result[x].BPPOriginal));
  FDiagnostic.Add('Compatibility            : '+DecodeOS(Result[x].OS));
  //Pixel Width
  if Result[x].BPPOriginal>0 then //BPP of 0 means that we can't handle it, currently
  begin
   Result[x].PixWidth:=((Result[x].WidthWord*32)
                        +Result[x].RightBit+1
                        -Result[x].LeftBit)
                        div Result[x].BPPOriginal;
   FDiagnostic.Add('Width in pixels          : '+IntToStr(Result[x].PixWidth));
   //Now we read in the data +++++++++++++++++++++++++++++++++++++++++++++++++++
   //Have we a palette?
   if Result[x].PixelData>Result[x].Transparency then
    p:=Result[x].Transparency
   else
    p:=Result[x].PixelData;
   //By default
   Result[x].PaletteType:=0;
   Result[x].HasPalette:=False;
   if Result[x].BPPOriginal<16 then //sprites with bpp of 16 or more have no palette
   begin
    if p>$2C then //Yes, palette is in the file
    begin
     Result[x].PaletteType:=1;
     Result[x].HasPalette:=True;
     //Number of entries in palette
     Result[x].PaletteColours:=(p-$2C) div 8;
     //Partial palette? 8bpp only
     if(Result[x].BPPOriginal=8)AND(Result[x].PaletteColours<256)then
      Result[x].PaletteType:=2;
    end;
    //Assign enough memory for the palette
    SetLength(Result[x].Palette,(1<<Result[x].BPPOriginal)*4);
    //We don't have a palette in file, or it is partial
    if(not Result[x].HasPalette)or(Result[x].PaletteType=2)then
    begin
     //Use standard palette (constants in file SpriteFilePalettes.pas)
     SetLength(buffer,0);
     case Result[x].BPPOriginal of
      1: buffer:=ColourPalette2;    //2 colour palette
      2: buffer:=ColourPalette4;    //4 colour palette
      4: buffer:=ColourPalette16;   //16 colour palette
      8: buffer:=ColourPalette256;  //256 colour palette
     end;
     //Extract the palette entries, discarding the VDU19,cn,16
     for t:=0 to (1<<Result[x].BPPOriginal)-1 do
     begin
      Result[x].Palette[ t*4   ]:=buffer[(t*6)+5];//Blue
      Result[x].Palette[(t*4)+1]:=buffer[(t*6)+4];//Green
      Result[x].Palette[(t*4)+2]:=buffer[(t*6)+3];//Red
      Result[x].Palette[(t*4)+3]:=$00;            //Alpha
     end;
    end;
    //We do have a palette in file - if partial, we will overwrite the standard one
    if Result[x].HasPalette then
    begin
     sx:=0;  //Pointer into our palette
     t:=$2C; //Pointer into the data
     repeat
      Result[x].Palette[sx  ]:=data[ptr+t+3]; //Blue
      Result[x].Palette[sx+1]:=data[ptr+t+2]; //Green
      Result[x].Palette[sx+2]:=data[ptr+t+1]; //Red
      Result[x].Palette[sx+3]:=$00;           //Alpha (0x00)
      inc(sx,4);
      inc(t,8);
     until sx>=Result[x].PaletteColours*4; //t>=p-1;
    end;
   end;
   FDiagnostic.Add('Palette                  : '+DecodePaletteType(Result[x].PaletteType));
   FDiagnostic.Add('Number of palette colours: '+IntToStr(Result[x].PaletteColours));
   //Assign the background colour, which will be transparent
   Result[x].BGColour:=BGColour;
   //Check to see that our BGColour is not used
{   if Length(Result[x].Palette)>0 then
   begin
    t:=0;
    //Go through the palette
    while(t<Length(Result[x].Palette))and(Result[x].BGColour<>0)do
    begin
     //Check each colour
     if (Result[x].Palette[ t*4   ]= Result[x].BGColour mod $100)//Red
     and(Result[x].Palette[(t*4)+1]=(Result[x].BGColour>>8) mod $100)//Green
     and(Result[x].Palette[(t*4)+2]=(Result[x].BGColour>>16) mod $100) then//Blue
     begin
      //If we have a match, start back at the beginning of the palette
      t:=0;
      //Move onto the next colour
      if Result[x].BGColour>$FF000000 then
      begin
       dec(Result[x].BGColour,$100);
       //If we are back at BGColour, we have failed, so reset to zero
       if Result[x].BGColour=BGColour then Result[x].BGColour:=0;
      end
      else
       //Loop round
       Result[x].BGColour:=$FFFFFFFF;
     end
     else inc(t,4); //Otherwise, check next colour
    end;
   end;}
   //Set up the mask array
   SetLength(Result[x].Mask,Result[x].PixWidth,Result[x].ScanLines+1);
   for sx:=0 to Length(Result[x].Mask)-1 do
    for sy:=0 to Length(Result[x].Mask[sx])-1 do
     Result[x].Mask[sx,sy]:=$00;
   //Is there a transparent mask?
   if Result[x].Transparency<>Result[x].PixelData then
   begin
    FDiagnostic.Add('Transparency             : Yes');
    FDiagnostic.Add('Background Colour        : 0x'+IntToHex(Result[x].BGColour,8));
    //We do, so read in transparent mask
    if Result[x].Transparency>Result[x].PixelData then
    begin //Transparency data is after pixel data
     t:=Result[x].Next-Result[x].Transparency; //Size of transparency
     t2:=Result[x].Transparency-Result[x].PixelData; //Size of pixel data
    end
    else
    begin //Transparency data is before pixel data
     t2:=Result[x].Next-Result[x].PixelData; //Size of Pixel Data
     t:=Result[x].PixelData-Result[x].Transparency; //Size of Transparency
    end;
    //Work out if it is old format or new format
    if t<t2 then
     //Look at bit 31 of the mode data - if set, it is a 'wide mask'.
     //This means that the mask will be 8bpp, whatever. Therefore, the size of
     //the pixel data and the mask data will be different.
     if Result[x].ModeData>>31=0 then
     begin
      Result[x].TransFormat:=2;
      maskbpp:=1; //Mask is 1bpp
     end
     else
     begin
      Result[x].TransFormat:=3;
      maskbpp:=8; //Mask is 8bpp Alpha
     end
    else
    begin
     Result[x].TransFormat:=1;
     maskbpp:=Result[x].BPPOriginal;
    end;
    FDiagnostic.Add('Format of Transparency   : '+DecodeMaskType(Result[x].TransFormat));
   end
   else
   begin //No mask
    Result[x].TransFormat:=0;
    FDiagnostic.Add('Transparency             : No');
   end;
   if(Result[x].BGColour=0)and(Result[x].TransFormat>0)then
   begin //No mask - no free colour found
    Result[x].TransFormat:=0;
    FDiagnostic.Add('Transparency             : No free colours to use');
   end;
   //All information now gathered from the file, for this sprite, so now create
   //the image. We still need to read in the mask and sprite data.
   //
   //Create the bitmap container in the array
   Result[x].Image:=TBitmap.Create;
   //Setup buffer to create bitmap data in
   SetLength(buffer,$36);
   amt:=bitmapHeader(Result[x].PixWidth,
                     Result[x].ScanLines+1,
                     Result[x].BPP,
                     0,
                     buffer);
   SetLength(buffer,amt);
   for t:=$36 to amt-1 do buffer[t]:=0;
   //Copy the palette across
   if Result[x].BPP<16 then
    for t:=0 to Length(Result[x].Palette)-1 do
     buffer[$36+t]:=Result[x].Palette[t];
   //Pointer to Pixel data in bitmap
   p2:=buffer[$0A]
     +(buffer[$0B]<<8)
     +(buffer[$0C]<<16)
     +(buffer[$0D]<<24);
   //Set and empty the Colours Used counter
   if Result[x].BPPOriginal<16 then
   begin
    //As we read the sprite data in, we will count the colours used
    SetLength(Result[x].ColoursUsed,1<<Result[x].BPPOriginal);
    for t:=0 to Length(Result[x].ColoursUsed)-1 do
     Result[x].ColoursUsed[t]:=0;
   end;
   //Extract the sprite data +++++++++++++++++++++++++++++++++++++++++++++++++++
   for sy:=0 to Result[x].ScanLines do
   begin
    //Loops through each pixel
    for sx:=0 to Result[x].PixWidth-1 do
    begin
     //We will read in the mask data first +++++++++++++++++++++++++++++++++++++
     if Result[x].TransFormat>0 then //Check we have a mask
      if sx<Length(Result[x].Mask) then //And the x and y are within bounds
       if sy<Length(Result[x].Mask[sx]) then
       begin
        //Pointer to mask pixel in sprite
        tp:=Result[x].Transparency+ptr+
            (((((Result[x].PixWidth*maskbpp)+7)div 8)+3)div 4)*4*sy+
            Floor(sx*(maskbpp/8))+
            (Result[x].LeftBit div 8);
        //Byte(s) containing the pixel
        t:=0;
        for bx:=0 to Ceil(maskbpp/8)-1 do t:=t+data[tp+bx]<<(bx*8);
        //Take account of the left hand wastage
        t:=t>>(Result[x].LeftBit mod 8);
        mask:=False;
        case maskbpp of
         1: mask:=t and($1<<(sx mod 8))=0;//Result[x].Mask[sx,sy]:=$FF XOR(t and($1<<(sx mod 8)));    //1bpp
         2: mask:=t and($3<<((sx mod 4)*2))=0;//Result[x].Mask[sx,sy]:=$FF XOR(t and($3<<((sx mod 4)*2)));//2bpp
         4: mask:=t AND($F<<((sx mod 2)*4))=0;//Result[x].Mask[sx,sy]:=$FF XOR(t AND($F<<((sx mod 2)*4)));//4bpp
         8,  //8bpp
         16, //16bpp
         24, //24bpp
         32: mask:=t and $FF=0;//Result[x].Mask[sx,sy]:=$FF XOR(t and $FF); //32bpp
        end;
        if mask then Result[x].Mask[sx,sy]:=$FF else Result[x].Mask[sx,sy]:=$00;
        if Result[x].TransFormat=3 then //Wide mask, so this is the alpha value
         Result[x].Mask[sx,sy]:=t and $FF;//Result[x].Mask[sx,sy] XOR $FF;
       end;
     //Pointer to pixel in sprite
     p:=Result[x].PixelData+ptr+
            (((((Result[x].PixWidth*Result[x].BPPOriginal)+7)div 8)+3)div 4)*4*sy+
            Floor(sx*(Result[x].BPPOriginal/8))+
            (Result[x].LeftBit div 8);
     //Byte(s) containing the pixel
     t:=0;
     for bx:=0 to Ceil(Result[x].BPPOriginal/8)-1 do t:=t+data[p+bx]<<(bx*8);
     //Take account of the left hand wastage
     t:=t>>(Result[x].LeftBit mod 8);
     //Pointer to pixel in bitmap
     amt:=p2+((Ceil((Result[x].PixWidth*Result[x].BPP)/32)*4)*(Result[x].ScanLines-sy))
            +Floor(sx*(Result[x].BPP/8));
     //Put it in the buffer, top down
     if amt>0 then
     begin
      case Result[x].BPP of
       1:
       begin
        buffer[amt]:=buffer[amt]OR(t and($1<<(sx mod 8)));
        inc(Result[x].ColoursUsed[(t>>(sx mod 8))AND 1]);
       end;
       4:
       begin
        //Expand 2bpp to 4bpp, swapping half nibbles
        if Result[x].BPPOriginal=2 then
        begin
         t:=(t>>((sx mod 4)*2))AND$3;//Isolate the pixel and shift it right
         t:=t<<((sx mod 2)*4);//Shift it left to make it 4bpp
        end;
        //Swap nibbles round
        t:=((t AND$F)<<4)OR((t AND$F0)>>4);
        buffer[amt]:=buffer[amt]OR(t and($F0>>((sx mod 2)*4)));
        inc(Result[x].ColoursUsed[(t>>((sx mod 2)*4))and$F]);
       end;
       8:
       begin
        buffer[amt]:=t;
        inc(Result[x].ColoursUsed[t]);
       end;
       16:
       begin
        //Default values
        r:=0;
        g:=0;
        b:=0;
        a:=0;
        //1:5:5:5 - Sprite Type 5
        if Result[x].SpriteType=5 then
        begin
         b:=(t AND $7C00)>>7;
         g:=(t AND $3E0)>>2;
         r:=(t AND $1F)<<3;
         a:=(t AND $8000)>>8;
        end;
        //5:6:5 - Sprite Type 10
        if Result[x].SpriteType=10 then
        begin
         b:=(t AND $F800)>>8;
         g:=(t AND $7E0)>>3;
         r:=(t AND $1F)<<3;
         a:=0;
        end;
        //4:4:4:4 - Sprite Type 16
        if Result[x].SpriteType=16 then
        begin
         b:=(t AND $F000)>>8;
         g:=(t AND $F00)>>4;
         r:= t AND $F0;
         a:=(t AND $F)<<4;
        end;
        // Is it RGB instead of BGR?
        if(Result[x].ModeFlag>>6)mod 2=1then
        begin
         //Swap the blue and red around
         swap:=b;
         b:=r;
         r:=swap;
        end;
        //Write to the bitmap
        buffer[amt  ]:=b>>3 or g<<2;
        buffer[amt+1]:=g>>6 or r>>1 or a;
        //Does it have an alpha channel?
        if Result[x].TransFormat=3 then
         buffer[amt+1]:=(buffer[amt+1]and$7F)or(Result[x].Mask[sx,sy]and$80); //Alpha
       end;
       32:
       begin
        //BGR
        r:=t and $FF;
        g:=(t>>8)and$FF;
        b:=(t>>16)and$FF;
        a:=(t>>24)and$FF;
        //Does not take account of ModeFlag 'Misc', being CMYK
        if(Result[x].ModeFlag>>6)mod 2=1then//RGB
        begin
         swap:=b;
         b:=r;
         r:=swap;
        end;
        if Result[x].SpriteType=7 then
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
        if Result[x].TransFormat=3 then
         a:=Result[x].Mask[sx,sy]; //Alpha
        //Write the values
        buffer[amt+0]:=b;
        buffer[amt+1]:=g;
        buffer[amt+2]:=r;
        buffer[amt+3]:=a;
       end;
      end;
     end;
    end;
   end;
   //Count the number of colours used
   if Result[x].BPP<16 then
   begin
    Result[x].ColourCount:=0;
    for t:=0 to Length(Result[x].ColoursUsed)-1 do
     if Result[x].ColoursUsed[t]<>0 then
      inc(Result[x].ColourCount);
    FDiagnostic.Add('Number of colours used   : '+IntToStr(Result[x].ColourCount));
   end;
   //Apply the mask
   if(Result[x].TransFormat=1)or(Result[x].TransFormat=2)then
   begin
    ctr:=-1;
    //Change the pixel to the BGColour, depending on the BPP
    //Can only do this if there is a free colour in the palette
    if Result[x].BPP<9 then
    begin
     if Result[x].ColourCount<1<<Result[x].BPP then
     begin
      //Find a free colour
      repeat
       inc(ctr);
      until (Result[x].ColoursUsed[ctr]=0) or (ctr=1<<Result[x].BPP-1);
      //Have we found one?
      if Result[x].ColoursUsed[ctr]<>0 then ctr:=-1; //No - mark as not found
     end;
     //No free colours, so we'll need to expand the bpp to add the extra colour
     if ctr=-1 then
     begin
      ExpandBPP(Result[x].BPP,buffer);
      if Result[x].BPP<16 then ctr:=(1<<Result[x].BPPOriginal)+1;
     end;
     //If we have any spare colours, add the mask colour to the palette
     if ctr<>-1 then
     begin //TColor format is ABGR, whereas BMP is ARGB
      buffer[$38+(ctr*4)]:= Result[x].BGColour      mod $100;//R
      buffer[$37+(ctr*4)]:=(Result[x].BGColour>>8)  mod $100;//G
      buffer[$36+(ctr*4)]:=(Result[x].BGColour>>16) mod $100;//B
      buffer[$39+(ctr*4)]:=(Result[x].BGColour>>24) mod $100;//A
     end;
    end;
    //Overlay the mask, overwriting whatever pixels are there with BGColour
    if(ctr>=0)or(Result[x].BPP>8)then
     for sy:=0 to Result[x].ScanLines do
      for sx:=0 to Result[x].PixWidth-1 do
      begin
       //Is pixel transparent?
       if Result[x].Mask[sx,sy]=$FF then
       begin
        //Pointer to pixel data
        p2:=buffer[$0A]
          +(buffer[$0B]<<8)
          +(buffer[$0C]<<16)
          +(buffer[$0D]<<24);
        //Pointer to start of row in bitmap
        p2:=p2+((Ceil((Result[x].PixWidth*Result[x].BPP)/32)*4)*(Result[x].ScanLines-sy));
        //Put the mask pixel in
        case Result[x].BPP of
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
          buffer[p2+(sx*2)]  :=((Result[x].BGColour AND$F80000)>>19)   //B
                              +((Result[x].BGColour AND$3800)>>6);     //G
          buffer[p2+(sx*2)+1]:=((Result[x].BGColour AND$C000)>>14)     //G
                              +((Result[x].BGColour AND$F8)>>1)        //R
                              +((Result[x].BGColour AND$80000000)>>24);//A
         end;
         32:
         begin //TColor format is ABGR, whereas BMP is ARGB
          buffer[p2+(sx*4)+2]:= Result[x].BGColour mod $100;     //Red
          buffer[p2+(sx*4)+1]:=(Result[x].BGColour>>8) mod $100; //Green
          buffer[p2+(sx*4)+0]:=(Result[x].BGColour>>16) mod $100;//Blue
          //Not sure why, but the next line will make the entire image transparent
//          buffer[p2+(sx*4)+3]:=(Result[x].BGColour>>24) mod $100;//Alpha
         end;
        end;
       end;
     end;
   end;
   if Result[x].BPP<>Result[x].BPPOriginal then
    FDiagnostic.Add('New bits per pixel       : '+IntToStr(Result[x].BPP));
   //Set the bitmap to be transparent, if needed
   if(Result[x].TransFormat<>0){and(Result[x].BPP<>32)}then
   begin
    Result[x].Image.Transparent:=True;
    if Result[x].BPP=16 then
     Result[x].Image.TransparentColor:=Result[x].BGColour and $00FFFFF7
    else// if Result[x].BPP<>32 then
     Result[x].Image.TransparentColor:=Result[x].BGColour mod $1000000;
   end
   else
    Result[x].Image.Transparent:=False;
   //Load the buffer into the bitmap
   ms.Position:=0;
   ms.WriteBuffer(buffer[0],Length(buffer));
   ms.Position:=0;
   Result[x].Image.LoadFromStream(ms);
   //Move onto the next sprite
   ptr:=ptr+Result[x].Next;
  end;
  UpdateProgress(Round((x/sprites*100)));
 end;
 //Free up the memory stream
 ms.Free;
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

function TSpriteFile.DecodeModeFlags(modeflag: Byte): String;
begin
 Result:='';
 if modeflag AND $1=$1 then
  Result:=Result+' Full res interlace';
 if modeflag AND $2=$2 then
  Result:=Result+' Greyscale';
 case (modeflag>>4)AND$3 of
  0: //RGB
   case modeflag>>6 of
    0: Result:=Result+' RGB - TBGR';
    1: Result:=Result+' RGB - TRGB';
    2: Result:=Result+' RGB - ABGR';
    3: Result:=Result+' RGB - ARGB';
   end;
  1: //Misc
   case modeflag>>6 of
    0: Result:=Result+' KYMC';
    1: Result:=Result+' Reserved';
    2: Result:=Result+' Reserved';
    3: Result:=Result+' Reserved';
   end;
  2:
   case modeflag>>6 of
    0: Result:=Result+' YCbCr - ITU-R BT.601 Full';
    1: Result:=Result+' YCbCr - ITU-R BT.601 Video';
    2: Result:=Result+' YCbCr - ITU-R BT.709 Full';
    3: Result:=Result+' YCbCr - ITU-R BT.709 Video';
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

function TSpriteFile.bitmapHeader(sizex,sizey,bpp,cols:Integer;var bmp:array of Byte):Integer;
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
end;

procedure TSpriteFile.ExpandBPP(var OldBPP:Byte;var buffer:TDynByteArray);
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
end;

procedure TSpriteFile.AddToPalette(r,g,b: Byte;Sprite: TSprite);
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
end;

procedure TSpriteFile.UpdateProgress(Fupdate: Integer);
begin
 //If the main program has defined a procedure then call it
 if Assigned(FProgress) then FProgress(Fupdate);
end;

end.
