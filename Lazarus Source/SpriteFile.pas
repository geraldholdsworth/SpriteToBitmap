unit SpriteFile;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,Graphics,Math;

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
   ColoursUsed   : array of Integer;         // Tally of each colour used (calculated)
   ColourCount   : Integer;                  // Keep track of how many colours are used (calculated)
   ModeFlag,                                 // Mode flag (calculated)
   SpriteType,                               // Sprite type (calculated)
   BPP,                                      // Bits per pixel (calculated)
   BPPOriginal,                              // Original BPP
   OS,                                       // Risc OS compatibility
   TransFormat   : Byte;                     // Old (1), New (2) or no (0) mask format (calculated)
   Palette       : array of Byte;            // Palette (if not in file, loaded from resource or built from pixel data)
   PaletteType   : String;                   // What type of palette? Yes, No or Partial
   Image         : TBitmap;                  // Actual bitmap image
   Mask          : array of array of Boolean // Transparency Mask (converted to true/false)
  end;
  TSprites = array of TSprite;
  //Provides feedback
  TProgressProc = procedure(Fupdate: Integer) of Object;
 private
  FSpriteList: TSprites;
  FSpriteFile: String;
  FDiagnostic: TStringList;
  FProgress  : TProgressProc;//Used for feedback
  function ReadSpriteFile(data: array of Byte): TSprites;
  function bitmapHeader(sizex,sizey,bpp,cols:Integer;var bmp:array of Byte):Integer;
  procedure ExpandBPP(var OldBPP: Byte;var buffer:TDynByteArray);
  procedure AddToPalette(r,g,b: Byte;Sprite: TSprite);
  procedure UpdateProgress(Fupdate: Integer);
  const
   ColourPalette2: array[0..$B] of Byte=($13,$00,$10,$FF,$FF,$FF,$13,$01,
                                         $10,$00,$00,$00);
   ColourPalette4: array[0..$17] of Byte=($13,$00,$10,$FF,$FF,$FF,$13,$01,
                                          $10,$BB,$BB,$BB,$13,$02,$10,$77,
                                          $77,$77,$13,$03,$10,$00,$00,$00);
   ColourPalette16: array[0..$77] of Byte=($13,$00,$10,$FF,$FF,$FF,$13,$01,
                                           $10,$DD,$DD,$DD,$13,$02,$10,$BB,
                                           $BB,$BB,$13,$03,$10,$99,$99,$99,
                                           $13,$04,$10,$77,$77,$77,$13,$05,
                                           $10,$55,$55,$55,$13,$06,$10,$33,
                                           $33,$33,$13,$07,$10,$00,$00,$00,
                                           $13,$08,$10,$00,$44,$99,$13,$09,
                                           $10,$EE,$EE,$00,$13,$0A,$10,$00,
                                           $CC,$00,$13,$0B,$10,$DD,$00,$00,
                                           $13,$0C,$10,$EE,$EE,$BB,$13,$0D,
                                           $10,$55,$88,$00,$13,$0E,$10,$FF,
                                           $BB,$00,$13,$0F,$10,$00,$BB,$FF,
                                           $13,$00,$18,$00,$00,$00,$13,$01,
                                           $19,$00,$FF,$FF,$13,$02,$19,$00,
                                           $00,$99,$13,$03,$19,$FF,$00,$00);
   ColourPalette256: array[0..$5FF] of Byte=($13,$00,$10,$00,$00,$00,$13,$01,
                                             $10,$11,$11,$11,$13,$02,$10,$22,
                                             $22,$22,$13,$03,$10,$33,$33,$33,
                                             $13,$04,$10,$44,$00,$00,$13,$05,
                                             $10,$55,$11,$11,$13,$06,$10,$66,
                                             $22,$22,$13,$07,$10,$77,$33,$33,
                                             $13,$08,$10,$00,$00,$44,$13,$09,
                                             $10,$11,$11,$55,$13,$0A,$10,$22,
                                             $22,$66,$13,$0B,$10,$33,$33,$77,
                                             $13,$0C,$10,$44,$00,$44,$13,$0D,
                                             $10,$55,$11,$55,$13,$0E,$10,$66,
                                             $22,$66,$13,$0F,$10,$77,$33,$77,
                                             $13,$10,$10,$88,$00,$00,$13,$11,
                                             $10,$99,$11,$11,$13,$12,$10,$AA,
                                             $22,$22,$13,$13,$10,$BB,$33,$33,
                                             $13,$14,$10,$CC,$00,$00,$13,$15,
                                             $10,$DD,$11,$11,$13,$16,$10,$EE,
                                             $22,$22,$13,$17,$10,$FF,$33,$33,
                                             $13,$18,$10,$88,$00,$44,$13,$19,
                                             $10,$99,$11,$55,$13,$1A,$10,$AA,
                                             $22,$66,$13,$1B,$10,$BB,$33,$77,
                                             $13,$1C,$10,$CC,$00,$44,$13,$1D,
                                             $10,$DD,$11,$55,$13,$1E,$10,$EE,
                                             $22,$66,$13,$1F,$10,$FF,$33,$77,
                                             $13,$20,$10,$00,$44,$00,$13,$21,
                                             $10,$11,$55,$11,$13,$22,$10,$22,
                                             $66,$22,$13,$23,$10,$33,$77,$33,
                                             $13,$24,$10,$44,$44,$00,$13,$25,
                                             $10,$55,$55,$11,$13,$26,$10,$66,
                                             $66,$22,$13,$27,$10,$77,$77,$33,
                                             $13,$28,$10,$00,$44,$44,$13,$29,
                                             $10,$11,$55,$55,$13,$2A,$10,$22,
                                             $66,$66,$13,$2B,$10,$33,$77,$77,
                                             $13,$2C,$10,$44,$44,$44,$13,$2D,
                                             $10,$55,$55,$55,$13,$2E,$10,$66,
                                             $66,$66,$13,$2F,$10,$77,$77,$77,
                                             $13,$30,$10,$88,$44,$00,$13,$31,
                                             $10,$99,$55,$11,$13,$32,$10,$AA,
                                             $66,$22,$13,$33,$10,$BB,$77,$33,
                                             $13,$34,$10,$CC,$44,$00,$13,$35,
                                             $10,$DD,$55,$11,$13,$36,$10,$EE,
                                             $66,$22,$13,$37,$10,$FF,$77,$33,
                                             $13,$38,$10,$88,$44,$44,$13,$39,
                                             $10,$99,$55,$55,$13,$3A,$10,$AA,
                                             $66,$66,$13,$3B,$10,$BB,$77,$77,
                                             $13,$3C,$10,$CC,$44,$44,$13,$3D,
                                             $10,$DD,$55,$55,$13,$3E,$10,$EE,
                                             $66,$66,$13,$3F,$10,$FF,$77,$77,
                                             $13,$40,$10,$00,$88,$00,$13,$41,
                                             $10,$11,$99,$11,$13,$42,$10,$22,
                                             $AA,$22,$13,$43,$10,$33,$BB,$33,
                                             $13,$44,$10,$44,$88,$00,$13,$45,
                                             $10,$55,$99,$11,$13,$46,$10,$66,
                                             $AA,$22,$13,$47,$10,$77,$BB,$33,
                                             $13,$48,$10,$00,$88,$44,$13,$49,
                                             $10,$11,$99,$55,$13,$4A,$10,$22,
                                             $AA,$66,$13,$4B,$10,$33,$BB,$77,
                                             $13,$4C,$10,$44,$88,$44,$13,$4D,
                                             $10,$55,$99,$55,$13,$4E,$10,$66,
                                             $AA,$66,$13,$4F,$10,$77,$BB,$77,
                                             $13,$50,$10,$88,$88,$00,$13,$51,
                                             $10,$99,$99,$11,$13,$52,$10,$AA,
                                             $AA,$22,$13,$53,$10,$BB,$BB,$33,
                                             $13,$54,$10,$CC,$88,$00,$13,$55,
                                             $10,$DD,$99,$11,$13,$56,$10,$EE,
                                             $AA,$22,$13,$57,$10,$FF,$BB,$33,
                                             $13,$58,$10,$88,$88,$44,$13,$59,
                                             $10,$99,$99,$55,$13,$5A,$10,$AA,
                                             $AA,$66,$13,$5B,$10,$BB,$BB,$77,
                                             $13,$5C,$10,$CC,$88,$44,$13,$5D,
                                             $10,$DD,$99,$55,$13,$5E,$10,$EE,
                                             $AA,$66,$13,$5F,$10,$FF,$BB,$77,
                                             $13,$60,$10,$00,$CC,$00,$13,$61,
                                             $10,$11,$DD,$11,$13,$62,$10,$22,
                                             $EE,$22,$13,$63,$10,$33,$FF,$33,
                                             $13,$64,$10,$44,$CC,$00,$13,$65,
                                             $10,$55,$DD,$11,$13,$66,$10,$66,
                                             $EE,$22,$13,$67,$10,$77,$FF,$33,
                                             $13,$68,$10,$00,$CC,$44,$13,$69,
                                             $10,$11,$DD,$55,$13,$6A,$10,$22,
                                             $EE,$66,$13,$6B,$10,$33,$FF,$77,
                                             $13,$6C,$10,$44,$CC,$44,$13,$6D,
                                             $10,$55,$DD,$55,$13,$6E,$10,$66,
                                             $EE,$66,$13,$6F,$10,$77,$FF,$77,
                                             $13,$70,$10,$88,$CC,$00,$13,$71,
                                             $10,$99,$DD,$11,$13,$72,$10,$AA,
                                             $EE,$22,$13,$73,$10,$BB,$FF,$33,
                                             $13,$74,$10,$CC,$CC,$00,$13,$75,
                                             $10,$DD,$DD,$11,$13,$76,$10,$EE,
                                             $EE,$22,$13,$77,$10,$FF,$FF,$33,
                                             $13,$78,$10,$88,$CC,$44,$13,$79,
                                             $10,$99,$DD,$55,$13,$7A,$10,$AA,
                                             $EE,$66,$13,$7B,$10,$BB,$FF,$77,
                                             $13,$7C,$10,$CC,$CC,$44,$13,$7D,
                                             $10,$DD,$DD,$55,$13,$7E,$10,$EE,
                                             $EE,$66,$13,$7F,$10,$FF,$FF,$77,
                                             $13,$80,$10,$00,$00,$88,$13,$81,
                                             $10,$11,$11,$99,$13,$82,$10,$22,
                                             $22,$AA,$13,$83,$10,$33,$33,$BB,
                                             $13,$84,$10,$44,$00,$88,$13,$85,
                                             $10,$55,$11,$99,$13,$86,$10,$66,
                                             $22,$AA,$13,$87,$10,$77,$33,$BB,
                                             $13,$88,$10,$00,$00,$CC,$13,$89,
                                             $10,$11,$11,$DD,$13,$8A,$10,$22,
                                             $22,$EE,$13,$8B,$10,$33,$33,$FF,
                                             $13,$8C,$10,$44,$00,$CC,$13,$8D,
                                             $10,$55,$11,$DD,$13,$8E,$10,$66,
                                             $22,$EE,$13,$8F,$10,$77,$33,$FF,
                                             $13,$90,$10,$88,$00,$88,$13,$91,
                                             $10,$99,$11,$99,$13,$92,$10,$AA,
                                             $22,$AA,$13,$93,$10,$BB,$33,$BB,
                                             $13,$94,$10,$CC,$00,$88,$13,$95,
                                             $10,$DD,$11,$99,$13,$96,$10,$EE,
                                             $22,$AA,$13,$97,$10,$FF,$33,$BB,
                                             $13,$98,$10,$88,$00,$CC,$13,$99,
                                             $10,$99,$11,$DD,$13,$9A,$10,$AA,
                                             $22,$EE,$13,$9B,$10,$BB,$33,$FF,
                                             $13,$9C,$10,$CC,$00,$CC,$13,$9D,
                                             $10,$DD,$11,$DD,$13,$9E,$10,$EE,
                                             $22,$EE,$13,$9F,$10,$FF,$33,$FF,
                                             $13,$A0,$10,$00,$44,$88,$13,$A1,
                                             $10,$11,$55,$99,$13,$A2,$10,$22,
                                             $66,$AA,$13,$A3,$10,$33,$77,$BB,
                                             $13,$A4,$10,$44,$44,$88,$13,$A5,
                                             $10,$55,$55,$99,$13,$A6,$10,$66,
                                             $66,$AA,$13,$A7,$10,$77,$77,$BB,
                                             $13,$A8,$10,$00,$44,$CC,$13,$A9,
                                             $10,$11,$55,$DD,$13,$AA,$10,$22,
                                             $66,$EE,$13,$AB,$10,$33,$77,$FF,
                                             $13,$AC,$10,$44,$44,$CC,$13,$AD,
                                             $10,$55,$55,$DD,$13,$AE,$10,$66,
                                             $66,$EE,$13,$AF,$10,$77,$77,$FF,
                                             $13,$B0,$10,$88,$44,$88,$13,$B1,
                                             $10,$99,$55,$99,$13,$B2,$10,$AA,
                                             $66,$AA,$13,$B3,$10,$BB,$77,$BB,
                                             $13,$B4,$10,$CC,$44,$88,$13,$B5,
                                             $10,$DD,$55,$99,$13,$B6,$10,$EE,
                                             $66,$AA,$13,$B7,$10,$FF,$77,$BB,
                                             $13,$B8,$10,$88,$44,$CC,$13,$B9,
                                             $10,$99,$55,$DD,$13,$BA,$10,$AA,
                                             $66,$EE,$13,$BB,$10,$BB,$77,$FF,
                                             $13,$BC,$10,$CC,$44,$CC,$13,$BD,
                                             $10,$DD,$55,$DD,$13,$BE,$10,$EE,
                                             $66,$EE,$13,$BF,$10,$FF,$77,$FF,
                                             $13,$C0,$10,$00,$88,$88,$13,$C1,
                                             $10,$11,$99,$99,$13,$C2,$10,$22,
                                             $AA,$AA,$13,$C3,$10,$33,$BB,$BB,
                                             $13,$C4,$10,$44,$88,$88,$13,$C5,
                                             $10,$55,$99,$99,$13,$C6,$10,$66,
                                             $AA,$AA,$13,$C7,$10,$77,$BB,$BB,
                                             $13,$C8,$10,$00,$88,$CC,$13,$C9,
                                             $10,$11,$99,$DD,$13,$CA,$10,$22,
                                             $AA,$EE,$13,$CB,$10,$33,$BB,$FF,
                                             $13,$CC,$10,$44,$88,$CC,$13,$CD,
                                             $10,$55,$99,$DD,$13,$CE,$10,$66,
                                             $AA,$EE,$13,$CF,$10,$77,$BB,$FF,
                                             $13,$D0,$10,$88,$88,$88,$13,$D1,
                                             $10,$99,$99,$99,$13,$D2,$10,$AA,
                                             $AA,$AA,$13,$D3,$10,$BB,$BB,$BB,
                                             $13,$D4,$10,$CC,$88,$88,$13,$D5,
                                             $10,$DD,$99,$99,$13,$D6,$10,$EE,
                                             $AA,$AA,$13,$D7,$10,$FF,$BB,$BB,
                                             $13,$D8,$10,$88,$88,$CC,$13,$D9,
                                             $10,$99,$99,$DD,$13,$DA,$10,$AA,
                                             $AA,$EE,$13,$DB,$10,$BB,$BB,$FF,
                                             $13,$DC,$10,$CC,$88,$CC,$13,$DD,
                                             $10,$DD,$99,$DD,$13,$DE,$10,$EE,
                                             $AA,$EE,$13,$DF,$10,$FF,$BB,$FF,
                                             $13,$E0,$10,$00,$CC,$88,$13,$E1,
                                             $10,$11,$DD,$99,$13,$E2,$10,$22,
                                             $EE,$AA,$13,$E3,$10,$33,$FF,$BB,
                                             $13,$E4,$10,$44,$CC,$88,$13,$E5,
                                             $10,$55,$DD,$99,$13,$E6,$10,$66,
                                             $EE,$AA,$13,$E7,$10,$77,$FF,$BB,
                                             $13,$E8,$10,$00,$CC,$CC,$13,$E9,
                                             $10,$11,$DD,$DD,$13,$EA,$10,$22,
                                             $EE,$EE,$13,$EB,$10,$33,$FF,$FF,
                                             $13,$EC,$10,$44,$CC,$CC,$13,$ED,
                                             $10,$55,$DD,$DD,$13,$EE,$10,$66,
                                             $EE,$EE,$13,$EF,$10,$77,$FF,$FF,
                                             $13,$F0,$10,$88,$CC,$88,$13,$F1,
                                             $10,$99,$DD,$99,$13,$F2,$10,$AA,
                                             $EE,$AA,$13,$F3,$10,$BB,$FF,$BB,
                                             $13,$F4,$10,$CC,$CC,$88,$13,$F5,
                                             $10,$DD,$DD,$99,$13,$F6,$10,$EE,
                                             $EE,$AA,$13,$F7,$10,$FF,$FF,$BB,
                                             $13,$F8,$10,$88,$CC,$CC,$13,$F9,
                                             $10,$99,$DD,$DD,$13,$FA,$10,$AA,
                                             $EE,$EE,$13,$FB,$10,$BB,$FF,$FF,
                                             $13,$FC,$10,$CC,$CC,$CC,$13,$FD,
                                             $10,$DD,$DD,$DD,$13,$FE,$10,$EE,
                                             $EE,$EE,$13,$FF,$10,$FF,$FF,$FF);
 published
  constructor Create;
  procedure LoadSpriteFile(Afilename: String);
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

procedure TSpriteFile.LoadSpriteFile(Afilename: String);
var
 F: TFileStream;
 data: array of Byte;
begin
 //Open the file and read the data in
 F:=TFileStream.Create(Afilename,fmOpenRead or fmShareDenyNone);
 F.Position:=0;
 SetLength(data,F.Size+4);
 F.Read(data[$04],F.Size);
 F.Free;
 FSpriteFile:=Afilename;
 FSpriteList:=ReadSpriteFile(data);
end;

destructor TSpriteFile.Destroy;
begin
 inherited;
end;

function TSpriteFile.ReadSpriteFile(data: array of Byte): TSprites;
var
  buffer     : array of Byte;
  x,sprites,
  amt,ctr    : Integer;
  ptr,sx,sy,
  bx,p,p2,t,
  t2,tp,size,
  endoffile  : Cardinal;
  ms         : TMemoryStream;
const
 BGColour = $FFFF00FF;
 //BPP colour depth of the Arthur/RISC OS 2/RISC OS 3.1 modes
 modes: array[0..53] of Byte = (1,2,3,2,1,2,1,3,2,3,
                                4,2,3,4,3,4,3,3,1,2,
                                3,4,3,1,4,1,2,3,4,1,
                                2,3,4,1,2,3,4,1,2,3,
                                4,1,2,4,1,2,3,4,3,4,
                                1,2,3,4);
 //OS Compatibility string
 OSstr  : array[0..2]  of String = ('Arthur','RISC OS 3.5','RISC OS 5.0');
 //Sprite type string
 ModeStr: array[0..18] of String = ('Arthur mode','1bpp','2bpp','4bpp','8bpp',
                                    '16bpp 1:5:5:5 TBGR','32bpp 8:8:8:8 TBGR',
                                    '32bpp CMYK','24bpp','JPEG data',
                                    '16bpp 5:6:5 TBGR','Reserved','Reserved',
                                    'Reserved','Reserved','RISC OS 5',
                                    '16bpp 4:4:4:4','4:2:0 YCbCr','4:2:2 YCbCr');
begin
 //Set up the variables
 Result:=nil;
 SetLength(Result,0);
 FDiagnostic.Clear;
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
   16: Result[x].BPP:=16; //16bpp 4:4:4
   17: ;//4:2:0 YCbCr
   18: ;//4:2:4 YCbCr
   //11-14 and 19-127 Reserved
  end;
  Result[x].BPPOriginal:=Result[x].BPP;
  FDiagnostic.Add('Sprite Type              : '+IntToStr(Result[x].SpriteType));
  if Result[x].SpriteType<=High(ModeStr) then
   FDiagnostic.Add('Sprite Type meaning      : '+ModeStr[Result[x].SpriteType]);
  //Expand on RISC OS 5 details
  if Result[x].OS=2 then
  begin
   FDiagnostic.Add('Mode Flags               : 0x'+IntToHex(Result[x].ModeFlag,2));
   if Result[x].ModeFlag AND $1=$1 then
    FDiagnostic.Add('Mode Flag meaning        : Full res interlace');
   if Result[x].ModeFlag AND $2=$2 then
    FDiagnostic.Add('Mode Flag meaning        : Greyscale');
   case (Result[x].ModeFlag>>4)AND$3 of
    0: //RGB
     case Result[x].ModeFlag>>6 of
      0: FDiagnostic.Add('Mode Flag meaning        : RGB - TBGR');
      1: FDiagnostic.Add('Mode Flag meaning        : RGB - TRGB');
      2: FDiagnostic.Add('Mode Flag meaning        : RGB - ABGR');
      3: FDiagnostic.Add('Mode Flag meaning        : RGB - ARGB');
     end;
    1: //Misc
     case Result[x].ModeFlag>>6 of
      0: FDiagnostic.Add('Mode Flag meaning        : KYMC');
      1: FDiagnostic.Add('Mode Flag meaning        : Reserved');
      2: FDiagnostic.Add('Mode Flag meaning        : Reserved');
      3: FDiagnostic.Add('Mode Flag meaning        : Reserved');
     end;
    2:
     case Result[x].ModeFlag>>6 of
      0: FDiagnostic.Add('Mode Flag meaning        : YCbCr - ITU-R BT.601 Full');
      1: FDiagnostic.Add('Mode Flag meaning        : YCbCr - ITU-R BT.601 Video');
      2: FDiagnostic.Add('Mode Flag meaning        : YCbCr - ITU-R BT.709 Full');
      3: FDiagnostic.Add('Mode Flag meaning        : YCbCr - ITU-R BT.709 Video');
     end;
    3: FDiagnostic.Add('Mode Flag meaning        : Reserved');
   end;
  end;
  FDiagnostic.Add('Bits per pixel           : '+IntToStr(Result[x].BPP));
  FDiagnostic.Add('Compatibility            : '+OSStr[Result[x].OS]);
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
   //If we have, then read it in
   if (p>$2C) AND (Result[x].BPPOriginal<16) then
   begin
    //Palette is in the file
    Result[x].PaletteType:='Yes';
    //Number of entries in palette
    Result[x].PaletteColours:=(p-$2C) div 8;
    SetLength(Result[x].Palette,Result[x].PaletteColours*4);
    //For bpp less than 8, or 8bpp with full palette
    if (Result[x].BPPOriginal<8)
    or((Result[x].BPPOriginal=8) and (Result[x].PaletteColours=256)) then
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
   //We don't have a palette in file
   if (p=$2C) OR ((Result[x].BPPOriginal=8)AND(Result[x].PaletteColours<256))then
   begin
    //Use standard palette (constants at top of class definition)
    if(Result[x].BPPOriginal=8)AND(Result[x].PaletteColours>0)
                               AND(Result[x].PaletteColours<256)then
     Result[x].PaletteType:='Partial'
    else
     Result[x].PaletteType:='No';
    if Result[x].BPPOriginal<16 then //BPP of 16,24 and 32 have no palette
    begin
     SetLength(buffer,0);
     case Result[x].BPPOriginal of
      1: buffer:=ColourPalette2;    //2 colour palette
      2: buffer:=ColourPalette4;    //4 colour palette
      4: buffer:=ColourPalette16;   //16 colour palette
      8: buffer:=ColourPalette256;  //256 colour palette
     end;
     //Assign enough memory for the palette
     SetLength(Result[x].Palette,(1<<Result[x].BPPOriginal)*4);
     //Extract the palette entries, discarding the VDU19,cn,16
     for t:=0 to (1<<Result[x].BPPOriginal)-1 do
     begin
      Result[x].Palette[ t*4   ]:=buffer[(t*6)+5];//Red
      Result[x].Palette[(t*4)+1]:=buffer[(t*6)+4];//Green
      Result[x].Palette[(t*4)+2]:=buffer[(t*6)+3];//Blue
      Result[x].Palette[(t*4)+3]:=$00;            //Alpha
     end;
    end;
   end;
   FDiagnostic.Add('Palette                  : '+Result[x].PaletteType);
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
   //Is there a transparent mask?
   if Result[x].Transparency<>Result[x].PixelData then
   begin
    FDiagnostic.Add('Transparency             : Yes');
    FDiagnostic.Add('Background Colour        : 0x'+IntToHex(Result[x].BGColour,8));
    SetLength(Result[x].Mask,Result[x].PixWidth,
                                 Result[x].ScanLines+1);
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
    begin
     Result[x].TransFormat:=2;
     FDiagnostic.Add('Format of Transparency   : New');
    end
    else
    begin
     Result[x].TransFormat:=1;
     FDiagnostic.Add('Format of Transparency   : Old');
    end;
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
   //Set the bitmap to be transparent, if needed
   if Result[x].TransFormat<>0 then
   begin
    Result[x].Image.Transparent:=True;
    Result[x].Image.TransparentColor:=Result[x].BGColour mod $1000000;
   end;
   //Setup buffer to create bitmap data in
   SetLength(buffer,$36);
   amt:=bitmapHeader(Result[x].PixWidth,
                     Result[x].ScanLines+1,
                     Result[x].BPP,
                     0,
                     buffer);
   SetLength(buffer,amt);
   //Copy the palette across
   for t:=$36 to amt-1 do buffer[t]:=0;
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
    //X co-ordinate variable to point into some bitmaps, separate from sprites
    bx:=0;
    for sx:=0 to ((Result[x].WidthWord+1)*4)-1 do
    //Change so it reads in 4 bytes at a time, into a 4 element byte array
    //then we can deal with the tranparancy mask at the same time
    begin
     //Pointer to pixel in sprite
     p:=ptr+Result[x].PixelData+((Result[x].WidthWord+1)*sy*4)+sx;
     //We will read in the mask data first +++++++++++++++++++++++++++++++++++++
     //Old style mask
     if Result[x].TransFormat=1 then
     begin
      //Pointer to mask pixel in sprite
      tp:=Result[x].Transparency+ptr+((Result[x].WidthWord+1)*sy*4)+sx;
      case Result[x].BPPOriginal of
       4: //4bpp
       begin
        //Read in a byte from the transparent data
        t:=data[tp+(Result[x].LeftBit div 8)];
        //Allow for left hand wastage
        t:=t>>(Result[x].LeftBit mod 8);
        //one byte has information for two pixels
        if sx*2<Length(Result[x].Mask) then
         if sy<Length(Result[x].Mask[sx*2]) then
          Result[x].Mask[sx*2,sy]:=(t AND $0F)=$00;
        if (sx*2)+1<Length(Result[x].Mask) then
         if sy<Length(Result[x].Mask[(sx*2)+1]) then
          Result[x].Mask[(sx*2)+1,sy]:=(t AND $F0)=$00;
       end;
       8: //8bpp
         if sx<Length(Result[x].Mask) then
           if sy<Length(Result[x].Mask[sx]) then
            Result[x].Mask[sx,sy]:=data[tp]=$00;
      end;
     end;
     //New style mask
     if Result[x].TransFormat=2 then
      if sx<Length(Result[x].Mask) then
       if sy<Length(Result[x].Mask[sx]) then
        begin
         //Get the byte
         tp:=Result[x].Transparency+ptr+
             ((((((Result[x].WidthWord+1)*4)+7)div 8)+3)div 4)*4*sy+
             (sx div 8);
         Result[x].Mask[sx,sy]:=data[tp]and(1<<(sx mod 8))=0;
        end;
     //Do we have a 2bpp sprite - bitmaps don't have this format, so we need to upscale
     if Result[x].BPPOriginal=2 then
     begin
      //We will begin to use the new variable, as we have changed this
      Result[x].BPP:=4;
      //As 2bpp is getting converted to 4bpp, we need to know the new word width
      if bx*2<Result[x].PixWidth then
       amt:=p2+(Cardinal(Ceil((Result[x].PixWidth*4)/32))
              *(Result[x].ScanLines-sy)*4)+bx
      else amt:=0;
      inc(bx,2);
     end
     else
      amt:=p2+((Result[x].WidthWord+1)*(Result[x].ScanLines-sy)*4)+sx;
     //Read in a byte from the picture data
     t:=data[p+(Result[x].LeftBit div 8)];
     t2:=0;
     //Allow for left hand wastage
     t:=t>>(Result[x].LeftBit mod 8);
     //swap the nibbles with 4bpp
     if Result[x].BPPOriginal=4 then
      t:=(t<<4) OR (t>>4);
     //Expand 2bpp to 4bpp, swapping half nibbles
     if Result[x].BPPOriginal=2 then
     begin
      //Bits 0,1 and 4,5 are used in 2 to 4bpp conversion. Other bits are 0
      t2:=((t AND $C0)>>6)  //bits 6 & 7 become 0 & 1
         + (t AND $30);     //bits 4 & 5  stay  4 & 5
      t :=((t AND $0C)>>2)  //bits 4 & 5 become 0 & 1
         +((t AND $03)<<4); //bits 0 & 1 become 4 & 5
     end;
     //Put it in the buffer, top down
     if amt>0 then
     begin
      buffer[amt]:=t;
      if Result[x].BPPOriginal=2 then
       buffer[amt+1]:=t2;
      //And count up the colours used
      case Result[x].BPPOriginal of
       1:
       begin
        inc(Result[x].ColoursUsed[ t AND $01]);
        inc(Result[x].ColoursUsed[(t AND $02)>>1]);
        inc(Result[x].ColoursUsed[(t AND $04)>>2]);
        inc(Result[x].ColoursUsed[(t AND $08)>>3]);
        inc(Result[x].ColoursUsed[(t AND $10)>>4]);
        inc(Result[x].ColoursUsed[(t AND $20)>>5]);
        inc(Result[x].ColoursUsed[(t AND $40)>>6]);
        inc(Result[x].ColoursUsed[(t AND $80)>>7]);
       end;
       2:
       begin
        inc(Result[x].ColoursUsed[ t AND $03]);
        inc(Result[x].ColoursUsed[(t AND $0C)>>2]);
        inc(Result[x].ColoursUsed[ t2 AND $03]);
        inc(Result[x].ColoursUsed[(t2 AND $0C)>>2]);
       end;
       4:
       begin
        inc(Result[x].ColoursUsed[ t AND $0F]);
        inc(Result[x].ColoursUsed[(t AND $F0)>>4]);
       end;
       8: inc(Result[x].ColoursUsed[t]);
      end;
     end;
    end;
   end;
   //Change the Red and Blues around - 16 bpp
   if Result[x].BPP=16 then
    repeat
     //Read it in as a 16 bit value
     t:=buffer[p2]
      +(buffer[p2+1])<<8;
     //Shuffle around
     t2:=((t AND $7C00)>>10)  //Bits 10-14
        + (t AND $03E0)       //Bits 5-9
        +((t AND $001F)<<10); //Bits 0-4
     //Add to palette
//      AddToPalette((t AND $7C00)shr 10,
//                   (t AND $03E0)shr 5,
//                   t AND $001F,x);
     //Put back
     buffer[p2]  :=t2 mod $100;
     buffer[p2+1]:=t2 div $100;
     //Move onto next 16 bits
     inc(p2,2);
     p:=Length(buffer)-1; //Length is an Integer, p2 is a Cardinal
    until p2>=p;
   //Change the Red and Blues around - 32 bpp
   if Result[x].BPP=32 then
    repeat
     //Read it in as a 32 bit value
     t:=buffer[p2]
      +(buffer[p2+1])<<8
      +(buffer[p2+2])<<16
      +(buffer[p2+3])<<24;
     //Shuffle around
     t2:=((t AND $00FF0000)>>16)
        + (t AND $0000FF00)
        +((t AND $000000FF)<<16);
     //Add to palette
//      AddToPalette((t AND $00FF0000)shr 16,
//                   (t AND $0000FF00)shr 8,
//                   (t AND $000000FF),x);
     //Put back
     buffer[p2]  := t2      mod $100;
     buffer[p2+1]:=(t2>>8 ) mod $100;
     buffer[p2+2]:=(t2>>16) mod $100;
     buffer[p2+3]:=(t2>>24) mod $100;
     //Move onto next 32 bits
     inc(p2,4);
     p:=Length(buffer)-1; //Length is an Integer, p2 is a Cardinal
    until p2>=p;
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
   if Result[x].TransFormat>0 then
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
     begin
      buffer[$36+(ctr*4)]  := Result[x].BGColour      mod $100;//R
      buffer[$36+(ctr*4)+1]:=(Result[x].BGColour>>8)  mod $100;//G
      buffer[$36+(ctr*4)+2]:=(Result[x].BGColour>>16) mod $100;//B
      buffer[$36+(ctr*4)+3]:=(Result[x].BGColour>>24) mod $100;//A
     end;
    end;
    //Overlay the mask, overwriting whatever pixels are there with BGColour
    if(ctr>=0)or(Result[x].BPP>8)then
    for sy:=0 to Result[x].ScanLines do
     for sx:=0 to Result[x].PixWidth-1 do
     begin
      //Is pixel transparent?
      if Result[x].Mask[sx,sy] then
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
        16:
        begin
         buffer[p2+(sx*2)]  :=(((Result[x].BGColour AND$F8)>>3)
                             +((Result[x].BGColour AND$F800)>>6))mod$100;
         buffer[p2+(sx*2)+1]:=(((Result[x].BGColour AND$F800)>>6)
                             +((Result[x].BGColour AND$F80000)>>9))div$100;
        end;
        32:
        begin
         buffer[p2+(sx*4)]  := Result[x].BGColour mod $100;
         buffer[p2+(sx*4)+1]:=(Result[x].BGColour>>8) mod $100;
         buffer[p2+(sx*4)+2]:=(Result[x].BGColour>>16) mod $100;
         buffer[p2+(sx*4)+3]:=(Result[x].BGColour>>24) mod $100;
        end;
       end;
      end;
     end;
   end;
   if Result[x].BPP<>Result[x].BPPOriginal then
    FDiagnostic.Add('New bits per pixel       : '+IntToStr(Result[x].BPP));
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
   begin
    newbmp[NewPX]  :=(newbmp[NewPX]  AND$F0)OR(buffer[$38+amt*4]and$0F);//Blue
    newbmp[NewPX]  :=(newbmp[NewPX]  AND$0F)OR(buffer[$37+amt*4]and$0F);//Green
    newbmp[NewPX+1]:=(newbmp[NewPX+1]AND$F0)OR(buffer[$36+amt*4]and$0F);//Red
    newbmp[NewPX+1]:=(newbmp[NewPX+1]AND$0F)OR(buffer[$39+amt*4]and$0F);//Alpha
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
