unit MainUnit;

interface

uses
  Controls,Math,SysUtils,ExtCtrls,StdCtrls,Forms,Buttons,LCLType,Dialogs,
  Classes,Graphics,FPImage,IntfGraphics,LazCanvas;

type
  TSprite = record
   Name          : String;                   // Name of the sprite
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
   BPP,                                      // Bits per pixel (calculated)
   BPPOriginal,                              // Original BPP
   TransFormat   : Byte;                     // Old (1), New (2) or no (0) mask format (calculated)
   Palette       : array of Byte;            // Palette (if not in file, loaded from resource or built from pixel data)
   Image         : TBitmap;                  // Actual bitmap image
   Mask          : array of array of Boolean // Transparency Mask (converted to true/false)
  end;

  { TMainForm }

  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    Memo1: TMemo;
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    sb_OpenFile: TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    sb_Save: TSpeedButton;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure sb_OpenFileClick(Sender: TObject);
    procedure LoadSpriteFile(filename: String);
    function createImage(cparent: TObject;ctop,cleft,cwidth,cheight: Integer): TImage;
    procedure FormCreate(Sender: TObject);
    function bitmapHeader(sizex,sizey,bpp,cols: Integer; var bmp: array of Byte): Integer;
    function validateFilename(f: String): String;
    procedure Image1DblClick(Sender: TObject);
    procedure sb_SaveClick(Sender: TObject);
    procedure SaveAsPng(screen:TBitmap;AFileName: String;Mask:Boolean);
    procedure AddToPalette(r,g,b: Byte;x: Integer);
  private
   image: array of TImage;
  public
   SpriteList: array of TSprite;
   const
    BGColour = $FFFF00FF;
    AppTitle = 'Sprite Converter';
    AppVersion = '0.01.4';
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
   BigImageForm.ZoomedImage.Width:=SpriteList[x].Image.Width;
   BigImageForm.ZoomedImage.Height:=SpriteList[x].Image.Height;
   BigImageForm.ZoomedImage.Canvas.AntialiasingMode:=amOff;
   BigImageForm.ZoomedImage.Stretch:=True;
   BigImageForm.ZoomedImage.Proportional:=True;
   BigImageForm.ZoomedImage.Canvas.Pen.Color:=BigImageForm.Color;
   BigImageForm.ZoomedImage.Canvas.Brush.Color:=BigImageForm.Color;
   BigImageForm.ZoomedImage.Canvas.Rectangle(0,0,BigImageForm.ZoomedImage.Width,BigImageForm.ZoomedImage.Height);
   BigImageForm.ZoomedImage.Canvas.Draw(0,0,SpriteList[x].Image);
   BigImageForm.ZoomedImage.Align:=alClient;
//  BigImageForm.ZoomedImage.Picture.Bitmap:=SpriteList[i].Image;
  BigImageForm.Caption:=SpriteList[x].Name;
  BigImageForm.lb_size.Caption:='Size:'+IntToStr(SpriteList[x].PixWidth)+'x'
                                       +IntToStr(SpriteList[x].ScanLines+1)
                               +' BPP:'+IntToStr(SpriteList[x].BPP);
  if SpriteList[x].BPP<>SpriteList[x].BPPOriginal then
   BigImageForm.lb_size.Caption:=BigImageForm.lb_size.Caption
                                +' (converted from '
                                +IntToStr(SpriteList[x].BPPOriginal)+'bpp)';
  BigImageForm.ShowModal;
 end;
end;

procedure TMainForm.sb_OpenFileClick(Sender: TObject);
begin
 if OpenDialog.Execute then
  LoadSpriteFile(OpenDialog.Filename);
end;

procedure TMainForm.LoadSpriteFile(filename: String);
var
  F: TFileStream;
  data,buffer: array of Byte;
  x,sprites,amt,ctr: Integer;
  ptr,
  sx,sy,bx,
  ix,iy,
  p,p2,
  t,t2,tp,
  size,
  endoffile: Cardinal;
  res: TResourceStream;
  ms: TMemoryStream;
  palette: String;
  bigH: Integer;
const
 modes: array[0..53] of Byte = (1,2,3,2,1,2,1,3,2,3,
                                4,2,3,4,3,4,3,3,1,2,
                                3,4,3,1,4,1,2,3,4,1,
                                2,3,4,1,2,3,4,1,2,3,
                                4,1,2,4,1,2,3,4,3,4,
                                1,2,3,4);
begin
 //First, we'll destroy any images that have been opened before
 if Length(image)>0 then
  for x:=0 to Length(image)-1 do
   image[x].Free;
 SetLength(image,0);
 if Length(SpriteList)>0 then
  for x:=0 to Length(SpriteList)-1 do
  begin
   SpriteList[x].Image.Free;
   SetLength(SpriteList[x].Palette,0);
   SetLength(SpriteList[x].ColoursUsed,0);
  end;
 SetLength(SpriteList,0);
 //Create the memory stream
 ms:=TMemoryStream.Create;
 //Open the file and read the data in
 F:=TFileStream.Create(filename,fmOpenRead or fmShareDenyNone);
 F.Position:=0;
 size:=F.Size;
 SetLength(data,size+4);
 amt:=F.Read(data[$04],size);
 F.Free;
 //Currently no check for valid sprite file
 Memo1.Lines.Clear;
 Memo1.Lines.Add(filename);
 //Size of sprite area - not in file, but in Sprite Area
 data[$00]:= size MOD $100;
 data[$01]:=(size DIV $100) MOD $100;
 data[$02]:=(size DIV $10000) MOD $100;
 data[$03]:=(size DIV $1000000) MOD $100;
 //Get the number of sprites
 sprites:=(data[$07]*$1000000)
         +(data[$06]*$10000)
         +(data[$05]*$100)
         + data[$04];
 Memo1.Lines.Add('Number of sprites in file: '+IntToStr(sprites));
 //Get the position of the first sprite
 ptr:=(data[$0B]*$1000000)
     +(data[$0A]*$10000)
     +(data[$09]*$100)
     + data[$08];
 Memo1.Lines.Add('Pointer to first sprite  : 0x'+IntToHex(ptr,8));
 //Get the position of the last free word
 endoffile:=(data[$0F]*$1000000)
           +(data[$0E]*$10000)
           +(data[$0D]*$100)
           + data[$0C];
 Memo1.Lines.Add('Pointer to last free word: 0x'+IntToHex(endoffile,8));
 if (ptr>size) or (endoffile>size+4) then
 begin
  ShowMessage('Invalid Sprite File');
  exit;
 end;
 //Set up the arrays for the sprites
 SetLength(image,sprites);
 SetLength(SpriteList,sprites);
 //Image position on the form
 ix:=4;
 iy:=4;
 bigH:=0;
 for x:=0 to sprites-1 do
 begin
  Memo1.Lines.Add('-------------------------------------');
  Memo1.Lines.Add('Memory Pointer           : '+IntToHex(ptr,8));
  Memo1.Lines.Add('-------------------------------------');
  Memo1.Lines.Add('Sprite                   : '+IntToStr(x));
  //Pointer to next sprite
  SpriteList[x].Next:=(data[ptr+$03]*$1000000)
                     +(data[ptr+$02]*$10000)
                     +(data[ptr+$01]*$100)
                     + data[ptr+$00];
  Memo1.Lines.Add('Pointer to next sprite   : 0x'+IntToHex(SpriteList[x].Next,8));
  if ptr+SpriteList[x].Next>endoffile then
  begin
   ShowMessage('Invalid Sprite File');
   exit;
  end;
  //Get the sprite name
  SpriteList[x].Name:='';
  for sx:=0 to 11 do
  begin
   t:=data[ptr+$04+sx];
   if (t>31) AND (t<127) then //macOS doesn't like top bit set characters
    SpriteList[x].Name:=SpriteList[x].Name+chr(t);
  end;
  //No spritename? Can't save it to disc.
  if SpriteList[x].Name='' then SpriteList[x].Name:='Sprite'+IntToStr(x);
  Memo1.Lines.Add('Sprite Name              : '+SpriteList[x].Name);
  //Width of sprite
  SpriteList[x].WidthWord :=(data[ptr+$13]*$1000000)
                           +(data[ptr+$12]*$10000)
                           +(data[ptr+$11]*$100)
                           + data[ptr+$10];
  Memo1.Lines.Add('Width in words           : '+IntToStr(SpriteList[x].WidthWord+1));
  //Height of sprite
  SpriteList[x].ScanLines:=(data[ptr+$17]*$1000000)
                          +(data[ptr+$16]*$10000)
                          +(data[ptr+$15]*$100)
                          + data[ptr+$14];
  Memo1.Lines.Add('Height in scan lines     : '+IntToStr(SpriteList[x].ScanLines+1));
  //First bit used
  SpriteList[x].LeftBit:=(data[ptr+$1B]*$1000000)
                        +(data[ptr+$1A]*$10000)
                        +(data[ptr+$19]*$100)
                        + data[ptr+$18];
  Memo1.Lines.Add('Left bit                 : 0x'+IntToHex(SpriteList[x].LeftBit,8));
  //Last bit used
  SpriteList[x].RightBit :=(data[ptr+$1F]*$1000000)
                          +(data[ptr+$1E]*$10000)
                          +(data[ptr+$1D]*$100)
                          + data[ptr+$1C];
  Memo1.Lines.Add('Right bit                : 0x'+IntToHex(SpriteList[x].RightBit,8));
  //Pointer to first pixel of sprite
  SpriteList[x].PixelData:=(data[ptr+$23]*$1000000)
                          +(data[ptr+$22]*$10000)
                          +(data[ptr+$21]*$100)
                          +(data[ptr+$20]);
  Memo1.Lines.Add('Pixel Data offset        : 0x'+IntToHex(SpriteList[x].PixelData,8));
  //Pointer to transparent mask
  SpriteList[x].Transparency:=(data[ptr+$27]*$1000000)
                             +(data[ptr+$26]*$10000)
                             +(data[ptr+$25]*$100)
                             +(data[ptr+$24]);
  Memo1.Lines.Add('Transparent Data offset  : 0x'+IntToHex(SpriteList[x].Transparency,8));
  //Mode data
  SpriteList[x].ModeData:= (data[ptr+$2B]*$1000000)
                          +(data[ptr+$2A]*$10000)
                          +(data[ptr+$29]*$100)
                          + data[ptr+$28];
  Memo1.Lines.Add('Mode data                : 0x'+IntToHex(SpriteList[x].ModeData,8));
  Memo1.Lines.Add('Calculations:');
  //Bits per pixel
  if SpriteList[x].ModeData<256 then                          //Old format
   SpriteList[x].ModeFlag:=modes[SpriteList[x].ModeData mod 54] //no modes 54+
  else
   if (SpriteList[x].ModeData AND $7FFFFFFF)shr 27=$FFFF then //RISC OS 5
    SpriteList[x].ModeFlag:=(SpriteList[x].ModeData AND $3F80000)>>20
   else                                     //RISC OS 3.5
    SpriteList[x].ModeFlag:=(SpriteList[x].ModeData AND $7FFFFFFF)>>27;
  //Bits per pixel
  SpriteList[x].BPP:=0;
  case SpriteList[x].ModeFlag of
   1: SpriteList[x].BPP:= 1;
   2: SpriteList[x].BPP:= 2;
   3: SpriteList[x].BPP:= 4;
   4: SpriteList[x].BPP:= 8;
   5: SpriteList[x].BPP:=16;
   6: SpriteList[x].BPP:=32;
  end;
  SpriteList[x].BPPOriginal:=SpriteList[x].BPP;
  Memo1.Lines.Add('Mode Flag                : '+IntToStr(SpriteList[x].ModeFlag));
  Memo1.Lines.Add('Bits per pixel           : '+IntToStr(SpriteList[x].BPP));
  //Pixel Width
  if SpriteList[x].BPP>0 then //BPP of 0 means that we can't handle it, currently
  begin
   SpriteList[x].PixWidth:=((SpriteList[x].WidthWord*32)
                            +SpriteList[x].RightBit+1
                            -SpriteList[x].LeftBit)
                         div SpriteList[x].BPP;
   Memo1.Lines.Add('Width in pixels          : '+IntToStr(SpriteList[x].PixWidth));
   //Get the palette
   if SpriteList[x].PixelData>SpriteList[x].Transparency then
    p:=SpriteList[x].Transparency
   else
    p:=SpriteList[x].PixelData;
   if (p>$2C) AND (SpriteList[x].BPP<16) then
   begin
     //Palette is in file
    Memo1.Lines.Add('Palette                  : Yes');
    //Number of entries in palette
    SpriteList[x].PaletteColours:=(p-$2C) div 8;
    SetLength(SpriteList[x].Palette,SpriteList[x].PaletteColours*4);
    Memo1.Lines.Add('Number of palette colours: '+IntToStr(SpriteList[x].PaletteColours));
    if (SpriteList[x].BPP<8)
    or ((SpriteList[x].BPP=8) and (SpriteList[x].PaletteColours=256)) then
    begin
     sx:=0;
     t:=$2C;
     repeat
      SpriteList[x].Palette[sx  ]:=data[ptr+t+3]; //Blue
      SpriteList[x].Palette[sx+1]:=data[ptr+t+2]; //Green
      SpriteList[x].Palette[sx+2]:=data[ptr+t+1]; //Red
      SpriteList[x].Palette[sx+3]:=$00;           //Alpha (0x00)
      inc(sx,4);
      inc(t,8);
     until sx>=SpriteList[x].PaletteColours*4; //t>=p-1;
    end;
   end;
   if (p=$2C) OR ((SpriteList[x].BPP=8)AND(SpriteList[x].PaletteColours<256))then
   begin
    //Use standard palette, from resources
    Memo1.Lines.Add('Palette                  : No');
    if SpriteList[x].BPP<16 then //BPP of 16,24 and 32 have no palette
    begin
     //Resource Name
     palette:='';
     case SpriteList[x].BPP of
      1: palette:='2ColourPalette';
      2: palette:='4ColourPalette';
      4: palette:='16ColourPalette';
      8: palette:='256ColourPalette';
     end;
     //Assign enough memory for the palette
     SetLength(SpriteList[x].Palette,(1<<SpriteList[x].BPP)*4);
     //Read in the resource into a temporary buffer
     res:=TResourceStream.Create(hInstance,palette,RT_RCDATA);
     res.Position:=0;
     SetLength(buffer,res.Size);
     res.ReadBuffer(buffer[0],res.Size);
     res.Free;
     //Extract the palette entries, discarding the VDU19,cn,16
     for t:=0 to (1<<SpriteList[x].BPP)-1 do
     begin
      SpriteList[x].Palette[ t*4   ]:=buffer[(t*6)+5];//Red
      SpriteList[x].Palette[(t*4)+1]:=buffer[(t*6)+4];//Green
      SpriteList[x].Palette[(t*4)+2]:=buffer[(t*6)+3];//Blue
      SpriteList[x].Palette[(t*4)+3]:=$00;
     end;
    end;
   end;
   SpriteList[x].BGColour:=BGColour;
   if SpriteList[x].Transparency<>SpriteList[x].PixelData then
   begin
    Memo1.Lines.Add('Transparency             : Yes');
    SetLength(SpriteList[x].Mask,SpriteList[x].PixWidth,
                                 SpriteList[x].ScanLines+1);
    if SpriteList[x].Transparency>SpriteList[x].PixelData then
    begin //Transparency data is after pixel data
     t:=SpriteList[x].Next-SpriteList[x].Transparency; //Size of transparency
     t2:=SpriteList[x].Transparency-SpriteList[x].PixelData; //Size of pixel data
    end
    else
    begin //Transparency data is before pixel data
     t2:=SpriteList[x].Next-SpriteList[x].PixelData; //Size of Pixel Data
     t:=SpriteList[x].PixelData-SpriteList[x].Transparency; //Size of Transparency
    end;
    if t<t2 then
    begin
     SpriteList[x].TransFormat:=2;
     Memo1.Lines.Add('Format of Transparency   : New');
    end
    else
    begin
     SpriteList[x].TransFormat:=1;
     Memo1.Lines.Add('Format of Transparency   : Old');
    end;
   end
   else
   begin
    SpriteList[x].TransFormat:=0;
    Memo1.Lines.Add('Transparency             : No');
   end;
   //All information now gathered from the file, for this sprite, so now create
   //the image.
   //
   //Create the bitmap container in the array
   SpriteList[x].Image:=TBitmap.Create;
   if SpriteList[x].Transparency<>SpriteList[x].PixelData then
   begin
    SpriteList[x].Image.Transparent:=True;
    SpriteList[x].Image.TransparentColor:=BGColour mod $1000000;
   end;
   //Setup buffer to create bitmap data in
   SetLength(buffer,$36);
   amt:=bitmapHeader(SpriteList[x].PixWidth,
                     SpriteList[x].ScanLines+1,
                     SpriteList[x].BPP,
                     0,
                     buffer);
   SetLength(buffer,amt);
   //Copy the palette across
   for t:=$36 to amt-1 do buffer[t]:=0;
   if SpriteList[x].BPP<16 then
    for t:=0 to Length(SpriteList[x].Palette)-1 do
     buffer[$36+t]:=SpriteList[x].Palette[t];
   //Pointer to Pixel data in bitmap
   p2:=buffer[$0A]
     +(buffer[$0B]*$100)
     +(buffer[$0C]*$10000)
     +(buffer[$0D]*$1000000);
   //Set and empty the Colours Used counter
   if SpriteList[x].BPP<16 then
   begin
    SetLength(SpriteList[x].ColoursUsed,1<<SpriteList[x].BPP);
    for t:=0 to Length(SpriteList[x].ColoursUsed)-1 do
     SpriteList[x].ColoursUsed[t]:=0;
   end;
   //Extract the sprite data
   for sy:=0 to SpriteList[x].ScanLines do
   begin
    //X co-ordinate variable to point into some bitmaps, seperate from sprites
    bx:=0;
    palette:='';
    for sx:=0 to ((SpriteList[x].WidthWord+1)*4)-1 do
    //Change so it reads in 4 bytes at a time, into a 4 element byte array
    //then we can deal with the tranparancy mask at the same time
    begin
     //Pointer to pixel in sprite
     p:=ptr+SpriteList[x].PixelData+((SpriteList[x].WidthWord+1)*sy*4)+sx;
     //Pointer to mask pixel in sprite
     if SpriteList[x].TransFormat=1 then
     begin
      tp:=SpriteList[x].Transparency+ptr+((SpriteList[x].WidthWord+1)*sy*4)+sx;
      case SpriteList[x].BPP of
       4: //4bpp
       begin
        //Read in a byte from the transparent data
        t:=data[tp+(SpriteList[x].LeftBit div 8)];
        //Allow for left hand wastage
        t:=t>>(SpriteList[x].LeftBit mod 8);
        //one byte has information for two pixels
        if sx*2<Length(SpriteList[x].Mask) then
         if sy<Length(SpriteList[x].Mask[sx*2]) then
          SpriteList[x].Mask[sx*2,sy]:=(t AND $0F)=$00;
        if (sx*2)+1<Length(SpriteList[x].Mask) then
         if sy<Length(SpriteList[x].Mask[(sx*2)+1]) then
          SpriteList[x].Mask[(sx*2)+1,sy]:=(t AND $F0)=$00;
       end;
       8: //8bpp
         if sx<Length(SpriteList[x].Mask) then
           if sy<Length(SpriteList[x].Mask[sx]) then
            SpriteList[x].Mask[sx,sy]:=data[tp]=$00;
      end;
     end;
     //New style mask
     if SpriteList[x].TransFormat=2 then
      if sx<Length(SpriteList[x].Mask) then
       if sy<Length(SpriteList[x].Mask[sx]) then
        begin
         //Get the byte
         tp:=SpriteList[x].Transparency+ptr+
             ((((((SpriteList[x].WidthWord+1)*4)+7)div 8)+3)div 4)*4*sy+
             (sx div 8);
         SpriteList[x].Mask[sx,sy]:=data[tp]and(1<<(sx mod 8))=0;
        end;
     //Pointer to pixel in bitmap
     if SpriteList[x].BPP=2 then
     begin
      //As 2bpp is getting converted to 4bpp, we need to know the new word width
      if bx*2<SpriteList[x].PixWidth then
       amt:=p2+(Cardinal(Ceil((SpriteList[x].PixWidth*4)/32))
              *(SpriteList[x].ScanLines-sy)*4)+bx
      else amt:=0;
      inc(bx,2);
     end
     else
      amt:=p2+((SpriteList[x].WidthWord+1)*(SpriteList[x].ScanLines-sy)*4)+sx;
     //Read in a byte from the picture data
     t:=data[p+(SpriteList[x].LeftBit div 8)];
     t2:=0;
     //Allow for left hand wastage
     t:=t>>(SpriteList[x].LeftBit mod 8);
     //swap the nibbles with 4bpp
     if SpriteList[x].BPP=4 then
      t:=(t<<4) OR (t>>4);
     //Expand 2bpp to 4bpp, swapping half nibbles
     if SpriteList[x].BPP=2 then
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
      if SpriteList[x].BPP=2 then
       buffer[amt+1]:=t2;
      //And count up the colours used
      case SpriteList[x].BPP of
       1:
       begin
        inc(SpriteList[x].ColoursUsed[ t AND $01]);
        inc(SpriteList[x].ColoursUsed[(t AND $02)shr 1]);
        inc(SpriteList[x].ColoursUsed[(t AND $04)shr 2]);
        inc(SpriteList[x].ColoursUsed[(t AND $08)shr 3]);
        inc(SpriteList[x].ColoursUsed[(t AND $10)shr 4]);
        inc(SpriteList[x].ColoursUsed[(t AND $20)shr 5]);
        inc(SpriteList[x].ColoursUsed[(t AND $40)shr 6]);
        inc(SpriteList[x].ColoursUsed[(t AND $80)shr 7]);
       end;
       2:
       begin
        inc(SpriteList[x].ColoursUsed[ t AND $03]);
        inc(SpriteList[x].ColoursUsed[(t AND $0C)shr 2]);
        inc(SpriteList[x].ColoursUsed[ t2 AND $03]);
        inc(SpriteList[x].ColoursUsed[(t2 AND $0C)shr 2]);
       end;
       4:
       begin
        inc(SpriteList[x].ColoursUsed[ t AND $0F]);
        inc(SpriteList[x].ColoursUsed[(t AND $F0)>>4]);
       end;
       8: inc(SpriteList[x].ColoursUsed[t]);
      end;
     end;
    end;
   end;
   //Change the Red and Blues around - 16 bpp
   if SpriteList[x].BPP=16 then
    repeat
     //Read it in as a 16 bit value
     t:=buffer[p2]
      +(buffer[p2+1])*$100;
     //Shuffle around
     t2:=((t AND $7C00)shr 10)  //Bits 10-14
        + (t AND $03E0)         //Bits 5-9
        +((t AND $001F)shl 10); //Bits 0-4
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
   if SpriteList[x].BPP=32 then
    repeat
     //Read it in as a 32 bit value
     t:=buffer[p2]
      +(buffer[p2+1])*$100
      +(buffer[p2+2])*$10000
      +(buffer[p2+3])*$1000000;
     //Shuffle around
     t2:=((t AND $00FF0000)shr 16)
        + (t AND $0000FF00)
        +((t AND $000000FF)shl 16);
     //Add to palette
//      AddToPalette((t AND $00FF0000)shr 16,
//                   (t AND $0000FF00)shr 8,
//                   (t AND $000000FF),x);
     //Put back
     buffer[p2]  := t2               mod $100;
     buffer[p2+1]:=(t2 div $100    ) mod $100;
     buffer[p2+2]:=(t2 div $10000  ) mod $100;
     buffer[p2+3]:=(t2 div $1000000) mod $100;
     //Move onto next 32 bits
     inc(p2,4);
     p:=Length(buffer)-1; //Length is an Integer, p2 is a Cardinal
    until p2>=p;
   //Count the number of colours used
   if SpriteList[x].BPP<16 then
   begin
    SpriteList[x].ColourCount:=0;
    for t:=0 to Length(SpriteList[x].ColoursUsed)-1 do
     if SpriteList[x].ColoursUsed[t]<>0 then
      inc(SpriteList[x].ColourCount);
    Memo1.Lines.Add('Number of colours used   : '+IntToStr(SpriteList[x].ColourCount));
   end;
   //Apply the mask
   if SpriteList[x].TransFormat>0 then
   begin
    //Change the pixel to the BGColour, depending on the BPP
    //Can only do this if there is a free colour in the palette
    if SpriteList[x].BPP<9 then
    begin
     ctr:=-1;
     if SpriteList[x].ColourCount<1<<SpriteList[x].BPP then
     begin
      //Find a free colour
      repeat
       inc(ctr);
      until (SpriteList[x].ColoursUsed[ctr]=0) or (ctr=1<<SpriteList[x].BPP-1);
      //Found one? then assign it in the palette
      if SpriteList[x].ColoursUsed[ctr]=0 then
      begin
       buffer[$36+(ctr*4)]  :=BGColour mod $100;
       buffer[$36+(ctr*4)+1]:=(BGColour div $100) mod $100;
       buffer[$36+(ctr*4)+2]:=(BGColour div $10000) mod $100;
       buffer[$36+(ctr*4)+3]:=(BGColour div $1000000) mod $100;
      end else ctr:=-1; //Otherwise mark as not found
     end;
     if ctr=-1 then ;//No free colours, so we'll need to convert this one to the
                     //next bpp up to get the extra colour.
//     if x=152 then
//      trans:=True;
    end;
    for sy:=0 to SpriteList[x].ScanLines do
     for sx:=0 to SpriteList[x].PixWidth-1 do
     begin
      //Is pixel transparent?
      if SpriteList[x].Mask[sx,sy] then
      begin
       //Pointer to pixel data
       p2:=buffer[$0A]
         +(buffer[$0B]*$100)
         +(buffer[$0C]*$10000)
         +(buffer[$0D]*$1000000);
       //Pointer to start of row in bitmap
       p2:=p2+((SpriteList[x].WidthWord+1)*(SpriteList[x].ScanLines-sy)*4);
       case SpriteList[x].BPP of
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
         buffer[p2+(sx*2)]  :=(((BGColour AND$F8)>>3)+((BGColour AND$F800)>>6))mod$100;
         buffer[p2+(sx*2)+1]:=(((BGColour AND$F800)>>6)+((BGColour AND$F80000)>>9))div$100;
        end;
        32:
        begin
         buffer[p2+(sx*4)]  :=BGColour mod $100;
         buffer[p2+(sx*4)+1]:=(BGColour div $100) mod $100;
         buffer[p2+(sx*4)+2]:=(BGColour div $10000) mod $100;
         buffer[p2+(sx*4)+3]:=(BGColour div $1000000) mod $100;
        end;
       end;
      end;
     end;
   end;
   //Load the buffer into the bitmap
   ms.Position:=0;
   ms.WriteBuffer(buffer[0],Length(buffer));
   ms.Position:=0;
   SpriteList[x].Image.LoadFromStream(ms);
   //Display it
   image[x]:=TImage.Create(Scrollbox1 as TComponent);
   image[x].Parent:=ScrollBox1 as TWinControl;
   image[x].Width:=SpriteList[x].Image.Width;
   image[x].Height:=SpriteList[x].Image.Height;
   image[x].OnDblClick:=@Image1DblClick;
   image[x].Tag:=x;
   image[x].Visible:=True;
   image[x].Canvas.AntialiasingMode:=amOff;
   image[x].Stretch:=True;
   image[x].Proportional:=True;
   if (ix+image[x].Width>=512) then
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
//   image[x].Picture.Bitmap.Transparent:=True;
//   image[x].Picture.Bitmap.TransparentColor:=MainForm.Color;//BGColour mod $1000000;
   image[x].Canvas.Draw(0,0,SpriteList[x].Image);
   image[x].Width:=32;
   image[x].Height:=32; // <- this does not work}
//   image[x].Transparent:=True;
   //Adjust for the next sprite position
   ix:=ix+image[x].Width+4;
   if image[x].Height>bigH then bigH:=image[x].Height;
   image[x].Hint:=SpriteList[x].Name;
   //Update the window
   ScrollBox1.Repaint;
   //Move onto the next sprite
   ptr:=ptr+SpriteList[x].Next;
  end;
  Application.ProcessMessages;
 end;
 //Free up the memory stream
 ms.Free;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
 const FileNames: array of String);
begin
 LoadSpriteFile(FileNames[0]);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
 Caption:=AppTitle+' '+AppVersion+' by Gerald J Holdsworth';
end;

procedure TMainForm.sb_SaveClick(Sender: TObject);
var
 Dir: String;
 x: Integer;
begin
 if Length(SpriteList)>0 then
 begin
  if SelectDirectoryDialog1.Execute then
  begin
   Dir:=SelectDirectoryDialog1.FileName;
   if Dir[Length(dir)]<>PathDelim then Dir:=Dir+PathDelim;
   for x:=0 to Length(SpriteList)-1 do
   begin
    //PNG
    SaveAsPng(SpriteList[x].Image,Dir+PathDelim
                                 +validateFilename(SpriteList[x].Name)+'.png',
                                 SpriteList[x].TransFormat>0);
    //Save BMP
    SpriteList[x].Image.SaveToFile(Dir+PathDelim
                                 +validateFilename(SpriteList[x].Name)+'.bmp');
   end;
  end;
 end;
end;

procedure TMainForm.SaveAsPng(screen:TBitmap;AFileName: String;Mask:Boolean);
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

function TMainForm.bitmapHeader(sizex,sizey,bpp,cols: Integer; var bmp: array of Byte): Integer;
var
 rowwidth,pal,pxd,amt: Integer;
begin
 //If BPP is not supported in bitmap, change to the next highest that is
 while (bpp<>1) and (bpp<>4) and (bpp<>8) and (bpp<>16)
 and (bpp<>24) and (bpp<>32) do
  inc(bpp);
 //Work out number of colours, under 16bpp
 if bpp<16 then
  if cols=0 then
   cols:=Round(IntPower(2,bpp));
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

procedure TMainForm.AddToPalette(r,g,b: Byte;x: Integer);
var
 ctr,found: Integer;
begin
 found:=-1;
 if Length(SpriteList[x].Palette)>0 then
  for ctr:=0 to (Length(SpriteList[x].Palette)div 3)-1 do
   if  (SpriteList[x].Palette[ ctr*3   ]=r)
   and (SpriteList[x].Palette[(ctr*3)+1]=g)
   and (SpriteList[x].Palette[(ctr*3)+2]=b) then
    found:=ctr;
 if found=-1 then
 begin
  SetLength(SpriteList[x].Palette,Length(SpriteList[x].Palette)+3);
  ctr:=Length(SpriteList[x].Palette)-3;
  SpriteList[x].Palette[ctr  ]:=r;
  SpriteList[x].Palette[ctr+1]:=g;
  SpriteList[x].Palette[ctr+2]:=b;
 end;
end;

end.
