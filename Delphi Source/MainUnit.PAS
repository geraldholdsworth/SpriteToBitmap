unit MainUnit;

interface

uses Winapi.Windows,System.Classes,Vcl.Graphics,Vcl.Forms,Vcl.Controls,Math,
  Vcl.Menus, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls,
  SysUtils,ShBrowseU;

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
   PaletteColours: Cardinal;                 // Number of palette entries (calculated) <16bpp
   ColoursUsed   : array of Integer;         // Tally of each colour used (calculated)
   ColourCount   : Integer;                  // Keep track of how many colours are used (calculated)
   ModeFlag,                                 // Mode flag (calculated)
   BPP,                                      // Bits per pixel (calculated)
   TransFormat   : Byte;                     // Old (1), New (2) or no (0) mask format (calculated)
   Palette       : array of Byte;            // Palette (if not in file, loaded from resource or built from pixel data)
   Image         : TBitmap;                  // Actual bitmap image
   Mask          : array of array of Boolean // Transparency Mask (converted to true/false)
  end;
  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    Memo1: TMemo;
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    sb_OpenFile: TSpeedButton;
    Splitter1: TSplitter;
    sb_Save: TSpeedButton;
    procedure sb_OpenFileClick(Sender: TObject);
    function createImage(parent: TObject;top,left,width,height: Integer): TImage;
    procedure FormCreate(Sender: TObject);
    function bitmapHeader(sizex,sizey,bpp,cols: Integer; var bmp: array of Byte): Integer;
    function validateFilename(f: String): String;
    procedure Image1DblClick(Sender: TObject);
    procedure sb_SaveClick(Sender: TObject);
    procedure AddToPalette(r,g,b: Byte;x: Integer);
  private
   image: array of TImage;
  public
   SpriteList: array of TSprite;
   const
    BGColour = $00FF00FF;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses BigImageUnit;

procedure TMainForm.FormCreate(Sender: TObject);
begin
 Application.Title:='!Paint for Windows';
end;

procedure TMainForm.Image1DblClick(Sender: TObject);
var
 i: Integer;
begin
 if Sender is TImage then
 begin
  i:=TImage(Sender).Tag;
  BigImageForm.ZoomedImage.Picture.Bitmap:=SpriteList[i].Image;
  BigImageForm.Caption:=SpriteList[i].Name;
  BigImageForm.lb_size.Caption:='Size:'+IntToStr(SpriteList[i].PixWidth)+'x'
                                       +IntToStr(SpriteList[i].ScanLines+1)
                               +' BPP:'+IntToStr(SpriteList[i].BPP);
  if SpriteList[i].BPP=2 then
   BigImageForm.lb_size.Caption:=BigImageForm.lb_size.Caption+' (converted to 4bpp)';
  BigImageForm.ShowModal;
 end;
end;

procedure TMainForm.sb_OpenFileClick(Sender: TObject);
var
  F: File;
  data,buffer: array of Byte;
  x,sprites,amt,ctr: Integer;
  ptr,
  sx,sy,bx,
  ix,iy,
  p,p2,
  t,t2,tp,
  size,
  endoffile: Cardinal;
  trans: Boolean;
  res: TResourceStream;
  ms: TMemoryStream;
  palette: String;
const
 modes: array[0..53] of Byte = (1,2,3,2,1,2,1,3,2,3,
                                4,2,3,4,3,4,3,3,1,2,
                                3,4,3,1,4,1,2,3,4,1,
                                2,3,4,1,2,3,4,1,2,3,
                                4,1,2,4,1,2,3,4,3,4,
                                1,2,3,4);
begin
 if OpenDialog.Execute then
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
  AssignFile(F,OpenDialog.FileName);
  Reset(F,1);
  size:=FileSize(F);
  SetLength(data,size+4);
  BlockRead(F,data[$04],size,amt);
  CloseFile(F);
  //Currently no check for valid sprite file
  Memo1.Lines.Clear;
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
    if (t>31) AND (t<256) AND (t<>127) then
     SpriteList[x].Name:=SpriteList[x].Name+chr(t);
   end;
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
     SpriteList[x].ModeFlag:=(SpriteList[x].ModeData AND $3F80000) shr 20
    else                                     //RISC OS 3.5
     SpriteList[x].ModeFlag:=(SpriteList[x].ModeData AND $7FFFFFFF) shr 27;
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
      SetLength(SpriteList[x].Palette,(1 shl SpriteList[x].BPP)*4);
      //Read in the resource into a temporary buffer
      res:=TResourceStream.Create(hInstance,palette,RT_RCDATA);
      res.Position:=0;
      SetLength(buffer,res.Size);
      res.ReadBuffer(buffer[0],res.Size);
      res.Free;
      //Extract the palette entries, discarding the VDU19,cn,16
      for t:=0 to (1 shl SpriteList[x].BPP)-1 do
      begin
       SpriteList[x].Palette[ t*4   ]:=buffer[(t*6)+5];//Red
       SpriteList[x].Palette[(t*4)+1]:=buffer[(t*6)+4];//Green
       SpriteList[x].Palette[(t*4)+2]:=buffer[(t*6)+3];//Blue
       SpriteList[x].Palette[(t*4)+3]:=$00;
      end;
     end;
    end;
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
    //Create the containers: image is to display on form, Bitmap is for the array
    image[x]:=createImage(ScrollBox1,iy,ix,32,32);
    image[x].OnDblClick:=Image1DblClick;
    image[x].Tag:=x;
    //Adjust for the next sprite position
    ix:=ix+36;
    if (ix>=512) then
    begin
     ix:=4;
     iy:=iy+36;
    end;
    image[x].Hint:=SpriteList[x].Name;
    //Create the bitmap container in the array
    SpriteList[x].Image:=TBitmap.Create;
    if SpriteList[x].Transparency<>SpriteList[x].PixelData then
    begin
     SpriteList[x].Image.TransparentColor:=BGColour;
     SpriteList[x].Image.Transparent:=True;
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
     SetLength(SpriteList[x].ColoursUsed,1 shl SpriteList[x].BPP);
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
{      if SpriteList[x].TransFormat=1 then
      begin
       tp:=SpriteList[x].Transparency+ptr+((SpriteList[x].WidthWord+1)*sy*4)+sx;
       case SpriteList[x].BPP of
        8: SpriteList[x].Mask[sx,sy]:=data[tp]=$00;
       end;
      end;}
{      if SpriteList[x].TransFormat=2 then
      begin
       tp:=SpriteList[x].Transparency+ptr+(Cardinal(Ceil(sx/32))*4*sy);
       SpriteList[x].Mask[sx,sy]:=((data[tp]
                                   +data[tp+1]*$100
                                   +data[tp+2]*$10000
                                   +data[tp+3]*$1000000)
                                   AND (1 shl (sx mod 32)))<>1 shl (sx mod 32);
      end;}
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
      t:=data[p];
      t2:=0;
      //Allow for left hand wastage
      t:=t shr SpriteList[x].LeftBit;
      //swap the nibbles with 4bpp
      if SpriteList[x].BPP=4 then
       t:=(t shl 4) OR (t shr 4);
      //Expand 2bpp to 4bpp, swapping half nibbles
      if SpriteList[x].BPP=2 then
      begin
       //Bits 0,1 and 4,5 are used in 2 to 4bpp conversion. Other bits are 0
       t2:=((t AND $C0) shr 6)
          + (t AND $30);
       t :=((t AND $0C) shr 2)
          +((t AND $03) shl 4);
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
         inc(SpriteList[x].ColoursUsed[t AND $01]);
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
         inc(SpriteList[x].ColoursUsed[t AND $03]);
         inc(SpriteList[x].ColoursUsed[(t AND $0C)shr 2]);
         inc(SpriteList[x].ColoursUsed[t2 AND $03]);
         inc(SpriteList[x].ColoursUsed[(t2 AND $0C)shr 2]);
        end;
        4:
        begin
         inc(SpriteList[x].ColoursUsed[t AND $0F]);
         inc(SpriteList[x].ColoursUsed[(t AND $0F)shr 4]);
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
     for sy:=0 to SpriteList[x].ScanLines do
      for sx:=0 to SpriteList[x].PixWidth-1 do
      begin
       //Is pixel transparent?
       trans:=SpriteList[x].Mask[sx,sy];
       //Pointer to pixel data
       p2:=buffer[$0A]
         +(buffer[$0B]*$100)
         +(buffer[$0C]*$10000)
         +(buffer[$0D]*$1000000);
       if trans then
       begin
        //Pointer to start of row in bitmap
        p2:=p2+((SpriteList[x].WidthWord+1)*(SpriteList[x].ScanLines-sy)*4);
        //Change the pixel to the BGColour, depending on the BPP
        //Can only do this if there is a free colour in the palette
        case SpriteList[x].BPP of
         1,2,4,8:
         begin
          if SpriteList[x].ColourCount<1 shl SpriteList[x].BPP then
          begin
           //Find a free colour
           ctr:=-1;
           repeat
            inc(ctr);
           until (SpriteList[x].ColoursUsed[ctr]=0) or (ctr=(1 shl SpriteList[x].BPP)-1);
           if SpriteList[x].ColoursUsed[ctr]=0 then
           begin
            buffer[$36+(ctr*4)]  :=BGColour mod $100;
            buffer[$36+(ctr*4)+1]:=(BGColour div $100) mod $100;
            buffer[$36+(ctr*4)+2]:=(BGColour div $10000) mod $100;
            buffer[$36+(ctr*4)+3]:=$FF;
            case SpriteList[x].BPP of
             8: buffer[p2+sx]:=ctr;
            end;
           end;
          end;
         end;
         16:
         begin
          buffer[p2+(sx*2)]  :=(((BGColour AND $F8) shr 3)+((BGColour AND $F800)shr 6))mod $100;
          buffer[p2+(sx*2)+1]:=(((BGColour AND $F800)shr 6)+((BGColour AND $F80000)shr 9))div $100;
         end;
         32:
         begin
          buffer[p2+(sx*4)]  :=BGColour mod $100;
          buffer[p2+(sx*4)+1]:=(BGColour div $100) mod $100;
          buffer[p2+(sx*4)+2]:=(BGColour div $10000) mod $100;
          buffer[p2+(sx*4)+3]:=$FF;
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
    image[x].Picture.Bitmap:=SpriteList[x].Image;
    image[x].Picture.Bitmap.TransparentColor:=BGColour;
    image[x].Picture.Bitmap.Transparent:=True;
    //Update the window
    ScrollBox1.Repaint;
    //Move onto the next sprite
    ptr:=ptr+SpriteList[x].Next;
   end;
  end;
  //Free up the memory stream
  ms.Free;
 end;
end;

procedure TMainForm.sb_SaveClick(Sender: TObject);
var
 Dir: String;
 x: Integer;
begin
 if Length(SpriteList)>0 then
 begin
  with TShBrowse.Create do begin
   Caption:='Save all images';
   UserMessage:='Select the folder to save all images to';
   if Execute then
   begin
    Dir:=Folder;
    for x:=0 to Length(SpriteList)-1 do
     SpriteList[x].Image.SaveToFile(Dir+'/'
                                  +validateFilename(SpriteList[x].Name)+'.bmp');
   end;
   Free;
  end;
 end;
end;

function TMainForm.createImage(parent: TObject;top,left,width,height: Integer): TImage;
var
 i: TImage;
begin
  i:=TImage.Create(parent as TComponent);
  i.Parent:=parent as TWinControl;
  i.Visible:=true;
  i.Top:=top;
  i.Left:=left;
  i.Width:=width;
  i.Height:=height;
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
