unit BigImageUnit;

{
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

interface

uses ExtCtrls, StdCtrls, Buttons, Dialogs, Forms, Classes;

type

  { TBigImageForm }

  TBigImageForm = class(TForm)
   lb_modeflags: TLabel;
   lb_bpp: TLabel;
   lb_OS: TLabel;
   lb_palette: TLabel;
   lb_mask: TLabel;
   lb_colours: TLabel;
   lb_mode: TLabel;
   lb_spritetype: TLabel;
   SizeLabel: TLabel;
   ModeFlagsLabel: TLabel;
   ModeFlagsPanel: TPanel;
   BPPLabel: TLabel;
   BPPPanel: TPanel;
   OSLabel: TLabel;
   OSPanel: TPanel;
   PaletteLabel: TLabel;
   PalettePanel: TPanel;
   MaskLabel: TLabel;
   MaskPanel: TPanel;
   ColoursLabel: TLabel;
   ColoursPanel: TPanel;
   ModeLabel: TLabel;
   ModePanel: TPanel;
   SpriteTypeLabel: TLabel;
   SizePanel: TPanel;
   SpriteTypePanel: TPanel;
    ZoomedImage: TImage;
    SpriteDetailsPanel: TPanel;
    lb_size: TLabel;
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BigImageForm: TBigImageForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TBigImageForm }

procedure TBigImageForm.FormPaint(Sender: TObject);
begin
 if Sender is TForm then
  MainForm.TileCanvas(TForm(Sender).Canvas);
 if Sender is TPanel then
  MainForm.TileCanvas(TPanel(Sender).Canvas);
end;

end.
