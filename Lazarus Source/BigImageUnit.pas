unit BigImageUnit;


interface

uses ExtCtrls, StdCtrls, Buttons, Dialogs, Forms;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BigImageForm: TBigImageForm;

implementation

{$R *.lfm}

end.
