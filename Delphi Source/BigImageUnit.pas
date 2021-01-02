unit BigImageUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons;

type
  TBigImageForm = class(TForm)
    ZoomedImage: TImage;
    Panel1: TPanel;
    lb_size: TLabel;
    sb_Save: TSpeedButton;
    SaveDialog1: TSaveDialog;
    procedure sb_SaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BigImageForm: TBigImageForm;

implementation

{$R *.dfm}

procedure TBigImageForm.sb_SaveClick(Sender: TObject);
begin
 if SaveDialog1.Execute then
  ZoomedImage.Picture.Bitmap.SaveToFile(SaveDialog1.FileName);
end;

end.
