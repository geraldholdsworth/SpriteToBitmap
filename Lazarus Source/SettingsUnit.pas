unit SettingsUnit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

 { TSettingsForm }

 TSettingsForm = class(TForm)
  ButtonImages: TImageList;
  OSCompatLabel: TLabel;
  OSGroupBox: TGroupBox;
  ArthurOption: TRadioButton;
  RO35Option: TRadioButton;
  SaveButton: TSpeedButton;
  CancelButton: TSpeedButton;
  procedure CancelButtonClick(Sender: TObject);
  procedure SaveButtonClick(Sender: TObject);
 private

 public

 end;

var
 SettingsForm: TSettingsForm;

implementation

{$R *.lfm}

{ TSettingsForm }

procedure TSettingsForm.SaveButtonClick(Sender: TObject);
begin
 ModalResult:=mrOK;
end;

procedure TSettingsForm.CancelButtonClick(Sender: TObject);
begin
 ModalResult:=mrCancel;
end;

end.

