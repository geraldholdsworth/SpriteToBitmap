program RISCOSPaint;

{$MODE Delphi}

{$R *.dres}

uses
  Forms, Interfaces,
  MainUnit in 'MainUnit.pas' {MainForm},
  BigImageUnit in 'BigImageUnit.pas',
  SpriteFile, Global, {BigImageForm}
  AboutUnit in 'AboutUnit.pas',
  SettingsUnit in 'SettingsUnit.pas';

{$R *.res}

begin
 Application.Scaled:=True;
  Application.Initialize;
 Application.Title:='Sprite Converter';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TBigImageForm, BigImageForm);
  Application.CreateForm(TAboutForm, AboutForm);
 Application.CreateForm(TSettingsForm, SettingsForm);
  Application.Run;
end.
 
