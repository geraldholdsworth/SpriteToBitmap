program RISCOSPaint;

{$MODE Delphi}

{$R *.dres}

uses
  Forms, Interfaces,
  MainUnit in 'MainUnit.pas' {MainForm},
  BigImageUnit in 'BigImageUnit.pas' {BigImageForm};

{$R *.res}

begin
 Application.Scaled:=True;
  Application.Initialize;
 Application.Title:='SpriteConverter';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TBigImageForm, BigImageForm);
  Application.Run;
end.
 
