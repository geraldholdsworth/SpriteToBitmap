program RISCOSPaint;

{$R *.dres}

uses
  Forms,
  MainUnit in 'MainUnit.PAS' {MainForm},
  BigImageUnit in 'BigImageUnit.pas' {BigImageForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := '!Paint For Windows';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TBigImageForm, BigImageForm);
  Application.Run;
end.
 
