unit AboutUnit;

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

{$MODE objFPC}{$H+}

interface

uses
  SysUtils,Variants,Classes,Controls,Forms,Dialogs,ExtCtrls,StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CreditsPanel: TPanel;
    IconImage: TImage;
    AckLabel: TLabel;
    GitHubWebsiteLabel: TLabel;
    LicenceLabel: TLabel;
    lb_Title: TLabel;
    WrittenByLabel: TLabel;
    lb_Version: TLabel;
    GHWebsiteLabel: TLabel;
    procedure CreditsPanelPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses MainUnit;

{ TAboutForm }

procedure TAboutForm.CreditsPanelPaint(Sender: TObject);
begin
 if Sender is TForm then
  MainForm.TileCanvas(TForm(Sender).Canvas);
 if Sender is TPanel then
  MainForm.TileCanvas(TPanel(Sender).Canvas);
end;

end.
