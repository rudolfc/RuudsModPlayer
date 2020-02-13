unit dlg_aboutbox;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    BitBtn1: TBitBtn;
    LbName: TLabel;
    LbAuthor: TLabel;
    LbCopyRight: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CenterTexts;

  public

  end;

var
  AboutBox: TAboutBox;

implementation

uses
  GlobalFunctions;

{$R *.lfm}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  LbName.Caption := LbName.Caption + GetVersionInfo;
end;

procedure TAboutBox.FormShow(Sender: TObject);
begin
  CenterTexts;
end;

procedure TAboutBox.CenterTexts;
begin
  with LbName do Left := (AboutBox.ClientWidth - Width) div 2;
  with LbAuthor do Left := (AboutBox.ClientWidth - Width) div 2;
  with LbCopyRight do Left := (AboutBox.ClientWidth - Width) div 2;
end;

end.

