program RuudsModPlayer;

uses
  LCLIntf, LCLType, LMessages, SysUtils, Dialogs, Forms, Interfaces,
{$IFNDEF MSWINDOWS}
  cthreads, (* Haiku and Linux require explicit thread support in the application *)
{$ENDIF}
  ModPlayMain, dlg_mediainfo, Dlg_MPSettings;

{$R *.res}

begin
  Application.Initialize;

  Application.CreateForm(TModMain, ModMain);
  Application.CreateForm(TMPSettings, MPSettings);
  Application.CreateForm(TFrmMediaInfo, FrmMediaInfo);

  (* Open Soundcard output *)
  ModMain.OpenWaveOutput;
  (* If we were started by File Association then load and play the file *)
  ModMain.OpenAssociatedFile;

  Application.Run;
end.
