program RuudsModPlayer;

uses
  LCLIntf, LCLType, LMessages, SysUtils, Dialogs, Forms, Interfaces,
  ModPlayMain, dlg_mediainfo, Dlg_MPSettings;

{$R *.res}

begin
  Application.Initialize;

  Application.CreateForm(TModMain, ModMain);
  Application.CreateForm(TMPSettings, MPSettings);
  Application.CreateForm(TFrmMediaInfo, FrmMediaInfo);

  (* Open Soundcard output *)
  ModMain.OpenWaveOutput;

  Application.Run;
end.
