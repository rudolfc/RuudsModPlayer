unit Dlg_MPSettings;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  (* Note: The Amiga soundchip max rate is 28Khz and we only support upsampling so we don't offer lower rates! *)
  TSampleRate = (SR32000,SR44100,SR48000,SR96000,SR192000);

const
  PALSpeed        = 7093789.2;
  NTSCSpeed       = 7159090.5;
  SampleRateInts  : array[TSampleRate] of Integer = (32000,44100,48000,96000,192000);

type
  TSettings = packed record
    MyTVSpeed       : Single;
    OutSampleRate   : Integer;
    CubicAudioSynth : Boolean;
  end;

  { TMPSettings }

  TMPSettings = class(TForm)
    CB_TVStandard: TComboBox;
    CB_SampleRate: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure CB_SampleRateChange(Sender: TObject);
    procedure CB_TVStandardChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    procedure ResetSettings;

  public
    MySettings     : TSettings;

    procedure UpdateInterfaceSettings;

  end;

var
  MPSettings: TMPSettings;

implementation

uses
  GlobalVars, ModPlayMain;

{$R *.lfm}

{ TMPSettings }

procedure TMPSettings.FormCreate(Sender: TObject);
var
  MyRate: TSampleRate;
begin
  ResetSettings;

  CB_TVStandard.Items.Add('PAL');
  CB_TVStandard.Items.Add('NTSC');

  for MyRate := Low(TSampleRate) to High(TSampleRate) do
    CB_SampleRate.AddItem(IntToStr(SampleRateInts[MyRate]), nil);
end;

procedure TMPSettings.CB_TVStandardChange(Sender: TObject);
begin
  if CB_TVStandard.ItemIndex = 1 then
    MySettings.MyTVSpeed := NTSCSpeed
  else
    MySettings.MyTVSpeed := PALSpeed;
end;

procedure TMPSettings.CB_SampleRateChange(Sender: TObject);
begin
  with ModMain do
    if CBSaveWave.Enabled then
    begin
      CloseWaveOutput;
      MySettings.OutSampleRate := SampleRateInts[TSampleRate(CB_SampleRate.ItemIndex)];
      ClearBufInterConnect; (* The content of these buffers is now invalid (samplerate changed!) *)
      OpenWaveOutput;
    end
    else
      UpdateInterfaceSettings;
end;

procedure TMPSettings.FormPaint(Sender: TObject);
begin
  UpdateInterfaceSettings;
end;

procedure TMPSettings.UpdateInterfaceSettings;
var
  Index : TSampleRate;
begin
  with MySettings do
  begin
    if MySettings.MyTVSpeed = NTSCSpeed then
      CB_TVStandard.ItemIndex := 1
    else
      CB_TVStandard.ItemIndex := 0;

    for Index := Low(TSampleRate) to High(TSampleRate) do
      if SampleRateInts[Index] = MySettings.OutSampleRate then
        CB_SampleRate.ItemIndex := Ord(Index);
  end;
end;

procedure TMPSettings.ResetSettings;
begin
  with MySettings do
  begin
    (* PAL is the most often used speed (Xtal ref. for TV output) looking at the sales figures for Amiga computers globally *)
    MyTVSpeed     := PALSpeed;
    (* we use CD quality output (44.1kHz @ 16bit, two channels *)
    OutSampleRate := 44100;
    (* we go for best quality audio synthesizing *)
    CubicAudioSynth := True;
  end;
end;


end.

