unit ModPlayMain;

interface

uses
  LCLIntf, LCLType,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Buttons,
  ExtCtrls, StdCtrls, ActnList, Sdl, FPTimer, GlobalVars;

  { TModMain }

Const
  //fixme: Make below setting user-selectable...
  NumOutChans   = 2;     (* we output in stereo (mono is also an option) *)

  MaxNumChBufs    = 4;                      (* we support this much input channels (M.K. files have 4 channels) *)
  MaxSampleRate   = 192000;                 (* Our max. supported output samplerate *)
  MyConBufSize    = MaxSampleRate div 2205; (* buffer inter-connection-buffer size: 20 @ 44k1 is enough (tested). *)
  DefaultTmrSpeed = 5000 / (125 * 2);       (* 125BPM = 50Hz = 20mS *)
  DefaultTckSpeed = 6;                      (* 6 ticks per row-increase in song *)

  (* Amiga sound period lookup table: up<>down is finetunes, 8 finetunes = 1 seminote; left<>right = seminotes *)
  MyPeriodTable : TPeriodTable = (
        // finetune -8
       (907,856,808,762,720,678,640,604,570,538,508,480,
	453,428,404,381,360,339,320,302,285,269,254,240,
	226,214,202,190,180,170,160,151,143,135,127,120),
        // finetune -7
       (900,850,802,757,715,675,636,601,567,535,505,477,
      	450,425,401,379,357,337,318,300,284,268,253,238,
      	225,212,200,189,179,169,159,150,142,134,126,119),
        // finetune -6
       (894,844,796,752,709,670,632,597,563,532,502,474,
      	447,422,398,376,355,335,316,298,282,266,251,237,
      	223,211,199,188,177,167,158,149,141,133,125,118),
        // finetune -5
       (887,838,791,746,704,665,628,592,559,528,498,470,
      	444,419,395,373,352,332,314,296,280,264,249,235,
      	222,209,198,187,176,166,157,148,140,132,125,118),
        // finetune -4
       (881,832,785,741,699,660,623,588,555,524,494,467,
      	441,416,392,370,350,330,312,294,278,262,247,233,
      	220,208,196,185,175,165,156,147,139,131,123,117),
        // finetune -3
       (875,826,779,736,694,655,619,584,551,520,491,463,
      	437,413,390,368,347,328,309,292,276,260,245,232,
      	219,206,195,184,174,164,155,146,138,130,123,116),
        // finetune -2
       (868,820,774,730,689,651,614,580,547,516,487,460,
      	434,410,387,365,345,325,307,290,274,258,244,230,
      	217,205,193,183,172,163,154,145,137,129,122,115),
        // finetune -1
       (862,814,768,725,684,646,610,575,543,513,484,457,
      	431,407,384,363,342,323,305,288,272,256,242,228,
      	216,203,192,181,171,161,152,144,136,128,121,114),
        // Normal
       (856,808,762,720,678,640,604,570,538,508,480,453, // C-1 to B-1
	428,404,381,360,339,320,302,285,269,254,240,226, // C-2 to B-2
	214,202,190,180,170,160,151,143,135,127,120,113),// C-3 to B-3
        // finetune +1
       (850,802,757,715,674,637,601,567,535,505,477,450, // same as above
	425,401,379,357,337,318,300,284,268,253,239,225, // but with
	213,201,189,179,169,159,150,142,134,126,119,113),// finetune +1
        // finetune +2
       (844,796,752,709,670,632,597,563,532,502,474,447, // etc,
	422,398,376,355,335,316,298,282,266,251,237,224, // finetune +2
	211,199,188,177,167,158,149,141,133,125,118,112),
        // finetune +3
       (838,791,746,704,665,628,592,559,528,498,470,444,
	419,395,373,352,332,314,296,280,264,249,235,222,
	209,198,187,176,166,157,148,140,132,125,118,111),
        // finetune +4
       (832,785,741,699,660,623,588,555,524,495,467,441,
	416,392,370,350,330,312,294,278,262,247,233,220,
	208,196,185,175,165,156,147,139,131,124,117,110),
        // finetune +5
       (826,779,736,694,655,619,584,551,520,491,463,437,
	413,390,368,347,328,309,292,276,260,245,232,219,
	206,195,184,174,164,155,146,138,130,123,116,109),
        // finetune +6
       (820,774,730,689,651,614,580,547,516,487,460,434,
	410,387,365,345,325,307,290,274,258,244,230,217,
	205,193,183,172,163,154,145,137,129,122,115,109),
        // finetune +7
       (814,768,725,684,646,610,575,543,513,484,457,431,
	407,384,363,342,323,305,288,272,256,242,228,216,
	204,192,181,171,161,152,144,136,128,121,114,108));

  MySineTable: Array[0..31] of Byte = (
  	   0, 24, 49, 74, 97,120,141,161,
	 180,197,212,224,235,244,250,253,
	 255,253,250,244,235,224,212,197,
	 180,161,141,120, 97, 74, 49, 24);

Type
  TSongLogic = record
    MyPrevSongPos,
    MySongPos,
    MyPatTabNr,
    MyPatTabPos,
    MySmpOffset,
    MyOldOffset,
    MySmpLength,
    MyOldLength,
    MySmpSkipLen,
    MyOldSkipLen,
    MyVolSlide,
    MyVolume,
    OldVolume,
    MyPortaSpeed,
    MyPortaToSpeed,
    OldPortaToSpeed,
    MyPortaToGoal,
    OldPortaToGoal,
    MyVibSpeed,
    MyVibDepth,
    OldVibSpeed,
    OldVibDepth,
    MyTremSpeed,
    MyTremDepth,
    OldTremSpeed,
    OldTremDepth,
    MyArpeggio,
    MyFineTuneOR,
    MyFinePorta,
    MyCutNote,
    MyDelayNote,
    MyPatDelayNr,
    RetrigEvery,
    MyPatLoopPos,
    MyPatLoopNr     : Integer;
    MyPatTabRunning,
    MyPatDelaying,
    MyPatBreak,
    MySongEnding,
    MySongDone      : Boolean;
    PatDecode,
    MyOldPattern,
    MyDelayPattern  : TModPattern;
  end;

  TSampleLogic = record
    MyInBufCnt,
    MyPrtaPerPart,
    MyPrtaToPerPart,
    MyVibratoPos,
    MyTremoloPos    : Integer;
    LastInSample,
    InSmpUpRemain   : Single;
    MySmpTCnt,
    MyOldInPerPart  : Integer;
    MyInSmpUp,
    SmpVibUpCnt     : Single;
    MyConBufContent : Array[0..MyConBufSize-1] of SmallInt;
    MyConBufFill    : Integer;
    MyOutBufLen     : Integer;
  end;

  AudioCallbackData = packed record
    Buffer  : PUInt8;
    ReadPtr : Integer; // in samples per total of output channels
    WritePtr: Integer; // in samples per total of output channels
    BufSize : Integer; // in samples per total of output channels
    Channels: Integer;
    BufEmpty: Boolean;
  End;

  TModMain = class(TForm)
    CBPatDebug: TCheckBox;
    CBCmdDebug: TCheckBox;
    CBSaveWave: TCheckBox;
    Ch1On: TCheckBox;
    Ch2On: TCheckBox;
    Ch3On: TCheckBox;
    Ch4On: TCheckBox;
    CbSongDebug: TCheckBox;
    Label2: TLabel;
    LRInterMix: TCheckBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    menuMPSettings: TMenuItem;
    linear_synth: TRadioButton;
    cubic_synth: TRadioButton;
    SongStartPos: TEdit;
    NextSmp: TAction;
    PrevSmp: TAction;
    ActionList1: TActionList;
    BtnPlaySong: TBitBtn;
    BtnPlayRaw: TBitBtn;
    BtnStopSong: TBitBtn;
    CBSample: TComboBox;
    CbRepeat: TCheckBox;
    RunDecInfo: TListBox;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuInfo: TMenuItem;
    MenuAbout: TMenuItem;
    MenuMediaInfo: TMenuItem;
    OpenModFile: TOpenDialog;
    procedure BtnPlayRawClick(Sender: TObject);
    procedure BtnPlaySongClick(Sender: TObject);
    procedure cubic_synthChange(Sender: TObject);
    procedure linear_synthChange(Sender: TObject);
    procedure PlayMySong;
    procedure BtnStopSongClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure menuMPSettingsClick(Sender: TObject);
    procedure ParseSpeedControl;
    procedure ParseRunControl;
    procedure SetMPState(NewState: MP_State);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuFileOpenClick(Sender: TObject);
    procedure MenuMediaInfoClick(Sender: TObject);
    procedure NextSmpExecute(Sender: TObject);
    procedure PrevSmpExecute(Sender: TObject);
    procedure SongStartPosEditingDone(Sender: TObject);
    procedure ClearBufInterConnect;

  private

    MyCircBufData  : AudioCallbackData;
    PMywfx         : TSDL_AudioSpec;
    IO_Timer       : TFPTimer;

    MySongLogic    : Array[1..MaxNumChBufs] of TSongLogic;
    MySampleLogic  : Array[1..MaxNumChBufs] of TSampleLogic;

    TickSpeed      : Byte;
    TmrInterval    : Single;
    MasterVolume   : Integer;
    MySongStartPos : Integer;
    MyWaveFile     : FILE;
    MyWaveHeader   : TWavHeader;
    MyOpenInFile   : String;
    MyJmpBreak     : Integer;
    MyCubicSynth   : Boolean;

  const
    (* the longest buffer getting used is @ 32PBM = 78mS * 31 ticks = 2418mS * Samples/sec. *)
    MonoMaxBufSizeNeeded = 78*31*MaxSampleRate div 1000;
    (* in samples, actual size used is set at approx. 0.2 Seconds in size *)
    CircBufSize = 40000; //in samples
  var
    MyChBuf   : Array[1..MaxNumChBufs, 0..  (1*MonoMaxBufSizeNeeded)] of SmallInt; (* mono channel output buffers *)
    MyConvBuf : Array[0..(2*MonoMaxBufSizeNeeded)] of SmallInt;                    (* Stereo mixed output buffer *)
    MyCircBuf : Array[0..(4*CircBufSize)] of Uint8;                                (* Circular buffer interface to SDL *)


    procedure ExecTick(Sender: TObject);
    procedure UpdateMyVolSlide(MyCh, MyTick: Integer);
    function  ExecNote(MyCh: Integer): Boolean;


  public

    Procedure ResetMyPattern(var MyPattern: TModPattern);
    function  StartWaveFile(FName: String): Boolean;
    function  WriteWaveChunk(MySize: Integer): Boolean;
    function  CompleteWaveFile: Boolean;
    function  LoadFile(MyFile: String): Boolean;
    procedure HandleNewFile(MyFile: String);
    procedure StopPlaying;
    function  MixAndOutputSamples(PlayRaw: Boolean): Boolean;
    function  PlaySample(MyCh: Integer; MySmpPtr: PInt8; MyBufLen: Integer = 0; MyFirstStart: Integer = 0;
                         MyRptLen: Integer = 0; MyRptStart: Integer = 0; AmigaSpeed: Word = 428; FineTune: ShortInt = 0): Boolean;
    Function  AllBufsDone: Boolean;
    procedure OpenAssociatedFile;
    procedure OpenWaveOutput;
    procedure CloseWaveOutput;

  end;

var
  ModMain: TModMain;

implementation

uses
  dlg_aboutbox, dlg_mediainfo, GlobalFunctions, Dlg_MPSettings, Math;

{$R *.lfm}

{ TModMain }

Procedure TModMain.ResetMyPattern(var MyPattern: TModPattern);
begin
  With MyPattern do
  begin
    SampleNumber := 0;
    SamplePeriod := 0;
    EffectNumber := 0;
    EffectParam  := 0;
  end;
end;


procedure TModMain.ExecTick(Sender: TObject);
var
  Ch       : Integer;
  S        : String;
  DoUpdate : Boolean;
  MyResult : Boolean;
BEGIN
  if (ModPlayerState < MPPlaying) or MyAppClosing then exit;
  (* make sure we don't overtake ourselves *)
  IO_Timer.Enabled := False;

  (* please note that all tick based effects are handled within the PlaySample routine/engine.. *)
  S := '';
  DoUpdate := False;
  For Ch := 1 to MyMediaRec.Channels do
    with MySongLogic[Ch] do
      if MySongPos <> MyPrevSongPos then
        DoUpdate := True;

  if DoUpdate and CBSongDebug.Checked then
  begin
    For Ch := 1 to MyMediaRec.Channels do
    begin
      with MySongLogic[Ch] do
      begin
        S := S + 'Ch' + IntToStr(Ch) + Format(': SongPos %03d', [MySongPos]) + Format(', Table %02d', [MyPatTabNr]) + '; ';
        MyPrevSongPos := MySongPos;
      end;
      if MySongLogic[Ch].MySongEnding then S := 'Song ending.';
    end;

    if not MyAppClosing then
      RunDecInfo.Items.Insert(0, S);
  end;

  (* Process all channels *)
  ParseSpeedControl;
  (* Note: The 'forward' playing order of channels is important (in case of multiple jump/break cmd's in one row) *)
  RunDecInfo.Items.BeginUpdate;
  For Ch := MyMediaRec.Channels downto 1 do
  begin
    MyResult := ExecNote(Ch);
    if not MyResult then break;
  end;
  RunDecInfo.Items.EndUpdate;
  if MyResult then
  begin
    MixAndOutputSamples(False);
    ParseRunControl;
  end;

  (* If we reached the end of the song let the currently queued buffers play out *)
  if (ModPlayerState = MPStopped) or not MyResult then
    while not AllBufsDone do sleep(5);

  (* We're done (note: 'StopPlaying' stops the timer) *)
  if (ModPlayerState <> MPStopped) and not MyAppClosing then
    IO_Timer.Enabled := True
  else
  begin
    SetMPState(MPStopped);
    StopPlaying;
  end;
END;

procedure TModMain.ParseSpeedControl;
var
  MyCh  : Integer;
begin
  For MyCh := 1 to MyMediaRec.Channels do
  begin
    with MySongLogic[MyCh] do
    begin
      if not MySongEnding then
      begin
        (* First update MyPatTabPos if needed (see routine ExecNote) *)
        if not MyPatTabRunning and not MyPatBreak then MyPatTabPos := 0;
        (* Fetch our channel's Pattern *)
        PatDecode := DecodePattern(MyPatternPtr[(MyCh-1)+
                    (MyPatTabPos*MyMediaRec.Channels)+
                    (MyPatTabNr*MyMediaRec.Channels*64)], True);

        (* Process effect 'Set Speed' (effects all channels) *)
        if PatDecode.EffectNumber = 15 then
        begin
          (* Range: xy = 00h-1Fh for speed; xy = 20h-FFh for BPM *)
          (* Note: The rate in Hz is equal to Hz = bpm * 2 / 5 (125BPM = 50Hz = 20mS) *)
          if PatDecode.EffectParam >= 32 then
            TmrInterval := 5000 / (PatDecode.EffectParam * 2)
          else
            TickSpeed := PatDecode.EffectParam;

          (* We don't accept coming to a full stop in sample processing *)
          if TickSpeed = 0 then Inc(TickSpeed);
        end;
      end;
    end;
  end;
end;

procedure TModMain.ParseRunControl;
var
  ChOut, Chin,
  ChSongDone   : Integer;
begin
  ChSongDone := 0;
  (* Scan for commands in reverse order since the one issued on the highest channel has priority *)
  for ChIn := MyMediaRec.Channels downto 1 do
    with MySongLogic[ChIn] do
    begin
      (* Copy 'Pattern Loop' cmd -jump- results to all channels *)
      if (PatDecode.EffectNumber = 14) and ((PatDecode.EffectParam shr 4) = 6) then
      begin
        for ChOut := 1 to MyMediaRec.Channels do
        begin
          (* Note: No -not- copy 'MyPatLoopPos' and 'MyPatLoopNr' since these are needed -per channel- (for nested loops) *)
          MySongLogic[ChOut].MyPatTabRunning := MyPatTabRunning;
          MySongLogic[ChOut].MyPatbreak      := MyPatbreak;
          MySongLogic[ChOut].MyPatTabPos     := MyPatTabPos;
          MySongLogic[ChOut].MySongPos       := MySongPos;
          MySongLogic[ChOut].MyPatTabNr      := MyPatTabNr;
          MySongLogic[ChOut].MySongEnding    := MySongEnding;
          MySongLogic[ChOut].MySongDone      := MySongDone;

          (* We explicitly only execute 'the most recent' -jump- command: kill all others. *)
          (* Note: Since 'ParseRunControl' is executed -after- each channel processed its commands,
             'ParseRunControl' in fact corrects already done channel-internal jumps on lower channels. *)
          if (ChOut <> ChIn) and
             ((MySongLogic[ChOut].PatDecode.EffectNumber = 14) and ((MySongLogic[ChOut].PatDecode.EffectParam shr 4) = 6)) then
          begin
            MySongLogic[ChOut].PatDecode.EffectNumber := 0;
            MySongLogic[ChOut].PatDecode.EffectParam  := 0;
          end;
        end;
      end;

      (* Copy 'Position Jump' cmd results to all channels *)
      if PatDecode.EffectNumber = 11 then
      begin
        for ChOut := 1 to MyMediaRec.Channels do
        begin
          MySongLogic[ChOut].MyPatTabRunning := MyPatTabRunning;
          MySongLogic[ChOut].MySongPos       := MySongPos;
          MySongLogic[ChOut].MyPatTabNr      := MyPatTabNr;
          MySongLogic[ChOut].MySongEnding    := MySongEnding;
          MySongLogic[ChOut].MySongDone      := MySongDone;
          (* No Pattern Loop active *)
          MySongLogic[ChOut].MyPatLoopPos := -1;
          MySongLogic[ChOut].MyPatLoopNr := 0;
          (* We explicitly only execute 'the most recent' command: kill all others. *)
          if (ChOut <> ChIn) and
             ((MySongLogic[ChOut].PatDecode.EffectNumber = 11) or (MySongLogic[ChOut].PatDecode.EffectNumber = 13)) then
          begin
            MySongLogic[ChOut].PatDecode.EffectNumber := 0;
            MySongLogic[ChOut].PatDecode.EffectParam  := 0;
          end;
        end;
      end;
      (* Copy 'Pattern break' cmd results to all channels *)
      if PatDecode.EffectNumber = 13 then
      begin
        for ChOut := 1 to MyMediaRec.Channels do
        begin
          MySongLogic[ChOut].MyPatTabRunning := MyPatTabRunning;
          MySongLogic[ChOut].MyPatbreak      := MyPatbreak;
          MySongLogic[ChOut].MyPatTabPos     := MyPatTabPos;
          MySongLogic[ChOut].MySongPos       := MySongPos;
          MySongLogic[ChOut].MyPatTabNr      := MyPatTabNr;
          MySongLogic[ChOut].MySongEnding    := MySongEnding;
          MySongLogic[ChOut].MySongDone      := MySongDone;
          (* No Pattern Loop active *)
          MySongLogic[ChOut].MyPatLoopPos := -1;
          MySongLogic[ChOut].MyPatLoopNr := 0;
          (* We explicitly only execute 'the most recent' (coupled) command: kill all others. *)
          if (ChOut <> ChIn) and
             ((MySongLogic[ChOut].PatDecode.EffectNumber = 11) or (MySongLogic[ChOut].PatDecode.EffectNumber = 13)) then
          begin
            MySongLogic[ChOut].PatDecode.EffectNumber := 0;
            MySongLogic[ChOut].PatDecode.EffectParam  := 0;
          end;
        end;
      end;
      (* Copy 'Pattern Delay' cmd results to all channels (they need to process it as well) *)
      if (PatDecode.EffectNumber = 14) and ((PatDecode.EffectParam shr 4) = $e) then
      begin
        for ChOut := 1 to MyMediaRec.Channels do
        begin
          MySongLogic[ChOut].MyPatDelayNr  := MyPatDelayNr;
          MySongLogic[ChOut].MyPatDelaying := MyPatDelaying;
          MySongLogic[ChOut].MyPatTabRunning := MyPatTabRunning;
          MySongLogic[ChOut].MyPatbreak      := MyPatbreak;
          MySongLogic[ChOut].MyPatTabPos     := MyPatTabPos;
          MySongLogic[ChOut].MySongPos       := MySongPos;
          MySongLogic[ChOut].MyPatTabNr      := MyPatTabNr;
          MySongLogic[ChOut].MySongEnding    := MySongEnding;
          MySongLogic[ChOut].MySongDone      := MySongDone;

          (* We explicitly only execute 'the most recent' command: kill all others. *)
          if (ChOut <> ChIn) and
             ((MySongLogic[ChOut].PatDecode.EffectNumber = 14) and ((MySongLogic[ChOut].PatDecode.EffectParam shr 4) = $e)) then
          begin
            MySongLogic[ChOut].PatDecode.EffectNumber := 0;
            MySongLogic[ChOut].PatDecode.EffectParam  := 0;
          end;
        end;
      end;
      (* Check for/signal song end: Wait for all channels to finish their songs *)
      if MySongDone then Inc(ChSongDone);
    end;

  (* If all channels finished playing signal complete stop. *)
  if ChSongDone = MyMediaRec.Channels then SetMPState(MPStopped);
end;

procedure TModMain.SetMPState(NewState: MP_State);
begin
  OldModPlayerState := ModPlayerState;
  ModPlayerState := NewState;
end;

procedure TModMain.FormCreate(Sender: TObject);
var
  a       : Integer;
  {$IFDEF MSWINDOWS}
  WinVer  : Single;
  {$ENDIF}
  SdlVerC : TSDL_version;
begin
  cubic_synth.Checked := True;
  MyCubicSynth := True;
  WaveOutErrReported := False;

  MyAppClosing := False;

  MyMediaRec.FileLoaded := False;
  MyFileHeader.FileID:= '    ';
  TickSpeed := DefaultTckSpeed;
  MasterVolume := 64;
  ModPlayerState := MPStopped;
  OldModPlayerState := MPStopped;
  WaveOutIsOpen := False;
  MySongStartPos := 0;
  TFileRec(MyWaveFile).Mode := fmClosed;
  MyOpenInFile := '';
  OrigFormatFile := False;
  OrigFmtTmrSpeed := DefaultTmrSpeed;

  (* use TFPTimer *)
  IO_Timer := TFPTimer.Create(nil);

  SDL_VERSION(SdlVerC);
  RunDecInfo.Items.Insert(0,
    'SDL compile-time version ' + IntToStr(SdlVerC.major) + '.' + IntToStr(SdlVerC.minor) + '.' + IntToStr(SdlVerC.patch));
  SDL_Init(SDL_INIT_AUDIO);

  {$IFDEF MSWINDOWS}
  RunDecInfo.Items.Insert(0, 'Windows version ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion));
  WinVer := Win32MajorVersion + Win32MinorVersion;
  if WinVer < 6.1 then
    IO_Timer.UseTimerThread := False (* Laz: XP SP3 totally messes up here if a seperate thread is used.. *)
  else
  {$ENDIF}
    IO_Timer.UseTimerThread := True; (* Laz: Improves accuracy a lot! (now +/- 2-3mS variation, otherwise +/- 10mS..) *)

  IO_Timer.Interval := 5; (* The shortest interval needed is 9.8mS (255 BPM) so this is fast enough and it may even vary.. *)
  IO_Timer.Enabled := False;
  IO_Timer.OnTimer := ExecTick;

  CBSample.AddItem('All',nil);
  for a := 1 to 31 do
    CBSample.AddItem('#' +IntToStr(a), nil);
  CBSample.ItemIndex := 0;

  if NumOutChans < 2 then LRInterMix.Enabled := False;
end;

procedure TModMain.OpenAssociatedFile;
begin
  if ParamCount > 0 then
  begin
    (* Only handle the first parameter given (behind our own filename) *)
    HandleNewFile(ParamStr(1));
    SetMPState(MPPlaying);
    PlayMySong;
  end;
end;

procedure OutputSamplesToSDL(Userdata: Pointer; Stream: PUInt8; Len: Integer); cdecl;
var
  a: Integer;
begin
  with (AudioCallbackData(Userdata^)) do
  begin
    a := 0;

    if BufEmpty then
    begin
      while a < Len do
      begin
        PChar(Stream)[a] := Char(0);
        Inc(a);
      end;
      exit;
    end;

    while a < Len do
    begin
      if Channels = 2 then
      begin
        (* Left *)
        PChar(Stream)[a+0] := PChar(Buffer)[ReadPtr*4+0];
        PChar(Stream)[a+1] := PChar(Buffer)[ReadPtr*4+1];
        (* Right *)
        PChar(Stream)[a+2] := PChar(Buffer)[ReadPtr*4+2];
        PChar(Stream)[a+3] := PChar(Buffer)[ReadPtr*4+3];
        Inc(a,4);
      end
      else
      begin
        (* Mono *)
        PChar(Stream)[a+0] := PChar(Buffer)[ReadPtr*2+0];
        PChar(Stream)[a+1] := PChar(Buffer)[ReadPtr*2+1];
        Inc(a,2);
      end;
      Inc(ReadPtr);
      if ReadPtr > BufSize-1 then ReadPtr := 0;
      if ReadPtr = WritePtr then
      begin
        BufEmpty := True;
        while a < Len do
        begin
          PChar(Stream)[a] := Char(0);
          Inc(a);
        end;
      end;
    end;
  end;
end;

procedure TModMain.OpenWaveOutput;
var
  MyStat: Integer;
begin
  if WaveOutIsOpen then exit;

  PMywfx.channels := NumOutChans;
  PMywfx.freq := MySettings.OutSampleRate;
  PMywfx.format := AUDIO_S16LSB;
  PMywfx.samples := MySettings.OutSampleRate div 100;
  PMywfx.callback := OutputSamplesToSDL;
  MyCircBufData.BufSize := Round(CircBufSize * (MySettings.OutSampleRate / MaxSampleRate));
  MyCircBufData.BufEmpty := True;
  MyCircBufData.ReadPtr := 0;
  MyCircBufData.WritePtr := 0;
  MyCircBufData.channels := NumOutChans;
  MyCircBufData.Buffer := PUInt8(Addr(MyCircBuf));
  PMywfx.userdata := Addr(MyCircBufData);

  MyStat := SDL_OpenAudio(PSDL_AudioSpec(Addr(PMywfx)), NIL);
  if MyStat < 0 then
    RunDecInfo.Items.Insert(0, 'Could not open Wave output device!')
  else
    WaveOutIsOpen := True;

  (* Make sure both output channels (stereo) are decently set.. (on mono input only left is open by default apparantly!) *)
  if WaveOutIsOpen then
  begin
    WaveOutErrReported := False;
  end;
end;

procedure TModMain.CloseWaveOutput;
begin
  if WaveOutIsOpen then SDL_CloseAudio;

  WaveOutIsOpen := False;
  (* Reset circular buffer *)
  with MyCircBufData do
  begin
    ReadPtr := 0;
    WritePtr := 0;
    BufEmpty := True;
  end;
end;

procedure TModMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MyAppClosing := True;
  SetMPState(MPStopped);
  StopPlaying;

  while not AllBufsDone do sleep(5);

  if WaveOutIsOpen then
  begin
    (* Close soundcard output *)
    CloseWaveOutput;
  end;
  SDL_Quit;

  IO_Timer.Enabled := False;
  IO_Timer.Free;

  (* Update and close the wavefile if it was open *)
  CompleteWaveFile;

  (* Force us seeing errors if we still reference RunDecInfo somewhere at this time (debug help) *)
  RunDecInfo.Clear;
  RunDecInfo.Free;
  RunDecInfo := nil;
end;

procedure TModMain.ClearBufInterConnect;
var
  Cnt : Integer;
begin
  for Cnt := 1 to 4 do
    with MySampleLogic[Cnt] do
    begin
      (* Force all upsampling related items to be recalculated *)
      MyInBufCnt := -1;
      (* Our 'previous buffer's last sample' is zero *)
      LastInSample := 0;
      InSmpUpRemain := 0;
      (* Connection buffer is empty *)
      MyConBufFill := 0;
    end;
end;

procedure TModMain.BtnPlaySongClick(Sender: TObject);
begin
  if not MyMediaRec.FileLoaded then exit;

  BtnPlaySong.Enabled := False;
  (* still playing 'raw' sample(s): quit that first *)
  if ModPlayerState = MPPlayingRaw then StopPlaying;

  if ModPlayerState = MPPlaying then
  begin
    SetMPState(MPPaused);
    StopPlaying;
    BtnPlaySong.Caption := 'Cont. song';
  end
  else
  begin
    SetMPState(MPPlaying);
    PlayMySong;
  end;
  BtnPlaySong.Enabled := True;
end;

procedure TModMain.cubic_synthChange(Sender: TObject);
begin
  MySettings.CubicAudioSynth := True;
end;

procedure TModMain.linear_synthChange(Sender: TObject);
begin
  MySettings.CubicAudioSynth := False;
end;

procedure TModMain.PlayMySong;
var
  Cnt: Integer;
begin
  if not WaveOutIsOpen then
  begin
    OpenWaveOutput;
    if WaveOutIsOpen then
      RunDecInfo.Items.Insert(0, 'Wave Output re-opened successfully..');
  end;

  BtnPlaySong.Caption := 'Pause song';
  (* Activate Wave file writer if requested *)
  CBSaveWave.Enabled := False;
  if CBSaveWave.Checked then StartWaveFile(MyOpenInFile + '.wav');
  RunDecInfo.Enabled := False;

  if OldModPlayerState <> MPPaused then
  begin (* Start song fresh (so no resume here) *)
    if CBSongDebug.Checked or CBPatDebug.Checked then RunDecInfo.Items.Clear;

    (* No Pattern Jump occurred *)
    MyJmpBreak := -1;

    for Cnt := 1 to 4 do
    begin
      (* Song player logic reset *)
      with MySongLogic[Cnt] do
      begin
        MyOldSkipLen := 0;
        (* Select song starting pattern table *)
        MyPrevSongPos := -1;
        MySongPos := MySongStartPos;
        MyPatTabNr := MyFileHeader.Patterns[MySongPos];
        MyPatTabRunning := False;
        MyPatBreak := False;
        (* Workaround: Some files have illegal entries not tied to the pattern tables, jump to next song position(s) if encountered.. *)
        while MyPatTabNr > (MyMediaRec.MaxPatTables - 1) do
        begin
          inc(MySongPos);
          (* Fail-safe to not read outside the song patterns array! (Song end reached, stopping song right away.) *)
          if MySongPos >= MyFileHeader.SongLength then SetMPState(MPStopped);
          (* Select song starting pattern table *)
          MyPatTabNr := MyFileHeader.Patterns[MySongPos];
        end;
        (* reset song-internal history *)
        ResetMyPattern(MyOldPattern);
        (* our volume is stable (not sliding) *)
        MyVolSlide := 0;
        (* start at Max volume *)
        MyVolume := 64;
        OldVolume := 64;
        (* Trigger samples normally *)
        RetrigEvery := 0;
        MySmpSkipLen := 0;
        (* No Porta active *)
        MyPortaSpeed := 0;
        MyFinePorta := 0;
        (* No PortaTo active *)
        MyPortaToSpeed := 0;
        OldPortaToSpeed := 0;
        OldPortaToGoal := 0;
        (* No Vibrato active *)
        MyVibSpeed := 0;
        MyVibDepth := 0;
        OldVibSpeed := 0;
        OldVibDepth := 0;
        (* No Tremolo active *)
        MyTremSpeed := 0;
        MyTremDepth := 0;
        OldTremSpeed := 0;
        OldTremDepth := 0;
        (* No Arpeggio active *)
        MyArpeggio := 0;
        (* No Finetune 'override' active *)
        MyFineTuneOR := -1;
        (* No Cutnote active *)
        MyCutNote := -1;
        (* No Delaynote active *)
        MyDelayNote := -1;
        ResetMyPattern(MyDelayPattern);
        (* No Pattern Delay active *)
        MyPatDelayNr := 0;
        MyPatDelaying := False;
        (* No Pattern Loop active *)
        MyPatLoopPos := -1;
        MyPatLoopNr := 0;
        (* We are _starting_ a song *)
        MySongEnding := False;
        MySongDone := False;
      end;

      (* Engine logic reset *)
      with MySampleLogic[Cnt] do
      begin
        MyInBufCnt := -1;
        MyPrtaPerPart := 0;
        MyPrtaToPerPart := 0;
        MyVibratoPos := 0;
        MyTremoloPos := 0;
        MyOldInPerPart := -1;
        (* Our 'previous buffer's last sample' is zero (we are not connected there since we are just starting a new song!) *)
        LastInSample := 0;
        InSmpUpRemain := 0;
        (* Reset internal Tick Counter *)
        MySmpTCnt := 0;
        (* No upsample factor known yet *)
        MyInSmpUp := 0;
        SmpVibUpCnt := 0;
        (* Connection buffer is empty *)
        MyConBufFill := 0;
        (* We have not generated any output yet *)
        MyOutBufLen := 0;
      end;
    end;

    (* Set default speed *)
    if OrigFormatFile then
      TmrInterval := OrigFmtTmrSpeed
    else
      TmrInterval := DefaultTmrSpeed;
    TickSpeed := DefaultTckSpeed;
  end;

  (* Start *)
  IO_Timer.Enabled := True;
  SDL_PauseAudio(0);
end;

procedure TModMain.UpdateMyVolSlide(MyCh, MyTick: Integer);
begin
  with MySongLogic[MyCh] do
  begin
    (* update our current volume in accorance with effect 10 and 12.. *)
    if MyVolSlide + MyVolume >= 64 then
    begin
      MyVolume := 64;
      MyVolSlide := 0;
    end
    else
    begin
      if MyVolSlide + MyVolume <= 0 then
      begin
        MyVolume := 0;
        MyVolSlide := 0;
      end
      else
        MyVolume := MyVolume + MyVolSlide;
    end;

    (* execute effect Cut Note if requested *)
    if MyTick = MyCutNote then
    begin
      MyVolume := 0;
      MyVolSlide := 0;
    end;
  end;

  (* Please note: we don't touch the application's volume slider, let the user do that. We simply mix the audio to volume.. *)
end;

function TModMain.ExecNote(MyCh: Integer): Boolean;
var
  d, MyShowPos : Integer;
  HasNote,
  RetrigSample : Boolean;
  S            : String;

procedure DebugString(MyText: String);
begin
  if CBCmdDebug.Checked then
    RunDecInfo.Items.Insert(0, '>> ' + MyText);
end;

procedure DoRetrigParamUpdate;
var
  MyParam : Integer;
begin
  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    (* We _must_ process effect Delay Note first since it might change PatDecode! *)
    MyDelayNote := -1;
    if PatDecode.EffectNumber = 14 then (* cmd: Extended commands *)
    begin
      if (PatDecode.EffectParam shr 4) = $d then (* cmd: effect Delay Note *)
      begin
        MyDelayNote := PatDecode.EffectParam and $0f;
        (* We must ignore this effect on Tick0, so if 0 is specified *)
        if MyDelayNote = 0 then MyDelayNote := -1;
        if MyDelayNote >= 0 then
        begin
          (* We might actually execute the delayed note: so save it.. *)
          MyDelayPattern := PatDecode;
          (* .. and for now keep running the previous effects. *)
          PatDecode := MyOldPattern;
          (* Note: If the old effect is a jump and the delayed note is never executed the jump will occur. *)
        end;
      end;
    end;

    (* Delay Note is done, so _now_ we can process the other effects on the possibly updated PatDecode *)
    MySmpSkipLen := 0;
    if PatDecode.EffectNumber = 9 then (* cmd: effect Set sample offset (= retrigger note) *)
    begin
      (* MySmpSkipLen is given in pages of -Bytes- ! *)
      MySmpSkipLen := PatDecode.EffectParam * 256;
      if MySmpSkipLen = 0 then MySmpSkipLen := MyOldSkipLen;
      if MySmpSkipLen >= MySmpLength then MySmpSkipLen := MySmpLength - 1;
      MyOldSkipLen := MySmpSkipLen;
      (* We should use our old volume on the already playing, now retriggering sample *)
    end;

    RetrigEvery := 0;
    MyCutNote := -1;
    if PatDecode.EffectNumber = 14 then         (* cmd: Extended commands *)
    begin
      if (PatDecode.EffectParam shr 4) = 9 then (* cmd: effect Retrigger note every x ticks *)
        RetrigEvery := PatDecode.EffectParam and $0f;

      if (PatDecode.EffectParam shr 4) = 6 then (* cmd: effect Pattern Loop *)
      begin
        MyParam := PatDecode.EffectParam and $0f;
        if MyParam = 0 then            (* Note current row as starting position for the loop *)
        begin
          MyPatLoopPos := MyPatTabPos;
          DebugString('Pat loop ch' + IntToStr(MyCh) + ': Setting start position: '+inttostr(MyPatLoopPos));
        end
        else
        begin
          if MyPatLoopNr = 0 then      (* Note number of loops to make *)
          begin
            MyPatLoopNr := MyParam;
            DebugString('Pat loop ch' + IntToStr(MyCh) + ': Set nr of loops to make: '+inttostr(MyPatLoopNr));
          end
          else
            Dec(MyPatLoopNr);          (* Update loop counter *)
          if MyPatLoopNr > 0 then      (* Initiate next loop if we're not done looping yet *)
          begin
            DebugString('Pat loop ch' + IntToStr(MyCh) + ': Jumping back, jumps to go: '+inttostr(MyPatLoopNr - 1));
            if MyPatLoopPos >= 0 then
              MyPatTabPos := MyPatLoopPos - 1 (* We increment again later so we restart correctly. *)
            else
              Dec(MyPatTabPos);               (* No start position was set, repeat only the current row. *)
          end
          else
            DebugString('Pat loop ch' + IntToStr(MyCh) + ': Done, continuing pat table walk.');
        end;
      end;

      if (PatDecode.EffectParam shr 4) = $c then (* cmd: effect Cut Note *)
      begin
        (* Please note: if MyCutNote is zero then it's ignored (seems correct, specified). *)
        MyCutNote := PatDecode.EffectParam and $0f;
      end;

      if (PatDecode.EffectParam shr 4) = $e then (* cmd: effect Pattern Delay *)
      begin
        (* Only 'Start' this effect if not already running *)
        if not MyPatDelaying then
          MyPatDelayNr := PatDecode.EffectParam and $0f;
      end;
    end;
  end;
end;

procedure DoVolParamUpdate(NewInstr, NewNote: Boolean);
var
  MyParam : Integer;
begin
  with MySongLogic[MyCh] do
  begin
    (* Only exec Slide-effect if currently requested *)
    MyVolSlide := 0;

    if (PatDecode.EffectNumber = 10) or  (* cmd: effect Volume slide *)
       (PatDecode.EffectNumber = 5) or   (* cmd: effect Porta to Note + Volume slide *)
       (PatDecode.EffectNumber = 6) then (* cmd: effect Vibrato + Volume slide *)
    begin
      (* Only exec command if we have just -one- direction *)
      if (PatDecode.EffectParam and $f0 = 0) or (PatDecode.EffectParam and $0f = 0) then
      begin
        if PatDecode.EffectParam and $f0 <> 0 then   //slide up
          MyVolSlide := PatDecode.EffectParam shr 4
        else
          if PatDecode.EffectParam and $0f <> 0 then //slide down
            MyVolSlide := (PatDecode.EffectParam and $0f) * -1;
      end;
    end;

    if PatDecode.EffectNumber = 14 then  (* cmd: Extended commands *)
    begin
      if (PatDecode.EffectParam shr 4) = $a then (* cmd: effect Fine Volume Slide Up *)
      begin
        MyParam := PatDecode.EffectParam and $0f;
        if (MyVolume + MyParam) <= 64 then
          Inc(MyVolume, MyParam)
        else
          MyVolume := 64;
      end;

      if (PatDecode.EffectParam shr 4) = $b then (* cmd: effect Fine Volume Slide Down *)
      begin
        MyParam := PatDecode.EffectParam and $0f;
        if MyVolume - MyParam >= 0 then
          Dec(MyVolume, MyParam)
        else
          MyVolume := 0;
      end;
    end;

    (* Only restore Volume to 'default' if a sample is specified *)
    if NewInstr then
    begin
      MyVolume := MySampleInfo[PatDecode.SampleNumber-1].Volume;
      OldVolume := MyVolume;
    end;

    if PatDecode.EffectNumber = 12 then (* cmd: effect Set Volume (0..64) *)
    begin
      MyVolume := PatDecode.EffectParam;
      OldVolume := MyVolume;
    end;
  end;
end;

procedure DoPortaParamUpdate(NewInstr, NewNote: Boolean);
begin
  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    if NewNote then
    begin
      (* Reset/stop Porta up/down on every new note (!) *)
      MyPortaSpeed := 0;  (* effect stop *)
      MyPrtaPerPart := 0; (* engine reset *)
      (* Reset/stop Fine Porta up/down also *)
      MyFinePorta := 0;
      (* We must keep PortaTo going if it was running before even if we have a new note! *)
      if (PatDecode.EffectNumber <> 3) and (PatDecode.EffectNumber <> 5) then
      begin
        (* Reset/stop Porta-to only if our current command isn't this one (!) *)
        MyPortaToSpeed := 0;  (* effect stop *)
        MyPrtaToPerPart := 0; (* engine reset *)
      end;
    end;

    if PatDecode.EffectNumber = 1 then   (* cmd: effect Porta up *)
      MyPortaSpeed := -PatDecode.EffectParam
    else
      if PatDecode.EffectNumber = 2 then (* cmd: effect Porta down *)
        MyPortaSpeed := PatDecode.EffectParam
      else
        MyPortaSpeed := 0; (* no Porta up/down cmd, stop effect, but keep it's period influence in place! *)

    if PatDecode.EffectNumber = 3 then   (* cmd: effect Porta to Note *)
    begin
      if PatDecode.EffectParam <> 0 then
      begin
        MyPortaToSpeed := PatDecode.EffectParam;
        OldPortaToSpeed := MyPortaToSpeed;
      end
      else
        MyPortaToSpeed := OldPortaToSpeed;
      if PatDecode.SamplePeriod <> 0 then
      begin
        MyPortaToGoal := PatDecode.SamplePeriod;
        OldPortaToGoal := MyPortaToGoal;
      end
      else
        MyPortaToGoal := OldPortaToGoal;

      (* effect stop, keeping period modifier in place (failsafe) *)
      if MyPortaToGoal = 0 then MyPortaToSpeed := 0;
    end
    else
      if PatDecode.EffectNumber = 5 then   (* cmd: effect Porta to Note + Volume slide *)
      begin
        MyPortaToSpeed := OldPortaToSpeed;
        MyPortaToGoal := OldPortaToGoal;

        (* effect stop, keeping period modifier in place (failsafe) *)
        if MyPortaToGoal = 0 then MyPortaToSpeed := 0;
      end;

    if PatDecode.EffectNumber = 14 then  (* cmd: Extended commands *)
    begin
      if (PatDecode.EffectParam shr 4) = 1 then (* cmd: effect Fine Porta Up *)
        Dec(MyFinePorta, PatDecode.EffectParam and $0f);

      if (PatDecode.EffectParam shr 4) = 2 then (* cmd: effect Fine Porta Down *)
        Inc(MyFinePorta, PatDecode.EffectParam and $0f);

      //fixme (low prio: not encountered yet): add Glissando Control (used for modification of Porta to Note)
      if (PatDecode.EffectParam shr 4) = 3 then (* cmd: effect Glissando Control *)
        RunDecInfo.Items.Insert(0, 'Warning: Glissando Control not yet implemented!');
    end;
  end;
end;

procedure DoVibParamUpdate(NewInstr, NewNote: Boolean);
begin
  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    if NewInstr then
    begin
      (* Reset/stop Vibrato on -every new instrument- (!) *)
      MyVibSpeed := 0;   (* effect stop *)
      MyVibDepth := 0;   (* effect stop *)
      MyVibratoPos := 0; (* engine reset *)
    end;

    if PatDecode.EffectNumber = 4 then   (* cmd: effect Vibrato *)
    begin
      (* both Params needed, othwise use old! *)
      if (PatDecode.EffectParam and $f0 <> 0) and (PatDecode.EffectParam and $0f <> 0) then
      begin
        MyVibSpeed := PatDecode.EffectParam shr 4;
        OldVibSpeed := MyVibSpeed;
        MyVibDepth := PatDecode.EffectParam and $0f;
        OldVibDepth := MyVibDepth;
      end
      else
      begin
        MyVibSpeed := OldVibSpeed;
        MyVibDepth := OldVibDepth;
      end;
    end
    else
    begin
      if PatDecode.EffectNumber = 6 then (* cmd: effect Vibrato + Volume slide *)
      begin
        MyVibSpeed := OldVibSpeed;
        MyVibDepth := OldVibDepth;
      end;
    end;

    if PatDecode.EffectNumber = 14 then  (* cmd: Extended commands *)
    begin
      //fixme (low prio: not encountered yet): add Vibrato Waveform/Retrig
      if (PatDecode.EffectParam shr 4) = 4 then (* cmd: effect Vibrato Waveform/Retrig *)
        RunDecInfo.Items.Insert(0, 'Warning: Vibrato Waveform/Retrig not yet implemented!');
    end;
  end;
end;

procedure DoArpeggioUpdate(NewInstr, NewNote: Boolean);
begin
  with MySongLogic[MyCh] do
  begin
    MyArpeggio := 0;
    if (PatDecode.EffectNumber = 0) and (PatDecode.EffectParam <> 0) then (* cmd: effect Arpeggio *)
      MyArpeggio := PatDecode.EffectParam;

    if NewInstr then
    begin
      (* Reset/stop Finetune 'override' on -every new instrument- (!) *)
      MyFineTuneOR := -1;
    end;
    if PatDecode.EffectNumber = 14 then  (* cmd: Extended commands *)
    begin
      if (PatDecode.EffectParam shr 4) = 5 then (* cmd: effect Set Finetune 'override' *)
        MyFineTuneOR := PatDecode.EffectParam and $0f;
    end;
  end;
end;

procedure DoTremoloUpdate(NewInstr, NewNote: Boolean);
begin
  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    if NewInstr then
    begin
      (* Reset/stop Tremolo on -every new note- (!) *)
      MyTremSpeed := 0;  (* effect stop *)
      MyTremDepth := 0;  (* effect stop *)
      MyTremoloPos := 0; (* engine reset *)
    end;

    if PatDecode.EffectNumber = 7 then
    begin
      if (PatDecode.EffectParam and $f0 <> 0) and (PatDecode.EffectParam and $0f <> 0) then
      begin
        MyTremSpeed := PatDecode.EffectParam shr 4;
        OldTremSpeed := MyTremSpeed;
        MyTremDepth := PatDecode.EffectParam and $0f;
        OldTremDepth := MyTremDepth;
      end
      else
      begin
        MyTremSpeed := OldTremSpeed;
        MyTremDepth := OldTremDepth;
      end;
      (* We need the Volume at the start of the effect to exec the effect correctly *)
      OldVolume := MyVolume;
    end;

    if PatDecode.EffectNumber = 14 then  (* cmd: Extended commands *)
    begin
      //fixme (low prio: not encountered yet): add Tremolo Waveform/Retrig
      if (PatDecode.EffectParam shr 4) = 7 then (* cmd: effect Tremolo Waveform/Retrig *)
        RunDecInfo.Items.Insert(0, 'Warning: Tremolo Waveform/Retrig not yet implemented!');
    end;
  end;
end;

function PlayCurrentSample(ResetSample: Boolean; Nr9Offset: Word): Boolean;
var
  RepeatStart,
  RepeatLength : Integer;
  MyFineTune   : ShortInt;
begin
  Result := True;

  with MySongLogic[MyCh] do
  begin
    RepeatLength := MySampleInfo[PatDecode.SampleNumber-1].RptLength;
    RepeatStart  := MySampleInfo[PatDecode.SampleNumber-1].RptStart;
    MyFineTune   := MySampleInfo[PatDecode.SampleNumber-1].FineTune;
  end;
  (* In original format files only the loop-area is played in looping samples.. *)
  if OrigFormatFile and (RepeatLength > 2) then Nr9Offset := RepeatStart;

  if MyAppClosing then
  begin
    Result := False;
    exit;
  end;

  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    (* Restart to SampleOffset (retrigger) and also Restart Effects! *)
    if ResetSample then MyInBufCnt := -1;

    (* playback full sample (minus effect 9 offset if there), including infinite repeats, until it gets overwritten *)
    Result := PlaySample(MyCh,
      MySamplesPtr+MySmpOffset, MySmpLength, Nr9Offset, RepeatLength, RepeatStart, PatDecode.SamplePeriod, MyFineTune);
  end;
end;

function PatTableInUse(PatTabNrToChk: Integer): Boolean;
var
  MyPattern: TModPattern;
  ChkPatPos: Integer;
begin
  Result := False;

  (* Scan through the complete pattable to see if it's in use at *some* point *)
  for ChkPatPos := 0 to 63 do
  begin
    MyPattern := DecodePattern(MyPatternPtr[(MyCh-1)+
                (ChkPatPos*MyMediaRec.Channels)+
                (PatTabNrToChk*MyMediaRec.Channels*64)], False);
    if MyPattern.EffectNumber <> 0 then Result := True;
    if MyPattern.EffectParam  <> 0 then Result := True;
    if MyPattern.SampleNumber <> 0 then Result := True;
    if MyPattern.SamplePeriod <> 0 then Result := True;
    if Result = True then break;
  end;
end;


begin
  (* Preset OK status *)
  result := True;

  (* Walk through song *)
  with MySongLogic[MyCh] do
  begin
    (* Song ending phase play.. *)
    if MySongEnding then
    begin
      (* Only repeat hearable, existing samples and only as long as they are playing.. *)
      if (MyOldPattern.SampleNumber > 0) and (MyVolume <> 0) and (MySampleLogic[MyCh].MyInBufCnt < MyOldLength) then
      begin
        (* ..for repeating samples force volume slide down since these otherwise keep playing forever! *)
        if MySampleInfo[MyOldPattern.SampleNumber-1].RptLength > 2 then
        begin
          if PatDecode.EffectNumber = 3 then   (* If effect was Porta to Note.. *)
            PatDecode.EffectNumber := 5        (* ..then set effect Porta to Note + Volume slide. *)
          else
            if PatDecode.EffectNumber = 4 then (* If effect was Vibrato.. *)
              PatDecode.EffectNumber := 6      (* ..then set effect Vibrato + Volume slide. *)
            else
              PatDecode.EffectNumber := 10;    (* For all other effects replace it with effect Volume slide. *)
          PatDecode.EffectParam := $0f;        (* This is a Volume slide -down- so we eventually mute this sample. *)
        end;
        (* fetch location and length of our previous sample again *)
        MySmpOffset := MyOldOffset;
        MySmpLength := MyOldLength;

        (* Note: don't exec any retrigger or loop command anymore: we want to -end-.. *)
        if (PatDecode.EffectNumber = 9) or ((PatDecode.EffectNumber = 14) and (* Sample Offset *)
         (((PatDecode.EffectParam shr 4) =  6) or                             (* Pattern Loop *)
          ((PatDecode.EffectParam shr 4) =  9) or                             (* Retrig Note *)
          ((PatDecode.EffectParam shr 4) = $d) or                             (* Delay Note *)
          ((PatDecode.EffectParam shr 4) = $e))) then                         (* Pattern Delay *)
        begin
          PatDecode.EffectNumber := 0;
          PatDecode.EffectParam := 0;
        end;
        DoRetrigParamUpdate;              (* in effect resets all Retrigger effects only since we blocked cmd exec above. *)
        DoPortaParamUpdate(False, False); (* all Porta effects *)
        DoVibParamUpdate(False, False);   (* all Vibrato effects *)
        DoArpeggioUpdate(False, False);   (* all Arpeggio effects *)
        DoTremoloUpdate(False, False);    (* all Tremolo effects *)
        DoVolParamUpdate(False, False);   (* all Volume effects *)

        (* We must repeat the previous sample since we don't have one now *)
        PatDecode.SampleNumber := MyOldPattern.SampleNumber;
        (* If we have no period we need to use the last one issued earlier *)
        if PatDecode.SamplePeriod = 0 then PatDecode.SamplePeriod := MyOldPattern.SamplePeriod;

        (* We don't retrigger the running note since we are finishing up! *)
        RetrigSample := False;
        Result := PlayCurrentSample(RetrigSample, MySmpSkipLen);
        if not Result then exit;
      end
      else
      begin
        (* This input channel is done *)
        MySongDone := True;
        (* Play silence buffer *)
        Result := PlaySample(MyCh, nil);
        if not Result then exit;
      end;
      Exit;
    end;

    (* Normal song play.. *)
    if (MyPatTabNr <= MyFileHeader.IgnSetNrOfPats - 1) and
       (MySongPos < MyFileHeader.SongLength) and not MyAppClosing then
    begin
      if not MyPatTabRunning then
      begin
        (* Workaround:
           Sometimes (often?) in a 'valid' song a channel has a completely non-used PatTable. If so, mute that
           channel as otherwise we might sometimes (in err) have a constant repeating sample over a _long_ time(!) *)
        if not PatTableInUse(MyPatTabNr) then
        begin
          (* Reset song-internal channel history:
             Effectively muting the channel until a new Sample (instrument) is specified. *)
          ResetMyPattern(MyOldPattern);
        end;

        if not MyPatBreak then MyPatTabPos := 0;   //(MyPatTabPos = 'row')
        MyPatBreak := False;
      end;
      if (MyPatTabPos <= 63) then
      begin
        MyPatTabRunning := True;

        (* Always show the table position we are actually playing *)
        if MyPatDelaying then
          MyShowPos := MyPatTabPos - 1
        else
          MyShowPos := MyPatTabPos;

        PatDecode := DecodePattern(MyPatternPtr[(MyCh-1)+
                    (MyShowPos *MyMediaRec.Channels)+
                    (MyPatTabNr*MyMediaRec.Channels*64)], True);

        if CBPatDebug.Checked then
        begin
          S := 'Ch' + IntToStr(MyCh) +
               Format(': SP %03d', [MySongPos]) +
               Format(', PT %02d', [MyPatTabNr]) +
               Format('; Ind %02d', [MyShowPos]) +
               Format('; Smp %02d', [PatDecode.SampleNumber]) +
               Format(': Per %03d', [PatDecode.SamplePeriod]) +
               ', Eff $' + ByteToHexString(PatDecode.EffectNumber) +
               ', Par $' + ByteToHexString(PatDecode.EffectParam);
          if PatDecode.SampleNumber > 0 then
            S := S + ' Info: ' + MySampleInfo[PatDecode.SampleNumber-1].Title +
                     ', Len ' + IntToStr(MySampleInfo[PatDecode.SampleNumber-1].Length) +
                     ', RpS ' + IntToStr(MySampleInfo[PatDecode.SampleNumber-1].RptStart) +
                     ', RpL ' + IntToStr(MySampleInfo[PatDecode.SampleNumber-1].RptLength) +
                     ', Fin ' + IntToStr(MySampleInfo[PatDecode.SampleNumber-1].FineTune);
          if not MyAppClosing then
            RunDecInfo.Items.Insert(0, S);
        end;

        (* Play sample *)
        HasNote := PatDecode.SamplePeriod > 0;
        (* If a sample (instrument) is specified and we are not executing effect Pattern Delay: Play it *)
        if (PatDecode.SampleNumber > 0) and not MyPatDelaying then
        begin (* we have a valid sample *)
          (* determine the location and length of our current new sample to play *)
          MySmpOffset := 0;
          for d := 0 to PatDecode.SampleNumber - 2 do //is really OK like this!
            MySmpOffset := MySmpOffset + MySampleInfo[d].Length;
          MyOldOffset := MySmpOffset;

          MySmpLength := MySampleInfo[PatDecode.SampleNumber-1].Length;
          MyOldLength := MySmpLength;

          DoRetrigParamUpdate;                               (* all Retrigger effects *)
          (* DoRetrigParamUpdate 'DelayNote' influences the remaining effects *)
          if MyDelayNote < 0 then
            RetrigSample := HasNote
          else
            RetrigSample := False;
          DoPortaParamUpdate(MyDelayNote < 0, RetrigSample); (* all Porta effects *)
          (* DoPortaParamUpdate 'PortaTo' blocks note retrigger *)
          if MyPortaToSpeed <> 0 then
            RetrigSample := False;
          DoVibParamUpdate(MyDelayNote < 0, RetrigSample);   (* all Vibrato effects *)
          DoArpeggioUpdate(MyDelayNote < 0, RetrigSample);   (* all Arpeggio effects *)
          DoTremoloUpdate(MyDelayNote < 0, RetrigSample);    (* all Tremolo effects *)
          DoVolParamUpdate(MyDelayNote < 0, RetrigSample);   (* all Volume effects *)

          (* If we have no period we need to use the last one issued earlier *)
          if not HasNote then PatDecode.SamplePeriod := MyOldPattern.SamplePeriod;

          (* Normally we retrigger each new note, but if PortaTo or DelayNote active we keep playing the old note *)
          Result := PlayCurrentSample(RetrigSample, MySmpSkipLen);
          if not Result then exit;

          (* Remember what we did as we might have to repeat (part of) it.. *)
          MyOldPattern := PatDecode;
        end
        else
        begin
          (* If we played a sample (instrument) before: Continue) playing it *)
          if (MyOldPattern.SampleNumber > 0) then
          begin
            (* fetch location and length of our previous sample again *)
            MySmpOffset := MyOldOffset;
            MySmpLength := MyOldLength;

            if MyPatDelaying then
            begin
              (* If we are executing effect Pattern Delay we must keep running the 'old' effects.. *)
              PatDecode := MyOldPattern;
              (* .. and we must indeed delay running the patterntable. *)
              (* Note: We increment again later so we restart correctly *)
              Dec(MyPatTabPos);
            end;

            DoRetrigParamUpdate;                     (* all Retrigger effects *)
            (* DoRetrigParamUpdate 'DelayNote' influences the remaining effects *)
            if (MyDelayNote < 0) and not MyPatDelaying then
              RetrigSample := HasNote
            else
              RetrigSample := False;
            DoPortaParamUpdate(False, RetrigSample); (* all Porta effects *)
            (* DoPortaParamUpdate 'PortaTo' blocks note retrigger *)
            if MyPortaToSpeed <> 0 then
              RetrigSample := False;
            DoVibParamUpdate(False, RetrigSample);   (* all Vibrato effects *)
            DoArpeggioUpdate(False, RetrigSample);   (* all Arpeggio effects *)
            DoTremoloUpdate(False, RetrigSample);    (* all Tremolo effects *)
            DoVolParamUpdate(False, RetrigSample);   (* all Volume effects *)

            (* We must repeat the previous sample since we don't have one now *)
            PatDecode.SampleNumber := MyOldPattern.SampleNumber;
            (* If we have no period we need to use the last one issued earlier *)
            if not HasNote then PatDecode.SamplePeriod := MyOldPattern.SamplePeriod;

            (* Normally we retrigger each new note, but if PortaTo or DelayNote active we keep playing the old note *)
            Result := PlayCurrentSample(RetrigSample, MySmpSkipLen);
            if not Result then exit;

            (* Remember what we did as we might have to repeat (part of) it.. *)
            MyOldPattern := PatDecode;
          end
          else
          begin
            (* Play silence buffer (nothing to do) *)
            Result := PlaySample(MyCh, nil);
            if not Result then exit;
          end;
        end;

        (* Process Pattern Delay effect if active *)
        if MyPatDelayNr > 0 then
        begin
          MyPatDelaying := True;
          Dec(MyPatDelayNr);
        end
        else
          MyPatDelaying := False;

        (* set target is next entry in this pattern table *)
        Inc(MyPatTabPos);
        if MyPatTabPos > 63 then
        begin
          (* Exiting current table *)
          MyPatTabRunning := False;
          (* No Pattern Loop active (always reset when going to the next table) *)
          MyPatLoopPos := -1;
          MyPatLoopNr := 0;
        end;


        (* check for pattern break or -jump to see if we should deviate.. *)

        if PatDecode.EffectNumber = 13 then (* effect 'Pattern Break' *)
        begin
          (* we must jump to another row in the next pattern table being nr b4-7 * 10 + b0-3 (so BCD!) *)
          MyPatTabPos := (PatDecode.EffectParam shr 4) * 10 + (PatDecode.EffectParam and $0f);
          if MyPatTabPos > 63 then MyPatTabPos := 0;
          MyPatbreak := True;
          (* Exiting current table *)
          MyPatTabRunning := False;
        end;

        (* If we are the first channel (in the row) preset no 'Pattern Jump' effect was encountered yet. *)
        if MyCh = 1 then MyJmpBreak := -1;
        if PatDecode.EffectNumber = 11 then (* effect 'Pattern Jump' *)
        begin
          (* Do no fail-safe check on MySongPos here as it's catched below! (overflow might be used to signal song end..) *)
          if PatDecode.EffectParam = MySongPos then
          begin
            (* Not documented, but seems correct (patch): But only if we are -not- combined with effect 'Pattern Break' (13) *)
            Inc(MySongPos);
          end
          else
            MySongPos := PatDecode.EffectParam;

          (* Remember where we're going globally in case effect 'Pattern Break' will occur in the same row *)
          (* Note: We exclude 'patch' Inc(MySongPos) from above here since selecting the same SongPos doesn't mean we'll loop now *)
          MyJmpBreak := PatDecode.EffectParam;
          (* Exiting current table *)
          MyPatTabRunning := False;
        end;
      end;

      if not MyPatTabRunning then
      begin
        (* Normally always increment song position at the end of a table, unless the new one is specified directly.. *)
        if PatDecode.EffectNumber <> 11 then inc(MySongPos);
        (* .. or indirectly.. *)
        if PatDecode.EffectNumber = 13 then
        begin
          if MyJmpBreak >= 0 then
            MySongPos := MyJmpBreak
          else (* .. or by convention (See openMPT). *)
            if MySongPos >= MyFileHeader.SongLength then
              MySongPos := 0;
        end;

        (* Fail-safe to not read outside the song patterns array! *)
        if MySongPos >= MyFileHeader.SongLength then
        begin
          (* Song end reached: Signal stop playing gracefully *)
          MySongEnding := True;
          Exit;
        end;
        (* Select song starting pattern table *)
        MyPatTabNr := MyFileHeader.Patterns[MySongPos];
        (* Workaround: Some files have illegal entries not tied to the pattern tables, jump to next song position(s) if encountered.. *)
        while MyPatTabNr > (MyMediaRec.MaxPatTables - 1) do
        begin
          inc(MySongPos);
          (* Fail-safe to not read outside the song patterns array! *)
          if MySongPos >= MyFileHeader.SongLength then
          begin
            (* Song end reached: Signal stop playing gracefully *)
            MySongEnding := True;
            Exit;
          end;
          (* Select song starting pattern table *)
          MyPatTabNr := MyFileHeader.Patterns[MySongPos];
        end;
      end;
    end
    else
    begin
      (* Song end reached: Signal stop playing gracefully *)
      MySongEnding := True;
    end;
  end;
end;

procedure TModMain.BtnStopSongClick(Sender: TObject);
begin
  SetMPState(MPStopped);
  StopPlaying;
end;

procedure TModMain.BtnPlayRawClick(Sender: TObject);
var
  MyRawOffset,
  MyRawRptLen,
  d, Ch            : Integer;
  MyOutFileName, s : String;
begin
  if not MyMediaRec.FileLoaded then exit;
  SetMPState(MPStopped);
  StopPlaying;

  (* mute non-used channels to be sure *)
  For Ch := 1 to MaxNumChBufs do
    MySongLogic[Ch].MyVolume := 0;

  (* No Pattern Jump occurred *)
  MyJmpBreak := -1;

  with MySongLogic[1] do
  begin
    (* set max volume.. *)
    MyVolume := 64;
    OldVolume := 64;
    (* ..not with sliding volume, but fixed *)
    MyVolSlide := 0;
    (* kill Porta *)
    MyPortaSpeed := 0;
    MyFinePorta := 0;
    (* kill PortaTo *)
    MyPortaToSpeed := 0;
    OldPortaToSpeed := 0;
    OldPortaToGoal := 0;
    (* kill vibrato *)
    MyVibSpeed := 0;
    MyVibDepth := 0;
    OldVibSpeed := 0;
    OldVibDepth := 0;
    (* No Tremolo active *)
    MyTremSpeed := 0;
    MyTremDepth := 0;
    OldTremSpeed := 0;
    OldTremDepth := 0;
    (* No Arpeggio active *)
    MyArpeggio := 0;
    (* No Finetune 'override' active *)
    MyFineTuneOR := -1;
    (* No Cutnote active *)
    MyCutNote := -1;
    (* No Delaynote active *)
    MyDelayNote := -1;
    (* No Pattern Delay active *)
    MyPatDelayNr := 0;
    MyPatDelaying := False;
    (* No Pattern Loop active *)
    MyPatLoopPos := -1;
    MyPatLoopNr := 0;
    (* Trigger samples normally *)
    RetrigEvery := 0;
    MySmpSkipLen := 0;
    (* We are _starting_ a 'song' *)
    MySongEnding := False;
    MySongDone := False;
  end;
  with MySampleLogic[1] do
  begin
    (* engines reset *)
    MyPrtaPerPart := 0;
    MyPrtaToPerPart := 0;
    MyVibratoPos := 0;
    MyTremoloPos := 0;
    MyOldInPerPart := -1;
    (* start at sample start *)
    MyInBufCnt := -1;
    (* Our 'previous buffer's last sample' is zero (we are not connected there since we are just starting a new song!) *)
    LastInSample := 0;
    InSmpUpRemain := 0;
    (* Reset internal Tick Counter *)
    MySmpTCnt := 0;
    (* No upsample factor known yet *)
    MyInSmpUp := 0;
    SmpVibUpCnt := 0;
    (* Connection buffer is empty *)
    MyConBufFill := 0;
    (* We have not generated any output yet *)
    MyOutBufLen := 0;
  end;

  (* Note: currently we don't use the timer to play raw. Instead we call PlaySample directly.. *)
  (* Set Default speed *)
  TmrInterval := DefaultTmrSpeed;
  TickSpeed := DefaultTckSpeed;

  (* Determine output filename *)
  MyOutFileName := MyOpenInFile + '_raw_';
  if CBSample.ItemIndex >= 1 then
  begin
    s := IntToStr(CBSample.ItemIndex);
    if Length(s) < 2 then s := '0' + s;
    MyOutFileName := MyOutFileName + s + '.wav';
  end
  else
    MyOutFileName := MyOutFileName + 'all.wav';
  (* Activate Wave file writer if requested *)
  CBSaveWave.Enabled := False;
  if CBSaveWave.Checked then StartWaveFile(MyOutFileName);
  RunDecInfo.Enabled := False;

  with MySampleLogic[1] do
  begin
    if CBSample.ItemIndex <= 0 then
    begin
      SetMPState(MPPlayingRaw);
      SDL_PauseAudio(0);

      (* play full sample once *)
      while (MyInBufCnt < MyTotalSampleSize) and (ModPlayerState = MPPlayingRaw) do
      begin
        PlaySample(1, MySamplesPtr, MyTotalSampleSize, 0, 1, 0, 428, 0); (* 428 denotes C2 (C, 2nd octave) *)
        MixAndOutputSamples(True);
        sleep(5);
        Application.processmessages;
      end;
    end;

    if CBSample.ItemIndex >= 1 then
    begin
      (* find the offset of the beginning of our sample to play *)
      MyRawOffset := 0;
      for d := 1 to CBSample.ItemIndex - 1 do
        MyRawOffset := MyRawOffset + MySampleInfo[d-1].Length; //is really OK like this!

      SetMPState(MPPlayingRaw);
      SDL_PauseAudio(0);
      with MySampleInfo[CBSample.ItemIndex-1] do
      begin
        (* Only play really existing samples *)
        if Length > 2 then
        begin
          if CbRepeat.Checked then
          begin
            (* Also repeat samples that aren't instructed to do so in the file *)
            MyRawRptLen := RptLength;
            if MyRawRptLen <= 2 then MyRawRptLen := Length - RptStart;
          end
          else
            MyRawRptLen := 0; (* force no repeat *)

          (* play full sample including repeats (if requested) *)
          while (MyInBufCnt < Length) and (ModPlayerState = MPPlayingRaw) do
          begin
            PlaySample(1, MySamplesPtr + MyRawOffset, Length, 0, MyRawRptLen, RptStart, 428, FineTune); (* 428 denotes C2 (C, 2nd octave) *)
            MixAndOutputSamples(True);
            sleep(5);
            Application.processmessages;
          end;
        end;
      end;
    end;
  end;

  if ModPlayerState = MPPlayingRaw then
  begin
    (* Wait for sample end *)
    while not AllBufsDone do
    begin
      sleep(5);
      application.ProcessMessages;
    end;
    if ModPlayerState = MPPlayingRaw then
    begin
      SetMPState(MPStopped);
      StopPlaying; //note might be done by PlaySong!
    end;
  end;
end;


Function TModMain.AllBufsDone: Boolean;
begin
  AllBufsDone := True;
  if WaveOutIsOpen and not MyCircBufData.BufEmpty then
    AllBufsDone := False;
end;


procedure TModMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  (* Laz: On Gtk2:
     - Only 'LCLIntf.MessageBox' keeps the background windows being updated;
     - 'MessageDlg' and 'application.MessageBox' stop the background!.
   *)
  if LCLIntf.MessageBox(Handle,'Are you sure you want to quit?', 'Confirmation', mb_YesNo OR MB_ICONQUESTION) <> ID_Yes then
    CanClose := False;
end;

procedure TModMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  HandleNewFile(FileNames[0]);
  BtnPlaySongClick(self);
end;

procedure TModMain.menuMPSettingsClick(Sender: TObject);
begin
  MPSettings.Show;
end;

procedure TModMain.FormDestroy(Sender: TObject);
begin
  ReAllocMem(MyPatternPtr, 0);
  ReAllocMem(MySamplesPtr, 0);
  ReAllocMem(MyTmpFileData, 0);
end;

procedure TModMain.MenuAboutClick(Sender: TObject);
begin
  Application.CreateForm(TAboutBox, aboutbox);
  aboutbox.ShowModal;
  aboutbox.Release;
end;

procedure TModMain.HandleNewFile(MyFile: String);
begin
  (* stop a possible playing song first *)
  SetMPState(MPStopped);
  StopPlaying;

  if not LoadFile(MyFile) then
  begin
    MyOpenInFile := '';
    Exit;
  end;
  (* Remember our intputfilename plus it's path, but excluding its extension *)
  MyOpenInFile := MyFile;
  MyOpenInFile := Copy(MyOpenInFile, 1, LastDelimiter('.',MyOpenInFile) - 1);

{$IFNDEF Haiku} (* When SDL_CloseAudio is called Haiku thinks we want to quit the application! *)
  (* Clean-up *)
  CloseWaveOutput;
  OpenWaveOutput;
{$ENDIF}
end;

procedure TModMain.StopPlaying;
begin
  IO_Timer.Enabled := False;

  if ModPlayerState <> MPPaused then BtnPlaySong.Caption := 'Play song';

  (* Wait for the currently playing audiobuffer to be done only *)
  while not AllBufsDone do
  begin
    Application.processmessages;
    sleep(5);
  end;
  SDL_PauseAudio(1);

  (* End Wave file writer if it was busy *)
  if CBSaveWave.Checked then CompleteWaveFile;
  if not MyAppClosing and (ModPlayerState < MPPlaying) then
  begin
    CBSaveWave.Enabled := True;
    RunDecInfo.Enabled := True;
  end;
end;

function TModMain.MixAndOutputSamples(PlayRaw: Boolean): Boolean;
var
  OutCnt,
  ChCnt     : Integer;
  MySample  : Single;
  ChOn      : Array[1..MaxNumChBufs] of Boolean;
  LRMix     : Boolean;
  LeftVal,
  RightVal,
  TempVal   : Single;
begin
  Result := True;

  if Ch1On.Checked then
    ChOn[1] := True
  else
    ChOn[1] := False;
  if Ch2On.Checked then
    ChOn[2] := True
  else
    ChOn[2] := False;
  if Ch3On.Checked then
    ChOn[3] := True
  else
    ChOn[3] := False;
  if Ch4On.Checked then
    ChOn[4] := True
  else
    ChOn[4] := False;
  if LRInterMix.Checked then
    LRMix := True
  else
    LRMix := False;

  with MySampleLogic[1] do (* All buffers have the same size.. *)
  begin
    for OutCnt := 0 to MyOutBufLen - 1 do
    begin
      if PlayRaw then
      begin
        (* Note: Limiting volume a bit so 'Raw' output is comparable in level to 'Song' output.. *)
        if NumOutChans = 2 then
        begin
          (* fill both channels with the same (mono) data *)
          (* Left channel *)
          MyConvBuf[OutCnt*NumOutChans + 0] := Round(MyChBuf[1][OutCnt] * 0.7);
          (* Right channel *)
          MyConvBuf[OutCnt*NumOutChans + 1] := Round(MyChBuf[1][OutCnt] * 0.7);
        end
        else
          MyConvBuf[OutCnt] := Round(MyChBuf[1][OutCnt] * 0.7);
      end
      else
      begin
        (* Note: Ch1 and Ch4 goto the Left channel, Ch2 and Ch3 to the right channel. *)
        if NumOutChans = 2 then
        begin
          (* Left channel *)
          MySample := 0;
          if ChOn[1] then MySample := MySample + MyChBuf[1][OutCnt];
          if ChOn[4] then MySample := MySample + MyChBuf[4][OutCnt];
          LeftVal := MySample * NumOutChans / MyMediaRec.Channels;
          (* Right channel *)
          MySample := 0;
          if ChOn[2] then MySample := MySample + MyChBuf[2][OutCnt];
          if ChOn[3] then MySample := MySample + MyChBuf[3][OutCnt];
          RightVal := MySample * NumOutChans / MyMediaRec.Channels;
          if LRMix then
          begin
            (* Intermix left and right output channels: keep 9.5dB (3x) channel separation (as does VLC) *)
            (* Note: 20 * Log10(3) = 9.5. *)
            TempVal  := (LeftVal*2 + RightVal) / 3;
            RightVal := (RightVal*2 + LeftVal) / 3;
            LeftVal  := TempVal;
          end;
          (* fill both channels with the calculated (stereo) data *)
          MyConvBuf[OutCnt*NumOutChans + 0] := Round(LeftVal);
          MyConvBuf[OutCnt*NumOutChans + 1] := Round(RightVal);
        end
        else
        begin
          (* Mono output *)
          MySample := 0;
          for ChCnt := 1 to MyMediaRec.Channels do
            if ChOn[ChCnt] then MySample := MySample + MyChBuf[ChCnt][OutCnt];
          MyConvBuf[OutCnt] := Round(MySample * NumOutChans / MyMediaRec.Channels);
        end;
      end;
    end;

    (* Save buffer in file if requested *)
    if CBSaveWave.Checked then
    begin
      WriteWaveChunk(MyOutBufLen * NumOutChans);
      Exit; (* Just decode and save, don't send to soundcard. *)
    end;
  end;

  if MyAppClosing then
  begin
    Result := False;
    exit;
  end;
  if not WaveOutIsOpen then
  begin
    if not WaveOutErrReported then
    begin
      WaveOutErrReported := True;
      RunDecInfo.Items.Insert(0, 'No Wave Output available! (Continuing silent playback..)');
    end;
    (* We might still be writing a wave output file.. *)
    Result := True;
    exit;
  end;

  SDL_LockAudio;
  for OutCnt := 0 to (MySampleLogic[1].MyOutBufLen - 1) do
  begin
    MyCircBufData.BufEmpty := False;
    if NumOutChans = 2 then
    begin
      (* Left *)
      MyCircBuf[MyCircBufData.WritePtr*NumOutChans*2 + 0] := MyConvBuf[OutCnt*NumOutChans + 0] and $ff;
      MyCircBuf[MyCircBufData.WritePtr*NumOutChans*2 + 1] := MyConvBuf[OutCnt*NumOutChans + 0] shr 8;
      (* Right *)
      MyCircBuf[MyCircBufData.WritePtr*NumOutChans*2 + 2] := MyConvBuf[OutCnt*NumOutChans + 1] and $ff;
      MyCircBuf[MyCircBufData.WritePtr*NumOutChans*2 + 3] := MyConvBuf[OutCnt*NumOutChans + 1] shr 8;
    end
    else
    begin
      (* Mono *)
      MyCircBuf[MyCircBufData.WritePtr*2 + 0] := MyConvBuf[OutCnt] and $ff;
      MyCircBuf[MyCircBufData.WritePtr*2 + 1] := MyConvBuf[OutCnt] shr 8;
    end;
    Inc(MyCircBufData.WritePtr);
    if MyCircBufData.WritePtr > MyCircBufData.BufSize-1 then MyCircBufData.WritePtr := 0;
    if MyCircBufData.WritePtr = MyCircBufData.ReadPtr then
    begin
      while MyCircBufData.WritePtr = MyCircBufData.ReadPtr do
      begin
        SDL_UnlockAudio;
        sleep(5);
        SDL_LockAudio;
      end;
    end;
  end;
  SDL_UnlockAudio;
end;

function TModMain.PlaySample(MyCh: Integer; MySmpPtr: PInt8; MyBufLen: Integer = 0; MyFirstStart: Integer = 0;
                             MyRptLen: Integer = 0; MyRptStart: Integer = 0; AmigaSpeed: Word = 428; FineTune: ShortInt = 0): Boolean;
var
  MyInPerPart,
  MyFullBufLen,
  FineTuneIdx,
  OutCnt, i    : Integer;
  MyVibDelta,
  MyTremDelta  : Single;
  MySample1,
  MySample2,
  MyTmpSample,
  MySampleStep : Single;
  Up           : Single;
  MyTickCnt    : Integer;
  S            : String;
  SmpA, SmpB,
  SmpC, SmpD,
  SmpX, QFAmp  : Single;


function AmigaSpeedChk(MyAmigaSpeed: Word; MyFineTune: ShortInt; out MyTabIdx: Integer): Integer;
var
  Cnt : Integer;
begin
  (* find AmigaSpeed in our default (non-finepitched) notes lookup table *)
  Result := -1;

  for Cnt := 1 to 36 do
    if (MyAmigaSpeed > (MyPeriodTable[0, Cnt]-2)) and (MyAmigaSpeed < (MyPeriodTable[0, Cnt]+2)) then Result := Cnt;
  (* save for future reference (Arpeggio) *)
  MyTabIdx := Result;

  if Result < 0 then
  begin
    (* Amigaspeed not found in table means no sound. *)
    exit;
  end;
  (* now apply sample's finetune -OR- effect 'Set Finetune' setting: and fetch finetuned AmigaSpeed.. *)
  with MySongLogic[MyCh] do
    if MyFineTuneOR >= 0 then
    begin
      MyFineTune := MyFineTuneOR;
      if MyFineTune > 7 then Dec(MyFineTune, 16);
    end;
  Result := MyPeriodTable[MyFineTune, MyTabIdx];
  (* ..correct with (possible) effect Fine Porta up/down.. *)
  Result := Result + MySongLogic[MyCh].MyFinePorta;
  (* ..and keep result within the boundaries of our PeriodTable. *)
  if Result < 108 then Result := 108; (* B3 + finepitch +7 *)
  if Result > 907 then Result := 907; (* C1 + FinePitch -8 *)

  if (MySmpPtr <> nil) then (* info has no meaning when playing silence *)
    if CBPatDebug.Checked and not MyAppClosing then
    begin
      S := '';
      if MySongLogic[MyCh].MySongEnding then S := 'Ch' + IntToStr(MyCh) + ': ';
      RunDecInfo.Items.Insert(0, S + 'Inspeed: '  + IntToStr(MyAmigaSpeed)+', Index: ' + IntToStr(MyTabIdx) +
        ', Finetune: ' + IntToStr(MyFineTune) + ', Outspeed: ' + inttostr(Result));
    end;
end;


procedure StartCurrentSample(MyNewInPeriod : Integer);
var
  RepeatStart,
  RepeatLength : Integer;
begin
  with MySongLogic[MyCh] do
  begin
    RepeatLength := MySampleInfo[PatDecode.SampleNumber-1].RptLength;
    RepeatStart  := MySampleInfo[PatDecode.SampleNumber-1].RptStart;

    (* Setup for playback full sample, including infinite repeats, until it gets overwritten *)
    MySmpPtr := MySamplesPtr+MySmpOffset;
    MyBufLen := MySmpLength;
    MyFirstStart := 0;
    MyRptLen := RepeatLength;
    MyRptStart := RepeatStart;
  end;
  with MySampleLogic[MyCh] do
  begin
    (* We already determined our official finepitched correct AmigaSpeed, apply it now *)
    MyInPerPart := MyNewInPeriod;
    (* calculate initial (coarse) up-sample rate *)
    Up := MySettings.OutSampleRate * 2 * (MyInPerPart) / MySettings.MyTVSpeed;
    (* New sample starting: point to first buffer position *)
    (* Please note: zero instead of 'value-1' since we are already busy building the current buffer! *)
    MyInBufCnt := 0;
    (* Setup neutral *)
    MyInSmpUp := Up;
    SmpVibUpCnt := MyInSmpUp;
    (* Vibrato is 'centered' (zero) *)
    MyVibratoPos := 0;
    (* Tremolo is 'centered' (zero) *)
    MyTremoloPos := 0;
  end;
end;


procedure StartDelayedNote;
var
  d, MyDummy,
  MyNewInPeriod  : Integer;
begin
  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    (* We can only start a sample (instrument) delayed if we have one specified *)
    if MyDelayPattern.SampleNumber <= 0 then
    begin
      if PatDecode.SampleNumber <= 0 then exit;
      (* We'll restart the already running sample *)
      MyDelayPattern.SampleNumber := PatDecode.SampleNumber;
    end;
    (* Let's assume it also needs a note to be specified *)
    if MyDelayPattern.SamplePeriod <= 0 then exit;
    (* Let's also assume it needs to be even valid *)
    MyNewInPeriod := AmigaSpeedChk(MyDelayPattern.SamplePeriod, MySampleInfo[MyDelayPattern.SampleNumber-1].FineTune, MyDummy);
    if MyNewInPeriod < 0 then exit;

    (* So we have a go.. *)

    (* Remember our now ending Pattern as we might have to repeat (part of) it.. *)
    MyOldPattern := PatDecode;
    (* Make the Delayed Pattern our current Pattern *)
    PatDecode := MyDelayPattern;

    (* Set it up for Playback *)

    (* Kill running effects *)
    RetrigEvery := 0;
    MyCutNote := -1;
    MyVolSlide := 0;
    (* Reset/stop Vibrato since we have a new instrument *)
    MyVibSpeed := 0;   (* effect stop *)
    MyVibDepth := 0;   (* effect stop *)
    MyVibratoPos := 0; (* engine reset *)
    (* Reset/stop Tremolo since we have a new instrument *)
    MyTremSpeed := 0;  (* effect stop *)
    MyTremDepth := 0;  (* effect stop *)
    MyTremoloPos := 0; (* engine reset *)
    (* Reset/stop Porta up/down *)
    MyPortaSpeed := 0;  (* effect stop *)
    MyPrtaPerPart := 0; (* engine reset *)
    (* Reset Fine Porta up/down *)
    MyFinePorta := 0;
    (* Reset/stop Porta-to *)
    MyPortaToSpeed := 0;  (* effect stop *)
    MyPrtaToPerPart := 0; (* engine reset *)
    (* No Arpeggio active *)
    MyArpeggio := 0; (* effect stop/engine reset *)
    (* Reset/stop Finetune 'override' *)
    MyFineTuneOR := -1;

    //fixme (so for remaining low prio effects): kill all running effects that are added to the player..

    (* Restore Volume to 'default' since a sample is specified *)
    MyVolume := MySampleInfo[PatDecode.SampleNumber-1].Volume;
    OldVolume := MyVolume;

    (* determine the location and length of our current new sample to play *)
    MySmpOffset := 0;
    for d := 0 to PatDecode.SampleNumber - 2 do //is really OK like this!
      MySmpOffset := MySmpOffset + MySampleInfo[d].Length;
    MyOldOffset := MySmpOffset;

    MySmpLength := MySampleInfo[PatDecode.SampleNumber-1].Length;
    MyOldLength := MySmpLength;

    (* Normally we retrigger each new note, but if PortaTo active we keep playing the old note () *)
    StartCurrentSample(MyNewInPeriod);
  end;
end;


procedure ChkDoPortaVibrato(MyIncFactor: Integer);
var
  MyCurPeriod,
  MyTickTime     : Integer;
  OldSmpVibUpCnt : Single;
begin
  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    Inc(MySmpTCnt, MyIncFactor);
    MyTickTime := Round(TmrInterval * MySettings.OutSampleRate / 1000);
    (* Execute once per 'Tick' -excluding Tick 0- .. *)
    if MySmpTcnt >= MyTickTime then
    begin
      (* We might vary a single upsampled input sample in time for the current tick, but on average we stay in sync! *)
      MySmpTCnt := MySmpTcnt - MyTickTime;
      (* Keep track of the ticks executed.. *)
      Inc(MyTickCnt);
      if MyTickCnt < TickSpeed then
      begin
        (* do 'Delay Note' effect *)
        if MyDelayNote = MyTickCnt then
        begin
          StartDelayedNote;
          (* We must set a new target input sample.. (the last done sample (#1) is still the same of course) *)
          if MySmpPtr <> nil then
            MySample2 := (MySmpPtr[MyInBufCnt] shl 8) * MyVolume / 64
          else
            MySample2 := 0;
        end;
        (* do 'Retrigger Note' effect *)
        if (RetrigEvery > 0) and ((MyTickCnt mod RetrigEvery) = 0) then
        begin
          (* Reset input to first sample (We might skip first part of sample buffer though) *)
          MyInBufCnt := MyFirstStart;
          (* We must set a new target input sample.. (the last done sample (#1) is still the same of course) *)
          if MySmpPtr <> nil then
            MySample2 := (MySmpPtr[MyInBufCnt] shl 8) * MyVolume / 64
          else
            MySample2 := 0;
        end;
        (* do 'Vibrato' effect *)
        MyVibDelta := 0;
        if MyVibSpeed <> 0 then
        begin
          (* get sinusoidal value from position.. *)
          //fixme (low prio: not encountered yet): Add other waveforms.. (Effect E4x: Set Vibrato Waveform)
          MyVibDelta := MySineTable[MyVibratoPos and $1f];
          if MyVibratoPos < 0 then MyVibDelta := -MyVibDelta;
          (* get amplitude and combine to get absolute frequency modifier.. *)
          MyVibDelta := (MyVibDelta * MyVibDepth) / 128;
          (* rotate through sinetable.. *)
          MyVibratoPos := MyVibratoPos + MyVibSpeed;
          if MyVibratoPos > 31 then MyVibratoPos := MyVibratoPos - 64;
        end;
        (* do 'Arpeggio' effect *)
        if MyArpeggio <> 0 then
        begin
          case MyTickCnt mod 3 of
            0: MyInPerPart := MyPeriodTable[FineTune,     FineTuneIdx];
            1: MyInPerPart := MyPeriodTable[FineTune, Min(FineTuneIdx + (MyArpeggio and $f0) shr 4, 36)];
            2: MyInPerPart := MyPeriodTable[FineTune, Min(FineTuneIdx + (MyArpeggio and $0f) shr 0, 36)];
          end;
        end;
        (* do 'Porta to Note' effect *)
        if MyPortaToSpeed <> 0 then
        begin
          MyCurPeriod := Round(MyInPerPart + MyPrtaToPerPart + MyPrtaPerPart + MyVibDelta);
          if MyCurPeriod > MyPortaToGoal then (* we need to subtract *)
          begin
            if MyCurPeriod - MyPortaToSpeed >= MyPortaToGoal then
              MyPrtaToPerPart := MyPrtaToPerPart - MyPortaToSpeed
            else
              MyPrtaToPerPart := MyPrtaToPerPart - (MyCurPeriod - MyPortaToGoal);
          end;
          if MyCurPeriod < MyPortaToGoal then (* we need to add *)
          begin
            if MyCurPeriod + MyPortaToSpeed <= MyPortaToGoal then
              MyPrtaToPerPart := MyPrtaToPerPart + MyPortaToSpeed
            else
              MyPrtaToPerPart := MyPrtaToPerPart + (MyPortaToGoal - MyCurPeriod);
          end;
        end;
        (* do 'Porta' effect *)
        MyPrtaPerPart := MyPrtaPerPart + MyPortaSpeed;

        (* keep above effects in frequency range *)
        if (MyPrtaPerPart + MyPrtaToPerPart + MyInPerPart + MyVibDelta) > 856 then    (* lowest  note at pitch 0: C1 *)
          MyPrtaPerPart := Round(856 - (MyPrtaToPerPart + MyInPerPart + MyVibDelta));
        if (MyPrtaPerPart + MyPrtaToPerPart + MyInPerPart + MyVibDelta) < 113 then    (* highest note at pitch 0: B3 *)
          MyPrtaPerPart := Round(113 - (MyPrtaToPerPart + MyInPerPart + MyVibDelta));

        (* do effects 'Volume slide' and 'Cut Note' *)
        UpdateMyVolSlide(MyCh, MyTickCnt);

        (* do 'Tremolo' effect *)
        MyTremDelta := 0;
        if MyTremSpeed <> 0 then
        begin
          (* get sinusoidal value from position.. *)
          //fixme (low prio: not encountered yet): Add other waveforms.. (Effect E7x: Set Tremolo Waveform)
          MyTremDelta := MySineTable[MyTremoloPos and $1f];
          if MyTremoloPos < 0 then MyTremDelta := -MyTremDelta;
          (* get effect amplitude and combine to get absolute volume modifier.. *)
          MyTremDelta := (MyTremDelta * MyTremDepth) / 64;
          (* rotate through sinetable.. *)
          MyTremoloPos := MyTremoloPos + MyTremSpeed;
          if MyTremoloPos > 31 then MyTremoloPos := MyTremoloPos - 64;
          (* Adjust our volume while keeping it in the valid range. *)
          MyVolume := Round(OldVolume + MyTremDelta);
          if MyVolume > 64 then MyVolume := 64;
          if MyVolume <  0 then MyVolume :=  0;
        end;

        (* finally calculate new upsampling factor *)
        MyInSmpUp :=
          MySettings.OutSampleRate * 2 * (MyInPerPart + MyPrtaPerPart + MyPrtaToPerPart + MyVibDelta) /
          MySettings.MyTVSpeed;
        if MyInSmpUp <= 0 then MyInSmpUp := up; (* failsave! *)
        (* Correct for already played period-part of the current input sample *)
        OldSmpVibUpCnt := SmpVibUpCnt;
        SmpVibUpCnt := MyInSmpUp - (1-InSmpUpRemain);
        (* Correct our Tick counter if needed *)
        MyIncFactor := Trunc(SmpVibUpCnt) - Trunc(OldSmpVibUpCnt);
        Inc(MySmpTCnt, MyIncFactor);
      end;
    end;
  end;
end;

begin
  Result := True;

  if MySmpPtr = nil then
  begin
    if CBPatDebug.Checked and not MyAppClosing then
    begin
      S := '';
      if MySongLogic[MyCh].MySongEnding then S := 'Ch' + IntToStr(MyCh) + ': ';
      RunDecInfo.Items.Insert(0, S + 'Playing empty buffer (silence)');
    end;

    (* Find AmigaSpeed in our default (non-finepitched) notes lookup table *)
    MyInPerPart := AmigaSpeedChk(AmigaSpeed, FineTune, FineTuneIdx);
    (* Abort on fail(!) *)
    if MyInPerPart < 0 then
    begin
      RunDecInfo.Items.Insert(0, '>>> ERROR <<< (Illegal AmigaSpeed specified, cannot play silence buffer)');
      exit;
    end;
  end;

  (* In case of looping samples we need to check against the full sample length at the moment
      such a repeat starts. Important if they loop more than once within -one- output buffer! *)
  MyFullBufLen := MyBufLen;

  (* We keep track of our locally 'generated' ticks *)
  MyTickCnt := 0;

  if MySmpPtr <> nil then
  begin
    with MySongLogic[MyCh], MySampleLogic[MyCh] do
    begin
      (* Find AmigaSpeed in our default (non-finepitched) notes lookup table *)
      MyInPerPart := AmigaSpeedChk(AmigaSpeed, FineTune, FineTuneIdx);
      (* -Only- if effect PortaTo is running we need to keep being based on the old period value.. *)
      if MyPortaToSpeed <> 0 then MyInPerPart := MyOldInPerPart;
      (* .. so remember the current period value in use. *)
      MyOldInPerPart := MyInPerPart;
    end;

    (* Amigaspeed not found in table means no sound; Only play really existing samples *)
    if (MyInPerPart < 0) or (MyBufLen <= 2) then
    begin
      (* Play empty buffer and exit. *)
      Result := PlaySample(MyCh, nil);
      exit;
    end;
  end;

  (* calculate initial (coarse) up-sample rate *)
  up := MySettings.OutSampleRate * 2 * (MyInPerPart) / MySettings.MyTVSpeed;

  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    if MySmpPtr <> nil then
    begin
      (* new sample starting: point to first buffer position *)
      if MyInBufCnt < 0 then
      begin
        (* We might skip first part of sample upon start *)
        (* Please note: 'value-1' below is used because of inter-new-sample-data-connection requirement! *)
        MyInBufCnt := MyFirstStart - 1;
        (* Setup neutral since on tick0 of a new sample ChkDoPortaVibrato should not be executed *)
        //fixme (low prio: not encountered yet): For effect 'Set Vibrato Waveform'! (retrig at Tick0 or not)
        MyInSmpUp := Up;
        SmpVibUpCnt := MyInSmpUp;
        (* Reset our internal Tick Counter *)
        MySmpTCnt := 0;
        (* Vibrato is 'centered' (zero) _only_ at new sample start! *)
        //fixme (low prio: not encountered yet): For Effect 'Set Vibrato Waveform'! (retrig at Tick0 or not)
        MyVibratoPos := 0;
        (* Tremolo is 'centered' (zero) _only_ at new sample start! *)
        //fixme (low prio: not encountered yet): For Effect 'Set Tremolo Waveform'! (retrig at Tick0 or not)
        MyTremoloPos := 0;
        (* Cancel possible buffer interconnection data as we do a 'fresh start' *)
        MyConBufFill := 0;
      end;
    end
    else
    begin
      (* playing silence: setup neutral and don't touch buffers *)
      MyInSmpUp := Up;
      SmpVibUpCnt := MyInSmpUp;
      (* Reset our internal Tick Counter *)
      MySmpTCnt := 0;
      (* Vibrato is 'centered' (zero) *)
      MyVibratoPos := 0;
      (* Tremolo is 'centered' (zero) *)
      MyTremoloPos := 0;
    end;

    (* We determine our output buffer size per note: tick-time * nr-ticks-per-note *)
    MyOutBufLen := Round(TmrInterval * TickSpeed * MySettings.OutSampleRate / 1000);
    (* First place still pending data from the previous buffer in our output buffer (if any) *)
    for i := 0 to MyConBufFill - 1 do
      MyChBuf[MyCh][i] := MyConBufContent[i];
    (* don't forget to add the interconnection data for our local 'Tick' time check *)
    ChkDoPortaVibrato(MyConBufFill);
    (* convert while copying sample from input samplerate to output rate (OutSampleRate) *)
    OutCnt := MyConBufFill;
    while OutCnt < MyOutBufLen do
    begin
      if MyAppClosing then
      begin
        Result := False;
        exit;
      end;

      (* Get samples, expand them from 8 to 16 bits and correct them for current Volume setting on the fly.
         Also interpolate evenly between all samples to minimize/prevent creation of harmonics/overtones(!) *)

      (* we are connected to the previous sample even if it's in the previous buffer: That sample is the first one here! *)
      (* Please note:
         - We are connected to the previous buffer in case these buffers play parts of the same sample (instrument);
         - We are also connected to the previous buffer in case these buffers play _different_ instruments!
         - In case if a new songstart we preset the previous sample to zero. *)
      MySample1 := LastInSample;

      if MySmpPtr <> nil then
      begin
        if MyInBufCnt+1 < MyBufLen then
          MySample2 := (MySmpPtr[MyInBufCnt+1] shl 8) * MyVolume / 64
        else
          if MyRptLen > 2 then
            MySample2 := (MySmpPtr[MyRptStart] shl 8) * MyVolume / 64
          else
            MySample2 := 0;
      end
      else
        MySample2 := 0;

      (* We need to end/correct for the previous unfinished sample in time and amplitude first *)
      SmpVibUpCnt := SmpVibUpCnt - (1-InSmpUpRemain);
      ChkDoPortaVibrato(1 + Trunc(SmpVibUpCnt)); (* counting mix sample plus loop samples below *)
      (* Keep last newly fetched buffer value for the next buffer as that needs to connect to this one (always!) *)
      (* Note: ChkDoPortaVibrato just might have updated MySample2 just now! *)
      LastInSample := MySample2;

      (* Qubic interpolation implies waveform 'overshoots' when input waveforms clip:
         Accomodate for that so our restored/improved waveforms are mostly not clipped again.
         Also using this for the linear interpolation to keep it's volume comparable. *)
      QFAmp := 0.85;

      if MySettings.CubicAudioSynth then
      begin
        if not MyCubicSynth then
        begin
          RunDecInfo.Items.Insert(0,'Switched to Cubic interpolation');
          MyCubicSynth := True;
        end;

        (* Execute cubic interpolation,
           which has 4 input points: signal goes -through- control points: so amplitude stays same to input signal)
           https://www.paulinternet.nl/?page=bicubic
           x = 0 at SmpB, x = 1 at SmpC (x = interpolation range: 0..1).
         *)

        (* Collect control points *)
        SmpB := MySample1;
        SmpC := MySample2;
        (* Fetch SmpA *)
        if MySmpPtr <> nil then
        begin
          if MyInBufCnt > 0 then
            SmpA := (MySmpPtr[MyInBufCnt-1] shl 8) * MyVolume / 64
          else
            SmpA := SmpB;
        end
        else
          SmpA := SmpB;
        (* Fetch SmpD *)
        if MySmpPtr <> nil then
        begin
          if MyInBufCnt+2 < MyBufLen then
            SmpD := (MySmpPtr[MyInBufCnt+2] shl 8) * MyVolume / 64
          else
            if MyRptLen > 2 then
            begin
              if MyInBufCnt+1 < MyBufLen then
                SmpD := (MySmpPtr[MyRptStart] shl 8) * MyVolume / 64
              else
                SmpD := (MySmpPtr[MyRptStart+1] shl 8) * MyVolume / 64;
            end
            else
              SmpD := SmpC;
        end
        else
          SmpD := SmpC;

        (* we 'play' already past the timestamp of our starting sample: Use delay from the previous run's leftover time *)
        SmpX := (1-InSmpUpRemain) / MyInSmpUp;

        (* Execute interpolation including the first 'special' sample *)
        SmpVibUpCnt := SmpVibUpCnt + 1;
        (* please note that we always finish upsamping each inbuffer sample, even across the set outbuffer boundary! *)
        While (SmpVibUpCnt >= 1.0) do
        begin
          MyTmpSample := (
            SmpB + 0.5*SmpX*(
              SmpC - SmpA + SmpX*(
                2.0*SmpA - 5.0*SmpB + 4.0*SmpC - SmpD + SmpX*(
                  3.0*(SmpB-SmpC) +
                    SmpD - SmpA)))) * QFAmp;
          (* Prevent signal polarity inversion when clipping occurs *)
          if MyTmpSample >  32767 then MyTmpSample := 32767;
          if MyTmpSample < -32768 then MyTmpSample := -32768;
          MyChBuf[MyCh][OutCnt] := Round(MyTmpSample);
          SmpX := SmpX + 1 / MyInSmpUp;
          SmpVibUpCnt := SmpVibUpCnt - 1;
          Inc(OutCnt);
        end;
      end
      else
      begin
        if MyCubicSynth then
        begin
          RunDecInfo.Items.Insert(0,'Switched to Linear interpolation');
          MyCubicSynth := False;
        end;

        (* Determine the upsampling value step per resulting full sample *)
        MySampleStep := (MySample2 - MySample1) / MyInSmpUp;
        (* we 'play' already past the timestamp of our starting sample: Use delay from the previous run's leftover time *)
        MyTmpSample := MySample1 + MySampleStep * (1-InSmpUpRemain);
        MyChBuf[MyCh][OutCnt] := Round(MyTmpSample * QFAmp);
        Inc(OutCnt);
        (* please note that we always finish upsamping each inbuffer sample, even across the set outbuffer boundary! *)
        While (SmpVibUpCnt >= 1.0) do
        begin
          MyTmpSample := MyTmpSample + MySampleStep;
          MyChBuf[MyCh][OutCnt] := Round(MyTmpSample * QFAmp);
          SmpVibUpCnt := SmpVibUpCnt - 1;
          Inc(OutCnt);
        end;
      end;

      if (MyInBufCnt < MyBufLen) then
      begin
        Inc(MyInBufCnt);
        if (MyInBufCnt = MyBufLen) and (MyRptLen > 2) then
        begin
          MyInBufCnt := MyRptStart;
          MyBufLen   := MyRptStart + MyRptLen;
          (* Don't try to play beyond our sample (orig MOD format files tend to instruct to do this sometimes) *)
          if MyBufLen > MyFullBufLen then
            MyBufLen := MyFullBufLen;
        end;
      end;
      (* the next sample-round (in this or the next buffer!) we correct for the missing part of this sample *)
      InSmpUpRemain := SmpVibUpCnt;
      (* finally fetch the new (original) upsampling factor *)
      SmpVibUpCnt := MyInSmpUp;
    end;
  end;

  if MySmpPtr <> nil then
  begin
    (* If we overfilled our output buffer transfer it to the next runner-up... *)
    (* Please note that this makes a possible new volume setting in the next round vary max. a single input sample in time.. *)
    with MySampleLogic[MyCh] do
      if OutCnt > MyOutBufLen then
      begin
        MyConBufFill := OutCnt - MyOutBufLen;
        if MyConBufFill > MyConBufSize then
        begin
          RunDecInfo.Items.Insert(0, 'Warning: Connection buffer too small! (Need ' + IntToStr(MyConBufFill) + ' samples)');
          MyConBufFill := MyConBufSize;
        end;
        for i := 0 to MyConBufFill - 1 do
          MyConBufContent[i] := MyChBuf[MyCh][MyOutBufLen + i];
      end
      else
        MyConBufFill := 0;
  end
  else
  begin
    with MySampleLogic[MyCh] do
    begin
      (* Update our last newly fetched buffer value for the next buffer to zero as that's what we did play last *)
      LastInSample := 0;
      (* Connection buffer is empty *)
      MyConBufFill := 0;
    end;
  end;
end;

procedure TModMain.MenuFileOpenClick(Sender: TObject);
begin
  if not OpenModFile.Execute then exit;
  HandleNewFile(OpenModFile.FileName);
end;

function TModMain.StartWaveFile(FName: String): Boolean;
begin
  Result := False;
  if Length(FName) < 1 then exit;
  if TFileRec(MyWaveFile).Mode <> fmClosed then exit;

  with MyWaveHeader do
  begin
    rId := 'RIFF';                   (* ID string *)
    rLen := 0;                       (* will be updated while writing chunks! *)
    wfId := 'WAVEfmt ';              (* ID string *)
    fLen := 16;                      (* below struct size *)
    wFormatTag      := 1;            (* WAVE_FORMAT_PCM *)
    nChannels       := PMywfx.channels;
    nSamplesPerSec  := PMywfx.freq;
    wBitsPerSample  := 16;
    nBlockAlign     := (wBitsPerSample shr 3) * PMywfx.channels;
    nAvgBytesPerSec := nBlockAlign * PMywfx.freq;
    dId := 'data';                   (* ID string *)
    wSampleLength := 0;              (* will be updated while writing chunks! *)
  end;

  AssignFile(MyWaveFile,FName);
  (* byte oriented file *)
  Rewrite(MyWaveFile,1);
  (* write preliminary header *)
  Blockwrite(MyWaveFile, MyWaveHeader, SizeOf(TWavHeader));

  (* Signal OK result *)
  Result := True;
end;

function TModMain.WriteWaveChunk(MySize: Integer): Boolean;
begin
  Result := False;
  if TFileRec(MyWaveFile).Mode = fmClosed then exit;

  (* write data *)
  BlockWrite(MyWaveFile,MyConvBuf[0], MySize * 2);
  with MyWaveHeader do
  begin
    wSampleLength := wSampleLength + MySize * 2;
    rLen := wSampleLength + 36;
  end;

  (* Signal OK result *)
  Result := True;
end;

function TModMain.CompleteWaveFile: Boolean;
begin
  Result := False;
  if TFileRec(MyWaveFile).Mode = fmClosed then exit;

  (* Finally update file size information and close *)
  Seek(MyWaveFile,0);
  (* (re)write updated header *)
  Blockwrite(MyWaveFile, MyWaveHeader, SizeOf(TWavHeader));
  CloseFile(MyWaveFile);

  (* Signal OK result *)
  Result := True;
end;

function TModMain.LoadFile(MyFile: String): Boolean;
var
  a, MyCorr,
  MyFileSize,
  MyCalcSize,
  MySongLen  : Integer;
  TF         : File;
  Supported  : Boolean;
  MyTmpPtr   : PInt8;

function ValidChar(a: Byte): Boolean;
begin
  Result := False;
  if ((a >= 48) and (a <= 57)) or  (* 0..9 *)
     ((a >= 65) and (a <= 90)) or  (* A..Z *)
      (a = 33) or (a = 46)         (* ! or . *)
  then
    Result := True;
end;

begin
  Result := False;
  MyMediaRec.FileLoaded := False;
  MyFileHeader.FileID:= '    ';
  ReAllocMem(MyPatternPtr, 0);
  ReAllocMem(MySamplesPtr, 0);
  ReAllocMem(MyTmpFileData, 0);

  AssignFile(TF, MyFile);
  FileMode := fmOpenRead;
  Reset(TF,1); //opens file as binary file

  RunDecInfo.Items.Clear;
  try
    BlockRead(TF,MyFileHeader,SizeOf(TModFileHeader));
  except
    CloseFile(TF);
    FrmMediaInfo.MyUpdate;
    Exit;
  end;
  Supported := False;
  with MyFileHeader do
  begin
    if (FileID = 'M.K.') or
       (FileID = 'FLT4') or
       (FileID = '4CHN') then
    begin
      MyMediaRec.Channels := 4;
      MyMediaRec.MaxPatTables := 64;
      MyMediaRec.MaxSamples := 31;
      MyMediaRec.MaxSampleSize := 65535 * 2;
      Supported := True;
    end;
    if (FileID = 'M!K!') then
    begin
      MyMediaRec.Channels := 4;
      MyMediaRec.MaxPatTables := 128;
      MyMediaRec.MaxSamples := 31;
      MyMediaRec.MaxSampleSize := 65535 * 2;
      Supported := True;
    end;
    OrigFormatFile := False;
    for a := 1 to 4 do
      if not ValidChar(Ord(FileID[a])) then
        OrigFormatFile := True;
    if OrigFormatFile then
    begin
      MyMediaRec.Channels := 4;
      MyMediaRec.MaxPatTables := 64;
      MyMediaRec.MaxSamples := 15;
      MyMediaRec.MaxSampleSize := 65535 * 2;
      Supported := True;
      (* Since our header is only 600 bytes long here and we just read 1084 bytes we need to do some patching.. *)
      (* Note: The original format lacks the last 16 TModFileSampleInfo's (480 bytes) and the ID field (4 bytes) *)
      Try
        (* First temporary store the first part of the Pattern data that has been loaded unintentionally.. *)
        ReAllocMem(MyTmpFileData, 16 * SizeOf(TModFileSampleInfo) + SizeOf(FileID));
        (* Note: The first 130 bytes are actually the SongLength, SongSpeed and Song PatternTable, so skip them *)
        MyTmpPtr := Pint8(Addr(FSampleInfo[15])) + 130;
        Move(MyTmpPtr^, MyTmpFileData^, 16 * SizeOf(TModFileSampleInfo) + SizeOf(FileID));
      except
        CloseFile(TF);
        FileID:= '    ';
        FrmMediaInfo.MyUpdate;
        Exit;
      end;
      (* .. now move SongLength, Songspeed (orig specific!) and PatternTable (= 130 bytes) to correct spot in our struct.. *)
      (* Note: Overwrites part of the unintentionally loaded Pattern data *)
      Move(FSampleInfo[15], SongLength, SizeOf(SongLength) + SizeOf(IgnSetNrOfPats) + SizeOf(Patterns));
      (* .. set our fake file-ID.. *)
      FileID := '----';
      (* .. and clear the unused but dirty (target) _My_SampleInfo space. *)
      for a := MyMediaRec.MaxSamples to 30 do
        (* copy our file sampleinfo to a decoded player version *)
        with MySampleInfo[a] do
        begin
          Title := '';
          Volume := 0;
          Length := 0;
          FineTune := 0;
          RptStart := 0;
          RptLength := 0;
        end;
    end;
    (* we only support 4 channel fileformats for now, also prevent div by zero on wrongly 'detected' orig. format files *)
    if not Supported or (OrigFormatFile and (IgnSetNrOfPats = 0)) then
    begin
      CloseFile(TF);
      FileID:= '    ';
      FrmMediaInfo.MyUpdate;
      Exit;
    end;
    (* do endianess switch *)
    for a := 0 to MyMediaRec.MaxSamples - 1 do
    begin
      (* copy our file sampleinfo to a decoded player version *)
      with FSampleInfo[a], MySampleInfo[a] do
      begin
        Title := Copy(FTitle, 0, 21);
        Volume := FVolume;
        FLength := FLength shr 8 or FLength shl 8;
        (* Length is given in units of -two- bytes! *)
        Length := FLength * 2;
        if OrigFormatFile and (Length > 9999) then
        begin
          (* 'Officially' the original format only supports samples upto/excluding 10000 bytes (or a few less even, unclear!) *)
          RunDecInfo.Items.Add('Warning: Sample size to big (> 9999 bytes)');
        end;
        (* range -8..+7, two's complement! *)
        FFineTune := ShortInt(Byte(FFineTune) << 4) div 16;
        FineTune := FFineTune;
        (* The original format does not support finetuning *)
        if OrigFormatFile then FineTune := 0;
        FRptStart := FRptStart shr 8 or FRptStart shl 8;
        (* RptStart is given in units of -two- bytes.. *)
        RptStart := FRptStart * 2;
        (* .. RptStart is given in units of singe bytes in the original format! *)
        if OrigFormatFile then RptStart := FRptStart;
        FRptLength := FRptLength shr 8 or FRptLength shl 8;
        (* RptLength is given in units of -two- bytes! *)
        RptLength := FRptLength * 2;
      end;
    end;
    (* Before we determine the number of patterns we must save/set the song speed for the original format *)
    (* Note: In the original format there's just one song speed that is set for the entire song *)
    if OrigFormatFile then
    begin
      RunDecInfo.Items.Add('Song Speed: ' + IntToStr(IgnSetNrOfPats) + ' BPM');
      OrigFmtTmrSpeed := 5000 / (IgnSetNrOfPats * 2); (* 'IgnSetNrOfPats' BPM = 50Hz = 20mS *)
    end;
    (* Determine the number of patterns in the file *)
    IgnSetNrOfPats := 0;
    MySongLen := 0;
    for a := 0 to 127 do
    begin
      (* Workaround: Some files have illegal entries not tied to the pattern tables (Pinball Dreams Intro mod (val = 255)) *)
      if (Patterns[a] < MyMediaRec.MaxPatTables) and (Patterns[a] > IgnSetNrOfPats) then
        IgnSetNrOfPats := Patterns[a];
      (* Workaround: Also doublecheck the reported song length, and patch if we determine the song to be longer *)
      if (Patterns[a] > 0) and (a >= MySongLen) then
        MySongLen := a + 1;
    end;
    Inc(IgnSetNrOfPats);
    (* Patch file's reported song length if needed *)
    if SongLength < MySongLen then
    begin
      RunDecInfo.Items.Add('Corrected song length (file reports ' + IntToStr(SongLength) + ', found ' + IntToStr(MySongLen) + ').');
      SongLength := MySongLen;
    end;
    (* Determine the total patterns size in bytes *)
    MyTotalPatternSize := MyMediaRec.Channels * 4 * 64 * IgnSetNrOfPats;
    (* Determine the total Samples size in bytes *)
    MyTotalSampleSize := 0;
    MyCorr := 0;
    for a := 0 to MyMediaRec.MaxSamples - 1 do
    begin
      MyTotalSampleSize := MyTotalSampleSize + MySampleInfo[a].Length;
      (* Workaround: Keep track of possible samples with size 2 that have got no space in the file *)
      if (MySampleInfo[a].Length <= 2) and (MySampleInfo[a].RptLength <= 2) then
        Inc(MyCorr, MySampleInfo[a].Length);
    end;
    (* Check for filesize error.. *)
    MyFileSize := FileSize(TF);
    MyCalcSize := SizeOf(TModFileHeader) + MyTotalPatternSize + MyTotalSampleSize;
    if OrigFormatFile then MyCalcSize := MyCalcSize - 16 * SizeOf(TModFileSampleInfo) - 4;
    if MyFileSize = MyCalcSize then
      RunDecInfo.Items.Add('Total filesize OK (' + IntToStr(MyFileSize) + ').')
    else
    begin
      RunDecInfo.Items.Add('Filesize error: Expected ' + IntToStr(MyCalcSize) + ', Got ' + IntToStr(MyFileSize) + '.');
      (* Check if file too small *)
      if MyFileSize < MyCalcSize then
      begin
        (* Workaround: sometimes empty samples mistakenly have size '2'. Correct.. *)
        if MyCorr <> 0 then
        begin
          RunDecInfo.Items.Add('Empty samples marked non-empty, decreasing samplesize expectation ('
                                                                     + IntToStr(MyCorr) + ') and patching sample data.');
          Dec(MyTotalSampleSize, MyCorr);
          Dec(MyCalcSize, MyCorr);
          (* Aside from patching the total samplesize, we also must fix the individual samples the patch applies for. *)
          for a := 0 to MyMediaRec.MaxSamples - 1 do
            if MySampleInfo[a].Length <= 2 then MySampleInfo[a].Length := 0;
        end;
      end;
      (* Recheck if file still to small *)
      if MyFileSize < MyCalcSize then
      begin
        RunDecInfo.Items.Add('File to small, aborting.');
        CloseFile(TF);
        FileID:= '    ';
        FrmMediaInfo.MyUpdate;
        Exit;
      end;
      (* Recheck for filesize error *)
      if MyFileSize = MyCalcSize then
        RunDecInfo.Items.Add('Corrected total filesize OK.')
      else
      begin
        (* File is too big *)
        a := MyFileSize - MyCalcSize;
        if MyCorr <> 0 then
          RunDecInfo.Items.Add('Corrected total filesize to large, skipping pre-sampledata surplus space (' + IntToStr(a) + ').')
        else
          RunDecInfo.Items.Add('File to large, skipping pre-sampledata surplus space (' + IntToStr(a) + ').');
        (* 'Dump' the pre-sampledata surplus at the end of our Patterns tables: it does not do any harm there. *)
        MyTotalPatternSize := MyTotalPatternSize + a;
      end;
    end;

    (* Load our pattern data (#Channels * 4 (= entry size) * 64 (= entries) * (= #Patterns) *)
    Try
      ReAllocMem(MyPatternPtr, MyTotalPatternSize);
      if OrigFormatFile then
      begin
        (* Rescue the already loaded first part of the pattern data first.. *)
        Move(MyTmpFileData^, MyPatternPtr^, 16 * SizeOf(TModFileSampleInfo) + SizeOf(FileID));
        (* ..and load the rest of the pattern data *)
        MyTmpPtr := Pint8(MyPatternPtr) + 16 * SizeOf(TModFileSampleInfo) + SizeOf(FileID);
        BlockRead(TF, MyTmpPtr^, MyTotalPatternSize - (16 * SizeOf(TModFileSampleInfo) + SizeOf(FileID)));
      end
      else
        BlockRead(TF,MyPatternPtr^, MyTotalPatternSize);
    except
      CloseFile(TF);
      FileID:= '    ';
      FrmMediaInfo.MyUpdate;
      Exit;
    end;
    (* Load our sample data (8 bit signed format) *)
    Try
      ReAllocMem(MySamplesPtr, MyTotalSampleSize);
      BlockRead(TF,MySamplesPtr^, MyTotalSampleSize);
    except
      CloseFile(TF);
      FileID:= '    ';
      FrmMediaInfo.MyUpdate;
      Exit;
    end;
    CloseFile(TF);
  end;

  Result := True;
  MyMediaRec.FileLoaded := True;
  MyMediaRec.FileName := MyFile;

  FrmMediaInfo.MyUpdate;
end;

procedure TModMain.MenuMediaInfoClick(Sender: TObject);
begin
  FrmMediaInfo.Show;
end;

procedure TModMain.NextSmpExecute(Sender: TObject);
begin
  if CBSample.ItemIndex < CBSample.Items.Count then CBSample.ItemIndex := CBSample.ItemIndex + 1;
end;

procedure TModMain.PrevSmpExecute(Sender: TObject);
begin
  if CBSample.ItemIndex > 0 then CBSample.ItemIndex := CBSample.ItemIndex - 1;
end;

procedure TModMain.SongStartPosEditingDone(Sender: TObject);
var
  Fout, TS : Integer;
begin
  Val(SongStartPos.Text,TS,Fout);
  if (Fout = 0) and (TS > 0) and (TS < 128) then
  begin
    SongStartPos.Text := IntToStr(TS);
    MySongStartPos := TS;
  end
  else
  begin
    SongStartPos.Text := 'Default (0)';
    MySongStartPos := 0;
  end;
end;


end.

