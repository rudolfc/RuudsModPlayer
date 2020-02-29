unit ModPlayMain;

interface

uses
  LCLIntf, LCLType,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Buttons,
  ExtCtrls, StdCtrls, ActnList, MMSystem, FPTimer, GlobalVars;

  { TModMain }

Const
  //fixme: Make below 2 settings user-selectable...
  NumAudioBufs  = 3;     (* we use the 'double buffering' scheme *)
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
    MyTmrInterval   : Single;
    MyTickSpeed,
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
    MyPortaSpeed,
    MyPortaToSpeed,
    OldPortaToSpeed,
    MyPortaToGoal,
    OldPortaToGoal,
    MyVibSpeed,
    MyVibDepth,
    OldVibSpeed,
    OldVibDepth,
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
    MyVibratoPos    : Integer;
    LastInSample,
    InSmpUpRemain   : Single;
    OldAmigaSpeed   : Word;
    MyInPerPart     : Integer;
    MySmpTCnt       : Integer;
    MyInSmpUp,
    SmpVibUpCnt     : Single;
    MyConBufContent : Array[0..MyConBufSize-1] of SmallInt;
    MyConBufFill    : Integer;
    MyOutBufLen     : Integer;
  end;

  TModMain = class(TForm)
    CBPatDebug: TCheckBox;
    CBSaveWave: TCheckBox;
    Ch1On: TCheckBox;
    Ch2On: TCheckBox;
    Ch3On: TCheckBox;
    Ch4On: TCheckBox;
    CbSongDebug: TCheckBox;
    LRInterMix: TCheckBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    menuMPSettings: TMenuItem;
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
    procedure BtnStopSongClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure menuMPSettingsClick(Sender: TObject);
    procedure ParseRunControl;
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

    PHeader        : Array[0..NumAudioBufs-1] of PWaveHdr;
    PMyWaveOutDev  : LPHWAVEOUT;
    PMywfx         : PWaveFormatEx;
    IO_Timer       : TFPTimer;

    MySongLogic    : Array[1..MaxNumChBufs] of TSongLogic;
    MySampleLogic  : Array[1..MaxNumChBufs] of TSampleLogic;

    ActTckSpeed    : Byte;
    ActTmrInterval : Single;
    MasterVolume   : Integer;
    PlayingRaw     : Boolean;
    BufNr          : Byte;
    MySongStartPos : Integer;
    MyWaveFile     : FILE;
    MyWaveHeader   : TWavHeader;
    MyOpenInFile   : String;

    (* the longest buffer getting used is @ 32PBM = 78mS * 31 ticks = 2418mS * Samples/sec. *)
    MyChBuf      : Array[1..MaxNumChBufs, 0..  (1*78*31*MaxSampleRate div 1000)] of SmallInt; (* mono channel output buffers *)
    MyConvBuf    : Array[0..NumAudioBufs-1, 0..(2*78*31*MaxSampleRate div 1000)] of SmallInt; (* Stereo mixed device buffers *)


    procedure ExecTick(Sender: TObject);
    procedure UpdateMyVolSlide(MyCh, MyTick: Integer);
    //procedure SetMyVolSliderMax;
    procedure ExecNote(MyCh: Integer);


  public

    Procedure ResetMyPattern(var MyPattern: TModPattern);
    function  StartWaveFile(FName: String): Boolean;
    function  WriteWaveChunk(MyBuffer, MySize: Integer): Boolean;
    function  CompleteWaveFile: Boolean;
    function  LoadFile(MyFile: String): Boolean;
    procedure StopPlaying;
    function  MixAndOutputSamples(PlayRaw: Boolean): Boolean;
    function  PlaySample(MyCh: Integer;
      MySmpPtr: PInt8; MyBufLen, MyFirstStart, MyRptLen, MyRptStart: Integer; AmigaSpeed: Word; FineTune: ShortInt): Boolean;
    function  PlayEmptySample(MyCh: Integer): Boolean;
    Function  AllBufsDone: Boolean;
    procedure OpenWaveOutput;
    procedure CloseWaveOutput;

  end;

var
  ModMain: TModMain;

implementation

uses
  dlg_aboutbox, dlg_mediainfo, GlobalFunctions, Dlg_MPSettings;

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
BEGIN
  if MySongPaused or StoppingMySong or MyAppClosing then exit;
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
    begin
      RunDecInfo.Items.Add(S);
      RunDecInfo.ItemIndex := RunDecInfo.Items.Count - 1;
    end;
  end;

  (* Process all channels *)
  For Ch := 1 to MyMediaRec.Channels do ExecNote(Ch);
  MixAndOutputSamples(False);
  ParseRunControl;

  (* If we reached the end of the song let the currently queued buffers play out *)
  if StoppingMySong then
    while not AllBufsDone do sleep(5);

  (* We're done (note: 'StopPlaying' stops the timer again) *)
  if not MyAppClosing then IO_Timer.Enabled := True;

  (* Check/execute Stop *)
  if StoppingMySong or MyAppClosing then StopPlaying;
END;

procedure TModMain.ParseRunControl;
var
  ChOut, Chin,
  ChSongDone   : Integer;
  Cmd11, Cmd13 : Boolean;
begin
  Cmd11 := False;
  Cmd13 := False;

  for ChIn := 1 to MyMediaRec.Channels do
  begin
    with MySongLogic[ChIn] do
    begin
      if PatDecode.EffectNumber = 11 then Cmd11 := True;
      if PatDecode.EffectNumber = 13 then Cmd13 := True;
    end;
  end;
  if Cmd11 and Cmd13 then
  begin
    //fixme: complete 11 and 13 on same row!
    RunDecInfo.Items.Add('--> ''Pos Jump''&''Pat break'' not yet done, please contact Rudolf with this song''s author and title..');
    RunDecInfo.ItemIndex := RunDecInfo.Items.Count - 1;
  end;

  ChSongDone := 0;
  for ChIn := 1 to MyMediaRec.Channels do
    with MySongLogic[ChIn] do
    begin
      (* When a channel reaches the end of a pattern table advance all channels to the next one in the song *)
      (* Note: without this provision Pattern Loops will mess things up big time.. *)
      if (MyPatTabPos > 63) and not Cmd11 and not Cmd13 then
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
        end;
      end;
      (* Copy 'Pattern Delay' cmd results to all channels (they need to process it as well) *)
      if (PatDecode.EffectNumber = 14) and ((PatDecode.EffectParam shr 4) = $e) then
      begin
        for ChOut := 1 to MyMediaRec.Channels do
        begin
          MySongLogic[ChOut].MyPatDelayNr  := MyPatDelayNr;
          MySongLogic[ChOut].MyPatDelaying := MyPatDelaying;
        end;
      end;
      (* Copy 'Set Speed' cmd results to global system (effects all channels) *)
      if PatDecode.EffectNumber = 15 then
      begin
        (* Often both variants occur in the same row *)
        if PatDecode.EffectParam >= 32 then
          ActTmrInterval := MyTmrInterval
        else
          ActTckspeed := MyTickSpeed;
      end;
      (* Check for/signal song end: Wait for all channels to finish their songs *)
      if MySongDone then Inc(ChSongDone);
    end;

  (* If all channels finished playing signal complete stop. *)
  if ChSongDone = MyMediaRec.Channels then StoppingMySong := True;
end;

procedure TModMain.FormCreate(Sender: TObject);
var
  a       : Integer;
  {$IFDEF MSWINDOWS}
  WinVer  : Single;
  {$ENDIF}
begin
  for a := 0 to NumAudioBufs-1 do
  begin
    new(PHeader[a]);
    pheader[a]^.dwFlags := WHDR_DONE;
  end;
  new(PMywfx);
  new(pMyWaveOutDev);
  MyAppClosing := False;
  PlayingRaw := False;

  MyMediaRec.FileLoaded := False;
  MyFileHeader.FileID:= '    ';
  ActTckSpeed := DefaultTckSpeed;
  MasterVolume := 64;
  MySongPaused := False;
  WaveOutIsOpen := False;
  BufNr := 0;
  MySongStartPos := 0;
  TFileRec(MyWaveFile).Mode := fmClosed;
  MyOpenInFile := '';
  OrigFormatFile := False;
  OrigFmtTmrSpeed := DefaultTmrSpeed;

  (* use TFPTimer *)
  IO_Timer := TFPTimer.Create(nil);

  {$IFDEF MSWINDOWS}
  RunDecInfo.Items.Add('Windows version ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion));
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

procedure TModMain.OpenWaveOutput;
var
  MyStat, a: Integer;
begin
  if WaveOutIsOpen then exit;

  PMywfx^.nChannels := NumOutChans;
  PMywfx^.nSamplesPerSec := MPSettings.MySettings.OutSampleRate;
  PMywfx^.wBitsPerSample := 16;
  PMywfx^.cbSize := 0; // size of _extra_ info, none for PCM data *
  PMywfx^.wFormatTag := WAVE_FORMAT_PCM;
  PMywfx^.nBlockAlign := (PMywfx^.wBitsPerSample shr 3) * PMywfx^.nChannels;
  PMywfx^.nAvgBytesPerSec := PMywfx^.nBlockAlign * PMywfx^.nSamplesPerSec;

  (* Make sure all our headers are marked 'clean' (Closing WaveOutput might not do that for all buffers!) *)
  for a := 0 to NumAudioBufs-1 do
    pheader[a]^.dwFlags := WHDR_DONE;

  MyStat := waveOutOpen(PMyWaveOutDev,WAVE_MAPPER,PMywfx,0,0,CALLBACK_NULL);
  if MyStat <> MMSYSERR_NOERROR then
    RunDecInfo.Items.Add('Could not open Wave output device!')
  else
    WaveOutIsOpen := True;

  (* Make sure both output channels (stereo) are decently set.. (on mono input only left is open by default apparantly!) *)
  if WaveOutIsOpen then
    waveOutSetVolume(PMyWaveOutDev^, ((MasterVolume * $FFFF div 64) shl 16) or (MasterVolume * $FFFF div 64));
end;

procedure TModMain.CloseWaveOutput;
begin
  if WaveOutIsOpen then
  begin
    waveOutReset(PMyWaveOutDev^); //Also marks all buffers as 'WHDR_DONE'
    waveOutClose(PMyWaveOutDev^);
    WaveOutIsOpen := False;
  end;
end;

procedure TModMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  StopPlaying;
  MyAppClosing := True;

  while not AllBufsDone do sleep(5);

  if WaveOutIsOpen then
  begin
    (* Close soundcard output *)
    CloseWaveOutput;
  end;

  IO_Timer.Enabled := False;
  IO_Timer.Free;

  (* Update and close the wavefile if it was open *)
  CompleteWaveFile;

  (* Force us seeing errors if we still reference RunDecInfo somewhere at this time (debug help) *)
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
var
  Cnt: Integer;
begin
  if not MyMediaRec.FileLoaded then exit;

  MySongPaused := not MySongPaused;

  if MySongPaused then
  begin (* Pause song *)
    StopPlaying;
    StoppingMySong := False;
    BtnPlaySong.Caption := 'Cont. song';
    exit;
  end
  else
  begin (* Resume song *)
    (* still playing 'raw' sample(s): quit that first *)
    if PlayingRaw then
    begin
      StopPlaying;
      if WaveOutIsOpen then
        waveOutReset(PMyWaveOutDev^); (* Also marks all buffers as 'WHDR_DONE', thereby also ending PlayRaw. *)
    end;
  end;

  BtnPlaySong.Caption := 'Pause song';
  (* Activate Wave file writer if requested *)
  CBSaveWave.Enabled := False;
  if CBSaveWave.Checked then StartWaveFile(MyOpenInFile + '.wav');

  if StoppingMySong then
  begin (* Start song fresh (so no resume here) *)
    StoppingMySong := False;
    if CBSongDebug.Checked or CBPatDebug.Checked then RunDecInfo.Items.Clear;

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
          if MySongPos >= MyFileHeader.SongLength then StoppingMySong := True;
          (* Select song starting pattern table *)
          MyPatTabNr := MyFileHeader.Patterns[MySongPos];
        end;
        (* reset song-internal history *)
        ResetMyPattern(MyOldPattern);
        (* our volume is stable (not sliding) *)
        MyVolSlide := 0;
        (* start at Max volume *)
        MyVolume := 64;
        (* Trigger samples normally *)
        RetrigEvery := 0;
        (* No Porta active *)
        MyPortaSpeed := 0;
        (* No PortaTo active *)
        MyPortaToSpeed := 0;
        OldPortaToSpeed := 0;
        OldPortaToGoal := 0;
        (* No Vibrato active *)
        MyVibSpeed := 0;
        MyVibDepth := 0;
        OldVibSpeed := 0;
        OldVibDepth := 0;
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
        (* Set default speed (request) *)
        if OrigFormatFile then
          MyTmrInterval := OrigFmtTmrSpeed
        else
          MyTmrInterval := DefaultTmrSpeed;
        MyTickSpeed := DefaultTckSpeed;
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
        (* Our 'previous buffer's last sample' is zero (we are not connected there since we are just starting a new song!) *)
        LastInSample := 0;
        InSmpUpRemain := 0;
        (* We have no previous frequency yet *)
        OldAmigaSpeed := 0;
        MyInPerPart := 0;
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

    (* Set default speed (Actual) *)
    if OrigFormatFile then
      ActTmrInterval := OrigFmtTmrSpeed
    else
      ActTmrInterval := DefaultTmrSpeed;
    ActTckSpeed := DefaultTckSpeed;
  end;

  (* Start *)
  IO_Timer.Enabled := True;
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

procedure TModMain.ExecNote(MyCh: Integer);
var
  d            : Integer;
  HasNote,
  RetrigSample : Boolean;
  S            : String;


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
          (* .. and for now keep running the previous effects *)
          PatDecode := MyOldPattern;
        end;

        //fixme: add Delay Note (i.e. Subi King of Boggle: end of song, Beasty boys sample 'King of Boggle': smp 24)
        RunDecInfo.Items.Add('Warning: Delay Note not yet implemented!');
        RunDecInfo.ItemIndex := RunDecInfo.Items.Count - 1;
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
        (* Note: I have the feeling this item is used sometimes while writing a song:
                 MyParam = 0: Start position to jump back to;
                 MyParam = 1: We run the sequence once, so we -don't- loop back;
                 MyParam > 1: We loop (MyParam-1) times, so we play it (MyParam) times. *)
        MyParam := PatDecode.EffectParam and $0f;
        if MyParam = 0 then            (* Note current row as starting position for the loop *)
          MyPatLoopPos := MyPatTabPos
        else
        begin
          if MyPatLoopNr = 0 then      (* Note number of loops to make *)
            MyPatLoopNr := MyParam;
          (* Note: Always decrement MyPatLoopNr! (not only if MyPatLoopNr <> 0) *)
          Dec(MyPatLoopNr);            (* Update loop counter *)
          if MyPatLoopNr > 0 then      (* Initiate next loop if we're not done looping yet *)
          begin
            if MyPatLoopPos >= 0 then
              MyPatTabPos := MyPatLoopPos - 1 (* We increment again later so we restart correctly. *)
            else
              Dec(MyPatTabPos);               (* We increment again later so we restart correctly (same row). *)
          end;
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
      // Fixme: Only do something if one is zero or..
      if (PatDecode.EffectParam and $f0 = 0) or (PatDecode.EffectParam and $0f = 0) then
      begin
        // Fixme: .. let slide up take precedence?
        if PatDecode.EffectParam and $f0 <> 0 then
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
    if NewInstr then MyVolume := MySampleInfo[PatDecode.SampleNumber-1].Volume;

    if PatDecode.EffectNumber = 12 then (* cmd: effect Set Volume (0..64) *)
      MyVolume := PatDecode.EffectParam;
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
  end;
end;

procedure DoVibParamUpdate(NewInstr, NewNote: Boolean);
begin
  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    if NewInstr then
    begin
      (* Reset/stop Vibrato on -every new note- (!) *)
      MyVibSpeed := 0;    (* effect stop *)
      MyVibDepth := 0;    (* effect stop *)
      MyVibratoPos := 0;  (* engine reset *)
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
  end;
end;

procedure DoArpeggioUpdate;
begin
  with MySongLogic[MyCh] do
  begin
    if (PatDecode.EffectNumber = 0) and (PatDecode.EffectParam <> 0) then
    begin
      //fixme: add Arpeggio
      RunDecInfo.Items.Add('Warning: Arpeggio not yet implemented!');
      RunDecInfo.ItemIndex := RunDecInfo.Items.Count - 1;
    end;
  end;
end;

procedure DoTremoloUpdate;
begin
  with MySongLogic[MyCh] do
  begin
    if PatDecode.EffectNumber = 7 then
    begin
      //fixme: add Tremolo
      RunDecInfo.Items.Add('Warning: Tremolo not yet implemented!');
      RunDecInfo.ItemIndex := RunDecInfo.Items.Count - 1;
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

  if MyAppClosing or StoppingMySong or MySongPaused then
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

        (* Note: don't exec any retrigger command anymore: we want to -end-.. *)
        if (PatDecode.EffectNumber = 9) or (PatDecode.EffectNumber = 14) then
        begin
          PatDecode.EffectNumber := 0;
          PatDecode.EffectParam := 0;
        end;
        DoRetrigParamUpdate;              (* in effect resets all Retrigger effects only since we blocked cmd exec above. *)
        DoPortaParamUpdate(False, False); (* all Porta effects *)
        DoVibParamUpdate(False, False);   (* all Vibrato effects *)
        DoArpeggioUpdate;                 (* all Arpeggio effects *)
        DoTremoloUpdate;                  (* all Tremolo effects *)
        DoVolParamUpdate(False, False);   (* all Volume effects *)

        (* we must repeat the previous sample, but we exec the current effect and period! *)
        PatDecode.SampleNumber := MyOldPattern.SampleNumber;

        (* We don't retrigger the running note since we are finishing up! *)
        RetrigSample := False;
        if not PlayCurrentSample(RetrigSample, MySmpSkipLen) then exit;
      end
      else
      begin
        (* This input channel is done *)
        MySongDone := True;
        (* Play silence buffer *)
        if not PlayEmptySample(MyCh) then exit;
      end;
      Exit;
    end;

    (* Normal song play.. *)
    if (MyPatTabNr <= MyFileHeader.IgnSetNrOfPats - 1) and
       (MySongPos  < MyFileHeader.SongLength) and not StoppingMySong and
       not MyAppClosing then
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
      if (MyPatTabPos <= 63) and not StoppingMySong then
      begin
        MyPatTabRunning := True;
        PatDecode := DecodePattern(MyPatternPtr[(MyCh-1)+
                    (MyPatTabPos*MyMediaRec.Channels)+
                    (MyPatTabNr*MyMediaRec.Channels*64)], True);

        if CBPatDebug.Checked then
        begin
          S := 'Ch' + IntToStr(MyCh) +
               Format(': SP %03d', [MySongPos]) +
               Format(', PT %02d', [MyPatTabNr]) +
               Format('; Ind %02d', [MyPatTabPos]) +
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
          begin
            RunDecInfo.Items.Add(S);
            RunDecInfo.ItemIndex := RunDecInfo.Items.Count - 1;
          end;
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
          DoArpeggioUpdate;                                  (* all Arpeggio effects *)
          DoTremoloUpdate;                                   (* all Tremolo effects *)
          DoVolParamUpdate(MyDelayNote < 0, RetrigSample);   (* all Volume effects *)

          (* Normally we retrigger each new note, but if PortaTo or DelayNote active we keep playing the old note *)
          if not PlayCurrentSample(RetrigSample, MySmpSkipLen) then exit;

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
            if MyDelayNote < 0 then
              RetrigSample := HasNote
            else
              RetrigSample := False;
            DoPortaParamUpdate(False, RetrigSample); (* all Porta effects *)
            (* DoPortaParamUpdate 'PortaTo' blocks note retrigger *)
            if MyPortaToSpeed <> 0 then
              RetrigSample := False;
            DoVibParamUpdate(False, RetrigSample);   (* all Vibrato effects *)
            DoArpeggioUpdate;                        (* all Arpeggio effects *)
            DoTremoloUpdate;                         (* all Tremolo effects *)
            DoVolParamUpdate(False, RetrigSample);   (* all Volume effects *)

            (* we must repeat the previous sample, but we exec the current effect and period! *)
            PatDecode.SampleNumber := MyOldPattern.SampleNumber;

            (* Normally we retrigger each new note, but if PortaTo or DelayNote active we keep playing the old note *)
            if not PlayCurrentSample(RetrigSample, MySmpSkipLen) then exit;
          end
          else
          begin
            (* Play silence buffer (nothing to do) *)
            if not PlayEmptySample(MyCh) then exit;
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

        (* check for pattern jumps to see if we should deviate.. *)
        if PatDecode.EffectNumber = 13 then
        begin
          (* we must jump to another pattern in the next pattern table being nr b4-7 * 10 + b0-3 (so BCD!) *)
          MyPatTabPos := (PatDecode.EffectParam shr 4) * 10 + (PatDecode.EffectParam and $0f);
          if MyPatTabPos > 63 then MyPatTabPos := 0;
          MyPatbreak := True;
          (* Exiting current table *)
          MyPatTabRunning := False;
        end;
        if PatDecode.EffectNumber = 11 then
        begin
          (* Do no fail-safe check on MySongPos here as it's catched below! (overflow might be used to signal song end..) *)
          if PatDecode.EffectParam = MySongPos then
          begin
            (* Not explicitly documented, but seems very correct! *)
            Inc(MySongPos);
          end
          else
            MySongPos := PatDecode.EffectParam;

          (* Exiting current table *)
          MyPatTabRunning := False;
        end;
        if PatDecode.EffectNumber = 15 then //set speed
        begin
          (* Range: xy = 00h-1Fh for speed; xy = 20h-FFh for BPM *)
          (* Note: The rate in Hz is equal to Hz = bpm * 2 / 5 (125BPM = 50Hz = 20mS) *)
          if PatDecode.EffectParam >= 32 then
            MyTmrInterval := 5000 / (PatDecode.EffectParam * 2)
          else
            MyTickSpeed := PatDecode.EffectParam;
        end;
      end;

      if not MyPatTabRunning then
      begin
        if PatDecode.EffectNumber <> 11 then inc(MySongPos);
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
  StopPlaying;

  (* mute non-used channels to be sure *)
  For Ch := 1 to MaxNumChBufs do
    MySongLogic[Ch].MyVolume := 0;
  with MySongLogic[1] do
  begin
    (* set max volume.. *)
    MyVolume := 64;
    (* ..not with sliding volume, but fixed *)
    MyVolSlide := 0;
    (* kill Porta *)
    MyPortaSpeed := 0;
    (* kill PortaTo *)
    MyPortaToSpeed := 0;
    OldPortaToSpeed := 0;
    OldPortaToGoal := 0;
    (* kill vibrato *)
    MyVibSpeed := 0;
    MyVibDepth := 0;
    OldVibSpeed := 0;
    OldVibDepth := 0;
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
    (* Set default speed (request) *)
    MyTmrInterval := DefaultTmrSpeed;
    MyTickSpeed := DefaultTckSpeed;
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
    (* start at sample start *)
    MyInBufCnt := -1;
    (* Our 'previous buffer's last sample' is zero (we are not connected there since we are just starting a new song!) *)
    LastInSample := 0;
    InSmpUpRemain := 0;
    (* We have no previous frequency yet *)
    OldAmigaSpeed := 0;
    MyInPerPart := 0;
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
  (* Set Default speed (Actual) *)
  ActTmrInterval := DefaultTmrSpeed;
  ActTckSpeed := DefaultTckSpeed;

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

  with MySampleLogic[1] do
  begin
    if CBSample.ItemIndex <= 0 then
    begin
      PlayingRaw := True;

      (* play full sample once *)
      while (MyInBufCnt < MyTotalSampleSize) and PlayingRaw do
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

      PlayingRaw := True;
      with MySampleInfo[CBSample.ItemIndex-1] do
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
        while (MyInBufCnt < Length) and PlayingRaw do
        begin
          PlaySample(1, MySamplesPtr + MyRawOffset, Length, 0, MyRawRptLen, RptStart, 428, FineTune); (* 428 denotes C2 (C, 2nd octave) *)
          MixAndOutputSamples(True);
          sleep(5);
          Application.processmessages;
        end;
      end;
    end;
  end;

  if PlayingRaw then
  begin
    (* Wait for sample end *)
    while not AllBufsDone do
    begin
      sleep(5);
      application.ProcessMessages;
    end;
    if PlayingRaw then
    begin
      PlayingRaw := False;
      StopPlaying; //note might be done by PlaySong!
    end;
  end;
end;


Function TModMain.AllBufsDone: Boolean;
var
  a: Integer;
begin
  AllBufsDone := True;

  if WaveOutIsOpen then
    for a := 0 to NumAudioBufs-1 do
      if pheader[a]^.dwFlags and WHDR_DONE = 0 then AllBufsDone := False;
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

procedure TModMain.menuMPSettingsClick(Sender: TObject);
begin
  MPSettings.Show;
end;

procedure TModMain.FormDestroy(Sender: TObject);
var
  a: Integer;
begin
  for a := 0 to NumAudioBufs-1 do
    Dispose(PHeader[a]);
  Dispose(PMywfx);
  Dispose(pMyWaveOutDev);
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

procedure TModMain.StopPlaying;
begin
  IO_Timer.Enabled := False;

  if not PlayingRaw then
  begin
    MySongPaused := True;
    BtnPlaySong.Caption := 'Play song';
    StoppingMySong := True;
  end;
  PlayingRaw := False;

  if WaveOutIsOpen then waveOutReset(PMyWaveOutDev^); //Also marks all buffers as 'WHDR_DONE'

  (* End Wave file writer if it was busy *)
  if CBSaveWave.Checked then CompleteWaveFile;
  CBSaveWave.Enabled := True;
end;

function TModMain.MixAndOutputSamples(PlayRaw: Boolean): Boolean;
var
  MyStat,
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

  (* switch to next buffer *)
  Inc(BufNr);
  if BufNr >= NumAudioBufs then BufNr := 0;

  (* we must wait for our buffer to complete if it's still busy *)
  if WaveOutIsOpen then
    while pheader[BufNr]^.dwFlags and WHDR_DONE = 0 do sleep(5);

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
          MyConvBuf[BufNr][OutCnt*NumOutChans + 0] := Round(MyChBuf[1][OutCnt] * 0.7);
          (* Right channel *)
          MyConvBuf[BufNr][OutCnt*NumOutChans + 1] := Round(MyChBuf[1][OutCnt] * 0.7);
        end
        else
          MyConvBuf[BufNr][OutCnt] := Round(MyChBuf[1][OutCnt] * 0.7);
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
          MyConvBuf[BufNr][OutCnt*NumOutChans + 0] := Round(LeftVal);
          MyConvBuf[BufNr][OutCnt*NumOutChans + 1] := Round(RightVal);
        end
        else
        begin
          (* Mono output *)
          MySample := 0;
          for ChCnt := 1 to MyMediaRec.Channels do
            if ChOn[ChCnt] then MySample := MySample + MyChBuf[ChCnt][OutCnt];
          MyConvBuf[BufNr][OutCnt] := Round(MySample * NumOutChans / MyMediaRec.Channels);
        end;
      end;
    end;

    (* Save buffer in file if requested *)
    if CBSaveWave.Checked then WriteWaveChunk(BufNr, MyOutBufLen * NumOutChans);
  end;

  if MyAppClosing then
  begin
    Result := False;
    exit;
  end;
  if not WaveOutIsOpen then
  begin
    RunDecInfo.Items.Add('No Wave Output available!');
    (* We might still be writing a wave output file.. *)
    Result := True;
    exit;
  end;

  (* All buffers have the same size.. (MyOutBufLen) *)
  PHeader[BufNr]^.lpData := PChar(Addr(MyConvBuf[BufNr]));
  PHeader[BufNr]^.dwBufferLength := MySampleLogic[1].MyOutBufLen * 2 * NumOutChans;  //must be in bytes!
  PHeader[BufNr]^.dwFlags := 0;  //zie MSDN specs. MMsystem vult de flags in.
  PHeader[BufNr]^.dwLoops := 0;  //loops doen we dmv uitrollen en daarmee herhalen
  PHeader[BufNr]^.dwBytesRecorded := 0;
  MyStat := waveOutPrepareHeader(PMyWaveOutDev^, pheader[BufNr], sizeof(WAVEHDR));
  if MyStat <> MMSYSERR_NOERROR then
  begin
    RunDecInfo.Items.Add('waveOutPrepareHeader returns error: '+IntToStr(Mystat)+ '.');
    exit;
  end;
  if pheader[BufNr]^.dwFlags and WHDR_PREPARED <> WHDR_PREPARED then
  begin
    RunDecInfo.Items.Add('waveOutPrepareHeader did not prepare for buffer #' + IntToStr(BufNr+1) + '! Aborting.');
    exit;
  end;
  MyStat := waveOutWrite(PMyWaveOutDev^, pheader[BufNr], sizeof(WAVEHDR));
  if MyStat <> MMSYSERR_NOERROR then
  begin
    RunDecInfo.Items.Add('waveOutWrite did not output buffer #' + IntToStr(BufNr+1) + '! Aborting.');
    exit;
  end;
  MyStat := waveOutUnPrepareHeader(PMyWaveOutDev^, pheader[BufNr], sizeof(WAVEHDR));
end;

function TModMain.PlaySample(MyCh: Integer;
  MySmpPtr: PInt8; MyBufLen, MyFirstStart, MyRptLen, MyRptStart: Integer; AmigaSpeed: Word; FineTune: ShortInt): Boolean;
var
  MyInPeriod,
  MyFullBufLen,
  OutCnt, i    : Integer;
  MyVibDelta   : Single;
  MySample1,
  MySample2,
  MyTmpSample,
  MySampleStep : Single;
  Up           : Single;
  MyTickCnt    : Integer;
  S            : String;


procedure ChkDoPortaVibrato(MyIncFactor: Integer);
var
  MyCurPeriod,
  MyTickTime     : Integer;
  OldSmpVibUpCnt : Single;
begin
  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    Inc(MySmpTCnt, MyIncFactor);
    MyTickTime := Round(ActTmrInterval * MPSettings.MySettings.OutSampleRate / 1000);
    (* Execute once per 'Tick' -excluding Tick 0- .. *)
    if MySmpTcnt >= MyTickTime then
    begin
      (* We might vary a single upsampled input sample in time for the current tick, but on average we stay in sync! *)
      MySmpTCnt := MySmpTcnt - MyTickTime;
      (* Keep track of the ticks executed.. *)
      Inc(MyTickCnt);
      if MyTickCnt < ActTckSpeed then
      begin
        if (RetrigEvery > 0) and ((MyTickCnt mod RetrigEvery) = 0) then
        begin
          (* Reset input to first sample (We might skip first part of sample buffer though) *)
          MyInBufCnt := MyFirstStart;
          (* We must set a new target input sample.. (the last done sample (#1) is still the same of course) *)
          MySample2 := (MySmpPtr[MyFirstStart] shl 8) * MyVolume / 64;
        end;
        (* get sinusoidal value from position.. *)
        // Fixme: We should add the other possible waveforms.. (Effect E4x: Set Vibrato Waveform)
        MyVibDelta := MySineTable[MyVibratoPos and $1f];
        if MyVibratoPos < 0 then MyVibDelta := -MyVibDelta;
        (* get amplitude and combine to get absolute frequency modifier.. *)
        MyVibDelta := (MyVibDelta * MyVibDepth) / 128;
        (* rotate through sinetable.. *)
        MyVibratoPos := MyVibratoPos + MyVibSpeed;
        if MyVibratoPos > 31 then MyVibratoPos := MyVibratoPos - 64;
        (* do porta-to-note effect as well.. *)
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
        (* do porta effect as well.. *)
        MyPrtaPerPart := MyPrtaPerPart + MyPortaSpeed;
        (* keep effects in range.. *)
        if (MyPrtaPerPart + MyPrtaToPerPart + MyInPerPart + MyVibDelta) > 856 then    (* lowest  note at pitch 0: C1 *)
          MyPrtaPerPart := Round(856 - (MyPrtaToPerPart + MyInPerPart + MyVibDelta));
        if (MyPrtaPerPart + MyPrtaToPerPart + MyInPerPart + MyVibDelta) < 113 then    (* highest note at pitch 0: B3 *)
          MyPrtaPerPart := Round(113 - (MyPrtaToPerPart + MyInPerPart + MyVibDelta));
        (* finally calculate new upsampling factor *)
        MyInSmpUp :=
          MPSettings.MySettings.OutSampleRate * 2 * (MyInPerPart + MyPrtaPerPart + MyPrtaToPerPart + MyVibDelta) /
          MPSettings.MySettings.MyTVSpeed;
        if MyInSmpUp <= 0 then MyInSmpUp := up; (* failsave! *)
        (* Correct for already played period-part of the current input sample *)
        OldSmpVibUpCnt := SmpVibUpCnt;
        SmpVibUpCnt := MyInSmpUp - (1-InSmpUpRemain);
        (* Correct our Tick counter if needed *)
        MyIncFactor := Trunc(SmpVibUpCnt) - Trunc(OldSmpVibUpCnt);
        Inc(MySmpTCnt, MyIncFactor);
        (* We also update our 'Tick' volume here! *)
        UpdateMyVolSlide(MyCh, MyTickCnt);
      end;
    end;
  end;
end;

begin
  Result := True;
  (* In case of looping samples we need to check against the full sample length at the moment
      such a repeat starts. Important if they loop more than once within -one- output buffer! *)
  MyFullBufLen := MyBufLen;

  (* We keep track of our locally 'generated' ticks *)
  MyTickCnt := 0;

  (* if no new period is supplied, we are supposed to re-use the previous one! *)
  with MySampleLogic[MyCh] do
  begin
    if AmigaSpeed = 0 then AmigaSpeed := OldAmigaSpeed;
    OldAmigaSpeed := AmigaSpeed;
  end;

  (* find AmigaSpeed in our default (non-finepitched) notes lookup table *)
  MyInPeriod := -1;
  for i := 1 to 36 do
    if (AmigaSpeed > (MyPeriodTable[0, i]-2)) and (AmigaSpeed < (MyPeriodTable[0, i]+2)) then MyInPeriod := i;
  if MyInPeriod < 0 then
  begin
    (* Amigaspeed not found in table means no sound. Play empty buffer and exit. *)
    Result := PlayEmptySample(MyCh);
    exit;
  end;
  (* now apply sample's finetune setting and fetch finetuned AmigaSpeed. *)
  if CBPatDebug.Checked and not MyAppClosing then
  begin
    S := '';
    if MySongLogic[MyCh].MySongEnding then S := 'Ch' + IntToStr(MyCh) + ': ';
    RunDecInfo.Items.Add(S + 'Inspeed: '  + IntToStr(Amigaspeed)+', Index: ' + IntToStr(MyInPeriod) +
      ', Finetune: ' + IntToStr(FineTune) + ', Outspeed: ' + inttostr(MyPeriodTable[FineTune, MyInPeriod]));
    RunDecInfo.ItemIndex := RunDecInfo.Items.Count - 1;
  end;
  MyInPeriod := MyPeriodTable[FineTune, MyInPeriod];

  (* calculate initial (coarse) up-sample rate *)
  up := MPSettings.MySettings.OutSampleRate * 2 * (MyInPeriod) / MPSettings.MySettings.MyTVSpeed;

  with MySongLogic[MyCh], MySampleLogic[MyCh] do
  begin
    (* new sample starting: point to first buffer position *)
    if MyInBufCnt < 0 then
    begin
      (* We might skip first part of sample upon start *)
      (* Please note: 'value-1' below is used because of inter-new-sample-data-connection requirement! *)
      MyInBufCnt := MyFirstStart - 1;
      (* Setup neutral since on tick0 of a new sample ChkDoPortaVibrato should not be executed *)
      MyInSmpUp := Up; // Fixme for Vibrato: depends on Effect E4x: Set Vibrato Waveform! (retrig at Tick0 or no)
      SmpVibUpCnt := MyInSmpUp;
      (* Reset our internal Tick Counter *)
      MySmpTCnt := 0;
      (* Vibrato is 'centered' (zero) _only_ at new sample start! *)
      // Fixme: depends on Effect E4x: Set Vibrato Waveform! (retrig at Tick0 or not)
      MyInPerPart := MyInPeriod;
      MyVibratoPos := 0;
      (* Cancel possible buffer interconnection data as we do a 'fresh start' *)
      MyConBufFill := 0;
    end;

    (* We determine our output buffer size per note: tick-time * nr-ticks-per-note *)
    MyOutBufLen := Round(ActTmrInterval * ActTckSpeed * MPSettings.MySettings.OutSampleRate / 1000);
    (* First place still pending data from the previous buffer in our output buffer (if any) *)
    for i := 0 to MyConBufFill - 1 do
      MyChBuf[MyCh][i] := MyConBufContent[i];
    (* don't forget to add the interconnection data for our local 'Tick' time check *)
    ChkDoPortaVibrato(MyConBufFill);
    (* convert while copying sample from input samplerate to output rate (OutSampleRate) *)
    OutCnt := MyConBufFill;
    while OutCnt < MyOutBufLen do
    begin
      if MyAppClosing or (MySongPaused and not PlayingRaw) then
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

      if MyInBufCnt+1 < MyBufLen then
        MySample2 := (MySmpPtr[MyInBufCnt+1] shl 8) * MyVolume / 64
      else
        if MyRptLen > 2 then
          MySample2 := (MySmpPtr[MyRptStart] shl 8) * MyVolume / 64
        else
          MySample2 := 0;

      (* Keep last newly fetched buffer value for the next buffer as that needs to connect to this one (always!) *)
      LastInSample := MySample2;

      (* We need to end/correct for the previous unfinished sample in time and amplitude first *)
      SmpVibUpCnt := SmpVibUpCnt - (1-InSmpUpRemain);
      ChkDoPortaVibrato(1 + Trunc(SmpVibUpCnt)); (* counting mix sample plus loop samples below *)
      (* Determine the upsampling value step per resulting full sample *)
      MySampleStep := (MySample2 - MySample1) / MyInSmpUp;
      (* we 'play' 'InSmpUpRemain' time of the old sample, combined with '1-InSmpUpRemain' of the new sample *)
      MyTmpSample := MySample1 + MySampleStep * (1-InSmpUpRemain);
      MyChBuf[MyCh][OutCnt] := Round(MyTmpSample);
      Inc(OutCnt);
      (* please note that we always finish upsamping each inbuffer sample, even across the set outbuffer boundary! *)
      While (SmpVibUpCnt >= 1.0) do
      begin
        MyTmpSample := MyTmpSample + MySampleStep;
        MyChBuf[MyCh][OutCnt] := Round(MyTmpSample);
        SmpVibUpCnt := SmpVibUpCnt - 1;
        Inc(OutCnt);
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

  (* If we overfilled our output buffer transfer it to the next runner-up... *)
  (* Please note that this makes a possible new volume setting in the next round vary max. a single input sample in time.. *)
  with MySampleLogic[MyCh] do
    if OutCnt > MyOutBufLen then
    begin
      MyConBufFill := OutCnt - MyOutBufLen;
      if MyConBufFill > MyConBufSize then
      begin
        RunDecInfo.Items.Add('WARNING: Connection buffer too small! (Need ' + IntToStr(MyConBufFill) + ' samples)');
        MyConBufFill := MyConBufSize;
      end;
      for i := 0 to MyConBufFill - 1 do
        MyConBufContent[i] := MyChBuf[MyCh][MyOutBufLen + i];
    end
    else
      MyConBufFill := 0;
end;

function TModMain.PlayEmptySample(MyCh: Integer): Boolean;
var
  OutCnt, i    : Integer;
  MyTmpSample,
  MySampleStep : Single;
  Up           : Single;
  S            : String;
begin
  Result := True;

  if CBPatDebug.Checked and not MyAppClosing then
  begin
    S := '';
    if MySongLogic[MyCh].MySongEnding then S := 'Ch' + IntToStr(MyCh) + ': ';
    RunDecInfo.Items.Add(S + 'Playing empty buffer (silence)');
    RunDecInfo.ItemIndex := RunDecInfo.Items.Count - 1;
  end;

  with MySampleLogic[MyCh] do
  begin
    (* We determine our output buffer size per note: tick-time * nr-ticks-per-note *)
    MyOutBufLen := Round(ActTmrInterval * ActTckSpeed * MPSettings.MySettings.OutSampleRate / 1000);
    (* We have no period available: simulate it (using 4) as we need to slowly fallback from current DC-level to zero *)
    Up := 4;

    (* First place still pending data from the previous buffer in our output buffer (if any) *)
    for i := 0 to MyConBufFill - 1 do
      MyChBuf[MyCh][i] := MyConBufContent[i];
    (* setup (rest as) empty output buffer (silence) *)
    OutCnt := MyConBufFill;
    while OutCnt < MyOutBufLen do
    begin
      if Up <> 0 then (* We need to fallback to zero level only once.. *)
      begin
        (* Determine the upsampling value step per resulting full sample *)
        MySampleStep := (0 - LastInSample) / Up;
        (* We need to end/correct for the previous unfinished sample in time and amplitude first *)
        Up := Up - (1-InSmpUpRemain);
        (* we 'play' 'InSmpUpRemain' time of the old sample, combined with '1-InSmpUpRemain' of the new sample *)
        MyTmpSample := LastInSample + MySampleStep * (1-InSmpUpRemain);
        MyChBuf[MyCh][OutCnt] := Round(MyTmpSample);
        Inc(OutCnt);
        (* gradually fall-back to zero level.. *)
        While (Up >= 1.0) do
        begin
          MyTmpSample := MyTmpSample + MySampleStep;
          MyChBuf[MyCh][OutCnt] := Round(MyTmpSample);
          Up := Up - 1;
          Inc(OutCnt);
        end;
        Up := 0;
      end;
      (* setup rest of output buffer to silence *)
      MyChBuf[MyCh][OutCnt] := 0;
      Inc(OutCnt);
    end;

    (* Update our last newly fetched buffer value for the next buffer to zero as that's what we did play last *)
    LastInSample := 0;
    (* Connection buffer is empty *)
    MyConBufFill := 0;
  end;
end;

procedure TModMain.MenuFileOpenClick(Sender: TObject);
begin
  (* stop a possible playing song first *)
  StopPlaying;

  if not OpenModFile.Execute then exit;
  if not LoadFile(OpenModFile.FileName) then
  begin
    MyOpenInFile := '';
    Exit;
  end;
  (* Remember our intputfilename plus it's path, but excluding its extension *)
  MyOpenInFile := OpenModFile.FileName;
  MyOpenInFile := Copy(MyOpenInFile, 1, LastDelimiter('.',MyOpenInFile) - 1);

 // MyPath := GetCurrentDir + PathDelim + 'derde_oefening_roeli_jo_ruud.wav';
 // sndPlaySound(PChar(MyPath), SND_SYNC or SND_NODEFAULT);
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
    wFormatTag      := PMywfx^.wFormatTag;
    nChannels       := PMywfx^.nChannels;
    nSamplesPerSec  := PMywfx^.nSamplesPerSec;
    nAvgBytesPerSec := PMywfx^.nAvgBytesPerSec;
    nBlockAlign     := PMywfx^.nBlockAlign;
    wBitsPerSample  := PMywfx^.wBitsPerSample;
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

function TModMain.WriteWaveChunk(MyBuffer, MySize: Integer): Boolean;
begin
  Result := False;
  if TFileRec(MyWaveFile).Mode = fmClosed then exit;

  (* write data *)
  BlockWrite(MyWaveFile,MyConvBuf[MyBuffer][0], MySize * 2);
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
  MyCalcSize : Integer;
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
    (* we only support 4 channel fileformats for now *)
    if not Supported then
    begin
      CloseFile(TF);
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
          RunDecInfo.Items.Add('WARNING: Sample size to big (> 9999 bytes)');
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
    for a := 0 to 127 do
      (* Workaround: Some files have illegal entries not tied to the pattern tables (Pinball Dreams Intro mod (val = 255)) *)
      if (Patterns[a] < MyMediaRec.MaxPatTables) and (Patterns[a] > IgnSetNrOfPats) then
        IgnSetNrOfPats := Patterns[a];
    Inc(IgnSetNrOfPats);
    (* Determine the total patterns size in bytes *)
    MyTotalPatternSize := MyMediaRec.Channels * 4 * 64 * IgnSetNrOfPats;
    (* Determine the total Samples size in bytes *)
    MyTotalSampleSize := 0;
    MyCorr := 0;
    for a := 0 to MyMediaRec.MaxSamples - 1 do
    begin
      MyTotalSampleSize := MyTotalSampleSize + MySampleInfo[a].Length;
      if (MySampleInfo[a].Length <= 2) and (MySampleInfo[a].RptLength <= 2) then
        Inc(MyCorr, MySampleInfo[a].Length);
    end;
    (* Workaround: sometimes empty samples mistakenly have size '2'. Correct.. *)
    if MyCorr <> 0 then
    begin
      RunDecInfo.Items.Add('Empty samples marked non-empty, decreasing samplesize expectation (' + IntToStr(MyCorr) + ').');
      Dec(MyTotalSampleSize, MyCorr);
    end;
    (* Check for filesize error.. *)
    MyFileSize := FileSize(TF);
    MyCalcSize := SizeOf(TModFileHeader) + MyTotalPatternSize + MyTotalSampleSize;
    if OrigFormatFile then MyCalcSize := MyCalcSize - 16 * SizeOf(TModFileSampleInfo) - 4;
    if MyFileSize = MyCalcSize then
    begin
      if MyCorr = 0 then
        RunDecInfo.Items.Add('Total filesize OK (' + IntToStr(MyFileSize) + ').')
      else
        RunDecInfo.Items.Add('Corrected total filesize expectation OK (' + IntToStr(MyFileSize) + ').');
    end
    else
    begin
      RunDecInfo.Items.Add('Filesize error: Expected ' + IntToStr(MyCalcSize) + ', Got ' + IntToStr(MyFileSize) + '.');
      if MyFileSize < MyCalcSize then
      begin
        RunDecInfo.Items.Add('File to small, aborting.');
        CloseFile(TF);
        FileID:= '    ';
        FrmMediaInfo.MyUpdate;
        Exit;
      end;
      (* Determine if we have (a) extra Pattern table(s) *)
      a := MyFileSize - MyCalcSize;
      if a mod (MyMediaRec.Channels * 4 * 64) <> 0 then
      begin
        RunDecInfo.Items.Add('File to big with non-Pattern table size factor, skipping pre-sampledata surplus space.');
        (* 'Dump' the pre-sampledata surplus at the end of our Patterns tables: it does not do any harm there. *)
        MyTotalPatternSize := MyTotalPatternSize + a;
      end
      else
      begin
        (* We have (a) extra Pattern table(s) *)
        a := a div (MyMediaRec.Channels * 4 * 64);
        RunDecInfo.Items.Add('File has ' + IntToStr(a) + ' extra Pattern table(s), adding Patterns to song.');
        (* Add them 'in order' to the end of our Song and hope for the best. *)
        While a > 0 do
        begin
          Patterns[SongLength] := IgnSetNrOfPats;
          Inc(SongLength);
          Inc(IgnSetNrOfPats);
          Dec(a);
        end;
        (* Finally correct the total patterns size in bytes so we load it all correctly. *)
        MyTotalPatternSize := MyMediaRec.Channels * 4 * 64 * IgnSetNrOfPats;
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

