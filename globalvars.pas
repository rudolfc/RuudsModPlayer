unit GlobalVars;

interface

uses
  Classes, SysUtils;

type
  TModMediaRec = packed RECORD
                 FileLoaded     : BOOLEAN;
                 FileName       : string[200];
                 Channels       : Byte;
                 MaxPatTables   : Word;
                 MaxSamples     : Word;
                 MaxSampleSize  : LongWord;
              END;

  TModFileSampleInfo = packed RECORD
                 FTitle     : array [0..21] of AnsiChar;
                 FLength    : Word;
                 FFineTune  : ShortInt;
                 FVolume    : Byte;
                 FRptStart  : Word;
                 FRptLength : Word;
              END;

  TModPlaySampleInfo = packed RECORD
                 Title     : array [0..21] of AnsiChar;
                 Length    : LongWord;
                 FineTune  : ShortInt;
                 Volume    : Byte;
                 RptStart  : LongWord;
                 RptLength : LongWord;
              END;

  TModFileHeader = packed RECORD
                 SongTitle      : array [0..19] of AnsiChar;
                 FSampleInfo    : array [0..30] of TModFileSampleInfo;
                 SongLength     : Byte;
                 IgnSetNrOfPats : Byte;
                 Patterns       : array [0..127] of Byte;
                 FileID         : array [1..4] of AnsiChar;      //should hold i.e. 'M.K.'
              END;

  (* Note: File layout differs from TModPattern layout(!) *)
  TModPattern = packed RECORD
                 SampleNumber : Byte;
                 SamplePeriod : Word;
                 EffectNumber : Byte;
                 EffectParam  : Byte;
              END;

  TNotes       = Array[ 1..36] of Word;
  TPeriodTable = Array[-8.. 7] of TNotes;


  { format of WAV file header (Total size = 44 bytes) }
  TWavHeader = packed record                   { parameter description }
    rId             : array [0..3] of AnsiChar;  { 'RIFF'  4 characters }
    rLen            : longint;                   { length of DATA + FORMAT chunk (= wSampleLength + 36) }
    { FORMAT CHUNK }
    wfId            : array [0..7] of AnsiChar;  { 'WAVEfmt ' }
    fLen            : longint;                   { length of FORMAT DATA = 16 }
    { format data: See MMSystem PWaveFormatEx }
    wFormatTag      : word;                      { $01 = PCM }
    nChannels       : word;                      { 1 = mono, 2 = stereo }
    nSamplesPerSec  : longint;                   { Sample frequency ie 11025}
    nAvgBytesPerSec : longint;                   { = nChannels * nSamplesPerSec * (nBitsPerSample/8) }
    nBlockAlign     : word;                      { = nChannels * (nBitsPerSAmple / 8 }
    wBitsPerSample  : word;                      { 8 or 16 }
    { DATA CHUNK }
    dId             : array [0..3] of AnsiChar;  { 'data' }
    wSampleLength   : longint;                   { length of SAMPLE DATA }

  { Sample data starts at file offset 44.
      The sample data must end on an even byte boundary. All numeric data fields are in the Intel format
      of low-high byte ordering.
      8-bit samples are stored as unsigned bytes, ranging from 0 to 255.
      16-bit samples are stored as 2's-complement signed integers, ranging from -32768 to 32767.

      For multi-channel data, samples are interleaved between channels, like this:

      sample 0 for channel 0
      sample 0 for channel 1
      sample 1 for channel 0
      sample 1 for channel 1
      ... }
  end;


const
  TAB = CHR(09);

var
  MyMediaRec: TModMediaRec;
  MyFileHeader: TModFileHeader;
  MyProductVersionMS,
  MyProductVersionLS       : Cardinal;(* Product version as numbers *)
  MyPatternPtr             : Puint32;
  MySamplesPtr,
  MyTmpFileData            : PInt8;
  MyTotalPatternSize,
  MyTotalSampleSize        : Integer;
  MyAppClosing,
  MyAppStarting,
  StoppingMySong,
  OrigFormatFile,
  MySongPaused,
  WaveOutIsOpen,
  WaveOutErrReported       : Boolean;
  OrigFmtTmrSpeed          : Single;
  MySampleInfo             : array [0..30] of TModPlaySampleInfo;

implementation



end.

