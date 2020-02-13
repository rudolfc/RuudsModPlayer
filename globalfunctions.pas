unit GlobalFunctions;

{$mode delphi}

interface

uses
  FileInfo,         {Laz: To fetch executable version info}
  LazVersion,       {Laz: To fetch the Lazarus version}
  Classes, SysUtils,
  GlobalVars;

function  GetVersionInfo: string; (* LAZ: Multiplatform method *)
FUNCTION  ByteToHexString(B: BYTE): STRING;
FUNCTION  WordToHexString(w: Word): STRING;
FUNCTION  LongWordToHexString(w: LongWord): STRING;
Function  DecodePattern(MyPattern: Uint32; Translate: Boolean): TModPattern;

implementation

//uses


(* Laz: automatically fetch program version info from internal resource *)
function GetVersionInfo: string; (* LAZ: Multiplatform method *)
var
  FileVerInfo: TFileVersionInfo;
  MyVersion  : TProgramVersion;
begin
  Result := 'Err: No info.';

  FileVerInfo:= TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
//    writeln('Company: ',FileVerInfo.VersionStrings.Values['CompanyName']);
//    writeln('File description: ',FileVerInfo.VersionStrings.Values['FileDescription']);
//    writeln('File version: ',FileVerInfo.VersionStrings.Values['FileVersion']);
//    writeln('Internal name: ',FileVerInfo.VersionStrings.Values['InternalName']);
//    writeln('Legal copyright: ',FileVerInfo.VersionStrings.Values['LegalCopyright']);
//    writeln('Product name: ',FileVerInfo.VersionStrings.Values['ProductName']);
//    writeln('Product version: ',FileVerInfo.VersionStrings.Values['ProductVersion']);

  Result := FileVerInfo.VersionStrings.Values['FileVersion'];
  (* V6.1.3 Keep the numbers for our reference as well *)
  GetProgramVersion(MyVersion);
  MyProductVersionMS := MyVersion.Major shl 16 + MyVersion.Minor;
  MyProductVersionLS := MyVersion.Revision shl 16 + MyVersion.Build;

  finally
    FileVerInfo.Free;
  end;
end;

FUNCTION ByteToHexString(B: BYTE): STRING;
const
  hexChars: array [0..$F] of AnsiChar =
    '0123456789ABCDEF';
BEGIN
  ByteToHexString := hexChars[B shr 4] + hexChars[B AND $F] ;
END;

FUNCTION WordToHexString(w: Word): STRING;
const
  hexChars: array [0..$F] of AnsiChar =
    '0123456789ABCDEF';
BEGIN
  WordToHexString := hexChars[Hi(w) shr 4]+ hexChars[Hi(w) and $F] + hexChars[Lo(w) shr 4]+hexChars[Lo(w) and $F];
END;

FUNCTION LongWordToHexString(w: LongWord): STRING;
BEGIN
  LongWordToHexString := WordToHexString(w shr 16) + WordToHexString(w);
END;

Function DecodePattern(MyPattern: Uint32; Translate: Boolean): TModPattern;
var
  PatDecode : TModPattern;
begin
  {
  //Amiga:
  aaaaBBBB CCCCCCCCC DDDDeeee FFFFFFFFF
  //rud: looks like this on Intel:
  FFFFFFFFF DDDDeeee CCCCCCCCC aaaaBBBB

  aaaaDDDD     = sample number
  BBBBCCCCCCCC = sample period value
  eeee         = effect number
  FFFFFFFF     = effect parameters
  }

  PatDecode.SampleNumber := ((MyPattern shr  0) and   $f0) or ((MyPattern shr 20) and   $0f);
  PatDecode.SamplePeriod := ((MyPattern shl  8) and $0f00) or ((MyPattern shr  8) and $00ff);
  PatDecode.EffectNumber := ((MyPattern shr 16) and   $0f);
  PatDecode.EffectParam  := ((MyPattern shr 24) and   $ff);

  (* In 'original format' files we need to translate some effects *)
  (* Note: Not blocking other effects since they just might be there (i.e. effect 'Set Speed') *)
  if OrigFormatFile and Translate then
  begin
    (* Translate effect 'Arpeggio' *)
    if PatDecode.EffectNumber = 1 then PatDecode.EffectNumber := 0;
    (* Translate effect 'Pitchbend' *)
    if PatDecode.EffectNumber = 2 then
    begin
      if (PatDecode.EffectParam shr 4) = 0 then
        PatDecode.EffectNumber := 1                            (* EffectParam stays same *)
      else
        if (PatDecode.EffectParam and $0f) = 0 then
          PatDecode.EffectParam := PatDecode.EffectParam shr 4 (* EffectNumber stays same *)
        else
        begin
          (* Illegal effect, should not happen: Kill it. *)
          PatDecode.EffectNumber := 0;
          PatDecode.EffectParam := 0;
        end;
    end;
  end;

  result := PatDecode;
end;

end.

