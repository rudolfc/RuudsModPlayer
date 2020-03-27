unit dlg_mediainfo;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TFrmMediaInfo }

  TFrmMediaInfo = class(TForm)
    BitBtn1: TBitBtn;
    BtSaveInfo: TBitBtn;
    BtClearTxt: TBitBtn;
    CbDmpTables: TCheckBox;
    ListBox1: TListBox;
    SaveDialog1: TSaveDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure BtClearTxtClick(Sender: TObject);
    procedure BtSaveInfoClick(Sender: TObject);
    procedure CbDmpTablesChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure MyUpdate;

  end;

var
  FrmMediaInfo: TFrmMediaInfo;
  MT : TEXT;
  SavedFilePath : STRING;

implementation

uses
  GlobalVars, GlobalFunctions, ModPlayMain;

{$R *.lfm}

{ TFrmMediaInfo }

procedure TFrmMediaInfo.BitBtn1Click(Sender: TObject);
begin
  Hide;
end;

procedure TFrmMediaInfo.BtClearTxtClick(Sender: TObject);
begin
  ListBox1.Items.Clear;
end;

procedure TFrmMediaInfo.BtSaveInfoClick(Sender: TObject);
var
  i : Integer;
begin
  if ListBox1.items.Count = 0 then exit;

  SaveDialog1.InitialDir := SavedFilePath;
  if not SaveDialog1.Execute then exit;
  SavedFilePath := SaveDialog1.FileName;

  AssignFile(MT,SavedFilePath);
  Rewrite(MT);
  For i := 0 TO ListBox1.items.Count-1 DO
  WriteLn(MT,ListBox1.items[i]);
  CloseFile(MT);
end;

procedure TFrmMediaInfo.CbDmpTablesChange(Sender: TObject);
begin
  MyUpdate;
end;

procedure TFrmMediaInfo.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  (* Only accept close if we aren't busy, otherwise the mainform gets sent to the background.. *)
  CanClose := ModMain.Enabled;
end;

procedure TFrmMediaInfo.FormCreate(Sender: TObject);
begin
  SavedFilePath := GetCurrentDir;
  MyUpdate;
end;

procedure TFrmMediaInfo.MyUpdate;
var
  a, b, c   : Integer;
  s         : String;
  PatDecode : TModPattern;
begin
  (* Don't load a new file when we are busy reading the current one!! (or corrupted info will be listed) *)
  ModMain.Enabled := False;
  (* Also block our local items *)
  BtClearTxt.Enabled  := False;
  BtSaveInfo.Enabled  := False;
  BitBtn1.Enabled     := False;
  CbDmpTables.Enabled := False;
  (* Clear MediaInfo *)
  ListBox1.Items.Clear;

  with MyFileHeader do
  begin
    if not MyMediaRec.FileLoaded then
    begin
      if FileID <> '    ' then
      begin
        if (Ord(FileID[1]) < 32) or (Ord(FileID[1]) > 126) then  FileID[1] := '?';
        if (Ord(FileID[2]) < 32) or (Ord(FileID[2]) > 126) then  FileID[2] := '?';
        if (Ord(FileID[3]) < 32) or (Ord(FileID[3]) > 126) then  FileID[3] := '?';
        if (Ord(FileID[4]) < 32) or (Ord(FileID[4]) > 126) then  FileID[4] := '?';
        ListBox1.Items.Add('No File Loaded: Wrong FileID (' + FileID  + ')');
      end
      else
        ListBox1.Items.Add('No File Loaded');
    end
    else
    begin
      with ListBox1.Items do
      begin
        Add('');
        Add('File Loaded: ' + MyMediaRec.FileName);
        Add('Type: ' + FileID + ' (' + IntToStr(MyMediaRec.Channels) + ' channels).');
        Add('Title: ' + SongTitle);
        Add('Song pattern length: ' + IntToStr(SongLength));
        Add('Patterns in file: ' + IntToStr(IgnSetNrOfPats));
        Add('Patterns order:');
        For a := 0 to 127 do
        begin
          if a mod 10 = 0 then
          begin
            if a < 120 then
              Add(IntToStr(a) + ' -  ' +
                  IntToStr(Patterns[a]) + ',  ' +
                  IntToStr(Patterns[a+1]) + ',  ' +
                  IntToStr(Patterns[a+2]) + ',  ' +
                  IntToStr(Patterns[a+3]) + ',  ' +
                  IntToStr(Patterns[a+4]) + ',  ' +
                  IntToStr(Patterns[a+5]) + ',  ' +
                  IntToStr(Patterns[a+6]) + ',  ' +
                  IntToStr(Patterns[a+7]) + ',  ' +
                  IntToStr(Patterns[a+8]) + ',  ' +
                  IntToStr(Patterns[a+9]) + ',')
            else
              Add(IntToStr(a) + ' -  ' +
                  IntToStr(Patterns[a]) + ',  ' +
                  IntToStr(Patterns[a+1]) + ',  ' +
                  IntToStr(Patterns[a+2]) + ',  ' +
                  IntToStr(Patterns[a+3]) + ',  ' +
                  IntToStr(Patterns[a+4]) + ',  ' +
                  IntToStr(Patterns[a+5]) + ',  ' +
                  IntToStr(Patterns[a+6]) + ',  ' +
                  IntToStr(Patterns[a+7]) + '.');
          end;
        end;
        Add('Samples:');
        For a := 0 to MyMediaRec.MaxSamples-1 do
        begin
          (* Note: If sample '0' should be played actually the previous sample should be repeated! *)
          Add(IntToStr(a+1) + ' -' + MySampleInfo[a].Title +
              '-   Length: ' + IntToStr(MySampleInfo[a].Length) +
              ' RptStart: ' + IntToStr(MySampleInfo[a].RptStart) +
              ' RptLength: ' + IntToStr(MySampleInfo[a].RptLength) +
              ' FineTune: ' + IntToStr(MySampleInfo[a].FineTune) +
              ' Volume: ' + IntToStr(MySampleInfo[a].Volume));
        end;
        Add('Total samples size: ' + IntToStr(MyTotalSampleSize));

        if CbDmpTables.Checked then
        begin
          Add('');
          Add('Patterns dump:');
          For a := 0 to IgnSetNrOfPats - 1 do                     //# pattern tables in file
          begin
            Add('Pattern table ' + IntToStr(a) + ':');
            For b := 0 to 63 do                                   //# entries per pattern table
            begin
              application.ProcessMessages;  //keep audio going while we are _busy_!

              s := 'Entry ' + IntToStr(b) + ':';
              For c:= 0 to MyMediaRec.Channels - 1 do             //# channels per pattern table entry
              begin
                //MyMediaRec.Channels * 4 * 64 * IgnSetNrOfPats
                PatDecode := DecodePattern(MyPatternPtr[c+(b*MyMediaRec.Channels)+(a*MyMediaRec.Channels*64)], False);
                if PatDecode.SampleNumber > 31 then  Add('>>>>>>>>>>>>>>>>>>>>> ERROR <<<<<<<<<<<<<<<<<<<');

                S := S + ' ch ' + IntToStr(c+1) +
                     ' = Sn:' + ByteToHexString(PatDecode.SampleNumber) +
                     ', Sp:' + WordToHexString(PatDecode.SamplePeriod) +
                     ', En:' + ByteToHexString(PatDecode.EffectNumber) +
                     ', Ep:' + ByteToHexString(PatDecode.EffectParam) + ';';
              end;
              Add(S);
            end;
          end;
        end;
      end;
    end;
  end;

  (* Accept loading new files again *)
  ModMain.Enabled := True;
  (* Also enable our local items again. *)
  BtClearTxt.Enabled  := True;
  BtSaveInfo.Enabled  := True;
  BitBtn1.Enabled     := True;
  CbDmpTables.Enabled := True;
end;

end.

