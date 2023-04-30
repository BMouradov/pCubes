{************************************************************}
{                                                            }
{  Unit uUtils                                               }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{  (LogFileOutput used for tests only)                       }
{************************************************************}

unit uUtils;
interface
uses
  Classes, Graphics;

const DELTA_ANGLE = 1e-4;
const DELTA_DISTANCE = 1e-5;

type
  TLogProc = procedure(const Mess: String) of object;

type
  TUtils = class
  private
    class var FLogProc: TLogProc;
    class var FLogWarningProc: TLogProc;
  public
    class property LogProc: TLogProc write FLogProc;
    class property LogWarningProc: TLogProc write FLogWarningProc;

    class procedure Log(const Mess: String); static;
    class procedure LogWarning(const Mess: String); static;
    class procedure LogFileOutput(const filepath, str: String); static;
    class function  GetStartDir: String; static;
    class function  AbsToRelative(const AbsPath, BasePath: string): string;
    class procedure GetFilesListRecursive(Folder, Mask: String; slFiles: TStringList); static;
    class function LoadFileFromZip(Stream: TStream; const ArchiveFileName: String; const FileName: String): Boolean;
    class procedure LoadFileListFromZip(const ArchiveFileName: String; FilesList: TStringList);
    class function  StrToColor(S: String): TColor; static;
    class function  ColorToStr(Color: Graphics.TColor): String; static;

    class function Trim(const S : String): String;
    class function Parse(var S : String; const Separators: String): String;
    class function ParseInt(var S: String): integer;
    class function HexToInt(const Value : String): integer;
    class function ParseHex(var S: String): integer;
    class function PrivPi(Al: extended): extended; // -Pi < x <= Pi
  end;

type
  TSettings = class
    class var AntiAlias: Boolean;
    class var Wired: Boolean;
    class var ShowAxes: Boolean;
    class var ShowNormals: Boolean;
    class var ContourMode: Boolean;
    class var MinimizeToTray: Boolean;
    class var MouseMode: integer;
    class var AppConfigDir: string;
    class var GraphicsEngineName: String;
    class var InteractiveDisabled: Boolean;
    class var PreScript: string;
    class var ScanFiles: Boolean;
    class procedure InitializeDefaults;
  end;

type

  { TAppStatus }

  TAppStatus = class
    class var PuzzleLoading: Boolean;
    class procedure InitializeDefaults;
    class function GetVersionName: string;
  end;

const ProgramName = 'pCubes';

implementation

uses
//  uLog,
  Windows, SysUtils, Unzip, ZipUtils;

class procedure TUtils.LogFileOutput(const filepath, str: String);
var F: THandle;
  Tmp: AnsiString;
begin
  F := FileOpen(filepath, fmOpenWrite {, ofOpenWrite or ofOpenAlways or ofShareDenyWrite });
  if F = INVALID_HANDLE_VALUE then
    F := FileCreate(filepath);
  if F = INVALID_HANDLE_VALUE then
    Exit;
  FileSeek(F, 0, 2 {spEnd});
  Tmp := AnsiString(str + {$IFDEF LIN}#10{$ELSE}#13#10{$ENDIF});
  FileWrite(F, PAnsiChar(Tmp)^, Length(Tmp));
  FileClose(F);
end;

class procedure TUtils.Log(const Mess: String);
begin
//  FLog.Add(Mess);
  if Assigned(FLogProc) then
    FLogProc(Mess);
end;

class procedure TUtils.LogWarning(const Mess: String);
begin
  if Assigned(FLogWarningProc) then
    FLogWarningProc(Mess);
end;

class function TUtils.GetStartDir: String;
//var Buffer:array[0..MAX_PATH] of Char;
//    i : Integer;
begin
  Result := ExtractFilePath(ParamStr(0));
  //Application.ExeName
  //i := GetModuleFileName( 0, Buffer, MAX_PATH );
  //for i := i downto 0 do
  //  if Buffer[i] = {$IFDEF LIN} '/' {$ELSE} '\' {$ENDIF} then begin
  //    Buffer[i + 1] := #0;
  //    break;
  //  end;
  //Result := UTF8Encode(Buffer);
end;

function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall; external 'shlwapi.dll' name 'PathRelativePathToW';

class function TUtils.AbsToRelative(const AbsPath, BasePath: string): string;
var
  Path: array[0..MAX_PATH-1] of char;
begin
  PathRelativePathTo(@Path[0], PChar(BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar(AbsPath), 0);
  Result := Path;
  if Result.StartsWith('.\') then
    Delete(Result, 1, 2);
end;

class procedure TUtils.GetFilesListRecursive(Folder, Mask: String; slFiles: TStringList);
var
  SearchRec: TSearchRec;
  iRes, i: Integer;
  Folders: TStringList;
begin
  iRes := FindFirst(Folder + '\' + Mask, faAnyFile, SearchRec);
  while iRes = 0 do begin
    slFiles.Add(Folder + '\' + SearchRec.Name);
    iRes := FindNext(SearchRec);
  end;
  FindClose(SearchRec);

  Folders := TStringList.Create;

  iRes := FindFirst(Folder + '\*.*', faDirectory, SearchRec);
  while iRes = 0 do begin
    if ((SearchRec.Attr and faDirectory) <> 0) and (copy(SearchRec.Name, 1, 1) <> '.') then
      Folders.Add(SearchRec.Name);
    iRes := FindNext(SearchRec);
  end;

  for i := 0 to Folders.Count - 1 do
    GetFilesListRecursive(Folder + '\' + Folders[i], Mask, slFiles);

  Folders.Free;
end; // GetFilesListRecursive

const
  CASESENSITIVITY = 0; // system default - case insensitive in Windows and case sensitive in Unix
  WRITEBUFFERSIZE = 8192;

class function TUtils.LoadFileFromZip(Stream: TStream; const ArchiveFileName: String; const FileName: String): Boolean;
var
  uf: unzFile;
//  file_info: unz_file_info;
  buf:    pointer;
  err:    LongInt;
begin
  Result := False;
  if not FileExists(ArchiveFileName) then
    Exit(False);

  uf := unzOpen(PChar(ArchiveFileName));
  if uf = nil then
    Exit(False);
  try
    if unzLocateFile(uf, PChar(FileName.Replace('\', '/')), CASESENSITIVITY) <> UNZ_OK then
      Exit(False);
//    err := unzGetCurrentFileInfo(uf, @file_info, PChar(FileName),SizeOf(FileName), nil, 0, nil, 0);
    buf := allocmem(WRITEBUFFERSIZE);
    err := unzOpenCurrentFile(uf);
    try
      repeat
        err := unzReadCurrentFile(uf, buf, WRITEBUFFERSIZE);
        if err < 0 then
          Exit(False);
        Stream.Write(buf^, err);
      until (err = 0);
    finally
      unzCloseCurrentFile(uf);
      FreeMemAndNil(buf);
    end;
    Result := True;
  finally
    unzClose(uf);
  end;
end; // LoadFileFromZip

class procedure TUtils.LoadFileListFromZip(const ArchiveFileName: String; FilesList: TStringList);
var
  i: integer;
  uf: unzFile;
  gi: unz_global_info {= (number_entry:0; size_comment: 0)};
  filename_inzip: array[0..255] of char;
  file_info: unz_file_info;
begin
  if not FileExists(ArchiveFileName) then
    Exit;

  uf := unzOpen(PChar(ArchiveFileName));
  if uf = nil then
    Exit;

  try
    {$WARN 5057 off : Local variable "$1" does not seem to be initialized}
    if unzGetGlobalInfo(uf, gi) <> UNZ_OK then
      raise Exception.Create('Error with zipfile in LoadFileListFromZip');
    {$WARN 5057 on}

    for i := 0 to gi.number_entry - 1 do begin
      if unzGetCurrentFileInfo(uf, @file_info, filename_inzip, sizeof(filename_inzip), nil, 0, nil, 0) <> UNZ_OK then
        raise Exception.Create('Error reading filelist with zipfile in LoadFileListFromZip');

      if file_info.uncompressed_size <> 0 then // exclude directories
        FilesList.Add(filename_inzip);

      if i < gi.number_entry -1 then
        if unzGoToNextFile(uf) <> UNZ_OK then
          break;
    end;
  finally
    unzClose(uf);
  end;
end; // LoadFileListFromZip

class function TUtils.HexToInt(const Value: String): integer;
var i : Integer;
begin
  Result := 0;
  i := 1;
  if Value = '' then
    exit;
  if Value[1] = '$' then Inc(i);
  while i <= Length(Value) do begin
    if CharInSet(Value[i], ['0'..'9']) then
       Result := (Result shl 4) or (Ord(Value[i]) - Ord('0'))
    else if CharInSet(Value[i], ['A'..'F']) then
       Result := (Result shl 4) or (Ord(Value[i]) - Ord('A') + 10)
    else if CharInSet(Value[i], ['a'..'f']) then
       Result := (Result shl 4) or (Ord(Value[i]) - Ord('a') + 10)
    else
      break;
    Inc(i);
  end;
end; // HexToInt

class function TUtils.StrToColor(S: String): TColor;
begin
  if copy(S, 1, 1) = '-' then
    Result := StrToInt(S)
  else
    Result := RGB(HexToInt(copy(S, 1, 2)), HexToInt(copy(S, 3, 2)), HexToInt(copy(S, 5, 2)));
end; // StrToColor

class function TUtils.ColorToStr(Color: Graphics.TColor): String;

  function IntToHex(Value: DWord): String;
  const HexDigitChr: array[ 0..15 ] of Char = ( '0','1','2','3','4','5','6','7', '8','9','A','B','C','D','E','F' );
  begin
    Result := HexDigitChr[(Value and $F0) shr 4] + HexDigitChr[Value and $F];
  end; // IntToHex

begin // ColorToStr
  if Color = -1 then
    Result := '-1'
  else
    Result := IntToHex(GetRValue(ColorToRGB(Color))) +
              IntToHex(GetGValue(ColorToRGB(Color))) +
              IntToHex(GetBValue(ColorToRGB(Color)));
end; // ColorToStr

class function TUtils.Trim(const S : String): String;
  function TrimLeft(const S: String): String;
  var
    I, L: Integer;
  begin
    L := Length(S);
    I := 1;
    while (I <= L) and (S[I] <= ' ') do Inc(I);
    Result := Copy(S, I, Maxint);
  end;
  function TrimRight(const S: String): String;
  var
    I: Integer;
  begin
    I := Length(S);
    while (I > 0) and (S[I] <= ' ') do Dec(I);
    Result := Copy(S, 1, I);
  end;
begin
   Result := TrimLeft( TrimRight( S ) );
end;

class function TUtils.Parse(var S: String; const Separators: String): String;

  function indexOfCharsMin(const S, Chars: String): integer;
  var i, j : integer;
  begin
    Result := -1;
    for i := 1 to Length(Chars) do begin
      j := Pos(Chars[i], S);
      if j > 0 then
        if (Result < 0) or (j < Result) then
           Result := j;
    end;
  end; // indexOfCharsMin

var Pos: Integer;
begin
  Pos := indexOfCharsMin(S, Separators);
  if Pos <= 0 then
     Pos := Length(S) + 1;
  Result := S;
  S := copy(Result, Pos + 1, MaxInt);
  Result := copy(Result, 1, Pos - 1);
end; // Parse

class function TUtils.ParseInt(var S: String): integer;
begin
  Result := StrToIntDef(Trim(Parse(S, ',')), 0);
end; // ParseInt

class function TUtils.ParseHex(var S: String): integer;
begin
  Result := HexToInt(Trim(Parse(S, ',')));
end; // ParseInt

// putting -Pi <= x <= Pi
class function TUtils.PrivPi(Al: extended): extended;
const delta = 1e-5;
begin
  while Al > Pi do
    Al := Al - 2 * Pi;
  while Al < -Pi do
    Al := Al + 2 * Pi;
  if Al < -Pi + delta then
    Al := Pi;

  Result := Al;
end; // Priv

class procedure TSettings.InitializeDefaults;
begin
  AntiAlias := False;
  Wired := False;
  ShowAxes := False;
  ShowNormals := False;
  ContourMode := False;
  MinimizeToTray := False;
  MouseMode := 0;
  PreScript := '';
  ScanFiles := False;

  InteractiveDisabled := False;
  AppConfigDir := GetAppConfigDir(False);
end; // InitializeDefaults

class procedure TAppStatus.InitializeDefaults;
begin
  PuzzleLoading := False;
end;

{$WARN 5057 off : Local variable "$1" does not seem to be initialized}
class function TAppStatus.GetVersionName: string;
type
  TVerInfo = packed record
    Dummy: array[0..47] of byte; // unnecessary 48 bytes
    Minor, Major, Build, Release: word;
  end;
var
  s: TResourceStream;
  v: TVerInfo;
begin
  Result := '';
  try
    s := TResourceStream.Create(HInstance, '#1', RT_VERSION); // get resource
    try
      if s.Size > 0 then begin
        s.Read(v, SizeOf(v)); // read bytes needed
        Result := IntToStr(v.Major) + '.' + IntToStr(v.Minor) + ' beta. Build ' +
                  {IntToStr(v.Release) + '.' +} IntToStr(v.Build);
      end;
    finally
      s.Free;
    end;
  except;
  end;
end; // GetVersionName
{$WARN 5057 on : Local variable "$1" does not seem to be initialized}

initialization
//  MaskParts := TList.Create;
end.

