{************************************************************}
{                                                            }
{  Unit frLibrary                                            }
{  2023-2023                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit frLibrary;

{$mode ObjFPC}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Grids, StdCtrls, Buttons, Menus, ActnList,
  Types;

type

  { TLibraryFrame }

  TLibraryFrame = class(TFrame)
    alLibrary: TActionList;
    aNext: TAction;
    aPrevious: TAction;
    aRandom: TAction;
    bClearFilter: TSpeedButton;
    eFilter: TEdit;
    Label1: TLabel;
    miCopyName: TMenuItem;
    miCopyPath: TMenuItem;
    miNotepad: TMenuItem;
    miOpeninExplorer: TMenuItem;
    miOpenLink: TMenuItem;
    miOpenXml: TMenuItem;
    pmLibrary: TPopupMenu;
    sbPrevious: TSpeedButton;
    sbNext: TSpeedButton;
    sgLibrary: TStringGrid;
    sbRandom: TSpeedButton;
    procedure aNextExecute(Sender: TObject);
    procedure aPreviousExecute(Sender: TObject);
    procedure aRandomExecute(Sender: TObject);
    procedure bClearFilterClick(Sender: TObject);
    procedure eFilterChange(Sender: TObject);
    procedure miCopyNameClick(Sender: TObject);
    procedure miCopyPathClick(Sender: TObject);
    procedure miNotepadClick(Sender: TObject);
    procedure miOpeninExplorerClick(Sender: TObject);
    procedure miOpenLinkClick(Sender: TObject);
    procedure miOpenXmlClick(Sender: TObject);
    procedure pmLibraryPopup(Sender: TObject);
    procedure sgLibraryDblClick(Sender: TObject);
    procedure sgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgLibraryHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure sgLibraryMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure OnShow;
//    procedure OnClose;
    procedure LoadIni;
    procedure SaveIni;
    procedure RefreshLibrary;
    procedure FocusCurrent;
  end;

implementation
{$R *.lfm}
uses
  uUtils, uLibrary,
  fMain,
  Clipbrd, IniFiles, LCLIntf, Graphics, Dialogs, Windows, Process;

constructor TLibraryFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  sgLibrary.RowCount := 2;
  sgLibrary.ColCount := 10;

  sgLibrary.ColWidths[0] := 30;
  sgLibrary.ColWidths[1] := 200;
  sgLibrary.ColWidths[2] := 100;
  sgLibrary.ColWidths[3] := 250;
  sgLibrary.ColWidths[4] := 550;
  sgLibrary.ColWidths[5] := 150;
  sgLibrary.ColWidths[6] := 150;
  sgLibrary.ColWidths[7] := 100;
  sgLibrary.ColWidths[8] := 50;
  sgLibrary.ColWidths[9] := 50;

  sgLibrary.Cells[ 0, 0] := 'No';
  sgLibrary.Cells[ 1, 0] := 'Name';
  sgLibrary.Cells[ 2, 0] := 'Class';
  sgLibrary.Cells[ 3, 0] := 'Menu';
  sgLibrary.Cells[ 4, 0] := 'File';
  sgLibrary.Cells[ 5, 0] := 'Inventor';
  sgLibrary.Cells[ 6, 0] := 'Programmer';
  sgLibrary.Cells[ 7, 0] := 'Added';
  sgLibrary.Cells[ 8, 0] := 'Links';
  sgLibrary.Cells[ 9, 0] := 'Aliases';

  LoadIni;
  RefreshLibrary;
end;

destructor TLibraryFrame.Destroy;
begin
//  SaveIni; // moved to TMainForm.FormDestroy
  inherited Destroy;
end;

procedure TLibraryFrame.OnShow;
begin
  FocusCurrent;

  eFilter.SelectAll;
  eFilterChange(Self);
end;

// ============================================================================
// Ini
// ============================================================================

procedure TLibraryFrame.LoadIni;
var
  i, W: integer;
  Ini: TIniFile;
  ColWidths: String;
begin //LoadIni
  Ini := TIniFile.Create(TSettings.AppConfigDir + ProgramName + '.ini');
  try
    ColWidths := Ini.ReadString('LibraryFrame', 'ColWidths', '');
    for i := 0 to sgLibrary.ColCount - 1 do begin
      W := StrToIntDef(TUtils.Parse(ColWidths, ';'), 0);
      if W <> 0 then
        sgLibrary.ColWidths[i] := W;
    end;

    TLibrary.Puzzles.Sort(TSortType(Ini.ReadInteger('LibraryFrame', 'SortMode', Ord(SORT_BY_MENU))));
  finally
    Ini.Free;
  end;
end; // LoadIni

procedure TLibraryFrame.SaveIni;
var
  i: integer;
  Ini: TIniFile;
  ColWidths: String;
begin //SaveIni
  Ini := TIniFile.Create(TSettings.AppConfigDir + ProgramName + '.ini');
  try
    ColWidths := '';
    for i := 0 to sgLibrary.ColCount - 1 do
      ColWidths := ColWidths + IntToStr(sgLibrary.ColWidths[i]) + ';';
    Ini.WriteString('LibraryFrame', 'ColWidths', ColWidths);

    Ini.WriteInteger('LibraryFrame', 'SortMode', Ord(TLibrary.Puzzles.SortMode));

  finally
    Ini.Free;
  end;
end; //SaveIni

// ============================================================================
// Popup menu
// ============================================================================

procedure TLibraryFrame.pmLibraryPopup(Sender: TObject);
begin
  miOpenLink.Enabled := TLibrary.Puzzles.Filtered[sgLibrary.Row - 1].Link <> '';
  miOpenXml.Enabled  := TLibrary.Puzzles.Filtered[sgLibrary.Row - 1].FileName <> '';
  miNotepad.Enabled  := TLibrary.Puzzles.Filtered[sgLibrary.Row - 1].FileName <> '';
end;

procedure TLibraryFrame.miCopyNameClick(Sender: TObject);
begin
  Clipboard.AsText := TLibrary.Puzzles.Filtered[sgLibrary.Row - 1].Name;
end;

procedure TLibraryFrame.miCopyPathClick(Sender: TObject);
begin
  Clipboard.AsText := TLibrary.Puzzles.Filtered[sgLibrary.Row - 1].FileName;
end;

procedure TLibraryFrame.miOpenXmlClick(Sender: TObject);
begin
   OpenDocument(TLibrary.Puzzles.Filtered[sgLibrary.Row - 1].FileName);
end;

procedure TLibraryFrame.miNotepadClick(Sender: TObject);
  function GetWindowsDir: string;
  var Path: array[0..MAX_PATH-1] of char;
  begin
   Result := '';
   if GetWindowsDirectory(path, MAX_PATH) > 0 then
     Result := string(path);
  end;
begin
   ExecuteProcess('notepad.exe',[TLibrary.Puzzles.Filtered[sgLibrary.Row - 1].FileName]);
//   OpenDocument(PWideChar(GetWindowsDir + '\notepad.exe')); { *Преобразовано из ShellExecute* }
end;

procedure TLibraryFrame.miOpeninExplorerClick(Sender: TObject);
  procedure OpenLocation(const FileName: string);
  var
    Process: TProcess;
  begin
    Process := TProcess.Create(nil);
    try
      {$IFDEF WINDOWS}
      Process.Executable := 'explorer.exe';
      Process.Parameters.Add('/select,' + FileName.QuotedString('"'));
      {$ELSE}
      //This is for macOS, on Linux it will likely depend on WM.
      Process.Executable := 'open';
      Process.Parameters.Add('-R');
      Process.Parameters.Add(FileName);
      {$ENDIF}
      Process.Execute;
    finally
      Process.Free;
    end;
  end;
begin
   OpenLocation(TLibrary.Puzzles.Filtered[sgLibrary.Row - 1].FileName);
end;

procedure TLibraryFrame.miOpenLinkClick(Sender: TObject);
var S: string;
begin
  S := Trim(TLibrary.Puzzles.Filtered[sgLibrary.Row - 1].Link);
  while S <> '' do
     OpenDocument(PWideChar(WideString(TUtils.Parse(S, ';')))); { *Преобразовано из ShellExecute* }
end;

// ============================================================================
//
// ============================================================================

procedure TLibraryFrame.bClearFilterClick(Sender: TObject);
//var i: integer;
//sl: TStringList;
begin
  eFilter.Text := '';
  eFilter.SetFocus;
//sl := TStringList.Create;
//  for i := 0 to TLibrary.Puzzles.FilteredCount - 1 do
//    sl.Add(TLibrary.Puzzles.Filtered[i].FileName);
//  Clipboard.AsText := sl.Text;
//    if (TLibrary.Puzzles.Filtered[i].FileName <> '') and (not FileExists(TLibrary.Puzzles.Filtered[i].FileName)) then begin
//      ShowMessage(TLibrary.Puzzles.Filtered[i].FileName);
//      exit;
//    end;
//  ShowMessage('Ok');
end;

procedure TLibraryFrame.eFilterChange(Sender: TObject);
begin
  TLibrary.Puzzles.Filter := eFilter.Text;
  RefreshLibrary;
  FocusCurrent;
end;

procedure TLibraryFrame.RefreshLibrary;
var i: Integer;
begin
  sgLibrary.RowCount := Max(2, TLibrary.Puzzles.FilteredCount + 1);
  sgLibrary.FixedRows := 1;
  if TLibrary.Puzzles.FilteredCount > 0 then
    for i := 0 to TLibrary.Puzzles.FilteredCount - 1 do begin
      with TLibrary.Puzzles.Filtered[i] do begin
        sgLibrary.Cells[0, i + 1] := IntToStr(i + 1);
        sgLibrary.Cells[1, i + 1] := Name;
        sgLibrary.Cells[2, i + 1] := PuzzleClass.ClassName;
        sgLibrary.Cells[3, i + 1] := MenuString;
        sgLibrary.Cells[4, i + 1] := FileName;
        sgLibrary.Cells[5, i + 1] := Inventor;
        sgLibrary.Cells[6, i + 1] := Programmer;
        sgLibrary.Cells[7, i + 1] := Added;

        if Link <> '' then begin
          if Pos(';', TLibrary.Puzzles.Filtered[i].Link) > 0 then
            sgLibrary.Cells[8, i + 1] := 'links'
          else
            sgLibrary.Cells[8, i + 1] := 'link';
        end else
          sgLibrary.Cells[8, i + 1] := '';
        sgLibrary.Cells[9, i + 1] := Aliases;
      end;
    end
  else
    for i := 0 to sgLibrary.ColCount - 1 do
      sgLibrary.Cells[i, 1] := '';
end; // RefreshLibrary

procedure TLibraryFrame.sgLibraryDblClick(Sender: TObject);
begin
  if TLibrary.Puzzles.FilteredCount <> 0 then
    MainForm.LoadFromLibraryItem(TLibrary.Puzzles.Filtered[sgLibrary.Row - 1]);

  MainForm.SetCurrentPage(PAGE_3D);
end;

procedure TLibraryFrame.sgLibraryDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  DRect: TRect;
  Y: integer;
  TempColor: Graphics.TColor;
begin
  if (ARow > 0) and (TLibrary.Puzzles.FilteredCount > 0) and (MainForm.CurrentPuzzle <> nil) and
     (TLibrary.Puzzles.Filtered[ARow - 1].PuzzleClass = MainForm.CurrentPuzzle.ClassType) and
     (TLibrary.Puzzles.Filtered[ARow - 1].Name = MainForm.CurrentPuzzle.Header.Name) then begin

    with Sender as TStringGrid, Canvas do begin
      TempColor := Brush.Color;
      Brush.Color := clHighlight;
      DRect := ARect;
      DRect.Left := DRect.Left - 4;
      FillRect(DRect);
      SetBkMode(Handle, TRANSPARENT);
      //SetBkMode(Handle, 1);

      Y := ARect.Top + ((ARect.Bottom - ARect.Top - TextHeight('Wg')) div 2);
      TextOut(ARect.Left + 2, Y, Cells[ACol, ARow]);
      Brush.Color := TempColor;
    end;
  end;
end; // sgLibraryDrawCell

procedure TLibraryFrame.FocusCurrent;
var i: integer;
begin
  i := TLibrary.Puzzles.FindPuzzleInFiltered(MainForm.CurrentPuzzle);

  if i >= 0 then begin
    // особенности StringGrid
    if i > 0 then
      sgLibrary.Row := i
    else if sgLibrary.RowCount > 2 then
      sgLibrary.Row := 2;

    sgLibrary.Row := i + 1;
  end;
end;

procedure TLibraryFrame.sgLibraryHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  if IsColumn and (Index in [1..7]) then begin
    case Index of
      1: TLibrary.Puzzles.Sort(SORT_BY_NAME);
      2: TLibrary.Puzzles.Sort(SORT_BY_CLASS);
      3: TLibrary.Puzzles.Sort(SORT_BY_MENU);
      4: TLibrary.Puzzles.Sort(SORT_BY_FILE);
      5: TLibrary.Puzzles.Sort(SORT_BY_INVERTOR);
      6: TLibrary.Puzzles.Sort(SORT_BY_PROGRAMMER);
      7: TLibrary.Puzzles.Sort(SORT_BY_ADDED);
    end;
    RefreshLibrary;
    FocusCurrent;
  end;
end;

//procedure TLibraryFrame.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//begin
//  if Key = VK_RETURN then
//    if TLibrary.Puzzles.FilteredCount <> 0 then begin
//      MainForm.LoadFromLibraryItem(TLibrary.Puzzles.Filtered[sgLibrary.Row - 1]);
//      close;
//    end;
//  if Key = VK_ESCAPE then
//    close;
//end;
//
//procedure TLibraryFrame.sgLibraryKeyPress(Sender: TObject; var Key: Char);
//begin
//  if Key = Chr(VK_RETURN) then
//    close;
//end;

// makes right mouse button to select row
procedure TLibraryFrame.sgLibraryMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Col, Row: integer;
begin
  sgLibrary.MouseToCell(x, y, Col, Row);
  if Row > 0 then
    sgLibrary.Row := Row;
end;

procedure TLibraryFrame.aRandomExecute(Sender: TObject);
begin
  if TLibrary.Puzzles.FilteredCount > 0 then
    MainForm.LoadFromLibraryItem(TLibrary.Puzzles.Filtered[Random(TLibrary.Puzzles.FilteredCount)]);
end; // aRandomExecute

procedure TLibraryFrame.aNextExecute(Sender: TObject);
var i: integer;
begin
  if TLibrary.Puzzles.FilteredCount = 0 then
    Exit;
  for i := 0 to TLibrary.Puzzles.FilteredCount - 1 do
    if (TLibrary.Puzzles.Filtered[i].PuzzleClass = MainForm.CurrentPuzzle.ClassType) and
        (TLibrary.Puzzles.Filtered[i].Name = MainForm.CurrentPuzzle.Header.Name) then begin
      MainForm.LoadFromLibraryItem(TLibrary.Puzzles.Filtered[(i + 1) mod TLibrary.Puzzles.FilteredCount]);
      Exit;
    end;
  MainForm.LoadFromLibraryItem(TLibrary.Puzzles.Filtered[0]);
end;

procedure TLibraryFrame.aPreviousExecute(Sender: TObject);
var i: integer;
begin
  if TLibrary.Puzzles.FilteredCount = 0 then
    Exit;
  for i := 0 to TLibrary.Puzzles.FilteredCount - 1 do
    if (TLibrary.Puzzles.Filtered[i].PuzzleClass = MainForm.CurrentPuzzle.ClassType) and (TLibrary.Puzzles.Filtered[i].Name = MainForm.CurrentPuzzle.Header.Name) then begin
      MainForm.LoadFromLibraryItem(TLibrary.Puzzles.Filtered[(i - 1 + TLibrary.Puzzles.FilteredCount) mod TLibrary.Puzzles.FilteredCount]);
      Exit;
    end;
  MainForm.LoadFromLibraryItem(TLibrary.Puzzles.Filtered[TLibrary.Puzzles.FilteredCount - 1]);
end;

end.
