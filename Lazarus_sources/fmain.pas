{************************************************************}
{                                                            }
{  Unit fMain                                                }
{  2014-2023                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit fMain;

{ $mode objfpc}{$H+}
{$mode delphi}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Menus, Buttons, ActnList, Generics.Collections,
  frLibrary, frPuzzle3d, frSettings, frLog,
  uPuzzle, uBaseGraphics, uSolvedChecker, uLibrary, uMatrix, uLog,
  Types, DOM;

const
  AutoSavedFileName = 'Saved.cub';

  PAGE_3D       = 0;
  PAGE_SETTINGS = 1;
  PAGE_LIBRARY  = 2;
  PAGE_LOG      = 3;

  CM_CURRENT = 1;
  CM_NEW     = 2;
  CM_ABSENT  = 3;

type
  TGraphicsEngine = (geNone, geBsp, geOpenGl, geZBuffer);

type

  { TMainForm }

  TMainForm = class(TForm)
    dOpen: TOpenDialog;
    dSave: TSaveDialog;
    dSaveCapture: TSaveDialog;
    iLogo: TImage;
    ImageList1: TImageList;
    MainMenu: TMainMenu;
    miCommands: TMenuItem;
    miExit: TMenuItem;
    miMacro: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miSaveCapture: TMenuItem;
    N1: TMenuItem;
    N4: TMenuItem;
    pLibrary: TPage;
    pLog: TPage;
    nMain: TNotebook;
    pmLibrary: TPopupMenu;
    pnlPuzzle3D: TPanel;
    pnlSettings: TPanel;
    pnlLibrary: TPanel;
    pnlLog: TPanel;
    pnlPuzzle3DLamp: TPanel;
    pnlSettingsLamp: TPanel;
    pnlLibraryLamp: TPanel;
    pnlLogLamp: TPanel;
    pSettings: TPage;
    pLeft: TPanel;
    pPuzzle3D: TPage;
    sbMain: TStatusBar;
    tMain: TTimer;
    TrayIcon: TTrayIcon;
    procedure bSplitOrderClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    //procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    //procedure FormKeyPress(Sender: TObject; var Key: char);
    //procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormWindowStateChange(Sender: TObject);
    procedure iLogoClick(Sender: TObject);
    procedure lPuzzle3DClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miMacroClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveCaptureClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure pnlPuzzle3DMouseEnter(Sender: TObject);
    procedure pnlPuzzle3DMouseLeave(Sender: TObject);
    procedure sbMainClick(Sender: TObject);
    procedure tMainTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
//    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
  private
    FVersionName: String;
    FCheckedMenuItems: TList<TMenuItem>;
    FSolvedChecker: TSolvedChecker;
    FGraphicsEngine: TGraphicsEngine;
    frmPuzzle3d: TPuzzle3dFrame;
    frmSettings: TSettingsFrame;
    frmLibrary: TLibraryFrame;
    frmLog: TLogFrame;

    procedure ParseCommandLine;
//    function GetVersionName: string;
    procedure OnMinimize(Sender: TObject);

    procedure ClearMenus;
    procedure AddMenus;
    procedure MenuHandler(Sender: TObject);
    function FindSubMenuByCaption(mi: TMenuItem; Caption: String): TMenuItem;
    procedure SetStateForMenuAndParents(mi: TMenuItem; State: integer);
    function LoadPuzzle(PuzzleClass: TPuzzleClass; const PuzzleName: String; const FileName: String): Boolean;
    procedure CheckMenusState;
    procedure ClearPuzzle;
    procedure UpdateCaption;
    procedure UpdateTimer;
    procedure LoadIni;
    procedure SaveIni;
    procedure SetupGraphicsEngine;
    procedure LoadDefaultPuzzle;
    procedure SavePuzzleToFile(FileName: String);
    function GetStatusBarPanelNo(xCoord: integer): integer;
    procedure UpdateStatusText(const AText: string);
    procedure UpdateStatusSolved;

    procedure OnAfterTurn(AType: TTurningType; AState: TTurningState);
    procedure OnBeforeTurn(AType: TTurningType; AState: TTurningState);
    procedure OnBeforeLoadFromStream(Stream: TStream);
    procedure OnAfterLoadFromStream(Stream: TStream);
    procedure OnBeforeLoadFromXml(XmlNode: TDOMNode);
    procedure OnAfterLoadFromXml(XmlNode: TDOMNode);
    procedure OnTurning(AType: TTurningType; AState: TTurningState);
    procedure OnTransLayers(mat: T4x4Matrix; AxisNo: integer; Layers: array of Boolean);
    procedure OnMacroExecute(Macro: String);
  public
    CurrentPuzzle: TPuzzle;
    GraphicsEngine: TBaseGraphics;
    FLog: TLog;

    procedure SetGraphicsEngine(AGraphicsEngine: TGraphicsEngine);
    function LoadPuzzleFromFile(FileName: String): Boolean;
    procedure LoadFromLibrary(PuzzleClass: TPuzzleClass; PuzzleName, FileName: String);
    procedure UpdateControls;
    procedure ResetMovesCounter;
    procedure Scramble;
    procedure ResetTimer;
    procedure SetWired(Wired: Boolean);
    procedure SetShowAxes(ShowAxes: Boolean);
    procedure SetContourMode(ContourMode: Boolean);
    procedure SetAntiAlias(AntiAlias: Boolean);
    procedure SetShowNormals(ShowNormals: Boolean);
    procedure SetMinimizeToTray(MinimizeToTray: Boolean);
    procedure UpdateLog;
    procedure Update3dPage;
    procedure UpdateSettingsPage;
    procedure OnLogUpdated;

    procedure Log(const LogStr: String);
    procedure LogWarning(const WarnStr: String);
    procedure LoadFromLibraryItem(item: TLibraryItem);
    procedure SetCurrentPage(iPage: integer);
    function UpdateSolvedStatus: boolean;
    procedure UpdateMovesCounter;
    procedure EndTurnWhole;
    procedure EndTurn;
    procedure TurnWhole(x, y, dx, dy, nal: extended);
    procedure TransPuzzle(mat: T4x4Matrix);

    procedure aRandomExecute(Sender: TObject);
    procedure aNextExecute(Sender: TObject);
    procedure aPreviousExecute(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation
{$R *.lfm}
uses
  fLogo, fMacro, {fOrderDialog,}
  uVector, uSelection, uUtils, uPuzzleUtils, uXmlReadWrite,
  uZBufferGraphics, uBspGraphics, uOpenglGraphics, uSplitOrder, uScript,
  IniFiles, Math, Windows, IntfGraphics, LCLIntf, LCLType, FPImage,
  XMLRead;

// TMainForm

procedure TMainForm.FormCreate(Sender: TObject);

  procedure SetLogoText(const Text: string);
  begin
    LogoForm.lStatus.Caption := Text;
    Application.ProcessMessages;
  end; // SetLogoText

var FileName: String;
begin
  frmPuzzle3d := TPuzzle3dFrame.Create(pPuzzle3d);
  frmPuzzle3d.Parent := pPuzzle3d;

  frmSettings := TSettingsFrame.Create(pSettings);
  frmSettings.Parent := pSettings;

  frmLibrary := TLibraryFrame.Create(pLibrary);
  frmLibrary.Parent := pLibrary;

  frmLog := TLogFrame.Create(pLog);
  frmLog.Parent := pLog;

  FLog := TLog.Create;
  FLog.OnLogUpdated := OnLogUpdated;

  SetCurrentPage(PAGE_3D);

  TSettings.InitializeDefaults;
  TAppStatus.InitializeDefaults;

  SetLogoText('starting');
  CurrentPuzzle := nil;

  GraphicsEngine := nil;
  FGraphicsEngine := geNone;


  TSelection.OldSelected := nil;
  FCheckedMenuItems :=  TList<TMenuItem>.Create;

  FormatSettings.DecimalSeparator := '.';
  DoubleBuffered := True;
  ParseCommandLine; // set TSettings.AppConfigDir

  Randomize;

  dOpen.InitialDir := TUtils.GetStartDir;
  dSave.InitialDir := TUtils.GetStartDir;

  // Events
  TUtils.LogProc := Log;
  TUtils.LogWarningProc := LogWarning;
  Application.OnMinimize := OnMinimize;
  DragAcceptFiles(Handle, True);

  // prepare

  FVersionName := TAppStatus.GetVersionName;
  LogoForm.lVersion.Caption := 'v' + FVersionName;

  SetLogoText('loading library');
  if TSettings.ScanFiles then begin
    TLibrary.Puzzles.Load;
    TLibrary.Puzzles.SaveToXmlFile(LIBRARY_PATH);
  end else
    TLibrary.Puzzles.LoadFromCompressedXmlFile(LIBRARY_PATH);

  TLibrary.Puzzles.Filter := ''; // move to LoadIni
  SetLogoText('loading menu');
  AddMenus;
  SetLogoText('loading puzzle');
  LoadIni;

  // Load Puzzle

  if ParamCount > 0 then begin
    FileName := ParamStr(1);
    if LoadPuzzleFromFile(FileName) then
      CurrentPuzzle.Header.FileName := FileName;
  end;

  if CurrentPuzzle = nil then
    LoadPuzzleFromFile(TSettings.AppConfigDir + AutoSavedFileName);

  if CurrentPuzzle = nil then
    LoadDefaultPuzzle;

  SetLogoText('finishing');

  UpdateControls;
  UpdateTimer;
  UpdateMovesCounter;
  UpdateSolvedStatus;


  CheckMenusState;

  SetLogoText('');

//  TSelection.HighlightedParts := TList<TPart>.Create;

//  Puzzles.PrintCollection; // test
  // check duplicate names
//  for i := 0 to TLibrary.Puzzles.Count - 1 do
//    for j := i + 1 to TLibrary.Puzzles.Count - 1 do
//      if TLibrary.Puzzles[i].Name = TLibrary.Puzzles[j].Name then
//        ShowMessage(TLibrary.Puzzles[i].Name);

  //  if (ParamCount > 0) and (ParamStr(1) = '-Blend') then
//    tbBlend.Position := tbBlend.Max - 1;


//  Application.ShowMainForm := True;
//  ShowWindow(Handle,SW_SHOW);
//  ShowWindow(Application.Handle,SW_SHOW);

end;
{$WARN 5024 off : Parameter "$1" not used}
procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveIni;
  frmLibrary.SaveIni; // moved from TFrameLibrary.OnDestroy
  SavePuzzleToFile(TSettings.AppConfigDir + AutoSavedFileName);
//  if FFileNameToSave <> '' then
//    SavePuzzleToFile(FFileNameToSave);
end; // FormClose
{$WARN 5024 on : Parameter "$1" not used}

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  OnResize := nil; // в VCL при наличии MainMenu после Destroy почему-то вызывается OnResize

  ClearPuzzle;

  SetGraphicsEngine(geNone);

//  for i := 0 to TSelection.HighlightedParts.Count - 1 do
//    TSelection.HighlightedParts[i].Free;
//  TSelection.HighlightedParts.Free;

  FCheckedMenuItems.Free;

  FreeAndNil(TLibrary.Puzzles);

  FreeAndNil(FLog);
end; // FormDestroy

procedure TMainForm.ClearPuzzle;
begin
  if FSolvedChecker <> nil then
    FreeAndNil(FSolvedChecker);

  frmPuzzle3d.ClearPuzzle;

  if GraphicsEngine <> nil then
    GraphicsEngine.SetPuzzle(nil);

  if CurrentPuzzle <> nil then
    FreeAndNil(CurrentPuzzle);

  //ClearMenus;
  //UpdateScreen;
end; //ClearPuzzle

// ============================================================================
// Ini
// ============================================================================

procedure TMainForm.ParseCommandLine;
var
  i: integer;
  S: string;
begin
  for i := 1 to System.ParamCount do begin
    if ParamStr(i).ToLower.StartsWith('-appconfigdir') then begin
      S := ParamStr(i);
      TUtils.Parse(S, '=');
      if not S.EndsWith(DirectorySeparator) then
        S := S + DirectorySeparator;
      TSettings.AppConfigDir := S;
    end;
    if ParamStr(i).ToLower.StartsWith('-scanfiles') then begin
      TSettings.ScanFiles := True;
    end;
  end;
end;

procedure TMainForm.LoadIni;
var Ini: TIniFile;
begin //LoadIni
//  ShowMessage(GetAppConfigDir(True));
  Ini := TIniFile.Create(TSettings.AppConfigDir + ProgramName + '.ini');
  try
    SetAntiAlias(False);   // SetAntiAlias(Ini.ReadBool('Settings', 'AntiAlias', False));
    SetWired(False);       // SetWired(Ini.ReadBool('Settings', 'Wired', False));
    SetShowAxes(False);    // SetShowAxes(Ini.ReadBool('Settings', 'ShowAxes', False));
    SetShowNormals(False); // SetShowNormals(Ini.ReadBool('Settings', 'Normals', False));

    TSettings.GraphicsEngineName := Ini.ReadString('Graphics', 'GraphicsEngine', 'BspGraphics');
    SetupGraphicsEngine;
    frmSettings.tbPenWidth.Position := GraphicsEngine.PenWidth;

    Left   := Max(Ini.ReadInteger('Form', 'Left', 0), 0);
    Top    := Max(Ini.ReadInteger('Form', 'Top', 0), 0);
    Width  := Ini.ReadInteger('Form', 'Width', 800);
    Height := Ini.ReadInteger('Form', 'Height', 580);
    SetRestoredBounds(Left, Top, Width, Height);
    if Ini.ReadBool('Form', 'Maximized', True) then
      WindowState := wsMaximized;
    FormWindowStateChange(nil); // Save current window state to TrayIcon

    SetMinimizeToTray(Ini.ReadBool('Form', 'MinimizeToTray', False));

    dSaveCapture.FilterIndex := Ini.ReadInteger('DialogProperties', 'SaveCaptureType', 1);

//    FZAngle := Ini.ReadFloat('Init', 'ZAngle', -30);
//    FXAngle := Ini.ReadFloat('Init', 'XAngle', 20);
  finally
    Ini.Free;
  end;
end; // LoadIni

procedure TMainForm.SaveIni;
var
  Ini: TIniFile;
  Maximized: Boolean;
begin //SaveIni
  Ini := TIniFile.Create(TSettings.AppConfigDir + ProgramName + '.ini');
  try
    Ini.WriteBool('Settings', 'AntiAlias', TSettings.AntiAlias);
//    Ini.WriteBool('Settings', 'Wired',     TSettings.Wired);
//    Ini.WriteBool('Settings', 'Axes',      TSettings.ShowAxes);
//    Ini.WriteBool('Settings', 'Normals',   TSettings.ShowNormals);

    if GraphicsEngine <> nil then
      Ini.WriteInteger('Graphics', 'Zoom', Round(GraphicsEngine.Zoom)); // extended ?
//    Ini.WriteFloat('View', 'Zoom', GraphicsEngine.Zoom);

    Maximized := WindowState = wsMaximized;
    Ini.WriteInteger('Form', 'Left',   IfThen(Maximized, RestoredLeft,   Left  ));
    Ini.WriteInteger('Form', 'Top',    IfThen(Maximized, RestoredTop,    Top   ));
    Ini.WriteInteger('Form', 'Width',  IfThen(Maximized, RestoredWidth,  Width ));
    Ini.WriteInteger('Form', 'Height', IfThen(Maximized, RestoredHeight, Height));
    Ini.WriteBool('Form', 'Maximized', Maximized);

    Ini.WriteBool('Form', 'MinimizeToTray', TSettings.MinimizeToTray);

    Ini.WriteInteger('DialogProperties', 'SaveCaptureType', dSaveCapture.FilterIndex);

  finally
    Ini.Free;
  end;
end; //SaveIni

// ============================================================================
// TrayIcon
// ============================================================================

{$WARN 5044 off : Symbol "$1" is not portable}
procedure TMainForm.OnMinimize(Sender:TObject);
begin // When application is minimized by user and/or by code
  if TSettings.MinimizeToTray then begin
    //Hide; // This is to hide it from taskbar
    TrayIcon.Visible := True;
    TrayIcon.Hint := Caption;

    ShowWindow(Handle, SW_HIDE);  // Hide window
    ShowWindow(Application.Handle,SW_HIDE);  // Hide button from TaskBar
    SetWindowLong(Application.Handle, GWL_EXSTYLE,
    GetWindowLong(Application.Handle, GWL_EXSTYLE) or (not WS_EX_APPWINDOW));
  end;
end;
{$WARN 5044 on : Symbol "$1" is not portable}

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
//  TrayIcon.ShowBalloonHint;
  ShowWindow(Handle, IfThen(TrayIcon.Tag = 1, SW_MAXIMIZE, SW_RESTORE));
  SetForegroundWindow(Handle);
  if TrayIcon.Tag = 1 then
    ShowWindow(Handle, SW_MAXIMIZE);
  TrayIcon.Visible := False;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState in [wsMaximized, wsFullScreen] then
    TrayIcon.Tag := 1;
  if WindowState = wsNormal then
    TrayIcon.Tag := 0;
end;

// ============================================================================
// Form/app events
// ============================================================================

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
var FileName: string;
begin
  for FileName in FileNames do
    if LoadPuzzleFromFile(FileName) then begin
      CurrentPuzzle.Header.FileName := FileName;
      UpdateControls;
      exit;
    end;
end; // FormDropFiles

//procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
//  Shift: TShiftState);
//begin
//  //  Log('Down  ' + IntToStr(Key));
//end;
//
//procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
//begin
//  //  Log('Press ' + Key);
//end;
//
//procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
//begin
//  //  Log('Up    ' + IntToStr(Key));
//
//end;

// ============================================================================
//
// ============================================================================

//// change color of TTabSheet background
//procedure TTabSheet.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
//begin
//  Brush.Color := clBtnFace;
//  Windows.FillRect(Msg.dc, ClientRect, Brush.Handle);
//end;
//
// eliminate border of TPageControl
//procedure TPageControl.TCMAdjustRect(var Msg: TMessage);
//begin
//  inherited;
//  {$WARNINGS OFF}
//  if Msg.WParam = 0 then
//    InflateRect(PRect(Msg.LParam)^, 4, 4)
//  else
//    InflateRect(PRect(Msg.LParam)^, -4, -4);
//  {$WARNINGS ON}
//end;

// ============================================================================
// Load
// ============================================================================

procedure TMainForm.miOpenClick(Sender: TObject);
//var sl: TStringList;
begin
{  sl := TStringList.Create;
  TUtils.LoadFileListFromZip('Puzzles.zip', sl);
  ShowMessage(sl.Text);
  sl.Free;}

  if not dOpen.Execute then
    Exit;

  if LoadPuzzleFromFile(dOpen.FileName) then begin
    CurrentPuzzle.Header.FileName := dOpen.FileName;
    UpdateControls;
  end;
end;

function TMainForm.LoadPuzzle(PuzzleClass: TPuzzleClass; const PuzzleName: String; const FileName: String): Boolean;
var Ini: TIniFile;
    ZAngle, XAngle: extended;
    ext: String;
begin
  Result := False;
  if TAppStatus.PuzzleLoading then
    exit;
//  if not FileExists(FileName) then
//    PuzzleClass := TRCube;
  frmPuzzle3d.GraphicsUpdateEnabled := False;
  TSettings.InteractiveDisabled := True;
  try
    ClearPuzzle;

    CurrentPuzzle := PuzzleClass.Create;
    GraphicsEngine.SetPuzzle(CurrentPuzzle);
  //  CurrentPuzzle.On := OnBeforeTurn;
  //  CurrentPuzzle.On := OnBeforeTurn;
  //  CurrentPuzzle.On := OnBeforeTurn;
  //  CurrentPuzzle.On := OnBeforeTurn;
    CurrentPuzzle.OnBeforeTurn := OnBeforeTurn;
    CurrentPuzzle.OnTransLayers := OnTransLayers;
    CurrentPuzzle.OnTurning := OnTurning;
    CurrentPuzzle.OnAfterTurn := OnAfterTurn;
    FSolvedChecker := TSolvedChecker.Create(CurrentPuzzle);
    frmPuzzle3d.LoadPuzzle;

    if FileName <> '' then begin
  //    CurrentPuzzle.InteractiveDisabled := True; // при нажатии на reset перестаёт запрашивать размер
  //    try
  //    finally
  //      CurrentPuzzle.InteractiveDisabled := False;
  //    end;

      try
        UpdateStatusText('Loading ' + PuzzleName);
        TAppStatus.PuzzleLoading := True;
        Result := TLibrary.PuzzleLoadFromFile(CurrentPuzzle, FileName);
        TAppStatus.PuzzleLoading := False;
        UpdateStatusText('');
      except
        on E: Exception do begin
          TAppStatus.PuzzleLoading := False;
          UpdateStatusText('Failed loading, ' + E.Message);
          ClearPuzzle;
          CurrentPuzzle := PuzzleClass.Create;
          CurrentPuzzle.Header.Name := PuzzleName;
          CurrentPuzzle.Header.FileName := FileName;
          GraphicsEngine.PaintScene(CurrentPuzzle);
          frmPuzzle3d.pb3d.Invalidate;
          raise Exception.Create('Failed to load a puzzle: ' + E.Message);
        end;
      end;

      ext := LowerCase(ExtractFileExt(FileName));
      if ext = '.xml' then begin
        Ini := TIniFile.Create(TSettings.AppConfigDir + ProgramName + '.ini');
        try
          ZAngle := Ini.ReadFloat('Init', 'ZAngle', -30);
          XAngle := Ini.ReadFloat('Init', 'XAngle', 20);
        finally
          Ini.Free;
        end;
        TransPuzzle(TMatrix.GetRotateMatrix(Vector(0, 0, 1), DegToRad(ZAngle)));
        TransPuzzle(TMatrix.GetRotateMatrix(Vector(1, 0, 0), DegToRad(XAngle)));
      end;

    end;

    if CurrentPuzzle.Header.Name = '' then
      CurrentPuzzle.Header.Name := PuzzleName;

    CurrentPuzzle.AfterLoadEvent;
    GraphicsEngine.OnAfterPuzzleLoad;

    Log('Puzzle loaded. Parts: ' + IntToStr(CurrentPuzzle.Parts.Count) + '. Colored faces: ' + IntToStr(TPuzzleUtils.GetColoredFacesCount(CurrentPuzzle)) + '. Axes: ' + IntToStr(CurrentPuzzle.Axes.Count) + '.');

  finally
    frmPuzzle3d.GraphicsUpdateEnabled := True;
    TSettings.InteractiveDisabled := False;
  end;

  miMacro.Enabled := CurrentPuzzle.Macros.Count > 0;
  frmPuzzle3d.tbShowFrom.Max := 0;
  frmPuzzle3d.tbShowTo.Max := 0;
  UpdateControls;
end; // LoadPuzzle

procedure TMainForm.LoadFromLibraryItem(item: TLibraryItem);
begin
  if Item <> nil then
    LoadFromLibrary(Item.PuzzleClass, Item.Name, Item.FileName);
end;

procedure TMainForm.LoadFromLibrary(PuzzleClass: TPuzzleClass; PuzzleName, FileName: String);
var
  i: integer;
//  Name: String;
  Found : Boolean;
begin
  if TAppStatus.PuzzleLoading then
    exit;

  TSelection.Clear;

//  FileName := '';
  Found := False;

  if FileName <> '' then begin
    for i := 0 to TLibrary.Puzzles.Count - 1 do
      if (TLibrary.Puzzles[i].PuzzleClass = PuzzleClass) and (TLibrary.Puzzles[i].Name = PuzzleName) and (TLibrary.Puzzles[i].FileName = FileName) then begin
        FileName := TLibrary.Puzzles[i].FileName;
//        Name := TLibrary.Puzzles[i].Name;
        Found := True;
        break;
      end;

    if not Found then
      for i := 0 to TLibrary.Puzzles.Count - 1 do
        if TLibrary.Puzzles[i].FileName = FileName then begin
          PuzzleClass := TLibrary.Puzzles[i].PuzzleClass;
          FileName := TLibrary.Puzzles[i].FileName;
//          Name := TLibrary.Puzzles[i].Name;
          break;
        end;
  end;

  if not Found then
    for i := 0 to TLibrary.Puzzles.Count - 1 do
      if (TLibrary.Puzzles[i].PuzzleClass = PuzzleClass) and (TLibrary.Puzzles[i].Name = PuzzleName) then begin
        FileName := TLibrary.Puzzles[i].FileName;
//        Name := TLibrary.Puzzles[i].Name;
        break;
      end;

  LoadPuzzle(PuzzleClass, PuzzleName, FileName);

//  if Name <> '' then
//    CurrentPuzzle.Name := Name;
  if PuzzleName <> '' then
    CurrentPuzzle.Header.Name := PuzzleName;

  GraphicsEngine.OnAfterPuzzleLoad;

  UpdateControls;

  CheckMenusState;

  ResetMovesCounter;
  ResetTimer;
end; // LoadFromLibrary

procedure TMainForm.LoadDefaultPuzzle;
const DefaultName = 'Rubik''s cube 3x3x3';
begin
  LoadFromLibrary(TPuzzle, DefaultName, '');
end;

function TMainForm.LoadPuzzleFromFile(FileName: String): Boolean;
var PuzzleClass: TPuzzleClass;
    Puzzle: TPuzzle;
    {Name,} OldPuzzleName: String;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;

  OldPuzzleName := '';
  if CurrentPuzzle <> nil then
    OldPuzzleName := CurrentPuzzle.Header.Name; // for Cuboid's reset

  Puzzle := TPuzzle.Create;
  try
    if not TLibrary.PuzzleLoadHeaderFromFile(Puzzle, FileName) then
      Exit;
    PuzzleClass := TLibrary.GetPuzzleClassByName(Puzzle.Header.ClassString);
//    Name := Puzzle.Header.Name;
  finally
    Puzzle.Free;
  end;

  tMain.Enabled := False;

  Result := LoadPuzzle(PuzzleClass, OldPuzzleName, FileName);
end; // LoadPuzzleFromFile

//procedure TMainForm.tsNextPage(Sender: TObject; Button: TMouseButton;  (TabSheet.OnMouseUp)
//  ButtonsState: TShiftState; X, Y: Integer);
//begin
//  if (GetKeyState(VK_SHIFT) < 0) or (pcRightPanel.ActivePageIndex > 0) then begin
//    pcRightPanel.ActivePageIndex := (pcRightPanel.ActivePageIndex + 1) mod pcRightPanel.PageCount;
////    GraphicsEngine.SetPenWidth(1);
//    if pcRightPanel.ActivePage = tsColorMode then
//      GraphicsEngine.SetPenWidth(tbPenWidth.Position);
//    pb3d.Invalidate;
//    UpdateLog;
//  end;
//end;

// ============================================================================
// Save
// ============================================================================

procedure TMainForm.miSaveClick(Sender: TObject);
begin
  if CurrentPuzzle.Header.FileName = '' then
    miSaveAsClick(nil)
  else
    SavePuzzleToFile(CurrentPuzzle.Header.FileName);
  UpdateControls;
end;

procedure TMainForm.miSaveAsClick(Sender: TObject);
begin
  if not dSave.Execute then
    Exit;

  SavePuzzleToFile(dSave.FileName);

  CurrentPuzzle.Header.FileName := dSave.FileName;
end; // miSaveAsClick

procedure TMainForm.SavePuzzleToFile(FileName: String);
var ext: String;
begin
  if CurrentPuzzle = nil then
    exit;
  ext := LowerCase(ExtractFileExt(FileName));
  if Ext = '.cub' then
    TLibrary.PuzzleSaveToStreamFile(CurrentPuzzle, FileName)
  else if Ext = '.xml' then
    TXmlReadWrite.PuzzleSaveToXmlFile(CurrentPuzzle, FileName)
  else if Ext = '.lib' then
    TLibrary.Puzzles.SaveAllPuzzlesToStream(FileName);
end; // SavePuzzleToFile

procedure TMainForm.miSaveCaptureClick(Sender: TObject);

  procedure CreateAnimatedGif;
  var
    gif: TGIFImage;
//    frame: TGifFrame;
    i: integer;
  const N = 10;
  begin
    gif := TGIFImage.Create;
//    frame := gif.Add(GraphicsEngine.BitMap);
    //Setting the delay for each frame
//    TGIFGraphicControlExtension.Create(frame).Delay := 150;

    while CurrentPuzzle.RedoStack.Count > 0 do begin
      for i := 0 to N - 2 do begin
        CurrentPuzzle.PartialRedo(1 / N);

        with CurrentPuzzle.RedoStack do
          GraphicsEngine.OnBeforePuzzleTurn(Last.AxisNo); // не понимаю, почему без этого сбивается gif.

        GraphicsEngine.PaintScene(CurrentPuzzle);
//        frame := gif.Add(GraphicsEngine.BitMap);
//        TGIFGraphicControlExtension.Create(frame).Delay := 40 div N;
      end;
      CurrentPuzzle.PartialRedo(-1 / N * (N - 1));
      CurrentPuzzle.Redo(True);
      GraphicsEngine.PaintScene(CurrentPuzzle);
      UpdateMovesCounter;
//      frame := gif.Add(GraphicsEngine.BitMap);
//      if CurrentPuzzle.RedoStack.Count <> 0 then
//        TGIFGraphicControlExtension.Create(frame).Delay := 30
//      else
//        TGIFGraphicControlExtension.Create(frame).Delay := 200;
//      gif.Pack;
    end;

    //Adding loop extension in the first frame (0 = forever)
//    TGIFAppExtNSLoop.Create(Gif.Images.Frames[0]).Loops := 0;

    //Удаляем лишние цвета из палитры
    //gif.OptimizeColorMap; // после этой команды, gif неправильно отображается в Firefox
//    gif.Pack;
    gif.SaveToFile(dSaveCapture.FileName);
    FreeAndNil(gif);
  end; // CreateAnimatedGif

var
  i: integer;
  ext: String;
begin
  if not dSaveCapture.Execute then
    Exit;

  if GetKeyState(VK_SHIFT) >= 0 then begin // if Shift is not pressed
    ext := LowerCase(ExtractFileExt(dSaveCapture.FileName));
    if (ext <> '.gif') or (CurrentPuzzle.RedoStack.Count = 0) then
      GraphicsEngine.SaveBitmapAs(dSaveCapture.FileName)
    else
      CreateAnimatedGif;
  end else begin
    Calc.DisableInteractiveFunctions;
    try
      for i := 0 to TLibrary.Puzzles.FilteredCount - 1 do begin
        LoadFromLibraryItem(TLibrary.Puzzles.Filtered[i]);
        Application.ProcessMessages;
        GraphicsEngine.SaveBitmapAs(ExtractFileDir(dSaveCapture.FileName) + '\' + IntToStr(i) + ' - ' + ChangeFileExt(ExtractFileName(CurrentPuzzle.Header.FileName),'') + '.bmp');
      end;
    finally
      Calc.EnableInteractiveFunctions;
    end;
  end;
end; // miSaveCaptureClick

// ============================================================================
// Status bar
// ============================================================================

function TMainForm.GetStatusBarPanelNo(xCoord: integer): integer;
var
  x, i: integer;
begin
  Result := -1;
  if (sbMain.SimplePanel) or (sbMain.Panels.Count = 0) then
    exit;
  x := 0;
  for i := 0 to sbMain.Panels.Count - 1 do begin
    x := x + sbMain.Panels[i].Width;
    if xCoord < x then
      Exit(i);
  end;
end; // GetStatusBarPanelNo

procedure TMainForm.sbMainClick(Sender: TObject);
var
  mpt: TPoint;
begin
  mpt := sbMain.ScreenToClient(Mouse.CursorPos) ;
  case GetStatusBarPanelNo(mpt.X) of
    0: ResetTimer;
    1: ResetMovesCounter;
  end;
end; // sbMainClick

procedure TMainForm.UpdateStatusText(const AText: string);
begin
  sbMain.Panels[2].Text := 'Status: ' + AText;
end; // UpdateStatusText

procedure TMainForm.UpdateStatusSolved;
begin
  if CurrentPuzzle = nil then
    UpdateStatusText('puzzle not loaded')
  else if not FSolvedChecker.isSolved then
    UpdateStatusText('unsolved')
  else
    UpdateStatusText('Solved!');
end; // UpdateSolvedStatus

// ============================================================================
// buttons
// ============================================================================

procedure TMainForm.pnlPuzzle3DMouseEnter(Sender: TObject);
begin
  TPanel(Sender).Color := clScrollBar;
//  TPanel(Sender).BevelOuter := bvRaised;
//  TPanel(Sender).Font.Color := clWhite;
end;

procedure TMainForm.pnlPuzzle3DMouseLeave(Sender: TObject);
begin
  TPanel(Sender).Color := clDefault;
//  TPanel(Sender).BevelOuter := bvNone;
//  TPanel(Sender).Font.Color := clBlack;
end;

procedure TMainForm.lPuzzle3DClick(Sender: TObject);
var
  Button: TControl;
begin
  Button := TControl(Sender);
  SetCurrentPage(Button.Tag);
end;

procedure TMainForm.iLogoClick(Sender: TObject);
begin
  LogoForm := TLogoForm.Create(Application);
  try
    with LogoForm do begin
      lStatus.Caption := '';
      lVersion.Caption := 'v' + FVersionName;
      ShowModal;
      Update;
    end;
  finally
    LogoForm.Free;
  end;
end;

procedure TMainForm.bSplitOrderClick(Sender: TObject);
var so: TSplitOrder;
begin
  so := TSplitOrder.Create;
  try
    so.CalcSplitOrder(CurrentPuzzle);
  //  so.CalcCutPartsOrder(CurrentPuzzle);
  finally
    so.Free;
  end;
end; // bSplitOrderClick

// ============================================================================
// menu
// ============================================================================

procedure TMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miMacroClick(Sender: TObject);
begin
  if MacroForm = nil then begin
    Application.CreateForm(TMacroForm, MacroForm);
    MacroForm.OnMacroExecute := OnMacroExecute;
  end;

  MacroForm.SetMacros(CurrentPuzzle.Macros);
  MacroForm.RefreshMacros;
  MacroForm.Show;
end; // miMacroClick

// ============================================================================
// Setters
// ============================================================================

procedure TMainForm.SetCurrentPage(iPage: integer);
var i: integer;
begin
  nMain.PageIndex := iPage;
  for i := 0 to pLeft.ControlCount - 1 do begin
//    pLeft.Controls[i].Font.Style := [];
    pLeft.Controls[i].Color := clDefault;
    if pLeft.Controls[i].Tag = iPage then begin
      if Pos('Lamp', pLeft.Controls[i].Name) > 0 then
        pLeft.Controls[i].Color := clHighlight
      {else if Pos('pnl', pLeft.Controls[i].Name) > 0 then
        pLeft.Controls[i].Font.Style := [fsBold]};
    end;
  end;
  if nMain.PageIndex = PAGE_3D then
    Update3dPage;
  if nMain.PageIndex = PAGE_SETTINGS then
    UpdateSettingsPage;
  if nMain.PageIndex = PAGE_LIBRARY then
    frmLibrary.OnShow;
  if nMain.PageIndex = PAGE_LOG then
    UpdateLog;
end; // SetCurrentPage

procedure TMainForm.SetAntiAlias(AntiAlias: Boolean);
begin
  TSettings.AntiAlias := AntiAlias;
  frmPuzzle3d.miAntiAlias.Checked := AntiAlias;
  frmSettings.cbAntiAlias.Checked := AntiAlias;
  frmPuzzle3d.pb3d.Invalidate;
end; // SetAntiAlias

procedure TMainForm.SetWired(Wired: Boolean);
begin
  TSettings.Wired := Wired;
  frmPuzzle3d.miWired.Checked := Wired;
  frmPuzzle3d.cbWired.Checked := Wired;
  frmSettings.cbWired.Checked := Wired;
  frmPuzzle3d.pb3d.Invalidate;
end; // SetWired

procedure TMainForm.SetShowAxes(ShowAxes: Boolean);
begin
  TSettings.ShowAxes := ShowAxes;
  frmPuzzle3d.miShowAxes.Checked := ShowAxes;
  frmPuzzle3d.cbShowAxes.Checked := ShowAxes;
  frmSettings.cbShowAxes.Checked := ShowAxes;
  frmPuzzle3d.pb3d.Invalidate;
end; // SetShowAxes

procedure TMainForm.SetShowNormals(ShowNormals: Boolean);
begin
  TSettings.ShowNormals := ShowNormals;
  frmPuzzle3d.miShowNormals.Checked := ShowNormals;
  frmSettings.cbShowNormals.Checked := ShowNormals;
  frmPuzzle3d.pb3d.Invalidate;
end; // SetShowNormals

procedure TMainForm.SetContourMode(ContourMode: Boolean);
begin
  TSettings.ContourMode := ContourMode;
  frmPuzzle3d.miContourMode.Checked := ContourMode;
  frmSettings.cbContourMode.Checked := ContourMode;
  frmPuzzle3d.pb3d.Invalidate;
end; // SetContourMode

procedure TMainForm.SetMinimizeToTray(MinimizeToTray: Boolean);
begin
  TSettings.MinimizeToTray := MinimizeToTray;
  frmSettings.cbMinimizeToTray.Checked := MinimizeToTray;
end;

// ============================================================================
// Graphics engine
// ============================================================================

procedure TMainForm.SetupGraphicsEngine;
begin
  if SameText(TSettings.GraphicsEngineName, 'OpenGl') then
    SetGraphicsEngine(geOpenGl)
  else if SameText(TSettings.GraphicsEngineName, 'ZBuffer') then
    SetGraphicsEngine(geZBuffer)
  else
    SetGraphicsEngine(geBsp);

  GraphicsEngine.LoadIni;
end; // SetupGraphicsEngine

procedure TMainForm.SetGraphicsEngine(AGraphicsEngine: TGraphicsEngine);
begin
  if AGraphicsEngine = FGraphicsEngine then
    exit;

//    SaveIni; // for saving Zoom

  if GraphicsEngine <> nil then begin
    GraphicsEngine.SaveIni;
    FreeAndNil(GraphicsEngine);
  end;

  FGraphicsEngine := AGraphicsEngine;

  case FGraphicsEngine of
    geBsp:     GraphicsEngine := TBspGraphics.Create;
    geOpenGl:  GraphicsEngine := TOpenGlGraphics.Create;
    geZBuffer: GraphicsEngine := TZBufferGraphics.Create;
    else exit;
  end;

  GraphicsEngine.LoadIni;

  if CurrentPuzzle <> nil then
    GraphicsEngine.SetPuzzle(CurrentPuzzle);
//  GraphicsEngine.Recalc;

  GraphicsEngine.SetExplode(frmPuzzle3d.tbExplode.Position);
  frmPuzzle3d.tbShowToChange(nil);
  frmPuzzle3d.pb3dResize(nil);
  frmPuzzle3d.pb3d.Invalidate;
end; // SetGraphicsEngine

// ============================================================================
// Mouse events
// ============================================================================

{$WARN 5024 off : Parameter "$1" not used}
procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (WheelDelta <> 0) and PtInRect(frmPuzzle3d.pb3d.ClientRect, frmPuzzle3d.pb3d.ScreenToClient(ClientToScreen(MousePos))) then begin
    if WheelDelta > 0 then
      GraphicsEngine.Zoom := GraphicsEngine.Zoom * 1.1
    else
      GraphicsEngine.Zoom := GraphicsEngine.Zoom / 1.1;

    frmPuzzle3d.pb3d.Invalidate;
    Handled := True;
  end;

  if (nMain.PageIndex = PAGE_LOG) and (WheelDelta <> 0) and PtInRect(frmLog.mLog.ClientRect, frmLog.mLog.ScreenToClient(MousePos)) then begin
    if WheelDelta > 0 then
      frmLog.mLog.ScrollBy(0, 1)
    else
      frmLog.mLog.ScrollBy(0,-1);
  end;
end; // FormMouseWheel

{$WARN 5024 on : Parameter "$1" not used}

// ============================================================================
// Turn
// ============================================================================

procedure TMainForm.EndTurn;
//const delta2 = 1e-5;
var ResultingAngle: extended;
begin
  if CurrentPuzzle.TurningState = tsTurningAnimated then
    Exit;
  if not CurrentPuzzle.IsTurning then
    Exit;
  ResultingAngle := CurrentPuzzle.GetTurningAngle(TSelection.TurnAxisNo, TSelection.TurnLayers, TSelection.TurningAngle, TSelection.AngleMin, TSelection.AngleMax);

  // end animation
  CurrentPuzzle.TurnWithAnimation(TUtils.PrivPi(ResultingAngle - TSelection.TurningAngle));

//  CurrentPuzzle.FPositionHolder.Invalidate;

//  if Abs(ResultingAngle) > delta2 then begin
//    if CurrentPuzzle.UndoRecordEnabled then
//      CurrentPuzzle.AddUndo(FTurnAxisNo, FTurnLayerFrom, FTurnLayerTo, ResultingAngle);
//    CurrentPuzzle.ClearRedoStack;
//  end;
//  CurrentPuzzle.AddAngles(TSelection.TurnAxisNo, TSelection.TurnLayerFrom, TSelection.TurnLayerTo, ResultingAngle);
  TSelection.TurningAngle := ResultingAngle;
  CurrentPuzzle.EndTurn;

  TSelection.TurningAngle := 0;
  CurrentPuzzle.TurningState := tsFree;
  UpdateSolvedStatus;

//  if Assigned(CurrentPuzzle.OnAfterTurn) then
//    CurrentPuzzle.OnAfterTurn(ttManual, tsTurningAnimated);
//  CurrentPuzzle.AfterTurnEvent(ttManual);
end; // EndTurn

{$WARN 5024 off : Parameter "$1" not used}
procedure TMainForm.TurnWhole(x, y, dx, dy, nal: extended);
var mat: T4x4Matrix;
begin
  if not (CurrentPuzzle.TurningState in [tsFree, tsWholeRotating]) then
    Exit;
//  if CurrentPuzzle.TurningState <> tsWholeRotating then begin
//    if nal < 6 / 90 then
//      Exit;
//    if TSelection.SelectedPart = nil then
//      Exit;
//
//    TSelection.TurnAxisNo := CurrentPuzzle.GetRotateAxis(x, y, dx, dy);
//    if TSelection.TurnAxisNo <> -1 then
//      TSelection.TurnAxis := CurrentPuzzle.Axes[TSelection.TurnAxisNo]
//    else begin
//      TSelection.TurnAxis := nil;
//      Exit;
//    end;
//    TSelection.RotateDirection := GetRotateDirection(dx, dy);
//
//    TSelection.TurningAngle := 0;
//    CurrentPuzzle.TurningState := tsWholeRotating;
//  end;
//MainForm.Log(FloatToStr(nal*180/PI));
//  nal := nal * TSelection.RotateDirection;

  mat := CurrentPuzzle.Axes[TSelection.TurnAxisNo].GetRotateMatrix(nal - TSelection.TurningAngle);
  TransPuzzle(mat);

//  if Assigned(FOnTransLayers) then
//    FOnTransLayers(mat, -1, 0, 1000); // todo

  TSelection.TurningAngle := nal;
//MainForm.Log(FloatToStr(TurningAngle*180/PI));
end; // TurnWhole
{$WARN 5024 on : Parameter "$1" not used}

procedure TMainForm.EndTurnWhole;
begin
  if CurrentPuzzle.TurningState <> tsWholeRotating then
    Exit;
  TSelection.TurningAngle := 0;
  CurrentPuzzle.TurningState := tsFree;

//  if Assigned(FOnAfterTurn) then
//    FOnAfterTurn(ttManual, tsTurningAnimated);
//  AfterTurnEvent(ttManual);
end; // EndTurnWhole

procedure TMainForm.TransPuzzle(mat: T4x4Matrix);
begin
  CurrentPuzzle.Trans(mat);
  if frmPuzzle3d.GraphicsUpdateEnabled then
    GraphicsEngine.OnPuzzleTrans(mat);
end;

// ============================================================================
// Menu reorganization
// ============================================================================

procedure TMainForm.ClearMenus;
begin
  while MainMenu.Items.Count > 1 do
    MainMenu.Items.Delete(1);
end; //ClearMenus

function TMainForm.FindSubMenuByCaption(mi: TMenuItem; Caption: String): TMenuItem;
var i: integer;
begin
  for i := 0 to mi.Count - 1 do
    if SameText(StripHotKey(mi.Items[i].Caption), Caption) then
      Exit(mi.Items[i]);
  Result := nil;
end; // FindSubMenuByCaption

procedure TMainForm.SetStateForMenuAndParents(mi: TMenuItem; State: integer);
var i: integer;
begin
  // отмечаем как checked всех пра-родителей
  while mi <> nil do begin
    if (State = CM_ABSENT) or (mi.ImageIndex = CM_ABSENT) then
      mi.ImageIndex := CM_ABSENT
    else
      mi.ImageIndex := (mi.ImageIndex + 1) or State - 1;

    if mi.ImageIndex = -1 then begin
      i := FCheckedMenuItems.IndexOf(mi);
      if i <> -1 then
        FCheckedMenuItems.Delete(i);
    end else begin
      i := FCheckedMenuItems.IndexOf(mi);
      if i = -1 then
        FCheckedMenuItems.Add(mi);
    end;

    mi := mi.Parent;
  end;
end; // SetStateForMenuAndParents

// Lazarus code - sets flag MF_MENUBREAK to break menu into several columns
function myChangeMenuFlag(const AMenuItem: TMenuItem; Flag: Cardinal; Value: boolean): boolean;
var MenuInfo: MENUITEMINFO; // TMenuItemInfoA and TMenuItemInfoW have same size and same structure type
begin
  {$Push}
  {$WARN 5057 off} // Hint: Local variable "$1" does not seem to be initialized
  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  {$Pop}
  MenuInfo.cbSize := sizeof(TMenuItemInfo);
  MenuInfo.fMask := MIIM_FTYPE;         // don't retrieve caption (MIIM_STRING not included)
  GetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);
  if Value then
    MenuInfo.fType := MenuInfo.fType or Flag
  else
    MenuInfo.fType := MenuInfo.fType and (not Flag);
  Result := SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);
  //TriggerFormUpdate(AMenuItem);
end;

procedure TMainForm.AddMenus;
var
  menuItemHeight: integer;
  itemsPerColumn: integer;

  procedure AddSubMenu(mi: TMenuItem; XmlNode: TDomNode);
  var
    SubNode, TextNode: TDomNode;
    sm: TMenuItem;
    i: integer;
  begin
    for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
      SubNode := XmlNode.ChildNodes[i];
      if SameText(SubNode.NodeName, 'Menu') then begin
        TextNode := SubNode.FindNode('#text');
        if Assigned(TextNode) then begin
          mi.Enabled := True;

          sm := TMenuItem.Create(Self);
          sm.Caption := Trim(TextNode.NodeValue);
          sm.Enabled := False;

          // Delphi code
          //if (mi.Count > 0) and (mi.Count mod itemsPerColumn = 0) then
          //  sm.break := mbBarBreak;

          mi.Add(sm);
          AddSubMenu(sm, SubNode);

          // Lazarus code
          if (mi.Count > 0) and (mi.Count mod itemsPerColumn = 0) then
            myChangeMenuFlag(sm, MF_MENUBREAK, true);

        end;
      end;
    end;
  end; // AddSubMenu

  procedure SetupTerminalMenu(mi: TMenuItem; Puzzle: TLibraryItem);
  begin
    mi.OnClick := MenuHandler;
    mi.Tag := Puzzle.Id;
    mi.Enabled := True;
{     // draw menu's picture
    if FCity.InfoFiles[i].MenuImage <> '' then begin
      Image := FCity.FindImage(FCity.InfoFiles[i].MenuImage);
      if Image <> nil then begin
        BitMap := TBitMap.Create;
        BitMap.Width := GetSystemMetrics(SM_CXMENUCHECK);
        BitMap.Height := GetSystemMetrics(SM_CYMENUCHECK);
        BitMap.PixelFormat := pf24bit;
        Scale := TScale.Create;
        Scale.ScaleType := stScreen;
        Scale.Zoom(Min((BitMap.Width ) / Image.Width, (BitMap.Height ) / Image.Height));
        Scale.CenterX := -(BitMap.Width - 1 - Image.Width * Scale.ScaleX);
        Scale.CenterY := -(BitMap.Height - 1 - Image.Height * Scale.ScaleY);
        Image.Draw(BitMap, 0, 0, Bitmap.Canvas.ClipRect, FSmooth, Scale);
        miNew.Bitmap.Assign(BitMap);
        Scale.Free;
        BitMap.Free;
      end;
    end;}
  end; // SetupTerminalMenu

  function MakePictureForDisabledMenus(mi: TMenuItem): Boolean;
  var SubMenu: TMenuItem;
  begin
    Result := mi.Enabled or (mi.Caption = '-');

    if not Result then begin
      mi.ImageIndex := CM_ABSENT;
      Exit;
    end;

    for SubMenu in mi do
      if not MakePictureForDisabledMenus(SubMenu) then
        Result := False;
    if not Result then
      mi.ImageIndex := CM_ABSENT;
  end; // MakePictureForDisabledMenus

var
  S, MenuName, MenuString: String;
  miParent, mi: TMenuItem;
  i: integer;
  FDoc: TXMLDocument;
  Node, SubNode:TDOMNode;
  mStream: TMemoryStream;
begin //AddMenus
  menuItemHeight := GetSystemMetrics(SM_CYMENU) + 2;
  itemsPerColumn := screen.height div menuItemHeight - 1;

  LockWindowUpdate(Handle);
  try
    // Load all menus from "Menu.xml"
    ClearMenus;

    mStream := TMemoryStream.Create;
    try
      if not TUtils.LoadFileFromZip(mStream, PUZZLES_ARCHIVE_PATH, MENU_FILE) then
        Exit;
      mStream.Seek(0, soBeginning);
      ReadXMLFile(FDoc, mStream);
      Node := FDoc.FindNode('xml');

      for i := 0 to Node.ChildNodes.Count - 1 do begin
        SubNode := Node.ChildNodes[i];
        if SameText(SubNode.NodeName, 'Menu') then begin
          mi := TMenuItem.Create(Self);
          mi.Caption := Trim(SubNode.FindNode('#text').NodeValue);
          MainMenu.Items.Insert(MainMenu.Items.Count, mi);
          AddSubMenu(mi, SubNode);

          //MenuCaption :=
        end;
      //ShowMessage(Node.ChildNodes[i].FindNode('#text').NodeValue);
      end;

      FDoc.Free;

      // Load from Puzzles collection
      for i := 0 to TLibrary.Puzzles.Count - 1 do begin
        MenuString := TLibrary.Puzzles[i].MenuString;
        while MenuString <> '' do begin
          S := TUtils.Parse(MenuString, ';');
          if S <> '' then begin
            miParent := MainMenu.Items;

            while S <> '' do begin
              MenuName := TUtils.Parse(S, '\');

              // find menu
              mi := FindSubMenuByCaption(miParent, MenuName);

              // insert if not found
              if mi = nil then begin
                mi := TMenuItem.Create(Self);
                mi.Caption := MenuName;

                if miParent.Parent = nil then
                  miParent.Insert(miParent.Count - 1, mi)
                else
                  miParent.Add(mi);

                // set "New" mark for all parents
                SetStateForMenuAndParents(mi, CM_NEW);
              end;

              if S = '' then
                SetupTerminalMenu(mi, TLibrary.Puzzles[i])
              else begin
                mi.OnClick := nil;
                miParent := mi;
              end;

            end;
          end;
        end;
      end;

      for i := 1 to MainMenu.Items.Count - 1 do
        MakePictureForDisabledMenus(MainMenu.Items[i]);

    finally
      mStream.Free;
    end;
  finally
    LockWindowUpdate(0);
  end;
end; //AddMenus

procedure TMainForm.CheckMenusState;

  procedure ClearCurrentState(mi: TMenuItem; i: integer);
  begin
    mi.ImageIndex := (mi.ImageIndex + 1) and (not CM_CURRENT) - 1;
    if mi.ImageIndex = -1 then
      FCheckedMenuItems.Delete(i);
  end;

var i: integer;
    MenuString, MenuName, S: string;
    mi, miParent: TMenuItem;
    ws: TWindowState;
begin
  ws := WindowState;
  LockWindowUpdate(Handle);

  for i := FCheckedMenuItems.Count - 1 downto 0 do
    ClearCurrentState(FCheckedMenuItems[i], i);

  i := TLibrary.Puzzles.FindPuzzle(CurrentPuzzle);
  if i >= 0 then begin
    MenuString := TLibrary.Puzzles[i].MenuString;
    while MenuString <> '' do begin
      S := TUtils.Parse(MenuString, ';');

      miParent := MainMenu.Items;

      mi := nil;
      while S <> '' do begin
        MenuName := TUtils.Parse(S, '\');

        mi := FindSubMenuByCaption(miParent, MenuName);
        if mi = nil then
          break;

        miParent := mi;
      end;

      if mi <> nil then
        SetStateForMenuAndParents(mi, CM_CURRENT);
    end;
  end;
  LockWindowUpdate(0);
  if ws <> WindowState then
    WindowState := ws;
end; // CheckMenusState

//procedure TMainForm.HighlightMenu;
{  procedure ClearMenus(miParent: TMenuItem);
  var i: integer;
  begin
    miParent.F
    for i := 0 to miParent.Count - 1 do begin

    end;

  end;}
//begin
{  miParent := MainMenu.Items;


    for i := 0 to Puzzles.Count - 1 do begin
      S := Puzzles[i].MenuString;
      if S = '' then
        continue;

      miParent := MainMenu.Items;

      while S <> '' do begin
        MenuName := Parse(S, '\');

        // ищем меню
        mi := nil;
        for j := 0 to miParent.Count - 1 do
          if AnsiSameText(StripHotKey(miParent.Items[j].Caption), MenuName) then begin
            mi := miParent.Items[j];
            if S = '' then
//            mi.Checked := True;  // test
            break;
          end;
      end;
    end;}
//end; // FindMenuMenu

procedure TMainForm.MenuHandler(Sender: TObject);
begin
  LoadFromLibraryItem(TLibrary.Puzzles.GetById(TMenuItem(Sender).Tag))
end;

// ============================================================================
// Puzzle timer
// ============================================================================

procedure TMainForm.UpdateTimer;
begin
  if CurrentPuzzle <> nil then
    sbMain.Panels[0].Text := 'Timer: ' + Format('%d:%.2d:%.2d', [CurrentPuzzle.TimerValue div 3600, CurrentPuzzle.TimerValue div 60 mod 60, CurrentPuzzle.TimerValue mod 60])
  else
    sbMain.Panels[0].Text := '';
end;

procedure TMainForm.tMainTimer(Sender: TObject);
begin
  if CurrentPuzzle <> nil then
    Inc(CurrentPuzzle.TimerValue);
  UpdateTimer;
end;

procedure TMainForm.ResetTimer;
begin
  if CurrentPuzzle <> nil then
    CurrentPuzzle.TimerValue := 0;
  tMain.Enabled := False;
  UpdateTimer;
end;

// ============================================================================
// Moves Counter
// ============================================================================

procedure TMainForm.ResetMovesCounter;
begin
  if CurrentPuzzle <> nil then
    CurrentPuzzle.MovesCounter := 0;
  UpdateMovesCounter;
end;

procedure TMainForm.UpdateMovesCounter;
begin
  if CurrentPuzzle <> nil then
    sbMain.Panels[1].Text := 'Moves: ' + IntToStr(CurrentPuzzle.MovesCounter)
  else
    sbMain.Panels[1].Text := 'Moves: 0';
end;

// ============================================================================
// Puzzle events
// ============================================================================

{$WARN 5024 off : Parameter "$1" not used}

procedure TMainForm.OnBeforeLoadFromXml(XmlNode: TDOMNode);
begin
//  Log('OnLoadFromXml');
end;

procedure TMainForm.OnAfterLoadFromXml(XmlNode: TDOMNode);
begin
//  Log('OnAfterLoadFromXml');
end;

procedure TMainForm.OnAfterLoadFromStream(Stream: TStream);
begin
//  Log('OnAfterLoadFromStream');
end;

procedure TMainForm.OnBeforeLoadFromStream(Stream: TStream);
begin
//  Log('OnLoadFromStream');
end;

procedure TMainForm.OnBeforeTurn(AType: TTurningType; AState: TTurningState);
begin
  if frmPuzzle3d.GraphicsUpdateEnabled then
    GraphicsEngine.OnBeforePuzzleTurn(TSelection.TurnAxisNo);
//  Log('OnBeforeTurn');
end;

procedure TMainForm.OnTransLayers(mat: T4x4Matrix; AxisNo: integer; Layers: array of Boolean);
begin
  if frmPuzzle3d.GraphicsUpdateEnabled then
    GraphicsEngine.TransLayers(mat, AxisNo, Layers);
end;

procedure TMainForm.OnTurning(AType: TTurningType; AState: TTurningState);
begin
  if (AType = ttManual) and (AState = tsTurningAnimated) then begin
    frmPuzzle3d.pb3d.Invalidate;
    Application.ProcessMessages;
  end;
end;

procedure TMainForm.OnAfterTurn(AType: TTurningType; AState: TTurningState);
begin
//  Log('OnAfterTurn');
end;

procedure TMainForm.OnMacroExecute(Macro: String);
begin
  TSettings.InteractiveDisabled := True;
  frmPuzzle3d.GraphicsUpdateEnabled := False;
  try
    TXmlReadWrite.PuzzleExecMacroFromXmlFile(CurrentPuzzle, CurrentPuzzle.Header.FileName, Macro);
  finally
    frmPuzzle3d.GraphicsUpdateEnabled := True;
    TSettings.InteractiveDisabled := False;
  end;

  GraphicsEngine.ReCalc;
  frmPuzzle3d.pb3d.Invalidate;
  UpdateSolvedStatus;
  UpdateMovesCounter;
//    UpdateTimer;
end; // OnMacroExecute

{$WARN 5024 on : Parameter "$1" not used}

// ============================================================================
// UpdateControls
// ============================================================================

procedure TMainForm.UpdateCaption;
begin
  if CurrentPuzzle <> nil then
    Caption := 'pCubes - ' + CurrentPuzzle.Header.Name
  else
    Caption := 'pCubes';
end; // UpdateCaption

// set timer and StatusBar test
function TMainForm.UpdateSolvedStatus: boolean;
begin
  Result := False;

  UpdateStatusSolved; // set text in StatusBar

  if CurrentPuzzle = nil then
    tMain.Enabled := False
  else if not FSolvedChecker.isSolved then
    tMain.Enabled := True
  else begin
    tMain.Enabled := False;
    Result := True;
  end;
end; // UpdateSolvedStatus

procedure TMainForm.Update3dPage;
begin
  frmPuzzle3d.miAntiAlias.Checked   := TSettings.AntiAlias;
  frmPuzzle3d.miWired.Checked       := TSettings.Wired;
  frmPuzzle3d.miShowAxes.Checked    := TSettings.ShowAxes;
  frmPuzzle3d.miShowNormals.Checked := TSettings.ShowNormals;
  frmPuzzle3d.miContourMode.Checked := TSettings.ContourMode;

  frmPuzzle3d.miTurn.Checked         := TSettings.MouseMode = 0;
  frmPuzzle3d.miHidePart.Checked     := TSettings.MouseMode = 1;
  frmPuzzle3d.miRemovePart.Checked   := TSettings.MouseMode = 2;
  frmPuzzle3d.miHideSticker.Checked  := TSettings.MouseMode = 3;
  frmPuzzle3d.miStickerColor.Checked := TSettings.MouseMode = 4;
  frmPuzzle3d.miChangeColor.Checked  := TSettings.MouseMode = 5;
end; // Update3dPage

procedure TMainForm.UpdateSettingsPage;
begin
  frmSettings.rbGraphicsEngineBsp.Checked     := FGraphicsEngine = geBsp;
  frmSettings.rbGraphicsEngineOpenGl.Checked  := FGraphicsEngine = geOpenGl;
  frmSettings.rbGraphicsEngineZBuffer.Checked := FGraphicsEngine = geZBuffer;

  frmSettings.cbContourMode.Checked := TSettings.ContourMode;

  case TSettings.MouseMode of
    0: frmSettings.rbTurn.Checked         := True;
    1: frmSettings.rbHidePart.Checked     := True;
    2: frmSettings.rbRemovePart.Checked   := True;
    3: frmSettings.rbHideSticker.Checked  := True;
    4: frmSettings.rbStickerColor.Checked := True;
    5: frmSettings.rbChangeColor.Checked  := True;
  end;

  frmSettings.bBodyColor.Color       := GraphicsEngine.BodyColor;
  frmSettings.bEdgeColor.Color       := GraphicsEngine.EdgeColor;
  frmSettings.bBackgroundColor.Color := GraphicsEngine.BackgroundColor;

  frmSettings.tbLighting.Position := GraphicsEngine.Lighting;
end; // UpdateSettingsPage

procedure TMainForm.UpdateControls;
begin
  frmPuzzle3d.pb3d.Invalidate;
  UpdateSolvedStatus;
  UpdateCaption;

  if CurrentPuzzle <> nil then begin
    frmPuzzle3d.tbShowFrom.Max := CurrentPuzzle.Parts.Count;
    frmPuzzle3d.tbShowTo.Max := CurrentPuzzle.Parts.Count;

    frmLibrary.FocusCurrent;
    frmLibrary.sgLibrary.Refresh;
  end else begin
    frmPuzzle3d.tbShowFrom.Max := 0;
    frmPuzzle3d.tbShowTo.Max := 0;
  end;
  frmPuzzle3d.tbShowToChange(nil);
end; // UpdateControls

// ============================================================================
// Log
// ============================================================================

procedure TMainForm.UpdateLog;
begin
  frmLog.UpdateLog;
end;

procedure TMainForm.OnLogUpdated();
begin
  if nMain.PageIndex = PAGE_LOG then
    UpdateLog;
end;

procedure TMainForm.Log(const LogStr: String);
begin
  FLog.Add(LogStr);
  //if Assigned(LogList) then begin
  //  LogList.Add(TimeToStr(Now) + ': ' + LogStr);
  //  while LogList.Count > 500 do
  //    LogList.Delete(0);
  //end;
//  if nMain.PageIndex = PAGE_LOG then
//    UpdateLog;
end; // Log

procedure TMainForm.LogWarning(const WarnStr: String);
begin
  FLog.Add('Warning: ' + WarnStr);
  //if Assigned(LogList) then
  //  LogList.Add(TimeToStr(Now) + ': ' + 'Warning: ' + WarnStr);
//  if nMain.PageIndex = PAGE_LOG then
//    UpdateLog;
end; // LogWarning

// ============================================================================
// Utils
// ============================================================================

procedure TMainForm.Scramble;
begin
  if not TXmlReadWrite.PuzzleExecMacroFromXmlFile(CurrentPuzzle, CurrentPuzzle.Header.FileName, 'Scramble') then
    CurrentPuzzle.Scramble;
end; // Scramble

//{$WARN 5057 off : Local variable "$1" does not seem to be initialized}
//function TMainForm.GetVersionName: string;
//type
//  TVerInfo = packed record
//    Dummy: array[0..47] of byte; // unnecessary 48 bytes
//    Minor, Major, Build, Release: word;
//  end;
//var
//  s: TResourceStream;
//  v: TVerInfo;
//begin
//  Result := '';
//  try
//    s := TResourceStream.Create(HInstance, '#1', RT_VERSION); // get resource
//    try
//      if s.Size > 0 then begin
//        s.Read(v, SizeOf(v)); // read bytes needed
//        Result := IntToStr(v.Major) + '.' + IntToStr(v.Minor) + 'beta. Build ' +
//                  {IntToStr(v.Release) + '.' +} IntToStr(v.Build);
//      end;
//    finally
//      s.Free;
//    end;
//  except;
//  end;
//end; // GetVersionName
//{$WARN 5057 on : Local variable "$1" does not seem to be initialized}

procedure TMainForm.aRandomExecute(Sender: TObject);
begin
  frmLibrary.aRandomExecute(Sender);
end; // aRandomExecute

procedure TMainForm.aNextExecute(Sender: TObject);
begin
  frmLibrary.aNextExecute(Sender);
end;

procedure TMainForm.aPreviousExecute(Sender: TObject);
begin
  frmLibrary.aPreviousExecute(Sender);
end;

end.

