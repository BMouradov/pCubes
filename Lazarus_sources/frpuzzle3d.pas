{************************************************************}
{                                                            }
{  Unit frPuzzle3d                                           }
{  2023-2023                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit frPuzzle3d;

{$mode ObjFPC}{$H+}

interface

uses
  uPuzzleScreen, Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  ActnList, ComCtrls, Graphics, Dialogs, Menus;

type

  { TPuzzle3dFrame }

  TPuzzle3dFrame = class(TFrame)
    aAddMark: TAction;
    aOpenLibraryPage: TAction;
    aNext: TAction;
    aPrevious: TAction;
    aRandom: TAction;
    aZoomOut: TAction;
    aZoomIn: TAction;
    aUndoToMark: TAction;
    aRedo: TAction;
    aUndo: TAction;
    aScrambleInf: TAction;
    alPuzzle3d: TActionList;
    aReset: TAction;
    aScramble: TAction;
    bMark: TButton;
    bRedo: TButton;
    bReset: TButton;
    bScramble: TButton;
    bScrambleInf: TButton;
    bUndo: TButton;
    bUndoToMark: TButton;
    bZoomIn: TButton;
    bZoomOut: TButton;
    cbShowAxes: TCheckBox;
    cbWired: TCheckBox;
    dBkColor: TColorDialog;
    lFromTo: TLabel;
    miLibraryRandom: TMenuItem;
    miLibraryPrev: TMenuItem;
    miLibraryNext: TMenuItem;
    miLibrary: TMenuItem;
    miContourMode: TMenuItem;
    miShowNormals: TMenuItem;
    miWired: TMenuItem;
    miShowAxes: TMenuItem;
    miAntiAlias: TMenuItem;
    miVisualization: TMenuItem;
    miChangeColor: TMenuItem;
    miStickerColor: TMenuItem;
    miHideSticker: TMenuItem;
    miRemovePart: TMenuItem;
    miHidePart: TMenuItem;
    miTurn: TMenuItem;
    miMouseClickMode: TMenuItem;
    p3dRightPanel: TPanel;
    pb3d: TPaintBox;
    pm3d: TPopupMenu;
    StaticText2: TStaticText;
    tbExplode: TTrackBar;
    tbShowFrom: TTrackBar;
    tbShowTo: TTrackBar;
    procedure aAddMarkExecute(Sender: TObject);
    procedure aOpenLibraryPageExecute(Sender: TObject);
    procedure aRedoExecute(Sender: TObject);
    procedure aResetExecute(Sender: TObject);
    procedure aScrambleExecute(Sender: TObject);
    procedure aScrambleInfExecute(Sender: TObject);
    procedure aUndoExecute(Sender: TObject);
    procedure aUndoToMarkExecute(Sender: TObject);
    procedure aZoomInExecute(Sender: TObject);
    procedure aZoomOutExecute(Sender: TObject);
    procedure cbShowAxesClick(Sender: TObject);
    procedure cbWiredClick(Sender: TObject);
    procedure miAntiAliasClick(Sender: TObject);
    procedure miContourModeClick(Sender: TObject);
    procedure miLibraryNextClick(Sender: TObject);
    procedure miLibraryPrevClick(Sender: TObject);
    procedure miLibraryRandomClick(Sender: TObject);
    procedure miShowAxesClick(Sender: TObject);
    procedure miShowNormalsClick(Sender: TObject);
    procedure miTurnClick(Sender: TObject);
    procedure miWiredClick(Sender: TObject);
    procedure pb3dMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pb3dMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pb3dMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pb3dPaint(Sender: TObject);
    procedure pb3dResize(Sender: TObject);
    procedure tbExplodeChange(Sender: TObject);
    procedure tbShowFromChange(Sender: TObject);
    procedure tbShowToChange(Sender: TObject);
  private
    FImage: TBitmap;
    mx, my: integer;
    FPuzzleScreen: TPuzzleScreen;
    FMouseMoved: Boolean;
  public
    GraphicsUpdateEnabled: Boolean;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadPuzzle;
    procedure ClearPuzzle;
  end;

implementation
{$R *.lfm}
uses uUtils, uSelection, uFace, uPart, uPuzzleUtils, uMatrix, uVector, uPuzzle, uLibrary,
     fMain,
     Math, IntfGraphics, LCLIntf, LCLType, FPImage;

constructor TPuzzle3dFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FImage := nil;
  GraphicsUpdateEnabled := True;
end;

destructor TPuzzle3dFrame.Destroy;
begin
  if FImage <> nil then
    FImage.Free;

  inherited Destroy;
end;

procedure TPuzzle3dFrame.pb3dResize(Sender: TObject);
begin
  MainForm.GraphicsEngine.SetBitmap(nil);

  if FImage <> nil then
    FreeAndNil(FImage);

  MainForm.GraphicsEngine.d := 10 / 3;  //!!!!!

  MainForm.GraphicsEngine.Resize(pb3d.Width, pb3d.Height);
  //  pb3d.Invalidate;
end;

procedure TPuzzle3dFrame.tbExplodeChange(Sender: TObject);
begin
  MainForm.GraphicsEngine.SetExplode(tbExplode.Position);
  pb3d.Invalidate;
end;

procedure TPuzzle3dFrame.tbShowFromChange(Sender: TObject);
begin
  if tbShowTo.Position <> tbShowFrom.Position then
    tbShowTo.Position := tbShowFrom.Position
  else
    tbShowToChange(Sender);
end; // tbShowFromChange

procedure TPuzzle3dFrame.tbShowToChange(Sender: TObject);
begin
  if MainForm.CurrentPuzzle <> nil then begin
    tbShowFrom.Max := MainForm.CurrentPuzzle.Parts.Count;
    tbShowTo.Max := MainForm.CurrentPuzzle.Parts.Count;
  end else begin
    tbShowFrom.Max := 0;
    tbShowTo.Max := 0;
  end;

  if (tbShowFrom.Position = 0) or (tbShowTo.Position = 0) then begin
    MainForm.GraphicsEngine.SetPartsFromTo(-1, -1);
    lFromTo.Caption := 'All Visible';
  end else begin
    MainForm.GraphicsEngine.SetPartsFromTo(tbShowFrom.Position - 1, tbShowTo.Position - 1);
    lFromTo.Caption := IntToStr(tbShowFrom.Position - 1) + '-' + IntToStr(tbShowTo.Position - 1);
  end;
  MainForm.GraphicsEngine.Recalc;
  pb3d.Invalidate;
end; // tbShowToChange

const
    MaxPixelCount   =  32768;
type
  TRGBArray = array[0..MaxPixelCount - 1] of TRGBTriple; // rgba
  pRGBArray = ^TRGBArray;

procedure TPuzzle3dFrame.pb3dPaint(Sender: TObject);
var i, j: integer;
  sl: pRGBArray;
begin // pb3dPaint
  if FImage = nil then begin
    FImage := Graphics.TBitmap.Create;
    FImage.Width := pb3d.Width;
    FImage.Height := pb3d.Height;
    FImage.PixelFormat := pf24bit;
  end;

  MainForm.GraphicsEngine.SetBitmap(FImage);
  MainForm.GraphicsEngine.PaintScene(MainForm.CurrentPuzzle);

  {$WARN 5044 off : Symbol "$1" is not portable}
  if TSettings.ContourMode then begin
    FImage.BeginUpdate;
    for j := 0 to FImage.Height - 1 do begin
      sl := FImage.ScanLine[j];
      for i := 0 to FImage.Width - 1 do begin
        if (sl^[i].rgbtRed <> 0) or (sl^[i].rgbtGreen <> 0) or (sl^[i].rgbtBlue <> 0) then begin
          sl^[i].rgbtRed := $ff;
          sl^[i].rgbtGreen := $ff;
          sl^[i].rgbtBlue := $ff;
        end;
  //      ptr_texture_pixel^ := Cardinal(sl[i]);
  //      inc(ptr_texture_pixel, 1);
      end;
    end;
    FImage.EndUpdate;
  end;
  {$WARN 5044 on : Symbol "$1" is not portable}

  pb3d.Canvas.Draw(0,0,FImage);
end; // pbMainPaint

procedure TPuzzle3dFrame.aResetExecute(Sender: TObject);
var FileName: String;
begin
  if MainForm.CurrentPuzzle = nil then
    Exit;
  if TSettings.InteractiveDisabled then
    Exit;
  FileName := MainForm.CurrentPuzzle.Header.FileName;
  if FileExists(FileName) then begin
    MainForm.LoadPuzzleFromFile(FileName);
    MainForm.CurrentPuzzle.Header.FileName := FileName;
    MainForm.UpdateControls;
  end else
    MainForm.LoadFromLibrary(TPuzzleClass(MainForm.CurrentPuzzle.ClassType), MainForm.CurrentPuzzle.Header.Name, MainForm.CurrentPuzzle.Header.FileName);
  MainForm.ResetMovesCounter;
//CurrentPuzzle.CalcPartPositions;
end; // aResetExecute

var N: integer;
    TicksOld, TicksNew: Cardinal;

procedure TPuzzle3dFrame.aScrambleExecute(Sender: TObject);
begin
  if MainForm.CurrentPuzzle = nil then
    Exit;
  if TSettings.InteractiveDisabled then
    Exit;

  TSettings.InteractiveDisabled := True;
  GraphicsUpdateEnabled := False;
  try
    N := 1;
    if GetKeyState(VK_SHIFT) < 0 then
      N := 10
    else if GetKeyState(VK_CONTROL) < 0 then
      N := 100
    else if GetKeyState(VK_MENU) < 0 then
      N := 1000;
    TicksOld := GetTickCount;

    while N > 0 do begin
      MainForm.Scramble;

      TicksNew := GetTickCount;
      if TicksNew > TicksOld + 100 then begin
        Application.ProcessMessages;
        TicksOld := TicksNew;
      end;

      Dec(N);
    end;

    MainForm.UpdateMovesCounter;
  finally
    GraphicsUpdateEnabled := True;
    TSettings.InteractiveDisabled := False;
  end;

  MainForm.CurrentPuzzle.AfterTurnEvent(ttManual); // for JailBreaks;

  MainForm.GraphicsEngine.ReCalc;
  pb3d.Invalidate;
  MainForm.UpdateSolvedStatus;
  MainForm.ResetTimer;
  MainForm.ResetMovesCounter;
end; // aScrambleExecute

procedure TPuzzle3dFrame.aScrambleInfExecute(Sender: TObject);
begin
  if bScrambleInf.Tag = 1 then begin
    bScrambleInf.Tag := 0;
    Exit;
  end;

  if TSettings.InteractiveDisabled then
    Exit;

  bScrambleInf.Tag := 1; // in process
  bScrambleInf.Caption := 'STOP scrmb';

  TSettings.InteractiveDisabled := True;
  GraphicsUpdateEnabled := False;
  try
    while True do begin
      MainForm.Scramble;

      TicksNew := GetTickCount;
      if TicksNew > TicksOld + 100 then begin
        MainForm.UpdateMovesCounter;
        MainForm.GraphicsEngine.ReCalc;
        pb3d.Invalidate;
        Application.ProcessMessages;
        TicksOld := TicksNew;

        if bScrambleInf.Tag = 0 then
          break;
      end;
    end;
  finally
    GraphicsUpdateEnabled := True;
    TSettings.InteractiveDisabled := False;
    bScrambleInf.Caption := 'Scrmb inf';
    bScrambleInf.Tag := 0; // in case of exception
  end;
  MainForm.GraphicsEngine.ReCalc;
  pb3d.Invalidate;
  MainForm.UpdateSolvedStatus;
  MainForm.ResetTimer;
  MainForm.ResetMovesCounter;
end; // aScrambleInfExecute

procedure TPuzzle3dFrame.aUndoExecute(Sender: TObject);

  procedure MultUndo(Count: integer);
  var i: integer;
  begin
    for i := 0 to Count - 1 do
      MainForm.CurrentPuzzle.Undo(False);
    MainForm.GraphicsEngine.ReCalc;
  end;

begin
  if (Sender = bUndo) and (GetKeyState(VK_SHIFT) < 0) then
    MultUndo(10)
  else if (Sender = bUndo) and (GetKeyState(VK_CONTROL) < 0) then
    MultUndo(100)
  else if (Sender = bUndo) and (GetKeyState(VK_MENU) < 0) then
    MultUndo(1000)
  else
    MainForm.CurrentPuzzle.Undo(True);

  MainForm.UpdateMovesCounter;
  pb3d.Invalidate;
  MainForm.UpdateSolvedStatus;
end; // aUndoExecute

procedure TPuzzle3dFrame.aRedoExecute(Sender: TObject);

  procedure MultRedo(Count: integer);
  var i: integer;
  begin
    for i := 0 to Count - 1 do
      MainForm.CurrentPuzzle.Redo(False);
    MainForm.GraphicsEngine.ReCalc;
  end;

begin
  if (Sender = bRedo) and (GetKeyState(VK_SHIFT) < 0) then
    MultRedo(10)
  else if (Sender = bRedo) and (GetKeyState(VK_CONTROL) < 0) then
    MultRedo(100)
  else if (Sender = bRedo) and (GetKeyState(VK_MENU) < 0) then
    MultRedo(1000)
  else
    MainForm.CurrentPuzzle.Redo(True);

  MainForm.UpdateMovesCounter;
  pb3d.Invalidate;
  MainForm.UpdateSolvedStatus;
end; // aRedoExecute

procedure TPuzzle3dFrame.aAddMarkExecute(Sender: TObject);
begin
  MainForm.CurrentPuzzle.AddMark;
end; // aAddMarkExecute

procedure TPuzzle3dFrame.aOpenLibraryPageExecute(Sender: TObject);
begin
  MainForm.SetCurrentPage(PAGE_LIBRARY);
end;

procedure TPuzzle3dFrame.aUndoToMarkExecute(Sender: TObject);
begin
  MainForm.CurrentPuzzle.UndoToMark;
  MainForm.GraphicsEngine.ReCalc;
  MainForm.UpdateMovesCounter;
  pb3d.Invalidate;
  MainForm.UpdateSolvedStatus;
end; // aUndoToMarkExecute

procedure TPuzzle3dFrame.aZoomInExecute(Sender: TObject);
begin
  MainForm.GraphicsEngine.Zoom := MainForm.GraphicsEngine.Zoom * 1.2;
  pb3d.Invalidate;
end; // aZoomInExecute

procedure TPuzzle3dFrame.aZoomOutExecute(Sender: TObject);
begin
  MainForm.GraphicsEngine.Zoom := MainForm.GraphicsEngine.Zoom / 1.2;
  pb3d.Invalidate;
end; // aZoomOutExecute

procedure TPuzzle3dFrame.cbWiredClick(Sender: TObject);
begin
  MainForm.SetWired(cbWired.Checked);
end; // cbWiredClick

procedure TPuzzle3dFrame.cbShowAxesClick(Sender: TObject);
begin
  MainForm.SetShowAxes(cbShowAxes.Checked);
end; // cbShowAxes1Click



{$WARN 5024 off : Parameter "$1" not used}
procedure TPuzzle3dFrame.pb3dMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  procedure RemoveSelectedPart;
  begin
    if TSelection.SelectedPart <> nil then begin
      MainForm.CurrentPuzzle.Parts.Remove(TSelection.SelectedPart);
      MainForm.CurrentPuzzle.PositionsInvalidate;

      TSelection.SelectedPart.Free;
      TSelection.SelectedPart := nil;

      MainForm.CurrentPuzzle.ClearState;
      TSelection.ClearSelection;
      MainForm.GraphicsEngine.Select(MainForm.CurrentPuzzle, mx, my);
      TSelection.OldSelected := TSelection.SelectedPart;
      MainForm.GraphicsEngine.ReCalc;
      pb3d.Invalidate;
      MainForm.UpdateSolvedStatus;
    end
  end; // RemoveSelectedPart

  procedure HideSelectedPart;
  begin
    if MainForm.CurrentPuzzle.Parts.IndexOf(TSelection.SelectedPart) <> -1 then
      TSelection.SelectedPart.Visible := not TSelection.SelectedPart.Visible;
    TSelection.ClearSelection;
    MainForm.GraphicsEngine.Select(MainForm.CurrentPuzzle, mx, my);
    TSelection.OldSelected := TSelection.SelectedPart;
    MainForm.GraphicsEngine.ReCalc;
    pb3d.Invalidate;
    MainForm.UpdateSolvedStatus;
  end; // HideSelectedPart

  procedure HideSticker;
  begin
    TSelection.SelectedFace.IsStickerVisible := not TSelection.SelectedFace.IsStickerVisible;
    pb3d.Invalidate;
    MainForm.UpdateSolvedStatus;
  end; // HideSticker

  procedure FaceColor;
  var Face: TFace;
  begin
    Face := TSelection.SelectedFace;
    if Face = nil then
      Exit;
    dBkColor.Color := Face.Color;
    if dBkColor.Execute then begin
      Face.Color := dBkColor.Color;
      pb3d.Invalidate;
      MainForm.UpdateSolvedStatus;
    end;
  end; // FaceColor

  procedure ChangeColor;
  var OldColor: TColor;
      Part: TPart;
      Face: TFace;
  begin
    OldColor := TSelection.SelectedFace.Color;
    dBkColor.Color := OldColor;
    if dBkColor.Execute then begin
      for Part in MainForm.CurrentPuzzle.Parts do
        for Face in Part.Faces do
          if Face.Color = OldColor then
            Face.Color := dBkColor.Color;
      pb3d.Invalidate;
      MainForm.UpdateSolvedStatus;
    end;
  end; // ChangeColor

  procedure CombineParts;
  begin
    if (TSelection.OldSelected <> nil) and (TSelection.SelectedPart <> nil) and
       (TSelection.OldSelected <> TSelection.SelectedPart) and (not MainForm.CurrentPuzzle.IsTurning) then begin
      TPuzzleUtils.CombineParts(MainForm.CurrentPuzzle, TSelection.OldSelected, TSelection.SelectedPart);
      MainForm.CurrentPuzzle.ClearState;
      TSelection.ClearSelection;
      MainForm.GraphicsEngine.Select(MainForm.CurrentPuzzle, mx, my);
      TSelection.OldSelected := TSelection.SelectedPart;
      MainForm.GraphicsEngine.ReCalc;
      pb3d.Invalidate;
      MainForm.UpdateSolvedStatus;
    end
  end; // CombineParts

begin // pbMainMouseDown
  FMouseMoved := False;
  MainForm.FocusControl(nil);

  if MainForm.CurrentPuzzle = nil then
    Exit;
  if TSettings.InteractiveDisabled then
    Exit;

  mx := X;
  my := Y;
  MainForm.GraphicsEngine.Select(MainForm.CurrentPuzzle, mx, my);

  if TSelection.SelectedFace <> nil then begin
    if Shift = [ssLeft] then begin
      case TSettings.MouseMode of
        1: HideSelectedPart;
        2: RemoveSelectedPart;
        3: HideSticker;
        4: FaceColor;
        5: ChangeColor;
      end;
{      if MainForm.frmSettings.rbRemovePart.Checked then
        RemoveSelectedPart
      else if MainForm.frmSettings.rbHidePart.Checked then
        HideSelectedPart
      else if MainForm.frmSettings.rbHideSticker.Checked then
        HideSticker
      else if MainForm.frmSettings.rbStickerColor.Checked then
        FaceColor
      else if MainForm.frmSettings.rbChangeColor.Checked then
        ChangeColor;}
    end else if (Shift = [ssShift, ssLeft]) then
      CombineParts;
{  end else if (Shift = [ssCtrl, ssLeft]) then begin
    RemoveSelectedPart;
   if (TSelection.OldSelected <> nil) and (TSelection.SelectedPart <> nil) and (not CurrentPuzzle.IsTurning) then begin
      TSelection.OldSelected.AddChain(TSelection.SelectedPart);
      TSelection.SelectedPart.AddChain(TSelection.OldSelected);
    end}
  end;
  if Shift = [] then
    TSelection.OldSelected := TSelection.SelectedPart;

  if TSelection.SelectedFace <> nil then
    MainForm.Log('Clicked: Part ' + IntToStr(MainForm.CurrentPuzzle.Parts.IndexOf(TSelection.SelectedPart)) + ', Face ' + IntToStr(TSelection.SelectedPart.Faces.IndexOf(TSelection.SelectedFace)) + ', Nearest Vertex ' + IntToStr(TSelection.MouseNearestVertex));
end; // pbMainMouseDown

procedure TPuzzle3dFrame.pb3dMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);

  procedure RotateWholeFree(dx, dy: integer);
  var a, b: TVector;
    al: extended;
  begin
    al := Hypot(dx, dy) * 3 / MainForm.GraphicsEngine.Zoom;

    if (dx = 0) and (dy = 0) then
      Exit;

    a := [dx, 0, dy];
    b := [0, -1, 0];
    a := a * b;
    if a.Abs > DELTA_DISTANCE then begin
      a.Normalize;
      MainForm.TransPuzzle(TMatrix.GetRotateMatrix(a, -al));
    end;
  end; // RotateWhole

  procedure CalcSelection(dx, dy: integer);
  begin
    TSelection.SetMouseMotion(dx, dy);

    TSelection.TurnAxisNo := FPuzzleScreen.GetRotateAxis(MainForm.GraphicsEngine, dx, dy);
    MainForm.Log('Turning Axis ' + IntToStr(TSelection.TurnAxisNo));
    if TSelection.TurnAxisNo = -1 then begin
      TSelection.TurnAxis := nil;
      Exit;
    end;

    TSelection.TurnAxis := MainForm.CurrentPuzzle.Axes[TSelection.TurnAxisNo];

    TSelection.RotateDirection := FPuzzleScreen.GetRotateDirection(dx, dy);
    TSelection.TurningAngle := 0;
  end; // CalcSelection

  procedure BeginTurnLayers(dx, dy: integer);
  var S: string;
      i: integer;
  begin
    CalcSelection(dx, dy);
    if TSelection.TurnAxisNo = -1 then
      Exit;

    if not MainForm.CurrentPuzzle.CalcMovingLayersFromPart(TSelection.TurnAxisNo, TSelection.SelectedPart) then
      Exit;
//Log(IntToStr(Ord(TSelection.TurnLayers[0]))+IntToStr(Ord(TSelection.TurnLayers[1]))+IntToStr(Ord(TSelection.TurnLayers[2]))+IntToStr(Ord(TSelection.TurnLayers[3]))+IntToStr(Ord(TSelection.TurnLayers[4]))+IntToStr(Ord(TSelection.TurnLayers[5])));

    MainForm.CurrentPuzzle.GetRotateAngleRange(TSelection.TurnAxisNo, TSelection.TurnLayers, TSelection.AngleMin, TSelection.AngleMax);

    MainForm.CurrentPuzzle.BeforeTurnEvent(ttManual, TSelection.TurnAxisNo, TSelection.TurnLayers, TSelection.AngleMin, TSelection.AngleMax, TSelection.RotateDirection); // Event
    if Assigned(MainForm.CurrentPuzzle.OnBeforeTurn) then
      MainForm.CurrentPuzzle.OnBeforeTurn(ttManual, tsFree);

    S := '';
    for i := 0 to High(TSelection.TurnLayers) do
      if TSelection.TurnLayers[i] then
        S := S + IntToStr(i) + '; ';
    MainForm.Log('Turning Layers ' + S);

    MainForm.CurrentPuzzle.TurningState := tsMidTurn;
  end; // BeginTurnLayers

  procedure ContinueTurnLayers(dx, dy: integer);
  var al: extended;
      MouseMotion: TVector;
  //  MinAngle, MaxAngle: extended;
  begin
    MouseMotion := [dx, dy, 0];

    al := TSelection.RotateDirection * MouseMotion ** TSelection.MouseVector * 3 / MainForm.GraphicsEngine.Zoom;

  //  MinAngle := TSelection.TurnAxis.Layers[Layer].MinAngle - TSelection.TurnAxis.Layers[Layer].Angle;
  //  MaxAngle := TSelection.TurnAxis.Layers[Layer].MaxAngle - TSelection.TurnAxis.Layers[Layer].Angle;
  //  if (MinAngle < -DELTA) and (MaxAngle < -DELTA) then begin
  //    MinAngle := MinAngle + 2 * Pi;
  //    MaxAngle := MaxAngle + 2 * Pi;
  //  end else if (MinAngle > DELTA) and (MaxAngle > DELTA) then begin
  //    MinAngle := MinAngle - 2 * Pi;
  //    MaxAngle := MaxAngle - 2 * Pi;
  //  end;
    if al < TSelection.AngleMin then
      al := TSelection.AngleMin;
    if al > TSelection.AngleMax then
      al := TSelection.AngleMax;

    //CurrentPuzzle.TransLayers(CurrentPuzzle.Axes[TSelection.TurnAxisNo].GetRotateMatrix(al - TSelection.TurningAngle), TSelection.TurnAxisNo, TSelection.TurnLayerFrom, TSelection.TurnLayerTo);
    MainForm.CurrentPuzzle.TransLayers(MainForm.CurrentPuzzle.Axes[TSelection.TurnAxisNo].GetRotateMatrix(al - TSelection.TurningAngle), TSelection.TurnAxisNo, TSelection.TurnLayers);

    MainForm.CurrentPuzzle.TurningEvent(ttManual); // Event
    if Assigned(MainForm.CurrentPuzzle.OnTurning) then
      MainForm.CurrentPuzzle.OnTurning(ttManual, tsMidTurn);

    TSelection.TurningAngle := al;
  //  FGraphicsEngine.Recalc;
  end; // ContinueTurnLayers

  procedure BeginTurnWhole(dx, dy: integer); // turn whole around _selected_ axis
  begin
    if TSelection.SelectedPart = nil then
      Exit;

    CalcSelection(dx, dy);

    MainForm.CurrentPuzzle.TurningState := tsWholeRotating;
  end; // BeginTurnWhole

  procedure ContinueTurnWhole(dx, dy: integer);
  var al: extended;
      MouseVector: TVector;
  begin
    MouseVector := [dx, dy, 0];
    al := TSelection.RotateDirection *
          MouseVector ** TSelection.MouseVector * 3 / MainForm.GraphicsEngine.Zoom;
    MainForm.TurnWhole(x, y, dx, dy, al);
  end; // ContinueTurnWhole

var
  dx, dy: integer;
//  d: extended;
const GAP = 5;
begin // pbMainMouseMove
  FMouseMoved := True;
  if MainForm.CurrentPuzzle = nil then
    Exit;
  if TSettings.InteractiveDisabled then
    Exit;
  if MainForm.CurrentPuzzle.TurningState = tsTurningAnimated then
    Exit;

  dx := X - mx;
  dy := -(Y - my);

  if Shift = [] then begin
    // Todo highlight one-click parts to rotate

//    GraphicsEngine.GetMouseParts(CurrentPuzzle, X, Y);
//    TSelection.HighlightedAxis := TSelection.MouseOverAxis;
//    TSelection.HighlightedParts.ClearWOItems;

//    Puzzle.HighlightLayers(MouseOverAxisNo, 0, 0);

  end else if (Shift = [ssRight]) or ((Shift = [ssLeft]) and (TSelection.SelectedPart = nil)) then begin
    // rotate whole
    RotateWholeFree(dx, dy);
    mx := X;
    my := Y;
    pb3d.Invalidate;
  end else if Shift = [ssLeft] then begin
    // turn face
    if MainForm.CurrentPuzzle.IsTurning then
      ContinueTurnLayers(dx, dy)
    else if Hypot(dx, dy) > GAP  then
      BeginTurnLayers(dx, dy); // !!!!! если слой повернуть нельзя, то вызывается много раз (

    pb3d.Invalidate;
  end else if (Shift = [ssAlt, ssLeft]) or (Shift = [ssMiddle]) then begin
    // turn whole around selected axis
    if MainForm.CurrentPuzzle.IsTurning or (Hypot(dx, dy) > GAP) then begin
      if MainForm.CurrentPuzzle.TurningState <> tsWholeRotating then
        BeginTurnWhole(dx, dy)
      else
        ContinueTurnWhole(dx, dy);
      pb3d.Invalidate;
    end;
  end;
end; // MouseMove

procedure TPuzzle3dFrame.pb3dMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (not FMouseMoved) and (not MainForm.CurrentPuzzle.IsTurning) then
    pm3d.PopUp;

  if MainForm.CurrentPuzzle = nil then
    Exit;
  if TSettings.InteractiveDisabled then
    Exit;

  if (Button = mbLeft) or (Button = mbRight) then
    if MainForm.CurrentPuzzle.IsTurning and (Shift = []) then
      MainForm.EndTurn;

  if MainForm.CurrentPuzzle.TurningState = tsWholeRotating then
    if not (ssLeft in Shift) or not (ssAlt in Shift) then
      MainForm.EndTurnWhole;

  TSelection.ClearSelection;
  pb3d.Invalidate;

  MainForm.UpdateMovesCounter;
end; //pbMainMouseUp
{$WARN 5024 on : Parameter "$1" not used}

procedure TPuzzle3dFrame.LoadPuzzle;
begin
  FPuzzleScreen := TPuzzleScreen.Create(MainForm.CurrentPuzzle);
end;

procedure TPuzzle3dFrame.ClearPuzzle;
begin
  if FPuzzleScreen <> nil then
    FreeAndNil(FPuzzleScreen);
end;

procedure TPuzzle3dFrame.miTurnClick(Sender: TObject);
begin
  TSettings.MouseMode := tMenuItem(Sender).Tag;
  MainForm.Update3dPage;
  MainForm.Update3dPage;
end; // miTurnClick

procedure TPuzzle3dFrame.miAntiAliasClick(Sender: TObject);
begin
  MainForm.SetAntiAlias(not TSettings.AntiAlias);
end;

procedure TPuzzle3dFrame.miShowAxesClick(Sender: TObject);
begin
  MainForm.SetShowAxes(not TSettings.ShowAxes);
end;

procedure TPuzzle3dFrame.miWiredClick(Sender: TObject);
begin
  MainForm.SetWired(not TSettings.Wired);
end;

procedure TPuzzle3dFrame.miShowNormalsClick(Sender: TObject);
begin
  MainForm.SetShowNormals(not TSettings.ShowNormals);
end;

procedure TPuzzle3dFrame.miContourModeClick(Sender: TObject);
begin
  MainForm.SetContourMode(not TSettings.ContourMode);
end;

procedure TPuzzle3dFrame.miLibraryNextClick(Sender: TObject);
begin
  MainForm.aNextExecute(Sender);
end;

procedure TPuzzle3dFrame.miLibraryPrevClick(Sender: TObject);
begin
  MainForm.aPreviousExecute(Sender);
end;

procedure TPuzzle3dFrame.miLibraryRandomClick(Sender: TObject);
begin
  MainForm.aRandomExecute(Sender);
end;

end.

