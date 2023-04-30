{************************************************************}
{                                                            }
{  Unit uPuzzle                                              }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uPuzzle;
{$mode delphi}

interface
uses uSolvedState, uMatrix, uPart, uAxis, uMacro, uSelection, uPuzzleBase, uTurn,
     DOM,
     Classes, Generics.Collections;

type
  TTurningState = (tsFree, tsMidTurn, tsTurningAnimated, tsWholeRotating);

type
  TTurningType = (ttManual, ttScramble, ttUndo, ttRedo, ttMacro);

type
  TCheckSolvedProc = (
    cspEachColorHasSameDirection,                    // default
    cspEachColorHasSameOrOppositeDirection,
    cspCountUniquePlanes,                            // Bump
    cspEachFaceConsistsOfDifferentColors,            // Sudoku
    cspEachRowColDiagConsistsOfDifferentColors,      // Sudoku 4
    cspCheckSavedStates,
    cspEachPlaneHasOneColor,                         // Bubbles
    cspEachColorHasSameDirectionOrCountUniquePlanes, // two solutions
    cspAdjoiningFacesHaveSameColors,                 // Tetravex
    cspEachRowColConsistsOfDifferentColors           // Sudoku IV
    );

type
  TTurnEvent = procedure(AType: TTurningType; AState: TTurningState) of object;
//  TLoadFromXmlEvent = procedure(XmlNode: IXmlNode) of object;
  TLoadFromStreamEvent = procedure(Stream: TStream) of object;
  TTransLayersEvent = procedure(mat: T4x4Matrix; AxisNo: integer; Layers: array of Boolean) of object;

type

  { TPuzzle }

  TPuzzle = class (TPuzzleBase)
  private
    FCheckSolvedProc: TCheckSolvedProc;
    FMarks: TList<Integer>; // Marks stack - todo переместить в MainForm?
    FSolvedStates: TSolvedStates;
    FMovingParts: TMovingParts;

    FUndoRecordEnabled: Boolean;
    FCheckSliceEnabled: Boolean;
    FCheckFixedLayersEnabled: Boolean;
    FAnimationEnabled: Boolean;

//    FPositions: array of array of TPartPosition;
//    FPositionsValid: Boolean;
    // Selection
    //FPartLayerFrom, FPartLayerTo: array of integer; // for selected axis holds parts position
//    FPartPositions: array of array of TPartPosition;

    function GetUndoCount: integer;
    procedure AddUndo(AxisNo: integer; Layers: array of Boolean; Angle: extended);
    procedure ClearRedoStack;
    procedure ClearUndoStack;
    function LayerTurnable(AxisNo: integer; Layers: array of Boolean; RotateDirection: integer): Boolean;
    function GetMovingParts(AxisNo: integer; const Layers: array of Boolean): Boolean;
  private
  // Events
    FOnBeforeTurn: TTurnEvent;
    FOnTurning: TTurnEvent;
    FOnAfterTurn: TTurnEvent;
    FOnTransLayers: TTransLayersEvent;

    procedure AddMovingLayer(AxisNo: integer; LayerNo: integer; var Layers: array of Boolean);
    function GetMovesCounter: integer;
    procedure SetMovesCounter(AValue: integer);
  protected
    procedure TransSelection(mat: T4x4Matrix);
    procedure RotateWOCheckWOAnimation(AxisNo: integer; Layers: array of Boolean; Al: extended); virtual;
    procedure GetPartsBetweenLayers(AParts: TList<Integer>; AxisNo: integer; iFrom, iTo: integer);
    function IsPlaneBlocked(AxisNo: integer; PlaneNo: integer; RotateDirection: integer): Boolean; virtual;
    procedure AddAngles(AxisNo: integer; const Layers: array of Boolean; Angle: extended);
    function IsTurnWithAxes(AxisNo: integer; const Layers: array of Boolean): Boolean;

  public
    Macros: TList<TMacro>;
    LibraryId: integer; // for puzzles from library
    TurningState: TTurningState;
    TimerValue: integer;
    MovesStartingPoint: integer;
    UndoStack: TList<TTurn>;
    RedoStack: TList<TTurn>;

    property UndoCount: integer read GetUndoCount;
    property MovesCounter: integer read GetMovesCounter write SetMovesCounter;
    property Marks: TList<Integer> read FMarks;
    property SolvedStates: TSolvedStates read FSolvedStates;
    property CheckSolvedProc: TCheckSolvedProc read FCheckSolvedProc write FCheckSolvedProc;

    procedure TurnWithAnimation(AngleToTurn: extended);
    function CalcMovingLayers(AxisNo: integer; Layers: array of Boolean): Boolean; overload;
    function CalcMovingLayers(AxisNo: integer; LayerNo: integer; LayersCount: integer = 1): Boolean; overload;
    function CalcMovingLayersFromPart(AxisNo: integer; Part: TPart): Boolean;
//    procedure CalcPartPositionsForAxis(AxisNo: integer);
//    procedure CalcPartPositions;
//    procedure GetPartPosition(AxisNo, PartNo: integer; var iFrom, iTo: integer); virtual;
//    procedure GetPartPosition1(AxisNo: integer; Part: TPart; var iFrom, iTo: integer); virtual;
    function GetTurningAngle(AxisNo: integer; Layers: array of Boolean; Al, AngleMin, AngleMax: extended): extended; virtual;
    procedure TransLayers(mat: T4x4Matrix; AxisNo: integer; const Layers: array of Boolean);
    function IsTurningWhole(const Layers: array of Boolean): Boolean;
    procedure GetRotateAngleRange(AxisNo: integer; const Layers: array of Boolean; var AngleMin, AngleMax: extended);

    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearHeader; override;
    procedure ClearState;
    function IsTurning: Boolean;

    procedure AfterLoadEvent; virtual;
    procedure EndTurn; virtual;
    procedure Scramble; virtual;
    function Undo(WithAnimation: Boolean): boolean; virtual;
    function Redo(WithAnimation: Boolean): boolean; virtual;
    procedure PartialRedo(Partial: extended); // for gif-export

    procedure RotateLayer(AxisNo, LayerNo: integer; Al: extended; AllowWhole: Boolean = False);
    procedure RotateLayers(AxisNo: integer; Layers: array of Boolean; Al: extended; AllowWhole: Boolean = False); virtual;
    procedure MacroRotateLayers(AxisNo: integer; Layers: array of Boolean; Al: extended);
    procedure AddMark;
    procedure CheckMarks;
    procedure UndoToMark;

  public
    procedure BeforeTurnEvent(AType: TTurningType; AxisNo: integer; var Layers: array of Boolean; var AngleMin, AngleMax: extended; RotateDirection: integer); virtual;
    procedure TurningEvent(AType: TTurningType); virtual;
    procedure AfterTurnEvent(AType: TTurningType); virtual;

    procedure BeforeLoadFromXmlEvent(XmlNode: TDOMNode); virtual;
    procedure AfterLoadFromXmlEvent(XmlNode: TDOMNode); virtual;
    procedure AfterLoadFromStreamEvent(Stream: TStream); virtual;
    procedure AfterSaveToStreamEvent(Stream: TStream); virtual;

    procedure DisableAnimation;
    procedure DisableCheckFixedLayers;
    procedure DisableCheckSlice;
    procedure DisableUndoRecord;
    procedure EnableAnimation;
    procedure EnableCheckFixedLayers;
    procedure EnableCheckSlice;
    procedure EnableUndoRecord;
  public
//    property OnBeforeLoadFromXml: TTurnEvent write FOnBeforeTurn;
//    property OnAfterLoadFromXml: TTurnEvent write FOnBeforeTurn;
//    property OnAfterLoadFromStream: TTurnEvent write FOnBeforeTurn;
//    property OnAfterSaveToStream: TTurnEvent write FOnBeforeTurn;

    property OnBeforeTurn: TTurnEvent read FOnBeforeTurn write FOnBeforeTurn;
    property OnTurning: TTurnEvent read FOnTurning write FOnTurning;
    property OnAfterTurn: TTurnEvent read FOnAfterTurn write FOnAfterTurn;
    property OnTransLayers: TTransLayersEvent write FOnTransLayers;
  end;

// TODO добавить: OnPuzzleChanged(pcPartDeleted, pcPartsCombined)

implementation
uses
  uUtils, uLayer, uLibrary,
  Math, SysUtils;

const DELTA = 1e-3;
const DELTA_LINEAR = 1e-3;
const DELTA_ANGLE  = 1e-5;

constructor TPuzzle.Create;
begin
  inherited;

  FSolvedStates := TSolvedStates.Create;
//  FPositionHolder := TPositionHolder.Create(Self);

  UndoStack := TList<TTurn>.Create;
  RedoStack := TList<TTurn>.Create;
  FMarks := TList<Integer>.Create;
  FMovingParts := TMovingParts.Create;

  Macros := TList<TMacro>.Create;

  TSelection.Clear;

  FUndoRecordEnabled := True;
  FCheckSliceEnabled := True;
  FCheckFixedLayersEnabled := True;
  FAnimationEnabled := True;

//  FPositionsValid := False;
end; // Create

destructor TPuzzle.Destroy;
begin
  Clear;

  Macros.Free;
  FMovingParts.Free;
  FMarks.Free;
  RedoStack.Free;
  UndoStack.Free;

//  FPositionHolder.Free;
//  FPartPositions := nil;

  FSolvedStates.Free;

//  SetLength(FPositions, 0, 0);
//  FPositions := nil;

  inherited;
end; // Destroy

//==============================================================================
// Clear
//==============================================================================

procedure TPuzzle.ClearHeader;
begin
  inherited;

  TurningState := tsFree;
  FCheckSolvedProc := cspEachColorHasSameDirection;
end; // ClearHeader

procedure TPuzzle.Clear;
var i: Integer;
begin
  inherited;

  TimerValue := 0;
  MovesStartingPoint := 0;

  for i := 0 to Macros.Count - 1 do
    Macros[i].Free;
  Macros.Clear;

  FMovingParts.Clear;
  FMarks.Clear;

  ClearUndoStack;
  ClearRedoStack;

  FSolvedStates.Clear;
end; // Clear

procedure TPuzzle.ClearUndoStack;
var Turn: TTurn;
begin
  for Turn in UndoStack do
    Turn.Free;
  UndoStack.Clear;
end;

procedure TPuzzle.ClearRedoStack;
var Turn: TTurn;
begin
  for Turn in RedoStack do
    Turn.Free;
  RedoStack.Clear;
end;

procedure TPuzzle.ClearState;
begin
  ClearUndoStack;
  ClearRedoStack;
//  FPositionHolder.Invalidate;
//  FPositionsValid := False;
end; // ClearState


//==============================================================================
// Setters
//==============================================================================

procedure TPuzzle.DisableUndoRecord;
begin
  FUndoRecordEnabled := False;
end;
procedure TPuzzle.EnableUndoRecord;
begin
  FUndoRecordEnabled := True;
end;

procedure TPuzzle.DisableCheckSlice;
begin
  FCheckSliceEnabled := False;
end;
procedure TPuzzle.EnableCheckSlice;
begin
  FCheckSliceEnabled := True;
end;

procedure TPuzzle.DisableCheckFixedLayers;
begin
  FCheckFixedLayersEnabled := False;
end;
procedure TPuzzle.EnableCheckFixedLayers;
begin
  FCheckFixedLayersEnabled := True;
end;

procedure TPuzzle.DisableAnimation;
begin
  FAnimationEnabled := False;
end;
procedure TPuzzle.EnableAnimation;
begin
  FAnimationEnabled := True;
end;

//procedure TPuzzle.StartTurn();
//begin
//  FState := False;
//end;
//procedure TPuzzle.FinishTurn;
//begin
//  FAnimationEnabled := True;
//end;

//==============================================================================
// Events
//==============================================================================

procedure TPuzzle.BeforeTurnEvent(AType: TTurningType; AxisNo: integer;
  var Layers: array of Boolean; var AngleMin, AngleMax: extended;
  RotateDirection: integer);
begin
//  Log('BeforeTurnEvent');
end;

procedure TPuzzle.TurningEvent(AType: TTurningType);
begin
//  Log('TurningEvent');
end;

procedure TPuzzle.AfterTurnEvent(AType: TTurningType);
begin
  PositionsInvalidate;
//  CalcPartPositions;
//  Log('AfterTurnEvent');
end;

procedure TPuzzle.BeforeLoadFromXmlEvent(XmlNode: TDomNode);
begin
//  Log('BeforeLoadFromXmlEvent');
end;

procedure TPuzzle.AfterLoadFromXmlEvent(XmlNode: TDomNode);
begin
//  Log('AfterLoadFromXmlEvent');
end;

procedure TPuzzle.AfterLoadFromStreamEvent(Stream: TStream);
begin
//  Log('AfterLoadFromStreamEvent');
end;

procedure TPuzzle.AfterLoadEvent;
begin
  //  Log('AfterLoadEvent');
//  FPositionHolder.Invalidate;
//  FPositionsValid := False;
  CalcPartPositions;
end;

procedure TPuzzle.AfterSaveToStreamEvent(Stream: TStream);
begin
//  Log('AfterSaveToStreamEvent');
end;

//==============================================================================
// Transformations
//==============================================================================

// rotate all selected cubies without any check
procedure TPuzzle.TransSelection(mat: T4x4Matrix);
begin // TransLayersWOPlanes
  // Axes
  if FMovingParts.TurnWithAxes then
    TransAxes(mat, FAxes);

  // Parts
  TransParts(mat, FMovingParts.Parts);
end; // TransSelection

// returns true if axes-system must be rotated too
function TPuzzle.IsTurnWithAxes(AxisNo: integer; const Layers: array of Boolean): Boolean;
var BaseLayer, BasePartNo, i, hFrom, hTo: integer;
    Axis: TAxis;
begin
  Axis := FAxes[AxisNo];
  BaseLayer := Axis.TurnAxesWithLayer;
  BasePartNo := Axis.TurnAxesWithPartNo;

  if (BaseLayer <> -1) and Layers[BaseLayer] then
    Exit(True);

  if BasePartNo <> -1 then begin
    GetPartPosition(AxisNo, BasePartNo, hFrom, hTo);
    for i := hFrom to hTo do
      if Layers[i] then
        Exit(True);
  end;

  Result := False;
end; // IsTurnWithAxes

//for layers' puzzles

procedure TPuzzle.TransLayers(mat: T4x4Matrix; AxisNo: integer; const Layers: array of Boolean);
begin
  GetMovingParts(AxisNo, Layers); // TODO direction
  FMovingParts.TurnWithAxes := IsTurnWithAxes(AxisNo, Layers);

  TransSelection(mat);

  if Assigned(FOnTransLayers) then
    FOnTransLayers(mat, AxisNo, Layers);
end; // TransLayers

procedure TPuzzle.MacroRotateLayers(AxisNo: integer; Layers: array of Boolean; Al: extended);
var //i: integer;
    Axis: TAxis;
    AngleMin, AngleMax: extended;
    mat: T4x4Matrix;
begin
  GetRotateAngleRange(AxisNo, Layers, AngleMin, AngleMax);

  BeforeTurnEvent(ttMacro, AxisNo, Layers, AngleMin, AngleMax, Sign(Al)); // Event
  if Assigned(FOnBeforeTurn) then
    FOnBeforeTurn(ttMacro, tsFree);

  Axis := FAxes[AxisNo];

  //  if FCheckFixedLayersEnabled then
  if Axis.IsLayerFixed(Layers) then
    exit;

  GetRotateAngleRange(AxisNo, Layers, AngleMin, AngleMax); // второй раз?
  Al := GetTurningAngle(AxisNo, Layers, Al, AngleMin, AngleMax);

  mat := Axis.GetRotateMatrix(Al);

//    RotateWOCheckWOAnimation(AxisNo, Layers, Al);
//  TransSelection(mat);
  GetMovingParts(AxisNo, Layers); // TODO direction
  //FMovingParts.TurnWithAxes := IsTurnWithAxes(AxisNo, Layers);

  // Axes
  if IsTurnWithAxes(AxisNo, Layers) then
    TransAxes(mat, FAxes);

  // Parts
  TransParts(mat, FMovingParts.Parts);

  if Assigned(FOnTransLayers) then
    FOnTransLayers(mat, AxisNo, Layers);

  AddAngles(AxisNo, Layers, Al);

  if FUndoRecordEnabled then
    AddUndo(AxisNo, Layers, Al);

  ClearRedoStack;

  AfterTurnEvent(ttMacro); // Event
  if Assigned(FOnAfterTurn) then
    FOnAfterTurn(ttMacro, tsFree);


  //  Axis := Axes[AxisNo];
  // если в макросе написано Angle = Pi/2 * random(3), то надо привести его в соответствие с max и min.
//  Al := GetTurningAngle(AxisNo, LayerFrom, LayerTo, Al);
//  for i := 0 to High(Layers) do
//    if Layers[i] then
//      with Axis.Layers[i] do
//  //      if (Al + Angle < MinAngle - DELTA_ANGLE) or (Al + Angle > MaxAngle + DELTA_ANGLE) then
//        if (Al < MinAngle - DELTA_ANGLE) or (Al > MaxAngle + DELTA_ANGLE) then
//          exit;

end; // MacroRotateLayers

procedure TPuzzle.RotateLayer(AxisNo, LayerNo: integer; Al: extended; AllowWhole: Boolean = False);
var Layers: array of Boolean;
    i: integer;
begin
  SetLength(Layers, FAxes[AxisNo].Layers.Count);
  for i := 0 to FAxes[AxisNo].Layers.Count - 1 do
    Layers[i] := False;
  Layers[LayerNo] := True;

  RotateLayers(AxisNo, Layers, Al, AllowWhole);
end; // RotateLayer

procedure TPuzzle.RotateLayers(AxisNo: integer; Layers: array of Boolean; Al: extended; AllowWhole: Boolean = False);
begin
//  Log('RotateLayers start');
  if FCheckSliceEnabled then
    if not GetMovingParts(AxisNo, Layers{, Sign(Al)}) then // дублируется при вращении мышью
      if not AllowWhole then
        exit;

  if FCheckFixedLayersEnabled then
    if FAxes[AxisNo].IsLayerFixed(Layers) then
      exit;

  RotateWOCheckWOAnimation(AxisNo, Layers, Al);

  if FUndoRecordEnabled then
    AddUndo(AxisNo, Layers, Al);

  ClearRedoStack;
//  Log('RotateLayers finish');
end; // RotateLayers

procedure TPuzzle.RotateWOCheckWOAnimation(AxisNo: integer; Layers: array of Boolean; Al: extended);
begin
  GetMovingParts(AxisNo, Layers); // TODO direction

  FMovingParts.TurnWithAxes := IsTurnWithAxes(AxisNo, Layers);

  TransSelection(FAxes[AxisNo].GetRotateMatrix(Al));

  AddAngles(AxisNo, Layers, Al);
end; // RotateWOCheckWOAnimation

procedure TPuzzle.AddAngles(AxisNo: integer; const Layers: array of Boolean; Angle: extended);
var i: integer;
    Axis: TAxis;
begin
  Axis := FAxes[AxisNo];
  if not IsTurnWithAxes(AxisNo, Layers) then begin
    for i := 0 to High(Layers) do
      if Layers[i] then
        Axis.Layers[i].AddAngle(Angle)
  end else
    for i := 0 to High(Layers) do
      if not Layers[i] then
        Axis.Layers[i].AddAngle(-Angle);
end; // AddAngles

procedure TPuzzle.TurnWithAnimation(AngleToTurn: extended);
const
  delta = 1e-6;
var
  TicksOld, TicksNew: Cardinal;
//  nx, ny, nz: extended;
  Diff: extended;
begin // TurnWithAnimation
  if TurningState = tsTurningAnimated then
    exit;

//  with Axes[TurnAxisNo].NormVector do begin
//    nx := X;
//    ny := Y;
//    nz := Z;
//  end;

  TicksNew := GetTickCount;
  TicksOld := TicksNew;
  TurningState := tsTurningAnimated;
  try
    while Abs(AngleToTurn) > delta do begin
      Diff := Sign(AngleToTurn) * integer(TicksNew - TicksOld) / 200;
      if Abs(AngleToTurn) < Abs(diff) then
        diff := AngleToTurn;

      TransLayers(FAxes[TSelection.TurnAxisNo].GetRotateMatrix(Diff), TSelection.TurnAxisNo, TSelection.TurnLayers);

      AngleToTurn := AngleToTurn - diff;

      TurningEvent(ttManual); // Event
      if Assigned(FOnTurning) then
        FOnTurning(ttManual, tsTurningAnimated);

      TicksOld := TicksNew;
      TicksNew := GetTickCount;
    end;
  finally
    TurningState := tsFree;
  end;
end; // TurnWithAnimation

//for layers' puzzles

procedure TPuzzle.EndTurn;
const delta2 = 1e-5;
begin
  if Abs(TSelection.TurningAngle) > delta2 then begin
    AddAngles(TSelection.TurnAxisNo, TSelection.TurnLayers, TSelection.TurningAngle);

    if FUndoRecordEnabled then
      AddUndo(TSelection.TurnAxisNo, TSelection.TurnLayers, TSelection.TurningAngle);
    ClearRedoStack;
  end;

//  FPositionHolder.Invalidate;
//  FPositionsValid := False;

  TSelection.TurningAngle := 0;
  TurningState := tsFree;

  if Assigned(FOnAfterTurn) then
    FOnAfterTurn(ttManual, tsTurningAnimated);
  AfterTurnEvent(ttManual);
end; // EndTurn

//==============================================================================
// utils
//==============================================================================

// place Moving Parts to FMovingParts
function TPuzzle.GetMovingParts(AxisNo: integer; const Layers: array of Boolean): Boolean;
var
  pFrom, pTo: integer;
  i, j: integer;
begin
  FMovingParts.Clear;

  if FAxes[AxisNo].Enabled then begin
    FMovingParts.AxisNo := AxisNo;

    for i := 0 to FParts.Count - 1 do begin
      GetPartPosition(AxisNo, i, pFrom, pTo);
      for j := pFrom to pTo do
        if Layers[j] then begin
           FMovingParts.AddPart(FParts[i]);
           continue;
        end;
    end;
  end;

  Result := (FMovingParts.Parts.Count > 0) and (FMovingParts.Parts.Count < FParts.Count);
end; // GetMovingParts

// Calculates all linked cubies to move
//procedure TPuzzle.CalculateMovingParts(Part: TPart; AxisNo: integer; Direction: integer);
//var iFrom, iTo: integer;
//begin
//  GetPartPosition(AxisNo, Part, iFrom, iTo);
////  CalculateMovingPartsL(AxisNo, iFrom, iTo);
//  FMovingParts.TurnWithAxes := TurnWithAxes(AxisNo, iFrom, iTo);
//end; // CalculateMovingParts

// for layers' puzzle

// get list of all cubies between planes
procedure TPuzzle.GetPartsBetweenLayers(AParts: TList<Integer>; AxisNo: integer; iFrom, iTo: integer);
var
  hFrom, hTo: integer;
  i: integer;
begin
  if (AxisNo < 0) or (AxisNo >= FAxes.Count) then
    exit;

  for i := 0 to FParts.Count - 1 do begin
    GetPartPosition(AxisNo, i, hFrom, hTo);
    if (hFrom >= iFrom) and (hTo <= iTo) then
      AParts.Add(i);
  end;
end; // GetPartsBetweenLayers

//procedure TPuzzle.GetPartPosition(AxisNo, PartNo: integer; var iFrom, iTo: integer);
//var PartPosition: TPartPosition;
//begin
////GetPartPosition1(AxisNo, Part, iFrom, iTo);
//  PartPosition := FPartPositions[AxisNo][PartNo];
//  iFrom := PartPosition.iFrom;
//  iTo := PartPosition.iTo;
//end; // GetPartPosition

//// procedure returns the layers list to which the cubie belongs
//// for base TPuzzle class it's assumed that layers are flat
//procedure TPuzzle.GetPartPosition1(AxisNo: integer; Part: TPart; var iFrom, iTo: integer);
//var
//  AMin, AMax: extended;
//  Axis: TAxis;
//
//  // this code only for flat layers
//  function GetProjectionBoundaries: Boolean;
//  var
//    d: extended;
//    NormVect, Vector: TVector;
//  begin
//    Result := False;
//    NormVect := Axis.NormVector;
//
//    for Vector in Part.Vertices do begin
//      d := Vector.ScalarMult(NormVect);
//      if not Result then begin
//        AMin := d;
//        AMax := d;
//        Result := True;
//      end else begin
//        if d < AMin then
//          AMin := d;
//        if d > AMax then
//          AMax := d;
//      end;
//    end;
//  end; // GetProjectionBoundary
//
//var
//  i: integer;
//const DELTA = 1e-4;
//begin // GetPartPosition
//  Axis := FAxes[AxisNo];
//
//  GetProjectionBoundaries;
//
//  iFrom := 0;
//  for i := 1 to Axis.Layers.Count - 1 do
//    if Axis.Layers[i].DistanceFrom < AMin + DELTA then
//      iFrom := i
//    else
//      break;
//
//  iTo := Axis.Layers.Count - 1;
//  for i := Axis.Layers.Count - 2 downto 0 do
//    if Axis.Layers[i].DistanceTo > AMax - DELTA then
//      iTo := i
//    else
//      break;
//
////TUtils.Log('GetPartPosition iFrom = ' + IntToStr(iFrom) + ' iTo = ' + IntToStr(iTo));
//end; // GetPartPosition

//procedure TPuzzle.GetPlanesFromFaces(Parts: TList<TPart>; Planes: TList<TPlane>);
//
//  function SameOrOppositePlaneExists(pl: TPlane): Boolean;
//  var i: integer;
//  begin
//    Result := True;
//    for i := 0 to Planes.Count - 1 do
//      if Planes[i].IsNear(pl) or Planes[i].IsOpposite(pl) then
//        exit;
//    Result := False;
//  end; // SameOrOppositePlaneExists
//
//var
//  i, j: integer;
//  pl: TPlane;
//  Face: TFace;
//begin // GetPlanesFromFaces
//  for i := 0 to Parts.Count - 1 do
//    for j := 0 to Parts[i].Faces.Count - 1 do begin
//      Face := Parts[i].Faces[j];
//      pl := TPlane.Create;
//      Face.GetPlane(pl);
//      if not SameOrOppositePlaneExists(pl) then
//        Planes.Add(pl)
//      else
//        pl.Free;
//    end;
//end; // GetPlanesFromFaces

//==============================================================================
// Turning parameters
//==============================================================================

//function TPuzzle.IsPlaneBlocked(Plane: TPlane; RotateDirection: integer): Boolean;
//var i: integer;
//begin
//  Result := True;
//  for i := 0 to FParts.Count - 1 do
//    if FParts[i].Visible and (FParts[i].HalfPlane(Plane) = 0) then
//      exit;
//  Result := False;
//end;

// for layers' puzzle

function TPuzzle.IsPlaneBlocked(AxisNo: integer; PlaneNo: integer; RotateDirection: integer): Boolean;
var iFrom, iTo, i: integer;
begin
  if not FAxes[AxisNo].Enabled then
    exit(True);

  for i := 0 to FParts.Count - 1 do
    if FParts[i].Visible then begin
      GetPartPosition(AxisNo, i, iFrom, iTo);
      if (iFrom < PlaneNo + 1) and (iTo > PlaneNo) then
        exit(True);
    end;
  Result := False;
end; // IsPlaneBlocked

// for layers' puzzle

function TPuzzle.GetTurningAngle(AxisNo: integer; Layers: array of Boolean; Al, AngleMin, AngleMax: extended): extended;
var
  Axis: TAxis;
  iFirst: integer;

  function AngleValid(Angle: extended): Boolean;
  var
    i: integer;
    Layer: TLayer;
  begin
    Result := False;

    if (Al < AngleMin - DELTA_ANGLE) or (Al > AngleMax + DELTA_ANGLE) then
      exit;

    for i := iFirst{ + 1} to High(Layers) do
      if Layers[i] then begin
        Layer := Axis.Layers[i];
        if not Layer.IsValidAngle(Angle + Layer.Angle) then
          exit;
      end;
    Result := True;
  end; // AngleValid

var
  i: integer;
  FirstLayer: TLayer;
  A, AnglePlus, AngleMinus: extended;
  PlusValid, MinusValid, TurnWithAxes: Boolean;
const PI2 = 2 * Pi;
begin // GetTurningAngle
  TurnWithAxes := IsTurnWithAxes(AxisNo, Layers);
  if TurnWithAxes then begin
    Al := -Al;

    A := AngleMin;
    AngleMin := -AngleMax;
    AngleMax := -A;

    for i := 0 to High(Layers) do
      Layers[i] := not Layers[i];
  end;

  Axis := FAxes[AxisNo];

  AnglePlus := Al;
  AngleMinus := Al;
  FirstLayer := nil;

  // find first movable layer
  for i := 0 to High(Layers) do
    if Layers[i] then begin
      iFirst := i;
      FirstLayer := Axis.Layers[i];
      break;
    end;
  if FirstLayer = nil then begin
    Result := 0;
    //raise Exception.Create('TPuzzle.GetTurningAngle: no turning layers');
    exit;
  end;

  PlusValid := False;
  while not PlusValid do begin
    AnglePlus := FirstLayer.GetNextAngle(FirstLayer.Angle + AnglePlus) - FirstLayer.Angle;
    if (AnglePlus > PI2) or (AnglePlus > AngleMax + DELTA) then
      break;

    PlusValid := AngleValid(AnglePlus);
    if not PlusValid then
      AnglePlus := AnglePlus + 0.01;
  end;

  MinusValid := False;
  while not MinusValid do begin
    AngleMinus := FirstLayer.GetPrevAngle(FirstLayer.Angle + AngleMinus) - FirstLayer.Angle;

    if (AngleMinus < -PI2) or (AngleMinus < AngleMin - DELTA) then
      break;

    MinusValid := AngleValid(AngleMinus);
    if not MinusValid then
      AngleMinus := AngleMinus - 0.01;
  end;

  if PlusValid and MinusValid then begin
    if AnglePlus - Al < Al - AngleMinus then
      Result := AnglePlus
    else
      Result := AngleMinus;
  end else if PlusValid then
    Result := AnglePlus
  else if MinusValid then
    Result := AngleMinus
  else
    Result := 0;
//    raise Exception.Create('TPuzzle.GetTurningAngle: Valid angle not found');

  if TurnWithAxes then
    Result := -Result;
end; // GetTurningAngle

function TPuzzle.GetUndoCount: integer;
begin
  Result := UndoStack.Count;
end; // GetUndoCount

function TPuzzle.GetMovesCounter: integer;
begin
  Result := UndoStack.Count - MovesStartingPoint;
end;

procedure TPuzzle.SetMovesCounter(AValue: integer);
begin
  MovesStartingPoint := UndoStack.Count + AValue;
end;

//procedure TPuzzle.CalcAngles(LayerFrom, LayerTo: integer; var AngleMin, AngleMax: extended);
//var i: integer;
//begin
//  AngleMin := FAxes[FTurnAxisNo].Layers[TSelection.TurnLayerFrom].MinAngle;
//  AngleMax := FAxes[FTurnAxisNo].Layers[TSelection.TurnLayerFrom].MaxAngle;
//  for i := TSelection.TurnLayerFrom + 1 to TSelection.TurnLayerTo do begin
//    if TSelection.AngleMin
//  end;
//end; // CalcAngles

function TPuzzle.IsTurningWhole(const Layers: array of Boolean): Boolean;
var i: integer;
begin
  Result := False;
  for i := 0 to High(Layers) do
    if not Layers[i] then
      exit;
  Result := True;
end; // IsTurningWhole

// private
procedure TPuzzle.AddMovingLayer(AxisNo: integer; LayerNo: integer; var Layers: array of Boolean);
var i, j, k: integer;
    Axis: TAxis;
    iFrom, iTo: integer;
begin
  if TSelection.TurnLayers[LayerNo] then
    exit;

  TSelection.TurnLayers[LayerNo] := True;

  Axis := FAxes[AxisNo];
  if not Axis.Enabled then
    exit;
  if (LayerNo < Axis.Layers.Count - 1) and IsPlaneBlocked(AxisNo, LayerNo, 0) then // Direction ??
    AddMovingLayer(AxisNo, LayerNo + 1, Layers);
  if (LayerNo > 0) and IsPlaneBlocked(AxisNo, LayerNo - 1, 0) then // Direction ??
    AddMovingLayer(AxisNo, LayerNo - 1, Layers);


  for i := 0 to FParts.Count - 1 do begin
    GetPartPosition(AxisNo, i, iFrom, iTo);

    if (LayerNo >= iFrom) and (LayerNo <= iTo) then
      for j := iFrom to iTo do
        AddMovingLayer(AxisNo, j, Layers);
  end;

  // add connected layers
  for i := 0 to Length(Axis.ConnectedLayers) - 1 do
    for j := 0 to Length(Axis.ConnectedLayers[i]) - 1 do
      if Axis.ConnectedLayers[i][j] = LayerNo then begin
        for k := 0 to Length(Axis.ConnectedLayers[i]) - 1 do
          AddMovingLayer(AxisNo, Axis.ConnectedLayers[i][k], Layers);
        break;
      end;
end; // AddMovingLayer

function TPuzzle.CalcMovingLayers(AxisNo: integer; Layers: array of Boolean): Boolean;
var Axis: TAxis;
    i: integer;
begin
  Axis := FAxes[AxisNo];
  if not Axis.Enabled then
    exit(False);
  SetLength(TSelection.TurnLayers, Axis.Layers.Count);
  for i := 0 to High(TSelection.TurnLayers) do
    TSelection.TurnLayers[i] := False;

//  Result := True;
  for i := 0 to High(Layers) do
    if Layers[i] then
      AddMovingLayer(AxisNo, i, TSelection.TurnLayers);

  // if all layers turned then return false
  Result := not IsTurningWhole(TSelection.TurnLayers);
end; // CalcMovingLayers

function TPuzzle.CalcMovingLayers(AxisNo: integer; LayerNo: integer; LayersCount: integer = 1): Boolean;
// returns false if whole puzzle is selected. 'Fixed' layers are ignored
var Axis: TAxis;
    i: integer;
begin // CalcMovingLayers
  Axis := FAxes[AxisNo];

  if not Axis.Enabled then
    exit(False);

  SetLength(TSelection.TurnLayers, Axis.Layers.Count);
  for i := 0 to High(TSelection.TurnLayers) do
    TSelection.TurnLayers[i] := False;

//  Result := True;
  for i := 0 to LayersCount - 1 do
    AddMovingLayer(AxisNo, LayerNo + i, TSelection.TurnLayers);

  // if all layers turned then return false
  Result := not IsTurningWhole(TSelection.TurnLayers);
end; // CalcMovingLayers

function TPuzzle.CalcMovingLayersFromPart(AxisNo: integer; Part: TPart): Boolean;
var iFrom, iTo: integer;
begin
  GetPartPosition(AxisNo, FParts.IndexOf(Part), iFrom, iTo);
  Result := CalcMovingLayers(AxisNo, iFrom, iTo - iFrom + 1);
end; // CalcMovingLayersFromPart

procedure TPuzzle.GetRotateAngleRange(AxisNo: integer; const Layers: array of Boolean; var AngleMin, AngleMax: extended);
var Axis: TAxis;
  i: integer;
  Layer: TLayer;
begin
  AngleMin := -1000000;
  AngleMax := 1000000;

  if IsTurningWhole(Layers) then
    exit;

  Axis := Faxes[AxisNo];

  if not Axis.Enabled then
    exit;

  if not IsTurnWithAxes(AxisNo, Layers) then begin
    for i := 0 to High(Layers) do
      if Layers[i] then begin
        Layer := Axis.Layers[i];
        AngleMin := Max(AngleMin, Layer.MinAngle - Layer.Angle);
        AngleMax := Min(AngleMax, Layer.MaxAngle - Layer.Angle);
      end;
  end else begin
    for i := 0 to High(Layers) do
      if not Layers[i] then begin
        Layer := Axis.Layers[i];
        AngleMin := Max(AngleMin, -(Layer.MaxAngle - Layer.Angle));
        AngleMax := Min(AngleMax, -(Layer.MinAngle - Layer.Angle));
      end;
  end;
end; // GetRotateAngleRange

function TPuzzle.LayerTurnable(AxisNo: integer; Layers: array of Boolean; RotateDirection: integer): Boolean;
var
  Layer: TLayer;
  Axis: TAxis;
  i: integer;
begin
  Axis := FAxes[AxisNo];
  Result := False;
  for i := 0 to High(Layers) do
    if Layers[i] then begin
      Layer := Axis.Layers[i];
      if (RotateDirection < 0) and (Layer.Angle < Layer.MinAngle + DELTA) then
        exit;
      if (RotateDirection > 0) and (Layer.Angle > Layer.MaxAngle - DELTA) then
        exit;
      if (RotateDirection = 0) and ((Layer.Angle < Layer.MinAngle + DELTA) or (Layer.Angle > Layer.MaxAngle - DELTA)) then
        exit;
    end;
  Result := True;
end; // LayerTurnable

function TPuzzle.IsTurning: Boolean;
begin
  Result := TurningState in [tsMidTurn, tsTurningAnimated];
end;

// =============================== Undo/Redo ==================================

function TPuzzle.Undo(WithAnimation: Boolean): boolean;
var
  Turn: TTurn;
  Angle: extended;
  i: integer;
//  Parts: TList<TPart>;
begin
  Result := False;
  if IsTurning or (TurningState = tsTurningAnimated) then
    exit;

  if UndoStack.Count = 0 then
    exit;

  Turn := UndoStack[UndoStack.Count - 1];
  TSelection.TurnAxisNo := Turn.AxisNo;
  TSelection.TurnAxis := FAxes[TSelection.TurnAxisNo];

  SetLength(TSelection.TurnLayers, TSelection.TurnAxis.Layers.Count);
  for i := 0 to High(Turn.Layers) do
    TSelection.TurnLayers[i] := Turn.Layers[i];

  Angle := Turn.Angle;

  BeforeTurnEvent(ttUndo, TSelection.TurnAxisNo, TSelection.TurnLayers, TSelection.AngleMin, TSelection.AngleMax, TSelection.RotateDirection);
  if Assigned(FOnBeforeTurn) then
    FOnBeforeTurn(ttUndo, tsFree);

  if not WithAnimation then begin
//    FMovingParts.Clear;
//    FMovingParts.AxisNo := FTurnAxisNo;
//    Parts := TList<TPart>.Create;
//    GetCubiesListBetweenLayers(Parts, FTurnAxisNo, TSelection.TurnLayerFrom, TSelection.TurnLayerTo);
//    FMovingParts.AddParts(Parts);
//    Parts.Free;
    GetMovingParts(TSelection.TurnAxisNo, TSelection.TurnLayers); // TODO direction
    FMovingParts.TurnWithAxes := IsTurnWithAxes(TSelection.TurnAxisNo, TSelection.TurnLayers);

    TransSelection(FAxes[TSelection.TurnAxisNo].GetRotateMatrix(-Angle));

  end else begin
//    FGraphicsEngine.AddExtraPlanes;
//    FGraphicsEngine.ReCalcEx(TurnAxisNo, TurnLayerFrom, TurnLayerTo, TurnWithAxes);
    TurnWithAnimation(TUtils.PrivPi(-Angle));
    TurningState := tsFree;
//    FGraphicsEngine.RemoveExtraPlanes;
  end;
//  FPositionHolder.Invalidate;
//  FPositionsValid := False;

  RedoStack.Add(Turn);
  UndoStack.Delete(UndoStack.Count - 1);
  AddAngles(TSelection.TurnAxisNo, TSelection.TurnLayers, -Angle);

  //FGraphicsEngine.ReCalc;
  AfterTurnEvent(ttUndo);

  Result := True;
end; // Undo

function TPuzzle.Redo(WithAnimation: Boolean): boolean;
var
  Turn: TTurn;
  Angle: extended;
  i: integer;
//  Parts: TList<TPart>;
begin
  Result := False;
  if IsTurning or (TurningState = tsTurningAnimated) then
    exit;

  if RedoStack.Count = 0 then
    exit;

  Turn := RedoStack[RedoStack.Count - 1];
  TSelection.TurnAxisNo := Turn.AxisNo;
  if TSelection.TurnAxisNo = -1 then
    exit;

  TSelection.TurnAxis := FAxes[TSelection.TurnAxisNo];

  SetLength(TSelection.TurnLayers, TSelection.TurnAxis.Layers.Count);
  for i := 0 to High(TSelection.TurnLayers) do
    TSelection.TurnLayers[i] := Turn.Layers[i];

  Angle := Turn.Angle;

  BeforeTurnEvent(ttRedo, TSelection.TurnAxisNo, TSelection.TurnLayers, TSelection.AngleMin, TSelection.AngleMax, TSelection.RotateDirection);
  if Assigned(FOnBeforeTurn) then
    FOnBeforeTurn(ttRedo, tsFree);

  if not WithAnimation then begin
//    FMovingParts.Clear;
//    FMovingParts.AxisNo := TSelection.TurnAxisNo;
//    Parts := TList<TPart>.Create;
//    GetCubiesListBetweenLayers(Parts, TSelection.TurnAxisNo, TSelection.TurnLayerFrom, TSelection.TurnLayerTo);
//    FMovingParts.AddParts(Parts);
//    Parts.Free;
    GetMovingParts(TSelection.TurnAxisNo, TSelection.TurnLayers{, 0}); // TODO direction
    FMovingParts.TurnWithAxes := IsTurnWithAxes(TSelection.TurnAxisNo, TSelection.TurnLayers);

    TransSelection(FAxes[TSelection.TurnAxisNo].GetRotateMatrix(Angle));
  end else begin
    //FGraphicsEngine.AddExtraPlanes;
    //FGraphicsEngine.ReCalcEx(TurnAxisNo, TurnLayerFrom, TurnLayerTo, TurnWithAxes);
    //FGraphicsEngine.ReCalc;
    TurnWithAnimation(TUtils.PrivPi(Angle));
    TurningState := tsFree;
    //FGraphicsEngine.RemoveExtraPlanes;
  end;
//  FPositionHolder.Invalidate;
//  FPositionsValid := False;

//  RotateLayers(TurnAxisNo, Turning.Angle, TurnLayerFrom, TurnLayerTo);
  UndoStack.Add(Turn);
  RedoStack.Delete(RedoStack.Count - 1);
  AddAngles(TSelection.TurnAxisNo, TSelection.TurnLayers, Angle);

  AfterTurnEvent(ttRedo);
  Result := True;
end; // Redo

// need for gifs saving
procedure TPuzzle.PartialRedo(Partial: extended);
var
  Turn: TTurn;
  Angle: extended;
  i: integer;
//  Parts: TList<TPart>;
begin
  if IsTurning or (TurningState = tsTurningAnimated) then
    exit;

  if RedoStack.Count = 0 then
    exit;

  Turn := RedoStack[RedoStack.Count - 1];
  TSelection.TurnAxisNo := Turn.AxisNo;
  if TSelection.TurnAxisNo <> -1 then
    TSelection.TurnAxis := FAxes[TSelection.TurnAxisNo]
  else
    TSelection.TurnAxis := nil;

  SetLength(TSelection.TurnLayers, TSelection.TurnAxis.Layers.Count);
  for i := 0 to High(TSelection.TurnLayers) do
    TSelection.TurnLayers[i] := Turn.Layers[i];

  Angle := Turn.Angle;

//  FMovingParts.Clear;
//  FMovingParts.AxisNo := TSelection.TurnAxisNo;
//  Parts := TList<TPart>.Create;
//  GetCubiesListBetweenLayers(Parts, TSelection.TurnAxisNo, TSelection.TurnLayerFrom, TSelection.TurnLayerTo);
//  FMovingParts.AddParts(Parts);
//  Parts.Free;
  GetMovingParts(TSelection.TurnAxisNo, TSelection.TurnLayers{, 0}); // TODO direction
  FMovingParts.TurnWithAxes := IsTurnWithAxes(TSelection.TurnAxisNo, TSelection.TurnLayers);

  TransSelection(FAxes[TSelection.TurnAxisNo].GetRotateMatrix(Angle * Partial));
end; // PartialRedo

procedure TPuzzle.AddUndo(AxisNo: integer; Layers: array of Boolean; Angle: extended);
const delta = 1e-4;

//  function Priv2pi(Angle: extended): extended;
//  begin
//    Result := Angle;
//    while Result > 2 * Pi - delta do
//      Result := Result - 2 * Pi;
//    while Result < -2 * Pi + delta do
//      Result := Result + 2 * Pi;
//  end; // Priv2pi

var Turn: TTurn;
  i: Integer;
begin  // AddUndo
  if Abs(Angle) < delta then
    exit;

  Turn := TTurn.Create;
  Turn.typ := ttyLayerTurn;
  Turn.AxisNo := AxisNo;
  SetLength(Turn.Layers, Length(Layers));
  for i := 0 to High(Turn.Layers) do
    Turn.Layers[i] := Layers[i];
  Turn.Angle := Angle;
  UndoStack.Add(Turn);
end; // AddUndo

// ================================== Mark ===================================

procedure TPuzzle.CheckMarks;
var Current: integer;
begin
  Current := UndoStack.Count;
  while (FMarks.Count > 0) and (FMarks[FMarks.Count - 1] >= Current) do
    FMarks.Delete(FMarks.Count - 1);
end; // CheckMarks

procedure TPuzzle.AddMark;
begin
  CheckMarks;
  FMarks.Add(UndoStack.Count);
end;

procedure TPuzzle.UndoToMark;
var Mark: integer;
begin
  CheckMarks;

  if FMarks.Count = 0 then
    exit;

  Mark := FMarks[FMarks.Count - 1];
  while UndoStack.Count > Mark do
    Undo(False);
end; // UndoToMark

// ============================================================

procedure TPuzzle.Scramble;
var
  i, AxisNo, LayerNo: integer;
  Angle: extended;
  Axis: TAxis;
begin
  for i := 0 to 200 do begin
    AxisNo := Random(FAxes.Count);
    TSelection.TurnAxisNo := AxisNo;
    Axis := FAxes[AxisNo];
    LayerNo := Random(Axis.Layers.Count);

//    CalcPartPositionsForAxis(AxisNo);
    if not CalcMovingLayers(AxisNo, LayerNo) then
      continue;
    if Axis.IsLayerFixed(TSelection.TurnLayers) then
      continue;
    if IsTurningWhole(TSelection.TurnLayers) then
      continue;

    Angle := Axis.Layers[LayerNo].TurningAngle;
    Angle := Angle * (Random(Round(2*Pi / Angle) - 1) + 1);

    GetRotateAngleRange(AxisNo, TSelection.TurnLayers, TSelection.AngleMin, TSelection.AngleMax);
    Angle := GetTurningAngle(AxisNo, TSelection.TurnLayers, Angle, TSelection.AngleMin, TSelection.AngleMax);
    if (Angle > TSelection.AngleMax + DELTA) or (Angle < TSelection.AngleMin - DELTA) then
      continue;

    TSelection.RotateDirection := Sign(Angle);

    if LayerTurnable(AxisNo, TSelection.TurnLayers, TSelection.RotateDirection) then begin
      BeforeTurnEvent(ttScramble, AxisNo, TSelection.TurnLayers, TSelection.AngleMin, TSelection.AngleMax, TSelection.RotateDirection);
      if (Angle >= TSelection.AngleMin - DELTA) and (Angle <= TSelection.AngleMax + DELTA) then begin
        if Assigned(FOnBeforeTurn) then
          FOnBeforeTurn(ttScramble, tsFree);

        RotateLayers(AxisNo, TSelection.TurnLayers, Angle);

        AfterTurnEvent(ttScramble);
        if Assigned(FOnAfterTurn) then
          FOnAfterTurn(ttScramble, tsFree);
      end;
    end;
  end;

  for i := 0 to 2000 do begin
    AxisNo := Random(FAxes.Count);
    LayerNo := Random(FAxes[AxisNo].Layers.Count);
    Angle := Pi * (Random * 2 - 1); // (-Pi; Pi)

    TSelection.TurnAxisNo := AxisNo;
    Axis := FAxes[AxisNo];
//    CalcPartPositionsForAxis(AxisNo);
    if not CalcMovingLayers(AxisNo, LayerNo) then
      continue;
    if Axis.IsLayerFixed(TSelection.TurnLayers) then
      continue;
    if IsTurningWhole(TSelection.TurnLayers) then
      continue;

    GetRotateAngleRange(AxisNo, TSelection.TurnLayers, TSelection.AngleMin, TSelection.AngleMax);
    Angle := GetTurningAngle(AxisNo, TSelection.TurnLayers, Angle, TSelection.AngleMin, TSelection.AngleMax);
    if (Angle > TSelection.AngleMax + DELTA) or (Angle < TSelection.AngleMin - DELTA) then
      continue;

    TSelection.RotateDirection := Sign(Angle);

    if LayerTurnable(AxisNo, TSelection.TurnLayers, TSelection.RotateDirection) then begin
      BeforeTurnEvent(ttScramble, AxisNo, TSelection.TurnLayers, TSelection.AngleMin, TSelection.AngleMax, TSelection.RotateDirection);
      if (Angle >= TSelection.AngleMin - DELTA) and (Angle <= TSelection.AngleMax + DELTA) then begin
        if Assigned(FOnBeforeTurn) then
          FOnBeforeTurn(ttScramble, tsFree);

        RotateLayers(AxisNo, TSelection.TurnLayers, Angle);

        AfterTurnEvent(ttScramble);
        if Assigned(FOnAfterTurn) then
          FOnAfterTurn(ttScramble, tsFree);
      end;
    end;
  end;
end; // Scramble

//procedure TPuzzle.HighlightLayers(AxisNo, iFrom, iTo: integer);
//begin
//  GetCubiesListBetweenLayers(TSelection.HighlightedParts, AxisNo, iFrom, iTo);
//end;

//function TPuzzle.GetIntFromScript(VarName: String): integer;
//var V: TVar;
//begin
//  if Calc.VarByName(VarName, V) then
//    Result := V.Value
//  else begin
//    ShowMessage('Variable ' + VarName + ' not found');
//    Result := 0;
//  end;
//end; // Init

//procedure TPuzzle.ExecuteMacro(index: integer);
//var
//  AxisDirection: Boolean;
//  TurnAxisNo: integer;
//
//  procedure FindNearestAxis(Axis: TAxis);
//  var i: integer;
//     V1, V2: TVector;
//     dRes, d: extended;
//  begin
//    v1.Assign(Axis.NormVector);
//    TurnAxisNo := 0;
//    AxisDirection := True;
//    dRes := 0;
//    for i := 0 to FAxes.Count - 1 do begin
//      v2.Assign(FAxes[i].NormVector);
//      d := v2.ScalarMult(v1);
//      if d > dRes then begin
//        dRes := d;
//        AxisDirection := True;
//        TurnAxisNo := i;
//      end else if -d > dRes then begin
//        dRes := -d;
//        AxisDirection := False;
//        TurnAxisNo := i;
//      end;
//    end;
//  end; // FindNearestAxis
//
//var
//  Macro: TMacro;
//  i: integer;
////  Turn: TMacroTurn;
//begin // ExecuteMacro
//  if IsTurning or (TurningState = tsTurningAnimated) then
//    exit;
//
//  Macro := Macros[index];
//
//  if Macro.MacroString <> '' then
//    ExecuteMacroString(Macro.MacroString)
//  else
//    for i := 0 to Macro.Turns.Count - 1 do begin
//      Turn := Macro.Turns[i];
//      FindNearestAxis(Turn.Axis);
//      if AxisDirection then
//        RotateLayers(TurnAxisNo, Turn.LayerFrom, Turn.LayerTo, Turn.Angle, True)
//      else
//        RotateLayers(TurnAxisNo, FAxes[TurnAxisNo].Layers.Count - 1 - Turn.LayerFrom, FAxes[TurnAxisNo].Layers.Count - 1 - Turn.LayerTo, -Turn.Angle, True);
//
////      TurnAxisNo := FindNearestAxis(Macro.Turns[i].Axis);
////      TSelection.TurnAxis := Axes[TurnAxisNo];
////      TurnLayerFrom := Macro.Turns[i].LayerFrom;
////      TurnLayerTo := Macro.Turns[i].LayerTo;
////      TurningAngle := Macro.Turns[i].Angle;
////
////      if True{not WithAnimation} then
////        TransSelection(Axes[TurnAxisNo].GetRotateMatrix(TurningAngle));
////      else begin
////  //      AddExtraPlanes;
////  //      FGraphicEngine.ReCalcEx(TurnAxisNo, TurnLayerFrom, TurnLayerTo, TurnWithAxes);
////  //      TurnWithAnimation(PrivPi(-Turning.Angle));
////  //      TurningState := tsFree;
////  //      RemoveExtraPlanes;
////      end;
//
//    //    for j := TurnLayerFrom to TurnLayerTo do
//    //      Axes[TurnAxisNo].Angles[j] := Axes[TurnAxisNo].Angles[j] - Turning.Angle;
//
//  //    lRedo.Add(Turning);
//  //    lUndo.Delete(lUndo.Count - 1);
//    end;
//
//  //FGraphicsEngine.ReCalc;
////MainForm.Log(IntToStr(lUndo.Count) + ' ' + IntToStr(lRedo.Count));
//
//
////    FString: String;
////    FTurns: TMacroTurns;
//end; // ExecuteMacro

//procedure TPuzzle.ExecuteMacroByName(MacroName: String);
//var
//  i: integer;
//  Macro: TMacro;
//begin
//  for i := 0 to Macros.Count - 1 do begin
//    Macro := Macros[i];
//    if Macro.Name = MacroName then begin
//      ExecuteMacro(i);
//      exit;
//    end;
//  end;
//end; // ExecuteMacroByName

//procedure TPuzzle.ExecuteMacroString(Macro: String);
//var S: string;
//begin
//  while Macro <> '' do begin
//    S := Trim(Parse(Macro, ' ' + #9#10#13));
//    if S <> '' then
//      ExecuteMacroByName(S);
//  end;
//end;

//function TPuzzle.CheckSolved: Boolean;
//var CurrentState: TSolvedState;
//  i: integer;
//begin
//  Result := False;
//  CurrentState := TSolvedState.Create;
//  CurrentState.CalcFromPuzzle(Self);
//  for i := 0 to FSolvedStates.Count - 1 do
//    if CurrentState.CompareTo(FSolvedStates[i]) then begin
//      Result := True;
//      break;
//    end;
//
//  CurrentState.Free;
//end; // AddSolvedState
//

//==============================================================================
// TPositionHolder
//==============================================================================

//constructor TPositionHolder.Create(Puzzle: TPuzzle);
//begin
//  FPuzzle := Puzzle;
//  FValid := False;
//end;
//
//destructor TPositionHolder.Destroy;
//begin
//  SetLength(FPositions, 0, 0);
//  FPositions := nil;
//  inherited;
//end;
//
//procedure TPositionHolder.Invalidate;
//begin
//  FValid := False;
//end;
//
//procedure TPositionHolder.GetPartPosition(AxisNo: integer; Part: TPart; var iFrom, iTo: integer);
//var PartNo: integer;
//begin
//  if not FValid then
//    InvalidatePositions;
//
//  PartNo := FPuzzle.Parts.indexOf(Part);
//  iFrom := FPositions[PartNo][AxisNo].iFrom;
//  iTo := FPositions[PartNo][AxisNo].iTo;
//
//  if iFrom = -1 then begin
//    CalcPartPosition(AxisNo, Part, iFrom, iTo);
//    FPositions[PartNo][AxisNo].iFrom := iFrom;
//    FPositions[PartNo][AxisNo].iTo := iTo;
//  end;
//end; // GetPartPosition

//procedure TPositionHolder.CalcPartPosition(AxisNo: integer; Part: TPart; var iFrom, iTo: integer);
//const DELTA = 1e-4;
//var
//  i: integer;
//  Axis: TAxis;
//  AMin, AMax: extended;
//begin
//  Axis := FPuzzle.Axes[AxisNo];
//  TPartUtils.GetProjectionBoundary(Part, Axis.NormVector, AMin, AMax);
//  iFrom := 0;
//  iTo := Axis.Layers.Count - 1;
//
//  for i := 1 to Axis.Layers.Count - 1 do
//    if Axis.Layers[i].DistanceFrom < AMin + DELTA then
//      iFrom := i
//    else
//      break;
//
//  for i := Axis.Layers.Count - 2 downto 0 do
//    if Axis.Layers[i].DistanceTo > AMax - DELTA then
//      iTo := i
//    else
//      break;
//end; // CalcPartPosition

//procedure TPositionHolder.InvalidatePositions;
//var i, j: integer;
//begin // CalcPositions
//  SetLength(FPositions, FPuzzle.Parts.Count, FPuzzle.Axes.Count);
//  if (FPuzzle.Axes.Count = 0) or (FPuzzle.Parts.Count = 0) then
//    exit;
//
//  for i := 0 to FPuzzle.Parts.Count - 1 do
//    for j := 0 to FPuzzle.Axes.Count - 1 do begin
//      FPositions[i, j].iFrom := -1;
//      FPositions[i, j].iTo := -1;
//    end;
//  FValid := True;
//end; // InvalidatePositions

initialization
  TLibrary.RegisterPuzzleClass(TPuzzle);
end.

