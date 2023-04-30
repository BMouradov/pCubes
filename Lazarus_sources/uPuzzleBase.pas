{************************************************************}
{                                                            }
{  Unit uPuzzleBase                                          }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{  Parts and Axes                                            }
{************************************************************}

unit uPuzzleBase;
{$mode delphi}

// author BMouradov based on Andrea's code

interface
uses uMatrix, uPart, uAxis, uTexture, uPuzzleHeader, uVector, Generics.Collections;

//type
//  TTurningState = (tsFree, tsMidTurn, tsTurningAnimated, tsWholeRotating);
//
//type
//  TTurningType = (ttManual, ttScramble, ttUndo, ttRedo, ttMacro);
//
//type
//  TTurnEvent = procedure(AType: TTurningType; AState: TTurningState) of object;
//
type
  TPartPosition = record
    iFrom, iTo: integer;
  end;

type
  TPuzzleBase = class
  protected
    FParts: TList<TPart>;
    FAxes: TList<TAxis>;
    FTextures: TList<TTexture>; // todo - вынести или в Puzzle или в Main

    procedure TransParts(mat: T4x4Matrix; Parts: TList<TPart>);
    procedure TransAxes(mat: T4x4Matrix; Axes: TList<TAxis>);
//    procedure LoadTextures;
  public
    Header: TPuzzleHeader;
//    TurningState: TTurningState;

    property Parts: TList<TPart> read FParts;
    property Axes: TList<TAxis> read FAxes;
    property Textures: TList<TTexture> read FTextures;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure ClearHeader; virtual;
    procedure Clear;

    procedure Trans(mat: T4x4Matrix); virtual;

    procedure CalcPartPositions;
    procedure GetPartPosition(AxisNo, PartNo: integer; var iFrom, iTo: integer);
    procedure PositionsInvalidate;
  private
    FPartPositions: array of array of TPartPosition;
    FAxesValid: array of Boolean;
    procedure CalcPartPosition(AxisNo: integer; Part: TPart; var iFrom, iTo: integer);
    procedure CalcPartPositionsForAxis(AxisNo: integer);
//procedure TPuzzle.CalcPartPositionsForAxis(AxisNo: integer);
//var i, PCount: integer;
//begin
//  PCount := FParts.Count;
//  SetLength(FPartLayerFrom, PCount);
//  SetLength(FPartLayerTo,   PCount);
//
//  for i := 0 to PCount - 1 do
//    GetPartPosition1(AxisNo, FParts[i], FPartLayerFrom[i], FPartLayerTo[i]);
//end; // PartPositionsForAxis

  end;

implementation

constructor TPuzzleBase.Create;
begin
  inherited;

  Header := TPuzzleHeader.Create;

  FAxes := TList<TAxis>.Create;
  FParts := TList<TPart>.Create;
  FTextures := TList<TTexture>.Create;
end; // Create

destructor TPuzzleBase.Destroy;
begin
  Clear;

  FTextures.Free;
  FParts.Free;
  FAxes.Free;

  Header.Free;

  FPartPositions := nil;

  inherited;
end; // Destroy

//==============================================================================
// Clear
//==============================================================================

procedure TPuzzleBase.ClearHeader;
begin
  Header.Clear;
end; // ClearHeader

procedure TPuzzleBase.Clear;
var i: Integer;
begin
  ClearHeader;

  for i := 0 to FTextures.Count - 1 do
    FTextures[i].Free;
  FTextures.Clear;

  for i := 0 to FParts.Count - 1 do
    FParts[i].Free;
  FParts.Clear;

  for i := 0 to FAxes.Count - 1 do
    FAxes[i].Free;
  FAxes.Clear;
end; // Clear

//==============================================================================
// Transformations
//==============================================================================

procedure TPuzzleBase.TransParts(mat: T4x4Matrix; Parts: TList<TPart>);
var Part: TPart;
begin
  for Part in Parts do
    Part.Trans(mat);
end; // Trans

procedure TPuzzleBase.TransAxes(mat: T4x4Matrix; Axes: TList<TAxis>);
var Axis: TAxis;
begin
  for Axis in Axes do
    Axis.Trans(mat);
end; // Trans

procedure TPuzzleBase.Trans(mat: T4x4Matrix);
begin
  TransParts(mat, FParts);
  TransAxes(mat, FAxes);
end; // Trans

//==============================================================================
// Positions
//==============================================================================

procedure TPuzzleBase.GetPartPosition(AxisNo, PartNo: integer; var iFrom, iTo: integer);
var PartPosition: TPartPosition;
begin
//CalcPartPosition(AxisNo, FParts[PartNo], iFrom, iTo);
//exit;
  SetLength(FAxesValid, FAxes.Count);

  if not FAxesValid[AxisNo] then
    CalcPartPositionsForAxis(AxisNo);

  PartPosition := FPartPositions[AxisNo][PartNo];
  iFrom := PartPosition.iFrom;
  iTo := PartPosition.iTo;
end; // GetPartPosition

procedure TPuzzleBase.PositionsInvalidate;
var i, n: integer;
begin
  n := FAxes.Count;
  SetLength(FAxesValid, n);
  for i := 0 to n - 1 do
    FAxesValid[i] := False;
end; // PositionsInvalidate

// procedure returns the layers list to which the cubie belongs
// for base TPuzzle class it's assumed that layers are flat
// private
procedure TPuzzleBase.CalcPartPosition(AxisNo: integer; Part: TPart; var iFrom, iTo: integer);
var
  AMin, AMax: extended;
  Axis: TAxis;

  // this code only for flat layers
  function GetProjectionBoundaries: Boolean;
  var
    d, BaseProjection: extended;
    NormVect, Vector: TVector;
  begin
    Result := False;
    NormVect := Axis.NormVector;

    for Vector in Part.Vertices do begin
      d := Vector ** NormVect;
      if not Result then begin
        AMin := d;
        AMax := d;
        Result := True;
      end else begin
        if d < AMin then
          AMin := d;
        if d > AMax then
          AMax := d;
      end;
    end;

    if Result then begin
      BaseProjection := Axis.NormVector ** Axis.BaseVector;
      AMin := AMin - BaseProjection;
      AMax := AMax - BaseProjection;
    end;
  end; // GetProjectionBoundaries

var
  i: integer;
const DELTA = 1e-4;
begin // CalcPartPosition
  Axis := FAxes[AxisNo];

  GetProjectionBoundaries;

  iFrom := 0;
  for i := 1 to Axis.Layers.Count - 1 do
    if Axis.Layers[i].DistanceFrom < AMin + DELTA then
      iFrom := i
    else
      break;

  iTo := Axis.Layers.Count - 1;
  for i := Axis.Layers.Count - 2 downto 0 do
    if Axis.Layers[i].DistanceTo > AMax - DELTA then
      iTo := i
    else
      break;
end; // CalcPartPosition

// private
procedure TPuzzleBase.CalcPartPositionsForAxis(AxisNo: integer);
var i, PCount: integer;
begin
  PCount := FParts.Count;
  SetLength(FPartPositions, FAxes.Count, PCount);

  for i := 0 to PCount - 1 do begin
    with FPartPositions[AxisNo][i] do
      CalcPartPosition(AxisNo, FParts[i], iFrom, iTo);
  end;
  FAxesValid[AxisNo] := True;
end; // PartPositionsForAxis

procedure TPuzzleBase.CalcPartPositions;
var i: integer;
begin
  SetLength(FAxesValid, FAxes.Count);
  for i := 0 to FAxes.Count - 1 do
    CalcPartPositionsForAxis(i);
end; // PartPositionsForAxis

end.

