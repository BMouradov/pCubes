{************************************************************}
{                                                            }
{  Unit uAxis                                                }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uAxis;
{$mode delphi}

interface
uses
  uLine, uLayer, uPlane, uVector, uMatrix,
  Generics.Collections;

type
  TAxis = class
  public
    Line: TLine; // base ray of axis
    Layers: TList<TLayer>;
    ConnectedLayers: array of array of integer;
    TurnAxesWithLayer: integer;
    TurnAxesWithPartNo: integer;
    Enabled: Boolean;

    property NormVector: TVector read Line.NormVector;
    property BaseVector: TVector read Line.BaseVector;

    constructor Create; overload;
    constructor Create(const ANormVector: TVector); overload;
    constructor Create(const ABaseVector, ANormVector: TVector); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Axis: TAxis);
    function GetRotateMatrix(Angle: extended): T4x4Matrix;
    procedure Trans(mat: T4x4Matrix);
    function ConnectLayers(Layer1, Layer2: integer): Boolean;
    function IsLayerFixed(ALayers: array of Boolean): Boolean;
    procedure CreateLayers(Distances: array of extended);
    function GetLayerPlaneTo(LayerNo: integer): TPlane;
    function GetLayerPlaneFrom(LayerNo: integer): TPlane;

//    procedure GetLayers(const Vectors: TList<TVector>; var iFrom, iTo: integer); virtual;// for Flat layers
  private
//    function GetProjectionBoundaries(const Vectors: TList<TVector>; var AMin, AMax: extended): Boolean; // for Flat layers
  end;

const INFINITE_LAYER_DISTANCE = 1000;

implementation
uses uUtils;

constructor TAxis.Create;
begin
  Enabled := True;
  Layers := TList<TLayer>.Create;
  TurnAxesWithLayer := -1;
  TurnAxesWithPartNo := -1;
  SetLength(ConnectedLayers, 0, 0);
end;

constructor TAxis.Create(const ANormVector: TVector);
var ABaseVector: TVector;
begin
  ABaseVector := [0, 0, 0];
  Create(ABaseVector, ANormVector);
end;

constructor TAxis.Create(const ABaseVector, ANormVector: TVector);
begin
  Enabled := True;
  Line := uLine.Line(ABaseVector, ANormVector);
  Layers := TList<TLayer>.Create;
  TurnAxesWithLayer := -1;
  TurnAxesWithPartNo := -1;
  SetLength(ConnectedLayers, 0, 0);
end;

destructor TAxis.Destroy;
begin
  Clear;
  Layers.Free;
  inherited;
end;

procedure TAxis.Clear;
var
  Layer: TLayer;
begin
  for Layer in Layers do
    Layer.Free;
  Layers.Clear;

  SetLength(ConnectedLayers, 0, 0);
end;

procedure TAxis.Assign(Axis: TAxis);
var
  Layer: TLayer;
begin
  Clear;

  Enabled := Axis.Enabled;
  Line := Axis.Line;

  for Layer in Axis.Layers do
    Layers.Add(TLayer.Create(Layer.DistanceFrom, Layer.DistanceTo));

  TurnAxesWithLayer := Axis.TurnAxesWithLayer;
  TurnAxesWithPartNo := Axis.TurnAxesWithPartNo;
end; // Assign

function TAxis.GetRotateMatrix(Angle: extended): T4x4Matrix;
begin
{  with BaseVector do
    Result := TMatrix.GetMoveMatrix(-X, -Y, -Z);
  with NormVector do
    Result := TMatrix.Mult(Result, TMatrix.GetRotateMatrix(X, Y, Z, Angle));
  with BaseVector do
    Result := TMatrix.Mult(Result, TMatrix.GetMoveMatrix(X, Y, Z));}
  Result := TMatrix.GetMoveMatrix(-BaseVector);
  Result := TMatrix.Mult(Result, TMatrix.GetRotateMatrix(NormVector, Angle));
  Result := TMatrix.Mult(Result, TMatrix.GetMoveMatrix(BaseVector));
end;

{
function GetTurningAngle(LayerNo: integer; Al: extended): extended;
var Angle, A1, AMin, AMax: extended;
const delta = 1e-4;
begin
  if (TurnLayerFrom = TurnLayerTo) then begin
    if (TurnLayerTo = 0) or (TurnLayerTo = Axes[TurnAxisNo].Layers.Count - 1) then
      Angle := FOutermostTurningAngle
    else
      Angle := Axes[TurnAxisNo].TurningAngles[LayerNo];
  end else if (TurnLayerFrom > 0) and (TurnLayerTo < Axes[TurnAxisNo].Layers.Count - 1) then
    Angle := Axes[TurnAxisNo].TurningAngles[LayerNo]
  else begin
    AMin := Min(FOutermostTurningAngle, Axes[TurnAxisNo].TurningAngles[LayerNo]);
    AMax := Max(FOutermostTurningAngle, Axes[TurnAxisNo].TurningAngles[LayerNo]);
    if AMax = 0 then begin
      Angle := 0;
    end else if AMin = 0 then
      Angle := AMax
    else begin
      A1 := 0;
      Angle := AMax;
      repeat
        while A1 < Angle + delta - AMin do
          A1 := A1 + AMin;
        if Abs(Angle - A1) < delta then
          break
      until (Abs(Angle - A1) < delta) or (Angle > 2 * Pi - delta);

      if Abs(Angle - A1) >= delta then
        Angle := 0;
    end;
  end;

  if Angle = 0 then
    Result := 0
  else
    Result := Round(Al / Angle) * Angle;
end; // GetTurningAngle

}

{procedure TAxis.AddPlane(Plane: TPlane);
begin
  if Plane.NormVector.IsNear(NormVector) then
    InsertPlane(Plane)
  else if Plane.NormVector.IsNearOpposite(NormVector) then begin
    Plane.TurnRound;
    InsertPlane(Plane);
  end else
    raise Exception.Create('TAxis.AddPlane: Nonparallel axis');
end;}

procedure TAxis.CreateLayers(Distances: array of extended);
var
  i, Count: integer;
  Layer: TLayer;
  BaseDistance: extended;
  FDistances: array of extended;
begin
  for Layer in Layers do
    Layer.Free;
  Layers.Clear;

  if Length(Distances) > 0 then begin
    // sort
    TArrayHelper<Extended>.Sort(Distances);

    // count different distances
    Count := 1;
    for i := Low(Distances) to High(Distances) - 1 do
      if (Distances[i + 1] - Distances[i]) > delta then
        Inc(Count);

    SetLength(FDistances, Count);

    // fill distances
    Count := 1;
    FDistances[0] := Distances[0];
    for i := 0 to High(Distances) - 1 do
      if (Distances[i + 1] - Distances[i]) > delta then begin
        FDistances[Count] := Distances[i + 1];
        Inc(Count);
      end else
        TUtils.LogWarning('Duplicated Distances');

    // create layers
    BaseDistance := 0; //Line.BaseVector.ScalarMult(Line.NormVector);
    Layers.Add(TLayer.Create(-INFINITE_LAYER_DISTANCE, FDistances[0] + BaseDistance));
    for i := 0 to Count - 2 do
      Layers.Add(TLayer.Create(FDistances[i] + BaseDistance, FDistances[i + 1] + BaseDistance));
    Layers.Add(TLayer.Create(FDistances[Count - 1] + BaseDistance, INFINITE_LAYER_DISTANCE));

  end else
    Layers.Add(TLayer.Create(-INFINITE_LAYER_DISTANCE, INFINITE_LAYER_DISTANCE));
end; // CreateFlatLayers

procedure TAxis.Trans(mat: T4x4Matrix);
begin
  Line.Trans(mat);
end; // Trans

function TAxis.IsLayerFixed(ALayers: array of Boolean): Boolean;
var i: integer;
begin
  Result := True;
  if not Enabled then
    exit;
  for i := 0 to High(ALayers) do
    if ALayers[i] and Layers[i].Fixed then
      exit;
  Result := False;
end; // IsLayerFixed

function TAxis.GetLayerPlaneTo(LayerNo: integer): TPlane;
begin
  Result := Plane(NormVector, Layers[LayerNo].DistanceTo + BaseVector ** NormVector);
end;

function TAxis.GetLayerPlaneFrom(LayerNo: integer): TPlane;
begin
  Result := Plane(NormVector, Layers[LayerNo].DistanceFrom + BaseVector ** NormVector);
end;

function TAxis.ConnectLayers(Layer1, Layer2: integer): Boolean;
var
  i, j: integer;
  Found1, Found2: integer;
begin
  Result := False;
  if Layer2 = Layer1 then
    exit;

  Found1 := -1;
  for i := 0 to Length(ConnectedLayers) - 1 do
    for j := 0 to Length(ConnectedLayers[i]) - 1 do
      if ConnectedLayers[i][j] = Layer1 then
        Found1 := i;

  Found2 := -1;
  for i := 0 to Length(ConnectedLayers) - 1 do
    for j := 0 to Length(ConnectedLayers[i]) - 1 do
      if ConnectedLayers[i][j] = Layer2 then
        Found2 := i;

  if (Found1 >= 0) and (Found2 >= 0) then
    exit; // todo if Found1 <> Found2 then combine

  if (Found1 >= 0) and (Found2 = -1) then begin
    SetLength(ConnectedLayers[Found1], Length(ConnectedLayers[Found1]) + 1);
    ConnectedLayers[Found1][Length(ConnectedLayers[Found1]) - 1] := Layer2;
  end;

  if (Found2 >= 0) and (Found1 = -1) then begin
    SetLength(ConnectedLayers[Found2], Length(ConnectedLayers[Found2]) + 1);
    ConnectedLayers[Found2][Length(ConnectedLayers[Found2]) - 1] := Layer1;
  end;

  if (Found1 = -1) and (Found2 = -1) then begin
    SetLength(ConnectedLayers, Length(ConnectedLayers) + 1);
    SetLength(ConnectedLayers[Length(ConnectedLayers) - 1], 2);
    ConnectedLayers[Length(ConnectedLayers) - 1][0] := Layer1;
    ConnectedLayers[Length(ConnectedLayers) - 1][1] := Layer2;
  end;

  Result := True;
{  if Layer2 < Layer1 then begin
    i := Layer2;
    Layer2 := Layer1;
    Layer1 := i;
  end;

  i := Layers[Layer1].ConnectedLayer;
  if i = -1 then
    Layers[Layer1].ConnectedLayer := Layer2
  else if Layer2 < i then begin
    Layers[Layer1].ConnectedLayer := Layer2;
    ConnectLayers(Layer2, i);
  end else if Layer2 > i then begin
    Layers[Layer1].ConnectedLayer := i;
    ConnectLayers(i, Layer2);
  end;}
end; // ConnectLayers

// this code only for flat layers
//function TAxis.GetProjectionBoundaries(const Vectors: TList<TVector>; var AMin, AMax: extended): Boolean;
//var
//  d: extended;
//  Vector: TVector;
//begin
//  Result := False;
//
//  for Vector in Vectors do begin
//    d := Vector.ScalarMult(NormVect);
//    if not Result then begin
//      AMin := d;
//      AMax := d;
//      Result := True;
//    end else begin
//      if d < AMin then
//        AMin := d;
//      if d > AMax then
//        AMax := d;
//    end;
//  end;
//end; // GetProjectionBoundary

// this code only for flat layers
//procedure TAxis.GetLayers(const Vectors: TList<TVector>; var iFrom, iTo: integer);
//const Delta = 1e-4;
//var
//  i: integer;
//  AMin, AMax: extended;
//begin
//  GetProjectionBoundaries(Vectors, AMin, AMax);
//
//  iFrom := 0;
//  for i := 1 to Layers.Count - 1 do
//    if Layers[i].DistanceFrom < AMin + delta then
//      iFrom := i
//    else
//      break;
//
//  iTo := Layers.Count - 1;
//  for i := Layers.Count - 2 downto 0 do
//    if Layers[i].DistanceTo > AMax - delta then
//      iTo := i
//    else
//      break;
//end; // GetLayers

end.

