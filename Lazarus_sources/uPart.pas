{************************************************************}
{                                                            }
{  Unit uPart                                                }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{  single Cubie                                              }
{************************************************************}

unit uPart;
{$mode delphi}

interface
uses uPlane, uMatrix, uVector, uFace, uGeometryUtils,
     Generics.Collections;

type
  TPart = class
  private
    FVertices: TList<TVector>;
    FFaces: TList<TFace>;
    FVisible: Boolean;
//    FChainedParts: TList<TPart>; // not used

  public
    property Vertices: TList<TVector> read FVertices;
    property Faces: TList<TFace> read FFaces;
    property Visible: Boolean read FVisible write FVisible;
//    property ChainedParts: TList<TPart> read FChainedParts; // not used

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Assign(Part: TPart);

    function AddVertex(const V: TVector): integer;
    function AddFace(idx: array of integer): TFace;
//    procedure AddChain(Part: TPart); // not used

    function HalfPlane(const Plane: TPlane): THalfOfPlane; // hopInPlane, hopInside, hopOutside, hopIntersect
    function Contains(const Point: TVector): Boolean;

    procedure Trans(Matrix: T4x4Matrix); overload;

  private
    function FlatContains(const Point, NormVec: TVector): Boolean;
    function FaceContains(const Point, NormVec: TVector; Face: TFace): Boolean;
  end;

implementation
uses SysUtils;

const
  DELTA = 2e-4;

constructor TPart.Create;
begin
  inherited;
  FVertices := TList<TVector>.Create;
  FFaces := TList<TFace>.Create;
  FVisible := True;
//  FChainedParts := TList<TPart>.Create;
end;

destructor TPart.Destroy;
begin
  Clear;
//  FChainedParts.Free;

  FFaces.Free;
  FVertices.Free;

  inherited;
end;

procedure TPart.Clear;
var Face: TFace;
begin
  FVertices.Clear;

  for Face in FFaces do
    Face.Free;
  FFaces.Clear;

  FVisible := True;

//  FChainedParts.Clear;
end;

procedure TPart.Assign(Part: TPart);
var
  i: integer;
  FaceSrc, FaceDst: TFace;
begin
  Clear;

  FVertices.AddRange(Part.FVertices);

  for FaceSrc in Part.Faces do begin
    FaceDst := TFace.Create(FVertices);
    try
      FaceDst.Color := FaceSrc.Color;
      for i := 0 to FaceSrc.GetVertexCount - 1 do begin
        FaceDst.AddVertexByIndex(FaceSrc.GetVertexIndex(i));
        FaceDst.SetEdgeVisibility(i, FaceSrc.IsEdgeVisible((i)));
      end;
    except
      FaceDst.Free;
      raise;
    end;

    Faces.Add(FaceDst);
  end;

  FVisible := Part.FVisible;

//  FChainedParts.AddRange(Part.FChainedParts);
end; // Assign

// return hopInPlane if the part is Flat and lies on Plane
// return hopInside if the part is completely in negative halfplane
// return hopOutside if the part is completely in positive halfplane
// return hopIntersect if the part is cut by the TPlane

function TPart.HalfPlane(const Plane: TPlane): THalfOfPlane;
var
  D: extended;
  Vertex, NormVect, PlaneVect: TVector;
begin
  Result := hopInPlane;
  NormVect := Plane.NormVect;
  PlaneVect := NormVect * Plane.NormDistance;
  for Vertex in FVertices do begin
    D := (Vertex - PlaneVect) ** NormVect;

    if D > DELTA then begin // outside
      if Result = hopInside then begin
        Result := hopIntersect;
        exit;
      end;
      Result := hopOutside;
    end else if D < -DELTA then begin // inside
      if Result = hopOutside then begin
        Result := hopIntersect;
        exit;
      end;
      Result := hopInside;
    end;
  end;
end; // HalfPlane

// Check if Point is into the Part (Part must be convex or flat+convex)
function TPart.Contains(const Point: TVector): Boolean;
var NormVec, Vertex, MinVec, MaxVec: TVector;
    Plane: TPlane;
    hf: THalfOfPlane;
    Face: TFace;
    d: extended;
begin // Contains
  if (Faces.Count = 0) or (Vertices.Count = 0) then
    exit(False);

  MinVec := Vertices[0];
  MaxVec := Vertices[0];
  for Vertex in Vertices do begin
    if Vertex.X > MaxVec.X then
      MaxVec.X := Vertex.X;
    if Vertex.X < MinVec.X then
      MinVec.X := Vertex.X;
    if Vertex.Y > MaxVec.Y then
      MaxVec.Y := Vertex.Y;
    if Vertex.Y < MinVec.Y then
      MinVec.Y := Vertex.Y;
    if Vertex.Z > MaxVec.Z then
      MaxVec.Z := Vertex.Z;
    if Vertex.Z < MinVec.Z then
      MinVec.Z := Vertex.Z;
  end;

  if (Point.X > MaxVec.X + DELTA) or
     (Point.X < MinVec.X - DELTA) or
     (Point.Y > MaxVec.Y + DELTA) or
     (Point.Y < MinVec.Y - DELTA) or
     (Point.Z > MaxVec.Z + DELTA) or
     (Point.Z < MinVec.Z - DELTA) then
    exit(False);

  Result := True;
  for Face in Faces do begin
    Plane := Face.GetPlane;

    NormVec := Plane.NormVect;
    d := NormVec ** (Point - NormVec * Plane.NormDistance);

    hf := HalfPlane(Plane);

    if ((d > DELTA) and (hf = hopInside)) {or ((d < -DELTA) and (hf = hpOutside))} then
      exit(False);

    if hf = hopInPlane then begin // flat part (circle etc)
      if (d < DELTA) and (d> -DELTA) then
        Result := FlatContains(Point, NormVec)
      else
        Result := False;

      break;
    end;
  end;
end; // Contains

// Check if Point is into the flat Part (point must be in part's plane)
function TPart.FlatContains(const Point, NormVec: TVector): Boolean;
var Face: TFace;
begin
  Result := True;

  for Face in FFaces do
    if FaceContains(Point, NormVec, Face) then
      exit;

  Result := False;
end; // FlatContains

// Check if Point is into the Face (point must be in face's plane, face must be convex)
function TPart.FaceContains(const Point, NormVec: TVector; Face: TFace): Boolean;
var
  d: extended;
  i: integer;
  V: TVector;
begin
  Result := True;

  for i := 0 to Face.GetVertexCount - 1 do begin
    V := ((Face.GetVertex(i + 1) - Face.GetVertex(i)) * NormVec);
    V.Normalize;

    d := V ** (Point - Face.GetVertex(i));

    if d > DELTA then begin
      Result := False;
      break;
    end;
  end;
end; // FaceContains

procedure TPart.Trans(Matrix: T4x4Matrix);
var i: integer;
begin
  for i := 0 to FVertices.Count - 1 do
    FVertices[i] := TMatrix.VectorMul(Matrix, FVertices[i]);
end; // Trans

function TPart.AddVertex(const V: TVector): integer;
var i: integer;
begin
  for i := 0 to FVertices.Count - 1 do
    if V.IsNear(FVertices[i]) then begin
      Result := i;
      exit;
    end;

  Result := FVertices.Add(V);
end;

function TPart.AddFace(idx: array of integer): TFace;
var i: integer;
begin
  Result := TFace.Create(Vertices);
  try
    for i := Low(idx) to High(idx) do
      if idx[i] < FVertices.Count then
        Result.AddVertexByIndex(idx[i])
      else
        raise Exception.Create('TPart.AddFace: invalid index');
  except
    Result.Free;
    raise;
  end;

  FFaces.Add(Result);
end; // AddFace

// not used
//procedure TPart.AddChain(Part: TPart);
//begin
//  if FChainedParts.IndexOf(Part) = -1 then
//    FChainedParts.Add(Part);
//end; // AddChain

end.

