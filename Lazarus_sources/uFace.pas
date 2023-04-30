{************************************************************}
{                                                            }
{  Unit uFace                                                }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uFace;
{$mode delphi}

interface
uses
  uVector, uGeometryUtils, uPlane,
  Graphics, Generics.Collections;

type TFaceType = (ftUnknown, ftConvex, ftReflex);

type
  TFaceVertex = record
    Index: integer;
    SegmentVisible: Boolean;
  end;

type
  TFace = class
  private
    FPartVertices: TList<TVector>; // pointer to parent's Vertices
    FSpikes: TList<Integer>; // Spikes and grooves шипы (>0) и пазы (<0)
//    FTriangles: TList<TVector>; // not used
    FConvexity: TFaceType;
    FNormalIndex: integer; // normal calulates from this vertex and its neighbours

    FVertices: TList<TFaceVertex>;

    procedure CheckConvexity;
  public
    Color: TColor;
    TextureId: integer;
    IsStickerVisible: Boolean;  // for temporary Sticker hiding

    // not used yet
//    TextureFileName: String;
//    TextureXCoords: array of integer;
//    TextureYCoords: array of integer;

    constructor Create(PartVectices: TList<TVector>);
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Face: TFace);
    function IsColored: Boolean;
    procedure Invalidate;

    function GetNorm: TVector;
    function GetVertex(i: integer): TVector;
    function GetVertexIndex(i: integer): integer;
    function GetVertexCount: integer;
    function HalfPlane(Plane: TPlane): THalfOfPlane;
//    function GetDepth: extended; // среднеарифметическая координата y
    function GetPlane: TPlane;
    procedure Rotate(n: integer);
    function AddVector(v: TVector; Visible: Boolean = True): Boolean;
    procedure AddVertexByIndex(idx: integer; SegmentVisible: Boolean = True);
    function IsEdgeVisible(idx: integer): Boolean;
    function AllEdgesVisible: Boolean;
    procedure SetEdgeVisibility(idx: integer; Visible: Boolean);
    procedure SetSpike(Level: integer);

    procedure CheckFace;
  end;

implementation

uses uFaceUtils;

const
  DELTA = 2.5e-4;

constructor TFace.Create(PartVectices: TList<TVector>);
begin
  inherited Create;
  FPartVertices := PartVectices;
  FVertices := TList<TFaceVertex>.Create;
  FSpikes := TList<Integer>.Create;

//  FTriangles := nil;
  FConvexity := ftUnknown;
  Color := -1;
  TextureId := -1;
  IsStickerVisible := True;
  Invalidate;
end; // Create

destructor TFace.Destroy;
begin
  Clear;
//  if FTriangles <> nil then
//    FTriangles.Free;
  FSpikes.Free;
  FVertices.Free;
  inherited;
end; // Destroy

procedure TFace.Clear;
begin
  Color := -1;
//  if FTriangles <> nil then
//    FTriangles.Clear;
  FSpikes.Clear;
  FVertices.Clear;
//  TextureFileName := '';
  TextureId := -1;
//  SetLength(TextureXCoords, 0);
//  SetLength(TextureYCoords, 0);
  Invalidate;
end; // Clear

procedure TFace.Assign(Face: TFace);
var i: integer;
begin
  Clear;
  Color := Face.Color;
  TextureId := Face.TextureId;

  if FPartVertices = Face.FPartVertices then // same part (not ever used)
    FVertices.AddRange(Face.FVertices)
  else // different parts
    for i := 0 to Face.GetVertexCount - 1 do
      AddVector(Face.GetVertex(i), Face.IsEdgeVisible(i));
  FSpikes.AddRange(Face.FSpikes);

//  FTriangles := nil;
  FConvexity := Face.FConvexity;
end; // Assign

function TFace.IsColored: Boolean;
begin
  Result := (Color <> -1) and IsStickerVisible;
end;

procedure TFace.Invalidate;
begin
  FConvexity := ftUnknown;
end;

function TFace.GetVertex(i: integer): TVector;
var n: integer;
begin
  n := FVertices.Count;
  if n = 0 then begin
    Result := [0, 0, 0];
    exit;
  end;
  while i >= n do
    Dec(i, n);
  while i < 0 do
    Inc(i, n);
  Result := FPartVertices[FVertices[i].Index];
end; // GetVertex

// returns parent's index of vertex
function TFace.GetVertexIndex(i: integer): integer;
var n: integer;
begin
  n := FVertices.Count;
  while i >= n do
    Dec(i, n);
  while i < 0 do
    Inc(i, n);
  Result := FVertices[i].Index;
end;

function TFace.GetVertexCount: integer;
begin
  Result := FVertices.Count;
end;

// eliminates double points and straight angles
procedure TFace.CheckFace;
var idx: integer;
begin
  idx := 0;
  while idx < GetVertexCount do begin
    if TFaceUtils.IsSameAsPrev(Self, idx) or
       TFaceUtils.IsStraightAngle(Self, idx) or
       TFaceUtils.IsZeroAngle(Self, idx) then begin
      FVertices.Delete(idx);
      if idx > 0 then
        Dec(idx, 2)
      else
        Dec(idx);
    end;
    Inc(idx);
  end;
end; // CheckFace

// return hopInPlane if the face is degenerate and lies on Plane
// return hopInside if the face is completely in negative halfplane
// return hopOutside if the face is completely in positive halfplane
// return hopIntersect if the face is cut by the TPlane

function TFace.HalfPlane(Plane: TPlane): THalfOfPlane;
var
  i: integer;
  D: extended;
  NormVect, PlaneVect: TVector;
begin
  Result := hopInPlane;
  NormVect := Plane.NormVect;
  PlaneVect := NormVect * Plane.NormDistance;
  for i := 0 to GetVertexCount - 1 do begin
    D := (GetVertex(i) - PlaneVect) ** NormVect;

    if D > DELTA then begin
      if Result = hopInside then begin
        Result := hopIntersect;
        exit;
      end;
      Result := hopOutside;
    end else if D < -DELTA then begin
      if Result = hopOutside then begin
        Result := hopIntersect;
        exit;
      end;
      Result := hopInside;
    end;
  end;
end; // HalfPlane

function TFace.GetNorm: TVector;
var v0, v1, v2: TVector;
begin
  if FConvexity = ftUnknown then
    CheckConvexity;

  v0 := GetVertex(FNormalIndex - 1);
  v1 := GetVertex(FNormalIndex);
  v2 := GetVertex(FNormalIndex + 1);

  Result := ((v2 - v1) * (v0 - v1));
  Result.Normalize;
end;

procedure TFace.CheckConvexity;
begin
  if not TFaceUtils.IsFaceConvex(Self) then
    FConvexity := ftReflex
  else
    FConvexity := ftConvex;
  FNormalIndex := TFaceUtils.CalculateBaseVertex(Self);
end;

//function TFace.GetDepth: extended;
//var
//  d: extended;
//  i: integer;
//begin
//  d := 0;
//  for i := 0 to FVertices.Count - 1 do
//    d := d + GetVertex(i).y;
//  d := d / FVertices.Count;
//  Result := d;
//end; // GetDepth

//procedure TFace.GetPlane(Plane: TPlane);
//begin
//  Plane.NormVect := GetNorm;
//  Plane.NormDistance := Plane.NormVect.ScalarMult(GetVertex(0));
//end; // GetPlane

function TFace.GetPlane: TPlane;
begin
  Result.NormVect := GetNorm;
  Result.NormDistance := Result.NormVect ** GetVertex(0);
end; // GetPlane

procedure TFace.Rotate(n: integer);
var i: integer;
begin
  if n > 0 then begin
    for i := 0 to n - 1 do
      FVertices.Move(0, FVertices.Count - 1);
  end else if n < 0 then begin
    for i := 0 to -n - 1 do
      FVertices.Move(FVertices.Count - 1, 0);
  end;
end; // Rotate

function TFace.AddVector(v: TVector; Visible: Boolean = True): Boolean;
var i: integer;
begin
  for i := 0 to FPartVertices.Count - 1 do begin
    if FPartVertices[i].IsNear(v) then begin
//      if (FVertexIndexes.Count > 0) and (i = FVertexIndexes[FVertexIndexes.Count - 1]) then
//        raise Exception.Create('Error Message1');
      AddVertexByIndex(i, Visible);
      Result := True;

      exit;
    end;
  end;

  FPartVertices.Add(v);
  AddVertexByIndex(FPartVertices.Count - 1, Visible);

  Result := False;
end; // AddVector

procedure TFace.AddVertexByIndex(idx: integer; SegmentVisible: Boolean = True);
var Vertex: TFaceVertex;
begin
  if (FVertices.Count = 0) or (idx <> FVertices[FVertices.Count - 1].Index) then begin // avoid duplicates
    Vertex.Index := idx;
    Vertex.SegmentVisible := SegmentVisible;
    FVertices.Add(Vertex);
  end;
end; // AddVertexByIndex

function TFace.IsEdgeVisible(idx: integer): Boolean;
begin
  Result := FVertices[idx].SegmentVisible;
end; // IsEdgeVisible

function TFace.AllEdgesVisible: Boolean; // used in graphics engine
var i: integer;
begin
  Result := False;
  for i := 0 to FVertices.Count - 1 do
    if not FVertices[i].SegmentVisible then
      exit;
  Result := True;
end; // AllEdgesVisible

procedure TFace.SetEdgeVisibility(idx: integer; Visible: Boolean);
var Vertex: TFaceVertex;
begin
  Vertex := FVertices[idx];
  Vertex.SegmentVisible := Visible;
  FVertices[idx] := Vertex;
end; // SetEdgeVisibility

procedure TFace.SetSpike(Level: integer);
begin
  if not FSpikes.Contains(Level) then
    FSpikes.Add(Level);
  if FSpikes.Contains(-Level) then
    FSpikes.Remove(-Level);
end; // SetSpike

end.

