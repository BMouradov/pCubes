{************************************************************}
{                                                            }
{  Unit uPartUtils                                           }
{  2018-2020                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uPartUtils;

{$mode Delphi}

interface

uses uPart, uPlane, uVector, uFace,
  Generics.Collections, Graphics;

type TPartUtils = class
private
  class procedure SetFaceEdgesVisibility(Part: TPart; Face: TFace; InvisibleEdges: integer);
public
  class function CutPart(Part: TPart; Plane: TPlane; NewParts: TList<TPart>; SliceColor: TColor; InvisibleEdges: integer = 0): Boolean;
  class function CombineParts(Part1, Part2: TPart; NewPart: TPart): Boolean;
//  class function GetProjectionBoundary(Part: TPart; NormVect: TVector; var AMin, AMax: extended): Boolean; static;
  class function GetNearestFace(Part: TPart; Vector: TVector): TFace; static;
end;

implementation

uses
  Math,
  uFaceUtils, uGeometryUtils;

const DELTA = 0.001 / 240;

{type TSegment = class
  v1, v2: integer;
  f1, f2: integer;
  e1, e2: integer;
end;}

class procedure TPartUtils.SetFaceEdgesVisibility(Part: TPart; Face: TFace; InvisibleEdges: integer);
var i, j: integer;
    EdgeVisible: boolean;
    v1, v2: integer;
    TestFace: TFace;
begin
  for i := 0 to Face.GetVertexCount - 1 do begin
    EdgeVisible := (InvisibleEdges and (1 shl i)) = 0;
    Face.SetEdgeVisibility(i, EdgeVisible);
    V1 := Face.GetVertexIndex(i);
    V2 := Face.GetVertexIndex(i + 1);
    for TestFace in Part.Faces do
      if TestFace <> Face then
        for j := 0 to TestFace.GetVertexCount - 1 do
          if (TestFace.GetVertexIndex(j + 1) = V1) and (TestFace.GetVertexIndex(j) = V2)then
            TestFace.SetEdgeVisibility(j, EdgeVisible);
  end;
end; // SetFaceEdgesVisibility


// предполагаем, что уже известно, что есть хоть одно пересечение с плоскостью
// ¬ данной версии процедуры нет разбиени€ на части, если их образовалось две
class function TPartUtils.CutPart(Part: TPart; Plane: TPlane; NewParts: TList<TPart>; SliceColor: TColor; InvisibleEdges: integer = 0): Boolean;
var
  Distances: array of extended;
  Segments1, Segments2: TList<TVector>;
//  Segments: TList<TSegment>;
//  Segment: TSegment;
  i, j: integer;
  vi1, vi2: integer;
  hp: THalfOfPlane;
  NewPart: TPart;
  Face, NewFace: TFace;
  NewFaces: TList<TFace>;
  V0, V2: TVector;
  Found{, EdgeVisible}: Boolean;
begin // CutPart

  NewPart := TPart.Create;
  NewParts.Add(NewPart);

  // add all faces from old part

  for Face in Part.Faces do begin
    hp := Face.HalfPlane(Plane);
    if (hp = hopOutside) or ((hp = hopInPlane) and Plane.NormVect.IsNearOpposite(Face.GetNorm)) then begin
      NewFace := TFace.Create(NewPart.Vertices);
      NewFace.Assign(Face);
      NewPart.Faces.Add(NewFace);
    end else if hp = hopIntersect then begin
      NewFaces := TList<TFace>.Create;
      if TFaceUtils.CutFace(Face, Plane, False, NewFaces, NewPart.Vertices) then
        NewPart.Faces.AddRange(NewFaces);
      NewFaces.Free;
    end;
  end;

  // create slice face

  SetLength(Distances, NewPart.Vertices.Count);
  for i := 0 to NewPart.Vertices.Count - 1 do
    Distances[i] := TGeometryUtils.DistancePointToPlane(Plane, NewPart.Vertices[i]);

  Segments1 := TList<TVector>.Create;
  Segments2 := TList<TVector>.Create;
//  Segments := TList<TSegment>.Create;
  try

    // collect segments in plane
    for i := 0 to NewPart.Faces.Count - 1 do
      for j := 0 to NewPart.Faces[i].GetVertexCount - 1 do
        //CheckSegment(NewPart.Faces[i], j);
        with NewPart.Faces[i] do begin
          vi1 := GetVertexIndex(j);
          vi2 := GetVertexIndex(j + 1);
          if  IsZero(Distances[vi1], DELTA) and IsZero(Distances[vi2], DELTA) then begin
//          if (d1 <= DELTA) and (d1 >= -DELTA) and (d2 <= DELTA) and (d2 >= -DELTA) then begin
//            Segment := TSegment.Create;
//            Segment.v1 := vi1;
//            Segment.v2 := vi2;
//            Segment.f1 := i;
//            Segment.f2 := NewPart.Faces.Count;
//            Segment.e1 := j;
//            Segment.e2 := Segments.Count - 1;
//            Segments.Add(Segment);
            Segments2.Add(GetVertex(j)); // reverse order
            Segments1.Add(GetVertex(j + 1));

            SetEdgeVisibility(j, True); // дл€ цилиндров
          end;
        end;

    // delete opposite edges
    for i := Segments1.Count - 2 downto 0 do
      for j := Segments1.Count - 1 downto i + 1 do
        if Segments1[i].IsNear(Segments2[j]) and Segments1[j].IsNear(Segments2[i]) then begin
          Segments1.Delete(j);
          Segments2.Delete(j);
//          Segments[j].Free;
//          Segments.Delete(j);
//          Segments.Delete(j);
          Segments1.Delete(i);
          Segments2.Delete(i);
//          Segments[i].Free;
//          Segments.Delete(i);
          break;
        end;

    while Segments1.Count > 2 do begin
      NewFace := TFace.Create(NewPart.Vertices);
      NewFace.Color := SliceColor;

      V0 := Segments1[0];
      V2 := Segments2[0];
      NewFace.AddVector(V0);
      Segments1.Delete(0);
      Segments2.Delete(0);
//      Segments[0].Free;
//      Segments.Delete(0);

      repeat
        Found := False;
        for i := 0 to Segments1.Count - 1 do begin
          if Segments1[i].IsNear(V2) then begin
            NewFace.AddVector(V2, True);
            Found := True;
            V2 := Segments2[i];
            Segments1.Delete(i);
            Segments2.Delete(i);
//            Segments[i].Free;
//            Segments.Delete(i);
            break;
          end;
        end;
      until not Found;

      if V2.IsNear(V0) and (NewFace.GetVertexCount > 2) then begin
        NewFace.CheckFace; // удал€ем развЄрнутые точки

        if NewFace.GetVertexCount > 2 then begin
          NewPart.Faces.Add(NewFace);
          if InvisibleEdges <> 0 then
            SetFaceEdgesVisibility(NewPart, NewFace, InvisibleEdges);
//
//          // EdgeVisibility
//          for i := 0 to NewFace.GetVertexCount - 1 do begin
//            EdgeVisible := InvisibleEdges and (1 shl i) = 0;
//            Segment
//            NewFace.SetEdgeVisibility(i, EdgeVisible);
//            SetEdgeVisibility(NewFace.GetVertexIndex(i), NewFace.GetVertexIndex(i + 1), InvisibleEdges and (1 shl i) = 0)
//
//
//          end;
//
        end else
          NewFace.Free;
      end else
        NewFace.Free;
    end;

  finally
//    for Segment in Segments do
//      Segment.Free;
//    Segments.Free;
    Segments1.Free;
    Segments2.Free;
  end;

  Result := True;
end; // CutPart

class function TPartUtils.CombineParts(Part1, Part2: TPart; NewPart: TPart): Boolean;
var
  i, j: integer;
  iFace1, iFace2: integer;
  f1, f2, NewFace: TFace;
  Found: Boolean;
  Faces2: TList<TFace>;
begin // CombineParts
  iFace1 := -1;
  iFace2 := -1;
  //FindAdjoining faces
  Result := False;
  Found := False;
  for i := 0 to Part1.Faces.Count - 1 do begin
    f1 := Part1.Faces[i];
    for j := 0 to Part2.Faces.Count - 1 do begin
      f2 := Part2.Faces[j];
      if (not f1.IsColored) and (not f2.IsColored) and TFaceUtils.FacesAreSameButOpposite(f1, f2) then begin // faces must be gray
        iFace1 := i;
        iFace2 := j;
        Found := True;
        break;
      end;
    end;
    if Found then
      break;
  end;

  if not Found then
    exit;

  Faces2 := TList<TFace>.Create;
  for i := 0 to Part2.Faces.Count - 1 do
    if i <> iFace2 then
      Faces2.Add(Part2.Faces[i]);

  for i := 0 to Part1.Faces.Count - 1 do begin
    if i = iFace1 then
      continue;
    f1 := Part1.Faces[i];

    Found := False;
    NewFace := TFace.Create(NewPart.Vertices);
    for j := 0 to Faces2.Count - 1 do begin
      f2 := Faces2[j];

      if TFaceUtils.CombineFaces(f1, f2, NewFace) then begin
        Faces2.Delete(j);
        NewPart.Faces.Add(NewFace);
        Found := True;
        break;
      end;
    end;
    if not Found then begin
      NewFace.Assign(f1);
      NewPart.Faces.Add(NewFace);
    end;
  end;

  for i := 0 to Faces2.Count - 1 do begin
    NewFace := TFace.Create(NewPart.Vertices);
    NewFace.Assign(Faces2[i]);
    NewPart.Faces.Add(NewFace);
  end;

  Faces2.Free;

  Result := True;
end; // CombineParts

{class function TPartUtils.GetProjectionBoundary(Part: TPart; NormVect: TVector; var AMin, AMax: extended): Boolean;
var
  i: integer;
  d: extended;
begin
  Result := False;
  for i := 0 to Part.Vertices.Count - 1 do begin
    d := Part.Vertices[i].ScalarMult(NormVect);
    if i = 0 then begin
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
end; // GetProjectionBoundary}

class function TPartUtils.GetNearestFace(Part: TPart; Vector: TVector): TFace;
var d, MinDistance: extended;
    Face: TFace;
begin
  Result := nil;
  MinDistance := 1e10;
  for Face in Part.Faces do begin
    d := Abs(Face.GetPlane.DistanceToPoint(Vector));
    if d < MinDistance then begin
      MinDistance := d;
      Result := Face;
    end;
  end;
end;

end.

