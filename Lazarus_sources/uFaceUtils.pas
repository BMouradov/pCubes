{************************************************************}
{                                                            }
{  Unit uFaceUtils                                           }
{  2018-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uFaceUtils;
{$mode delphi}

interface
uses uFace, uPlane, uVector, Generics.Collections;

type
  TFaceUtils = class
    class function IsSameAsPrev(const Face: TFace; idx: integer): Boolean; static;
    class function IsStraightAngle(const Face: TFace; idx: integer): Boolean; static;
    class function IsZeroAngle(const Face: TFace; idx: integer): Boolean; static;
    class function CalculateBaseVertex(const Face: TFace): integer; static;
    class function IsFaceConvex(const Face: TFace): Boolean; static;
    class procedure Triangulate(const Face: TFace; var list: TList<Integer>); static;
    class function CutFace(Face: TFace; Plane: TPlane; InvisibleSlice: Boolean; NewFaces: TList<TFace>; NewVectors: TList<TVector>): Boolean; static;
    class function FacesAreSameButOpposite(Face1, Face2: TFace): Boolean; static;
    class function CombineFaces(Face1, Face2, NewFace: TFace): Boolean; static;
  private
    class function CalculateNormal(const Face: TFace): TVector; static;
    class function IsFlatFace(const Face: TFace): Boolean; static; // not used
  end;

implementation
uses SysUtils, Generics.Defaults, Math;

const DELTA = 0.000004;

// Verex has same coordinates as previous vertex
class function TFaceUtils.IsSameAsPrev(const Face: TFace; idx: integer): Boolean;
var v0, v1: TVector;
begin
  Result := Face.GetVertexIndex(idx) = Face.GetVertexIndex(idx - 1);
  if Result then
    exit;

  v0 := Face.GetVertex(idx);
  v1 := Face.GetVertex(idx - 1);
  Result := v0.IsNear(v1);
end; // IsSameAsPrev

// Is 180 degrees?
class function TFaceUtils.IsStraightAngle(const Face: TFace; idx: integer): Boolean;
var v0, v1, v2: TVector;
begin
  v0 := Face.GetVertex(idx);
  v1 := v0 - Face.GetVertex(idx - 1);
  v2 := Face.GetVertex(idx + 1) - v0;
  v1.Normalize;
  v2.Normalize;
  Result := v1.IsNear(v2);
end;

// is zero-angle?
class function TFaceUtils.IsZeroAngle(const Face: TFace; idx: integer): Boolean;
var v0, v1, v2: TVector;
begin
  v0 := Face.GetVertex(idx);
  v1 := v0 - Face.GetVertex(idx - 1);
  v2 := v0 - Face.GetVertex(idx + 1);
  v1.Normalize;
  v2.Normalize;
  Result := v1.IsNear(v2);
end;

// find leftmost point with convex angle - for normal's calculation
class function TFaceUtils.CalculateBaseVertex(const Face: TFace): integer;
var i, n, iX, iY, iZ: integer;
  XMin, XMax, YMin, YMax, ZMin, ZMax, dX, dY, dZ: extended;
begin
  Result := -1;

  n := Face.GetVertexCount;
  if n < 3 then
    exit;
  with Face.GetVertex(0) do begin
    XMin := X;
    XMax := X;
    YMin := Y;
    YMax := Y;
    ZMin := Z;
    ZMax := Z;
  end;
  iX := 0;
  iY := 0;
  iZ := 0;

  // leftmost point
  for i := 1 to n - 1 do begin
    with Face.GetVertex(i) do begin
      if X < XMin then begin
        XMin := X;
        iX := i;
      end;
      if X > XMax then
        XMax := X;

      if Y < YMin then begin
        YMin := Y;
        iY := i;
      end;
      if Y > YMax then
        YMax := Y;

      if Z < ZMin then begin
        ZMin := Z;
        iZ := i;
      end;
      if Z > ZMax then
        ZMax := Z;
    end;
  end;

  dX := Abs(XMax - XMin);
  dY := Abs(YMax - YMin);
  dZ := Abs(ZMax - ZMin);
  if (dX >= dY) and (dX >= dZ) then
    Result := iX
  else if (dY >= dX) and (dY >= dZ) then
    Result := iY
  else
    Result := iZ;

  // Resultin angle may be 180, then find non-180 angle
  for i := 0 to n - 1 do
    if not IsStraightAngle(Face, Result + i) then begin
      Result := (Result + i) mod n;
      exit;
    end;
end; // CalculateBaseVertex

// face's normal. Slow procedure
class function TFaceUtils.CalculateNormal(const Face: TFace): TVector;
var
  idx: integer;
  v0, v1, v2: TVector;
begin
  idx := CalculateBaseVertex(Face);

  v0 := Face.GetVertex(idx - 1);
  v1 := Face.GetVertex(idx);
  v2 := Face.GetVertex(idx + 1);
  Result := (v2 - v1) * (v0 - v1);
  Result.Normalize;
end; // CalculateNormal

// if face is flat
class function TFaceUtils.IsFlatFace(const Face: TFace): Boolean;
var
  i: integer;
  nv: TVector;
  d: extended;
begin
  nv := CalculateNormal(Face);
  d := nv ** Face.GetVertex(0);

  Result := False;
  for i := 1 to Face.GetVertexCount - 1 do
    if Abs(nv ** Face.GetVertex(i) - d) > DELTA then
      exit;
  Result := True;
end; // IsFlatFace

class function TFaceUtils.IsFaceConvex(const Face: TFace): Boolean;
var
  nv, v: TVector;
  i, n: integer;
  v0, v1, v2: TVector;
begin
  Result := False;
  nv := CalculateNormal(Face);
  n := Face.GetVertexCount;
  for i := 0 to n - 1 do begin
    v0 := Face.GetVertex(i);
    v1 := Face.GetVertex(i + 1);
    v2 := Face.GetVertex(i + 2);

    v := ((v0 - v1) * (v2 - v1));
    if v ** nv < 0 then
      if not IsStraightAngle(Face, i + 1) then
        exit;
  end;
  Result := True;
end; // IsFaceConvex

class procedure TFaceUtils.Triangulate(const Face: TFace; var list: TList<Integer>);

  function InTriangle(const v, v1, v2, v3: TVector): Boolean;
    function CalcNorm(const v, v1, v2: TVector): TVector;
    begin
      Result := (v1 - v) * (v2 - v);
      Result.Normalize;
    end;
  var n1, n2, n3: TVector;
  begin
     n1 := CalcNorm(v, v1, v2);
     n2 := CalcNorm(v, v2, v3);
     n3 := CalcNorm(v, v3, v1);
     Result := n1.IsNear(n2) and n1.IsNear(n3);
  end;

var
  VertIdxs: TList<Integer>;
  i, j, n, i1, i2, i3: integer;
  v, nv, v1, v2, v3: TVector;
  Found: Boolean;
begin // Triangulate
  if Face.GetVertexCount <= 3 then
    exit;

  nv := Face.GetNorm;

  VertIdxs := TList<Integer>.Create;
  try
    for i := 0 to Face.GetVertexCount - 1 do
      VertIdxs.Add(i);

    while VertIdxs.Count > 3 do begin
      n := VertIdxs.Count;
      for i := 0 to n - 1 do begin
        i1 := (i - 1 + n) mod n;
        i2 := i;
        i3 := (i + 1) mod n;
        v1 := Face.GetVertex(VertIdxs[i1]);
        v2 := Face.GetVertex(VertIdxs[i2]);
        v3 := Face.GetVertex(VertIdxs[i3]);

        // convex vertex
        v := ((v3 - v2) * (v1 - v2));
        if v ** nv < DELTA then
          continue;

        Found := False;
        for j := 0 to n - 1 do begin
          if (j <> i1) and (j <> i2) and (j <> i3) and InTriangle(Face.GetVertex(VertIdxs[j]), v1, v2, v3) then begin
            Found := True;
            break;
          end;
        end;
        if not Found then begin
          list.AddRange([Face.GetVertexIndex(VertIdxs[i2]), Face.GetVertexIndex(VertIdxs[i3]), Face.GetVertexIndex(VertIdxs[i1])]);
          VertIdxs.Delete(i);
          break;
        end;
      end;
      if n = VertIdxs.Count then
        raise Exception.Create('Triangulate: Error Message');
    end;
    list.AddRange([Face.GetVertexIndex(VertIdxs[1]), Face.GetVertexIndex(VertIdxs[2]), Face.GetVertexIndex(VertIdxs[0])]);

  finally
    VertIdxs.Free;
  end;
end; // Triangulate

type
  TVertexType = (vtInside, vtSide, vtOutside, vtGoInside, vtGoOutside);

type
  TVertex = class
    Vector: TVector;
    typ: TVertexType;
    LinkedPoint, Next: TVertex;
    InvisibleEdge: Boolean;
    used: Boolean;
    Distance: extended;

    constructor Create;
  end;

constructor TVertex.Create;
begin
  LinkedPoint := nil;
  Next := nil;
  InvisibleEdge := False;
  used := False;
  Distance := 0;
end;

function CompareDistances(constref A, B: TVertex): Integer;
begin
  Result := Sign(A.Distance - B.Distance);
end;

// Returns True, if polygon was clipped (changed)
// in NewFaces - resulting polygons (may be several)
class function TFaceUtils.CutFace(Face: TFace; Plane: TPlane; InvisibleSlice: Boolean; NewFaces: TList<TFace>; NewVectors: TList<TVector>): Boolean;
//type
//  TComparer<TVertex> = TComparer<TVertex>;
var
  FaceVertices: TList<TVertex>;
  CutVertices: TList<TVertex>;

  function CalcNewVertex(i1, i2: integer): TVector;
  var c1, c2: TVertex;
  begin
    c1 := FaceVertices[i1];
    c2 := FaceVertices[i2];
    Result := c1.Vector + (c2.Vector - c1.Vector) * ((0 - c1.Distance) / (c2.Distance - c1.Distance));
  end; // CalcNewVertexV

var
  i, i2, n: integer;
  TempFace, NewFace: TFace;
  c, c1, c2, first, current, slicePoint: TVertex;
  PlaneVector, IntersectionVector: TVector;
  TempVectors: TList<TVector>;
begin // CutFace
  Result := False;
  if Face.GetVertexCount < 3 then
    exit;

  PlaneVector := Plane.NormVect * Plane.NormDistance;
  IntersectionVector := Plane.NormVect * Face.GetNorm;
  IntersectionVector.Normalize;

// Weiler–Atherton clipping algorithm
// 1. List the vertices of the clipping-region polygon A and those of the subject polygon B.
// 2. Label the listed vertices of subject polygon B as either inside or outside of clipping region A.
// 3. Find all the polygon intersections and insert them into both lists, linking the lists at the intersections.
// 4. Generate a list of "inbound" intersections – the intersections where the vector from the intersection to the subsequent vertex of subject polygon B begins inside the clipping region.
// 5. Follow each intersection clockwise around the linked lists until the start position is found.

  // make vertex list and intersections between them
//  for i := 0 to Face.GetVertexCount - 1 do
//    ProcessSegment(i, (i + 1) mod Face.GetVertexCount);

// 1. List the vertices of the clipping-region polygon A and those of the subject polygon B.
  FaceVertices := TList<TVertex>.Create;
  CutVertices := TList<TVertex>.Create(
    TComparer<TVertex>.Construct(@CompareDistances)
  );

  try

    for i := 0 to Face.GetVertexCount - 1 do begin
      c := TVertex.Create;
      c.Vector := Face.GetVertex(i);
      c.InvisibleEdge := not Face.IsEdgeVisible(i);
      c.Distance := Plane.NormVect ** (c.Vector - PlaneVector);

  // 2. Label the listed vertices of subject polygon B as either inside or outside of clipping region A.
  // ãðàíè÷íûå - ïî ïðåäûäóùèì
      if c.Distance < -DELTA then
        c.typ := vtInside
      else if c.Distance > DELTA then
        c.typ := vtOutside
      else begin// vtSide
        if i > 0 then
          c.typ := FaceVertices[i - 1].typ
        else
          c.typ := vtSide;
      end;
      FaceVertices.Add(c);
      //Rec.InvisibleEdge = Face.
    end;

    // check code (warning ñ may not be initialized)
    if (FaceVertices[0].typ = vtSide) and (c.typ <> vtSide) then begin
      for i := 0 to FaceVertices.Count - 1 do
        if FaceVertices[i].typ = vtSide then
          FaceVertices[i].typ := c.typ
        else
          break;
    end;
    if FaceVertices[0].typ = vtSide then
      exit;

    n := FaceVertices.Count;

  // 3. Find all the polygon intersections and insert them into both lists, linking the lists at the intersections.
    for i := n - 1 downto 0 do begin
      i2 := i + 1;
      if i2 = n then
        i2 := 0;

      if FaceVertices[i].typ <> FaceVertices[i2].typ then begin
        c1 := TVertex.Create;
        c2 := TVertex.Create;
        c1.Vector := CalcNewVertex(i, i2);
        c1.InvisibleEdge := FaceVertices[i].InvisibleEdge;
        if FaceVertices[i].typ = vtOutside then
          c1.typ := vtGoInside
        else
          c1.typ := vtGoOutside;

        c1.LinkedPoint := c2;
        FaceVertices.Insert(i + 1, c1);

        c2.Vector := c1.Vector;
        c2.LinkedPoint := c1;
        CutVertices.Add(c2);
      end;
    end;

    for i := 0 to FaceVertices.Count - 2 do
      FaceVertices[i].Next := FaceVertices[i + 1];
    FaceVertices[FaceVertices.Count - 1].Next := FaceVertices[0];

    // sorting Cut Points
    for c in CutVertices do
      c.Distance := c.Vector ** IntersectionVector;
    CutVertices.Sort;

    for i := 0 to CutVertices.Count - 2 do begin
      CutVertices[i].Next := CutVertices[i + 1];
      CutVertices[i].InvisibleEdge := InvisibleSlice;
    end;

  // 4. Generate a list of "inbound" intersections – the intersections where the vector from
  //    the intersection to the subsequent vertex of subject polygon B begins inside the clipping region.
  // 5. Follow each intersection clockwise around the linked lists until the start position is found.

    repeat
      // find first visible and not used
      first := nil;
      for i := 0 to FaceVertices.Count - 1 do begin
        if (FaceVertices[i].typ = vtOutside) and (not FaceVertices[i].used) then begin
          first := FaceVertices[i];
          break;
        end;
      end;

      if first = nil then
        break;

      TempVectors := TList<TVector>.Create;
      TempFace := TFace.Create(TempVectors);

      current := first;
      repeat
        current.used := True;
        TempFace.AddVector(current.Vector);

        if current.typ = vtGoInside then begin
          TempFace.SetEdgeVisibility(TempFace.GetVertexCount - 1, not InvisibleSlice);
          SlicePoint := current.LinkedPoint;
          SlicePoint := SlicePoint.Next;
          current := SlicePoint.LinkedPoint;
        end else begin
          TempFace.SetEdgeVisibility(TempFace.GetVertexCount - 1, not current.InvisibleEdge);
          current := current.next;
        end;

      until (current = nil) or (current = first) or current.used;
      TempFace.CheckFace;

      // because of lost points it must be collected in temporary list and then copied to final list
      NewFace := TFace.Create(NewVectors);
      NewFace.Color := Face.Color;
      NewFaces.Add(NewFace);
      for i := 0 to TempFace.GetVertexCount - 1 do
        NewFace.AddVector(TempFace.GetVertex(i), TempFace.IsEdgeVisible(i));
      TempFace.Free;
      TempVectors.Free;
      Result := True;

    until first = nil;
  finally
    for i := 0 to FaceVertices.Count - 1 do
      FaceVertices[i].Free;
    FaceVertices.Free;
    for i := 0 to CutVertices.Count - 1 do
      CutVertices[i].Free;
    CutVertices.Free;
  end;
end; // CutFace

class function TFaceUtils.FacesAreSameButOpposite(Face1, Face2: TFace): Boolean;
var
  i, j, n, idx: integer;
  v0: TVector;
begin
  Result := False;
  n := Face1.GetVertexCount;
  if n <> Face2.GetVertexCount then
    exit;

  v0 := Face1.GetVertex(0);
  for i := 0 to n - 1 do begin
    if v0.IsNear(Face2.GetVertex(i)) then begin
      idx := i;

      for j := 1 to n - 1 do
        if not Face1.GetVertex(j).IsNear(Face2.GetVertex(idx - j)) then
          exit;
      Result := True;
      exit;
    end;
  end;
end; // FacesAreSameButOpposite

class function TFaceUtils.CombineFaces(Face1, Face2, NewFace: TFace): Boolean;

  procedure AddIfNotStraight(i1, i2, i3: integer);
  var V1, V2{, V3}: TVector;
  begin
    V1 := Face1.GetVertex(i1);
    V2 := V1 - Face1.GetVertex(i2);
//    V3 := V1 - Face2.GetVertex(i3);
    if (V1 * V2).Abs < DELTA * V1.Abs * V2.Abs then
      NewFace.AddVector(V1);
  end;

var
  i, j, n1, n2, idx1, idx2: integer;
  Found: Boolean;
  v1: TVector;
begin // CombineFaces
  Result := False;

  // check same color
  if Face1.Color <> Face2.Color then
    exit;

  // check same plane
  if not Face1.GetNorm.IsNear(Face2.GetNorm) then
    exit;

  // find common section
  n1 := Face1.GetVertexCount;
  n2 := Face2.GetVertexCount;
  idx1 := -1;
  idx2 := -1;
  Found := False;
  for i := 0 to n1 - 1 do begin
    v1 := Face1.GetVertex(i);
    for j := 0 to n2 - 1 do begin
      if v1.IsNear(Face2.GetVertex(j)) then begin
        if Face1.GetVertex(i + 1).IsNear(Face2.GetVertex(j - 1)) then begin
          Found := True;
          idx1 := i;
          idx2 := j;
          break;
        end;
      end;
    end;
  end;
  if not Found then
    exit;

  NewFace.Color := Face1.Color;

  NewFace.TextureId := Face1.TextureId;
  if NewFace.TextureId = -1 then
    NewFace.TextureId := Face2.TextureId;

  // add all points except 180-degrees nodes

  AddIfNotStraight(idx1 + 1, idx1 + 2, idx2 - 2);
  for i := 0 to n1 - 3 do
    NewFace.AddVector(Face1.GetVertex(idx1 + 2 + i));
  AddIfNotStraight(idx1, idx1 - 2, idx2 + 1);
  for i := 0 to n2 - 3 do
    NewFace.AddVector(Face2.GetVertex(idx2 + 1 + i));

  Result := True;
end; // CombineFaces

end.
