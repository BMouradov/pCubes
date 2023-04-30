{************************************************************}
{                                                            }
{  Module uPuzzleUtils                                       }
{  2014-2020                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uPuzzleUtils;

{$mode Delphi}

interface
uses uVector, uPuzzle, uFace, uPlane, uPart,
  Generics.Collections, Graphics;

type TPuzzleUtils = class
  class function CountUniquePlanes(Puzzle: TPuzzle): integer; static;
  class function GetColoredFacesCount(Puzzle: TPuzzle): integer; static;
  class procedure ListUniqueColors(Puzzle: TPuzzle; Colors: TList<TColor>; IgnoreInvisibleEdges: Boolean = True); static;
  class function CheckColorHasSingleDirection(Puzzle: TPuzzle; Color: TColor; WithOpposite: Boolean): Boolean; static;
  class function CheckEachColorHasSingleDirection(Puzzle: TPuzzle; WithOpposite: Boolean): Boolean; static;
  class procedure GetColoredFaces(Puzzle: TPuzzle; Faces: TList<TFace>); static;
  class function CheckEachFaceConsistsOfDifferentColors(Puzzle: TPuzzle): Boolean; static; // Sudoku
  class function CheckEachRowColConsistsOfDifferentColors(Puzzle: TPuzzle): Boolean; static; // Sudoku IV
  class function CheckEachRowColDiagConsistsOfDifferentColors(Puzzle: TPuzzle): Boolean; static; // Sudoku 4
  class function CheckEachPlaneHasOneColor(Puzzle: TPuzzle): Boolean; static; // Triakis Octahedron Alternate Coloring (DaYan Gem 6)
  class function CheckAdjoiningFacesHaveSameColors(Puzzle: TPuzzle): Boolean; // Tetravex
  class function SideHasTexture(Puzzle: TPuzzle; AxisNo: integer; Dir: Boolean; TextureId: integer): Boolean; static;
  class procedure RemoveGrayParts(Puzzle: TPuzzle); static;
  class procedure CutByPlane(Puzzle: TPuzzle; Plane: TPlane; Color: TColor = -1; InvisibleEdges: integer = 0); static;
  class procedure SplitByPlane(Puzzle: TPuzzle; Plane: TPlane; Color1: TColor = -1; Color2: TColor = -1; InvisibleEdges1: integer = 0; InvisibleEdges2: integer = 0); static;
  class procedure CombineParts(Puzzle: TPuzzle; Part1, Part2: TPart); static;
  class procedure InvertVisibility(Puzzle: TPuzzle); static;
  class procedure SetAllInvisible(Puzzle: TPuzzle); static;
  class procedure SetAllVisible(Puzzle: TPuzzle); static;
  class procedure SetPartVisible(Puzzle: TPuzzle; PartNo: integer); static;
  class procedure SplitByAxes(Puzzle: TPuzzle); static;
  class function GetVisiblePartContaining(Puzzle: TPuzzle; Vector: TVector): TPart; static;
  class function GetNearestVisibleFace(Puzzle: TPuzzle; Vector: TVector): TFace; static;
end;

implementation
uses uAxis, uPartUtils, uGeometryUtils, SysUtils;

class function TPuzzleUtils.CountUniquePlanes(Puzzle: TPuzzle): integer;
var
  pl, Plane: TPlane;
  Part: TPart;
  Face: TFace;
  Planes: TList<TPlane>;
  Found: Boolean;
begin
  Planes := TList<TPlane>.Create;

  for Part in Puzzle.Parts do begin
    if Part.Visible then
      for Face in Part.Faces do begin
        if Face.IsColored and Face.AllEdgesVisible then begin
          pl := Face.GetPlane;
          Found := False;
          for Plane in Planes do
            if pl.IsNear(Plane) then begin
              Found := True;
              break;
            end;
          if not Found then
            Planes.Add(pl);
        end;
      end;
  end;

  Result := Planes.Count;
  Planes.Free;
end; // CountUniquePlanes

class function TPuzzleUtils.GetColoredFacesCount(Puzzle: TPuzzle): integer;
var
  Part: TPart;
  Face: TFace;
begin
  Result := 0;
  for Part in Puzzle.Parts do
    if Part.Visible then
      for Face in Part.Faces do
        if Face.Color >= 0 then
          Inc(Result);
end;

class procedure TPuzzleUtils.ListUniqueColors(Puzzle: TPuzzle; Colors: TList<TColor>; IgnoreInvisibleEdges: Boolean = True);
var
  Color, LastColor: TColor;
  Face: TFace;
  Part: TPart;
begin
  LastColor := -1;

  for Part in Puzzle.Parts do
    if Part.Visible then
      for Face in Part.Faces do begin
        Color := Face.Color;
        if Face.IsColored and (Color <> LastColor) and ((not IgnoreInvisibleEdges) or Face.AllEdgesVisible) then begin
          if not Colors.Contains(Color) then
            Colors.Add(Color);
          LastColor := Color;
        end;
      end;
end; // ListUniqueColors

class function TPuzzleUtils.CheckColorHasSingleDirection(Puzzle: TPuzzle; Color: TColor; WithOpposite: Boolean): Boolean;
var
  Face: TFace;
  Part: TPart;
  First: Boolean;
  v1, v2: TVector;
begin
  Result := False;
  First := True;
  for Part in Puzzle.Parts do
    if Part.Visible then
      for Face in Part.Faces do
        if (Face.Color = Color) and Face.AllEdgesVisible then begin
          if First then begin
            v1 := Face.GetNorm;
            First := False;
          end else begin
            v2 := Face.GetNorm;
            if WithOpposite then begin
              if not v2.IsNear(v1) and not v2.IsNearOpposite(v1) then
                exit;
            end else
              if not v2.IsNear(v1) then
                exit;
          end;
        end;
  Result := True;
end; // CheckColorHasSingleDirection

class function TPuzzleUtils.CheckEachColorHasSingleDirection(Puzzle: TPuzzle; WithOpposite: Boolean): Boolean;
var
  Colors: TList<TColor>;
  Color: TColor;
begin
  Colors := TList<TColor>.Create;
  Result := False;
  try
    ListUniqueColors(Puzzle, Colors);

    // check each color: all faces with this color must have the same Norm Vector
    for Color in Colors do
      if not CheckColorHasSingleDirection(Puzzle, Color, WithOpposite) then
        exit;

    Result := True;
  finally
    Colors.Free;
  end;
end; // CheckEachColorHasSingleDirection

//class function TPuzzleUtils.CountUniqueNormals(Puzzle: TPuzzle): integer;
//var
//  n, v: TVector;
//  Part: TPart;
//  Face: TFace;
//  Vectors: TList<TVector>;
//  Found: Boolean;
//begin
//  Vectors := TList<TVector>.Create;
//
//  for Part in Puzzle.Parts do begin
//    for Face in Part.Faces do begin
//      if Face.IsColored then begin
//        n := Face.GetNorm;
//        Found := False;
//        for v in Vectors do begin
//          if n.IsNear(v) then begin
//            Found := True;
//            break;
//          end;
//        end;
//        if not Found then
//          Vectors.Add(v);
//      end;
//    end;
//  end;
//
//  Result := Vectors.Count;
//  Vectors.Free;
//end; // CountUniqueNormals

class procedure TPuzzleUtils.GetColoredFaces(Puzzle: TPuzzle; Faces: TList<TFace>);
var
  Part: TPart;
  Face: TFace;
begin
  for Part in Puzzle.Parts do
    if Part.Visible then
      for Face in Part.Faces do
        if Face.IsColored then
          Faces.Add(Face);
end; // GetColoredFaces

class function TPuzzleUtils.CheckEachFaceConsistsOfDifferentColors(Puzzle: TPuzzle): Boolean; // Sudoku
var
  i, j: integer;
  pl1, pl2: TPlane;
  Face1, Face2: TFace;
  Faces: TList<TFace>;
begin
  Result := False;
  Faces := TList<TFace>.Create;
  try
    GetColoredFaces(Puzzle, Faces);

    for i := 0 to Faces.Count - 2 do begin
      Face1 := Faces[i];
      pl1 := Face1.GetPlane;
      for j := i + 1 to Faces.Count - 1 do begin
        Face2 := Faces[j];
        if Face1.Color = Face2.Color then begin
          pl2 := Face2.GetPlane;
          if pl1.IsNear(pl2) then
            exit;
        end;
      end;
    end;
  finally
    Faces.Free;
  end;
  Result := True;
end; // CheckEachFaceConsistsOfDifferentColors

class function TPuzzleUtils.CheckEachRowColConsistsOfDifferentColors(Puzzle: TPuzzle): Boolean; // Sudoku 4
var Coords1, Coords2: array [0..2] of integer;

  procedure CheckPartBetweenLayers(PartNo: integer; var Coords: array of integer);
  var i, iFrom, iTo: integer;
  begin
    for i := 0 to Puzzle.Axes.Count - 1 do begin
      Puzzle.GetPartPosition(i, PartNo, iFrom, iTo);
      if iFrom = iTo then
        Coords[i] := iFrom;
    end;
  end; // GetCoords

  function CheckRowCol(i0, i1, i2: integer): Boolean;
  begin
    Result := (Coords1[i0] = Coords2[i0]) and
      ( (Coords1[i1] = Coords2[i1]) or // Row
        (Coords1[i2] = Coords2[i2])   // Col
      );
  end; // CheckRowCol

var
  i, k: integer;
  pl1, pl2: TPlane;
  Part1, Part2: TPart;
  Face1, Face2: TFace;
begin // CheckEachRowColConsistsOfDifferentColors
  Result := False;
  for i := 0 to Puzzle.Parts.Count - 2 do begin
    Part1 := Puzzle.Parts[i];
    if not Part1.Visible then
      continue;
    CheckPartBetweenLayers(i, Coords1);
    for Face1 in Part1.Faces do
      if Face1.IsColored then begin
        pl1 := Face1.GetPlane;
        for k := i + 1 to Puzzle.Parts.Count - 1 do begin
          Part2 := Puzzle.Parts[k];
          if not Part2.Visible then
            continue;
          for Face2 in Part2.Faces do
            if Face1.Color = Face2.Color then begin
              pl2 := Face2.GetPlane;
              if pl1.IsNear(pl2) then begin
                CheckPartBetweenLayers(k, Coords2);
                if CheckRowCol(0, 1, 2) or CheckRowCol(1, 2, 0) or CheckRowCol(2, 0, 1) then
                  exit;
              end;
            end;
        end;
      end;
  end;
  Result := True;
end; // CheckEachRowColConsistsOfDifferentColors

class function TPuzzleUtils.CheckEachRowColDiagConsistsOfDifferentColors(Puzzle: TPuzzle): Boolean; // Sudoku 4
var Coords1, Coords2: array [0..2] of integer;

  procedure CheckPartBetweenLayers(PartNo: integer; var Coords: array of integer);
  var i, iFrom, iTo: integer;
  begin
    for i := 0 to Puzzle.Axes.Count - 1 do begin
      Puzzle.GetPartPosition(i, PartNo, iFrom, iTo);
      if iFrom = iTo then
        Coords[i] := iFrom;
    end;
  end; // GetCoords

  function CheckRowColDiag(i0, i1, i2: integer): Boolean;
  begin
    Result := (Coords1[i0] = Coords2[i0]) and
      ( (Coords1[i1] = Coords2[i1]) or // Row
        (Coords1[i2] = Coords2[i2]) or // Col
        ((Coords1[i1] = Coords1[i2]) and (Coords2[i1] = Coords2[i2])) or //Diag1
        ((Coords1[i1] = 3 - Coords1[i2]) and (Coords2[i1] = 3 - Coords2[i2])) //Diag2
      );
  end; // CheckRowColDiag

var
  i, k: integer;
  pl1, pl2: TPlane;
  Part1, Part2: TPart;
  Face1, Face2: TFace;
begin // CheckEachRowColDiagConsistsOfDifferentColors
  Result := False;
  for i := 0 to Puzzle.Parts.Count - 2 do begin
    Part1 := Puzzle.Parts[i];
    if not Part1.Visible then
      continue;
    CheckPartBetweenLayers(i, Coords1);
    for Face1 in Part1.Faces do
      if Face1.IsColored then begin
        pl1 := Face1.GetPlane;
        for k := i + 1 to Puzzle.Parts.Count - 1 do begin
          Part2 := Puzzle.Parts[k];
          if not Part2.Visible then
            continue;
          for Face2 in Part2.Faces do
            if Face1.Color = Face2.Color then begin
              pl2 := Face2.GetPlane;
              if pl1.IsNear(pl2) then begin
                CheckPartBetweenLayers(k, Coords2);
                if CheckRowColDiag(0, 1, 2) or CheckRowColDiag(1, 2, 0) or CheckRowColDiag(2, 0, 1) then
                  exit;
              end;
            end;
        end;
      end;
  end;
  Result := True;
end; // CheckEachRowColDiagConsistsOfDifferentColors

class function TPuzzleUtils.CheckEachPlaneHasOneColor(Puzzle: TPuzzle): Boolean; // Triakis Octahedron Alternate Coloring (DaYan Gem 6)
var
  k: integer;
  Part: TPart;
  Face: TFace;
  pl: TPlane;
  Color: TColor;
//  Planes: TPlanes;
  Found: Boolean;
  Planes: TList<TPlane>;
  Colors: TList<TColor>;
begin
//  Planes := TPlanes.Create;
  Result := False;
  Planes := TList<TPlane>.Create;
  Colors := TList<TColor>.Create;
  try

    for Part in Puzzle.Parts do
      if Part.Visible then
        for Face in Part.Faces do
          if Face.IsColored and Face.AllEdgesVisible then begin
            Color := Face.Color;
            pl := Face.GetPlane;
            Found := False;
            for k := 0 to Planes.Count - 1 do
              if pl.IsNear(Planes[k]) then begin
                if Color <> Colors[k] then
                  exit
                else begin
                  Found := True;
                  break;
                end;
              end;
            if not Found then begin
              Planes.Add(pl);
              Colors.Add(Color);
            end;
          end;
    Result := True;
  finally
    Colors.Free;
    Planes.Free;
  end;
end; // CheckEachPlaneHasOneColor

class function TPuzzleUtils.CheckAdjoiningFacesHaveSameColors(Puzzle: TPuzzle): Boolean; // Tetravex
var
  i, j: integer;
  Part1, Part2: TPart;
  Face1, Face2: TFace;
begin
  for Part1 in Puzzle.Parts do
    if Part1.Visible then
      for Face1 in Part1.Faces do
        if Face1.IsColored {and Face1.AllEdgesVisible} then
          for Part2 in Puzzle.Parts do
            if Part2.Visible and (Part1 <> Part2) then
              for Face2 in Part2.Faces do
                if Face2.IsColored {and Face2.AllEdgesVisible} then
                  if Face1.Color <> Face2.Color then
                    for i := 0 to Face1.GetVertexCount - 1 do
                      for j := 0 to Face2.GetVertexCount - 1 do
                        if Face1.GetVertex(i).IsNear(Face2.GetVertex(j)) then
                          if Face1.GetVertex(i + 1).IsNear(Face2.GetVertex(j - 1)) then
                            Exit(False);
  Result := True;
end; // CheckAdjoiningFacesHaveSameColors

class function TPuzzleUtils.SideHasTexture(Puzzle: TPuzzle; AxisNo: integer; Dir: Boolean; TextureId: integer): Boolean;
var
  norm, AxisVector: TVector;
  Face: TFace;
  Part: TPart;
begin
  Result := True;
  AxisVector := Puzzle.Axes[AxisNo].NormVector;
  for Part in Puzzle.Parts do
    if Part.Visible then
      for Face in Part.Faces do
        if Face.TextureId = TextureId then begin
          norm := Face.GetNorm;
          if ((Dir = False) and norm.IsNearOpposite(AxisVector)) or
             ((Dir = True) and norm.IsNear(AxisVector)) then
            exit;
        end;

  Result := False;
end; // SideHasTexture

class procedure TPuzzleUtils.RemoveGrayParts(Puzzle: TPuzzle);

  function PartIsColored(const Part: TPart): Boolean;
  var Face: TFace;
  begin
    Result := True;
    for Face in Part.Faces do
      if Face.Color <> -1 then
        exit;
    Result := False;
  end; // PartIsColored

var
  i: integer;
  Part: TPart;
begin
  for i := Puzzle.Parts.Count - 1 downto 0 do begin
    Part := Puzzle.Parts[i];
    if Part.Visible and not PartIsColored(Part) then begin
      Part.Free;
      Puzzle.Parts.Delete(i);
    end;
  end;
end; // RemoveGrayParts

class procedure TPuzzleUtils.CutByPlane(Puzzle: TPuzzle; Plane: TPlane; Color: TColor = -1; InvisibleEdges: integer = 0);
var
  i: integer;
  hp: THalfOfPlane;
  NewParts: TList<TPart>;
  Part: TPart;
begin
  NewParts := TList<TPart>.Create;
  try
    for i := Puzzle.Parts.Count - 1 downto 0 do begin
      Part := Puzzle.Parts[i];
      if Part.Visible then begin
        hp := Part.HalfPlane(Plane);
        if hp = hopIntersect then begin
          if TPartUtils.CutPart(Part, Plane, NewParts, Color, InvisibleEdges) then begin
            Part.Free;
            Puzzle.Parts.Delete(i);
            Puzzle.Parts.InsertRange(i, NewParts);
            NewParts.Clear;
          end;
        end else if hp = hopInside then begin
          Part.Free;
          Puzzle.Parts.Delete(i);
        end;
      end;
    end;
    Puzzle.PositionsInvalidate;

  finally
    NewParts.Free;
  end;
end; // Cut

class procedure TPuzzleUtils.SplitByPlane(Puzzle: TPuzzle; Plane: TPlane; Color1: TColor = -1; Color2: TColor = -1; InvisibleEdges1: integer = 0; InvisibleEdges2: integer = 0);
var
  i: integer;
  Copy, Part: TPart;
  OppositePlane: TPlane;
  NewParts: TList<TPart>;
begin
  OppositePlane := Plane;
  OppositePlane.TurnRound;

  NewParts := TList<TPart>.Create;
  try

    for i := Puzzle.Parts.Count - 1 downto 0 do begin
      Part := Puzzle.Parts[i];
      if Part.Visible and (Part.HalfPlane(Plane) = hopIntersect) then begin
        Copy := TPart.Create;
        Copy.Assign(Part);

        TPartUtils.CutPart(Part, Plane, NewParts, Color1, InvisibleEdges1);
        Part.Free;
        Puzzle.Parts.Delete(i);
        Puzzle.Parts.InsertRange(i, NewParts);
        NewParts.Clear;

        TPartUtils.CutPart(Copy, OppositePlane, NewParts, Color2, InvisibleEdges2);
        Puzzle.Parts.InsertRange(i, NewParts);
        NewParts.Clear;
        Copy.Free;
      end;
    end;
    Puzzle.PositionsInvalidate;

  finally
    NewParts.Free;
  end;
end; // Split

class procedure TPuzzleUtils.SplitByAxes(Puzzle: TPuzzle);
var
  i: integer;
  Axis: TAxis;
begin
  for Axis in Puzzle.Axes do
    for i := 0 to Axis.Layers.Count - 2 do
      SplitByPlane(Puzzle, Axis.GetLayerPlaneTo(i));
end; // SplitByAxes

class procedure TPuzzleUtils.CombineParts(Puzzle: TPuzzle; Part1, Part2: TPart);
var
  n1, n2: integer;
  NewPart: TPart;
begin // CombineParts
  NewPart := TPart.Create;

  if TPartUtils.CombineParts(Part1, Part2, NewPart) then begin
    n1 := Puzzle.Parts.Remove(Part1);
    Part1.Free;
    n2 := Puzzle.Parts.Remove(Part2);
    if n2 < n1 then
      Dec(n1);
    Part2.Free;
    Puzzle.Parts.Insert(n1, NewPart);
    Puzzle.PositionsInvalidate;
  end else
    NewPart.Free;
end; // CombineParts

class procedure TPuzzleUtils.SetAllVisible(Puzzle: TPuzzle);
var Part: TPart;
begin
  for Part in Puzzle.Parts do
    Part.Visible := True;
end;

class procedure TPuzzleUtils.SetAllInvisible(Puzzle: TPuzzle);
var Part: TPart;
begin
  for Part in Puzzle.Parts do
    Part.Visible := False;
end;

class procedure TPuzzleUtils.InvertVisibility(Puzzle: TPuzzle);
var Part: TPart;
begin
  for Part in Puzzle.Parts do
    Part.Visible := not Part.Visible;
end;

class procedure TPuzzleUtils.SetPartVisible(Puzzle: TPuzzle; PartNo: integer);
begin
  if (PartNo >= 0) and (PartNo < Puzzle.Parts.Count) then
    Puzzle.Parts[PartNo].Visible := True;
end;

//------------------------------------------------------------------------------

// todo move to PuzzleUtils
//class procedure TBasePuzzle.CheckPuzzle(Puzzle: TPuzzle);
//var i, j, k, l: integer;
//    Found: Boolean;
//    v: TVector;
//begin
//  // ищем вершины, которые не используются
//  for i := 0 to FParts.Count - 1 do
//    for j := 0 to FParts[i].Vectors.Count - 1 do begin
//      Found := False;
//      for k := 0 to FParts[i].Faces.Count - 1 do
//        for l := 0 to FParts[i].Faces[k].GetVertexCount - 1 do
//          if FParts[i].Faces[k].GetVertex(l).IsNear(FParts[i].Vectors[j]) then begin
//            Found := True;
//            break;
//          end;
//      if not Found then
//        ShowMessage('vertex p:' + IntToStr(i) + ' v:' + IntToStr(j) + ' not used found!');
//    end;
//
//  for i := 0 to FParts.Count - 1 do
//    for j := 0 to FParts[i].Vectors.Count - 1 do begin
//      v.Assign(FParts[i].Vectors[j]);
//      Log(FloatToStr(v.ScalarMult(Axes[0].NormVector)));
//    end;
//end;

//==============================================================================
// utils
//==============================================================================

//procedure TBasePuzzle.GetPlanesFromFaces(Parts: TList<TPart>; Planes: TList<TPlane>);
//
//  function SameOrOppositePlaneExists(pl: TPlane): Boolean;
//  var Plane: TPlane;
//  begin
//    Result := True;
//    for Plane in Planes do
//      if Plane.IsNear(pl) or Plane.IsOpposite(pl) then
//        exit;
//    Result := False;
//  end; // SameOrOppositePlaneExists
//
//var
//  pl: TPlane;
//  Face: TFace;
//begin // GetPlanesFromFaces
//  for Part in Parts do
//    for Face in Part.Faces do begin
//      pl := TPlane.Create;
//      Face.GetPlane(pl);
//      if not SameOrOppositePlaneExists(pl) then
//        Planes.Add(pl)
//      else
//        pl.Free;
//    end;
//end; // GetPlanesFromFaces

class function TPuzzleUtils.GetVisiblePartContaining(Puzzle: TPuzzle; Vector: TVector): TPart;
var Part: TPart;
begin
  Result := nil;
  for Part in Puzzle.Parts do
    if Part.Visible and Part.Contains(Vector) then
      exit(Part);
end;

class function TPuzzleUtils.GetNearestVisibleFace(Puzzle: TPuzzle; Vector: TVector): TFace;
var Part: TPart;
begin
  Part := GetVisiblePartContaining(Puzzle, Vector);
  if Part <> nil then
    Result := TPartUtils.GetNearestFace(Part, Vector)
  else
    Result := nil;
end;

end.
