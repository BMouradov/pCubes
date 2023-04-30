{************************************************************}
{                                                            }
{  Unit uSolvedState                                         }
{  2017-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uSolvedState;
{$mode delphi}

interface
uses
  Generics.Collections, uVector;

type
  TFaceProperties = class
    vc, vn: TVector; // center and normal
    ColorIndex: integer;
    constructor Create;
  end;

type
  TSolvedState = class
    FacesProperties: TList<TFaceProperties>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CalcFromPuzzle(Puzzle: Pointer);
    function CompareTo(State2: TSolvedState): Boolean;
  end;

type
  TSolvedStates = class
  private
    FSolvedStates: TList<TSolvedState>;
  public
    UniquePlanes: integer; // for mirror cube and others to check form
    property SolvedStates: TList<TSolvedState> read FSolvedStates;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

implementation
uses
  uPuzzle, uFace, uPart, uPuzzleUtils,
  Graphics, Dialogs;

// ========================== TFaceProperties ==============================

constructor TFaceProperties.Create;
begin
  ColorIndex := -1;
end;

// ===========================  TSolvedState ===============================

constructor TSolvedState.Create;
begin
  FacesProperties := TList<TFaceProperties>.Create;
end;

destructor TSolvedState.Destroy;
var i: integer;
begin
  for i := 0 to FacesProperties.Count - 1 do
    FacesProperties[i].Free;
  FacesProperties.Free;

  inherited;
end;

procedure TSolvedState.CalcFromPuzzle(Puzzle: Pointer);
var FPuzzle: TPuzzle;
  v1, v2, v3: TVector;
  Colors: TList<TColor>;

  procedure AddFace(Face: TFace);
  var i: integer;
    c1, c2, c3: extended;
    fp: TFaceProperties;
  begin
    fp := TFaceProperties.Create;

    fp.ColorIndex := Colors.IndexOf(Face.Color);

    // calc center
    fp.vc := Face.GetVertex(0);
    for i := 1 to Face.GetVertexCount - 1 do begin
      //fp.vc.Add(Face.Vertices[i]);
      fp.vc.X := fp.vc.X + Face.GetVertex(i).X;
      fp.vc.Y := fp.vc.Y + Face.GetVertex(i).Y;
      fp.vc.Z := fp.vc.Z + Face.GetVertex(i).Z;
    end;
    fp.vc := fp.vc / Face.GetVertexCount;

    // position of center in v1, v2, v3 basis
    c1 := fp.vc ** v1;
    c2 := fp.vc ** v2;
    c3 := fp.vc ** v3;
    fp.vc.X := c1;
    fp.vc.Y := c2;
    fp.vc.Z := c3;

    // normal to face in v1, v2, v3 basis
    fp.vn := Face.GetNorm;
    c1 := fp.vn ** v1;
    c2 := fp.vn ** v2;
    c3 := fp.vn ** v3;
    fp.vn.X := c1;
    fp.vn.Y := c2;
    fp.vn.Z := c3;

    FacesProperties.Add(fp);
  end; // AddFace

var i, j: integer;
begin // CalcFromPuzzle
  FPuzzle := TPuzzle(Puzzle);

  if FPuzzle.Axes.Count < 2 then begin
    ShowMessage('TSolvedState.CalcFromPuzzle: Axes.Count < 2');
    exit;
  end;

  v1 := FPuzzle.Axes[0].NormVector;
  v2 := FPuzzle.Axes[1].NormVector;
  v2 := v2 * v1;

  if v2.Abs < 1e-4 then begin
    ShowMessage('TSolvedState.CalcFromPuzzle: first and second axes can''t be parallel');
    exit;
  end;

  v2.Normalize;
  v3 := v2;
  v3 := v3 * v1;
  v3.Normalize;

  Colors := TList<TColor>.Create;
  TPuzzleUtils.ListUniqueColors(FPuzzle, Colors, False);

  for i := 0 to FPuzzle.Parts.Count - 1 do
    for j := 0 to FPuzzle.Parts[i].Faces.Count - 1 do
      if FPuzzle.Parts[i].Faces[j].Color <> -1 then
        AddFace(FPuzzle.Parts[i].Faces[j]);

  Colors.Free;
end; // CalcFromPuzzle

function TSolvedState.CompareTo(State2: TSolvedState): Boolean;
var l1, l2: TList<TFaceProperties>;
  i, i1, j, ci1, ci2: integer;
  fp1, fp2: TFaceProperties;
  Found: Boolean;
begin

  Result := False;
  if FacesProperties.Count <> State2.FacesProperties.Count then
    exit;

  l1 := TList<TFaceProperties>.Create;
  l1.AddRange(FacesProperties);

  l2 := TList<TFaceProperties>.Create;
  l2.AddRange(State2.FacesProperties);

  try

    for i := 0 to l1.Count - 1 do begin
      fp1 := l1[i];
      if fp1 <> nil then begin

        // find new color at this coordinate
        ci1 := fp1.ColorIndex;
        ci2 := -1;
        for j := 0 to l2.Count - 1 do begin
          fp2 := l2[j];
          if fp2 <> nil then
            if fp1.vc.IsNear(fp2.vc) and fp1.vn.IsNear(fp2.vn) then begin
              ci2 := fp2.ColorIndex;
              l1[i] := nil;
              l2[j] := nil;
              break;
            end;
        end;
        if ci2 = -1 then
          exit;

        // find and remove all with that color
        for i1 := 0 to l1.Count - 1 do begin
          fp1 := l1[i1];
          if fp1 <> nil then
            if fp1.ColorIndex = ci1 then begin
              Found := False;
              for j := 0 to l2.Count - 1 do begin
                fp2 := l2[j];
                if fp2 <> nil then
                  if fp2.ColorIndex = ci2 then
                    if fp1.vc.IsNear(fp2.vc) and fp1.vn.IsNear(fp2.vn) then begin
                      l1[i1] := nil;
                      l2[j] := nil;
                      Found := True;
                      break;
                    end;
              end;
              if not Found then
                exit;
            end;
        end;
      end;
    end;

  finally
    l1.Free;
    l2.Free;
  end;
  Result := True;
end; // CompareTo

// =========================== TSolvedStates ===============================

constructor TSolvedStates.Create;
begin
  FSolvedStates := TList<TSolvedState>.Create;
end;

destructor TSolvedStates.Destroy;
begin
  Clear;
  FSolvedStates.Free;
  inherited;
end;

procedure TSolvedStates.Clear;
var i: integer;
begin
  for i := 0 to FSolvedStates.Count - 1 do
    FSolvedStates[i].Free;
  FSolvedStates.Clear;
end;

end.
