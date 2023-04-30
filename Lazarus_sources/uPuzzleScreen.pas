{************************************************************}
{                                                            }
{  Unit Name: uPuzzleScreen                                  }
{  2019-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{  Converts screen coords into motion                        }
{************************************************************}

unit uPuzzleScreen;

{$mode Delphi}

interface
uses uPuzzle, uBaseGraphics;

type TPuzzleScreen = class
  FPuzzle: TPuzzle;

  constructor Create(Puzzle: TPuzzle);

  function GetRotateDirection(dx, dy: extended): integer;
  function GetRotateAxis(GraphicEngine: TBaseGraphics; dx, dy: extended): integer;
end;

implementation
uses
  uVector, uAxis, uFace, uSelection, uGeometryUtils,
  Math, Generics.Collections;

constructor TPuzzleScreen.Create(Puzzle: TPuzzle);
begin
  FPuzzle := Puzzle;
end;

// 1 - clockwise -1 - counterclockwise
function TPuzzleScreen.GetRotateDirection(dx, dy: extended): integer;
var v: TVector;
begin
  v := [dx, 0, dy];
  v := v * TSelection.SelectedFace.GetNorm;
//  v.Normalize;

  Result := -Sign(v ** TSelection.TurnAxis.NormVector);
end;

function TPuzzleScreen.GetRotateAxis(GraphicEngine: TBaseGraphics; dx, dy: extended): integer;
const delta = 1e-3;
var
  v, RotationVector: TVector;
  h1, h: extended;
  i, AxisNo: integer;
  First: Boolean;
  NAxes: TList<TAxis>;
  Face: TFace;
  Axis: TAxis;
begin // GetRotateAxis
//  Result := -1;
//  TSelection.TurnAxis := nil;
//
//  RotationVector := [dx, 0, dy];
//
//  h := 0;
//  First := True;
//  for AxisNo := 0 to FPuzzle.Axes.Count - 1 do begin
//    Axis := FPuzzle.Axes[AxisNo];
//
//    //FPuzzle.CalcPartPositionsForAxis(AxisNo);
//    if not FPuzzle.CalcMovingLayersFromPart(AxisNo, TSelection.SelectedPart) then
//      continue;
//    if Axis.IsLayerFixed(TSelection.TurnLayers) then // delete ??
//      continue;
//
//    v := Axis.NormVector * (TSelection.MouseOver - Axis.BaseVector);
//    v.Y := 0;
//    v.Normalize;
//
//    h1 := Abs(RotationVector.ScalarMult(v));
//
//    if (Result = -1) or (h1 > h) then begin
//      h := h1;
//      Result := AxisNo;
//    end;
//  end;
  NAxes := TList<TAxis>.Create;
  try
    RotationVector := [dx, 0, dy];
    RotationVector := RotationVector * TSelection.SelectedFace.GetNorm;
    RotationVector.Normalize;

    Result := -1;

    h := 0;
    First := True;
    for AxisNo := 0 to FPuzzle.Axes.Count - 1 do begin
      Axis := FPuzzle.Axes[AxisNo];

      //FPuzzle.CalcPartPositionsForAxis(AxisNo);
      if not FPuzzle.CalcMovingLayersFromPart(AxisNo, TSelection.SelectedPart) then
        continue;
      if Axis.IsLayerFixed(TSelection.TurnLayers) then
        continue;

      h1 := Abs(RotationVector ** Axis.NormVector);

      if First then begin
        First := False;
        h := h1;
        Result := AxisNo;
        NAxes.Add(Axis);
      end else begin
        if Abs(h1 - h) < delta then
          NAxes.Add(Axis)
        else if h1 > h then begin
          NAxes.Clear;
          NAxes.Add(Axis);
        end;

        if h1 > h then begin
          h := h1;
          Result := AxisNo;
        end;
      end;
    end;

    if NAxes.Count = 0 then
      TSelection.TurnAxis := nil;

    // if there are two proper axes
    if NAxes.Count >= 2 then begin
      // find center of face and distance to it
      Face := TSelection.SelectedFace;
      v := Face.GetVertex(0);
      for i := 1 to Face.GetVertexCount - 1 do
        v := v + Face.GetVertex(i);
      v := v / Face.GetVertexCount;
      h1 := TGeometryUtils.DistancePointToLine(NAxes[0].Line, v);

      // find distance from touchpoint to first axis. Select axis if it's far than distance from center
      v := TSelection.MouseOver;
      h := TGeometryUtils.DistancePointToLine(NAxes[0].Line, v);
      if h < h1 then
        Result := FPuzzle.Axes.IndexOf(NAxes[1])
      else
        Result := FPuzzle.Axes.IndexOf(NAxes[0]);
    end;

//    if Result <> -1 then
//      FPuzzle.CalcPartPositionsForAxis(Result);
  finally
    NAxes.Free;
  end;
end; // GetRotateAxis

end.
