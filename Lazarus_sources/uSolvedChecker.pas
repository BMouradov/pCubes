{************************************************************}
{                                                            }
{  Unit uSolvedChecker                                       }
{  2019-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uSolvedChecker;

interface
uses
  uPuzzle;

type
  TSolvedChecker = class
  private
    FPuzzle: TPuzzle;

    function CheckSolved: Boolean;
//    function CountUniqueNormals: integer;
  public
    constructor Create(Puzzle: TPuzzle);
    function IsSolved: Boolean;
  end;

implementation

uses uPuzzleUtils, uSolvedState;

constructor TSolvedChecker.Create(Puzzle: TPuzzle);
begin
  FPuzzle := Puzzle;
end;

function TSolvedChecker.IsSolved: Boolean;
begin
  case FPuzzle.CheckSolvedProc of
    cspEachColorHasSameOrOppositeDirection:          Result := TPuzzleUtils.CheckEachColorHasSingleDirection(FPuzzle, True);
    cspCountUniquePlanes:                            Result := TPuzzleUtils.CountUniquePlanes(FPuzzle) = FPuzzle.SolvedStates.UniquePlanes;
    cspEachFaceConsistsOfDifferentColors:            Result := TPuzzleUtils.CheckEachFaceConsistsOfDifferentColors(FPuzzle);
    cspEachRowColConsistsOfDifferentColors:          Result := TPuzzleUtils.CheckEachRowColConsistsOfDifferentColors(FPuzzle);
    cspEachRowColDiagConsistsOfDifferentColors:      Result := TPuzzleUtils.CheckEachRowColDiagConsistsOfDifferentColors(FPuzzle);
    cspCheckSavedStates:                             Result := CheckSolved;
    cspEachPlaneHasOneColor:                         Result := TPuzzleUtils.CheckEachPlaneHasOneColor(FPuzzle);
    cspEachColorHasSameDirectionOrCountUniquePlanes: Result := TPuzzleUtils.CheckEachColorHasSingleDirection(FPuzzle, False) or (TPuzzleUtils.CountUniquePlanes(FPuzzle) = FPuzzle.SolvedStates.UniquePlanes);
    cspAdjoiningFacesHaveSameColors:                 Result := TPuzzleUtils.CheckAdjoiningFacesHaveSameColors(FPuzzle);
  else // cspEachColorHasSingleDirection
    Result := TPuzzleUtils.CheckEachColorHasSingleDirection(FPuzzle, False);
  end;
end;

function TSolvedChecker.CheckSolved: Boolean;
var CurrentState: TSolvedState;
  i: integer;
begin
  Result := False;
  CurrentState := TSolvedState.Create;
  CurrentState.CalcFromPuzzle(FPuzzle);
  for i := 0 to FPuzzle.SolvedStates.SolvedStates.Count - 1 do
    if CurrentState.CompareTo(FPuzzle.SolvedStates.SolvedStates[i]) then begin
      Result := True;
      break;
    end;

  CurrentState.Free;
end; // AddSolvedState

end.
