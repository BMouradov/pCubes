{************************************************************}
{                                                            }
{  Unit uLine                                                }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uLine;

{$MODESWITCH ADVANCEDRECORDS}

interface
uses
  uMatrix, uVector;

type
  TLine = record
    BaseVector: TVector;
    NormVector: TVector;

    function isNear(Line: TLine): Boolean;
    function isNearOpposite(Line: TLine): Boolean;

    procedure Trans(mat: T4x4Matrix);
  end;

function Line(const BaseVector, NormVector: TVector): TLine;

const DELTA = 1e-4;

implementation

uses uGeometryUtils;

function Line(const BaseVector, NormVector: TVector): TLine;
begin
  Result.BaseVector := BaseVector;
  Result.NormVector := NormVector;
end; // Line

function TLine.isNear(Line: TLine): Boolean;
begin
  if NormVector.IsNear(Line.NormVector) then
    Result := TGeometryUtils.DistancePointToLine(Self, Line.BaseVector) < DELTA
  else
    Result := False;
end; // isNear

function TLine.isNearOpposite(Line: TLine): Boolean;
begin
  if NormVector.IsNearOpposite(Line.NormVector) then
    Result := TGeometryUtils.DistancePointToLine(Self, Line.BaseVector) < DELTA
  else
    Result := False;
end; // isNearOpposite

procedure TLine.Trans(mat: T4x4Matrix);
begin
  NormVector := NormVector + BaseVector;

  BaseVector := TMatrix.VectorMul(mat, BaseVector);
  NormVector := TMatrix.VectorMul(mat, NormVector);

  NormVector := NormVector - BaseVector;

  NormVector.Normalize;
end; // Trans

end.
