{************************************************************}
{                                                            }
{  Unit uPlane                                               }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uPlane;

{$MODESWITCH ADVANCEDRECORDS}

interface
uses
  uMatrix, uVector;

type
  TPlane = record
    NormVect: TVector;
    NormDistance: extended;

    function IsNear(Plane: TPlane): Boolean;
    function IsOpposite(Plane: TPlane): Boolean; // same plane but with opposite vector
    procedure TurnRound;
    procedure Trans(mat: T4x4Matrix);
    function DistanceToPoint(const Point: TVector): extended; overload;
  end;

  function Plane(NormVect: TVector; NormDistance: extended): TPlane;

implementation

const DELTA = 4e-7;

function Plane(NormVect: TVector; NormDistance: extended): TPlane;
begin
  Result.NormVect := NormVect;
  Result.NormDistance := NormDistance;
end; // Plane

function TPlane.IsNear(Plane: TPlane): Boolean;
begin
  Result := (Abs(NormDistance - Plane.NormDistance) < DELTA) and
             NormVect.IsNear(Plane.NormVect);
end; // IsNear

function TPlane.IsOpposite(Plane: TPlane): Boolean;
begin
  Result := (Abs(NormDistance + Plane.NormDistance) < DELTA) and
             NormVect.IsNearOpposite(Plane.NormVect);
end; // IsOpposite

procedure TPlane.TurnRound;
begin
  NormDistance := -NormDistance;
  NormVect := -NormVect;
end; // TurnRound

procedure TPlane.Trans(mat: T4x4Matrix);
var p1, p2: TVector;
begin
  p1 := NormVect * NormDistance;
  p2 := p1 + NormVect;

  p1 := TMatrix.VectorMul(mat, p1);
  p2 := TMatrix.VectorMul(mat, p2);

  // set
  NormVect := (p2 - p1).Norm;
  NormDistance := p1 ** NormVect;
end; // Trans

function TPlane.DistanceToPoint(const Point: TVector): extended;
begin
  Result := Point ** NormVect - NormDistance;
end;

end.

