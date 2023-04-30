{************************************************************}
{                                                            }
{  Unit uGeometryUtils                                       }
{  2019-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uGeometryUtils;

interface
uses uLine, uVector, uPlane;

type THalfOfPlane = (hopInPlane, hopInside, hopOutside, hopIntersect);

type TGeometryUtils = class
  class function DistancePointToLine(const Line: TLine; const Point: TVector): extended; static;
  class function DistancePointToPlane(const Plane: TPlane; const Point: TVector): extended; static;
//  class function HalfPlane(const Plane: TPlane; const Point: TVector): THalfOfPlane; static;
end;

implementation

//const DELTA = 0.0001;

class function TGeometryUtils.DistancePointToLine(const Line: TLine; const Point: TVector): extended;
begin
  Result := ((Line.BaseVector - Point) * Line.NormVector).Abs / Line.NormVector.Abs;
end;

class function TGeometryUtils.DistancePointToPlane(const Plane: TPlane; const Point: TVector): extended;
begin
  Result := (Point - Plane.NormVect * Plane.NormDistance) ** Plane.NormVect;
end;

//class function TGeometryUtils.HalfPlane(const Plane: TPlane; const Point: TVector): THalfOfPlane;
//var D: extended;
//begin
//  D := DistancePointToPlane(Plane, Point);
//  if D > DELTA then
//    Result := hopOutside
//  else if D < -DELTA then
//    Result := hopInside
//  else
//    Result := hopInPlane;
//end;

end.
