{************************************************************}
{                                                            }
{  Unit uVector                                              }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uVector;

{$mode ObjFPC}{ $H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, Generics.Collections;

type
  // 13.02.2018 https://habrahabr.ru/post/340612/

  //var
  //  V: TVector;
  //  M: TMatrix;
  //. . .
  //  V := [4, 5, 6];
  //  //
  //  M := [[1, 2, 3],
  //        [4, 5, 6],
  //        [7, 8, 9]];


  //var
  //  V, VResult: TVector;
  //  M: TMatrix;
  //. . .
  //  VResult := M * V;


  //V := TMat([[1, 2, 3],
  //             [4, 5, 6],
  //             [7, 8, 9]]) * TVec([4, 5, 6]);

  TVector = record
  public
    x, y, z: extended;
  private
  public
    function Abs: Extended;
//    function ModSubtract(V: TVector): extended;
    procedure Normalize;
    function Norm: TVector;
    function IsNear(const Vect: TVector): Boolean;
    function IsNearOpposite(const Vect: TVector): Boolean;
  end;

function Vector(x, y, z: extended): TVector;

operator := (const arr:  Array of extended): TVector;
operator +  (const ALeft, ARight: TVector): TVector;
operator -  (const ALeft: TVector): TVector;
operator -  (const ALeft, ARight: TVector): TVector;
operator *  (const V: TVector; Scalar: Extended): TVector;
operator *  (const ALeft, ARight: TVector): TVector;
operator /  (const V: TVector; Scalar: Extended): TVector;
operator ** (const ALeft, ARight: TVector): Extended;

//const
//  WRONG_ELEMENT = 'TVector: Element is not accessible';
//  WRONG_SIZE    = 'TVector: Sizes are not equal';
//  NOT_QUAD      = 'TVector: Matrix is not square';
//  SINGULAR      = 'TVector: Singular matrix found';

implementation

//--------------------------------- TVector ----------------------------------

const
  DELTA = 1e-4;

function Vector(x, y, z: extended): TVector;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

//function TVector.ModSubtract(V: TVector): extended;
//begin
//  Result := Sqrt(Sqr(x - V.x) + Sqr(y - V.y) + Sqr(z - V.z));
//end;

function TVector.Abs: Extended;
begin
  Result := Sqrt(Sqr(x) + Sqr(y) + Sqr(z));
end;

procedure TVector.Normalize;
var L: extended;
begin
  L := Abs;

  if System.Abs(L - 1) < 1e-8 then // already normalized
    exit;

  if L = 0 then
    exit;

  x := x / L;
  y := y / L;
  z := z / L;
end;

function TVector.Norm: TVector;
begin
  Result := Self;
  Result.Normalize;
end;

function TVector.IsNear(const Vect: TVector): Boolean;
begin
  Result :=
    System.Abs(x - Vect.x) +
    System.Abs(y - Vect.y) +
    System.Abs(z - Vect.z) < DELTA;
end;

function TVector.IsNearOpposite(const Vect: TVector): Boolean;
begin
  Result :=
    System.Abs(x + Vect.x) +
    System.Abs(y + Vect.y) +
    System.Abs(z + Vect.z) < DELTA;
end;

operator := (const arr: Array of extended): TVector;
begin
  Result.x := arr[0];
  Result.y := arr[1];
  Result.z := arr[2];
end;

operator + (const ALeft, ARight: TVector): TVector;
begin
  Result.x := ALeft.x + ARight.x;
  Result.y := ALeft.y + ARight.y;
  Result.z := ALeft.z + ARight.z;
end;

operator -(const ALeft: TVector): TVector;
begin
  Result.x := -ALeft.x;
  Result.y := -ALeft.y;
  Result.z := -ALeft.z;
end;

operator - (const ALeft, ARight: TVector): TVector;
begin
  Result.x := ALeft.x - ARight.x;
  Result.y := ALeft.y - ARight.y;
  Result.z := ALeft.z - ARight.z;
end;

operator * (const V: TVector; Scalar: Extended): TVector;
begin
  Result.x := V.x * Scalar;
  Result.y := V.y * Scalar;
  Result.z := V.z * Scalar;
end;

// vector mult
operator * (const ALeft, ARight: TVector): TVector;
begin
  Result.x := ALeft.y * ARight.z - ALeft.z * ARight.y;
  Result.y := ALeft.z * ARight.x - ALeft.x * ARight.z;
  Result.z := ALeft.x * ARight.y - ALeft.y * ARight.x;
end;

operator / (const V: TVector; Scalar: Extended): TVector;
begin
  Result := V * (1 / Scalar);
end;

// scalar mult
operator ** (const ALeft, ARight: TVector): Extended;
begin
  Result := ALeft.x * ARight.x + ALeft.y * ARight.y + ALeft.z * ARight.z;
end;

end.

