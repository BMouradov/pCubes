{************************************************************}
{                                                            }
{  Unit uMatrix                                              }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uMatrix;

interface

uses uVector;

type
  T4x4Matrix = array[0..3, 0..3] of extended;

type
  TMatrix = class
  public
    class function SingleMatrix: T4x4Matrix;
    class function GetMoveMatrix(const d: TVector): T4x4Matrix;
    class function GetRotateMatrix(nx, ny, nz, Al: extended): T4x4Matrix; overload;
    class function GetRotateMatrix(const V: TVector; Al: extended): T4x4Matrix; overload;
    class function GetScaleMatrix(dx, dy, dz: extended): T4x4Matrix;

    class function Mult(const a, b: T4x4Matrix): T4x4Matrix;
    class function VectorMul(const a: T4x4Matrix; const V: TVector): TVector;
  end;

implementation
uses SysUtils{, uUtils};

const FSingleMatrix: T4x4Matrix =
  ((1, 0, 0, 0),
   (0, 1, 0, 0),
   (0, 0, 1, 0),
   (0, 0, 0, 1));

class function TMatrix.Mult(const a, b: T4x4Matrix): T4x4Matrix;
var i, j, k: integer;
  s: extended;
begin
  for j := 0 to 3 do
    for i := 0 to 3 do begin
      s := 0;
      for k := 0 to 3 do
        s := s + a[j][k] * b[k][i];
      Result[j][i] := s;
    end;
end; // Mult

class function TMatrix.VectorMul(const a: T4x4Matrix; const V: TVector): TVector;
var a1, a2, a3, d: extended;
begin
  a1 := V.X * a[0, 0] + V.Y * a[1, 0] + V.Z * a[2, 0] + a[3, 0];
  a2 := V.X * a[0, 1] + V.Y * a[1, 1] + V.Z * a[2, 1] + a[3, 1];
  a3 := V.X * a[0, 2] + V.Y * a[1, 2] + V.Z * a[2, 2] + a[3, 2];
  d  := V.X * a[0, 3] + V.Y * a[1, 3] + V.Z * a[2, 3] + a[3, 3];
  if d <> 0 then begin
    Result.X := a1 / d;
    Result.Y := a2 / d;
    Result.Z := a3 / d;
  end else begin
    Result.X := 0;
    Result.Y := 0;
    Result.Z := 0;
  end;
end; // MatrixMul

class function TMatrix.SingleMatrix: T4x4Matrix;
begin
  Result := FSingleMatrix;
end; // Init

{class procedure TMatrix.Copy(dest, src: T4x4Matrix);
var i, j: integer;
begin
  for j := 0 to 3 do
    for i := 0 to 3 do
      dest[j, i] := src[j, i];
end; // Copy}

class function TMatrix.GetScaleMatrix(dx, dy, dz: extended): T4x4Matrix;
begin
  Result := FSingleMatrix;
  Result[0][0] := dx;
  Result[1][1] := dy;
  Result[2][2] := dz;
end; // GetScaleMatrix

class function TMatrix.GetMoveMatrix(const d: TVector): T4x4Matrix;
begin
  Result := FSingleMatrix;
  Result[3][0] := d.x;
  Result[3][1] := d.y;
  Result[3][2] := d.z;
end; // GetMoveMatrix

{class function TMatrix.GetPerspectiveMatrix(d: extended): T4x4Matrix;
begin
  Result := FSingleMatrix;
  Result[3][3] := 0;
  Result[2][2] := -far / (far - near);;
  Result[2][3] := -far * near / (far - near);;
  Result[3][2] := -1;
end; // Pers}

// turn clockwise
// nx, ny, nz - must be normalized
class function TMatrix.GetRotateMatrix(nx, ny, nz, Al: extended): T4x4Matrix;
var s, c, t: extended;
begin
  s := sin(Al);
  c := cos(Al);
  t := 1 - c;

  Result := FSingleMatrix;
  Result[0][0] := t * nx * nx + c;
  Result[1][0] := t * nx * ny - s * nz;
  Result[2][0] := t * nx * nz + s * ny;

  Result[0][1] := t * nx * ny + s * nz;
  Result[1][1] := t * ny * ny + c;
  Result[2][1] := t * ny * nz - s * nx;

  Result[0][2] := t * nx * nz - s * ny;
  Result[1][2] := t * nz * ny + s * nx;
  Result[2][2] := t * nz * nz + c;
end; // GetRotateMatrix

class function TMatrix.GetRotateMatrix(const V: TVector; Al: extended): T4x4Matrix;
var s, c, t: extended;
begin
  s := sin(Al);
  c := cos(Al);
  t := 1 - c;

  Result := FSingleMatrix;
  Result[0][0] := t * V.X * V.X + c;
  Result[1][0] := t * V.X * V.Y - s * V.Z;
  Result[2][0] := t * V.X * V.Z + s * V.Y;

  Result[0][1] := t * V.X * V.Y + s * V.Z;
  Result[1][1] := t * V.Y * V.Y + c;
  Result[2][1] := t * V.Y * V.Z - s * V.X;

  Result[0][2] := t * V.X * V.Z - s * V.Y;
  Result[1][2] := t * V.Z * V.Y + s * V.X;
  Result[2][2] := t * V.Z * V.Z + c;
end; // GetRotateMatrix

{class procedure TMatrix.Transpon(a: T4x4Matrix);
var
  h: extended;
  i, j: integer;
begin
  for j := 0 to 2 do
    for i := j + 1 to 3 do begin
      h := a[j][i];
      a[j][i] := a[i][j];
      a[i][j] := h;
    end;
end; // Transpon}

{class function TMatrix.Det(a: T4x4Matrix; x, y: integer): extended;
var
  i0, i1, i2: integer;
  j0, j1, j2: integer;
begin
  i0 := 0;
  i1 := 1;
  i2 := 2;
  j0 := 0;
  j1 := 1;
  j2 := 2;
  case x of
    0: i0 := 1;
    1: i1 := 2;
    2: i2 := 3;
  end;
  case y of
    0: j0 := 1;
    1: j1 := 2;
    2: j2 := 3;
  end;
  Result :=
    a[j0][i0] * a[j1][i1] * a[j2][i2] -
    a[j0][i0] * a[j1][i2] * a[j2][i1] +
    a[j0][i1] * a[j1][i2] * a[j2][i0] -
    a[j0][i1] * a[j1][i0] * a[j2][i2] +
    a[j0][i2] * a[j1][i0] * a[j2][i1] -
    a[j0][i2] * a[j1][i1] * a[j2][i0];
end; // Det}

{class procedure TMatrix.Invert(a: T4x4Matrix);
var
  d: extended;
  h: T4x4Matrix;
  i, j: integer;
begin
  d :=
    a[0][0] * TMatrix.Det(a, 0, 0) -
    a[0][1] * TMatrix.Det(a, 1, 0) +
    a[0][2] * TMatrix.Det(a, 2, 0) -
    a[0][3] * TMatrix.Det(a, 3, 0);
  d := 1 / d;
  for j := 0 to 3 do begin
    for i := 0 to 3 do begin
      h[i][j] := d * Det(a, i, j);
      d := -d;
    end;
    d := -d;
  end;
  Copy(a, h);
end; // Invert}

{class function TMatrix.GetInverse3(const p1, p2, p3: TVector): T4x4Matrix;
begin
  Result := FSingleMatrix;
  Result[0][0] := p1.x;
  Result[1][0] := p1.y;
  Result[2][0] := p1.z;
  Result[0][1] := p2.x;
  Result[1][1] := p2.y;
  Result[2][1] := p2.z;
  Result[0][2] := p3.x;
  Result[1][2] := p3.y;
  Result[2][2] := p3.z;
  Invert(Result);
end;}

{class function TMatrix.ToString(a: T4x4Matrix): string;
var
  i, j: integer;
begin
  Result := '';
  for j := 0 to 3 do begin
    for i := 0 to 3 do
      Result := Result + FloatToStr(a[j][i]) + ', ';
    Result := Result + #13#10;
  end;
end; // ToString}

{class procedure TMatrix.Log(a: T4x4Matrix);
begin
  LogFileOutput('Log.txt', ToString(a));
end;}

// ћатрица, вращающа€ так, что d переходит в z (или наоборот?). d и z - нормализованные (!)
//class function TMatrix.RotationAlign(const d, z: TVector): T4x4Matrix;
//var
//  v: TVector;
//  c, k: extended;
//begin
//  v := z * d;
//  c := z.ScalraMult(d);
//  k = 1.0f/(1.0f + c);
//
//  Result[0][0] := v.x * v.x * k + c;
//  Result[1][0] := v.y * v.x * k - v.z;
//  Result[2][0] := v.z * v.x * k + v.y;
//  Result[0][1] := v.x * v.y * k + v.z;
//  Result[1][1] := v.y * v.y * k + c;
//  Result[2][1] := v.z * v.y * k - v.x;
//  Result[0][2] := v.x * v.z * K - v.y;
//  Result[1][2] := v.y * v.z * k + v.x;
//  Result[2][2] := v.z * v.z * k + c;
//end;

end.

