{************************************************************}
{                                                            }
{  Unit uLayer                                               }
{  2016-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uLayer;
{$mode delphi}

interface
uses
  uMatrix, uPart, Generics.Collections;

type
  TLayer = class
  private
    FTurningAngle: extended; // удалить?
    procedure SetTurningAngle(AAngle: extended);
  public
    DistanceFrom, DistanceTo: extended;
    Fixed: Boolean;
    Angle: extended;
    AvailableAngles: array of extended;
//    IsConstrained: Boolean;  // удалить?
    MinAngle, MaxAngle: extended; // Constrained
    ConnectedLayer: integer;
    property TurningAngle: extended read FTurningAngle write SetTurningAngle;

    constructor Create(ADistanceFrom, ADistanceTo: extended);
    destructor Destroy; override;

//    procedure Trans(mat: T4x4Matrix); virtual;
    procedure AddAngle(AAngle: extended);

    function GetNextAngle(Al: extended): extended;
    function GetPrevAngle(Al: extended): extended;
    function IsValidAngle(Al: extended): Boolean;
    procedure AddAvailableAngle(AAngle: extended);
  end;

implementation

uses uGeometryUtils, uUtils, {uStreamReadWrite,}
     SysUtils;

const DELTA = 1e-4;

constructor TLayer.Create(ADistanceFrom, ADistanceTo: extended);
begin
  inherited Create;
  Self.DistanceFrom := ADistanceFrom;
  Self.DistanceTo   := ADistanceTo;
  Fixed := True;
  SetLength(AvailableAngles, 0);
  MinAngle := -1e6;
  MaxAngle := 1e6;
  ConnectedLayer := -1;
end; // Create

destructor TLayer.Destroy;
begin
  SetLength(AvailableAngles, 0);
  inherited;
end;

//procedure TLayer.Trans(mat: T4x4Matrix);
//begin
//
//end; // Trans

procedure TLayer.AddAngle(AAngle: extended);
begin
  Angle := Angle + AAngle;
end;

procedure TLayer.AddAvailableAngle(AAngle: extended);
var i: integer;
    d: extended;
begin
  if Fixed then
    exit;
  AAngle := TUtils.PrivPi(AAngle);
  for i := 0 to High(AvailableAngles) do
    if Abs(TUtils.PrivPi(AAngle - AvailableAngles[i])) < DELTA then
      exit;

  SetLength(AvailableAngles, Length(AvailableAngles) + 1);
  AvailableAngles[Length(AvailableAngles) - 1] := AAngle;

  // sort
  for i := High(AvailableAngles) downto 1 do
    if AvailableAngles[i] < AvailableAngles[i - 1] then begin
      d := AvailableAngles[i];
      AvailableAngles[i] := AvailableAngles[i - 1];
      AvailableAngles[i - 1] := d;
    end;
end; // AddAvailableAngle

procedure TLayer.SetTurningAngle(AAngle: extended);
const delta = 1e-6;
var i, N: integer;
begin // SetTurningAngle
  FTurningAngle := AAngle;

  if AAngle = 0 then
    exit;

  Fixed := False;
  N := Round(2 * Pi / AAngle);
  if Abs(N * AAngle - 2 * Pi) > delta then
    raise Exception.Create('TLayer.SetTurningAngle: 2Pi is not divisible by ' + FloatToStr(AAngle));

  for i := 0 to N - 1 do
    AddAvailableAngle(AAngle * i);
end; // SetTurningAngle

//function TVarList.Find(const S: String; var Index: Integer): Boolean;
//var
//  L, H, I, C: Integer;
//begin
//  Result := False;
//  L := 0;
//  H := Count - 1;
//  while L <= H do begin
//    I := (L + H) shr 1;
//    C := CompareStr(Items[I].Name, S);
//    if C < 0 then L := I + 1 else begin
//      H := I - 1;
//      if C = 0 then begin
//        Result := True;
//        L := I;
//      end;
//    end;
//  end;
//  Index := L;
//end; // Find


function TLayer.GetNextAngle(Al: extended): extended;
var MaxA: extended;
    i, loops: integer;
const Pi2 = Pi * 2;
begin
  if Length(AvailableAngles) = 0 then begin
    Result := Al;
    exit;
  end;

  Al := Al - DELTA;
  loops := 0;
  MaxA := AvailableAngles[High(AvailableAngles)];
  while Al > MaxA do begin
    Al := Al - Pi2;
    Inc(loops);
  end;

  while Al <= MaxA - Pi2 do begin
    Al := Al + Pi2;
    Dec(loops);
  end;

  for i := 0 to High(AvailableAngles) do
    if al < AvailableAngles[i] then begin
      Result := AvailableAngles[i] + Pi2 * loops;
      exit;
    end;
  Result := MaxA + Pi2 * loops; // dummy
end; // GetNextAngle

function TLayer.GetPrevAngle(Al: extended): extended;
var MinA: extended;
    i, loops: integer;
const Pi2 = Pi * 2;
begin
  if Length(AvailableAngles) = 0 then begin
    Result := Al;
    exit;
  end;

  Al := Al + DELTA;
  loops := 0;
  MinA := AvailableAngles[0];
  while Al < MinA do begin
    Al := Al + Pi2;
    Dec(loops);
  end;

  while Al >= MinA + Pi2 do begin
    Al := Al - Pi2;
    Inc(loops);
  end;

  for i := High(AvailableAngles) downto 0 do
    if al > AvailableAngles[i] then begin
      Result := AvailableAngles[i] + Pi2 * loops;
      exit;
    end;
  Result := MinA + Pi2 * loops; // dummy
end; // GetPrevAngle

function TLayer.IsValidAngle(Al: extended): Boolean;
// todo AvailableAngles - is a sorted array so binary search may be applied
var i: integer;
begin
  Result := True;
  Al := TUtils.PrivPi(Al);
  for i := 0 to High(AvailableAngles) do
    if Abs(TUtils.PrivPi(Al - AvailableAngles[i])) < DELTA then
      exit;
  Result := False;
end; // IsValidAngle

end.

