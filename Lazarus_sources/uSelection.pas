{************************************************************}
{                                                            }
{  Unit uSelection                                           }
{  2019-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uSelection;
{$mode delphi}

interface

uses uFace, uPart, uAxis, uVector,
  Generics.Collections;

type
  TMovingParts = class
  private
    FMovingParts: TList<TPart>;
  public
    SelectedPart: TPart;
    AxisNo: integer;
    TurnWithAxes: Boolean;
    Angle: extended;

    property Parts: TList<TPart> read FMovingParts;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddPart(Part: TPart);
    procedure AddParts(AParts: TList<TPart>);
  end;

type
  TSelection = class
  public
    class var SelectedPart: TPart;
    class var SelectedFace: TFace;
    class var MouseOverPart: TPart;
    class var MouseOverFace: TFace;
    class var MouseOverAxis: TAxis;
    class var MouseOverAxisNo: integer;
    class var MouseNearestVertex: integer;
    class var MouseOver: TVector;
    class var OldSelected: TPart;

//    class var HighlightedAxis: TAxis;
//    class var HighlightedParts: TList<TPart>;

    class var TurnAxisNo: integer;
    class var TurnAxis: TAxis;
    class var RotateDirection: integer;
    class var TurningAngle: extended;
    class var AngleMin: extended;
    class var AngleMax: extended;
//    class var DirectionX, DirectionY: integer;
    class var TurnLayers: array of Boolean;
//    class var TurnLayerNos: array of integer;

    class var MouseVector: TVector;

    class procedure Clear;
    class procedure ClearSelection;
    class procedure Select;
    class procedure SetMouseMotion(dx, dy: extended);
  end;

//var
//  HighlightedAxis: TAxis;
//  HighlightedParts: TParts;

implementation

// ============================= TMovingParts ================================

constructor TMovingParts.Create;
begin
  inherited;
  FMovingParts := TList<TPart>.Create;
  Clear;
end;

destructor TMovingParts.Destroy;
begin
  Clear;
  FMovingParts.Free;
  inherited;
end;

procedure TMovingParts.Clear;
begin
  SelectedPart := nil;
  AxisNo := -1;
  TurnWithAxes := False;
  Angle := 0;

  FMovingParts.Clear;
end;

procedure TMovingParts.AddPart(Part: TPart);
begin
  if FMovingParts.IndexOf(Part) = -1 then
    FMovingParts.Add(Part);
end;

procedure TMovingParts.AddParts(AParts: TList<TPart>);
var Part: TPart;
begin
  for Part in AParts do
    AddPart(Part);
end;

// ============================= TSelection ================================


class procedure TSelection.Clear;
begin
  SelectedPart := nil;
  SelectedFace := nil;
  MouseOverPart := nil;
  MouseOverFace := nil;
  MouseOverAxis := nil;
  MouseOverAxisNo := -1;
  MouseNearestVertex := -1;
  OldSelected := nil;
end;

class procedure TSelection.ClearSelection;
begin
  if SelectedPart <> nil then
    OldSelected := SelectedPart;
  SelectedPart := nil;
  SelectedFace := nil;
  MouseOverPart := nil;
  MouseOverFace := nil;
  MouseOverAxis := nil;
  MouseOverAxisNo := -1;
  MouseNearestVertex := -1;
end;

class procedure TSelection.Select;
begin
  SelectedPart := MouseOverPart;
  SelectedFace := MouseOverFace;
end;

class procedure TSelection.SetMouseMotion(dx, dy: extended);
begin
  MouseVector := [dx, dy, 0];
  MouseVector.Normalize;
end;

end.

