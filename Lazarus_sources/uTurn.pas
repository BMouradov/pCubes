{************************************************************}
{                                                            }
{  Unit uTurn                                                }
{  2020-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uTurn;
{$mode delphi}

interface
uses uPart, Generics.Collections;

type
  TTurningType = (ttyNone, ttyLayerTurn, ttyPartsMove);

type
  TTurn = class
  public
    typ: TTurningType;
    AxisNo: integer;
    Layers: array of Boolean;
    Parts: TList<TPart>;

    // ttyLayerTurn
    Angle: extended;

    // ttyPartsMove
    Distance: extended;
  public
    procedure Clear;
  end;

implementation

procedure TTurn.Clear;
begin
  typ := ttyNone;
  AxisNo := 0;
  SetLength(Layers, 0);
  Parts := nil; //.Clear;
  Angle := -1;
  Distance := 0;
end; // Clear

end.
