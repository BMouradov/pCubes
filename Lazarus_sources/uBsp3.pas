{************************************************************}
{                                                            }
{  Unit uBsp3                                                }
{  2019-2020                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{  class holds a pack of Bsp objects                         }
{************************************************************}

unit uBsp3;

{$MODE Delphi}

interface
uses
  uBsp, uPart, uPlane, uMatrix,
  Generics.Collections;

type
  TBsp3Item = class
    iFrom, iTo: integer;
    Parts: TList<TPart>;
    Bsp: TBsp;
    constructor Create;
    destructor Destroy; override;
  end;

type
  TBsp3 = class
  private
    FBsp: TBsp;
    FBspItems: TList<TBsp3Item>;
    procedure AddBsp(const Plane: TPlane; Bsp: TBsp);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddLayers(const Plane: TPlane; iFrom, iTo: integer; Parts: TList<TPart>; Planes: TList<TPlane>);
    procedure TransLayers(mat: T4x4Matrix; Layers: array of Boolean);
    procedure Paint(CallbackPaintPart: TCallbackPaintPart; CallbackIsPlaneVisible: TCallbackIsPlaneVisible);
    procedure Trans(mat: T4x4Matrix);
  end;

implementation

// ================== TBsp3Item ====================

constructor TBsp3Item.Create;
begin
  Parts := TList<TPart>.Create;
end;

destructor TBsp3Item.Destroy;
begin
  Parts.Free;
  inherited;
end;

// ================== TBsp3 ====================

constructor TBsp3.Create;
begin
  FBsp := TBsp.Create;
  FBspItems := TList<TBsp3Item>.Create;
end; // Create

procedure TBsp3.Clear;
var item: TBsp3Item;
begin
  for item in FBspItems do
    item.Free;
  FBspItems.Clear;
  FBsp.Clear;
end; // Clear

destructor TBsp3.Destroy;
begin
  Clear;
  FBspItems.Free;
  FBsp.Free;

  inherited;
end;

procedure TBsp3.AddBsp(const Plane: TPlane; Bsp: TBsp);
var OldBsp: TBsp;
begin
  OldBsp := FBsp;
  FBsp := TBsp.Create;
  FBsp.SetBsps(Plane, OldBsp, Bsp);
end;

procedure TBsp3.AddLayers(const Plane: TPlane; iFrom, iTo: integer; Parts: TList<TPart>; Planes: TList<TPlane>);
var Item: TBsp3Item;
begin
  Item := TBsp3Item.Create;
  try
    Item.iFrom := iFrom;
    Item.iTo := iTo;
    Item.Parts.AddRange(Parts);
    if (Plane.NormDistance < 10) and (Plane.NormDistance > -10) then begin  // !!!!!!!!!
      Item.Bsp := TBsp.Create;
      Item.Bsp.SplitByPlanes(Planes, Parts);
      AddBsp(Plane, Item.Bsp);
    end else begin
      FBsp.SplitByPlanes(Planes, Parts);
      Item.Bsp := FBsp;
    end;
  except
    Item.Free;
    raise;
  end;
  FBspItems.Add(Item);
end; // AddLayers

procedure TBsp3.Paint(CallbackPaintPart: TCallbackPaintPart; CallbackIsPlaneVisible: TCallbackIsPlaneVisible);
begin // Paint
  FBsp.Paint(CallbackPaintPart, CallbackIsPlaneVisible);
end; // Paint

procedure TBsp3.Trans(mat: T4x4Matrix);
begin
  FBsp.Trans(mat);
end; // Trans

procedure TBsp3.TransLayers(mat: T4x4Matrix; Layers: array of Boolean);
var BspItem: TBsp3Item;
begin
  for BspItem in FBspItems do
    if Layers[BspItem.iFrom] then
      BspItem.Bsp.Trans(mat);
end; // Trans

end.

