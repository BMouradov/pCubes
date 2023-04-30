{************************************************************}
{                                                            }
{  Unit uSplitOrder                                          }
{  2022-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{  Description - it's sometimes hard to split some bandaged  }
{    puzzles from solid figure. This class helps to find     }
{    an order of splitting                                   }
{************************************************************}

unit uSplitOrder;

{$MODE Delphi}

interface

uses uPuzzle, uPart, uPlane, Generics.Collections;

type
  TTypeOfPlane = (topSplit, topUnsplit, topIntersect, topOutside);

type
  TIPlane = record
    AxisNo: integer;
    LayerTo: integer;
    Plane: TPlane;
  end;

type
  TSplitOrder = class
    constructor Create{(Puzzle: TPuzzle; ResultOrder: TStringList)};
    destructor Destroy; override;
    procedure CalcSplitOrder(const Puzzle: TPuzzle);
    procedure CalcCutPartsOrder(const Puzzle: TPuzzle);
  private
//    FResultOrder: TStringList;
//    FPuzzle: TPuzzle;
//    procedure CalcSplitOrderForPart(const Part: TPart);
//    procedure CalcSplitOrderForPartAndAxis(const Part: TPart; const Axis: TAxis);

  private
//    FIsSplit: Boolean; // Is a single part (false) or is a node in a bsp-tree (true)
  private
//    FPartNo: integer;      // if a part
  private
//    FAxisNo, FLayerTo: integer;
//    FFront: TBsplit;
//    FBack: TBsplit;
//    procedure MakeSplitCommands;
//    function IsDividedByPlane(Parts: TList<TPart>; Planes: TList<TIPlane>; Plane: TPlane): Boolean;
    function SplitByIPlanes(Parts: TList<TPart>; Planes: TList<TIPlane>; Tabs: string): TTypeOfPlane;
    function SplitByIPlane(Parts: TList<TPart>; Planes: TList<TIPlane>; iPlane: integer; Tabs: string): TTypeOfPlane;

  public
//    procedure Clear;
//    procedure SetBsps(const LayerTo: integer; Bsp1, Bsp2: TBsplit);
//    function SplitByPlanes(const Puzzle: TPuzzle): Boolean;
  end;

implementation
uses uGeometryUtils, uAxis, uFace, uUtils,
  SysUtils, Classes;

// =============================== TSplitOrder =====================================

constructor TSplitOrder.Create{*(const Puzzle: TPuzzle; const ResultOrder: TStringList)*};
begin
//  FPuzzle := Puzzle;
//  FResultOrder := ResultOrder;

//  FIsSplit := False;
//  FPartNo := -1;

//  FFront := nil;
//  FBack := nil;
end;

destructor TSplitOrder.Destroy;
begin
//  FPuzzle := nil;
//  FResultOrder := nil;

//  if FIsSplit then begin
//    FreeAndNil(FFront);
//    FreeAndNil(FBack);
//  end;

  inherited;
end;

procedure TSplitOrder.CalcSplitOrder(const Puzzle: TPuzzle);
var
  Axis: TAxis;
  Planes: TList<TIPlane>;
  IPlane: TIPlane;
  i, j: integer;
begin
  Planes := TList<TIPlane>.Create;
  try
    for i := 0 to Puzzle.Axes.Count - 1 do begin
      Axis := Puzzle.Axes[i];
      for j := 0 to Axis.Layers.Count - 2 do begin
        IPlane.AxisNo := i;
        IPlane.LayerTo := j;
        IPlane.Plane := Axis.GetLayerPlaneTo(j);
        Planes.Add(IPlane);
      end;
    end;

    SplitByIPlanes(Puzzle.Parts, Planes, '');
  finally
    Planes.Free;
  end;

//  FResultOrder.Clear;
//  FPuzzle.CalcPartPositions;
//  for Part in Parts do
//    CalcSplitOrderForPart(Part);
end;

//function TBsplit.IsDividedByPlane(Parts: TList<TPart>; Planes: TList<TIPlane>; Plane: TPlane): Boolean;
//var Part: TPart;
//begin
//  for Part in Parts do
//    if Part.Visible then
//      if not (Part.HalfPlane(Plane) in [hopInside, hopOutside]) then
//        exit(False);
//  Result := True;
//end; // IsDividedByPlane

function TSplitOrder.SplitByIPlane(Parts: TList<TPart>; Planes: TList<TIPlane>; iPlane: integer; Tabs: string): TTypeOfPlane;
var Part: TPart;
    Plane: TPlane;
    Parts1, Parts2: TList<TPart>;
    Planes1, Planes2: TList<TIPlane>;
    top1, top2: TTypeOfPlane;
begin
  Plane := Planes[iPlane].Plane;
  Parts1 := TList<TPart>.Create;
  Parts2 := TList<TPart>.Create;
  Planes1 := TList<TIPlane>.Create;
  Planes2 := TList<TIPlane>.Create;
  try
    for Part in Parts do
      if Part.Visible then
        case Part.HalfPlane(Plane) of
          hopInside:  Parts1.Add(Part);
          hopOutside: Parts2.Add(Part);
        else
          exit(topIntersect);
        end;

    if (Parts1.Count = 0) or (Parts2.Count = 0) then
      exit(topOutside);

    if (Parts1.Count = 1) and (Parts2.Count = 1) then begin
      TUtils.Log(Tabs + IntToStr(Planes[iPlane].AxisNo) + IntToStr(Planes[iPlane].LayerTo) + '*');
      exit(topSplit);
    end;

    Planes1.AddRange(Planes);
    Planes1.Delete(iPlane);
    Planes2.AddRange(Planes1);
    top1 := SplitByIPlanes(Parts1, Planes1, Tabs + IntToStr(Planes[iPlane].AxisNo) + IntToStr(Planes[iPlane].LayerTo) + '-');
    top2 := SplitByIPlanes(Parts2, Planes2, Tabs + IntToStr(Planes[iPlane].AxisNo) + IntToStr(Planes[iPlane].LayerTo) + '+');
    if (top1 = topUnsplit) or (top2 = topUnsplit) then
      exit(topUnsplit);
  finally
    Parts1.Free;
    Parts2.Free;
    Planes1.Free;
    Planes2.Free;
  end;
  Result := topSplit;
end; // SplitByIPlane

function TSplitOrder.SplitByIPlanes(Parts: TList<TPart>; Planes: TList<TIPlane>; Tabs: string): TTypeOfPlane;
var i: integer;
begin
  if Parts.Count = 1 then
    exit(topSplit);

  for i := Planes.Count - 1 downto 0 do
    case SplitByIPlane(Parts, Planes, i, Tabs) of
      topSplit:
        exit(topSplit);
      topOutside:
        Planes.Delete(i);
    end;
  Result := topUnsplit;
end; // SplitByIPlanes



//function TBsplit.SplitByPlanes(Planes: TList<TPlane>; Parts: TList<TPart>; ForceSplit: Boolean = True): Boolean;
//var
//  Plane, PlaneToCheck: TPlane;
//  i, PlanesCount: integer;
//  IntersectionFound, Found: Boolean;
//  FrontParts, BackParts: TList<integer>;
//  Part: TPart;
////  Face: TFace;
//  PlanesToCheck: TList<TPlane>;
//begin // SplitByPlanes
//  Result := True;
//  if FIsSplit or (FPartNo <> -1) then
//    raise Exception.Create('TBsplit.SplitByPlanes: Bspit not empty');
//  if Parts.Count = 1 then
//    FPartNo := Parts[0];
//  if Parts.Count <= 1 then
//    exit;
//
//  FrontParts := TList<integer>.Create;
//  BackParts := TList<integer>.Create;
//  PlanesToCheck := TList<TPlane>.Create;
//  try
//    PlanesToCheck.AddRange(Planes);
//
//    for i := PlanesToCheck.Count - 1 downto 0 do begin
//      Plane := PlanesToCheck[i];
//
//      // Divide Parts to front and back
//      FrontParts.Clear;
//      BackParts.Clear;
//      IntersectionFound := False;
//      for Part in Parts do begin
//        if Part.Visible then begin
//          case Part.HalfPlane(Plane) of
//            hopOutside: FrontParts.Add(Part);
//            hopInside:  BackParts.Add(Part);
//          else // hopIntersect, hopInPlane
//            begin
//              IntersectionFound := True;
//              break;
//            end;
//          end;
//        end;
//      end;
//
//      if (FrontParts.Count = 0) or (BackParts.Count = 0) then
//        PlanesToCheck.Delete(i)
//      else if not IntersectionFound then begin
//        // create two bsp
//        FFront := TBsplit.Create;
//        FBack := TBsplit.Create;
//
//        try
//          if FFront.SplitByPlanes(PlanesToCheck, FrontParts) and
//             FBack.SplitByPlanes(PlanesToCheck, BackParts) then begin
//
//            FPlane := Plane;
//            FAxisNo :=
//            FLayerTo :=
//            FIsSplit := True;
//            Result := True;
//            exit;
//          end;
//        except
//          FreeAndNil(FFront);
//          FreeAndNil(FBack);
//          raise;
//        end;
//        FreeAndNil(FFront);
//        FreeAndNil(FBack);
//        Result := False;
//        exit;
//      end;
//    end; // for
//  finally
//    PlanesToCheck.Free;
//    FrontParts.Free;
//    BackParts.Free;
//  end;
//end; // SplitByPlanes
//
//procedure TBsplit.MakeSplitCommands;
//begin // Paint
//  if FIsSplit then begin
//    FBack.MakeSplitCommands;
//    FFront.MakeSplitCommands;
//  end else begin
//    if FPartNo <> -1 then begin
//      MakeSplitCommandsForPart(FPartNo, FAxisNo, FLayerTo);
//    end;
//  end;
//end; // MakeSplitCommands

procedure TSplitOrder.CalcCutPartsOrder(const Puzzle: TPuzzle);
var
  Axis: TAxis;
  Part: TPart;
  Face: TFace;
  Plane: TPlane;
  i, j: integer;
  S: String;
  sl: TStringList;
begin
  sl := TStringList.Create;
  for Part in Puzzle.Parts do begin
    S := '';
    for Face in Part.Faces do begin
      if Face.Color = -1 then begin
        Plane := Face.GetPlane;
        for i := 0 to Puzzle.Axes.Count - 1 do begin
          Axis := Puzzle.Axes[i];
//          if Plane.NormVect.IsNear(Axis.Line.NormVector) then begin
            for j := 1 to Axis.Layers.Count - 1 do begin
              if Axis.GetLayerPlaneFrom(j).IsNear(Plane) then
                S := S + IntToStr(i) + IntToStr(j) + '-'
              else if Axis.GetLayerPlaneFrom(j).IsOpposite(Plane) then
                S := S + IntToStr(i) + IntToStr(j) + '+';
            end;
          //end;
        end;
      end;
    end;
    sl.Add(S);
//    if Length(S) > 0 then
  end;
      TUtils.Log('Done');
//    sl.SaveToFile('a.txt');
  sl.Free;
end; // CalcCutPartsOrder



end.
