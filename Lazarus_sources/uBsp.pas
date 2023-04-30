{************************************************************}
{                                                            }
{  Unit uBsp                                                 }
{  2014-2020                                                 }
{                                                            }
{  Author: Andrea, Boris Mouradov                            }
{  bsp - binary space partitioning                           }
{************************************************************}

unit uBsp;

{$MODE Delphi}

interface
uses
  uPlane, uPart, uMatrix,
  Generics.Collections;

// PaintFlags
const
  pfNone = 0;
  pfError = 1;
//  pfSelected = 2; // future

type
  TCallbackPaintPart = procedure(Part: TPart; Flags: integer) of object;
  TCallbackIsPlaneVisible = function(const Plane: TPlane): Boolean of object;

type
  TBsp = class
  private
    FIsSplit: Boolean; // Is a single part (false) or is a node in a bsp-tree (true)
  private
    FPart: TPart;      // if a part
  private
    FPlane: TPlane;    // if a node in a tree
    FBspFront: TBsp;
    FBspBack: TBsp;
  public
    IsError: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetBsps(const Plane: TPlane; Bsp1, Bsp2: TBsp);
    function SplitByPlanes(Planes: TList<TPlane>; Parts: TList<TPart>; ForceSplit: Boolean = True): Boolean;
    procedure Paint(PaintPart: TCallbackPaintPart; IsPlaneVisible: TCallbackIsPlaneVisible);
    procedure Trans(mat: T4x4Matrix);
  end;

implementation
uses uFace, uGeometryUtils, SysUtils;

constructor TBsp.Create;
begin
  inherited;
  IsError := False;
  FIsSplit := False;
  FPart := nil;

  FBspFront := nil;
  FBspBack := nil;
end; // Create

destructor TBsp.Destroy;
begin
  Clear;
  inherited;
end;

procedure TBsp.Clear;
begin
  if FIsSplit then begin
    FreeAndNil(FBspFront);
    FreeAndNil(FBspBack);
  end;

  FIsSplit := False;
  FPart := nil;
end; // Clear

procedure TBsp.SetBsps(const Plane: TPlane; Bsp1, Bsp2: TBsp);
begin
  if FIsSplit or (FPart <> nil) then
    raise Exception.Create('TBsp.SetBsps: Bsp not empty');
  if (Bsp1 = nil) or (Bsp2 = nil) then
    raise Exception.Create('TBsp.SetBsps: Bsp must be non-nil');

  FIsSplit := True;

  FPlane := Plane;

  FBspBack := Bsp1;
  FBspFront := Bsp2;
end;

function TBsp.SplitByPlanes(Planes: TList<TPlane>; Parts: TList<TPart>; ForceSplit: Boolean = True): Boolean;
var
  Plane, PlaneToCheck: TPlane;
  i, PlanesCount: integer;
  IntersectionFound, Found: Boolean;
  FrontParts, BackParts: TList<TPart>;
  Part: TPart;
  Face: TFace;
  PlanesToCheck: TList<TPlane>;
begin // SplitByPlanes
  Result := True;
  if FIsSplit or (FPart <> nil) then
    raise Exception.Create('TBsp.SplitByPlanes: Bsp not empty');
  if Parts.Count = 1 then
    FPart := Parts[0];
  if Parts.Count <= 1 then
    exit;

  FrontParts := TList<TPart>.Create;
  BackParts := TList<TPart>.Create;
  PlanesToCheck := TList<TPlane>.Create;
  try
    PlanesToCheck.AddRange(Planes);

    for i := PlanesToCheck.Count - 1 downto 0 do begin
      Plane := PlanesToCheck[i];

      // Divide Parts to front and back
      FrontParts.Clear;
      BackParts.Clear;
      IntersectionFound := False;
      for Part in Parts do begin
        if Part.Visible then begin
          case Part.HalfPlane(Plane) of
            hopOutside: FrontParts.Add(Part);
            hopInside:  BackParts.Add(Part);
//            hopInPlane: BackParts.Add(Part); // ???
          else // hopIntersect, hopInPlane
            begin
              IntersectionFound := True;
              break;
            end;
          end;
        end;
      end;

      if (FrontParts.Count = 0) or (BackParts.Count = 0) then
        PlanesToCheck.Delete(i)
      else if not IntersectionFound then begin
        // create two bsp
//        if FPlane <> nil then
//          raise Exception.Create('FPlane <> nil');
        FBspFront := TBsp.Create;
        FBspBack := TBsp.Create;

        try
          if FBspFront.SplitByPlanes(PlanesToCheck, FrontParts) and
             FBspBack.SplitByPlanes(PlanesToCheck, BackParts) then begin

            FPlane := Plane;
            FIsSplit := True;
            Result := True;
            exit;
          end;
        except
          FreeAndNil(FBspFront);
          FreeAndNil(FBspBack);
          raise;
        end;
        FreeAndNil(FBspFront);
        FreeAndNil(FBspBack);
        Result := False;
        exit;
      end;
    end; // for

    // if can't divide then divide with faces
    if not FIsSplit and ForceSplit then begin
      PlanesCount := PlanesToCheck.Count;

      for Part in Parts do begin
        for Face in Part.Faces do begin
          Plane := Face.GetPlane;

          // check if already found
          Found := False;
          for PlaneToCheck in PlanesToCheck do
            if Plane.IsNear(PlaneToCheck) or Plane.IsOpposite(PlaneToCheck) then begin
              Found := True;
              break;
            end;
          if not Found then
            PlanesToCheck.Add(Plane);
        end;
      end;

      Result := SplitByPlanes(PlanesToCheck, Parts, False);
    end;

    if not FIsSplit then begin
      // если неразбитыми остались только плоские фигуры (состоящие из одного фейса)
      Found := False;
      for Part in Parts do
        if Part.Faces.Count > 1 then begin // !!! ещё надо проверить, что они в одной плоскости
          Found := True;
          break;
        end;
      if not Found then begin
        FrontParts.Clear;
        BackParts.Clear;
        FrontParts.Add(Parts[0]);
        BackParts.AddRange(Parts);
        BackParts.Delete(0);
        FBspFront := TBsp.Create;
        FBspBack := TBsp.Create;

        try
          if FBspFront.SplitByPlanes(PlanesToCheck, FrontParts) and
             FBspBack.SplitByPlanes(PlanesToCheck, BackParts) then begin

            FPlane := Plane;
            FIsSplit := True;
            Result := True;
            exit;
          end;
        except
          FreeAndNil(FBspFront);
          FreeAndNil(FBspBack);
          raise;
        end;
        FreeAndNil(FBspFront);
        FreeAndNil(FBspBack);
      end;
    end;
    if not FIsSplit then begin
      FrontParts.Clear;
      BackParts.Clear;
      FrontParts.Add(Parts[0]);
      BackParts.AddRange(Parts);
      BackParts.Delete(0);
      FBspFront := TBsp.Create;
      FBspFront.IsError := True;
      FBspBack := TBsp.Create;

      try
        if FBspFront.SplitByPlanes(PlanesToCheck, FrontParts) and
           FBspBack.SplitByPlanes(PlanesToCheck, BackParts) then begin

          FPlane := Plane;
          FIsSplit := True;
          Result := True;
          exit;
        end;
      except
        FreeAndNil(FBspFront);
        FreeAndNil(FBspBack);
        raise;
      end;
      FreeAndNil(FBspFront);
      FreeAndNil(FBspBack);
    end;
  finally
    PlanesToCheck.Free;
    FrontParts.Free;
    BackParts.Free;
  end;
end; // SplitByPlanes

procedure TBsp.Paint(PaintPart: TCallbackPaintPart; IsPlaneVisible: TCallbackIsPlaneVisible);
begin // Paint
  if FIsSplit then begin
    if IsPlaneVisible(FPlane) then begin
      FBspBack.Paint(PaintPart, IsPlaneVisible);
      FBspFront.Paint(PaintPart, IsPlaneVisible);
    end else begin
      FBspFront.Paint(PaintPart, IsPlaneVisible);
      FBspBack.Paint(PaintPart, IsPlaneVisible);
    end;
  end else begin
    if FPart <> nil then begin
      if not IsError then
        PaintPart(FPart, pfNone)
      else
        PaintPart(FPart, pfError);
    end;
  end;
end; // Paint

procedure TBsp.Trans(mat: T4x4Matrix);
begin
  if FIsSplit then begin
    FPlane.Trans(mat);
    FBspFront.Trans(mat);
    FBspBack.Trans(mat);
  end;
end; // Trans

end.

