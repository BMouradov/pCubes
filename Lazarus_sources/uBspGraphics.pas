{************************************************************}
{                                                            }
{  Unit uBspGraphics                                         }
{  2014-2020                                                 }
{                                                            }
{  Author: Andrea, Boris Mouradov                            }
{                                                            }
{************************************************************}

unit uBspGraphics;

{$MODE Delphi}

interface
uses types,
     uVector, uPlane, uFace, uPuzzle, uAxis, uPart,
     uBaseGraphics, uBsp3, uMatrix,
     LCLIntf, LCLType, Graphics,
     BGRABitmap, BGRABitmapTypes;

type
  TBspGraphics = class(TBaseGraphics)
  private
    FAntiAliasedBitmap: TBGRABitmap;
    FBsp3: TBsp3;
    ExplodeDelta: TVector;
//    Texture: TBitmap;
    procedure DrawVectorPicture(Face: TFace; norm: extended);
    procedure PaintSmallSquare(const ScreenPoint: TVector);
    procedure MoveTo(const ScreenPoint: TVector);
    procedure LineTo(const SceenPoint: TVector);
    procedure PenWidth(Width: extended);
    procedure Line3d(const V1, V2: TVector);
    function DimColor(Color1, Color2: TColor; Dim: extended): TColor;
    procedure CalcExplode(Part: TPart);
//    procedure PaintPoint(Point: TVector);
    procedure PaintRay(const VBase, V: TVector);
    procedure TestPaint;
  public
    LastScreen: TVector;

    constructor Create;
    procedure Clear; override;
    destructor Destroy; override;

    procedure DrawBackground; override;
//    procedure LineTo1(x, y, z: extended); overload;
    procedure Polygon(ScreenPath: array of TVector); override;
    procedure Polygon3d(Path: array of TVector); override;

    function MouseIsInFace(mx, my: extended; Face: TFace): Boolean;
    procedure Select(Puzzle: TPuzzle; mx, my: integer); override;
    procedure GetMouseParts(Puzzle: TPuzzle; mx, my: integer); override;

    procedure PaintScene(Puzzle: TPuzzle); override;
    procedure PaintPart(Part: TPart; Flags: integer); override;
    procedure PaintFace(Face: TFace);
    procedure PaintAxis(Axis: TAxis);
    procedure PaintPlane(const Plane: TPlane);
    procedure PaintFaceContour(Face: TFace);

    procedure TransLayers(mat: T4x4Matrix; AxisNo: integer; Layers: array of Boolean); override;
    procedure CalcPartitioning(AxisNo: integer = 0);
    procedure ReCalc; override;
    procedure OnAfterPuzzleLoad; override;
    procedure OnBeforePuzzleTurn(AxisNo: integer); override;
    procedure OnPuzzleTrans(mat: T4x4Matrix); override;

    procedure CallbackPaintPart(Part: TPart; Flags: integer);
    function CallbackIsPlaneVisible(const Plane: TPlane): Boolean;
  end;

implementation

uses uSelection, uUtils, uGeometryUtils, uVectorImage, uBsp,
  {EZLines,} SysUtils,
  System.UITypes, Generics.Collections, Math
  ;

//!!var EZLine: TEZLine;

// ================================ TBspGraphics =============================

constructor TBspGraphics.Create;
begin
  inherited Create;

  Zoom := DEFAULT_ZOOM;
  LastScreen := [0, 0, 0];

  FAntiAliasedBitmap := nil;
  FBsp3 := TBsp3.Create;
end; // Create

procedure TBspGraphics.Clear;
begin
  FBsp3.Clear;
  inherited;
end;

destructor TBspGraphics.Destroy;
begin
  FBsp3.Free;
  if FAntiAliasedBitmap <> nil then
    FreeAndNil(FAntiAliasedBitmap);

  inherited;
end;

procedure TBspGraphics.PenWidth(Width: extended);
begin
  FBitmap.Canvas.Pen.Width := Round(Width);
end;

procedure TBspGraphics.OnPuzzleTrans(mat: T4x4Matrix);
begin
  inherited;
  FBsp3.Trans(mat);
end;

procedure TBspGraphics.TransLayers(mat: T4x4Matrix; AxisNo: integer; Layers: array of Boolean);
begin
  FBsp3.TransLayers(mat, Layers);
end; // TransLayers

procedure TBspGraphics.MoveTo(const ScreenPoint: TVector);
begin
  if not TSettings.AntiAlias then
    FBitmap.Canvas.MoveTo(Round(ScreenPoint.X), Round(ScreenPoint.Y))
  else
    FAntiAliasedBitmap.CanvasBGRA.MoveTo(Round(ScreenPoint.X), Round(ScreenPoint.Y));

  LastScreen := ScreenPoint;
end; // MoveTo

procedure TBspGraphics.LineTo(const SceenPoint: TVector);
begin
  if not TSettings.AntiAlias then
    FBitmap.Canvas.LineTo(Round(SceenPoint.X), Round(SceenPoint.Y))
  else begin
    FAntiAliasedBitmap.CanvasBGRA.Pen.Color := FBitmap.Canvas.Pen.Color;
    FAntiAliasedBitmap.CanvasBGRA.Pen.Width := FBitmap.Canvas.Pen.Width;
    FAntiAliasedBitmap.CanvasBGRA.LineTo(Round(SceenPoint.X), Round(SceenPoint.Y));

    //EZLine.LineColor := FBitmap.Canvas.Pen.Color;
    //EZLine.LineWidth := FBitmap.Canvas.Pen.Width;
    //EZLine.Line(LastScreen.X, LastScreen.Y, SceenPoint.X, SceenPoint.Y, FBitmap);
    //FAntiAliasedBitmap := TBGRABitmap.Create(FBitmap);
  end;

  LastScreen := SceenPoint;
{  procedure iSwap(var i1, i2: integer);
  var i: integer;
  begin
    i := i1;
    i1 := i2;
    i2 := i;
  end;

var Steep: boolean;
    ix, dx, dy, iy, dum, x0, y0, x1, y1: integer;
    t, derror, error: extended;
    derror2, error2: integer;
begin // LineTo
    //Bitmap.Canvas.LineTo(Round(x), Round(y));
    //exit;
  x0 := Round(LastX);
  y0 := Round(LastY);
  x1 := Round(x);
  y1 := Round(y);
  Steep := False;
  if abs(x0 - x1) < abs(y0 - y1) then begin // if the line is steep, we transpose the image
    iSwap(x0, y0);
    iSwap(x1, y1);
    Steep := True;
  end;

  if x0 > x1 then begin // make it left-to-right
    iSwap(x0, x1);
    iSwap(y0, y1);
  end;

  dx := x1 - x0;
  dy := y1 - y0;
  derror2 := abs(dy * 2);
  error2 := 0;
  iy := y0;

  for ix := x0 to x1 do begin
    if (steep) then
      Bitmap.Canvas.Pixels[iy, ix] := Bitmap.Canvas.Pen.Color
    else
      Bitmap.Canvas.Pixels[ix, iy] := Bitmap.Canvas.Pen.Color;
    error2 := error2 + derror2;

    if error2 > dx then begin
      if y1 > y0 then
        iy := iy + 1
      else
        iy := iy - 1;
      error2 := error2 - dx * 2;
    end;

  end;
  LastX := x;
  LastY := y; }
end; // LineTo

procedure TBspGraphics.Line3d(const V1, V2: TVector);
begin
  MoveTo(GetPers(V1 + ExplodeDelta));
  LineTo(GetPers(V2 + ExplodeDelta));
end; // MoveTo

procedure TBspGraphics.Polygon3d(Path: array of TVector);
var i: integer;
begin
  for i := 0 to High(Path) do
    Path[i] := GetPers(Path[i] + ExplodeDelta);
  Polygon(Path);
end;

procedure TBspGraphics.DrawBackground;
begin
  if not TSettings.AntiAlias then
    inherited DrawBackground
  else begin

    // задвоение
    if FAntiAliasedBitmap <> nil then
      if (FAntiAliasedBitmap.Width <> FBitMap.Width) or (FAntiAliasedBitmap.Height <> FBitMap.Height) then
        FreeAndNil(FAntiAliasedBitmap);
    if (FAntiAliasedBitmap = nil) and TSettings.AntiAlias then
      FAntiAliasedBitmap := TBGRABitmap.Create(FBitmap);

    with FAntiAliasedBitmap.CanvasBGRA do begin
      Brush.Color := BackgroundColor;
      FillRect(ClipRect);
    end;
  end;
end;

procedure TBspGraphics.Polygon(ScreenPath: array of TVector);
var
  i: integer;
  PPath: array of TPoint;
//  Arr: array of double;
begin
  SetLength(PPath, High(ScreenPath) + 1);
  for i := 0 to High(ScreenPath) do
    PPath[i] := Types.Point(Round(ScreenPath[i].X), Round(ScreenPath[i].Y));
  if not TSettings.AntiAlias then
    with FBitmap.Canvas do
      Polygon(PPath)
  else
    with FAntiAliasedBitmap.CanvasBGRA do begin
      Pen.Color := FBitmap.Canvas.Pen.Color;
      Pen.Width := FBitmap.Canvas.Pen.Width;
      Brush.Color := FBitmap.Canvas.Brush.Color;
      Polygon(PPath);
    end;
end; // Polygon

function TBspGraphics.MouseIsInFace(mx, my: extended; Face: TFace): Boolean;
var
  Vecs: TList<TVector>;
  i, Hits: integer;
  LastX, LastY, CurX, CurY, LeftX: extended;
  Test1, Test2: extended;
  V: TVector;
begin
  LeftX := 0;
  Test1 := 0;
  Test2 := 0;

  if (not IsFaceVisible(Face, ExplodeDelta)) or (Face.GetVertexCount <= 2) then begin
    Result := False;
    exit;
  end;

  Vecs := TList<TVector>.Create;
  for i := 0 to Face.GetVertexCount - 1 do begin
    with GetPers(Face.GetVertex(i) + ExplodeDelta) do
      V := [X, Round(Y), 0];
    Vecs.Add(V);
  end;

  Hits := 0;

  CurX := Vecs[Vecs.Count - 1].X;
  CurY := Vecs[Vecs.Count - 1].Y;

  // Walk the edges of the Verticesygon
  for i := 0 to Vecs.Count - 1 do begin
    LastX := CurX;
    LastY := CurY;

    CurX := Vecs[i].X;
    CurY := Vecs[i].Y;

    if CurY = LastY then
      continue;

    if CurX < LastX then begin
      if mx >= LastX then
        continue;
      LeftX := CurX;
    end else begin
      if mx >= CurX then
        continue;
      LeftX := LastX;
    end;

    if CurY < LastY then begin
      if (my < CurY) or (my >= LastY) then
        continue;
      if mx < LeftX then begin
        Inc(Hits);
        continue;
      end;
      Test1 := mx - CurX;
      Test2 := my - CurY;
    end else begin
      if (my < LastY) or (my >= CurY) then
        continue;
      if mx < LeftX then begin
        Inc(Hits);
        continue;
      end;
      Test1 := mx - LastX;
      Test2 := my - LastY;
    end;

    if Test1 < Test2 / (LastY - CurY) * (LastX - CurX) then
      Inc(Hits);
  end;

  Result := (hits and 1) <> 0;
  Vecs.Free;
end; // MouseIsInFace

function DistanceFromPointToAxis(const Point: TVector; Axis: TAxis): extended;
begin
  Result := TGeometryUtils.DistancePointToLine(Axis.Line, Point);
end; // DistanceFromPointToAxis

// помещает в MouseOverFace, MouseOverPart, MouseOverAxis, MouseOverAxisNo, MouseNearestVertex
// то, что находится под курсором (mx, my)
procedure TBspGraphics.GetMouseParts(Puzzle: TPuzzle; mx, my: integer);
var
  i: integer;
  IsFirst: Boolean;
  Face: TFace;
  Part: TPart;
  Axis: TAxis;
  NearestPoint, Point: TVector;
  d, nd: extended;
begin
  // find Part, Face and Intersection Point under mouse
  TSelection.MouseOverPart := nil;
  TSelection.MouseOverFace := nil;
  TSelection.MouseOverAxisNo := -1;
  TSelection.MouseNearestVertex := -1;
  IsFirst := True;
  for Part in Puzzle.Parts do
    if Part.Visible then begin
      CalcExplode(Part);
      for Face in Part.Faces do
        if MouseIsInFace(mx, my, Face) then begin

          Point := GetIntersectPoint(Face, mx, my);

          if IsFirst or (Point.Y < NearestPoint.Y) then begin
            IsFirst := False;
            TSelection.MouseOverFace := Face;
            TSelection.MouseOverPart := Part;
            TSelection.MouseOver := Point;
            NearestPoint := Point;
          end;
        end;
    end;

  if TSelection.MouseOverPart = nil then
    exit;

  // find nearest axis
  IsFirst := True;
  nd := 1e6;
  for i := 0 to Puzzle.Axes.Count - 1 do begin
    Axis := Puzzle.Axes[i];
    d := DistanceFromPointToAxis(NearestPoint, Axis);
    if IsFirst or (d < nd) then begin
      IsFirst := False;
      nd := d;
      TSelection.MouseOverAxis := Axis;
      TSelection.MouseOverAxisNo := i;
    end;
  end;

  // find nearest vertex
  IsFirst := True;
  nd := 1e6;
  for i := 0 to TSelection.MouseOverFace.GetVertexCount - 1 do begin
    d := (Point - TSelection.MouseOverFace.GetVertex(i)).Abs;
    if IsFirst or (d < nd) then begin
      IsFirst := False;
      nd := d;
      TSelection.MouseNearestVertex := TSelection.MouseOverFace.GetVertexIndex(i);
    end;
  end;
end; // GetMouseParts

procedure TBspGraphics.Select(Puzzle: TPuzzle; mx, my: integer);
begin
  GetMouseParts(Puzzle, mx, my);
  TSelection.Select;
end; // Select

function TBspGraphics.CallbackIsPlaneVisible(const Plane: TPlane): Boolean;
begin
  Result := IsPlaneVisible(Plane);
end;

procedure TBspGraphics.CallbackPaintPart(Part: TPart; Flags: integer);
begin
  PaintPart(Part, Flags);
end;

procedure TBspGraphics.ReCalc;
begin
  inherited;
  CalcPartitioning;
end; // Recalc

procedure TBspGraphics.OnAfterPuzzleLoad;
begin
  CalcPartitioning;
end; // OnAfterPuzzleLoad

procedure TBspGraphics.OnBeforePuzzleTurn(AxisNo: integer);
begin
  CalcPartitioning(AxisNo);
//  TUtils.Log('***' + IntToStr(AxisNo));
end; // OnBeforePuzzleTurn

procedure TBspGraphics.CalcPartitioning(AxisNo: integer = 0);
var
  BlockedPlanes: TList<Integer>;
  i, j, iFrom, iTo, TestFrom, PartNo: integer;
//  Part: TPart;
  Axis: TAxis;
  Parts: TList<TPart>;
  Planes: TList<TPlane>;
  VisibleParts: TList<integer>;
begin // CalcPartitioning
  if FPuzzle = nil then
    exit;
  BlockedPlanes := TList<Integer>.Create;
  Planes := TList<TPlane>.Create;
  Parts := TList<TPart>.Create;
  VisibleParts := TList<integer>.Create;

  try
    iFrom := 0;
    iTo := FPuzzle.Parts.Count - 1;
    if (FPartsFrom >= 0) and (FPartsTo >= 0) then begin
      iFrom := Min(Max(0, FPartsFrom), iTo);
      iTo := Min(iTo, FPartsTo);
    end;
    for i := iFrom to iTo do
      if FPuzzle.Parts[i].Visible then
        VisibleParts.Add(i);

  // ищем заблокированные плоскости
    for PartNo in VisibleParts do begin
      FPuzzle.GetPartPosition(AxisNo, PartNo, iFrom, iTo);
//TUtils.Log('GetPartPosition1 iFrom = ' {+ IntToStr(iFrom) + ' iTo = ' + IntToStr(iTo)});
      for j := iFrom to iTo - 1 do
        if BlockedPlanes.IndexOf(j) = -1 then             // заменить на TDictionary
          BlockedPlanes.Add(j);
    end;

    // создаём полный список плоскостей из осей
    for Axis in FPuzzle.Axes do
      for j := 0 to Axis.Layers.Count - 2 do
        Planes.Add(Axis.GetLayerPlaneTo(j));

    // передаём в Bsp
    FBsp3.Clear;
    if AxisNo < FPuzzle.Axes.Count then begin

      Axis := FPuzzle.Axes[AxisNo];
      //  FBsp3.SetAxis()
      TestFrom := 0;
      for i := 0 to Axis.Layers.Count - 1 do
        if BlockedPlanes.IndexOf(i) = -1 then begin
          for PartNo in VisibleParts do begin
            FPuzzle.GetPartPosition(AxisNo, PartNo, iFrom, iTo);
//TUtils.Log('GetPartPosition2 iFrom = ' {+ IntToStr(iFrom) + ' iTo = ' + IntToStr(iTo)});
            if (iFrom >= TestFrom) and (iTo <= i) then
              Parts.Add(FPuzzle.Parts[PartNo]);
          end;

          FBsp3.AddLayers(Axis.GetLayerPlaneFrom(TestFrom), TestFrom, i, Parts, Planes);
          Parts.Clear;

          TestFrom := i + 1;
        end;
    end;

  finally
    VisibleParts.Free;
    Parts.Free;
    Planes.Free;
    BlockedPlanes.Free;
  end;
end; // CalcPartitioning

procedure TBspGraphics.PaintScene(Puzzle: TPuzzle);
var
  i: integer;
  Planes: TList<TPlane>;
  Part: TPart;
  Face: TFace;
  Plane: TPlane;
begin // PaintScene
  inherited;

  if Puzzle = nil then
    exit;

  if FAntiAliasedBitmap <> nil then
    if (FAntiAliasedBitmap.Width <> FBitMap.Width) or (FAntiAliasedBitmap.Height <> FBitMap.Height) then
      FreeAndNil(FAntiAliasedBitmap);
  if (FAntiAliasedBitmap = nil) and TSettings.AntiAlias then
    FAntiAliasedBitmap := TBGRABitmap.Create(FBitmap);

  FPuzzle := Puzzle;

  FBsp3.Paint(CallbackPaintPart, CallbackIsPlaneVisible);

  if TSettings.ShowAxes then begin
    // Axes
//    Bitmap.Canvas.Pen.Color := RGB(0, 100, 0);
    for i := 0 to Puzzle.Axes.Count - 1 do begin
//      if Puzzle.Axes[i] = MouseOverAxis then
//        FBitmap.Canvas.Pen.Color := RGB(100, 0, 0)
//      else
      PaintAxis(Puzzle.Axes[i]);
    end;

  end;

  if TSettings.ShowNormals then begin
    FBitmap.Canvas.Pen.Color := clBlue;

    Planes := TList<TPlane>.Create;
    try
      for Part in FPuzzle.Parts do
        for Face in Part.Faces do begin
          Plane := Face.GetPlane;
          if Planes.IndexOf(Plane) < 0 then begin
            Planes.Add(Plane);
            PaintPlane(Plane);
          end;
        end;

    finally
      PLanes.Free;
    end;
  end;

  if TSettings.Wired then begin
    FBitmap.Canvas.Pen.Color := 0;
    FBitmap.Canvas.Pen.Width := 1;
    for Part in Puzzle.Parts do begin
      CalcExplode(Part);
      for Face in Part.Faces do
        PaintFaceContour(Face);
    end;
    ExplodeDelta := [0, 0, 0];

//    FBitmap.Canvas.Pen.Color := clRed;
//    PenWidth(2);
//    for i := 0 to Puzzle.Parts.Count - 1 do
//      for j := 0 to Puzzle.Parts[i].Vertices.Count - 1 do
//        PaintSmallSquare(Puzzle.Parts[i].Vertices[j]);

  end;

  if TSettings.AntiAlias then
    FAntiAliasedBitmap.Draw(FBitmap.Canvas, 0, 0, True);
//bmp.CanvasBGRA.Pen.Width := 5;
//bmp.CanvasBGRA.Pen.Style := psDash;
//bmp.CanvasBGRA.Pen.Color := BGRAWhite;
//bmp.CanvasBGRA.MoveTo(10, 10);
//bmp.CanvasBGRA.LineTo(90, 13);
//bmp.Draw(FBitmap.Canvas, 0, 0, True);



  //TestPaint;
//          bmp := TBGRABitmap.Create(100, 100, BGRABlack); // создаем изображение размером 100x100 пикселей с черным фоном
          //bmp := TBGRABitmap.Create(FBitmap);
          //
          //bmp.FillRect(20, 20, 60, 60, BGRAWhite, dmSet); // рисуем белый квадрат без прозрачности
          //bmp.FillRect(40, 40, 80, 80, BGRA(0, 0, 255, 128), dmDrawWithTransparency); // рисуем прозрачный синий квадрат
          //bmp.Canvas.Pen.Width := 5;
          //bmp.Canvas.Pen.Style := psDash;
          //bmp.Canvas.Pen.Color := BGRAWhite;
          //bmp.Canvas.MoveTo(10, 20);
          //bmp.Canvas.LineTo(90, 23);
          //
          //bmp.CanvasBGRA.Pen.Width := 5;
          //bmp.CanvasBGRA.Pen.Style := psDash;
          //bmp.CanvasBGRA.Pen.Color := BGRAWhite;
          //bmp.CanvasBGRA.MoveTo(10, 10);
          //bmp.CanvasBGRA.LineTo(90, 13);
          //bmp.Draw(FBitmap.Canvas, 0, 0, True);
          //bmp.Free;
end; // PaintScene

procedure TBspGraphics.TestPaint;
var v1, v2, v3: TVector;
  Axis: TAxis;
begin
//  FBitmap.Canvas.Draw(400, 200, Texture);
//  PaintPoint(TSelection.MouseOver);
  for Axis in FPuzzle.Axes do begin
    v2 := Axis.NormVector * (TSelection.MouseOver - Axis.BaseVector);
    v2.Normalize;
    PaintRay(TSelection.MouseOver, v2);
  end;
end;

procedure TBspGraphics.CalcExplode(Part: TPart);
var V: TVector;
begin
  ExplodeDelta := [0, 0, 0];
  if FExplode > 10 then begin
    for V in Part.Vertices do
      ExplodeDelta := ExplodeDelta + V;
    ExplodeDelta := ExplodeDelta / Part.Vertices.Count; // Center
    ExplodeDelta := ExplodeDelta * (FExplode / 10 - 1);
  end;
end; // CalcExplode

procedure TBspGraphics.PaintAxis(Axis: TAxis);
const
  L = 0.5;
var
  V: TVector;
  i: integer;
begin
  FBitmap.Canvas.Pen.Color := RGB(0, 100, 0);
  PenWidth(10);

  V := Axis.Line.BaseVector;
  Line3d(V, V + Axis.NormVector * L);

  // Planes
  FBitmap.Canvas.Pen.Color := RGB(100, 0, 100);
  FBitmap.Canvas.Pen.Width := 1;
  for i := 0 to Axis.Layers.Count - 2 do
    PaintRay(Axis.BaseVector + Axis.NormVector * Axis.Layers[i].DistanceTo, Axis.NormVector);
end; // PaintAxis

procedure TBspGraphics.PaintSmallSquare(const ScreenPoint: TVector);
const
  Sq = 2;
var
  x, y: integer;
begin
  x := Round(ScreenPoint.X);
  y := Round(ScreenPoint.Y);
  if not TSettings.AntiAlias then
    with FBitmap.Canvas do begin
  //    Brush.Style := bsClear;
  //    Rectangle(x - Sq, y - Sq, x + Sq, y + Sq);
      MoveTo(x + Sq, y - Sq);
      LineTo(x + Sq, y + Sq);
      LineTo(x - Sq, y + Sq);
      LineTo(x - Sq, y - Sq);
      LineTo(x + Sq, y - Sq);
    end
  else
    with FAntiAliasedBitmap.CanvasBGRA do begin
      Pen.Color := FBitmap.Canvas.Pen.Color;
      Pen.Width := FBitmap.Canvas.Pen.Width;
  //    Brush.Style := bsClear;
  //    Rectangle(x - Sq, y - Sq, x + Sq, y + Sq);
      MoveTo(x + Sq, y - Sq);
      LineTo(x + Sq, y + Sq);
      LineTo(x - Sq, y + Sq);
      LineTo(x - Sq, y - Sq);
      LineTo(x + Sq, y - Sq);
    end

end; // PaintSmallSquare

procedure TBspGraphics.PaintPlane(const Plane: TPlane);
begin
  PaintRay(Plane.NormVect * Plane.NormDistance, Plane.NormVect);
end; // PaintPlane

procedure TBspGraphics.PaintFaceContour(Face: TFace);
var
  i: integer;
  PPath: array of TPoint;
begin
//  Bitmap.Canvas.Pen.Color := RGB(255, 100, 100);
  if not TSettings.AntiAlias then begin

    with FBitmap.Canvas do begin
      Pen.Style := psDot;
      Brush.Style := bsClear;
      PenWidth(1);
      SetLength(PPath, Face.GetVertexCount);
      for i := 0 to Face.GetVertexCount - 1 do
        with GetPers(Face.GetVertex(i) + ExplodeDelta) do
          PPath[i] := Types.Point(Round(X), Round(Y));
      Polygon(PPath);
    end;
  end else begin
    with FAntiAliasedBitmap.CanvasBGRA do begin
      Pen.Color := 0;
      Pen.Style := psDot;
      Brush.Style := bsClear;
      PenWidth(1);

      SetLength(PPath, Face.GetVertexCount);
      for i := 0 to Face.GetVertexCount - 1 do
        with GetPers(Face.GetVertex(i) + ExplodeDelta) do
          PPath[i] := Types.Point(Round(X), Round(Y));
      Polygon(PPath);
      Pen.Style := psSolid;
      Brush.Style := bsSolid;
    end;
  end;

  FBitmap.Canvas.Pen.Style := psSolid;
end; // PaintFaceContour

procedure TBspGraphics.PaintPart(Part: TPart; Flags: integer);
var i: integer;
begin
  CalcExplode(Part);

  if Flags and pfError <> 0 then begin
    Bitmap.Canvas.Pen.Color := RGB(255, Random(100), Random(100));
    for i := 0 to Part.Faces.Count - 1 do
      PaintFaceContour(Part.Faces[i]);
  end else if Part.Visible then
    for i := 0 to Part.Faces.Count - 1 do
      PaintFace(Part.Faces[i]);

  ExplodeDelta := [0, 0, 0];
end; // PaintPart

function TBspGraphics.DimColor(Color1, Color2: Graphics.TColor; Dim: extended): Graphics.TColor; // k from 0 to 1
  function CalcDim(Value1, Value2: integer): integer;
  begin
    Result := Round(Value1 * Dim + Value2 * (1 - Dim) / 2);
  end;
begin // DimColor
  Result := RGB(CalcDim(GetRValue(Color1), GetRValue(Color2)),
                CalcDim(GetGValue(Color1), GetGValue(Color2)),
                CalcDim(GetBValue(Color1), GetBValue(Color2)));
end; // DimColor

procedure TBspGraphics.PaintFace(Face: TFace);

  function FaceLighting: extended;
  var pp: TVector;
  begin
    pp := [0, -1, 0];
    Result := pp ** Face.GetNorm;
  end; // FaceLighting

var
  norm: extended;

var
  i: integer;
  Path: array of TVector;
  FaceColor: TColor;
  V: TVector;
begin // PaintFace
  if not IsFaceVisible(Face, ExplodeDelta) then
    exit;

  norm := 1 - (1 - FaceLighting) * FLighting / 10;
  if norm < 0 then
    norm := 0;

//if CurrentMask > 256 then
//  beep(1, 1);

(*  if DrawMask  then
    Bitmap.Canvas.Brush.Color := {RGB(CurrentMask and 255, (CurrentMask shr 8) * 80, 0)//}TColor(CurrentMask)
  else*)
//  Bitmap.Canvas.Brush.Color := GetFaceColor(Face);
  FaceColor := GetFaceColor(Face);
  FBitmap.Canvas.Brush.Color := DimColor(FaceColor, BackgroundColor, norm);

  if {DrawMask or} Face.AllEdgesVisible and (FaceColor <> -2) and (Face.TextureId < 0) then begin
    FBitmap.Canvas.Pen.Color := GetEdgeColor;
    PenWidth(FPenWidth);
    SetLength(Path, Face.GetVertexCount);
    for i := 0 to Face.GetVertexCount - 1 do
      with GetPers(Face.GetVertex(i) + ExplodeDelta) do
        Path[i] := [X, Y, Z + d];
    Polygon(Path);

//MoveTo(Path[0].X, Path[0].Y, 0);
//for i := 1 to High(Path) do
//  LineTo1(Path[i].X, Path[i].Y, 0);
//LineTo1(Path[0].X, Path[0].Y, 0);


  end else begin
    if FaceColor <> -2 then begin
      FBitmap.Canvas.Pen.Color := FBitmap.Canvas.Brush.Color;
  //Bitmap.Canvas.Pen.Color := 0;
  //    PenWidth(FPenWidth);
      PenWidth(1);
      SetLength(Path, Face.GetVertexCount);
      for i := 0 to Face.GetVertexCount - 1 do begin
        V := Face.GetVertex(i);
        Path[i] := getPers(V + ExplodeDelta);
      end;
      Polygon(Path);
    end;

    if Face.TextureId >= 0 then
      DrawVectorPicture(Face, norm);

    PenWidth(FPenWidth);
    FBitmap.Canvas.Pen.Color := GetEdgeColor;
    for i := 0 to Face.GetVertexCount - 1 do begin
      if Face.IsEdgeVisible(i) then begin
        Line3d(Face.GetVertex(i), Face.GetVertex(i + 1));

//MoveTo(getPersX(Face.Vertices[i]), getPersY(Face.Vertices[i]), 0);
//if i < Face.Vertices.Count - 1 then
//  LineTo1(getPersX(Face.Vertices[i + 1]), getPersY(Face.Vertices[i + 1]), 0)
//else
//  LineTo1(getPersX(Face.Vertices[0]), getPersY(Face.Vertices[0]), 0);

      end;
    end;
  end;
end; // PaintFace

procedure TBspGraphics.DrawVectorPicture(Face: TFace; norm: extended);
var
  i, j: integer;
  vCenter, vNorm, v0, vx, vy, v: TVector;
  Path: array of TVector;
  VectorImage: TVectorImage;
  Points: TList<TIPoint>;
begin // DrawPicture
  if Face.TextureId < 0 then
    exit;
  VectorImage := FPuzzle.Textures[Face.TextureId].VectorImage;
  if VectorImage = nil then
    exit;

//  PenWidth(FPenWidth);
  PenWidth(1);
  FBitmap.Canvas.Pen.Color := GetPictureColor(Face);
  FBitmap.Canvas.Brush.Color := DimColor(FBitmap.Canvas.Pen.Color, BackgroundColor, norm);

  vCenter := [0, 0, 0];

  for i := 0 to Face.GetVertexCount - 1 do begin
    V := Face.GetVertex(i);
    vCenter := vCenter + V;
  end;
  vCenter := vCenter / Face.GetVertexCount;

  v0 := Face.GetVertex(0);

  vNorm := Face.GetNorm;

  vx := v0 - vCenter;
  vx := TMatrix.VectorMul(TMatrix.GetRotateMatrix(vNorm, -Pi / 2), vx);

  vy := vCenter - vx;
  vx := vx + vCenter - v0;
  vy := vy - v0;

  for i := 0 to VectorImage.VectorCommands.Count - 1 do begin
    if VectorImage.VectorCommands[i] is TVectorCommandPenColor then
      PenColor(DimColor(TVectorCommandPenColor(VectorImage.VectorCommands[i]).PenColor, BackgroundColor, norm))
    else if VectorImage.VectorCommands[i] is TVectorCommandBrushColor then
      BrushColor(DimColor(TVectorCommandBrushColor(VectorImage.VectorCommands[i]).BrushColor, BackgroundColor, norm))
    else if VectorImage.VectorCommands[i] is TVectorCommandPolygon then begin
      Points := TVectorCommandPolygon(VectorImage.VectorCommands[i]).Points;
      SetLength(Path, Points.Count);
      for j := 0 to Points.Count - 1 do
        Path[j] := vx * (Points[j].x / VectorImage.Width) + vy * (Points[j].y / VectorImage.Height) + v0;
      Polygon3d(Path);
    end;
  end;
end; // DrawPicture

//procedure TBspGraphics.PaintPoint(Point: TVector);
//var
//  v: TVector;
//begin
//  v := getPers(Point);
//
//  FBitmap.Canvas.Pen.Color := RGB(255, 200, 200);
//  PenWidth(1);
//  PaintSmallSquare(v);
//end; // PaintPlane

procedure TBspGraphics.PaintRay(const VBase, V: TVector);
const
  L = 0.1;
var
  v0, vn: TVector;
begin
  v0 := getPers(VBase);
  vn := getPers(VBase + V.Norm * L);

  PenWidth(1);

  PaintSmallSquare(v0);

  // line in direction of plane's normal
  MoveTo(v0);
  LineTo(vn);
end; // PaintPlane

initialization
//  EZLine := TEZLine.Create;
finalization
//  EZLine.Free;
end.

