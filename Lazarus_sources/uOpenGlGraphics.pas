{************************************************************}
{                                                            }
{  Unit uOpenGlGraphics                                      }
{  2015-2020                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uOpenGlGraphics;

{$MODE Delphi}

interface
uses uVector, uPlane, uFace, uPuzzle, uAxis, uPart, uBaseGraphics,
     LCLIntf, LCLType, Windows, Graphics;

type
  TFPoint = record
    X, Y: extended;
  end;

type
  TOpenGlGraphics = class(TBaseGraphics)
  private
    ExplodeDelta: TVector;

    procedure PaintPuzzle(Puzzle: TPuzzle; DrawLines: Boolean);
    procedure PaintPart1(Part: TPart; DrawLines: Boolean);
    procedure PaintFace(Face: TFace; DrawLines: Boolean);
    procedure PenWidth(Width: extended);
    procedure Line3d(const V1, V2: TVector);
    procedure PaintSmallSquare(const ScreenPoint: TVector);
    procedure CalcExplode(Part: TPart);
  public
    LastScreen: TVector;
    constructor Create;
    procedure SetBitmap(Bitmap: TBitmap); override;

    procedure MoveTo(const ScreenPoint: TVector);
    procedure LineTo(const SceenPoint: TVector);
    procedure Polygon(Path: array of TVector); override;
    procedure Polygon3d(Path: array of TVector); override;
    function MouseIsInFace(mx, my: extended; Face: TFace): Boolean;
    procedure Select(Puzzle: TPuzzle; mx, my: integer); override;
    procedure GetMouseParts(Puzzle: TPuzzle; mx, my: integer); override;

    procedure DrawBackground; override;
    procedure PaintScene(Puzzle: TPuzzle); override;
    procedure PaintPart(Part: TPart; Flags: integer); override;
    procedure PaintAxis(Axis: TAxis);
    procedure PaintPlane(Plane: TPlane);
    procedure PaintRay(const VBase, V: TVector);
  end;

//var OpenGlGraphics: TOpenGlGraphics;

implementation
uses uSelection, uUtils, uGeometryUtils, uLayer,
  Classes, Math, System.UITypes, Generics.Collections, gl, glu;

procedure SetDCPixelFormat(DC: THandle; var Palette: HPalette; ToBitmap: Boolean);
var
  hHeap: THandle;
  nColors, i: Integer;
  lpPalette: PLogPalette;
  byRedMask, byGreenMask, byBlueMask: Byte;
  nPixelFormat: Integer;
  pfd: TPixelFormatDescriptor;
begin
  FillChar(pfd, SizeOf(pfd), 0);
  with pfd do begin
    nSize := sizeof(pfd);
    nVersion := 1;
    if ToBitmap then
      dwFlags := PFD_DRAW_TO_BITMAP or PFD_GENERIC_ACCELERATED or PFD_SUPPORT_OPENGL
    else
      dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cDepthBits := 32;
    iLayerType := PFD_MAIN_PLANE;
  end;
  nPixelFormat := ChoosePixelFormat(DC, @pfd);
  SetPixelFormat(DC, nPixelFormat, @pfd);
  DescribePixelFormat(DC, nPixelFormat, SizeOf(TPixelFormatDescriptor), @pfd);
  if (pfd.dwFlags and PFD_NEED_PALETTE) <> 0 then begin
    nColors := 1 shl pfd.cColorBits;
    hHeap := GetProcessHeap;
    lpPalette := HeapAlloc(hHeap, 0, SizeOf(TLogPalette) + (nColors * SizeOf(TPaletteEntry)));
    lpPalette^.palVersion := $300;
    lpPalette^.palNumEntries := nColors;
    byRedMask := 1 shl pfd.cRedBits - 1;
    byGreenMask := 1 shl pfd.cGreenBits - 1;
    byBlueMask := 1 shl pfd.cBlueBits - 1;
    for i := 0 to nColors - 1 do begin
      lpPalette^.palPalEntry[I].peRed   := (((I shr pfd.cRedShift)   and byRedMask)   * 255) div byRedMask;
      lpPalette^.palPalEntry[I].peGreen := (((I shr pfd.cGreenShift) and byGreenMask) * 255) div byGreenMask;
      lpPalette^.palPalEntry[I].peBlue  := (((I shr pfd.cBlueShift)  and byBlueMask)  * 255) div byBlueMask;
      lpPalette^.palPalEntry[I].peFlags := 0;
    end;
    Palette := CreatePalette(lpPalette^);
    HeapFree(hHeap, 0, lpPalette);
    if Palette <> 0 then begin
      SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
  end;
end; // SetDCPixelFormat

// ================================ TOpenGlGraphics =============================

constructor TOpenGlGraphics.Create;
begin
  inherited;

  Zoom := DEFAULT_ZOOM;
  LastScreen := [0, 0, 0];
end;

procedure TOpenGlGraphics.SetBitmap(Bitmap: Graphics.TBitmap);
var
  Palette: HPalette;
begin
  inherited;

  Palette := 0;
  if Bitmap <> nil then begin
//  try
    FBitmap.PixelFormat := pf24bit;
    // glfwWindowHint(GLFW_SAMPLES, 4);
//WGLisExtensionSupported(0);
    SetDCPixelFormat(FBitmap.Canvas.Handle, Palette, True);
//    Canvas.Draw(10, 10, Pict);
//  finally
//    DeleteObject(Palette);
//  end;
  end;
end;

procedure TOpenGlGraphics.PenWidth(Width: extended);
begin
  FBitmap.Canvas.Pen.Width := Round(Width);
end;

//procedure TOpenGlGraphics.MoveTo(v: TVector);
//begin
//  MoveTo(getPersX(v), getPersY(v));
//end; // MoveTo
//
//procedure TOpenGlGraphics.MoveTo(x, y: extended);
//begin
//  FBitmap.Canvas.MoveTo(Round(x), Round(y))
//end; // MoveTo
//
//procedure TOpenGlGraphics.LineTo(v: TVector);
//begin
//  LineTo(getPersX(v), getPersY(v));
procedure TOpenGlGraphics.MoveTo(const ScreenPoint: TVector);
begin
  FBitmap.Canvas.MoveTo(Round(ScreenPoint.X), Round(ScreenPoint.Y));
  LastScreen := ScreenPoint;
end; // MoveTo

procedure TOpenGlGraphics.LineTo(const SceenPoint: TVector);
begin
  FBitmap.Canvas.LineTo(Round(SceenPoint.X), Round(SceenPoint.Y));
  LastScreen := SceenPoint;
end; // LineTo
//
//procedure TOpenGlGraphics.LineTo(x, y: extended);
//begin
//  FBitmap.Canvas.LineTo(Round(x), Round(y))
//end; // MoveTo
//
procedure TOpenGlGraphics.Line3d(const V1, V2: TVector);
begin
  MoveTo(getPers(V1 + ExplodeDelta));
  LineTo(getPers(V2 + ExplodeDelta));
end; // MoveTo

procedure TOpenGlGraphics.Polygon(Path: array of TVector);
var
  i: integer;
  PPath: array of TPoint;
begin
  with FBitmap.Canvas do begin
    SetLength(PPath, High(Path) + 1);
    for i := 0 to High(Path) do
      PPath[i] := Point(Round(Path[i].X), Round(Path[i].Y));
    Polygon(PPath);
  end;
end; // Polygon

procedure TOpenGlGraphics.Polygon3d(Path: array of TVector);
var i: integer;
begin
//    glColor3f(GetRValue(FaceColor) / 255, GetGValue(FaceColor) / 255, GetBValue(FaceColor) / 255);
  glPolygonMode(GL_FRONT_AND_BACK, GL_LINES);
  glBegin(GL_POLYGON);
//    Norm := Face.GetNormV;

//    with Norm do
//      glNormal3f(X, Y, Z);

  for i := 0 to High(Path) do
    with Path[i] do
      glVertex3f(X, Y, Z);

  glEnd;
end;

// ====================================================================

function TOpenGlGraphics.MouseIsInFace(mx, my: extended; Face: TFace): Boolean;
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

  if (not isFaceVisible(Face, ExplodeDelta)) or (Face.GetVertexCount <= 2) then begin
    Result := False;
    exit;
  end;

  Vecs := TList<Tvector>.Create;
  for i := 0 to Face.GetVertexCount - 1 do begin
    with getPers(Face.GetVertex(i) + ExplodeDelta) do
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
  Result :=  TGeometryUtils.DistancePointToLine(Axis.Line, Point);
end; // DistanceFromPointToAxis

// помещает в MouseOverFace, MouseOverPart, MouseOverAxis, MouseOverAxisNo
// то, что находится под курсором (mx, my)
procedure TOpenGlGraphics.GetMouseParts(Puzzle: TPuzzle; mx, my: integer);
var
  i, j: integer;
  IsFirst: Boolean;
  Face: TFace;
  Part: TPart;
  Axis: TAxis;
  NearestPoint, Point: TVector;
  d, nd: extended;
begin
  // find Part, Face and Intersection Point under mouse
  IsFirst := True;
  for i := 0 to Puzzle.Parts.Count - 1 do begin
    Part := Puzzle.Parts[i];
    if Part.Visible then
      for j := 0 to Part.Faces.Count - 1 do begin
        Face := Part.Faces[j];
        if MouseIsInFace(mx, my, Face) then begin

          Point := GetIntersectPoint(Face, mx, my);

          if IsFirst or (Point.Y < NearestPoint.Y) then begin
            IsFirst := False;
            TSelection.MouseOverFace := Face;
            TSelection.MouseOverPart := Part;
            NearestPoint := Point;
          end;
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
end; // GetMouseParts

procedure TOpenGlGraphics.Select(Puzzle: TPuzzle; mx, my: integer);
begin
  GetMouseParts(Puzzle, mx, my);
  TSelection.Select;
end; // Select

//function CompareDepth(Item1, Item2: Pointer): Integer;
//begin
//  Result := Sign(TFace(Item2).GetDepth - TFace(Item1).GetDepth);
//end;

procedure TOpenGlGraphics.DrawBackground;
begin
  glClearColor(GetRValue(BackgroundColor) / 255, GetGValue(BackgroundColor) / 255, GetBValue(BackgroundColor) / 255, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TOpenGlGraphics.PaintScene(Puzzle: TPuzzle);
var
  hrc: HGLRC;
  i: integer;
  Part: TPart;
  Face: TFace;
begin
  hrc := wglCreateContext(FBitMap.Canvas.Handle);
  wglMakeCurrent(FBitMap.Canvas.Handle, hrc);
  glViewport(0, 0, FWidth, FHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(ArcTan(FHeight/20*3/Zoom) * 2 * 180 / Pi, FWidth/FHeight, 25/12, 55/12);
//  glFrustum(-40, 40, -40, 40, 3, 10);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  gluLookAt(0, -10/3, 0, 0, 0, 0, 0, 0, 1);

  glEnable(GL_DEPTH_TEST);
  DrawBackground;

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightModelI(GL_LIGHT_MODEL_TWO_SIDE, 1);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_CULL_FACE); // go not draw back side of poligone
//glEnable(GL_LINE_SMOOTH);
//glEnable(GL_POLYGON_SMOOTH);
// glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // vertical reflection becase opengl haz (0; 0) at left-down corner of window
  glScalef(1, 1,-1);
  glFrontFace(GL_CW); // clockwise faces

  PaintPuzzle(Puzzle, False);
  PaintPuzzle(Puzzle, True);

  //  if Wired then begin
//    glDisable(GL_LIGHTING);
//    glColor3f(1, 0, 0);
//    glPolygonMode(GL_FRONT, GL_LINE);
//    glLineWidth(10);
//
//    glBegin(GL_POLYGON);
//    for i := 0 to Puzzle.Parts.Count - 1 do
//      for j := 0 to Puzzle.Parts[i].Faces.Count - 1 do
//        for k := 0 to Puzzle.Parts[i].Faces[j].GetVertexCount - 1 do
//          with Puzzle.Parts[i].Faces[j].GetVertex(k) do
//            glVertex3f(X, Y, Z);
//    glEnd;
//    glEnable(GL_LIGHTING);
//  end;

  glFinish;
  glFlush;
  wglMakeCurrent(0, 0);
  wglDeleteContext(hrc);

  if TSettings.ShowAxes then begin
    for i := 0 to Puzzle.Axes.Count - 1 do
      PaintAxis(Puzzle.Axes[i]);
  end;

  if TSettings.ShowNormals then begin
    // BspPlanesPermanent
//    FBitmap.Canvas.Pen.Color := clBlue;
//    for i := 0 to Puzzle.BspPlanesPermanent.Count - 1 do
//      PaintPlane(Puzzle.BspPlanesPermanent[i]);
//
//    // BspPlanesExtra
//    FBitmap.Canvas.Pen.Color := clRed;
//    for i := 0 to Puzzle.BspPlanesExtra.Count - 1 do
//      PaintPlane(Puzzle.BspPlanesExtra[i]);
  end;

  if TSettings.Wired then begin
    FBitmap.Canvas.Pen.Color := 0;
    FBitmap.Canvas.Pen.Width := 1;
    FBitmap.Canvas.Pen.Style := psDot;
    FBitmap.Canvas.Brush.Style := bsClear;
    for Part in Puzzle.Parts do begin
      CalcExplode(Part);
      for Face in Part.Faces do
        for i := 0 to Face.GetVertexCount - 1 do
          Line3d(Face.GetVertex(i), Face.GetVertex(i + 1));
    end;
    FBitmap.Canvas.Pen.Style := psSolid;
  end;
end; // PaintScene

procedure TOpenGlGraphics.PaintPuzzle(Puzzle: TPuzzle; DrawLines: Boolean);
var i, iFrom, iTo: integer;
begin
  iFrom := 0;
  iTo := Puzzle.Parts.Count - 1;
  if (FPartsFrom >= 0) and (FPartsTo >= 0) then begin
    iFrom := Min(Max(0, FPartsFrom), iTo);
    iTo := Min(iTo, FPartsTo);
  end;

  for i := iFrom to iTo do
    PaintPart1(Puzzle.Parts[i], DrawLines);
end;

procedure TOpenGlGraphics.PaintPart(Part: TPart; Flags: integer);
begin

end;

procedure TOpenGlGraphics.CalcExplode(Part: TPart);
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

procedure TOpenGlGraphics.PaintPart1(Part: TPart; DrawLines: Boolean);
var Face: TFace;
begin
  if not Part.Visible then
    exit;

  CalcExplode(Part);

//  if HighlightedParts.IndexOf(Part) >= 0 then
//    MainForm.miWhiteBody.Checked := True;

  for Face in Part.Faces do
    PaintFace(Face, DrawLines);

  ExplodeDelta := [0, 0, 0];
//  MainForm.miWhiteBody.Checked := False;
end; // PaintPart

procedure TOpenGlGraphics.PaintFace(Face: TFace; DrawLines: Boolean);
var i: integer;
    Norm: TVector;
    Color: TColor;
begin
//  if not IsFaceVisible(Face) then // not correct in explode mode
//    exit;

  if not DrawLines then begin
    Color := GetFaceColor(Face);
    if  Color <> -2 then begin
//      glColor3f(GetRValue(Color) / 255, GetGValue(Color) / 255, GetBValue(Color) / 255);
//      glColor3ub(GetRValue(Color), GetGValue(Color), GetBValue(Color));
      glColor3ubv(@Color);

      glPolygonMode(GL_FRONT, GL_FILL);
      glLineWidth(FPenWidth + 1);
      glBegin(GL_POLYGON);
      Norm := Face.GetNorm;

      with Norm do
        glNormal3f(X, Y, Z);

//      for i := Face.GetVertexCount - 1 downto 0 do
      for i := 0 to Face.GetVertexCount - 1 do
        with Face.GetVertex(i) + ExplodeDelta do
          glVertex3f(X, Y, Z);

      glEnd;
    end;

//    if Face is TPictureFace then
//      DrawPicture(Face);
  end else begin
    glDisable(GL_LIGHTING);
    Color := GetEdgeColor;
//    glColor3f(GetRValue(Color) / 255, GetGValue(Color) / 255, GetBValue(Color) / 255);
    glColor3ubv(@Color);
    glPolygonMode(GL_FRONT, GL_LINE);
    glLineWidth(FPenWidth + 1);
    if Face.AllEdgesVisible then begin
      glBegin(GL_POLYGON);
      //for i := Face.GetVertexCount - 1 downto 0 do
      for i := 0 to Face.GetVertexCount - 1 do
        with Face.GetVertex(i) + ExplodeDelta do
          glVertex3f(X, Y, Z);
      glEnd;
    end else begin
      glLineWidth(FPenWidth + 1);
      glBegin(GL_LINES);
      //for i := Face.GetVertexCount - 1 downto 0 do begin
      for i := 0 to Face.GetVertexCount - 1 do begin
        if Face.IsEdgeVisible(i) then begin
          with Face.GetVertex(i) + ExplodeDelta do
            glVertex3f(X, Y, Z);
          with Face.GetVertex(i + 1) + ExplodeDelta do
            glVertex3f(X, Y, Z)
        end;
      end;
      glEnd;

    end;
    glEnable(GL_LIGHTING);
  end;

//  if Face is TPictureFace then
//    DrawPicture(Face);

end; // PaintFace

procedure TOpenGlGraphics.PaintAxis(Axis: TAxis);
const
  L = 0.5;
var
  V: TVector;
  i: integer;
begin
  FBitmap.Canvas.Pen.Color := RGB(0, 100, 100);
  PenWidth(10);

  V := Axis.Line.BaseVector;
  Line3d(V, V + Axis.NormVector * L);

  // Planes
  FBitmap.Canvas.Pen.Color := RGB(100, 0, 100);
  FBitmap.Canvas.Pen.Width := 1;
  for i := 0 to Axis.Layers.Count - 2 do
    PaintRay(Axis.BaseVector + Axis.NormVector * Axis.Layers[i].DistanceTo, Axis.NormVector);
end; // PaintAxis

procedure TOpenGlGraphics.PaintSmallSquare(const ScreenPoint: TVector);
const
  Sq = 2;
var
  x, y: integer;
begin
  x := Round(ScreenPoint.X);
  y := Round(ScreenPoint.Y);
  with FBitmap.Canvas do begin
    MoveTo(x + Sq, y - Sq);
    LineTo(x + Sq, y + Sq);
    LineTo(x - Sq, y + Sq);
    LineTo(x - Sq, y - Sq);
    LineTo(x + Sq, y - Sq);
  end;
end; // PaintSmallSquare

procedure TOpenGlGraphics.PaintPlane(Plane: TPlane);
begin
  PaintRay(Plane.NormVect * Plane.NormDistance, Plane.NormVect);
end; // PaintPlane

procedure TOpenGlGraphics.PaintRay(const VBase, V: TVector);
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

end.

