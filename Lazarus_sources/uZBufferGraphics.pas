{************************************************************}
{                                                            }
{  Unit uZBufferGraphics                                     }
{  2015-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uZBufferGraphics;

{$MODE Delphi}

interface
uses uVector, uPlane, uFace, uPuzzle, uAxis, uPart, uVectorImage, uBaseGraphics,
     Windows, LCLIntf, LCLType, FPImage, Graphics, {EZLines,} Generics.Collections;

type
  TScanRow = record
    ScreenX1, ScreenX2: integer;
    z1, z2: integer;
    R1, R2: integer;
    G1, G2: integer;
    B1, B2: integer;
    tx1, tx2: integer;
    ty1, ty2: integer;
  end;

type
  TPolygonVertex = record
    X, Y, Z, TX, TY: extended;
    Color: TColor;

    LX: integer; // X << wide1
    LY: integer;
    LTX, LTY: integer; // TextureY << wide1
    LZ: integer;
  end;

var
  PolygonArray: array[0..99] of TPolygonVertex;

const
  wide1 = 16;
  wide2 = 24; //$FFFFF

type
  TTexture = record
    FrameBuffer: Pointer;
    width, height: integer;
  end;

type
  Trgb24 = packed record
    Blue,Green,Red : byte;
  end;
  Trgb24scanline = array [word] of Trgb24;
  Prgb24scanline = ^Trgb24scanline;

var
  ScanRows: array of TScanRow;
  YMin, YMax: integer;


  BitmapInfo: TBitmapInfo;
  {FrameBuffer,} DepthBuffer: Pointer;
  Rows: array[0..2500] of Prgb24scanline;

  ptr_screen_pixel: ^Integer;
  ptr_screen_depth: ^Integer;


type
  TZBufferGraphics = class(TBaseGraphics)
  private
    FPuzzle: TPuzzle;
    ExplodeDelta: TVector;
//    FBitmap: Graphics.TBitmap;

    Textures: array of TTexture;
//!!!    FQuickPixels: TQuickPixels;
//    z_lookup: array[1..$FFFFF] of integer; // z_lookup[i] := round((1 shl wide2 {ffffff}) / i);

    procedure DrawVectorPicture(Face: TFace);
    procedure PaintSmallSquare(const ScreenPoint: TVector);
    procedure MoveTo(const ScreenPoint: TVector);
    procedure LineTo(const ScreenPoint: TVector);
    procedure PenWidth(Width: extended);
    procedure Line3d(const V1, V2: TVector);
    procedure CalcExplode(Part: TPart);
  public
    LastScreenPoint: TVector;

    constructor Create;
//    procedure Clear; override;
    destructor Destroy; override;

    procedure SetBitmap(Bitmap: TBitmap); override;

    procedure BorderLineTo(x, y, z: extended); overload;
    procedure Polygon(ScreenPath: array of TVector); override;
    procedure Polygon2(Path: array of TVector);
//    procedure Polygon3(Path: array of TVector);
    procedure Polygon3d(Path: array of TVector); override;

//    procedure CalcLine(id1, id2: integer);

    function MouseIsInFace(mx, my: extended; Face: TFace): Boolean;
    procedure Select(Puzzle: TPuzzle; mx, my: integer); override;
    procedure GetMouseParts(Puzzle: TPuzzle; mx, my: integer); override;

    procedure PaintScene(Puzzle: TPuzzle); override;
    procedure PaintPart(Part: TPart; Flags: integer); override;
    procedure PaintFace(Face: TFace);
    procedure PaintAxis(Axis: TAxis);
    procedure PaintPlane(Plane: TPlane);
    procedure PaintFaceContour(Face: TFace);
    procedure PaintRay(const VBase, V: TVector);

    procedure Resize(Width, Height: integer); override;
    function LoadTexture(fname: string): integer;
  end;

//var ZBufferGraphics: TZBufferGraphics;

implementation

uses uSelection, uUtils, uLayer, uMatrix, uGeometryUtils,
  Classes, Math, System.UITypes,
  {GifImg,} SysUtils;

//!!var EZLine: TEZLine;

// ================================ TZBufferGraphics =============================

procedure Swap(var A, B: Integer);
// Swaps the values A and B
var C: Integer;
begin
  C := A;
  A := B;
  B := C;

{   mov   ecx,[eax]
   xchg  ecx,[edx]
   mov  [eax],ecx}
end;

//procedure SwapVars(var Source, Dest; Size: Integer);
//  // By Mike Heydon, mheydon@eoh.co.za
//  // SwapVars(X1, X2, SizeOf(Integer));
//begin
//  asm
//     push edi
//     push esi
//     mov esi,Source
//     mov edi,Dest
//     mov ecx,Size
//     cld
// @1:
//     mov al,[edi]
//     xchg [esi],al
//     inc si
//     stosb
//     loop @1
//     pop esi
//     pop edi
//  end;
//end;

constructor TZBufferGraphics.Create;
//var
//  Picture: TPicture;
//  i: integer;
begin
  inherited Create;

  Zoom := DEFAULT_ZOOM;
  LastScreenPoint := [0, 0, 0];
//  FQuickPixels := TQuickPixels.Create;

//  for i := 1 to length(z_lookup) do
//    z_lookup[i] := round((1 shl wide2 {ffffff}) / i);
end; // Create

//procedure TBspGraphics.Clear;
//begin
//
//  inherited;
//end;

destructor TZBufferGraphics.Destroy;
begin
//  FQuickPixels.Free;

  FreeMem(DepthBuffer);
  DepthBuffer := nil;

  inherited;
end;

procedure TZBufferGraphics.SetBitmap(Bitmap: TBitmap);
var i: integer;
begin
  inherited;

//  FQuickPixels.Attach(Bitmap);

  if FBitmap <> nil then begin
    for i := 0 to Bitmap.Height - 1 do
      Rows[i] := Prgb24scanline(BitMap.ScanLine[i]);
  end;
end;

procedure TZBufferGraphics.PenWidth(Width: extended);
begin
  FBitmap.Canvas.Pen.Width := Round(Width);
end;

procedure TZBufferGraphics.MoveTo(const ScreenPoint: TVector);
begin
//  if not AntiAlias then
//    FBitmap.Canvas.MoveTo(Round(x), Round(y));

  LastScreenPoint := ScreenPoint;
  LastScreenPoint.Z := LastScreenPoint.Z * 1000;
end; // MoveTo

procedure TZBufferGraphics.LineTo(const ScreenPoint: TVector);
var Steep{, Updown}: boolean;
    x0, y0, z0: integer;
    x1, y1, z1: integer;
    dx, dy: integer;
    ix, iy, iz: integer;
    derry, erry: extended;
    dz, zz: extended;
    Color: TColor;
//    for i := 0 to Bitmap.Height - 1 do
//      Rows[i] := PRGBAQuad(BitMap.ScanLine[i]);
     Row: Prgb24scanline;
     scanline: Prgb24scanline;
begin
  if not TSettings.AntiAlias then begin
//    FBitMap.BeginUpdate;
//    TUtils.Log('6a ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));
    Color := FBitmap.Canvas.Pen.Color;

    x0 := Round(LastScreenPoint.X);
    y0 := Round(LastScreenPoint.Y);
    z0 := Round(LastScreenPoint.Z);
    x1 := Round(ScreenPoint.X);
    y1 := Round(ScreenPoint.Y);
    z1 := Round(ScreenPoint.Z * 1000);

//Bitmap.Canvas.MoveTo(x0, y0);
//Bitmap.Canvas.LineTo(x1, y1);
//exit;

//    Updown := y > LastY;

    Steep := abs(x0 - x1) < abs(y0 - y1);
    if Steep then begin // if the line is steep, we transpose the image
      Swap(x0, y0);
      Swap(x1, y1);
    end;

    if x0 > x1 then begin // make it left-to-right
      Swap(x0, x1);
      Swap(y0, y1);
      Swap(z0, z1);
    end;

    dx := x1 - x0;
    dy := sign(y1 - y0);
    dz := (z1 - z0) / dx;

    derry := abs((y1 - y0) * 2);
    erry := 0;
    iy := y0;
    zz := z0;

//    Row := Rows[iy]; //Prgb24scanline(FBitMap.ScanLine[iy]);
//FBitMap.BeginUpdate(False);
    Row := FBitMap.ScanLine[iy]; //Prgb24scanline(FBitMap.ScanLine[iy]);

//TUtils.Log('6c ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));
    if ((iy >= 0) and (iy < FHeight) and (not Steep)) or
       ((x0 >= 0) and (x0 < FHeight) and Steep) then

      for ix := x0 to x1 do begin
        iz := Round(zz);
        if steep then begin
          ptr_screen_depth := Pointer(DepthBuffer);
          inc(ptr_screen_depth, ix * FWidth + iy);
          if (iy >= 0) and (iy < FHeight) and (ptr_screen_depth^ >= iz) then begin
//            FQuickPixels[iy, ix] := Color
//          if (iy >= 0) and (iy < FHeight) then begin

            scanline := FBitmap.ScanLine[ix];
//          for x := 100 to 200{FBitmap.Width - 1} do begin
            scanline^[iy].Red   := GetRValue(Color);
            scanline^[iy].Green := GetGValue(Color);
            scanline^[iy].Blue  := GetBValue(Color);
          end;

          //FBitMap.Canvas.Pixels[iy, ix] := 255;
          //FBitMap.Canvas.Pixels[100, 100] := 255;
          //  Row[ix].Red := 255;//GetRValue(Color);
          //  Row[ix].Green := GetGValue(Color);
          //  Row[ix].Blue := GetBValue(Color);
          //end;
        end else begin
          ptr_screen_depth := Pointer(DepthBuffer);
          inc(ptr_screen_depth, iy * FWidth + ix);
//          if (ix >= 0) and (ix < FWidth) and (ptr_screen_depth^ >= iz) then
//            FQuickPixels[ix, iy] := Color;
        end;

        erry := erry + derry;

        if erry > dx then begin
          iy := iy + dy;
          erry := erry - dx * 2;
        end;
        zz := zz + dz;
      end;
//    TUtils.Log('6d ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));
//    FBitMap.EndUpdate;
  end else begin
//    EZLine.LineColor := FBitmap.Canvas.Pen.Color;
//    EZLine.LineWidth := FBitmap.Canvas.Pen.Width;
//    EZLine.Line(LastScreenPoint.X, LastScreenPoint.Y, ScreenPoint.X, ScreenPoint.Y, FBitmap);
  end;
//FBitMap.EndUpdate();

  LastScreenPoint := ScreenPoint;
end; // LineTo

procedure TZBufferGraphics.Line3d(const V1, V2: TVector);
begin
  MoveTo(getPers(V1 + ExplodeDelta));
  LineTo(getPers(V2 + ExplodeDelta));
end; // MoveTo

procedure TZBufferGraphics.Polygon3d(Path: array of TVector);
var i: integer;
begin
  for i := 0 to High(Path) do
    Path[i] := GetPers(Path[i] + ExplodeDelta);
  Polygon(Path);
end;

procedure TZBufferGraphics.Polygon(ScreenPath: array of TVector);
var
  i: integer;
  PPath: array of TPoint;
//  Arr: array of extended;
begin
  with FBitmap.Canvas do begin
//Pen.Color := Brush.Color;
    SetLength(PPath, High(ScreenPath) + 1);
    for i := 0 to High(ScreenPath) do begin
      PPath[i].X := Round(ScreenPath[i].X);
      PPath[i].Y := Round(ScreenPath[i].Y);
    end;

    Polygon(PPath);
//    Polyline(PPath);
//    MoveTo(PPath[High(Path)].X, PPath[High(Path)].Y);
//    LineTo(PPath[0].X, PPath[0].Y);

  end;

end; // Polygon

procedure TZBufferGraphics.BorderLineTo(x, y, z: extended);
var Steep, Updown: boolean;
    x0, y0, z0: integer;
    x1, y1, z1: integer;
    dx, dy: integer;
    ix, iy: integer;
    derry, erry: extended;
    dz, zz: extended;
begin // BorderLineTo
    //Bitmap.Canvas.LineTo(Round(x), Round(y));
    //exit;
  x0 := Round(LastScreenPoint.X);
  y0 := Round(LastScreenPoint.Y);
  z0 := Round(LastScreenPoint.Z);
  x1 := Round(x);
  y1 := Round(y);
  z1 := Round(z * 1000);

  Updown := y > LastScreenPoint.Y;

  Steep := abs(x0 - x1) < abs(y0 - y1);
  if Steep then begin // if the line is steep, we transpose the image
    Swap(x0, y0);
    Swap(x1, y1);
  end;

  if x0 > x1 then begin // make it left-to-right
    Swap(x0, x1);
    Swap(y0, y1);
    Swap(z0, z1);
  end;

  dx := x1 - x0;
  dy := sign(y1 - y0);
  dz := (z1 - z0) / dx;

  derry := abs((y1 - y0) * 2);
  erry := 0;
  iy := y0;
  zz := z0;

  for ix := x0 to x1 do begin
    if Updown then begin
      if (steep) then begin
        if (ix >= 0) and (ix < FHeight) and (ScanRows[ix].ScreenX1 > iy) then begin
          ScanRows[ix].ScreenX1 := iy;
          ScanRows[ix].Z1 := Round(zz);
        end;
      end else
        if (iy >= 0) and (iy < FHeight) and (ScanRows[iy].ScreenX1 > ix) then begin
          ScanRows[iy].ScreenX1 := ix;
          ScanRows[iy].Z1 := Round(zz);
        end;
    end else begin
      if (steep) then begin
        if (ix >= 0) and (ix < FHeight) and (ScanRows[ix].ScreenX2 < iy) then begin
          ScanRows[ix].ScreenX2 := iy;
          ScanRows[ix].Z2 := Round(zz);
        end;
      end else begin
        if (iy >= 0) and (iy < FHeight) and (ScanRows[iy].ScreenX2 < ix) then begin
          ScanRows[iy].ScreenX2 := ix;
          ScanRows[iy].Z2 := Round(zz);
        end;
      end;
    end;

    erry := erry + derry;

    if erry > dx then begin
      iy := iy + dy;
      erry := erry - dx * 2;
    end;
    zz := zz + dz;
  end;
  LastScreenPoint := [x, y, z * 1000];
end; // BorderLineTo

const
    MaxPixelCount   =  32768;

type
  TRGBArray = array[0..MaxPixelCount - 1] of TRGBQuad; // rgba
//  pRGBArray = ^TRGBArray;

var NPolygons: integer;

procedure TZBufferGraphics.Polygon2(Path: array of TVector);
var
  i, j, Y: integer;
//  iR, iG, iB: byte;
  Color: TColor;

  z, dz: extended;
  k1, iz: integer;
  scanline: Prgb24scanline;
begin // Polygon2
  Inc(NPolygons);

//  Polygon(Path);
//exit;

  for j := 0 to FHeight - 1 do begin
    ScanRows[j].ScreenX1 := High(Integer);
    ScanRows[j].ScreenX2 := Low(Integer);
  end;

  for i := 0 to High(Path) do begin
    PolygonArray[i].X := Path[i].X;
    PolygonArray[i].Y := Path[i].Y;
    PolygonArray[i].LX := Round(Path[i].X) shl Wide1;
    PolygonArray[i].LY := Round(Path[i].Y) shl Wide1;
  end;

//  PolygonArray[0].LTX := 0;
//  PolygonArray[0].LTY := 0;
//  PolygonArray[1].LTX := 0;
//  PolygonArray[1].LTY := 200;
//  PolygonArray[2].LTX := 200;
//  PolygonArray[2].LTY := 200;
//  PolygonArray[3].LTX := 200;
//  PolygonArray[3].LTY := 0;

  // преобразуем полигон в целые координаты
  YMin := FHeight - 1;
  YMax := 0;
  for i := 0 to High(Path) do begin
    if PolygonArray[i].LZ > 0 then begin
//          _p[i].z := _p[i].z shl 6;
      PolygonArray[i].LZ := (1 shl wide2) div (PolygonArray[i].LZ);
//      PolygonArray[i].LTX := PolygonArray[i].LTX * PolygonArray[i].LZ;
//      PolygonArray[i].LTY := PolygonArray[i].LTY * PolygonArray[i].LZ;
//          PolygonArray[i].LX := PolygonArray[i].LX shl wide1; //16bit
    end else
      PolygonArray[i].LZ := 0;

    Y := Round(Path[i].Y);
//      PPath[i] := Point(X, Y);
    if YMin > Y then
      YMin := Y;
    if YMax < Y then
      YMax := Y;
  end;
  if YMin < 0 then
    YMin := 0;
  if YMax >= FHeight then
    YMax := FHeight - 1;

  with FBitmap.Canvas do //begin
    Color := Brush.Color;

//    iR := GetRValue(Color);
//    iG := GetGValue(Color);
//    iB := GetBValue(Color);
//Pen.Color := Brush.Color;

  // инициализируем строки
//  for i := Low(ScanRows) to High(ScanRows) do begin
  for i := YMin to YMax do begin
    ScanRows[i].ScreenX1 := High(Integer);
    ScanRows[i].ScreenX2 := Low(Integer);
  end;

  Self.MoveTo(Path[0]);
  for i := 1 to High(Path) do
    BorderLineTo(Path[i].X, Path[i].Y, Path[i].Z);
  BorderLineTo(Path[0].X, Path[0].Y, Path[0].Z);

  for i := YMin to YMax do
    with ScanRows[i] do begin
      scanline := FBitmap.ScanLine[i];
      z := z1;

      if ScreenX2 <> ScreenX1 then begin
        dz := (z2 - z1) / (ScreenX2 - ScreenX1);

        ptr_screen_depth := Pointer(DepthBuffer);
        k1 := i * FWidth + ScanRows[i].ScreenX1;
        inc(ptr_screen_depth, k1);
        for j := ScreenX1 to ScreenX2 do begin
          iz := Round(z);

          if (j >= 0) and (j < FWidth) and (ptr_screen_depth^ >= iz) then begin
            scanline^[j].Red   := GetRValue(Color);
            scanline^[j].Green := GetGValue(Color);
            scanline^[j].Blue  := GetBValue(Color);
//            FQuickPixels[j, i] := Color;
            ptr_screen_depth^ := iz;
          end;
          inc(ptr_screen_depth);

          z := z + dz;
        end;
      end;
  end;

end; // Polygon2

//procedure TZBufferGraphics.Polygon3(Path: array of TVector);
//var Verts: array of TPolygonVertex; // TODO перенести в private, объявить как [0..100]
//    sl: TStringList;
//
//  procedure CalcLine(id1, id2: integer);
//  var
//    sn, sx: integer;
//    ScreenX, ScreenY, dScreenX, dScreenY: integer;
//    tx, ty, dtx, dty: integer;
//    z, dz: integer;
//  begin // DrawLine
//
//    if Verts[id1].LY = Verts[id2].LY then
//      exit;
//
////sl.Add(IntToStr(Round(Verts[id1].TextureX / Verts[id1].z)));
//
//    if Verts[id2].LY < Verts[id1].LY then begin
//      sn := id1;
//      id1 := id2;
//      id2 := sn;
//    end;
//
//    ScreenX := Verts[id1].LX;
//    tx := Verts[id1].LTX;
//    ty := Verts[id1].LTY;
//    z := Verts[id1].LZ;
//
////sl.Add(IntToStr(Round(tx / z)));
//  //        ShowMessage(IntToStr(u) + ' ' + IntToStr(v));
//
//    dScreenY := Verts[id2].LY - Verts[id1].LY;
//    dScreenX := (Verts[id2].LX - Verts[id1].LX) div dScreenY;
//    dtx := (Verts[id2].LTX - Verts[id1].LTX) div dScreenY;
//    dty := (Verts[id2].LTY - Verts[id1].LTY) div dScreenY;
//    dz := (Verts[id2].LZ - Verts[id1].LZ) div dScreenY;
//
////sl.Add('');
//    for ScreenY := Verts[id1].LY to Verts[id2].LY do begin
//
//      if (ScreenY >= 0) and (ScreenY < FHeight) then begin
//        if ScreenX >= 0 then
//          sx := ScreenX shr wide1
//        else
//          sx := -((-ScreenX) shr wide1);
//
//        if (sx < ScanRows[ScreenY].ScreenX1) then begin
//          ScanRows[ScreenY].ScreenX1 := sx;
//          ScanRows[ScreenY].tx1 := tx;
//          ScanRows[ScreenY].ty1 := ty;
//          ScanRows[ScreenY].z1 := z;
//        end;
//
//        if (sx > ScanRows[ScreenY].ScreenX2) then begin
//          ScanRows[ScreenY].ScreenX2 := sx;
//          ScanRows[ScreenY].tx2 := tx;
//          ScanRows[ScreenY].ty2 := ty;
//          ScanRows[ScreenY].z2 := z;
////sl.Add(IntToStr(Round(tx / z)));
//        end;
//  //        ShowMessage(IntToStr(u) + ' ' + IntToStr(v));
//
//      end;
//
//      ScreenX := ScreenX + dScreenX;
//      tx := tx + dtx;
//      ty := ty + dty;
//      z := z + dz;
//    end;
//  end; // CalcLine
//
//
//var
//  i, j, tx, ty: integer;
////  iR, iG, iB: byte;
////  PPath: array of TPoint;
////  Arr: array of extended;
////  Row1, Row2, Row3, DestRow: pRGBArray;
////  Triple0, Triple: PRGBTriple;
////  ScanRow: TScanRow;
////  Color: TColor;
//
//  k1, z, dScreenX, dtx, dty, dz, z0, TextureX, TextureY: integer;
//begin // Polygon3
////  Inc(NPolygons);
////  Polygon(Path);
////  exit;
//
//  for j := 0 to FHeight - 1 do begin
//    ScanRows[j].ScreenX1 := High(Integer);
//    ScanRows[j].ScreenX2 := Low(Integer);
//  end;
//
//  SetLength(Verts, High(Path) + 1);
//  for i := 0 to High(Path) do begin
//    Verts[i].LX := Round(Path[i].X) shl Wide1;
//    Verts[i].LY := Round(Path[i].Y) shl Wide1;
//    Verts[i].LZ := Round(Path[i].Z);//Round(Path[i].Z+300);
////    Verts[i].TextureX := Verts[i].ScreenX div 100 + 10;
////    Verts[i].TextureY := Verts[i].ScreenY div 100 + 10;
//  end;
//
//  Verts[0].LTX := 0;
//  Verts[0].LTY := 0;
//  Verts[1].LTX := 0;
//  Verts[1].LTY := 200;
//  Verts[2].LTX := 200;
//  Verts[2].LTY := 200;
//  Verts[3].LTX := 200;
//  Verts[3].LTY := 0;
//
//sl := TStringList.Create;
//
//  for i := 0 to High(Path) do
//    if Verts[i].LZ > 0 then begin
////      Verts[i].LZ := z_lookup[Verts[i].LZ];// (1 shl 24) div (Verts[i].z);
//      Verts[i].LTX := Verts[i].LTX * Verts[i].LZ;
//      Verts[i].LTY := Verts[i].LTY * Verts[i].LZ;
////      Verts[i].LX := Verts[i].LX shl wide1; //16bit
////if ScanRows[j].z2 <> 0 then
////sl.Add(IntToStr(i) + ': ' + IntToStr(ScanRows[j].tx1) + ' ' + IntToStr(ScanRows[j].tx2) + ' ' + IntToStr(Round(ScanRows[j].tx2 / ScanRows[j].z2)));
//    end else
//      Verts[i].LZ := 0;
//
//    CalcLine(2, 3);
//
////  for i := 0 to High(Path) - 1 do
////    DrawLine(i, i + 1);
//
////  DrawLine(High(Path), 0);
//  for j := 0 to FHeight - 1 do begin
//
////if j = 850 then
//
//    if ScanRows[j].z2 <> 0 then
//      sl.Add(IntToStr(j) + ': ' + IntToStr(ScanRows[j].tx1) + ' ' + IntToStr(ScanRows[j].tx2) + ' ' + IntToStr(Round(ScanRows[j].tx2 / ScanRows[j].z2)));
////        if Scanlines[j]._x2 > ww-1 then
////          ShowMessage(IntToStr(Scanlines[j]._x2));
//
//    k1 := j * FWidth + ScanRows[j].ScreenX1;
////    ptr_screen_pixel := Pointer(FrameBuffer);
////    inc(ptr_screen_pixel, k1);
//
//    ptr_screen_depth := Pointer(DepthBuffer);
//    inc(ptr_screen_depth, k1);
//
//    tx := (ScanRows[j].tx1);
//    ty := (ScanRows[j].ty1);
//    z := (ScanRows[j].z1);
//
//    dScreenX := ScanRows[j].ScreenX2 - ScanRows[j].ScreenX1 + 1; // +1?
//    if dScreenX <= 0 then
//      continue;
//    if ScanRows[j].ScreenX2 < 0 then
//      continue;
//
//    dtx := ((ScanRows[j].tx2 - ScanRows[j].tx1)) div dScreenX;
//    dty := ((ScanRows[j].ty2 - ScanRows[j].ty1)) div dScreenX;
//    dz := ((ScanRows[j].z2 - ScanRows[j].z1)) div dScreenX;
//
//    for i := 0 to dScreenX - 1 do begin
//      if (ScanRows[j].ScreenX1 + i >= 0) and (ScanRows[j].ScreenX1 + i < FWidth) then begin
//
////        z0 := z_lookup[z]; // lookup z0=1/z   z_lookup[i] := round((1 shl wide2 {ffffff}) / i);
//    // z0:=1;
//        TextureX := (z0 * tx) shr wide2; // tx / z
//        TextureY := (z0 * ty) shr wide2; // ty / z
//
//        if TextureX < 0 then
//          TextureX := 0;
//        if TextureY < 0 then
//          TextureY := 0;
//
//        if TextureX >= Textures[0].width then
//          TextureX := Textures[0].width - 1;
//        if TextureY >= Textures[0].height then
//          TextureY := Textures[0].height - 1;
//
//      // z-test
//        if z0 < ptr_screen_depth^ then begin
////          ptr_texture_pixel := Pointer(Textures[0].FrameBuffer);
////          inc(ptr_texture_pixel, Textures[0].width * TextureY + TextureX);
////          ptr_screen_pixel^ := ptr_texture_pixel^;
//          ptr_screen_depth^ := z0;
//
//  //        FQuickPixels.Pixels[ScanRows[j].ScreenX1 + i, j] := ptr_texture_pixel^;
//
//          //ptr_screen_pixel^ := ptr_screen_pixel^ and pixelshader;
//
//       // if i=0 then       ptr_screen_pixel^:=rgb(0,m,0);
//
//        end;
//      end;
//
//
//      tx := tx + dtx;
//      ty := ty + dty;
//      z := z + dz;
//
//      inc(ptr_screen_pixel, 1);
//      inc(ptr_screen_depth, 1);
//
//    end;
//
//  end;
//  sl.SaveToFile('qq');
//
//end; // Polygon3


//procedure TZBufferGraphics.CalcLine(id1, id2: integer);
//var
//  sn, sx: integer;
//  ScreenX, ScreenY, dScreenX, dScreenY: integer;
//  tx, ty, dtx, dty: integer;
//  z, dz: integer;
//begin
//
//  if PolygonArray[id1].LY = PolygonArray[id2].LY then
//    exit;
//
//  if PolygonArray[id2].LY < PolygonArray[id1].LY then begin
//    sn := id1;
//    id1 := id2;
//    id2 := sn;
//  end;
//
//  ScreenX := PolygonArray[id1].LX;
//  tx := PolygonArray[id1].LTX;
//  ty := PolygonArray[id1].LTY;
//  z := PolygonArray[id1].LZ;
//
////        ShowMessage(IntToStr(u) + ' ' + IntToStr(v));
//
//  dScreenY := (PolygonArray[id2].LY - PolygonArray[id1].LY);
//  dScreenX := (PolygonArray[id2].LX - PolygonArray[id1].LX) div dScreenY;
//  dtx := (PolygonArray[id2].LTX - PolygonArray[id1].LTX) div dScreenY;
//  dty := (PolygonArray[id2].LTY - PolygonArray[id1].LTY) div dScreenY;
//  dz := (PolygonArray[id2].LZ - PolygonArray[id1].LZ) div dScreenY;
//
//  for ScreenY := PolygonArray[id1].LY to PolygonArray[id2].LY do begin
//
//    if (ScreenY > 0) and (ScreenY < FHeight) then begin
//      sx := abs(ScreenX) shr wide1; // !!!!!!!
//
//      if (sx < ScanRows[ScreenY].ScreenX1) then begin
//        ScanRows[ScreenY].ScreenX1 := sx;
//        ScanRows[ScreenY].tx1 := tx;
//        ScanRows[ScreenY].ty1 := ty;
//        ScanRows[ScreenY].z1 := z;
//      end;
//
//      if (sx > ScanRows[ScreenY].ScreenX2) then begin
//        ScanRows[ScreenY].ScreenX2 := sx;
//        ScanRows[ScreenY].tx2 := tx;
//        ScanRows[ScreenY].ty2 := ty;
//        ScanRows[ScreenY].z2 := z;
//      end;
////        ShowMessage(IntToStr(u) + ' ' + IntToStr(v));
//
//    end;
//
//    ScreenX := ScreenX + dScreenX;
//    tx := tx + dtx;
//    ty := ty + dty;
//    z := z + dz;
//  end;
//end; // CalcLine



// ====================================================================

function TZBufferGraphics.MouseIsInFace(mx, my: extended; Face: TFace): Boolean;
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
  Result := TGeometryUtils.DistancePointToLine(Axis.Line, Point);
end; // DistanceFromPointToAxis

// помещает в MouseOverFace, MouseOverPart, MouseOverAxis, MouseOverAxisNo
// то, что находится под курсором (mx, my)
procedure TZBufferGraphics.GetMouseParts(Puzzle: TPuzzle; mx, my: integer);
var
  i, j: integer;
  IsFirst: Boolean;
  Face: TFace;
  Part: TPart;
  Axis: TAxis;
  Point, NearestPoint: TVector;
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
    d := Abs(DistanceFromPointToAxis(NearestPoint, Axis));
    if IsFirst or (d < nd) then begin
      IsFirst := False;
      nd := d;
      TSelection.MouseOverAxis := Axis;
      TSelection.MouseOverAxisNo := i;
    end;
  end;
end; // GetMouseParts

procedure TZBufferGraphics.Select(Puzzle: TPuzzle; mx, my: integer);
begin
  GetMouseParts(Puzzle, mx, my);
  TSelection.Select;
end; // Select

//function CompareDepth(Item1, Item2: Pointer): Integer;
//begin
//  Result := Sign(TFace(Item2).GetDepth - TFace(Item1).GetDepth);
//end;

procedure TZBufferGraphics.CalcExplode(Part: TPart);
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

procedure TZBufferGraphics.PaintScene(Puzzle: TPuzzle);
var
  i, iFrom, iTo: integer;
//  SortedFaces: TFaces;
  Part: TPart;
  Face: TFace;
  Axis: TAxis;
  VisibleParts: TList<TPart>;
//  scanline: Prgb24scanline;
begin
  inherited;

  FBitmap.BeginUpdate;

  FPuzzle := Puzzle;

  VisibleParts := TList<TPart>.Create;
  try
    iFrom := 0;
    iTo := Puzzle.Parts.Count - 1;
    if (FPartsFrom >= 0) and (FPartsTo >= 0) then begin
      iFrom := Min(Max(0, FPartsFrom), iTo);
      iTo := Min(iTo, FPartsTo);
    end;
    for i := iFrom to iTo do
      if Puzzle.Parts[i].Visible then
        VisibleParts.Add(Puzzle.Parts[i]);

  //  fillchar(FrameBuffer^, FWidth * FHeight * 4, $7F);
    fillchar(DepthBuffer^, FWidth * FHeight * 4, $7F);

    // at first draw uncolored faces
    for Part in VisibleParts do begin
      CalcExplode(Part);
      for Face in Part.Faces do
        if (not Face.IsColored) and (Face.TextureId = -1) then
          PaintFace(Face);
    end;

    // at second draw colored or textured faces
    for Part in VisibleParts do begin
      CalcExplode(Part);
        for Face in Part.Faces do
          if (Face.Color <> -1) or (Face.TextureId >= 0) then
            PaintFace(Face);
    end;

    ExplodeDelta := [0, 0, 0];


  //  if Puzzle.UseBspEx and (Puzzle.TurningState in [tsMidTurn, tsTurningAnimated]) then
  //    Puzzle.bspEx.Paint(FBitmap)
  //  else
  //    Puzzle.bsp.Paint(FBitmap);

  //  BitmapInfo.bmiHeader.biHeight := -fHeight; // ???
  //    SetDIBitsToDevice(Bitmap.Canvas.Handle,
  //      0, 0, FWidth, FHeight, 0, 0, 0, FHeight,
  //      FrameBuffer, BitmapInfo, DIB_RGB_COLORS);

    if TSettings.ShowAxes then begin
      // Axes
      for Axis in Puzzle.Axes do
        PaintAxis(Axis);
    end;

    if TSettings.ShowNormals then begin
      // BspPlanesPermanent
  //    FBitmap.Canvas.Pen.Color := clBlue;
  //    for i := 0 to Puzzle.BspPlanesPermanent.Count - 1 do
  //      PaintPlane(Puzzle.BspPlanesPermanent[i]);

      // BspPlanesExtra
  //    FBitmap.Canvas.Pen.Color := clRed;
  //    for i := 0 to Puzzle.BspPlanesExtra.Count - 1 do
  //      PaintPlane(Puzzle.BspPlanesExtra[i]);
    end;

    if TSettings.Wired then begin
      FBitmap.Canvas.Pen.Color := 0;
      FBitmap.Canvas.Pen.Width := 1;
      for Part in Puzzle.Parts do begin
        CalcExplode(Part);
        for Face in Part.Faces do
          PaintFaceContour(Face);
      end;
    end;
  finally
    VisibleParts.Free;
  end;

  //FBitmap.Canvas.Pen.Color := RGB(255,100,100);
  //FBitmap.Canvas.Pen.Width := 2;
  //MoveTo(Vector(100,100,3));
  //LineTo(Vector(300,300,3));
  //  Bitmap.Canvas.Draw(400, 200, Texture);

//for y := 0 to FBitmap.Height - 1 do
//  begin
//    scanline := FBitmap.ScanLine[y];
//    for x := 0 to FBitmap.Width - 1 do begin
//      scanline^[x].Red   := 255;
//      scanline^[x].Green := 0;
//      scanline^[x].Blue  := 0;
//    end;
//  end;

FBitmap.EndUpdate;


end; // PaintScene

procedure TZBufferGraphics.PaintAxis(Axis: TAxis);
const
  L = 0.5;
var
  v: TVector;
  i: integer;
begin
  if Axis <> TSelection.TurnAxis then
    FBitmap.Canvas.Pen.Color := RGB(200, 100, 0)
  else
    FBitmap.Canvas.Pen.Color := RGB(150, 100, 0);

  PenWidth(10);

  V := Axis.Line.BaseVector;
  Line3d(V, V + Axis.NormVector * L);

  with getPers(V) do
    FBitmap.Canvas.MoveTo(Round(X), Round(Y));
  with getPers(V + Axis.NormVector * L) do
    FBitmap.Canvas.LineTo(Round(X), Round(Y));

  // Planes
  FBitmap.Canvas.Pen.Color := RGB(100, 0, 100);
  FBitmap.Canvas.Pen.Width := 1;
  for i := 0 to Axis.Layers.Count - 2 do
    PaintRay(Axis.BaseVector + Axis.NormVector * (Axis.Layers[i].DistanceTo), Axis.NormVector);
end; // PaintAxis

procedure TZBufferGraphics.PaintSmallSquare(const ScreenPoint: TVector);
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
end;

procedure TZBufferGraphics.PaintPlane(Plane: TPlane);
const
  L = 0.1;
var
  v0, vn: TVector;
begin
  v0 := getPers(Plane.NormVect * Plane.NormDistance);
  vn := getPers(Plane.NormVect * (L + Plane.NormDistance));

  PenWidth(1);

  PaintSmallSquare(v0);

  // line in direction of plane's normal
  with v0 do
    FBitmap.Canvas.MoveTo(Round(X), Round(Y));
  with vn do
    FBitmap.Canvas.LineTo(Round(X), Round(Y));
end; // PaintPlane

procedure TZBufferGraphics.PaintFaceContour(Face: TFace);
var
  i: integer;
  PPath: array of TPoint;
begin
//  Bitmap.Canvas.Pen.Color := RGB(255, 100, 100);
  FBitmap.Canvas.Pen.Style := psDot;
  FBitmap.Canvas.Brush.Style := bsClear;
  PenWidth(1);

  with FBitmap.Canvas do begin
    SetLength(PPath, Face.GetVertexCount);
    for i := 0 to Face.GetVertexCount - 1 do
      with getPers(Face.GetVertex(i) + ExplodeDelta) do begin
        PPath[i].X := Round(X);
        PPath[i].Y := Round(Y);
      end;
    Polygon(PPath);
  end;

  FBitmap.Canvas.Pen.Style := psSolid;
end; // PaintFaceContour

procedure TZBufferGraphics.PaintPart(Part: TPart; Flags: integer);
var i: integer;
  V: TVector;
begin
  if not Part.Visible then
    exit;

  ExplodeDelta := [0, 0, 0];
  if FExplode > 10 then begin
    for V in Part.Vertices do
      ExplodeDelta := ExplodeDelta + V;
    ExplodeDelta := ExplodeDelta / Part.Vertices.Count; // Center
    ExplodeDelta := ExplodeDelta * (FExplode / 10 - 1);
  end;

  for i := 0 to Part.Faces.Count - 1 do
    PaintFace(Part.Faces[i]);

  ExplodeDelta := [0, 0, 0];
end; // PaintPart

procedure TZBufferGraphics.PaintFace(Face: TFace);

  function FaceLighting: extended;
  var pp: TVector;
  begin
    pp := [0, -1, 0];
    Result := pp ** Face.GetNorm;
  end; // FaceLighting

var
  norm: extended;

  function CalcColor(Value1, Value2: integer): integer;
  begin
    Result := Round(Value1 * norm + Value2 * (1 - norm) / 2);
  end;

var
  i: integer;
  Path: array of TVector;
  FaceColor: TColor;
//  V: TVector;
begin // PaintFace
//  TUtils.Log('1 ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));
//  ShowMessage(ColorToStr(FBitMap.Canvas.Pixels[10, 10]));
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
  if FaceColor <> -2 then begin

    FBitmap.Canvas.Brush.Color := RGB(CalcColor(GetRValue(FaceColor), GetRValue(BackgroundColor)),
                                      CalcColor(GetGValue(FaceColor), GetGValue(BackgroundColor)),
                                      CalcColor(GetBValue(FaceColor), GetBValue(BackgroundColor)));

  //  if {DrawMask or} (Face.InvisibleEdges = 0) then begin
  //    FBitmap.Canvas.Pen.Color := GetEdgeColor;
  //    PenWidth(FPenWidth);
  //    SetLength(Path, Face.Vertices.Count);
  //    for i := 0 to Face.Vertices.Count - 1 do begin
  //      V := Face.Vertices[i];
  //      Path[i] := F3Point(getPersX(V), getPersY(V), getPersZ(V) + d);
  //    end;
  //    Polygon2(Path);

  //MoveTo(Path[0].X, Path[0].Y, 0);
  //for i := 1 to High(Path) do
  //  LineTo1(Path[i].X, Path[i].Y, 0);
  //LineTo1(Path[0].X, Path[0].Y, 0);


  //  end else begin
      FBitmap.Canvas.Pen.Color := FBitmap.Canvas.Brush.Color;
  //Bitmap.Canvas.Pen.Color := 0;
  //    PenWidth(FPenWidth);
//  TUtils.Log('3 ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));
      PenWidth(1);
      SetLength(Path, Face.GetVertexCount);
      for i := 0 to Face.GetVertexCount - 1 do
        Path[i] := GetPers(Face.GetVertex(i) + ExplodeDelta);
      Polygon2(Path);
  end;

//  TUtils.Log('4 ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));
    FBitmap.Canvas.Pen.Color := GetEdgeColor;
    PenWidth(FPenWidth);
//    TUtils.Log('5 ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));
    for i := 0 to Face.GetVertexCount - 1 do begin
      if Face.IsEdgeVisible(i) then begin
//        TUtils.Log('6 ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));
        Line3d(Face.GetVertex(i), Face.GetVertex(i + 1));
//        TUtils.Log('7 ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));

//MoveTo(getPersX(Face.Vertices[i]), getPersY(Face.Vertices[i]), 0);
//if i < Face.Vertices.Count - 1 then
//  LineTo1(getPersX(Face.Vertices[i + 1]), getPersY(Face.Vertices[i + 1]), 0)
//else
//  LineTo1(getPersX(Face.Vertices[0]), getPersY(Face.Vertices[0]), 0);

      end;
    end;
//  end;
//TUtils.Log('10 ' + IntToStr(FBitMap.Canvas.Pixels[10, 10]));

  if Face.TextureId >= 0 then
    DrawVectorPicture(Face);
end; // PaintFace

procedure TZBufferGraphics.DrawVectorPicture(Face: TFace);
var
  i, j: integer;
  vNorm, vCenter, v0, vx, vy, v: TVector;
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
  FBitmap.Canvas.Brush.Color := FBitmap.Canvas.Pen.Color;

  vCenter := [0, 0, 0];
  for i := 0 to Face.GetVertexCount - 1 do begin
    v := Face.GetVertex(i);
    vCenter := vCenter + v;
  end;
  vCenter := vCenter / Face.GetVertexCount;

  v0 := Face.GetVertex(0);

  vNorm := Face.GetNorm;

  vx := v0 - vCenter;
  vx := TMatrix.VectorMul(TMatrix.GetRotateMatrix(vNorm, Pi / 2), vx);

  vy := vCenter - vx;
  vx := vx + vCenter - v0;
  vy := vy - v0;

//  DrawPolygon(1 / 350, [148,119, 125,61, 59,61, 59,77, 111,77, 134,128, 125,139, 106,113, 35,137, 40,153, 101,132, 117,154, 114,170, 90,156, 28,192, 36,206, 91,174, 116,188, 123,203, 81,203, 52,285, 68,291, 94,219, 136,219, 150,227, 175,213, 192,211, 204,197, 192,201, 175,203, 156,201, 144,197, 156,211, 175,213, 150,227, 164,231, 184,231, 198,227, 212,219, 254,219, 280,291, 296,285, 267,203, 225,203, 232,188, 257,174, 312,206, 320,192, 258,156, 234,170, 231,154, 247,132, 308,153, 313,137, 242,113, 223,139, 214,128, 237,77, 289,77, 289,61, 223,61, 200,119, 192,116, 180,114, 180,138, 191,136, 201,140, 207,148, 207,165, 203,173, 194,176, 185,176, 178,174, 174,168, 170,174, 163,176, 154,176, 145,173, 141,165, 141,148, 147,140, 157,136, 168,138, 174,144, 180,138, 180,114, 168,114, 156,116]);
  for i := 0 to VectorImage.VectorCommands.Count - 1 do begin
    if VectorImage.VectorCommands[i] is TVectorCommandPenColor then
      PenColor(TVectorCommandPenColor(VectorImage.VectorCommands[i]).PenColor)
    else if VectorImage.VectorCommands[i] is TVectorCommandBrushColor then
      BrushColor(TVectorCommandBrushColor(VectorImage.VectorCommands[i]).BrushColor)
    else if VectorImage.VectorCommands[i] is TVectorCommandPolygon then begin
      Points := TVectorCommandPolygon(VectorImage.VectorCommands[i]).Points;
      SetLength(Path, Points.Count);
      for j := 0 to Points.Count - 1 do
        Path[j] := vx * (Points[j].x / VectorImage.Width) + vy * (Points[j].y / VectorImage.Height) + v0;
      Polygon3d(Path);
    end;
  end;
end; // DrawVectorPicture

procedure TZBufferGraphics.Resize(Width, Height: integer);
begin
  inherited;

  SetLength(ScanRows, Height);

  // allocate memory for backbuffers
//  ReallocMem(Rows, hh);
//  ReallocMem(FrameBuffer, Width * Height * 4);
  ReallocMem(DepthBuffer, Width * Height * 4);

  // set backbuffer size and pixel format
  ZeroMemory(@BitmapInfo, sizeof(BitmapInfo));
  BitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfo);
  BitmapInfo.bmiHeader.biWidth := Width;
  BitmapInfo.bmiHeader.biHeight := Height;
//  BitmapInfo.bmiHeader.biBitCount := 32;
  BitmapInfo.bmiHeader.biBitCount := 24;
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biCompression := BI_RGB;
  BitmapInfo.bmiHeader.biSizeImage := Width * Height * 4;
end; // Resize

function TZBufferGraphics.LoadTexture(fname: string): integer;
var
  {i, j,} k: integer;
  Picture: TPicture;
//  sl: pRGBArray;
  bmp: TBitmap;
begin
  Picture := TPicture.Create;
  Picture.LoadFromFile(fname);
  bmp := TBitmap.Create;
  bmp.Width := Picture.Width;
  bmp.Height := Picture.Height;
//  bmp.PixelFormat := pf32bit;
  bmp.PixelFormat := pf24bit;
  bmp.Canvas.Draw(0, 0, Picture.Graphic);
  Picture.Free;

  k := Length(Textures);
  SetLength(Textures, k + 1);
  ReallocMem(Textures[k].FrameBuffer, bmp.Width * bmp.Height * 4);
  Textures[k].width := bmp.Width;
  Textures[k].height := bmp.Height;

//  ptr_texture_pixel := Pointer(Textures[k].FrameBuffer);
//
//  for j := 0 to bmp.Height - 1 do begin
//    sl := bmp.ScanLine[j];
//    for i := 0 to bmp.Width - 1 do begin
//      ptr_texture_pixel^ := Cardinal(sl[i]);
//      inc(ptr_texture_pixel, 1);
//    end;
//  end;

  result := k;
end; // LoadTexture

procedure TZBufferGraphics.PaintRay(const VBase, V: TVector);
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

  with v0 do
    FBitmap.Canvas.MoveTo(Round(X), Round(Y));
  with vn do
    FBitmap.Canvas.LineTo(Round(X), Round(Y));
end; // PaintPlane

initialization
  //ZBufferGraphics := TZBufferGraphics.Create;
//  ZBufferGraphics.LoadTexture('Texture.jpg');
//  EZLine := TEZLine.Create;
finalization
  //ZBufferGraphics.Free;
//  EZLine.Free;


{  Picture := TPicture.Create;
  Picture.LoadFromFile('RubiksYellowSticker.gif');
  Texture := TBitmap.Create;
  Texture.Width := Picture.Width;
  Texture.Height := Picture.Height;
  Texture.PixelFormat := pf32bit;
  Texture.Canvas.Draw(0, 0, Picture.Graphic);
  Picture.Free;
//    pRGBArray  =  ^TRGBArray;
//    TRGBArray  =  ARRAY[0..MaxPixelCount-1] of TRGBTriple;
//  ptr_screen_pixel: ^Cardinal;
//  ptr_screen_depth: ^Cardinal;
//  ptr_texture_pixel: ^Cardinal;

//    Texture: TBitmap;


//  if AntiAlias then begin
//    EZLine.LineColor := FBitmap.Canvas.Pen.Color;
//    EZLine.LineWidth := FBitmap.Canvas.Pen.Width;
//    SetLength(Arr, High(Path) * 2 + 4);
//    for i := 0 to High(Path) do begin
//      Arr[2 * i] := Path[i].X;
//      Arr[2 * i + 1] := Path[i].Y;
//    end;
//    Arr[High(Path) * 2 + 2] := Arr[0];
//    Arr[High(Path) * 2 + 3] := Arr[1];
//    EZLine.PolyLine(Arr, FBitmap);
//    SetLength(Arr, 0);
//  end;
//  nd; // Polygon


//    PolygonArray[i].LZ := (1 shl wide2) div (PolygonArray[i].LZ);
//    PolygonArray[i].TextureX := Verts[i].ScreenX div 100 + 10;
//    PolygonArray[i].TextureY := Verts[i].ScreenY div 100 + 10;
//    PolygonArray[i].LTX := PolygonArray[i].LTX * PolygonArray[i].LZ;
//    PolygonArray[i].LTY := PolygonArray[i].LTY * PolygonArray[i].LZ;

{  if AntiAlias then begin
    EZLine.LineColor := FBitmap.Canvas.Pen.Color;
    EZLine.LineWidth := FBitmap.Canvas.Pen.Width;
    SetLength(Arr, High(Path) * 2 + 4);
    for i := 0 to High(Path) do begin
      Arr[2 * i] := Path[i].X;
      Arr[2 * i + 1] := Path[i].Y;
    end;
    Arr[High(Path) * 2 + 2] := Arr[0];
    Arr[High(Path) * 2 + 3] := Arr[1];
    EZLine.PolyLine(Arr, FBitmap);
    SetLength(Arr, 0);
  end;}

end.

