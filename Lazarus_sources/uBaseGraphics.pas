{************************************************************}
{                                                            }
{  Unit uBaseGraphics                                        }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uBaseGraphics;

interface

uses uFace, uVector, uPuzzle, uPlane, uPart, uMatrix,
     Graphics, Classes;

type
  TBaseGraphics = class
  private
    procedure SetLighting(Lighting: integer);
    function GetLighting: integer;
  protected
    FWidth: integer;
    FHeight: integer;
    FLighting: integer;
    FBitmap: TBitmap;
    FPenWidth: integer;
    FExplode: integer; // 10..20
    FPuzzle: TPuzzle;
    FPartsFrom, FPartsTo: integer;

    function IsFaceVisible(Face: TFace; ExplodeDelta: TVector): boolean;

    function GetBodyColor: TColor;
    function GetEdgeColor: TColor;
    function GetFaceColor(Face: TFace): TColor;
    function GetPictureColor(Face: TFace): TColor;

  public
    d: extended;
    Zoom: extended;
    BodyColor: TColor;
    EdgeColor: TColor;
    BackgroundColor: TColor;
//    Textures: TList<TTexture>;

    constructor Create;
    procedure Clear; virtual;
    destructor Destroy; override;
    procedure LoadIni; virtual;
    procedure SaveIni; virtual;

    procedure SetPuzzle(Puzzle: TPuzzle);
    procedure SaveBitmapAs(FileName: String);
    procedure DrawBackground; virtual;
    procedure Resize(Width, Height: integer); virtual;
    procedure SetBitmap(Bitmap: TBitmap); virtual;
    procedure PaintScene(Puzzle: TPuzzle); virtual;
    procedure PaintPart(Part: TPart; Flags: integer); virtual; abstract;
    procedure Select(Puzzle: TPuzzle; mx, my: integer); virtual; abstract;
    procedure PenColor(Color: TColor);
    procedure BrushColor(Color: TColor);
    procedure Polygon(Path: array of TVector); virtual; abstract;
    procedure Polygon3d(Path: array of TVector); virtual; abstract;
    function IsPlaneVisible(const Plane: TPlane): Boolean;
    procedure SetPenWidth(PenWidth: integer);
    procedure SetExplode(Explode: integer);
    procedure SetPartsFromTo(iFrom, iTo: integer);

    // помещает в MouseOverFace, MouseOverPart, MouseOverAxis, MouseOverAxisNo
   // то, что находится под курсором (mx, my)
    procedure GetMouseParts(Puzzle: TPuzzle; mx, my: integer); virtual; abstract;

    property Lighting: integer read GetLighting write SetLighting;
    property PenWidth: integer read FPenWidth write SetPenWidth;
    property BitMap: TBitmap read FBitmap;

    function GetPers(const V: TVector): TVector;
    function GetIntersectPoint(Face: TFace; xs, ys: extended): TVector; virtual;
//    function ScreenToPlane(const x, y: integer; const Plane: TPlane): TVector;
    procedure TransLayers(mat: T4x4Matrix; AxisNo: integer; Layers: array of Boolean); virtual;
    procedure ReCalc; virtual;
    procedure OnAfterPuzzleLoad; virtual;
    procedure OnPuzzleTrans(mat: T4x4Matrix); virtual;
    procedure OnBeforePuzzleTurn(AxisNo: integer); virtual;

//    procedure SetExtraParameter(Parameter: Boolean); virtual;

  end;

const
  cWhite = $FFFFFF;
  cBlack = 0;
  cGray = $808080;

  DEFAULT_ZOOM = 240;

//const RegKey = '\Software\pCubes';

implementation

uses uUtils,
     IniFiles, {Jpeg, PngImage, GifImg,} SysUtils, Math;

constructor TBaseGraphics.Create;
begin
  d := 1;
  FPenWidth := 1;
  FExplode := 1;
  FPartsFrom := -1;
  FPartsTo := -1;
//  Textures := TList<TTexture>.Create;
end;

procedure TBaseGraphics.Clear;
begin

end;

destructor TBaseGraphics.Destroy;
//var
//  i: Integer;
begin
//  Clear;
//  for i := 0 to Textures.Count - 1 do
//    Textures[i].Free;
//  Textures.Free;

  inherited;
end;

procedure TBaseGraphics.DrawBackground;
begin
  with FBitmap.Canvas do begin
    Brush.Color := BackgroundColor;
    FillRect(ClipRect);
  end;
end; // DrawBackground

procedure TBaseGraphics.LoadIni;
var Ini: TIniFile;
//    Reg: TRegistry;
begin //LoadIni
  try
    Ini := TIniFile.Create(TSettings.AppConfigDir + ProgramName + '.ini');
    try
      FLighting := Ini.ReadInteger('Graphics', 'Lighting', 5);
      BodyColor := TUtils.StrToColor(Ini.ReadString('Graphics', 'BodyColor', '808080'));
      EdgeColor := TUtils.StrToColor(Ini.ReadString('Graphics', 'EdgeColor', '000000'));
      BackgroundColor := TUtils.StrToColor(Ini.ReadString('Graphics', 'BackgroundColor', '408080'));
      FPenWidth := Ini.ReadInteger('Graphics', 'PenWidth', 1);
      Zoom := Max(Ini.ReadFloat('Graphics', 'Zoom', Zoom), 1);
    finally
      Ini.Free;
    end;
  except
    Zoom := 240;
    FLighting := 5;
    BodyColor := TUtils.StrToColor('808080');
    EdgeColor := 0;
    BackgroundColor := TUtils.StrToColor('408080');
    FPenWidth := 1;
  end;

//  Reg := TRegistry.Create;
//  try
//    Reg.RootKey := HKEY_CURRENT_USER;
//    if Reg.OpenKey(RegKey + '\Graphics', False) then begin
//
//      if Reg.ValueExists('Lighting') then
//        FLighting := Reg.ReadInteger('Lighting');
//      if Reg.ValueExists('BackgroungColor') then
//        CString := Reg.ReadString('BackgroungColor');
//    end;
//  finally
//    Reg.CloseKey;
//    Reg.Free;
//  end;

end; // LoadIni

procedure TBaseGraphics.SaveIni;
var Ini: TIniFile;
//    Reg: TRegistry;
begin //SaveIni
  Ini := TIniFile.Create(TSettings.AppConfigDir + ProgramName + '.ini');
  try
    Ini.WriteInteger('Graphics', 'Lighting',        FLighting);
    Ini.WriteString ('Graphics', 'BodyColor',       TUtils.ColorToStr(BodyColor));
    Ini.WriteString ('Graphics', 'EdgeColor',       TUtils.ColorToStr(EdgeColor));
    Ini.WriteString ('Graphics', 'BackgroundColor', TUtils.ColorToStr(BackgroundColor));
    Ini.WriteInteger('Graphics', 'PenWidth',        FPenWidth);
    Ini.WriteInteger('Graphics', 'Zoom',            Round(Zoom)); // extended ?
  finally
    Ini.Free;
  end;

//  Reg := TRegistry.Create;
//  try
//    Reg.RootKey := HKEY_CURRENT_USER;
//    if Reg.OpenKey(RegKey + '\Graphics', True) then begin
//
//      Reg.WriteInteger('Lighting', FLighting);
//      Reg.WriteString('BackgroungColor', ColorToStr(BackgroungColor));
//    end;
//  finally
//    Reg.CloseKey;
//    Reg.Free;
//  end;
end; //SaveIni

procedure TBaseGraphics.SetPuzzle(Puzzle: TPuzzle);
begin
  Clear;
  FPuzzle := Puzzle;
end;

procedure TBaseGraphics.SaveBitmapAs(FileName: String);
var
  ext: String;
//  png: TPngImage;
  jpg: TJPEGImage;
  gif: TGIFImage;
begin
  ext := LowerCase(ExtractFileExt(FileName));

  if ext = '.bmp' then
    FBitmap.SaveToFile(FileName)
  else if ext = '.gif' then begin
    gif := TGIFImage.Create;
    gif.Assign(FBitMap);
//    gif.Add(FBitMap);

    //Удаляем лишние цвета из палитры
//    gif.OptimizeColorMap;
//    gif.Pack;
    gif.SaveToFile(FileName);
    FreeAndNil(gif);
  end else if ext = '.jpg' then begin
    jpg := TJPEGImage.Create;
    jpg.Assign(FBitMap);
    jpg.SaveToFile(FileName);
    FreeAndNil(jpg);
  end else if ext = '.png' then begin
    //png := TPngImage.Create;
    //png.Assign(FBitMap);
    //png.SaveToFile(FileName);
    //FreeAndNil(png);
  end;
end;

function TBaseGraphics.IsFaceVisible(Face: TFace; ExplodeDelta: TVector): boolean;
var V: TVector;
begin
  V := Face.GetVertex(0) + ExplodeDelta;
  V.Y := V.Y + d;
  Result := Face.GetNorm ** V < 0;
end;

function TBaseGraphics.IsPlaneVisible(const Plane: TPlane): Boolean;
var N: TVector;
begin
  N := Plane.NormVect * Plane.NormDistance;
  N.Y := N.Y + d;
  Result := Plane.NormVect ** N < 0;
end; // IsVisible

procedure TBaseGraphics.SetPenWidth(PenWidth: integer);
begin
  FPenWidth := PenWidth;
end;

procedure TBaseGraphics.SetExplode(Explode: integer);
begin
  FExplode := Explode;
end; // SetExplode

procedure TBaseGraphics.SetPartsFromTo(iFrom, iTo: integer);
begin
  FPartsFrom := iFrom;
  FPartsTo := iTo;
end; // SetFromToParts

procedure TBaseGraphics.TransLayers(mat: T4x4Matrix; AxisNo: integer; Layers: array of Boolean);
begin

end;

procedure TBaseGraphics.ReCalc;
begin

end;

procedure TBaseGraphics.OnAfterPuzzleLoad;
begin

end;

procedure TBaseGraphics.OnPuzzleTrans(mat: T4x4Matrix);
begin

end;

procedure TBaseGraphics.OnBeforePuzzleTurn(AxisNo: integer);
begin

end;

procedure TBaseGraphics.Resize(Width, Height: integer);
begin
  // get window size
  FWidth := Width;
  FHeight := Height;
end;

procedure TBaseGraphics.SetBitmap(Bitmap: TBitmap);
begin
  FBitmap := Bitmap;
end;

//procedure TBaseGraphics.SetExtraParameter(Parameter: Boolean);
//begin
//
//end;
//
function TBaseGraphics.GetBodyColor: TColor;
begin
  Result := BodyColor;
end;

function TBaseGraphics.GetEdgeColor: TColor;
begin
  Result := EdgeColor
end;

function TBaseGraphics.GetFaceColor(Face: TFace): TColor;
begin
  if Face.IsColored then
    Result := Face.Color
  else
    Result := GetBodyColor;
end;

function TBaseGraphics.GetPictureColor(Face: TFace): TColor;
begin
  if GetFaceColor(Face) = cBlack then
    Result := cWhite
  else
    Result := cBlack;
end;

function TBaseGraphics.GetLighting: integer;
begin
  Result := FLighting;
end;

procedure TBaseGraphics.SetLighting(Lighting: integer);
begin
  if Lighting > 10 then
    FLighting := 10
  else if Lighting < 0 then
    FLighting := 0
  else
    FLighting := Lighting;
end;

procedure TBaseGraphics.PaintScene(Puzzle: TPuzzle);
begin
  DrawBackground;
end;

procedure TBaseGraphics.PenColor(Color: TColor);
begin
  FBitmap.Canvas.Pen.Color := Color;
end;

procedure TBaseGraphics.BrushColor(Color: TColor);
begin
  FBitmap.Canvas.Brush.Color := Color;
end;

// ====================================================================

function TBaseGraphics.GetPers(const V: TVector): TVector;
begin
  Result.X :=  V.X * d * Zoom  / (V.Y + d) + FWidth / 2;
  Result.Y := -V.Z * d * Zoom  / (V.Y + d) + FHeight / 2;
  Result.Z :=  V.Y + d;
  Result := Result;
end; // GetPers

function TBaseGraphics.GetIntersectPoint(Face: TFace; xs, ys: extended): TVector;
var V0, VL, VN: TVector;
begin
  // line
  V0 := [xs - FWidth / 2, 0, -(ys - FHeight / 2)];
  V0 := V0 / Zoom;
  VL := [xs - FWidth / 2, d * Zoom, -ys + FHeight / 2];

  // plane
  VN := Face.GetNorm;

  Result := Face.GetVertex(0);
  Result := Result - V0;

  Result := VL * Result ** VN / (VL ** VN) + V0;
end; // GetIntersectPoint

//function TBaseGraphics.ScreenToPlane(const x, y: integer; const Plane: TPlane): TVector;
//begin
//  X :=  V.X * d * Zoom  / (V.Y + d) + FWidth / 2;
//  Y := -V.Z * d * Zoom  / (V.Y + d) + FHeight / 2;
//  Z :=  V.Y + d;
//
//  x y z принадлежат Plane
//
//  Y := Screen.Z - d;
//  (Screen.X - FWidth / 2) * Y + (Screen.X - FWidth / 2) * d :=  X * d * Zoom;
//  (Screen.Y - FHeight / 2) * Y + (Screen.Y - FHeight / 2) * d := -Z * d * Zoom;
//
//  (Screen.X - FWidth / 2) / d * Y + (Screen.X - FWidth / 2) :=  X * Zoom;
//  (Screen.Y - FHeight / 2) / d * Y + (Screen.Y - FHeight / 2) := -Z * Zoom;
//
//  X := ((Screen.X - FWidth / 2) / d * Y + (Screen.X - FWidth / 2)) / Zoom;
//  Y
//  Z := -((Screen.Y - FHeight / 2) / d * Y + (Screen.Y - FHeight / 2)) / Zoom;

//  [(Screen.X - FWidth / 2) / Zoom, 0, (Screen.Y - FHeight / 2) / Zoom]

//  [(Screen.X - FWidth / 2) / d, 1, -(Screen.Y - FHeight / 2) / d];




//  L0 := [(Screen.X - FWidth / 2) / Zoom, 0, (Screen.Y - FHeight / 2) / Zoom];
//  L1 := L0 + [(Screen.X - FWidth / 2), d, -(Screen.Y - FHeight / 2)];
//
//  (Vec - Plane.NormVect * Plane.NormDistance) * Plane.NormVect = 0
//  X * nx + Y * ny + Z * nz = nx * nd
//
//  (x - nx * nd) * nx   +  (y - ny * nd) * ny   +   (z - nz * nd) * nz = 0

//end; // ScreenToPlane

end.
