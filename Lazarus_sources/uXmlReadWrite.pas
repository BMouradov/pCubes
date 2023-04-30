{************************************************************}
{                                                            }
{  Unit uXmlReadWrite                                        }
{  2018-2023                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uXmlReadWrite;

{$mode Delphi}

interface

uses Types,
     uPuzzle, uVector, uPlane, uLine, uLayer, uAxis, uFace, uPart, uMacro, uTexture, uTurn,
     DOM, Graphics, Generics.Collections;

const
  XML_VERSION: integer = 2;
  PUZZLES_ARCHIVE_PATH = 'Puzzles.zip';
  LIBRARY_PATH = 'Library.xml';
  MENU_FILE = 'Menu.xml';

type
  TXmlReadWrite = class
  private
    class var lMacros: TList<TDomNode>;

//    FPuzzle: TPuzzle;
    class function FindMacros(const XmlNode: TDomNode; var MacroNodes: TList<TDomNode>; var RepeatN: integer): Boolean;
    class procedure AddCylinder(Part: TPart; const Base, Top: TVector; Radius: extended; Color, BaseColor, TopColor: TColor);
    class procedure AddCircle(Part: TPart; const Base, Center: TVector; Radius: extended; Color: TColor);
    class procedure Transfer(Puzzle: TPuzzle; XmlNode: TDomNode); static;
    class procedure Turn(Puzzle: TPuzzle; XmlNode: TDomNode); static;
    class procedure DecodeLayers(const LayersString: String; var Layers: array of Boolean);
    class function LoadArrayFromString(const Names: String): TArray<extended>;
    class function PuzzleLoadFromXmlCommon(Puzzle: TPuzzle; XmlDoc: TXmlDocument; const FileName: String): Boolean;
  public
//    property Puzzle: TPuzzle write FPuzzle;

    constructor Create;

    class function GetAttrOrParent(XmlNode: TDomNode; const aName, aDefault: DomString): DomString;

    class function GetVectorFromString(S: String): TVector;
    class procedure GetVectorsFromString(S: String; Vectors: TList<TVector>);

    class function VectorLoadFromXml(var Vector: TVector; XmlNode: TDomNode): Boolean;
    class function VectorSaveToXml(Vector: TVector; XmlNode: TDomElement): Boolean;
    class function VectorsLoadFromXml(Vectors: TList<TVector>; XmlNode: TDomNode): Boolean;
    class procedure VectorsSaveToXml(Vectors: TList<TVector>; XmlDoc: TXmlDocument; XmlNode: TDomNode);

    class function LineLoadFromXml(var Line: TLine; XmlNode: TDomNode): Boolean;
    class function LineSaveToXml(const Line: TLine; XmlDoc: TXmlDocument; XmlNode: TDomNode): Boolean;

//    function LoadHeaderFromXmlFile(Puzzle: TPuzzle; const FileName: String): Boolean;
//    class function LoadVectorsFromXml(var Vectors: TList<TVector>; XmlNode: TDomNode): Boolean;
//    class function PlanesLoadFromXml(Planes: TPlanes; XmlNode: TDomNode): Boolean;
//    class procedure PlanesSaveToXml(Planes: TPlanes; XmlNode: TDomElement);
    class function PlaneLoadFromXml(var Plane: TPlane; XmlNode: TDomNode): Boolean;
    class procedure PlaneSaveToXml(Plane: TPlane; XmlDoc: TXmlDocument; XmlNode: TDomElement);

//    class function LayerLoadFromXml(Layer: TLayer; XmlNode: TDomNode): Boolean;
//    class procedure LayerSaveToXml(Layer: TLayer; XmlNode: TDomElement);
//    function LayersLoadFromXml(XmlNode: TDomNode): Boolean;
//    procedure LayersSaveToXml(XmlNode: TDomElement);
    class function AxisLoadFromXml(Axis: TAxis; XmlNode: TDomNode): Boolean;
    class procedure AxisSaveToXml(Axis: TAxis; XmlDoc: TXmlDocument; XmlNode: TDomNode);
    class procedure FillAxis(Axis: TAxis; const PlaneDistances, TurningAngles, AvailableAngles, JumbleAngle, FixedLayers, MinAngles, MaxAngles: String; TurnAxesWithLayer, TurnAxesWithPartNo: integer);

    class function AxesLoadFromXml(Axes: TList<TAxis>; XmlNode: TDomNode): Boolean;
    class procedure AxesSaveToXml(Axes: TList<TAxis>; XmlDoc: TXmlDocument; XmlNode: TDomNode);

    class function FaceLoadFromXml(Face: TFace; XmlNode: TDomNode): Boolean;
    class procedure FaceSaveToXml(Face: TFace; XmlNode: TDomNode);
    class function FacesLoadFromXml(Faces: TList<TFace>; XmlNode: TDomNode; Points: TList<TVector>): Boolean;
    class procedure FacesSaveToXml(Faces: TList<TFace>; XmlDoc: TXmlDocument; XmlNode: TDomNode; Points: TList<TVector>);

    class function PartLoadFromXml(Part: TPart; XmlNode: TDomNode): Boolean;
    class function PartLoadCylinder(Part: TPart; XmlNode: TDomNode): Boolean;
    class function PartLoadCircle(Part: TPart; XmlNode: TDomNode): Boolean;
    class procedure PartSaveToXml(Part: TPart; XmlDoc: TXmlDocument; XmlNode: TDomNode);

    class function PuzzleCheckVersionFromXml(Puzzle: TPuzzle; XmlNode: TDomNode): Boolean;
    class function PuzzleLoadHeaderFromXml(Puzzle: TPuzzle; XmlNode: TDomNode): Boolean;
    class function PuzzleLoadHeaderFromXmlFile(Puzzle: TPuzzle; const FileName: String): Boolean;
    class function PuzzleLoadFromXml(Puzzle: TPuzzle; XmlNode: TDomNode): Boolean;
    class function PuzzleLoadFromXmlFile(Puzzle: TPuzzle; const FileName: String): Boolean;
    class function PuzzleLoadFromCompressedXmlFile(Puzzle: TPuzzle; const FileName: String): Boolean;
    class function PuzzleExecMacroFromXmlFile(Puzzle: TPuzzle; const FileName: String; MacroString: String): Boolean;
    class procedure PuzzleSaveHeaderToXml(Puzzle: TPuzzle; XmlDoc: TXmlDocument; XmlNode: TDomNode);
    class procedure PuzzleSaveToXml(Puzzle: TPuzzle; XmlDoc: TXmlDocument; XmlRoot: TDomNode); virtual;
    class procedure PuzzleSaveToXmlFile(Puzzle: TPuzzle; const FileName: String);

//    class function MacroTurnLoadFromXml(MacroTurn: TMacroTurn; XmlNode: TDomNode): Boolean;
//    class procedure MacroTurnSaveToXml(MacroTurn: TMacroTurn; XmlNode: TDomNode);
//    class function MacroTurnsLoadFromXml(MacroTurns: TList<TMacroTurn>; XmlNode: TDomNode): Boolean;
//    class procedure MacroTurnsSaveToXml(MacroTurns: TList<TMacroTurn>; XmlNode: TDomNode);
    class function MacroLoadFromXml(Macro: TMacro; XmlNode: TDomNode): Boolean;
    class procedure MacroSaveToXml(Macro: TMacro; XmlNode: TDomNode);
    class function MacrosLoadFromXml(Macros: TList<TMacro>; XmlNode: TDomNode): Boolean;
    class procedure MacrosSaveToXml(Macros: TList<TMacro>; XmlDoc: TXmlDocument; XmlNode: TDomNode);

    class function TextureLoadFromXml(Texture: TTexture; XmlNode: TDomNode): Boolean;

    class function TurnLoadFromXml(Puzzle: TPuzzle; Turn: TTurn; XmlNode: TDomNode): Boolean;
    class procedure TurnSaveToXml(Turn: TTurn; XmlNode: TDomElement);
    class function TurnsLoadFromXml(Puzzle: TPuzzle; Turns: TList<TTurn>; XmlNode: TDomNode): Boolean;
    class procedure TurnsSaveToXml(Turns: TList<TTurn>; XmlDoc: TXmlDocument; XmlNode: TDomNode);
  end;

implementation
uses
  uScript, uUtils, uXmlUtils, uMatrix, uSolvedState, uPuzzleUtils, uSelection, uGeometryUtils,
  Variants, SysUtils, StrUtils, Classes, Math,
{Unzip,} XmlRead, XMLWrite{, ZipUtils};

const
  CIRCLE_SECTORS = 64;

constructor TXmlReadWrite.Create;
begin
  inherited;
//  FPuzzle := nil;
end;

class function TXmlReadWrite.GetAttrOrParent(XmlNode: TDomNode; const aName, aDefault: DomString): DomString;
var i: integer;
begin
  Result := aDefault;

  i := GetAttrIndex(XmlNode, aName);
  if i >= 0 then
    Exit(XmlNode.Attributes[i].NodeValue);

  XmlNode := XmlNode.ParentNode;

  if XmlNode = nil then
    Exit;

  i := GetAttrIndex(XmlNode, aName);
  if i >= 0 then
    Exit(XmlNode.Attributes[i].NodeValue);
end;

// ========================= Vector ===================================

class function TXmlReadWrite.LoadArrayFromString(const Names: String): TArray<extended>;
var
  i: integer;
  EList: TList<Extended>;
  Values: TStringDynArray;
  V, V1: TVar;
  S, PS: String;
begin
  EList := TList<Extended>.Create;
  try
    Values := SplitString(Names, ';');
    for S in Values do begin
      PS := Trim(S);
      // check if it's an array
      if Calc.VarByName(PS + '[0]', V) and (not Calc.VarByName(PS, V1)) and (VarType(V.Value) = varDouble) then begin
        i := 0;
        repeat
          EList.Add(V.Value);
          //Axis.AddPlane(V.Value);
          Inc(i);
        until not (Calc.VarByName(PS + '[' + IntToStr(i) + ']', V) and (VarType(V.Value) = varDouble));
      end else
        EList.Add(CalcExpression(PS));
    end;

    Result := EList.ToArray;
  finally
    EList.Free;
  end;
end;

class function TXmlReadWrite.VectorLoadFromXml(var Vector: TVector; XmlNode: TDomNode): Boolean;
begin
  Result := False;

  if XmlNode = nil then
    exit;

  Vector.X := ReadDoubleExpressionFromXml(XmlNode, 'X');
  Vector.Y := ReadDoubleExpressionFromXml(XmlNode, 'Y');
  Vector.Z := ReadDoubleExpressionFromXml(XmlNode, 'Z');
  Result := True;
end;

class function TXmlReadWrite.VectorSaveToXml(Vector: TVector; XmlNode: TDomElement): Boolean;
begin
  XmlNode.AttribStrings['X'] := FloatToStr(Vector.X);
  XmlNode.AttribStrings['Y'] := FloatToStr(Vector.Y);
  XmlNode.AttribStrings['Z'] := FloatToStr(Vector.Z);
  Result := True;
end;

class function TXmlReadWrite.GetVectorFromString(S: String): TVector;
begin
  Result.X := CalcExpression(Trim(TUtils.Parse(S, ';')));
  Result.Y := CalcExpression(Trim(TUtils.Parse(S, ';')));
  Result.Z := CalcExpression(Trim(TUtils.Parse(S, ';')));
end; // GetVectorFromString

class procedure TXmlReadWrite.GetVectorsFromString(S: String; Vectors: TList<TVector>);
var
  isArray: array[0..2] of Boolean;
  ArrayName: array[0..2] of string;
  vec: array[0..2] of extended;

  function GetArrayValue(i, Dim: integer): Boolean;
  var V: TVar;
  begin
    if not isArray[Dim] then
      Exit(True);

    Result := Calc.VarByName(ArrayName[Dim] + '[' + IntToStr(i) + ']', V);
    if Result then
      vec[Dim] := V.Value
    else
      vec[Dim] := 0;
  end; // GetArrayValue

  procedure InitDim(Dim: integer);
  begin
    isArray[Dim] := True;
    ArrayName[Dim] := UpperCase(Trim(TUtils.Parse(S, ';')));
    isArray[Dim] := GetArrayValue(0, Dim);
    if not isArray[Dim] then
      Vec[Dim] := CalcExpression(ArrayName[Dim]);
  end; // InitDim

var
  i: integer;
begin
  if S = '' then
    exit;

  InitDim(0);
  InitDim(1);
  InitDim(2);
  Vectors.Add(Vector(Vec[0], Vec[1], Vec[2]));

  if isArray[0] or isArray[1] or isArray[2] then
    for i := 1 to MaxInt do begin
      if GetArrayValue(i, 0) and
         GetArrayValue(i, 1) and
         GetArrayValue(i, 2) then
        Vectors.Add(Vector(Vec[0], Vec[1], Vec[2]))
      else
        break;
    end;
end; // GetVectorsFromString

class function TXmlReadWrite.VectorsLoadFromXml(Vectors: TList<TVector>; XmlNode: TDomNode): Boolean;

  function ExecMacro(XmlNode: TDomNode): Boolean;
  var
    i, n: integer;
    MacroNodes: TList<TDomNode>;
    MacroNode: TDomNode;
  begin
    Result := False;
    MacroNodes := TList<TDomNode>.Create;
    try
      if FindMacros(XmlNode, MacroNodes, n) then
        for i := 0 to n - 1 do
          for MacroNode in MacroNodes do
            if not VectorsLoadFromXml(Vectors, MacroNode) then
              exit;
      Result := True;
    finally
      MacroNodes.Free;
    end;
  end; // ExecMacro

var
  i: integer;
  XmlSubNode: TDomNode;
  Vector: TVector;
  S: string;
begin // VectorsLoadFromXml
  Result := False;

  if XmlNode = nil then
    exit;

  GetVectorsFromString(ReadStringFromXml(XmlNode, 'Vectors'), Vectors);

  for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
    XmlSubNode := XmlNode.ChildNodes[i];
    S := String(XmlSubNode.NodeName);
    if S = 'Script' then
      ExecuteScript(GetNodeText(XmlSubNode))
    else if S = 'Vector' then begin
      if not VectorLoadFromXml(Vector, XmlSubNode) then
        exit;
      Vectors.Add(Vector);
    end else if S = 'Add' then begin
      S := GetAttrFromXml(XmlSubNode, 'Vector');
      if S <> '' then
        Vectors.Add(GetVectorFromString(S));
    end else if S = 'ExecMacro' then begin
      if not ExecMacro(XmlSubNode) then
        exit;
    end;
  end;

  Result := True;
end; // VectorsLoadFromXml

class procedure TXmlReadWrite.VectorsSaveToXml(Vectors: TList<TVector>; XmlDoc: TXmlDocument; XmlNode: TDomNode);
var
  NewNode: TDomElement;
  Vector: TVector;
begin
//  for i := 0 to Count - 1 do
//    Vectors(i).SaveToXml(XmlNode.AppendElement('Vector'));

  for Vector in Vectors do
    with Vector do begin
//      XmlNode.AppendElement('Add').SetAttr('Vector', FloatToXSTR(X) + '; ' + FloatToXSTR(Y) + '; ' + FloatToXSTR(Z));
      NewNode := XmlDoc.CreateElement('Add');
      NewNode.AttribStrings['Vector'] := FloatToStr(Vector.X) + '; ' + FloatToStr(Vector.Y) + '; ' + FloatToStr(Vector.Z);
      XmlNode.AppendChild(NewNode);
    end;
end;

//class function TXmlReadWrite.PlanesLoadFromXml(Planes: TPlanes; XmlNode: TDomNode): Boolean;
//var
//  i: integer;
//  List: TDomNodeList;
//  Plane: TPlane;
//begin
//  Result := False;
//  List := XmlNode.SelectNodes('Plane');
//
//  for i := 0 to List.Count - 1 do begin
//    Plane := TPlane.Create;
//    if not Plane.LoadFromXml(List.Item[i]) then
//      exit;
//    Planes.Add(Plane);
//  end;
//  Result := True;
//end;
//
//class procedure TXmlReadWrite.PlanesSaveToXml(Planes: TPlanes; XmlNode: TDomElement);
//var i: integer;
//begin
//  for i := 0 to Planes.Count - 1 do
//    Planes[i].SaveToXml(XmlNode.AppendElement('Plane'));
//end;

// ========================= Line ===================================

class function TXmlReadWrite.LineLoadFromXml(var Line: TLine; XmlNode: TDomNode): Boolean;
begin
  if not ReadVectorFromXml(Line.BaseVector, XmlNode, 'BaseVector') then
    Line.BaseVector := [0, 0, 0];

  if not ReadVectorFromXml(Line.NormVector, XmlNode, 'NormVector') then
    Line.NormVector := [1, 0, 0];

  Line.NormVector.Normalize;

  Result := True;
end; // LoadFromXml

class function TXmlReadWrite.LineSaveToXml(const Line: TLine; XmlDoc: TXmlDocument; XmlNode: TDomNode): Boolean;
var NewNode: TDomElement;
begin
  NewNode := XmlDoc.CreateElement('BaseVector');
  Result := VectorSaveToXml(Line.BaseVector, NewNode);
  XmlNode.AppendChild(NewNode);

  NewNode := XmlDoc.CreateElement('NormVector');
  Result := VectorSaveToXml(Line.NormVector, NewNode);
  XmlNode.AppendChild(NewNode);
end; // SaveToXml

// ========================= Plane ===================================

class function TXmlReadWrite.PlaneLoadFromXml(var Plane: TPlane; XmlNode: TDomNode): Boolean;
var BaseVector: TVector;
begin
  if XmlNode = nil then
    Exit(False);
  if not ReadVectorFromXml(BaseVector, XmlNode, 'BaseVector') then
    BaseVector := Vector(0, 0, 0);

  if not ReadVectorFromXml(Plane.NormVect, XmlNode, 'NormVector') then
    Plane.NormVect := Vector(0, 0, 1);

  Plane.NormVect.Normalize;

  Plane.NormDistance := ReadDoubleExpressionFromXml(XmlNode, 'NormDistance') + BaseVector ** (Plane.NormVect);
  Result := True;
end;

class procedure TXmlReadWrite.PlaneSaveToXml(Plane: TPlane; XmlDoc: TXmlDocument; XmlNode: TDomElement);
var NewNode: TDomElement;
begin
  NewNode := XmlDoc.CreateElement('NormVector');
  VectorSaveToXml(Plane.NormVect, NewNode);
  XmlNode.AppendChild(NewNode);

  NewNode.AttribStrings['NormDistance'] := FloatToStr(Plane.NormDistance);
end;

// ========================= Layer ===================================

//class function TXmlReadWrite.LayerLoadFromXml(Layer: TLayer; XmlNode: TDomNode): Boolean;
//begin
//  Result := True;
//end;
//
//class procedure TXmlReadWrite.LayerSaveToXml(Layer: TLayer; XmlNode: TDomElement);
//begin
//end;

// ========================= Axes ===================================

class procedure TXmlReadWrite.FillAxis(Axis: TAxis; const PlaneDistances, TurningAngles, AvailableAngles, JumbleAngle, FixedLayers, MinAngles, MaxAngles: String; TurnAxesWithLayer, TurnAxesWithPartNo: integer);

  procedure AddAvailableAngle(Angle: extended);
  var Layer: TLayer;
  begin
    Angle := TUtils.PrivPi(Angle);

    for Layer in Axis.Layers do
      if not Layer.Fixed then
        Layer.AddAvailableAngle(Angle);
  end; // AddAvailableAngle

  procedure AddJumbleAngles(Angle: extended);
  var
    i: integer;
    TurningAngle: extended;
    Layer: TLayer;
  begin
    for Layer in Axis.Layers do
      if not Layer.Fixed then begin
        TurningAngle := Layer.TurningAngle;

        for i := 0 to Round(2 * Pi / TurningAngle) + 1 do begin // why +1??
          Layer.AddAvailableAngle(TUtils.PrivPi(TurningAngle * i + Angle));
          Layer.AddAvailableAngle(TUtils.PrivPi(TurningAngle * i - Angle));
        end;
      end;
  end; // AddJumbleAngles

var
  i: integer;
  S, SMin, SMax: String;
  Al, D: extended;
begin // FillAxis
  Axis.CreateLayers(LoadArrayFromString(PlaneDistances));

  // !!!!хорошо бы поправить, чтобы Fixed слой не содержал лишних углов (Helicoper)
  S := TurningAngles;
  if Pos(';', S) <> 0 then
    for i := 0 to Axis.Layers.Count - 1 do
      Axis.Layers[i].TurningAngle := CalcExpression(Trim(TUtils.Parse(S, ';')))
  else begin
    Al := CalcExpression(S);
    for i := 0 to Axis.Layers.Count - 1 do
      Axis.Layers[i].TurningAngle := Al;
  end;
  for i := 0 to Axis.Layers.Count - 1 do
    with Axis.Layers[i] do
      if Abs(TurningAngle) < DELTA then
        Fixed := True;

  S := FixedLayers;
  while Trim(S) <> '' do begin
    i := Round(CalcExpression(Trim(TUtils.Parse(S, ';'))));
    if (i >= 0) and (i < Axis.Layers.Count) then
      Axis.Layers[i].Fixed := True
    else
      TUtils.Log('FillAxis: FixedLayers out of bounds: ' + IntToStr(i));
  end;

  S := AvailableAngles;
  while TUtils.Trim(S) <> '' do
    AddAvailableAngle(CalcExpression(TUtils.Trim(TUtils.Parse(S, ';'))));

  D := CalcExpression(JumbleAngle);
  if D <> 0 then
    AddJumbleAngles(D);

  SMin := MinAngles;
  SMax := MaxAngles;
  for i := 0 to Axis.Layers.Count - 1 do begin
    if (SMin <> '') or (SMax <> '') then begin
      S := Trim(TUtils.Parse(SMin, ';'));
      if S <> '' then
        Axis.Layers[i].MinAngle := CalcExpression(S);
      S := Trim(TUtils.Parse(SMax, ';'));
      if S <> '' then
        Axis.Layers[i].MaxAngle := CalcExpression(S);
    end;
  end;

  Axis.TurnAxesWithLayer := TurnAxesWithLayer;
  Axis.TurnAxesWithPartNo := TurnAxesWithPartNo;
end; // FillAxis

class function TXmlReadWrite.AxisLoadFromXml(Axis: TAxis; XmlNode: TDomNode): Boolean;
begin // AxisLoadFromXml
  Result := False;

  if not LineLoadFromXml(Axis.Line, XmlNode) then
    exit;

  FillAxis(Axis,
    string(GetAttrOrParent(XmlNode, 'PlaneDistances', '')),
    ReadStringFromXmlOrParent(XmlNode, 'TurningAngles'),
    ReadStringFromXmlOrParent(XmlNode, 'AvailableAngles'),
    ReadStringFromXmlOrParent(XmlNode, 'JumbleAngle'),
    ReadStringFromXmlOrParent(XmlNode, 'FixedLayers'),
    ReadStringFromXmlOrParent(XmlNode, 'MinAngles'),
    ReadStringFromXmlOrParent(XmlNode, 'MaxAngles'),
    ReadIntExpressionFromXmlOrParent(XmlNode, 'TurnAxesWithLayer', -1),
    ReadIntExpressionFromXmlOrParent(XmlNode, 'TurnAxesWithPartNo', -1)
  );

  Result := True;
end; // AxisLoadFromXml

{$WARN USE_BEFORE_DEF OFF}
class procedure TXmlReadWrite.AxisSaveToXml(Axis: TAxis; XmlDoc: TXmlDocument; XmlNode: TDomNode);
var
  i: integer;
//  SubNode: TDomElement;
  S: String;
begin
  LineSaveToXml(Axis.Line, XmlDoc, XmlNode);

  S := '';
  for i := 0 to Axis.Layers.Count - 2 do begin
    S := S + String(FloatToStr(Axis.Layers[i].DistanceTo));
    if i < Axis.Layers.Count - 2 then
      S := S + ';';
  end;
  TDomElement(XmlNode).AttribStrings['PlaneDistances'] := AnsiString(S);

  S := '';
  for i := 0 to Axis.Layers.Count - 1 do begin
    S := S + FloatToStr(Axis.Layers[i].TurningAngle);
    if i < Axis.Layers.Count - 1 then
      S := S + ';';
  end;
  TDomElement(XmlNode).AttribStrings['TurningAngles'] := AnsiString(S);

  S := '';
  for i := 0 to Axis.Layers.Count - 1 do begin
    if Axis.Layers[i].Fixed then
      S := S + IntToStr(i) + ';';
  end;
  if Length(S) > 0 then begin
    Delete(S, Length(S), 1);
    TDomElement(XmlNode).AttribStrings['FixedLayers'] := AnsiString(S);
  end;

  if Axis.TurnAxesWithLayer >= 0 then
    TDomElement(XmlNode).AttribStrings['TurnAxesWithLayer'] := IntToStr(Axis.TurnAxesWithLayer);

  if Axis.TurnAxesWithPartNo >= 0 then
    TDomElement(XmlNode).AttribStrings['TurnAxesWithPartNo'] := IntToStr(Axis.TurnAxesWithPartNo);
end; // AxisSaveToXml

class function TXmlReadWrite.AxesLoadFromXml(Axes: TList<TAxis>; XmlNode: TDomNode): Boolean;

  function GetValue(Name: string; idx: integer; var D: extended): Boolean;
  var V: TVar;
  begin
    Result := True;

    if Calc.VarByName(Name, V) and (VarType(V.Value) in [varDouble, varInteger]) then
      D := V.Value
    else if Calc.VarByName(Name + '[' + IntToStr(idx) + ']', V) and (VarType(V.Value) in [varDouble, varInteger]) then
      D := V.Value
    else
      Result := False;
  end; // GetValue

var
  i, i1, i2: integer;
//  List: TDomNodeList;
  Axis: TAxis;
  S1, S2, S3, S4, S5, S6, S7: string;
  Vectors: TList<TVector>;
begin // AxesLoadFromXml
  Result := False;

  Vectors := TList<TVector>.Create;
  try
    GetVectorsFromString(string(TDomElement(XmlNode).GetAttribute('NormVectors')), Vectors);
    if Vectors.Count > 0 then begin
      S1 := ReadStringFromXml(XmlNode, 'PlaneDistances');
      S2 := ReadStringFromXml(XmlNode, 'TurningAngles');
      S3 := ReadStringFromXml(XmlNode, 'AvailableAngles');
      S4 := ReadStringFromXml(XmlNode, 'JumbleAngle');
      S5 := ReadStringFromXml(XmlNode, 'FixedLayers');
      S6 := ReadStringFromXml(XmlNode, 'MinAngles');
      S7 := ReadStringFromXml(XmlNode, 'MaxAngles');
      i1 := ReadIntExpressionFromXml(XmlNode, 'TurnAxesWithLayer', -1);
      i2 := ReadIntExpressionFromXml(XmlNode, 'TurnAxesWithPartNo', -1);
    end;

    for i := 0 to Vectors.Count - 1 do begin
      Axis := TAxis.Create;
      Axis.Line.NormVector.X := Vectors[i].X;
      Axis.Line.NormVector.Y := Vectors[i].Y;
      Axis.Line.NormVector.Z := Vectors[i].Z;
      Axis.Line.NormVector.Normalize;
      FillAxis(Axis, S1, S2, S3, S4, S5, S6, S7, i1, i2);
      Axes.Add(Axis);
    end;
  finally
    Vectors.Free;
  end;

  for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
    if XmlNode.ChildNodes[i].CompareName('Axis') = 0 then begin
      Axis := TAxis.Create;
      if not AxisLoadFromXml(Axis, XmlNode.ChildNodes[i]) then
        exit;
      Axes.Add(Axis);
    end;
  end;

  Result := True;
end; // AxesLoadFromXml
{$WARN USE_BEFORE_DEF ON}

class procedure TXmlReadWrite.AxesSaveToXml(Axes: TList<TAxis>; XmlDoc: TXmlDocument; XmlNode: TDomNode);
var NewNode: TDomElement;
    Axis: TAxis;
begin
  for Axis in Axes do begin
    NewNode := XmlDoc.CreateElement('Axis');
    AxisSaveToXml(Axis, XmlDoc, NewNode);
    XmlNode.AppendChild(NewNode);
  end;
end;

// ========================= Face ===================================

class function TXmlReadWrite.FaceLoadFromXml(Face: TFace; XmlNode: TDomNode): Boolean;

  procedure AddIndexesFromString(const S: string);
  var
    Splitted: TArray<String>;
    VIndex: String;
    V: TVar;
  begin
    Splitted := S.Trim.Split([';'], TStringSplitOptions.ExcludeEmpty);
    for VIndex in Splitted do begin
      ExecuteScript('Result := ' + VIndex);
      if not Calc.VarByName('Result', V) then
        exit;
      if VarIsStr(V.Value) then
        AddIndexesFromString(V.Value)
      else
        Face.AddVertexByIndex(Round(extended(V.Value)));
    end;
  end; // AddIndexesFromString

var
  i: integer;
  InvisibleEdges: integer;
begin // FaceLoadFromXml
  Face.Clear;
  Face.Color := ReadColorExpressionFromXml(XmlNode, 'Color', -1);

  AddIndexesFromString(Trim(ReadStringFromXml(XmlNode, 'VertexIndexes')));

  //временно, переделать на '1,10,...'
  InvisibleEdges := ReadIntExpressionFromXml(XmlNode, 'InvisibleEdges');
  if InvisibleEdges <> 0 then
    for i := 0 to Face.GetVertexCount - 1 do
      Face.SetEdgeVisibility(i, (InvisibleEdges and (1 shl i)) = 0);

//  Face.TextureFileName := String(XmlNode.GetAttr('Texture'));
//  if Face.TextureFileName <> '' then begin
//    SetLength(Face.TextureXCoords, Face.GetVertexCount);
//    SetLength(Face.TextureYCoords, Face.GetVertexCount);
//
//    Coords := string(XmlNode.GetAttr('TextureCoordinates', ''));
//    for i := 0 to Face.GetVertexCount - 1 do begin
//      Face.TextureXCoords[i] := StrToIntDef(Trim(TUtils.Parse(Coords, ';')), 0);
//      Face.TextureYCoords[i] := StrToIntDef(Trim(TUtils.Parse(Coords, ';')), 0);
//    end;
//  end;

  Result := True;
end; // FaceLoadFromXml

class procedure TXmlReadWrite.FaceSaveToXml(Face: TFace; XmlNode: TDomNode);
var
  i, n, InvisibleEdges, ei: integer;
  S: String;
begin
  if Face.Color <> -1 then
    TDomElement(XmlNode).AttribStrings['Color'] :=  AnsiString(TUtils.ColorToStr(Face.Color));

  S := '';
  n := Face.GetVertexCount;
  for i := 0 to n - 1 do begin
    if i <> 0 then
      S := S + '; ';
    S := S + IntToStr(Face.GetVertexIndex(i));
  end;

  TDomElement(XmlNode).AttribStrings['VertexIndexes'] := AnsiString(S);

  InvisibleEdges := 0;
  ei := 1;
  for i := 0 to n - 1 do begin
    if not Face.IsEdgeVisible(i) then
      InvisibleEdges := InvisibleEdges or ei;
    ei := ei shl 1;
  end;
  if InvisibleEdges <> 0 then
    TDomElement(XmlNode).AttribStrings['InvisibleEdges'] := IntToStr(InvisibleEdges);

  // TODO !!
//  if Face.TextureFileName <> '' then begin
//    XmlNode.SetAttr('Texture', AnsiString(Face.TextureFileName));
//    S := '';
//    for i := 0 to Face.GetVertexCount - 1 do
//      S := S + IntToStr(Face.TextureXCoords[i]) + ';' + IntToStr(Face.TextureYCoords[i]) + ';';
//    XmlNode.SetAttr('TextureCoordinates', AnsiString(S));
//  end;
end; // FaceSaveToXml

// private
class function TXmlReadWrite.FindMacros(const XmlNode: TDomNode; var MacroNodes: TList<TDomNode>; var RepeatN: integer): Boolean;
var
  i: integer;
  MacroName, MacroNames, AttrName: String;
  MacroNode: TDomNode;
  lNames: TArray<String>;
begin
  MacroNames := Trim(TDomElement(XmlNode).GetAttribute('MacroName'));
  RepeatN := ReadIntExpressionFromXml(XmlNode, 'Repeat', 1);

  for i := 0 to XmlNode.Attributes.Length - 1 do begin
    AttrName := String(XmlNode.Attributes[i].NodeName);
    if (AttrName <> 'MacroName') and (AttrName <> 'Repeat') then
      ExecuteScript(AttrName + ':=' + String(TDomElement(XmlNode).GetAttribute(AttrName)));
  end;

  lNames := MacroNames.Split([' ']);
  for MacroName in lNames do
    if Length(MacroName) > 0 then
      for i := lMacros.Count - 1 downto 0 do begin
        MacroNode := lMacros[i];
        if SameText(MacroName, Trim(TDomElement(MacroNode).GetAttribute('Name'))) then begin
          MacroNodes.Add(MacroNode);
          break;
        end;
      end;
  Result := MacroNodes.Count > 0;
end; // FindMacros

class function TXmlReadWrite.FacesLoadFromXml(Faces: TList<TFace>; XmlNode: TDomNode; Points: TList<TVector>): Boolean;
var
  i: integer;
  XmlSubNode: TDomNode;
  Face: TFace;
  S: String;

  function ExecMacro(XmlNode: TDomNode): Boolean;
  var
    i, n: integer;
    MacroNodes: TList<TDomNode>;
    MacroNode: TDomNode;
  begin
    Result := False;
    MacroNodes := TList<TDomNode>.Create;
    try
      if FindMacros(XmlNode, MacroNodes, n) then
        for i := 0 to n - 1 do
          for MacroNode in MacroNodes do
            if not FacesLoadFromXml(Faces, MacroNode, Points) then
              exit;
      Result := True;
    finally
      MacroNodes.Free;
    end;
  end; // ExecMacro

begin
  Result := False;
  if XmlNode = nil then
    exit;

  for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
    XmlSubNode := XmlNode.ChildNodes[i];
    S := String(XmlSubNode.NodeName);
    if S = 'Face' then begin
      Face := TFace.Create(Points);
      if not FaceLoadFromXml(Face, XmlSubNode) then
        exit;
      Faces.Add(Face);
    end else if S = 'ExecMacro' then begin
      if not ExecMacro(XmlSubNode) then
        exit;
    end else if S = 'Script' then
      ExecuteScript(String(XmlSubNode.TextContent))
  end;

  Result := True;
end; // FacesLoadFromXml

class procedure TXmlReadWrite.FacesSaveToXml(Faces: TList<TFace>; XmlDoc: TXmlDocument; XmlNode: TDomNode; Points: TList<TVector>);
var i: integer;
    NewNode: TDomElement;
begin
  for i := 0 to Faces.Count - 1 do begin
    NewNode := XmlDoc.CreateElement('Face');
    XmlNode.AppendChild(NewNode);
    FaceSaveToXml(Faces[i], NewNode);
  end;
end;

// ========================= Part ===================================

class function TXmlReadWrite.PartLoadFromXml(Part: TPart; XmlNode: TDomNode): Boolean;
var //Script: String;
    i: integer;
    XmlSubNode: TDomNode;
    S: String;
//    UsedVertices: TList;
//  XmlRW: TXmlReadWrite;
begin
  Part.Clear;

//  ReadStringFromXml(XmlNode, 'Script', Script, '');
//  if Script <> '' then
//    ExecuteScript(Script);
//
//  VectorsLoadFromXml(Part.Vectors, XmlNode.SelectSingleNode('Vertices'));
//  FacesLoadFromXml(Part.Faces, XmlNode.SelectSingleNode('Faces'), Part.Vectors);

  for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
    XmlSubNode := XmlNode.ChildNodes[i];
    S := XmlSubNode.NodeName;
    if S = 'Script' then
      ExecuteScript(XmlSubNode.TextContent)
    else if S = 'Vertices' then
      VectorsLoadFromXml(Part.Vertices, XmlSubNode)
    else if S = 'Faces' then
      FacesLoadFromXml(Part.Faces, XmlSubNode, Part.Vertices)
  end;

  // проверяем на неиспользуемые вершины
//  UsedVertices := TList.Create;
//  for i := 0 to Faces.Count - 1 do
//    for j := 0 to Faces[i].Vertices.Count - 1 do
//      UsedVertices.Add(Faces[i].Vertices[j]);
//  for i := Vertices.Count - 1 downto 0 do
//    if UsedVertices.IndexOf(Vertices[i]) = -1 then begin
//      Vertices.Delete(i);
//    end;
//  UsedVertices.Free;

  Result := (Part.Faces.Count <> 0) and (Part.Vertices.Count <> 0);
end; // PartLoadFromXml

class procedure TXmlReadWrite.PartSaveToXml(Part: TPart; XmlDoc: TXmlDocument; XmlNode: TDomNode);
var NewNode: TDomElement;
begin
  NewNode := XmlDoc.CreateElement('Vertices');
  VectorsSaveToXml(Part.Vertices, XmlDoc, NewNode);
  XmlNode.AppendChild(NewNode);

  NewNode := XmlDoc.CreateElement('Faces');
  FacesSaveToXml(Part.Faces, XmlDoc, NewNode, Part.Vertices);
  XmlNode.AppendChild(NewNode);
end;

// ========================= Additional parts ===================================

class procedure TXmlReadWrite.AddCylinder(Part: TPart; const Base, Top: TVector; Radius: extended; Color, BaseColor, TopColor: TColor);
var
  i: integer;
  x: array[0..CIRCLE_SECTORS - 1] of extended;
  y: array[0..CIRCLE_SECTORS - 1] of extended;
  dA, Al, Height: extended;
  V, a: TVector;
  Vec: TVector;

var
  FaceTop, FaceBottom, FaceSide: TFace;
begin // AddCylinder
  dA := 2 * PI / CIRCLE_SECTORS;
  Al := 0;
  for i := 0 to CIRCLE_SECTORS - 1 do begin
    x[i] := cos(Al);
    y[i] := sin(Al);
    Al := Al + dA;
  end;

  FaceBottom := TFace.Create(Part.Vertices);
  FaceBottom.Color := BaseColor;

  FaceTop := TFace.Create(Part.Vertices);
  FaceTop.Color := TopColor;

  // top and bottom
  for i := 0 to CIRCLE_SECTORS - 1 do begin
    Vec := [x[i], y[i], 0];
    Part.Vertices.Add(Vec);
    FaceBottom.AddVector(Vec);
    FaceBottom.Rotate(-1);

    Vec := [x[i], y[i], 1];
    Part.Vertices.Add(Vec);
    FaceTop.AddVector(Vec);
  end;
  Part.Faces.Add(FaceBottom);
  Part.Faces.Add(FaceTop);

  // sides
  for i := 0 to CIRCLE_SECTORS - 2 do begin
    FaceSide := Part.AddFace([i * 2 + 1, i * 2, i * 2 + 2, i * 2 + 3]);
    FaceSide.Color := Color;
    FaceSide.SetEdgeVisibility(0, False);
    FaceSide.SetEdgeVisibility(2, False);
  end;
  FaceSide := Part.AddFace([(CIRCLE_SECTORS - 1) * 2 + 1, (CIRCLE_SECTORS - 1) * 2, 0, 1]);
  FaceSide.Color := Color;
  FaceSide.SetEdgeVisibility(0, False);
  FaceSide.SetEdgeVisibility(2, False);

  V := Top - Base;
  Height := V.Abs;
  V.Normalize;

  Part.Trans(TMatrix.GetScaleMatrix(Radius, Radius, Height));

  a := [0, 0, 1];
  if not V.IsNear(a) then begin
    Al := ArcCos(V ** a);
    if Abs(Abs(Al) - Pi) < 1e-6 then
      Part.Trans(TMatrix.GetRotateMatrix(Vector(1, 0, 0), Al))
    else if Abs(Al) > 1e-6 then begin
      a := a * V;
      a.Normalize;
      Part.Trans(TMatrix.GetRotateMatrix(a, Al));
    end;
  end;
  Part.Trans(TMatrix.GetMoveMatrix(Base));
end; // AddCylinder

class function TXmlReadWrite.PartLoadCylinder(Part: TPart; XmlNode: TDomNode): Boolean;
var
  Radius: extended;
  Color, BaseColor, TopColor: TColor;
  Base, Top: TVector;
begin // PartLoadCylinder
  Result := False;
  if XmlNode = nil then
    exit;

  Radius := ReadDoubleExpressionFromXml(XmlNode, 'Radius');
  Color := ReadColorExpressionFromXml(XmlNode, 'Color');

  ReadVectorFromXml(Base, XmlNode, 'Base');
  BaseColor := ReadColorExpressionFromXml(XmlNode, 'BaseColor', -1);
  if BaseColor = -1 then
    BaseColor := ReadColorExpressionFromXml(XmlNode.FindNode('Base'), 'Color', -1);

  ReadVectorFromXml(Top, XmlNode, 'Top');
  TopColor := ReadColorExpressionFromXml(XmlNode, 'TopColor');
  if TopColor = -1 then
    TopColor := ReadColorExpressionFromXml(XmlNode.FindNode('Top'), 'Color');

  AddCylinder(Part, Base, Top, Radius, Color, BaseColor, TopColor);
  Result := True;
end; // PartLoadCylinder

class procedure TXmlReadWrite.AddCircle(Part: TPart; const Base, Center: TVector; Radius: extended; Color: TColor);
var
  i: integer;
  Al: extended;
  V, a: TVector;
  Face: TFace;
begin // AddCircle
  Face := TFace.Create(Part.Vertices);
  for i := 0 to CIRCLE_SECTORS - 1 do begin
    Al := i * 2 * PI / CIRCLE_SECTORS;
    V := [cos(Al), sin(Al), 0];
    Part.AddVertex(V);
    Face.AddVector(V);
  end;

  Face.Color := Color;
  Part.Faces.Add(Face);

  V := Center - Base;
  V.Normalize;

  Part.Trans(TMatrix.GetScaleMatrix(Radius, Radius, 1));

  a := [0, 0, 1];
  if not V.IsNear(a) then begin
    Al := ArcCos(V ** a);
    if Abs(Abs(Al) - Pi) < 1e-6 then
      Part.Trans(TMatrix.GetRotateMatrix(Vector(1, 0, 0), Al))
    else if Abs(Al) > 1e-6 then begin
      a := a * V;
      a.Normalize;
      Part.Trans(TMatrix.GetRotateMatrix(a, Al));
    end;
  end;

  Part.Trans(TMatrix.GetMoveMatrix(Center));
end; // AddCircle

class function TXmlReadWrite.PartLoadCircle(Part: TPart; XmlNode: TDomNode): Boolean;
var
  Base, Center: TVector;
  Radius: extended;
  Color: TColor;
begin // LoadCircle
  Result := False;
  if XmlNode = nil then
    exit;

  Radius := ReadDoubleExpressionFromXml(XmlNode, 'Radius');
  Color := ReadColorExpressionFromXml(XmlNode, 'Color');

  if not ReadVectorFromXml(Base, XmlNode, 'Base') then
    Base := [0, 0, 0];

  if not ReadVectorFromXml(Center, XmlNode, 'Center') then
    Center := [1, 0, 0];

  AddCircle(Part, Base, Center, Radius, Color);

  Result := True;
end; // PartLoadCircle

// ========================= Header ===================================

class function TXmlReadWrite.PuzzleCheckVersionFromXml(Puzzle: TPuzzle; XmlNode: TDomNode): Boolean;
begin
  Result := StrToInt(TDomElement(XmlNode).GetAttribute('Data_Version')) = XML_VERSION;
//  if not Result then
//    ShowMessage('Wrong version of file ' + FileName); // TODO :
end; // PuzzleCheckVersionFromXml

class function TXmlReadWrite.PuzzleLoadHeaderFromXml(Puzzle: TPuzzle; XmlNode: TDomNode): Boolean;
var InterfaceNode: TDomNode;
    FileName: String;
begin
  Result := False;

  FileName := Puzzle.Header.FileName;
  Puzzle.ClearHeader;
  Puzzle.Header.FileName := FileName;

  if XmlNode = nil then
    exit;

  if not PuzzleCheckVersionFromXml(Puzzle, XmlNode) then
    exit;

  InterfaceNode := XmlNode.FindNode('Interface');
  if InterfaceNode = nil then
    exit;

  with Puzzle.Header do begin
    Name        := ReadStringFromXml     (InterfaceNode, 'Name');
    Aliases     := ReadStringFromXml     (InterfaceNode, 'Aliases');
    ClassString := ReadStringFromXml     (InterfaceNode, 'Class');
    MenuString  := ReadMultiStringFromXml(InterfaceNode, 'Menu', ';');
    Inventor    := ReadStringFromXml     (InterfaceNode, 'Inventor');
    Programmer  := ReadStringFromXml     (InterfaceNode, 'Programmer');
    Added       := ReadStringFromXml     (InterfaceNode, 'Added');
    Link        := ReadMultiStringFromXml(InterfaceNode, 'Link', ';');
  end;
  Result := True;
end; // PuzzleLoadHeaderFromXml

class function TXmlReadWrite.PuzzleLoadHeaderFromXmlFile(Puzzle: TPuzzle; const FileName: String): Boolean;
var
  XmlDoc: TXmlDocument;
begin
  Result := False;
  if not FileExists(FileName) then
    exit;

//  XmlDoc := TXmlDocument.Create;
  try
    ReadXMLFile(XmlDoc, FileName);
    Result := PuzzleLoadHeaderFromXml(Puzzle, XmlDoc.DocumentElement);
  finally
    XmlDoc.Free;
  end;

//  Clear;
end; // PuzzleLoadHeaderFromXmlFile

class procedure TXmlReadWrite.PuzzleSaveHeaderToXml(Puzzle: TPuzzle; XmlDoc: TXmlDocument; XmlNode: TDomNode);
var S: String;
    NewNode, NN: TDomElement;
begin

    NewNode := XmlDoc.CreateElement('Interface');
    XmlNode.AppendChild(NewNode);

//    NewNode.AttribStrings['NormDistance'] := FloatToStr(Plane.NormDistance);

    with Puzzle.Header do begin
      NN := XmlDoc.CreateElement('Name');
      NN.TextContent := Name;
      NewNode.AppendChild(NN);

      NN := XmlDoc.CreateElement('Aliases');
      NN.TextContent := Aliases;
      NewNode.AppendChild(NN);

      NN := XmlDoc.CreateElement('Class');
      NN.TextContent := ClassName;
      NewNode.AppendChild(NN);

      NN := XmlDoc.CreateElement('Inventor');
      NN.TextContent := Inventor;
      NewNode.AppendChild(NN);

      NN := XmlDoc.CreateElement('Programmer');
      NN.TextContent := Programmer;
      NewNode.AppendChild(NN);

      NN := XmlDoc.CreateElement('Added');
      NN.TextContent := Added;
      NewNode.AppendChild(NN);

      NN := XmlDoc.CreateElement('Link');
      NN.TextContent := Link;
      NewNode.AppendChild(NN);


      S := MenuString;
      while S <> '' do begin
        NN := XmlDoc.CreateElement('Menu');
        NN.TextContent := TUtils.Parse(S, ';');
        NewNode.AppendChild(NN);
      end;
    end;
end; // PuzzleSaveHeaderToXml

// ========================= Puzzle ===================================

class function TXmlReadWrite.PuzzleLoadFromXml(Puzzle: TPuzzle; XmlNode: TDomNode): Boolean;

  procedure UndoRepeat(XmlNode: TDomNode);
  var
    i, n: integer;

  begin
    n := ReadIntExpressionFromXml(XmlNode, 'Repeat', 1);
    for i := 0 to n - 1 do
      if not Puzzle.Undo(False) then
        exit;
  end; // UndoRepeat

  procedure Split(XmlNode: TDomNode);
  var
    AxisNo, FromPlane: integer;
    PlaneX: TPlane;
    Color1, Color2: TColor;
    InvisibleEdges1, InvisibleEdges2: integer;
  begin
    AxisNo := ReadIntExpressionFromXml(XmlNode, 'Axis', -1);
    FromPlane := ReadIntExpressionFromXml(XmlNode, 'From', -1) - 1;
    Color1 := ReadColorExpressionFromXml(XmlNode, 'Color1', -1);
    Color2 := ReadColorExpressionFromXml(XmlNode, 'Color2', -1);
    InvisibleEdges1 := ReadIntExpressionFromXml(XmlNode, 'InvisibleEdges1', 0);
    InvisibleEdges2 := ReadIntExpressionFromXml(XmlNode, 'InvisibleEdges2', 0);
    if (AxisNo <> -1) and (FromPlane <> -1) then
      TPuzzleUtils.SplitByPlane(Puzzle, Puzzle.Axes[AxisNo].GetLayerPlaneTo(FromPlane), Color1, Color2, InvisibleEdges1, InvisibleEdges2)
    else begin
      if PlaneLoadFromXml(PlaneX, XmlNode.FindNode('Plane')) then
        TPuzzleUtils.SplitByPlane(Puzzle, PlaneX, Color1, Color2, InvisibleEdges1, InvisibleEdges2);
    end;
  end; // Split

  procedure Cut(XmlNode: TDomNode);
  var
    AxisNo, FromPlane, ToPlane: integer;
    PlaneX: TPlane;
    Color: TColor;
    InvisibleEdges: integer;
  begin
    AxisNo := ReadIntExpressionFromXml(XmlNode, 'Axis', -1);
    InvisibleEdges := ReadIntExpressionFromXml(XmlNode, 'InvisibleEdges', 0);
    if (AxisNo <> -1) then begin
      FromPlane := ReadIntExpressionFromXml(XmlNode, 'From', -1) - 1;
      ToPlane := ReadIntExpressionFromXml(XmlNode, 'To', -1) - 1;
      Color := ReadColorExpressionFromXml(XmlNode, 'Color', -1);
      if (FromPlane > -1) then
        TPuzzleUtils.CutByPlane(Puzzle, Puzzle.Axes[AxisNo].GetLayerPlaneTo(FromPlane), Color, InvisibleEdges);
      if (ToPlane <> -2) then begin
        PlaneX := Puzzle.Axes[AxisNo].GetLayerPlaneTo(ToPlane);
        PlaneX.TurnRound;
        TPuzzleUtils.CutByPlane(Puzzle, PlaneX, Color, InvisibleEdges);
      end;
    end else begin
      if PlaneLoadFromXml(PlaneX, XmlNode.FindNode('Plane')) then begin
        Color := ReadColorExpressionFromXml(XmlNode, 'Color', -1);
        TPuzzleUtils.CutByPlane(Puzzle, PlaneX, Color, InvisibleEdges);
      end;
    end;
  end; // Cut

  procedure Hide(XmlNode: TDomNode);
  var
    AxisNo, FromLayer, ToLayer, Layer, iFrom, iTo: integer;
    PlaneX: TPlane;
    hp: THalfOfPlane;
    i, j: integer;
    Layers: array of Boolean;
    sLayers: string;
  begin
    AxisNo := ReadIntExpressionFromXml(XmlNode, 'Axis', -1);
    if (AxisNo <> -1) then begin
      FromLayer := ReadIntExpressionFromXml(XmlNode, 'From', -1);
      ToLayer := ReadIntExpressionFromXml(XmlNode, 'To', -1);
      Layer := ReadIntExpressionFromXml(XmlNode, 'Layer', -1);
      if ((FromLayer > -1) and (ToLayer = -1)) and (Layer = -1) then
        ToLayer := Puzzle.Axes[AxisNo].Layers.Count - 1
      else if ((FromLayer = -1) and (ToLayer > -1)) and (Layer = -1) then
        FromLayer := 0
      else if ((FromLayer = -1) or (ToLayer = -1)) and (Layer <> -1) then begin
        FromLayer := Layer;
        ToLayer := Layer;
      end;
      for i := 0 to Puzzle.Parts.Count - 1 do begin
        Puzzle.GetPartPosition(AxisNo, i, iFrom, iTo);
        if (iFrom >= FromLayer) and (iTo <= ToLayer) then
          Puzzle.Parts[i].Visible := False;
      end;

      sLayers := ReadStringFromXml(xmlNode, 'Layers');
      if sLayers <> '' then begin
        SetLength(Layers, Puzzle.Axes[AxisNo].Layers.Count);
        DecodeLayers(sLayers, Layers);
        for i := 0 to Puzzle.Parts.Count - 1 do begin
          Puzzle.GetPartPosition(AxisNo, i, iFrom, iTo);
          for j := iFrom to iTo do
            if Layers[j] then
              Puzzle.Parts[i].Visible := False;
        end;
      end;

    end else begin
      if PlaneLoadFromXml(PlaneX, XmlNode.FindNode('Plane')) then begin
        for i := 0 to Puzzle.Parts.Count - 1 do begin
          hp := Puzzle.Parts[i].HalfPlane(PlaneX);
          if hp = hopInside then
            Puzzle.Parts[i].Visible := False;
        end;
      end;
    end;
  end; // Hide

  procedure HideContaining(XmlNode: TDomNode);
  var Vector: TVector;
      Vectors: TList<TVector>;
      Part: TPart;
  begin
    Vectors := TList<TVector>.Create;
    try
      GetVectorsFromString(ReadStringFromXml(XmlNode, 'Vectors'), Vectors);
      GetVectorsFromString(ReadStringFromXml(XmlNode, 'Vector'), Vectors);

      for Part in Puzzle.Parts do begin
        if Part.Visible then
          for Vector in Vectors do

            if Part.Contains(Vector) then begin
              Part.Visible := False;
              break;
            end;
      end;

    finally
      Vectors.Free;
    end;
  end; // HideContaining

  procedure RemoveAllVisible;
  var Part: TPart;
      i: integer;
  begin
    for i := Puzzle.Parts.Count - 1 downto 0 do begin
      Part := Puzzle.Parts[i];
      if Part.Visible then begin
        Part.Free;
        Puzzle.Parts.Delete(i);
        Puzzle.PositionsInvalidate;
      end;
    end;
  end; // RemoveAllVisible

  procedure RemoveContaining(XmlNode: TDomNode);
  var Vector: TVector;
      Vectors: TList<TVector>;
      Part: TPart;
      i: integer;
  begin
    Vectors := TList<TVector>.Create;
    try
      GetVectorsFromString(ReadStringFromXml(XmlNode, 'Vectors'), Vectors);
      GetVectorsFromString(ReadStringFromXml(XmlNode, 'Vector'), Vectors);

      for i := Puzzle.Parts.Count - 1 downto 0 do begin
        Part := Puzzle.Parts[i];
        if Part.Visible then
          for Vector in Vectors do
            if Part.Contains(Vector) then begin
              Part.Free;
              Puzzle.Parts.Delete(i);
              Puzzle.PositionsInvalidate;
              break;
            end;
      end;

    finally
      Vectors.Free;
    end;
  end; // RemoveContaining

  procedure SetSpike(XmlNode: TDomNode);
  var Vector: TVector;
      Vectors: TList<TVector>;
      Level: integer;
      Face: TFace;
  begin
    Vectors := TList<TVector>.Create;
    try
      Level := ReadIntExpressionFromXml(XmlNode, 'Level', 0);
      if Level <> 0 then begin
        GetVectorsFromString(ReadStringFromXml(XmlNode, 'Near'), Vectors);

        for Vector in Vectors do begin
          Face := TPuzzleUtils.GetNearestVisibleFace(Puzzle, Vector);
          if Face <> nil then
            Face.SetSpike(Level);
        end;
      end;
    finally
      Vectors.Free;
    end;
  end; // SetSpike

  procedure LoadProcedures(XmlNode: TDomNode);
  var ProcName: String;
  begin
    if XmlNode = nil then
      exit;

    ReadStringFromXml(XmlNode, 'CheckSolved', ProcName, '');
    if SameText(ProcName, 'EachColorHasSameOrOppositeDirection') then
      Puzzle.CheckSolvedProc := cspEachColorHasSameOrOppositeDirection
    else if SameText(copy(ProcName, 1, 17), 'CountUniquePlanes') then begin
      Puzzle.CheckSolvedProc := cspCountUniquePlanes;
      TUtils.Parse(ProcName, '=');
      Puzzle.SolvedStates.UniquePlanes := TUtils.ParseInt(ProcName);
    end else if SameText(ProcName, 'EachFaceConsistsOfDifferentColors') then
      Puzzle.CheckSolvedProc := cspEachFaceConsistsOfDifferentColors
    else if SameText(ProcName, 'EachRowColConsistsOfDifferentColors') then
      Puzzle.CheckSolvedProc := cspEachRowColConsistsOfDifferentColors
    else if SameText(ProcName, 'EachRowColDiagConsistsOfDifferentColors') then
      Puzzle.CheckSolvedProc := cspEachRowColDiagConsistsOfDifferentColors
    else if SameText(ProcName, 'CheckSavedStates') then
      Puzzle.CheckSolvedProc := cspCheckSavedStates
    else if SameText(ProcName, 'EachPlaneHasOneColor') then
      Puzzle.CheckSolvedProc := cspEachPlaneHasOneColor
    else if SameText(copy(ProcName, 1, 44), 'EachColorHasSameDirectionOrCountUniquePlanes') then begin
      Puzzle.CheckSolvedProc := cspEachColorHasSameDirectionOrCountUniquePlanes;
      TUtils.Parse(ProcName, '=');
      Puzzle.SolvedStates.UniquePlanes := TUtils.ParseInt(ProcName);
    end else if SameText(ProcName, 'AdjoiningFacesHaveSameColors') then
      Puzzle.CheckSolvedProc := cspAdjoiningFacesHaveSameColors
    else
      Puzzle.CheckSolvedProc := cspEachColorHasSameDirection;
  end; // LoadProcedures

  procedure LoadTextures(XmlNode: TDomNode);
  var
    i: integer;
    XmlSubNode: TDomNode;
    S: String;
    Texture: TTexture;
  begin // LoadFigure
    for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
      XmlSubNode := XmlNode.ChildNodes[i];
      S := String(XmlSubNode.NodeName);
      if S = 'Texture' then begin
        Texture := TTexture.Create;
        TextureLoadFromXml(Texture, XmlSubNode);
        Puzzle.Textures.Add(Texture);
      end;
    end;
  end; // LoadTextures

  function FindByName(const Name: String): integer;
  var i: integer;
  begin
    Result := -1;
    for i := 0 to Puzzle.Textures.Count - 1 do
      if SameText(Name, Puzzle.Textures[i].Name) then begin
        Result := i;
        exit;
      end;
  end; // FindByName

  procedure SetTextures(XmlNode: TDomNode);
  var
    i, TextureId: integer;
    iPart, iFace: integer;
    S, SP: String;
    sl: TStringList;
    Face: TFace;
    Part: TPart;
  begin
    sl := TStringList.Create;
    S := String(XmlNode.TextContent);
    sl.Text := S;

    Result := False;

    if sl.Count <> 0 then begin

      for i := sl.Count - 1 downto 0 do
        if (Trim(sl[i]) = '') or (copy(Trim(sl[i]), 1, 1) = ';') then
          sl.Delete(i);

      for i := 0 to sl.Count - 1 do begin
        S := Trim(sl[i]);
        SP := Trim(TUtils.Parse(S, '='));
        TextureId := FindByName(SP);
        if TextureId <> -1 then
          while Trim(S) <> '' do begin
            iPart := TUtils.ParseInt(S);
            if (iPart < 0) or (iPart >= Puzzle.Parts.Count) then
              continue;
            Part := Puzzle.Parts[iPart];
            if Part = nil then
              continue;

            iFace := TUtils.ParseInt(S);
            if (iFace < 0) or (iFace >= Part.Faces.Count) then
              continue;
            Face := Part.Faces[iFace];
            if Face = nil then
              continue;

            Face.Rotate(TUtils.ParseInt(S));
            Face.TextureId := TextureId;
          end;
      end;

    end;
    sl.Free;
  end; // SetTextures

  procedure LoadFigure(XmlNode: TDomNode); forward;

  function LoadFrom(XmlNode: TDomNode): Boolean;
  var XmlDoc: TXmlDocument;

    function Load(const FileName: String): Boolean;
    begin
      Result := FileExists(FileName);
      if Result then
        ReadXMLFile(XmlDoc, FileName);
//        XmlDoc.Load(AnsiString(FileName));
    end;
  var
    S, AttrName: String;
    i: integer;
  begin
    for i := 0 to XmlNode.Attributes.Length - 1 do begin
      AttrName := XmlNode.Attributes[i].NodeName;
      if AttrName <> 'File' then
        ExecuteScript(AttrName + ':=' + TDomElement(XmlNode).GetAttribute(AttrName));
    end;

    ReadStringFromXml(XmlNode, 'File', S, '');
//    if S = '' then
//      ReadStringFromXml(XmlNode, '', S, '');
    S := Trim(S);

//    XmlDoc := TXmlDocument.Create;

    Result := Load(TUtils.GetStartDir + S);
    if not Result then
      Result := Load(ExtractFilePath(Puzzle.Header.FileName) + S);
    if Result then begin
      LoadFigure(XmlDoc.DocumentElement);
      XmlDoc.Free;
    end;
  end;

  procedure ExecMacro(XmlNode: TDomNode);
  var
    i, k, n: integer;
    MacroName, AttrName: String;
    MacroNames: TArray<String>;
  begin
    MacroName := Trim(TDomElement(XmlNode).GetAttribute('MacroName'));
    MacroNames := MacroName.Split([' ']);
    n := ReadIntExpressionFromXml(XmlNode, 'Repeat', 1);

    for i := 0 to XmlNode.Attributes.Length - 1 do begin
      AttrName := XmlNode.Attributes[i].NodeName;
      if (AttrName <> 'MacroName') and (AttrName <> 'Repeat') then
        ExecuteScript(AttrName + ':=' + TDomElement(XmlNode).GetAttribute(AnsiString(AttrName)));
    end;

    for k := 0 to n - 1 do
      for MacroName in MacroNames do
        if Length(MacroName) > 0 then
          for i := lMacros.Count - 1 downto 0 do begin
            XmlNode := lMacros[i];
            if SameText(MacroName, Trim(TDomElement(XmlNode).GetAttribute('Name'))) then begin
              LoadFigure(XmlNode);
              break;
            end;
          end;
  end; // ExecMacro

  procedure Connect(XmlNode: TDomNode);

    function ParseInt(var S: String): integer;
    begin
      Result := StrToIntDef(Trim(TUtils.Parse(S, ';')), -1);
    end; // ParseInt

  var
    AxisNo, Layer1, Layer2, i: integer;
    SLayers: string;
  begin
    AxisNo := ReadIntExpressionFromXml(XmlNode, 'Axis', -1);

    SLayers := TDomElement(XmlNode).GetAttribute('Layers');
    Layer1 := ParseInt(SLayers);
    Layer2 := ParseInt(SLayers);
    while (Layer1 <> -1) and (Layer2 <> -1) do begin
      if AxisNo >= 0 then
        Puzzle.Axes[AxisNo].ConnectLayers(Layer1, Layer2)
      else
        for i := 0 to Puzzle.Axes.Count - 1 do
          Puzzle.Axes[i].ConnectLayers(Layer1, Layer2);
      Layer1 := Layer2;
      Layer2 := ParseInt(SLayers);
    end;
  end; // Connect

  procedure LoadFigure(XmlNode: TDomNode);
  var
    i: integer;
    XmlSubNode: TDomNode;
    S, Script: String;
    Part: TPart;
    SolvedState: TSolvedState;
  begin // LoadFigure
    for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
      XmlSubNode := XmlNode.ChildNodes[i];
      S := String(XmlSubNode.NodeName);
      if S = 'LoadFrom' then begin
        LoadFrom(XmlSubNode);
      end else if S = 'Part' then begin
        Part := TPart.Create;
        if PartLoadFromXml(Part, XmlSubNode) then begin
          Puzzle.Parts.Add(Part);
          Puzzle.PositionsInvalidate;
        end else
          Part.Free;
      end else if S = 'Macro' then
        lMacros.Add(XmlSubNode)
      else if S = 'ExecMacro' then begin
        ExecMacro(XmlSubNode);
      end else if S = 'Circle' then begin
        Part := TPart.Create;
        if PartLoadCircle(Part, XmlSubNode) then begin
          Puzzle.Parts.Add(Part);
          Puzzle.PositionsInvalidate;
        end else
          Part.Free;
      end else if S = 'Cylinder' then begin
        Part := TPart.Create;
        if PartLoadCylinder(Part, XmlSubNode) then begin
          Puzzle.Parts.Add(Part);
          Puzzle.PositionsInvalidate;
        end else
          Part.Free;
      end else if S = 'Script' then begin
        ReadStringFromXml(XmlSubNode, '', Script, '');
        ExecuteScript(Script);
      end else if S = 'SplitByAxes' then
        TPuzzleUtils.SplitByAxes(Puzzle)
      else if S = 'Split' then
        Split(XmlSubNode)
      else if S = 'Cut' then
        Cut(XmlSubNode)
      else if S = 'Transfer' then
        Transfer(Puzzle, XmlSubNode)
      else if S = 'Turn' then
        Turn(Puzzle, XmlSubNode)
      else if S = 'Undo' then
        UndoRepeat(XmlSubNode)
      else if S = 'Hide' then
        Hide(XmlSubNode)
      else if S = 'HideAll' then
        TPuzzleUtils.SetAllInvisible(Puzzle)
      else if S = 'HideContaining' then
        HideContaining(XmlSubNode)
      else if S = 'SetSpike' then
        SetSpike(XmlSubNode)
      else if S = 'ShowAll' then
        TPuzzleUtils.SetAllVisible(Puzzle)
      else if S = 'InvertVisibility' then
        TPuzzleUtils.InvertVisibility(Puzzle)
      else if S = 'RemoveAllVisible' then
        RemoveAllVisible
      else if S = 'RemoveGrayParts' then
        TPuzzleUtils.RemoveGrayParts(Puzzle)
      else if S = 'RemoveContaining' then
        RemoveContaining(XmlSubNode)
      else if S = 'Connect' then // future
        Connect(XmlSubNode)
      else if S = 'SaveSolvedState' then begin
        SolvedState := TSolvedState.Create;
        SolvedState.CalcFromPuzzle(Puzzle);
        Puzzle.SolvedStates.SolvedStates.Add(SolvedState);
      end;
    end;
  end; // LoadFigure

  procedure LoadGraphicsOptions(XmlNode: TDomNode);
//  var
//    i: integer;
//    S: String;
//    XmlSubNode: TDomNode;
  begin
//    for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
//      XmlSubNode := XmlNode.ChildNodes[i];
//      S := XmlSubNode.Get_NodeName;
//      if SameText(S, 'BSP') then begin
//        if SameText(ReadStringFromXml(XmlSubNode, 'AlwaysRecalcBspPlanes'), 'yes') then
//          AlwaysRecalcBspPlanes := True;
//      end;
//    end;
  end; // LoadGraphicsOptions

//  procedure SearchAndCombineParallelAxes;
//  var
//    i, j, k: integer;
//    Axis1, Axis2: TAxis;
//  begin
//    // search for same or opposite axes
//    for i := Puzzle.Axes.Count - 2 downto 0 do
//      for j := Puzzle.Axes.Count - 1 downto i + 1 do begin
//        Axis1 := Puzzle.Axes[i];
//        Axis2 := Puzzle.Axes[j];
//        if Axis1.Line.isNear(Axis2.Line) then begin
//          TUtils.Log('Parallel axes: ' + IntToStr(i) + ':' + IntToStr(j));
////          ShowMessage(Name + ': parallel axes');
//          for k := 0 to Axis2.Layers.Count - 2 do
//            Axis1.AddPlane(Axis2.Layers[k].DistanceTo);
//          Puzzle.Axes.Delete(j);
//        end else if Axis1.Line.isNearOpposite(Axis2.Line) then begin
//          TUtils.Log('Opposite axes: ' + IntToStr(i) + ':' + IntToStr(j));
//          //ShowMessage(Name + ': opposite axes');
//          for k := 0 to Axis2.Layers.Count - 2 do
//            Axis1.AddPlane(-Axis2.Layers[k].DistanceTo);
//          Puzzle.Axes.Delete(j);
//        end;
//      end;
//  end; // SearchAndCombineParallelAxes

var
  i{, j}: integer;
  S: AnsiString;
  XmlSubNode: TDomNode;
begin // PuzzleLoadFromXml
  Result := False;
  lMacros := TList<TDomNode>.Create;
  try

    Calc.ClearVars; // очищаем Script во избежание накладок от предыдущих скриптов
    Calc.ClearProcs;
    Calc.DefineConsts;

    ExecuteScript(TSettings.PreScript); // 03.08.2017 по просьбе Евгения Григорьева - прескрипт

    //Clear;

    if not PuzzleLoadHeaderFromXml(Puzzle, XmlNode) then
      exit;

    Puzzle.BeforeLoadFromXmlEvent(XmlNode);

    for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
      XmlSubNode := XmlNode.ChildNodes[i];
      S := XmlSubNode.NodeName;
      if S = 'Script' then
        ExecuteScript(XmlSubNode.TextContent)
      else if S = 'Axes' then
        AxesLoadFromXml(Puzzle.Axes, XmlSubNode)
      else if S = 'Figure' then
        LoadFigure(XmlSubNode)
      else if S = 'Macros' then
        MacrosLoadFromXml(Puzzle.Macros, XmlSubNode)
      else if S = 'Procedures' then
        LoadProcedures(XmlSubNode)
      else if S = 'GraphicsOptions' then
        LoadGraphicsOptions(XmlSubNode)
      else if S = 'Textures' then
        LoadTextures(XmlSubNode)
      else if S = 'SetTextures' then
        SetTextures(XmlSubNode)
      else if S = 'Undo' then
        TurnsLoadFromXml(Puzzle, Puzzle.UndoStack, XmlSubNode)
    end;

    // Assign macro axes
  //  for i := 0 to Puzzle.Macros.Count - 1 do
  //    for j := 0 to Puzzle.Macros[i].Turns.Count - 1 do
  //      if Puzzle.Macros[i].Turns[j].AxisNo >= 0 then
  //        Puzzle.Macros[i].Turns[j].AssignAxis(Puzzle.Axes[Puzzle.Macros[i].Turns[j].AxisNo]);

  //  SearchAndCombineParallelAxes;

  Puzzle.AfterLoadFromXmlEvent(XmlNode);
  //  Puzzle.GraphicsEngineOnAfterLoadFromXml();

  //  Puzzle.CopyPlanesToBsp;
  //  Puzzle.GetPlanesFromFaces(Puzzle.Parts, Puzzle.BspPlanesPermanent);
  //  Puzzle.CalcBsp;

  finally
    FreeAndNil(lMacros);
  end;

  Result := True;

//CheckPuzzle;
end; // PuzzleLoadFromXml

class function TXmlReadWrite.PuzzleLoadFromXmlCommon(Puzzle: TPuzzle; XmlDoc: TXmlDocument; const FileName: String): Boolean;
begin
  Result := False;

//  Clear;
  Puzzle.DisableCheckFixedLayers;
  try
    Puzzle.Header.FileName := FileName;
    Result := PuzzleLoadFromXml(Puzzle, XmlDoc.DocumentElement);
    Puzzle.Header.FileName := FileName;
  finally
    Puzzle.EnableCheckFixedLayers;
  end;
end; // PuzzleLoadFromXmlCommon

class function TXmlReadWrite.PuzzleLoadFromXmlFile(Puzzle: TPuzzle; const FileName: String): Boolean;
var
  XmlDoc: TXmlDocument;
begin
  if not FileExists(FileName) then
    Exit(False);

  ReadXMLFile(XmlDoc, FileName);
  try
    //  XmlDoc := LoadXmlDocument(AnsiString(FileName));
    Result := PuzzleLoadFromXmlCommon(Puzzle, XmlDoc, FileName);
  finally
    XmlDoc.Free;
  end;
end; // PuzzleLoadFromXmlFile

const
  CASESENSITIVITY = 0;
//  WRITEBUFFERSIZE = 8192;

class function TXmlReadWrite.PuzzleLoadFromCompressedXmlFile(Puzzle: TPuzzle; const FileName: String): Boolean;
var
  XmlDoc: TXmlDocument;
  mStream: TMemoryStream;
begin
  mStream := TMemoryStream.Create;
  try
    Result := TUtils.LoadFileFromZip(mStream, PUZZLES_ARCHIVE_PATH, FileName);
    if Result then begin
      mStream.Seek(0, soBeginning);

      ReadXMLFile(XmlDoc, mStream);
//      XmlDoc := LoadXmlDocument(mStream);
      Result := PuzzleLoadFromXmlCommon(Puzzle, XmlDoc, FileName);
    end;
  finally
    XmlDoc.Free;
    mStream.Free;
  end;
end; // PuzzleLoadFromCompressedXmlFile

class procedure TXmlReadWrite.PuzzleSaveToXml(Puzzle: TPuzzle; XmlDoc: TXmlDocument; XmlRoot: TDomNode);
var
  XmlFigureNode, XmlNode, XmlProceduresNode: TDomNode;
  ProcName: String;
  i: integer;
begin
  TDomElement(XmlRoot).SetAttribute('Data_Version', IntToStr(XML_VERSION));

  PuzzleSaveHeaderToXml(Puzzle, XmlDoc, XmlRoot);

  XmlFigureNode := XmlDoc.CreateElement('Figure');
  XmlRoot.AppendChild(XmlFigureNode);

  for i := 0 to Puzzle.Parts.Count - 1 do begin
    XmlNode := XmlDoc.CreateElement('Part');
    PartSaveToXml(Puzzle.Parts[i], XmlDoc, XmlNode);
    XmlFigureNode.AppendChild(XmlNode);
  end;

  XmlNode := XmlDoc.CreateElement('Axes');
  AxesSaveToXml(Puzzle.Axes, XmlDoc, XmlNode);
  XmlRoot.AppendChild(XmlNode);

  XmlNode := XmlDoc.CreateElement('Macros');
  MacrosSaveToXml(Puzzle.Macros, XmlDoc, XmlNode);
  XmlRoot.AppendChild(XmlNode);

  XmlProceduresNode := XmlDoc.CreateElement('Procedures');
  case Puzzle.CheckSolvedProc of
    cspEachColorHasSameOrOppositeDirection:     ProcName := 'EachColorHasSameOrOppositeDirection';
    cspCountUniquePlanes:                       ProcName := 'CountUniquePlanes = ' + IntToStr(Puzzle.SolvedStates.UniquePlanes);
    cspEachFaceConsistsOfDifferentColors:       ProcName := 'EachFaceConsistsOfDifferentColors';
    cspEachRowColConsistsOfDifferentColors:     ProcName := 'EachRowColConsistsOfDifferentColors';
    cspEachRowColDiagConsistsOfDifferentColors: ProcName := 'EachRowColDiagConsistsOfDifferentColors';
    cspCheckSavedStates:                        ProcName := 'CheckSavedStates';
    cspEachPlaneHasOneColor:                    ProcName := 'EachPlaneHasOneColor';
    cspAdjoiningFacesHaveSameColors:            ProcName := 'AdjoiningFacesHaveSameColors';
    else                                        ProcName := 'EachColorHasSameDirection';
  end;
  TDomElement(XmlProceduresNode).SetAttribute('CheckSolved', ProcName);
  XmlRoot.AppendChild(XmlProceduresNode);

  XmlNode := XmlDoc.CreateElement('Undo');
  TurnsSaveToXml(Puzzle.UndoStack, XmlDoc, XmlNode);
  XmlRoot.AppendChild(XmlNode);
end; // PuzzleSaveToXml

class procedure TXmlReadWrite.PuzzleSaveToXmlFile(Puzzle: TPuzzle; const FileName: String);
var
  XmlDoc: TXmlDocument;
  XmlNode: TDomNode;
begin
  XmlDoc := TXmlDocument.Create;
  try
    XmlNode := XmlDoc.CreateElement('xml');
    PuzzleSaveToXml(Puzzle, XmlDoc, XmlNode);
    XmlDoc.AppendChild(XmlNode);

    WriteXMLFile(XmlDoc, FileName);
  finally
    XmlDoc.Free;
  end;
//  XmlDoc.Save(AnsiString(FileName));
end; // PuzzleSaveToXmlFile

// ========================= Textures ===================================

class function TXmlReadWrite.TextureLoadFromXml(Texture: TTexture; XmlNode: TDomNode): Boolean;
begin
  Texture.Name := Trim(TDomElement(XmlNode).GetAttribute('Name'));
  Texture.FileName := Trim(TDomElement(XmlNode).GetAttribute('File'));
  Result := True;
end; // TextureLoadFromXml

// ========================= Turns ===================================

class function TXmlReadWrite.TurnLoadFromXml(Puzzle: TPuzzle; Turn: TTurn; XmlNode: TDomNode): Boolean;
var S: String;
begin
  Turn.AxisNo := ReadIntExpressionFromXml(XmlNode, 'Axis');

  SetLength(Turn.Layers, Puzzle.Axes[Turn.AxisNo].Layers.Count);

  S := ReadStringFromXml(XmlNode, 'Layers');
  DecodeLayers(S, Turn.Layers);

  S := ReadStringFromXml(XmlNode, 'type');
  if (S = '') or SameText(S, 'LayerTurn') then begin
    Turn.typ      := ttyLayerTurn;
    Turn.Angle    := ReadDoubleExpressionFromXml(XmlNode, 'Angle');
  end else if SameText(S, 'PartsMove') then begin
    Turn.typ      := ttyPartsMove;
    Turn.Distance := ReadDoubleExpressionFromXml(XmlNode, 'Distance');
  end else
    Turn.typ := ttyNone;

  Result := True;
end; // TurnLoadFromXml

class procedure TXmlReadWrite.TurnSaveToXml(Turn: TTurn; XmlNode: TDomElement);
var S: String;
    i: integer;
begin
  XmlNode.SetAttribute('Axis', IntToStr(Turn.AxisNo));

  S := '';
  for i := 0 to High(Turn.Layers) do
    if Turn.Layers[i] then begin
      if S = '' then
        S := IntToStr(i)
      else
        S := S + ';' + IntToStr(i);
    end;
  XmlNode.SetAttribute('Layers', AnsiString(S));

  case Turn.typ of
    ttyNone: XmlNode.SetAttribute('typ', 'None');
    ttyLayerTurn:
      begin
        XmlNode.SetAttribute('type', 'LayerTurn');
        XmlNode.SetAttribute('Angle', FloatToStr(Turn.Angle));
      end;
    ttyPartsMove:
      begin
        XmlNode.SetAttribute('type', 'PartsMove');
        XmlNode.SetAttribute('Distance', FloatToStr(Turn.Distance));
      end;
  end;
end; // TurnSaveToXml

class function TXmlReadWrite.TurnsLoadFromXml(Puzzle: TPuzzle; Turns: TList<TTurn>; XmlNode: TDomNode): Boolean;
var
  i: integer;
  Turn: TTurn;
  XmlSubNode: TDomNode;
begin
  Result := False;

  for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
    XmlSubNode := XmlNode.ChildNodes[i];
    if SameText(XmlSubNode.NodeName, 'Turn') then begin
      Turn := TTurn.Create;
      if not TurnLoadFromXml(Puzzle, Turn, XmlSubNode) then begin
        Turn.Free;
        exit;
      end;
      Turns.Add(Turn);
    end;
  end;
  Result := True;
end; // TurnsLoadFromXml

class procedure TXmlReadWrite.TurnsSaveToXml(Turns: TList<TTurn>; XmlDoc: TXmlDocument; XmlNode: TDomNode);
var i: integer;
    NewNode: TDomElement;
begin
  for i := 0 to Turns.Count - 1 do begin
    NewNode := XmlDoc.CreateElement('Turn');
    TurnSaveToXml(Turns[i], NewNode);
    XmlNode.AppendChild(NewNode);
  end;
end; // TurnsSaveToXml

// ========================= Macro ===================================

class function TXmlReadWrite.MacroLoadFromXml(Macro: TMacro; XmlNode: TDomNode): Boolean;
begin
  Macro.Name := String(TDomElement(XmlNode).GetAttribute('Name'));
  Macro.Description := String(TDomElement(XmlNode).GetAttribute('Description'));
  Macro.Sequence := String(TDomElement(XmlNode).GetAttribute('Sequence'));

  Result := True;
end; // MacroLoadFromXml

class procedure TXmlReadWrite.MacroSaveToXml(Macro: TMacro; XmlNode: TDomNode);
begin
  TDomElement(XmlNode).SetAttribute('Name', AnsiString(Macro.Name));
  TDomElement(XmlNode).SetAttribute('Description', AnsiString(Macro.Description));
  TDomElement(XmlNode).SetAttribute('Sequence', AnsiString(Macro.Sequence));
end;

class function TXmlReadWrite.MacrosLoadFromXml(Macros: TList<TMacro>; XmlNode: TDomNode): Boolean;
var
  i: integer;
  Macro: TMacro;
  XmlSubNode: TDomNode;
  S: String;
begin // MacrosLoadFromXml
  for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
    XmlSubNode := XmlNode.ChildNodes[i];
    S := string(XmlSubNode.NodeName);
    if S = 'Macro' then begin
      Macro := TMacro.Create;
      if MacroLoadFromXml(Macro, XmlSubNode) and (TDomElement(XmlSubNode).GetAttribute('Public') = '1') then
        Macros.Add(Macro)
      else
        Macro.Free;
    end;
  end;

  Result := True;
end; // MacrosLoadFromXml

class procedure TXmlReadWrite.MacrosSaveToXml(Macros: TList<TMacro>; XmlDoc: TXmlDocument; XmlNode: TDomNode);
var i: integer;
    NewNode: TDomElement;
begin
  for i := 0 to Macros.Count - 1 do begin
    NewNode := XmlDoc.CreateElement('Macro');
    MacroSaveToXml(Macros[i], NewNode);
    XmlNode.AppendChild(NewNode);
  end;
end; // MacrosSaveToXml

class function TXmlReadWrite.PuzzleExecMacroFromXmlFile(Puzzle: TPuzzle; const FileName: String; MacroString: String): Boolean;
var lMacros: TDomNodeList;

  procedure ExecMacro(XmlNode: TDomNode); forward;

  procedure ExecMacroBody(XmlNode: TDomNode);
  var
    i: integer;
    XmlSubNode: TDomNode;
    S, Script: String;
  begin // ExecMacroBody
    for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
      XmlSubNode := XmlNode.ChildNodes[i];
      if SameText(XmlSubNode.NodeName, 'ExecMacro') then begin
        ExecMacro(XmlSubNode);
      end else if S = 'Script' then begin
        ReadStringFromXml(XmlSubNode, '', Script, '');
        ExecuteScript(Script);
      end else if S = 'Turn' then
        Turn(Puzzle, XmlSubNode);
    end;
  end; // ExecMacroBody

  procedure ExecMacro(XmlNode: TDomNode);
  var
    i, k, n: integer;
    MacroName, MacroNames, AttrName: String;
  begin
    MacroNames := Trim(TDomElement(XmlNode).AttribStrings['MacroName']);
    n := ReadIntExpressionFromXml(XmlNode, 'Repeat', 1);

    for i := 0 to XmlNode.Attributes.Length - 1 do begin
      AttrName := String(XmlNode.Attributes[i].NodeName);
      if (AttrName <> 'MacroName') and (AttrName <> 'Repeat') then
        ExecuteScript(AttrName + ':=' + TDomElement(XmlNode).GetAttribute(AttrName));
    end;

    while MacroNames <> '' do begin
      MacroName := Trim(TUtils.Parse(MacroNames, ' ' + #9#10#13));
      if MacroName <> '' then begin
        for i := lMacros.Count - 1 downto 0 do begin
          if SameStr(MacroName, Trim(TDomElement(lMacros[i]).GetAttribute('Name'))) then
            for k := 0 to n - 1 do
              ExecMacroBody(lMacros[i]);
        end;
      end;
    end;
  end; // ExecMacro

var
  XmlDoc: TXmlDocument;
  nodeMacros: TDomNode;
  i: integer;
  Found: Boolean;
  Macro: String;
  XmlSubNode: TDomNode;
begin // PuzzleExecMacroFromXmlFile
  Result := False;
  if not FileExists(FileName) then
    exit;

  // only 'public' macros allowed
//  Found := False;
//  for i := 0 to Puzzle.Macros.Count - 1 do
//    if SameText(MacroName, Puzzle.Macros[i].Name) then begin
//      Found := True;
//      break;
//    end;
//  if not Found then
//    exit;

//  XmlDoc := TXmlDocument.Create;
  ReadXMLFile(XmlDoc, FileName);
  try

    nodeMacros := XmlDoc.DocumentElement.FindNode('Macros');
    if nodeMacros = nil then
      exit;

    Result := True;
    for i := 0 to nodeMacros.ChildNodes.Count - 1 do begin
      XmlSubNode := nodeMacros.ChildNodes[i];
      if SameText(XmlSubNode.NodeName, 'Macro') then begin
        Macro := Trim(TUtils.Parse(MacroString, ' '));
        Found := False;
        if SameStr(Trim(TDomElement(XmlSubNode).GetAttribute('Name')), Macro) then begin
          Calc.ClearVars; // очищаем Script во избежание накладок от предыдущих скриптов
          Calc.ClearProcs;
          Calc.DefineConsts;
          ExecMacroBody(lMacros[i]);
          Found  := True;
        end;
      end;
      if not Found then begin
        Result := False;
        exit;
      end;
    end;

  finally
    XmlDoc.Free;
  end;
end; // PuzzleExecMacroFromXmlFile

// private
class procedure TXmlReadWrite.DecodeLayers(const LayersString: String; var Layers: array of Boolean);
var Splitted1, Splitted2: TArray<String>;
    S1: String;
    LayerFrom, LayerTo, i: integer;
begin
  if LayersString = '' then
    exit;
  Splitted1 := LayersString.Trim.Split([';']);
  for S1 in Splitted1 do begin
    Splitted2 := S1.Split(['-'], 2);
    if Length(Splitted2) = 1 then begin
      LayerFrom := Round(CalcExpression(Splitted2[0]));
      if (LayerFrom >= 0) and (LayerFrom < Length(Layers)) then
        Layers[LayerFrom] := True;
    end else if Length(Splitted2) = 2 then begin
      LayerFrom := Round(CalcExpression(Splitted2[0]));
      LayerTo := Round(CalcExpression(Splitted2[1]));
      if (LayerFrom >= 0) and (LayerFrom < Length(Layers)) and
         (LayerTo > 0) and (LayerTo < Length(Layers)) then
        for i := LayerFrom to LayerTo do
          Layers[i] := True;
    end;
  end;
end; // DecodeLayers

class procedure TXmlReadWrite.Transfer(Puzzle: TPuzzle; XmlNode: TDomNode);
var
  STransfer: string;
  VTransfer: TVector;
begin
  STransfer := TDomElement(XmlNode).GetAttribute('Vector');
  if STransfer <> '' then begin
    VTransfer := GetVectorFromString(STransfer);
    Puzzle.Trans(TMatrix.GetMoveMatrix(VTransfer));
  end;
end; // Transfer

class procedure TXmlReadWrite.Turn(Puzzle: TPuzzle; XmlNode: TDomNode);
var
  Angle : extended;
  SNorm, SBase: string;
  AxisNo, LayerFrom, LayerTo, Layer, i: integer;
  Layers: array of Boolean;
  IsLayersMove: Boolean;
  V, VS: TVector;
begin
  AxisNo := ReadIntExpressionFromXml(XmlNode, 'Axis');
  TSelection.TurnAxisNo := AxisNo;

  SetLength(Layers, Puzzle.Axes[AxisNo].Layers.Count);
  for i := 0 to High(Layers) do
    Layers[i] := False;

  LayerFrom := ReadIntExpressionFromXml(XmlNode, 'From', -1);
  LayerTo := ReadIntExpressionFromXml(XmlNode, 'To', -1);

  if (LayerFrom >= 0) and (LayerTo = -1) then
    LayerTo := Puzzle.Axes[AxisNo].Layers.Count;
  if (LayerTo >= 0) and (LayerFrom = -1) then
    LayerFrom := 0;
  if (LayerFrom >= 0) and (LayerTo >= LayerFrom) then
    for i := LayerFrom to LayerTo do
      Layers[i] := True;

  Layer := ReadIntExpressionFromXml(XmlNode, 'Layer', -1);
  if Layer >= 0 then
    Layers[Layer] := True;

  DecodeLayers(ReadStringFromXml(XmlNode, 'Layers'), Layers);

  Angle := ReadDoubleExpressionFromXml(XmlNode, 'Angle');

  IsLayersMove := False;
  for i := 0 to High(Layers) do
    IsLayersMove := IsLayersMove or Layers[i];

  if IsLayersMove then begin
//    Puzzle.CalcPartPositionsForAxis(AxisNo);
    Puzzle.CalcMovingLayers(AxisNo, Layers);
    Puzzle.MacroRotateLayers(AxisNo, TSelection.TurnLayers, Angle);
  end else begin
    SNorm := TDomElement(XmlNode).GetAttribute('NormVector');
    if SNorm <> '' then begin
      V := GetVectorFromString(SNorm);
      V.Normalize;

      SBase := TDomElement(XmlNode).GetAttribute('BaseVector');
      if SBase <> '' then begin
        VS := GetVectorFromString(SBase);
        Puzzle.Trans(TMatrix.GetMoveMatrix(-VS));
        Puzzle.Trans(TMatrix.GetRotateMatrix(V, Angle));
        Puzzle.Trans(TMatrix.GetMoveMatrix(VS));
      end else
        Puzzle.Trans(TMatrix.GetRotateMatrix(V, Angle));
    end;
  end;
end; // Turn

end.

