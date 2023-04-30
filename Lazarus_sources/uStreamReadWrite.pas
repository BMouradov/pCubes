{************************************************************}
{                                                            }
{  Module uStreamReadWrite                                   }
{  2014-2020                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uStreamReadWrite;

{$mode Delphi}

interface

uses uVector, uLine, uLayer, uAxis, uPart, uFace, uPuzzle, uTexture,
     uMacro, uSolvedState, uLibrary, uTurn,
     classes, Generics.Collections, Graphics;

const
  BINARY_VERSION: integer = 24;

type
  TStreamReadWrite = class
  private
    FStream: TStream;

    function ReadBoolean: Boolean;
    function ReadColor: TColor;
    function ReadExtended: extended;
    function ReadInteger: integer;
    function ReadString: string;
    function WriteBoolean(Value: Boolean): Boolean;
    function WriteColor(Value: TColor): Boolean;
    function WriteExtended(Value: extended): Boolean;
    function WriteInteger(Value: integer): Boolean;
    function WriteString(S: String): Boolean;

    procedure VectorLoadFromStream(var Vector: TVector);
    procedure VectorSaveToStream(Vector: TVector);
    procedure VectorsLoadFromStream(Vectors: TList<TVector>);
    procedure VectorsSaveToStream(Vectors: TList<TVector>);

    procedure LineLoadFromStream(var Line: TLine);
    procedure LineSaveToStream(const Line: TLine);

//    procedure PlanesLoadFromStream(Planes: TList<TPlane>);
//    procedure PlanesSaveToStream(Planes: TList<TPlane>);
//    procedure PlaneLoadFromStream(var Plane: TPlane);
//    procedure PlaneSaveToStream(const Plane: TPlane);

    procedure LayerLoadFromStream(Layer: TLayer);
    procedure LayerSaveToStream(Layer: TLayer);
    procedure LayersLoadFromStream(Layers: TList<TLayer>);
    procedure LayersSaveToStream(Layers: TList<TLayer>);

    procedure AxisLoadFromStream(Axis: TAxis);
    procedure AxisSaveToStream(Axis: TAxis);
    procedure AxesLoadFromStream(Axes: TList<TAxis>);
    procedure AxesSaveToStream(Axes: TList<TAxis>);

    procedure FaceLoadFromStream(Face: TFace);
    procedure FaceSaveToStream(Face: TFace);
    procedure FacesLoadFromStream(Part: TPart);
    procedure FacesSaveToStream(Part: TPart);

    procedure PartLoadFromStream(Part: TPart);
    procedure PartSaveToStream(Part: TPart);
    procedure PartsLoadFromStream(Parts: TList<TPart>);
    procedure PartsSaveToStream(Parts: TList<TPart>);

    procedure TextureLoadFromStream(Texture: TTexture);
    procedure TextureSaveToStream(Texture: TTexture);
    procedure TexturesLoadFromStream(Textures: TList<TTexture>);
    procedure TexturesSaveToStream(Textures: TList<TTexture>);

//    procedure MacroTurnLoadFromStream(MacroTurn: TMacroTurn);
//    procedure MacroTurnSaveToStream(MacroTurn: TMacroTurn);
//    procedure MacroTurnsLoadFromStream(MacroTurns: TList<TMacroTurn>);
//    procedure MacroTurnsSaveToStream(MacroTurns: TList<TMacroTurn>);
    procedure MacroLoadFromStream(Macro: TMacro);
    procedure MacroSaveToStream(Macro: TMacro);
    procedure MacrosLoadFromStream(Macros: TList<TMacro>);
    procedure MacrosSaveToStream(Macros: TList<TMacro>);

    procedure TurnLoadFromStream(Turn: TTurn);
    procedure TurnSaveToStream(Turn: TTurn);
    procedure TurnsLoadFromStream(Turns: TList<TTurn>);
    procedure TurnsSaveToStream(Turns: TList<TTurn>);

//    procedure TurningLoadFromStream(Turning: TUndoTurning); // удалить
//    procedure TurningSaveToStream(Turning: TUndoTurning); // удалить
//    procedure TurningsLoadFromStream(Turnings: TList<TUndoTurning>); // удалить
//    procedure TurningsSaveToStream(Turnings: TList<TUndoTurning>); // удалить

    procedure MarksLoadFromStream(Marks: TList<integer>);
    procedure MarksSaveToStream(Marks: TList<integer>);

    procedure FacePropertiesLoadFromStream(FaceProperties: TFaceProperties);
    procedure FacePropertiesSaveToStream(FaceProperties: TFaceProperties);
    procedure SolvedStateLoadFromStream(SolvedState: TSolvedState);
    procedure SolvedStateSaveToStream(SolvedState: TSolvedState);
    procedure SolvedStatesLoadFromStream(SolvedStates: TList<TSolvedState>);
    procedure SolvedStatesSaveToStream(SolvedStates: TList<TSolvedState>);

    function  PuzzleCheckVersionFromStream(Puzzle: TPuzzle): Boolean;
    procedure PuzzleSaveHeaderToStream(Puzzle: TPuzzle);

    procedure LibraryItemLoadFromStream(LibraryItem: TLibraryItem);
    procedure LibraryItemSaveToStream(LibraryItem: TLibraryItem);

  public
//    property Puzzle: TPuzzle write FPuzzle;

    constructor Create(Stream: TStream);
//    function LoadHeaderFromXmlFile(const FileName: String): Boolean;
//    function LoadVectorsFromXml(var Vectors: TVectorsV; XmlNode: IXmlNode): Boolean;

    procedure PuzzleLoadHeaderFromStream(Puzzle: TPuzzle);
    procedure PuzzleLoadFromStream(Puzzle: TPuzzle);
    procedure PuzzleSaveToStream(Puzzle: TPuzzle);

    procedure LibraryLoadFromStream(Puzzles: TLibrary);
    procedure LibrarySaveToStream(Puzzles: TLibrary);
  end;

implementation

uses
  uVectorImage,
  SysUtils;

const ERROR_MESSAGE = 'Read from stream error';

constructor TStreamReadWrite.Create(Stream: TStream);
begin
  FStream := Stream;
end;

function TStreamReadWrite.ReadExtended: extended;
begin
  if FStream.Read(Result, SizeOf(Result)) <= 0 then
    raise Exception.Create(ERROR_MESSAGE);
end;

function TStreamReadWrite.WriteExtended(Value: extended): Boolean;
begin
  Result := FStream.Write(Value, SizeOf(Value)) > 0;
end;

function TStreamReadWrite.ReadInteger: integer;
begin
  if FStream.Read(Result, SizeOf(Result)) <= 0 then
    raise Exception.Create(ERROR_MESSAGE);
end;

function TStreamReadWrite.WriteInteger(Value: integer): Boolean;
begin
  Result := FStream.Write(Value, SizeOf(Value)) > 0;
end;

function TStreamReadWrite.ReadBoolean: Boolean;
begin
  if FStream.Read(Result, SizeOf(Result)) <= 0 then
    raise Exception.Create(ERROR_MESSAGE);
end;

function TStreamReadWrite.WriteBoolean(Value: Boolean): Boolean;
begin
  Result := FStream.Write(Value, SizeOf(Value)) > 0;
end;

function TStreamReadWrite.ReadColor: TColor;
begin
  if FStream.Read(Result, SizeOf(Result)) <= 0 then
    raise Exception.Create(ERROR_MESSAGE);
end;

function TStreamReadWrite.WriteColor(Value: TColor): Boolean;
begin
  Result := FStream.Write(Value, SizeOf(Value)) > 0;
end;

function TStreamReadWrite.ReadString: string;
var n: integer;
begin
  FStream.Read(n, sizeof(n));
  SetLength(Result, n div sizeOf(Char));
  if n > 0 then
    FStream.Read(PChar(Result)^, n);
end; // ReadStr

function TStreamReadWrite.WriteString(S: String): Boolean;
var n: integer;
begin
  n := Length(S) * SizeOf(Char);
  Result := (FStream.Write(n, SizeOf(n)) > 0);
  if Result and (n > 0) then
    Result := Result and (FStream.Write(PChar(S)^, n) > 0);
end;

// ================================ Vector =====================================

procedure TStreamReadWrite.VectorLoadFromStream(var Vector: TVector);
begin
  Vector.x := ReadExtended;
  Vector.y := ReadExtended;
  Vector.z := ReadExtended;
end;

procedure TStreamReadWrite.VectorSaveToStream(Vector: TVector);
begin
  WriteExtended(Vector.x);
  WriteExtended(Vector.y);
  WriteExtended(Vector.z);
end;

procedure TStreamReadWrite.VectorsLoadFromStream(Vectors: TList<TVector>);
var
  i, n: integer;
  V: TVector;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    VectorLoadFromStream(V);
    Vectors.Add(V);
  end;
end;

procedure TStreamReadWrite.VectorsSaveToStream(Vectors: TList<TVector>);
var n, i: integer;
begin
  n := Vectors.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    VectorSaveToStream(Vectors[i]);
end;

// ================================ Line =====================================

procedure TStreamReadWrite.LineLoadFromStream(var Line: TLine);
begin
  VectorLoadFromStream(Line.BaseVector);
  VectorLoadFromStream(Line.NormVector);
end;

procedure TStreamReadWrite.LineSaveToStream(const Line: TLine);
begin
  VectorSaveToStream(Line.BaseVector);
  VectorSaveToStream(Line.NormVector);
end;

// ================================ Plane =====================================

//procedure TStreamReadWrite.PlaneLoadFromStream(var Plane: TPlane);
//begin
//  VectorLoadFromStream(Plane.NormVect);
//  Plane.NormDistance := ReadExtended;
//end;
//
//procedure TStreamReadWrite.PlaneSaveToStream(const Plane: TPlane);
//begin
//  VectorSaveToStream(Plane.NormVect);
//  WriteExtended(Plane.NormDistance);
//end;

//procedure TStreamReadWrite.PlanesLoadFromStream(Planes: TList<TPlane>);
//var
//  i, n: integer;
//  Plane: TPlane;
//begin
//  n := ReadInteger;
//  for i := 0 to n - 1 do begin
//    PlaneLoadFromStream(Plane);
//    Planes.Add(Plane);
//  end;
//end; // PlanesLoadFromStream
//
//procedure TStreamReadWrite.PlanesSaveToStream(Planes: TList<TPlane>);
//var n, i: integer;
//begin
//  n := Planes.Count;
//  WriteInteger(n);
//  for i := 0 to n - 1 do
//    PlaneSaveToStream(Planes[i]);
//end;

// ================================ Layer =====================================

procedure TStreamReadWrite.LayerLoadFromStream(Layer: TLayer);
var
  i, n: integer;
begin
  Layer.DistanceFrom := ReadExtended;
  Layer.DistanceTo   := ReadExtended;
  Layer.TurningAngle := ReadExtended;
  Layer.Fixed        := ReadBoolean;
  Layer.Angle        := ReadExtended;

  n := ReadInteger;
  SetLength(Layer.AvailableAngles, n);
  for i := 0 to n - 1 do
    Layer.AvailableAngles[i] := ReadExtended;

  Layer.ConnectedLayer := ReadInteger;

  // Constrained
//  Layer.IsConstrained := ReadBoolean;
  Layer.MinAngle      := ReadExtended;
  Layer.MaxAngle      := ReadExtended;
end; // LayerLoadFromStream

procedure TStreamReadWrite.LayerSaveToStream(Layer: TLayer);
var i, n: integer;
begin
  WriteExtended(Layer.DistanceFrom);
  WriteExtended(Layer.DistanceTo);
  WriteExtended(Layer.TurningAngle);
  WriteBoolean (Layer.Fixed);
  WriteExtended(Layer.Angle);

  n := Length(Layer.AvailableAngles);
  WriteInteger(n);
  for i := 0 to n - 1 do
    WriteExtended(Layer.AvailableAngles[i]);
  WriteInteger(Layer.ConnectedLayer);

  // Constrained
//  WriteBoolean (Layer.IsConstrained);
  WriteExtended(Layer.MinAngle);
  WriteExtended(Layer.MaxAngle);
end; // LayerSaveToStream

procedure TStreamReadWrite.LayersLoadFromStream(Layers: TList<TLayer>);
var
  i, n: integer;
  Layer: TLayer;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    Layer := TLayer.Create(0, 0);
    LayerLoadFromStream(Layer);
    Layers.Add(Layer);
  end;
end; // LayersLoadFromStream

procedure TStreamReadWrite.LayersSaveToStream(Layers: TList<TLayer>);
var Layer: TLayer;
begin
  WriteInteger(Layers.Count);
  for Layer in Layers do
    LayerSaveToStream(Layer);
end; // LayersSaveToStream

// ================================ Axis =====================================

procedure TStreamReadWrite.AxisLoadFromStream(Axis: TAxis);
var i, j, n: integer;
//    Distances: array of extended;
begin
  LineLoadFromStream(Axis.Line);
  LayersLoadFromStream(Axis.Layers);
  Axis.TurnAxesWithLayer := ReadInteger;
  Axis.TurnAxesWithPartNo := ReadInteger;

//  n := ReadInteger;
////  SetLength(Axis.Distances, n);
////  for i := 0 to n - 1 do
////    Axis.Distances[i] := ReadExtended;
//  SetLength(Distances, n);
//  for i := 0 to n - 1 do
//    Distances[i] := ReadExtended;
////  Crea
//
  n := ReadInteger;
  SetLength(Axis.ConnectedLayers, n);
  for i := 0 to Length(Axis.ConnectedLayers) - 1 do begin
    n := ReadInteger;
    SetLength(Axis.ConnectedLayers[i], n);
    for j := 0 to Length(Axis.ConnectedLayers[i]) - 1 do
      Axis.ConnectedLayers[i][j] := ReadInteger;
  end;

end; // LoadFromStream

// valid for FlatLayers
procedure TStreamReadWrite.AxisSaveToStream(Axis: TAxis);
var i, j, n: integer;
//    Layer: TLayer;
begin
  LineSaveToStream(Axis.Line);
  LayersSaveToStream(Axis.Layers);
  WriteInteger(Axis.TurnAxesWithLayer);
  WriteInteger(Axis.TurnAxesWithPartNo);

//  WriteInteger(Length(Axis.Distances));
//  for i := 0 to Length(Axis.Distances) - 1 do
//    WriteExtended(Axis.Distances[i]);
//  WriteInteger(Axis.Layers.Count - 1);
//  for i := 0 to Axis.Layers.Count - 2 do
//    WriteExtended(Axis.Layers[i].DistanceTo);

  n := Length(Axis.ConnectedLayers);
  WriteInteger(n);
  for i := 0 to Length(Axis.ConnectedLayers) - 1 do begin
    n := Length(Axis.ConnectedLayers[i]);
    WriteInteger(n);
    for j := 0 to Length(Axis.ConnectedLayers[i]) - 1 do
      WriteInteger(Axis.ConnectedLayers[i][j]);
  end;
end; // SaveToStream

procedure TStreamReadWrite.AxesLoadFromStream(Axes: TList<TAxis>);
var
  i, n: integer;
  Axis: TAxis;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    Axis := TAxis.Create;
    AxisLoadFromStream(Axis);
    Axes.Add(Axis);
  end;
end;

procedure TStreamReadWrite.AxesSaveToStream(Axes: TList<TAxis>);
var i, n: integer;
begin
  n := Axes.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    AxisSaveToStream(Axes[i]);
end;

// ================================ Face =====================================

procedure TStreamReadWrite.FaceLoadFromStream(Face: TFace);
var i, n: integer;
begin
  //Face.Clear;
  Face.Color := ReadColor;
  Face.IsStickerVisible := ReadBoolean;

  // delete
  n := ReadInteger;
  for i := 0 to n - 1 do
    Face.AddVertexByIndex(ReadInteger);

  for i := 0 to n - 1 do
    Face.SetEdgeVisibility(i, ReadBoolean);

  Face.TextureId := ReadInteger;
end; // FaceLoadFromStream

procedure TStreamReadWrite.FaceSaveToStream(Face: TFace);
var i, n: integer;
begin
  WriteColor(Face.Color);
  WriteBoolean(Face.IsStickerVisible);

  n := Face.GetVertexCount;
  WriteInteger(n);
  for i := 0 to n - 1 do
    WriteInteger(Face.GetVertexIndex(i));
  for i := 0 to n - 1 do
    WriteBoolean(Face.IsEdgeVisible(i));

  WriteInteger(Face.TextureId);
end; // FaceSaveToStream

procedure TStreamReadWrite.FacesLoadFromStream(Part: TPart);
var
  i, n: integer;
  Face: TFace;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    Face := TFace.Create(Part.Vertices);
    FaceLoadFromStream(Face);
    Part.Faces.Add(Face);
  end;
end;

procedure TStreamReadWrite.FacesSaveToStream(Part: TPart);
var i, n: integer;
begin
  n := Part.Faces.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    FaceSaveToStream(Part.Faces[i]);
end;


procedure TStreamReadWrite.PartLoadFromStream(Part: TPart);
begin
  VectorsLoadFromStream(Part.Vertices);
  FacesLoadFromStream(Part);
  Part.Visible := ReadBoolean;
end; // LoadFromStream

procedure TStreamReadWrite.PartSaveToStream(Part: TPart);
begin
  VectorsSaveToStream(Part.Vertices);
  FacesSaveToStream(Part);
  WriteBoolean(Part.Visible);
end; // SaveToStream

procedure TStreamReadWrite.PartsLoadFromStream(Parts: TList<TPart>);
var
  i, n: integer;
  Part: TPart;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    Part := TPart.Create;
    PartLoadFromStream(Part);
    Parts.Add(Part);
  end;
end; // LoadFromStream

procedure TStreamReadWrite.PartsSaveToStream(Parts: TList<TPart>);
var i, n: integer;
begin
  n := Parts.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    PartSaveToStream(Parts[i]);
end; // SaveToStream

function TStreamReadWrite.PuzzleCheckVersionFromStream(Puzzle: TPuzzle): Boolean;
begin
  Result := ReadInteger = BINARY_VERSION;
//  if not Result then
//    ShowMessage('Wrong version of file ' + FileName);
end; // CheckVersionFromBinary

// See also LibraryItemLoadFromStream
procedure TStreamReadWrite.PuzzleLoadHeaderFromStream(Puzzle: TPuzzle);
begin
  Puzzle.ClearHeader;

  if not PuzzleCheckVersionFromStream(Puzzle) then
    exit;

  with Puzzle.Header do begin
    Name        := ReadString;
    Aliases     := ReadString;
    ClassString := ReadString;
    MenuString  := ReadString;
    Inventor    := ReadString;
    Programmer  := ReadString;
    Added       := ReadString;
    Link        := ReadString;
    FileName    := ReadString;
  end;
end; // LoadHeaderFromStream

procedure TStreamReadWrite.PuzzleSaveHeaderToStream(Puzzle: TPuzzle);
begin
  WriteInteger(BINARY_VERSION);
  with Puzzle.Header do begin
    WriteString(Name);
    WriteString(Aliases);
    WriteString(Puzzle.ClassName);
    WriteString(MenuString);
    WriteString(Inventor);
    WriteString(Programmer);
    WriteString(Added);
    WriteString(Link);
    WriteString(FileName);
  end;
end; // SaveHeaderToStream

procedure TStreamReadWrite.PuzzleLoadFromStream(Puzzle: TPuzzle);
begin
  PuzzleLoadHeaderFromStream(Puzzle);
  PartsLoadFromStream(Puzzle.Parts);
  AxesLoadFromStream(Puzzle.Axes);
  MacrosLoadFromStream(Puzzle.Macros);
  TexturesLoadFromStream(Puzzle.Textures);
  Puzzle.CheckSolvedProc := TCheckSolvedProc(ReadInteger);
  Puzzle.SolvedStates.UniquePlanes := ReadInteger;
//  PlanesLoadFromStream(Puzzle.BspPlanesPermanent);
//  PlanesLoadFromStream(Puzzle.BspPlanesExtra);
  SolvedStatesLoadFromStream(Puzzle.SolvedStates.SolvedStates);
//  TurningsLoadFromStream(Puzzle.UndoStack);
//  TurningsLoadFromStream(Puzzle.RedoStack);
  TurnsLoadFromStream(Puzzle.UndoStack);
  TurnsLoadFromStream(Puzzle.RedoStack);
  MarksLoadFromStream(Puzzle.Marks);

  Puzzle.TimerValue := ReadInteger;
  Puzzle.MovesStartingPoint := ReadInteger;
//  Puzzle.UseBspEx := ReadBoolean;
  Puzzle.AfterLoadFromStreamEvent(FStream);
end; // LoadFromStream

procedure TStreamReadWrite.PuzzleSaveToStream(Puzzle: TPuzzle);
begin // SaveToStream
  PuzzleSaveHeaderToStream(Puzzle);
  PartsSaveToStream(Puzzle.Parts);
  AxesSaveToStream(Puzzle.Axes);
  MacrosSaveToStream(Puzzle.Macros);
  TexturesSaveToStream(Puzzle.Textures);
  WriteInteger(Ord(Puzzle.CheckSolvedProc));
  WriteInteger(Puzzle.SolvedStates.UniquePlanes);
//  PlanesSaveToStream(Puzzle.BspPlanesPermanent);
//  PlanesSaveToStream(Puzzle.BspPlanesExtra);
  SolvedStatesSaveToStream(Puzzle.SolvedStates.SolvedStates);
//  TurningsSaveToStream(Puzzle.UndoStack);
//  TurningsSaveToStream(Puzzle.RedoStack);
  TurnsSaveToStream(Puzzle.UndoStack);
  TurnsSaveToStream(Puzzle.RedoStack);
  MarksSaveToStream(Puzzle.Marks);

  WriteInteger(Puzzle.TimerValue);
  WriteInteger(Puzzle.MovesStartingPoint);
//  WriteBoolean(Puzzle.UseBspEx);
  Puzzle.AfterSaveToStreamEvent(FStream);
end; // SaveToStream

procedure TStreamReadWrite.TextureLoadFromStream(Texture: TTexture);
begin
//  Clear;
  Texture.Name := ReadString;
  Texture.FileName := ReadString;

//  if not FileExists(Texture.FileName) then begin
//    //ShowMessage('File ' + Texture.FileName + ' not found');
//    exit;
//  end;
//
//  if (Pos('.vec', LowerCase(Texture.FileName)) <> 0) and (FileExists(Texture.FileName)) then begin
//    Texture.VectorImage := TVectorImage.Create;
//    Texture.VectorImage.LoadFromFile(Texture.FileName);
//  end;
end;

procedure TStreamReadWrite.TextureSaveToStream(Texture: TTexture);
begin

//  with TVectorImage(Self) do begin
    WriteString(Texture.Name);
    WriteString(Texture.FileName);
//    (Stream.Read(Width, SizeOf(Width)) > 0);
//    (Stream.Read(Height, SizeOf(Height)) > 0);

//    VectorCommands.SaveToStream;
//  end;
end;

procedure TStreamReadWrite.TexturesLoadFromStream(Textures: TList<TTexture>);
var
  i, n: integer;
  Texture: TTexture;
begin
  for i := 0 to Textures.Count - 1 do
    Textures[i].Free;
  Textures.Clear;
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    Texture := TTexture.Create;
    TextureLoadFromStream(Texture);
    Textures.Add(Texture);
  end;
end; // TexturesLoadFromStream

procedure TStreamReadWrite.TexturesSaveToStream(Textures: TList<TTexture>);
var i, n: integer;
begin
  n := Textures.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    TextureSaveToStream(Textures[i]);
end; // SaveToStream

//procedure TStreamReadWrite.MacroTurnLoadFromStream(MacroTurn: TMacroTurn);
//begin
//  AxisLoadFromStream(MacroTurn.Axis);
//  MacroTurn.Angle := ReadExtended;
//  MacroTurn.LayerFrom := ReadInteger;
//  MacroTurn.LayerTo := ReadInteger;
//end; // LoadFromStream
//
//procedure TStreamReadWrite.MacroTurnSaveToStream(MacroTurn: TMacroTurn);
//begin
//  AxisSaveToStream(MacroTurn.Axis);
//  WriteExtended(MacroTurn.Angle);
//  WriteInteger(MacroTurn.LayerFrom);
//  WriteInteger(MacroTurn.LayerTo);
//end; // SaveToStream
//
//procedure TStreamReadWrite.MacroTurnsLoadFromStream(MacroTurns: TList<TMacroTurn>);
//var
//  i, n: integer;
//  MacroTurn: TMacroTurn;
//begin
//  MacroTurns.Clear;
//  n := ReadInteger;
//  for i := 0 to n - 1 do begin
//    MacroTurn := TMacroTurn.Create;
//    MacroTurnLoadFromStream(MacroTurn);
//    MacroTurns.Add(MacroTurn);
//  end;
//end; // LoadFromStream
//
//procedure TStreamReadWrite.MacroTurnsSaveToStream(MacroTurns: TList<TMacroTurn>);
//var i, n: integer;
//begin
//  n := MacroTurns.Count;
//  WriteInteger(n);
//  for i := 0 to n - 1 do
//    MacroTurnSaveToStream(MacroTurns[i]);
//end; // SaveToStream
//
procedure TStreamReadWrite.MacroLoadFromStream(Macro: TMacro);
begin
//  Macro.Clear;
  Macro.Name := ReadString;
  Macro.Description := ReadString;
//  Macro.Public := ReadBoolean;
  Macro.Sequence := ReadString;
//  MacroTurnsLoadFromStream(Macro.Turns);
end; // LoadFromStream

procedure TStreamReadWrite.MacroSaveToStream(Macro: TMacro);
begin
  WriteString(Macro.Name);
  WriteString(Macro.Description);
//  WriteBoolean(Macro.Public);
  WriteString(Macro.Sequence);
//  MacroTurnsSaveToStream(Macro.Turns);
end; // SaveToStream

procedure TStreamReadWrite.MacrosLoadFromStream(Macros: TList<TMacro>);
var
  i, n: integer;
  Macro: TMacro;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    Macro := TMacro.Create;
    MacroLoadFromStream(Macro);
    Macros.Add(Macro);
  end;
end; // MacrosLoadFromStream

procedure TStreamReadWrite.MacrosSaveToStream(Macros: TList<TMacro>);
var i, n: integer;
begin
  n := Macros.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    MacroSaveToStream(Macros[i]);
end; // MacrosSaveToStream

procedure TStreamReadWrite.TurnLoadFromStream(Turn: TTurn);
var i: integer;
begin
  Turn.typ := TTurningType(ReadInteger);
  Turn.AxisNo := ReadInteger;
  SetLength(Turn.Layers, ReadInteger);
  for i := 0 to High(Turn.Layers) do
    Turn.Layers[i] := ReadBoolean;

  case Turn.typ of
    ttyLayerTurn: Turn.Angle := ReadExtended;
    ttyPartsMove: Turn.Distance := ReadExtended;
  end;
end; // TurningLoadFromStream

procedure TStreamReadWrite.TurnSaveToStream(Turn: TTurn);
var i: integer;
begin
  WriteInteger(Ord(Turn.typ));
  WriteInteger(Turn.AxisNo);
  WriteInteger(Length(Turn.Layers));
  for i := 0 to High(Turn.Layers) do
    WriteBoolean(Turn.Layers[i]);

  case Turn.typ of
    ttyLayerTurn: WriteExtended(Turn.Angle);
    ttyPartsMove: WriteExtended(Turn.Distance);
  end;
end; // TurnSaveToStream

procedure TStreamReadWrite.TurnsLoadFromStream(Turns: TList<TTurn>);
var
  i, n: integer;
  Turn: TTurn;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    Turn := TTurn.Create;
    TurnLoadFromStream(Turn);
    Turns.Add(Turn);
  end;
end; // TurnsLoadFromStream

procedure TStreamReadWrite.TurnsSaveToStream(Turns: TList<TTurn>);
var i, n: integer;
begin
  n := Turns.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    TurnSaveToStream(Turns[i]);
end;

//procedure TStreamReadWrite.TurningLoadFromStream(Turning: TUndoTurning);
//begin
//  Turning.AxisNo := ReadInteger;
//  Turning.LayerFrom := ReadInteger;
//  Turning.LayerTo := ReadInteger;
//  Turning.Angle := ReadExtended;
//end; // TurningLoadFromStream
//
//procedure TStreamReadWrite.TurningSaveToStream(Turning: TUndoTurning);
//begin
//  WriteInteger(Turning.AxisNo);
//  WriteInteger(Turning.LayerFrom);
//  WriteInteger(Turning.LayerTo);
//  WriteExtended(Turning.Angle);
//end; // TurningSaveToStream
//
//procedure TStreamReadWrite.TurningsLoadFromStream(Turnings: TList<TUndoTurning>);
//var
//  i, n: integer;
//  Turning: TUndoTurning;
//begin
//  n := ReadInteger;
//  for i := 0 to n - 1 do begin
//    Turning := TUndoTurning.Create;
//    TurningLoadFromStream(Turning);
//    Turnings.Add(Turning);
//  end;
//end;
//
//procedure TStreamReadWrite.TurningsSaveToStream(Turnings: TList<TUndoTurning>);
//var i, n: integer;
//begin
//  n := Turnings.Count;
//  WriteInteger(n);
//  for i := 0 to n - 1 do
//    TurningSaveToStream(Turnings[i]);
//end;
//
procedure TStreamReadWrite.MarksLoadFromStream(Marks: TList<integer>);
var
  i, n: integer;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do
    Marks.Add(ReadInteger);
end;

procedure TStreamReadWrite.MarksSaveToStream(Marks: TList<integer>);
var i, n: integer;
begin
  n := Marks.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    WriteInteger(Marks[i]);
end;

procedure TStreamReadWrite.FacePropertiesLoadFromStream(FaceProperties: TFaceProperties);
begin
  VectorLoadFromStream(FaceProperties.vc);
  VectorLoadFromStream(FaceProperties.vn);
  FaceProperties.ColorIndex := ReadInteger;
end;

procedure TStreamReadWrite.FacePropertiesSaveToStream(FaceProperties: TFaceProperties);
begin
  VectorSaveToStream(FaceProperties.vc);
  VectorSaveToStream(FaceProperties.vn);
  WriteInteger(FaceProperties.ColorIndex);
end;

procedure TStreamReadWrite.SolvedStateLoadFromStream(SolvedState: TSolvedState);
var
  i, n: integer;
  fp: TFaceProperties;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    fp := TFaceProperties.Create;
    FacePropertiesLoadFromStream(fp);
    SolvedState.FacesProperties.Add(fp);
  end;
end; // SolvedStateLoadFromStream

procedure TStreamReadWrite.SolvedStateSaveToStream(SolvedState: TSolvedState);
var n, i: integer;
begin
  n := SolvedState.FacesProperties.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    FacePropertiesSaveToStream(SolvedState.FacesProperties[i]);
end;

procedure TStreamReadWrite.SolvedStatesLoadFromStream(SolvedStates: TList<TSolvedState>);
var
  i, n: integer;
  SolvedState: TSolvedState;
begin
  n := ReadInteger;
  for i := 0 to n - 1 do begin
    SolvedState := TSolvedState.Create;
    SolvedStateLoadFromStream(SolvedState);
    SolvedStates.Add(SolvedState);
  end;
end;

procedure TStreamReadWrite.SolvedStatesSaveToStream(SolvedStates: TList<TSolvedState>);
var i, n: integer;
begin
  n := SolvedStates.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    SolvedStateSaveToStream(SolvedStates[i]);
end;

procedure TStreamReadWrite.LibraryItemLoadFromStream(LibraryItem: TLibraryItem);
begin
  LibraryItem.PuzzleClass := TLibrary.GetPuzzleClassByName(ReadString);
  LibraryItem.Name        := ReadString;
  LibraryItem.Aliases     := ReadString;
  LibraryItem.MenuString  := ReadString;
  LibraryItem.FileName    := ReadString;
  LibraryItem.FileAge     := ReadExtended;
  LibraryItem.FileSize    := ReadInteger;
  LibraryItem.Inventor    := ReadString;
  LibraryItem.Programmer  := ReadString;
  LibraryItem.Added       := ReadString;
  LibraryItem.Link        := ReadString;
end; // LoadFromStream

procedure TStreamReadWrite.LibraryItemSaveToStream(LibraryItem: TLibraryItem);
begin
  WriteString  (LibraryItem.PuzzleClass.ClassName);
  WriteString  (LibraryItem.Name);
  WriteString  (LibraryItem.Aliases);
  WriteString  (LibraryItem.MenuString);
  WriteString  (LibraryItem.FileName);
  WriteExtended(LibraryItem.FileAge);
  WriteInteger (LibraryItem.FileSize);
  WriteString  (LibraryItem.Inventor);
  WriteString  (LibraryItem.Programmer);
  WriteString  (LibraryItem.Added);
  WriteString  (LibraryItem.Link);
end; // SaveToStream

procedure TStreamReadWrite.LibraryLoadFromStream(Puzzles: TLibrary);
var
  i, n: integer;
  LibraryItem: TLibraryItem;
begin
  if ReadInteger <> BINARY_VERSION then
    exit;
//    raise Exception.Create('Error loading library: wrong version');

  n := ReadInteger;
  for i := 0 to n - 1 do begin
    LibraryItem := TLibraryItem.Create;
    LibraryItemLoadFromStream(LibraryItem);
    LibraryItem.Id := Puzzles.Add(LibraryItem);
  end;
end;

procedure TStreamReadWrite.LibrarySaveToStream(Puzzles: TLibrary);
var i, n: integer;
begin
  WriteInteger(BINARY_VERSION);

  n := Puzzles.Count;
  WriteInteger(n);
  for i := 0 to n - 1 do
    LibraryItemSaveToStream(Puzzles[i]);
end;

end.
