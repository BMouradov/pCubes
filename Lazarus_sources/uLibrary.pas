{************************************************************}
{                                                            }
{  Unit uLibrary                                             }
{  2015-2023                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uLibrary;
{$mode Delphi}

interface
uses uPuzzle, Classes, DOM, Generics.Collections;

const
  CacheFileName = 'Puzzles.cache';

type TPuzzleClass = class of TPuzzle;

type
  TSortType = (
    SORT_BY_NAME,
    SORT_BY_ALIASES,
    SORT_BY_CLASS,
    SORT_BY_MENU,
    SORT_BY_FILE,
    SORT_BY_INVERTOR,
    SORT_BY_PROGRAMMER,
    SORT_BY_ADDED
  );

type
  TLibraryItem = class
    Id: integer;
    PuzzleClass: TPuzzleClass;
    Name: String;
    Aliases: String;
    MenuString: String;
    FileName: String;
    FileAge: TDateTime;
    FileSize: integer;
    Inventor: String;
    Programmer: String;
    Added: String;
    Link: String;

    procedure Assign(Item: TLibraryItem); overload;
//    procedure Assign(Puzzle: TPuzzle); overload;
    function MatchFilter(Filter: String): Boolean;
    procedure SaveHeaderToXml(XmlDoc: TXMLDocument; XmlNode: TDomNode);
    procedure LoadHeaderFromXml(XmlNode: TDomNode);
  end;

  TLibraryClassItem = class
    PuzzleClass: TPuzzleClass;
  end;

type
  TLibrary = class(Classes.TList)
  private
    fFilteredItems: TList<TLibraryItem>;
    fLibraryClasses: TList<TLibraryClassItem>;
    fSortMode: TSortType;
    fFilter: String;

    function Get(Index: Integer): TLibraryItem;
    procedure SetFilter(Text: String);
    function GetFiltered(Index: Integer): TLibraryItem;

    procedure RegisterPuzzle(Puzzle: TPuzzle; FileName: String; FileAge: TDateTime; FileSize: integer); overload;
    procedure RegisterPuzzle(LibraryItem: TLibraryItem); overload;
    class function EqualPuzzles(Puzzle1: TLibraryItem; Puzzle2: TPuzzle): Boolean;
    class procedure PuzzleLoadHeaderFromStreamFile(Puzzle: TPuzzle; const FileName: String); static;
    class procedure PuzzleLoadFromStreamFile(Puzzle: TPuzzle; const FileName: String); static;
  public
    class var Puzzles: TLibrary;
    class procedure RegisterPuzzleClass(PuzzleClass: TPuzzleClass);
    class function GetPuzzleClassByName(AClassName: String): TPuzzleClass;
    class function PuzzleLoadHeaderFromFile(Puzzle: TPuzzle; const FileName: string): Boolean;
    class function PuzzleLoadFromFile(Puzzle: TPuzzle; const FileName: String): Boolean;
    class procedure PuzzleSaveToStreamFile(Puzzle: TPuzzle; const FileName: String); static;
    procedure SaveAllPuzzlesToStream(const FileName: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

    property Items[Index: Integer]: TLibraryItem read Get; default;
    property Filtered[Index: Integer]: TLibraryItem read GetFiltered;

    property SortMode: TSortType read fSortMode;
    property Filter: String read fFilter write SetFilter;

    function FilteredCount: integer;
    function GetById(Id: integer): TLibraryItem;
    procedure Sort(Mode: TSortType);

    procedure Load;

    function LoadFromBinaryFile(const FileName: String): Boolean;
    function SaveToBinaryFile(const FileName: String): Boolean;
    procedure SaveToXmlFile(const FileName: String);
    procedure LoadFromCompressedXmlFile(const FileName: String);

    function FindPuzzle(Puzzle: TPuzzle): integer;
    function FindPuzzleInFiltered(Puzzle: TPuzzle): integer;
  end;

implementation
uses
  SysUtils,
//IOUtils, Types,
  uUtils, uXmlReadWrite, uStreamReadWrite, uTexture,
  uXmlUtils, XMLWrite, XMLRead;

constructor TLibrary.Create;
//var Dirs: TStringDynArray;
//S: String;
begin
  inherited;
  fFilteredItems := TList<TLibraryItem>.Create;
  fLibraryClasses := TList<TLibraryClassItem>.Create;
  fSortMode := SORT_BY_MENU;


//  if TDirectory.Exists('figures1') then
//    TUtils.Log('figures exists')
//  else
//    TUtils.Log('figures not exists');

//  Dirs := TDirectory.GetDirectories('.', 'O*');
//  for S in Dirs do
//    TUtils.Log(S);

end;

destructor TLibrary.Destroy;
begin
  Clear;
  FreeAndNil(fFilteredItems);
  FreeAndNil(fLibraryClasses);
  inherited;
end;

procedure TLibrary.Clear;
var i: integer;
begin
  if fFilteredItems <> nil then
    fFilteredItems.Clear;

  if fLibraryClasses <> nil then begin
    for i := fLibraryClasses.Count - 1 downto 0 do
      TLibraryClassItem(fLibraryClasses[i]).Free;
    fLibraryClasses.Clear;
  end;

  for i := Count - 1 downto 0 do
    get(i).Free;

  inherited;
end;

function TLibrary.FilteredCount: integer;
begin
  Result := fFilteredItems.Count;
end;

class function TLibrary.EqualPuzzles(Puzzle1: TLibraryItem; Puzzle2: TPuzzle): Boolean;
begin
  Result := (Puzzle1.PuzzleClass = Puzzle2.ClassType) and (Puzzle1.Name = Puzzle2.Header.Name);
end;

function TLibrary.FindPuzzle(Puzzle: TPuzzle): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Puzzles.Count - 1 do
    if EqualPuzzles(Puzzles[i], Puzzle) then begin
      Result := i;
      exit;
    end
end; // FindPuzzle

function TLibrary.FindPuzzleInFiltered(Puzzle: TPuzzle): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Puzzles.FilteredCount - 1 do
    if EqualPuzzles(Puzzles.Filtered[i], Puzzle) then begin
      Result := i;
      exit;
    end
end; // FindPuzzle

procedure TLibrary.RegisterPuzzle(Puzzle: TPuzzle; FileName: String; FileAge: TDateTime; FileSize: integer);
var Item: TLibraryItem;
begin
  if Puzzles = nil then
    Puzzles := TLibrary.Create;

  Item := TLibraryItem.Create;
//  Item.Assign(Puzzle);

  Item.Name       := Puzzle.Header.Name;
  Item.Aliases    := Puzzle.Header.Aliases;
  Item.MenuString := Puzzle.Header.MenuString;
  Item.Inventor   := Puzzle.Header.Inventor;
  Item.Programmer := Puzzle.Header.Programmer;
  Item.Added      := Puzzle.Header.Added;
  Item.Link       := Puzzle.Header.Link;
  Item.FileName   := Puzzle.Header.FileName;

  Item.PuzzleClass := GetPuzzleClassByName(Puzzle.Header.ClassString);
  Item.FileName := FileName;
  Item.FileAge  := FileAge;
  Item.FileSize := FileSize;

//  if not FileAge(FileName, Item.FileAge) then
//    Item.FileAge := -1;

  Item.Id := Puzzles.Add(Item);
end; // RegisterPuzzle

procedure TLibrary.RegisterPuzzle(LibraryItem: TLibraryItem);
var Item: TLibraryItem;
begin
  if Puzzles = nil then
    Puzzles := TLibrary.Create;

  Item := TLibraryItem.Create;
  Item.Assign(LibraryItem);
  Item.Id := Puzzles.Add(Item);
end; // RegisterPuzzle

class procedure TLibrary.RegisterPuzzleClass(PuzzleClass: TPuzzleClass);
var Item: TLibraryClassItem;
begin
  if Puzzles = nil then
    Puzzles := TLibrary.Create;

  Item := TLibraryClassItem.Create;
  Item.PuzzleClass := PuzzleClass;
  Puzzles.fLibraryClasses.Add(Item);
end;

class function TLibrary.GetPuzzleClassByName(AClassName: String): TPuzzleClass;
var i: integer;
begin
  for i := 0 to Puzzles.fLibraryClasses.Count - 1 do
    if SameText(AClassName, TLibraryClassItem(Puzzles.fLibraryClasses[i]).PuzzleClass.ClassName) then begin
      Result := TLibraryClassItem(Puzzles.fLibraryClasses[i]).PuzzleClass;
      exit;
    end;
  Result := TPuzzle;
end;

class procedure TLibrary.PuzzleLoadHeaderFromStreamFile(Puzzle: TPuzzle; const FileName: String);
var FStream: TFileStream;
  StreamReader: TStreamReadWrite;
begin
  if not FileExists(FileName) then
    exit;

  FStream := TFileStream.Create(FileName, fmOpenRead);
  StreamReader := TStreamReadWrite.Create(FStream);
  try
    StreamReader.PuzzleLoadHeaderFromStream(Puzzle);
  finally
    StreamReader.Free;
    FStream.Free;
  end;
end; // LoadHeaderFromBinaryFile

class procedure TLibrary.PuzzleLoadFromStreamFile(Puzzle: TPuzzle; const FileName: String);
var FStream: TFileStream;
  StreamReader: TStreamReadWrite;
begin
  if not FileExists(FileName) then
    exit;

  FStream := TFileStream.Create(FileName, fmOpenRead);
  StreamReader := TStreamReadWrite.Create(FStream);
  try
    StreamReader.PuzzleLoadFromStream(Puzzle);
  finally
    StreamReader.Free;
    FStream.Free;
  end;
end; // LoadFromBinaryFile

class function TLibrary.PuzzleLoadHeaderFromFile(Puzzle: TPuzzle; const FileName: string): Boolean;
var ext: String;
begin
  Result := False;

  ext := LowerCase(ExtractFileExt(FileName));
  if ext = '.cub' then
    PuzzleLoadHeaderFromStreamFile(Puzzle, FileName)
  else if ext = '.xml' then
    TXmlReadWrite.PuzzleLoadHeaderFromXmlFile(Puzzle, FileName)
  else
    exit;
  Result := True;
end; // LoadHeaderFromFile

class function TLibrary.PuzzleLoadFromFile(Puzzle: TPuzzle; const FileName: String): Boolean;
var ext: String;
  Texture: TTexture;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  if ext = '.cub' then
    PuzzleLoadFromStreamFile(Puzzle, FileName)
  else if ext = '.xml' then
    TXmlReadWrite.PuzzleLoadFromXmlFile(Puzzle, FileName);

//  Puzzle.LoadTextures;
  for Texture in Puzzle.Textures do
    if not Texture.Load(Puzzle.Header.FileName) then
      TUtils.Log('TLibrary.PuzzleLoadFromFile error: File ' + Texture.FileName + ' not found');

  Result := True;
end; // LoadFromFile

class procedure TLibrary.PuzzleSaveToStreamFile(Puzzle: TPuzzle; const FileName: String);
var MemoryStream: TMemoryStream;
  StreamWriter: TStreamReadWrite;
begin
  MemoryStream := TMemoryStream.Create;
  StreamWriter := TStreamReadWrite.Create(MemoryStream);
  try
    StreamWriter.PuzzleSaveToStream(Puzzle);
    MemoryStream.SaveToFile(FileName);
  finally
    StreamWriter.Free;
    MemoryStream.Free;
  end;
end; // SaveToBinaryFile

// test procedure 2023-02-16 ("save as... 1.lib")
procedure TLibrary.SaveAllPuzzlesToStream(const FileName: String);
var
  MemoryStream: TMemoryStream;
  StreamWriter: TStreamReadWrite;
  i: integer;
  Puzzle: TPuzzle;
begin
  MemoryStream := TMemoryStream.Create;
  StreamWriter := TStreamReadWrite.Create(MemoryStream);
  Puzzle := TPuzzle.Create;
  try
    for i := 0 to Count - 1 do
      if PuzzleLoadFromFile(Puzzle, Items[i].FileName) then begin
        StreamWriter.PuzzleSaveToStream(Puzzle);
        MemoryStream.SaveToFile(FileName);
      end;
  finally
    Puzzle.Free;
    StreamWriter.Free;
    MemoryStream.Free;
  end;
end; // SaveAllPuzzlesToStream

function TLibrary.GetById(Id: integer): TLibraryItem;
var i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Id = Id then begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

function TLibrary.Get(Index: Integer): TLibraryItem;
begin
  Result := TLibraryItem(inherited Get(Index));
end;

function TLibrary.GetFiltered(Index: Integer): TLibraryItem;
begin
  Result := fFilteredItems[Index];
end;

function SortByName(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TLibraryItem(Item1).Name, TLibraryItem(Item2).Name);
end;

function SortByAliases(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TLibraryItem(Item1).Aliases, TLibraryItem(Item2).Aliases);
end;

function SortByClass(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TLibraryItem(Item1).PuzzleClass.ClassName, TLibraryItem(Item2).PuzzleClass.ClassName);
end;

function SortByMenu(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TLibraryItem(Item1).MenuString, TLibraryItem(Item2).MenuString);
end;

function SortByFile(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TLibraryItem(Item1).FileName, TLibraryItem(Item2).FileName);
end;

function SortByInventor(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TLibraryItem(Item1).Inventor, TLibraryItem(Item2).Inventor);
end;

function SortByProgrammer(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TLibraryItem(Item1).Programmer, TLibraryItem(Item2).Programmer);
end;

function SortByAdded(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TLibraryItem(Item1).Added, TLibraryItem(Item2).Added);
end;

procedure TLibrary.Sort(Mode: TSortType);
begin
//exit; // !!!!!!!!!!!!!
  fSortMode := Mode;
  case Mode of
    SORT_BY_NAME:       inherited Sort(@SortByName);
    SORT_BY_ALIASES:    inherited Sort(@SortByAliases);
    SORT_BY_CLASS:      inherited Sort(@SortByClass);
    SORT_BY_MENU:       inherited Sort(@SortByMenu);
    SORT_BY_FILE:       inherited Sort(@SortByFile);
    SORT_BY_INVERTOR:   inherited Sort(@SortByInventor);
    SORT_BY_PROGRAMMER: inherited Sort(@SortByProgrammer);
    SORT_BY_Added:      inherited Sort(@SortByAdded);
  end;
  SetFilter(fFilter);
end; // Sort

procedure TLibrary.SetFilter(Text: String);
var i: integer;
begin
  fFilter := Text;
  fFilteredItems.Clear;
  Text := LowerCase(Text);

  for i := 0 to Count - 1 do
    if Items[i].MatchFilter(Text) then
      fFilteredItems.Add(Items[i]);
end;

type
  TFileRec = class
    TimeStamp: TDateTime;
    FileSize: integer;
  end;

procedure GetFilesListRecursive(Folder, Mask: String; slFiles: TStringList);
var
  SearchRec: TSearchRec;
  iRes, i: Integer;
  Folders: TStringList;
  FileRec: TFileRec;
begin
  iRes := FindFirst(Folder + PathDelim + Mask, faAnyFile, SearchRec);
  while iRes = 0 do begin
    FileRec := TFileRec.Create;
    FileRec.TimeStamp := SearchRec.TimeStamp;
    FileRec.FileSize := SearchRec.Size;
    slFiles.AddObject(Folder + PathDelim + SearchRec.Name, FileRec);
    iRes := FindNext(SearchRec);
  end;
  FindClose(SearchRec);

  Folders := TStringList.Create;

  iRes := FindFirst(Folder + PathDelim + '*.*', faDirectory, SearchRec);
  while iRes = 0 do begin
    if ((SearchRec.Attr and faDirectory) <> 0) and (copy(SearchRec.Name, 1, 1) <> '.') then
      Folders.Add(SearchRec.Name);
    iRes := FindNext(SearchRec);
  end;

  for i := 0 to Folders.Count - 1 do
    GetFilesListRecursive(Folder + PathDelim + Folders[i], Mask, slFiles);

  Folders.Free;
end; // GetFilesListRecursive

procedure TLibrary.Load;

  function AbsToRelative(const AbsPath, BasePath: string): string;
  begin
    if AbsPath.StartsWith(BasePath) then
      Result := AbsPath.Substring(Length(BasePath))
    else
      Result := AbsPath;
  end;

var
  i, idx: integer;
//  Count1, Count2: integer;
  slFiles, slOldFiles: TStringList;
  Puzzle: TPuzzle;
  OldLibrary: TLibrary;
  Modified: Boolean;
  CheckFileAge: Boolean;
  StartDir: String;
begin

  // !!!!!!!!!!!!
//  slFiles := TStringList.Create;
//  Puzzle := TPuzzle.Create;
//  try
//    slFiles.LoadFromFile('D:\Boris\PAS\Головоломки\Cubes\a.txt');
//    for i := 0 to slFiles.Count - 1 do begin
//      try
//        PuzzleLoadHeaderFromFile(Puzzle, slFiles[i]);
//        if Puzzle.Header.MenuString <> '' then begin // if this is a puzzle
//          Modified := True;
//          RegisterPuzzle(Puzzle, slFiles[i], 0, 0);
//        end;
//      except
//        on E: Exception do
//          TUtils.Log('Error loading header from ' + slFiles[i] + ': ' + E.Message);
//          //raise Exception.Create('Error loading header from ' + slFiles[i] + ': ' + E.Message);
//      end;
//
//    end;
//  finally
//    Puzzle.Free;
//    slFiles.Free;
//  end;
//exit;

  CheckFileAge := False;
  for i := 1 to System.ParamCount do
    if LowerCase(ParamStr(i)) = '-checkfileage' then
      CheckFileAge := True;

//  TUtils.Log('0: GetFilesListRecursive');
  slFiles := TStringList.Create;
  StartDir := TUtils.GetStartDir;
  GetFilesListRecursive(StartDir + 'Puzzles', '*.xml', slFiles);
  GetFilesListRecursive(StartDir + 'new', '*.xml', slFiles);

  for i := 0 to slFiles.Count - 1 do
    slFiles[i] := AbsToRelative(slFiles[i], StartDir);

  OldLibrary := TLibrary.Create;
  slOldFiles := TStringList.Create;
  slOldFiles.Sorted := True;
  Puzzle := TPuzzle.Create;
  try
    OldLibrary.LoadFromBinaryFile(StartDir + CacheFileName);
    for i := 0 to OldLibrary.Count - 1 do
      slOldFiles.AddObject(OldLibrary[i].FileName, OldLibrary[i]);

    Modified := False;
//    Count1 := 0;
//    Count2 := 0;
    for i := 0 to slFiles.Count - 1 do begin
      idx := slOldFiles.IndexOf(slFiles[i]);
      if idx >= 0 then begin
        if TLibraryItem(slOldFiles.Objects[idx]).FileSize = TFileRec(slFiles.Objects[i]).FileSize then begin
          if (not CheckFileAge) or (TLibraryItem(slOldFiles.Objects[idx]).FileAge = TFileRec(slFiles.Objects[i]).TimeStamp) then begin
            if TLibraryItem(slOldFiles.Objects[idx]).MenuString <> '' then // if this is a puzzle
              RegisterPuzzle(TLibraryItem(slOldFiles.Objects[idx]));
            slOldFiles.Delete(idx);
            Continue;
          end;
        end;
      end;

      try
        PuzzleLoadHeaderFromFile(Puzzle, slFiles[i]);
        if Puzzle.Header.MenuString <> '' then begin // if this is a puzzle
          Modified := True;
          RegisterPuzzle(Puzzle, slFiles[i], TFileRec(slFiles.Objects[i]).TimeStamp, TFileRec(slFiles.Objects[i]).FileSize);
        end;
      except
        on E: Exception do
          TUtils.Log('Error loading header from ' + slFiles[i] + ': ' + E.Message);
          //raise Exception.Create('Error loading header from ' + slFiles[i] + ': ' + E.Message);
      end;

    end;
//    TUtils.Log(':' + IntToStr(Count));
    if Modified or (slOldFiles.Count > 0) then
      SaveToBinaryFile(StartDir + CacheFileName);
  finally
    Puzzle.Free;
    OldLibrary.Free;
    slOldFiles.Free;
  end;

  for i := 0 to slFiles.Count - 1 do
    slFiles.Objects[i].Free;
  slFiles.Free;
end; // Load

function TLibrary.LoadFromBinaryFile(const FileName: String): Boolean;
var MemoryStream: TMemoryStream;
    StreamReader: TStreamReadWrite;
begin
  Result := False;
  if not FileExists(FileName) then
    exit;

  MemoryStream := TMemoryStream.Create;
  StreamReader := TStreamReadWrite.Create(MemoryStream);
  try
    MemoryStream.LoadFromFile(FileName);
    MemoryStream.Seek(0, soBeginning);
    StreamReader.LibraryLoadFromStream(Self);
    Result := True;
  finally
    StreamReader.Free;
    MemoryStream.Free;
  end;
end; // LoadFromBinaryFile

function TLibrary.SaveToBinaryFile(const FileName: String): Boolean;
var MemoryStream: TMemoryStream;
    StreamWriter: TStreamReadWrite;
begin
  MemoryStream := TMemoryStream.Create;
  StreamWriter := TStreamReadWrite.Create(MemoryStream);
  try
    StreamWriter.LibrarySaveToStream(Self);
    MemoryStream.SaveToFile(FileName);
    Result := True;
  finally
    StreamWriter.Free;
    MemoryStream.Free;
  end;
end; // SaveToBinaryFile

procedure TLibrary.SaveToXmlFile(const FileName: String);
var
  XmlDoc: TXmlDocument;
  XmlNode: TDomNode;
  i: integer;
begin
//  XmlDoc := TXmlDocument.Create;
  try
    XmlNode := XmlDoc.FindNode('xml');

    TDomElement(XmlNode).SetAttribute('Data_Version', IntToStr(XML_VERSION));

    for i := 0 to Count - 1 do
      Items[i].SaveHeaderToXml(XmlDoc, XmlDoc.DocumentElement);

    WriteXML(XmlDoc, FileName);
  //  XmlDoc.Save(AnsiString(FileName));
  finally
    XmlDoc.Free;
  end;
end; // PuzzleSaveToXmlFile

procedure TLibrary.LoadFromCompressedXmlFile(const FileName: String);
var
  XmlDoc: TXmlDocument;
  XmlNode: TDomNode;
  mStream: TMemoryStream;
  LibraryItem: TLibraryItem;
  i: integer;
begin
//  XmlDoc := TXmlDocument.Create;
  mStream := TMemoryStream.Create;
  try
    if not TUtils.LoadFileFromZip(mStream, PUZZLES_ARCHIVE_PATH, FileName) then
      Exit;

    mStream.Seek(0, soBeginning);

    ReadXMLFile(XmlDoc, mStream);
    XmlNode := XmlDoc.FindNode('xml');

    for i := 0 to XmlNode.ChildNodes.Count - 1 do
      if SameText(XmlNode.ChildNodes[i].NodeName, 'Puzzle') then begin
        LibraryItem := TLibraryItem.Create;
        LibraryItem.LoadHeaderFromXml(XmlNode.ChildNodes[i]);
        LibraryItem.Id := Self.Add(LibraryItem);
      end;
  finally
    mStream.Free;
    XmlDoc.Free;
  end;
end; // LoadFromXmlFile

// ========================= TLibraryItem =========================

procedure TLibraryItem.Assign(Item: TLibraryItem);
begin
  PuzzleClass := Item.PuzzleClass;
  Name        := Item.Name;
  Aliases     := Item.Aliases;
  MenuString  := Item.MenuString;
  FileName    := Item.FileName;
  FileAge     := Item.FileAge;
  FileSize    := Item.FileSize;
  Inventor    := Item.Inventor;
  Programmer  := Item.Programmer;
  Added       := Item.Added;
  Link        := Item.Link;
end; // Assign

function TLibraryItem.MatchFilter(Filter: String): Boolean;

  function MatchSimple(Filter: String): Boolean;
  begin
    Result := (Filter = '') or
      (Pos(Filter, LowerCase(Name)) > 0) or
      (Pos(Filter, LowerCase(Aliases)) > 0) or
      (Pos(Filter, LowerCase(PuzzleClass.ClassName)) > 0) or
      (Pos(Filter, LowerCase(MenuString)) > 0) or
      (Pos(Filter, LowerCase(FileName)) > 0) or
      (Pos(Filter, LowerCase(Inventor)) > 0) or
      (Pos(Filter, LowerCase(Programmer)) > 0) or
      (Pos(Filter, LowerCase(Link)) > 0) or
      (Pos(Filter, LowerCase(Added)) > 0);
  end;

begin
  Result := False;
  while Trim(Filter) <> '' do
    if not MatchSimple(TUtils.Parse(Filter, ' ')) then
      exit;
  Result := True;
end;

procedure TLibraryItem.SaveHeaderToXml(XmlDoc: TXMLDocument; XmlNode: TDomNode);
var S: String;
    NewNode, NN: TDomElement;
begin
  NewNode := XmlDoc.CreateElement('Puzzle');
  XmlNode.AppendChild(NewNode);

  NN := XmlDoc.CreateElement('Name');
  NN.TextContent := Name;
  NewNode.AppendChild(NN);

  NN := XmlDoc.CreateElement('Aliases');
  NN.TextContent := Aliases;
  NewNode.AppendChild(NN);

  NN := XmlDoc.CreateElement('ClassName');
  NN.TextContent := PuzzleClass.ClassName;
  NewNode.AppendChild(NN);

  S := MenuString;
  while S <> '' do begin
    NN := XmlDoc.CreateElement('Menu');
    NN.TextContent := TUtils.Parse(S, ';');
    NewNode.AppendChild(NN);
  end;

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

  NN := XmlDoc.CreateElement('File');
  NN.TextContent := FileName;
  NewNode.AppendChild(NN);
end; // TurnSaveToXml

procedure TLibraryItem.LoadHeaderFromXml(XmlNode: TDomNode);
begin
  Name        := ReadStringFromXml (XmlNode, 'Name');
  Aliases     := ReadStringFromXml (XmlNode, 'Aliases');
  PuzzleClass := TLibrary.Puzzles.GetPuzzleClassByName(ReadStringFromXml(XmlNode, 'Class'));
  MenuString  := ReadStringFromXml (XmlNode, 'Menu'{, ';'}); // ReadMultiStringFromXml
  Inventor    := ReadStringFromXml (XmlNode, 'Inventor');
  Programmer  := ReadStringFromXml (XmlNode, 'Programmer');
  Added       := ReadStringFromXml (XmlNode, 'Added');
  Link        := ReadStringFromXml (XmlNode, 'Link'{, ';'}); // ReadMultiStringFromXml
  FileName    := ReadStringFromXml (XmlNode, 'File');
end; // TurnSaveToXml

initialization
  if TLibrary.Puzzles = nil then
    TLibrary.Puzzles := TLibrary.Create;
end.
