{************************************************************}
{                                                            }
{  Unit uXmlUtils                                            }
{  2019-2023                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uXmlUtils;

interface
uses
  uVector,
  Classes, Graphics, DOM;

function GetNodeText(XmlNode: TDomNode): string;
procedure ReadStringFromXml(XmlNode: TDomNode; const AttrName: String; var Str: String; const Default: String); overload;
function ReadStringFromXml(XmlNode: TDomNode; const AttrName: String; var Str: String): Boolean; overload;
function ReadStringFromXml(XmlNode: TDomNode; const AttrName: String): string; overload;
function ReadStringFromXmlOrParent(XmlNode: TDomNode; const AttrName: String): string; overload;
function ReadMultiStringFromXml(XmlNode: TDomNode; const AttrName: String; Delimiter: String): String;
function GetAttrIndex(XmlNode: TDomNode; const AttrName: String): integer;
function GetAttrFromXml(XmlNode: TDomNode; const AttrName: String; const Default: String = ''): string;

function ReadIntExpressionFromXml(XmlNode: TDomNode; const AttrName: String; Default: integer = 0): integer;
function ReadIntExpressionFromXmlOrParent(XmlNode: TDomNode; const AttrName: String; Default: integer = 0): integer;
function ReadDoubleExpressionFromXml(XmlNode: TDomNode; const AttrName: String): extended;
function ReadColorExpressionFromXml(XmlNode: TDomNode; const AttrName: String; Default: TColor = -1): TColor;
function ReadVectorFromXml(var Vec: TVector; XmlNode: TDomNode; const AttrName: String): Boolean;

//type
//
//  { TXmlDoc }
//
//  TXmlDoc = class
//    private
//      FDoc: TXMLDocument;
//    public
//      constructor Create(const FileName: String);
//      constructor Create(const Stream: TStream);
//      destructor Destroy; override;
//  end;

implementation

uses
  uScript, uUtils,
  SysUtils; // for Lazarus-xml

function GetNodeText(XmlNode: TDomNode): string;
var TextNode: TDomNode;
begin
  TextNode := XmlNode.FindNode('#text');
  if Assigned(TextNode) then
    Result := Trim(TextNode.NodeValue)
  else
    Result := '';
end; // GetNodeText

function GetAttrIndex(XmlNode: TDomNode; const AttrName: String): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to XmlNode.Attributes.Length - 1 do
    if SameText(XmlNode.Attributes[i].NodeName, AttrName) then
      Exit(i);
end; // GetAttrIndex

function GetAttrFromXml(XmlNode: TDomNode; const AttrName: String; const Default: String = ''): string;
var i: integer;
begin
  Result := Default;
  i := GetAttrIndex(XmlNode, AttrName);
  if i >= 0 then
    Result := XmlNode.Attributes[i].NodeValue;
end; // GetAttrFromXml

function ReadStringFromXml(XmlNode: TDomNode; const AttrName: String; var Str: String): Boolean; overload;
var i: integer;
begin
  Str := '';
  if XmlNode = nil then
    Exit(False);

  if AttrName = '' then begin
    Str := UTF8ToString(GetNodeText(XmlNode));
    Exit(Str <> '');
  end;

  i := GetAttrIndex(XmlNode, AttrName);
  if i >= 0 then begin
    Str := UTF8ToString(XmlNode.Attributes[i].NodeValue);
    Exit(Str <> ''); // ?? Exit(True)
  end;

  XmlNode := XmlNode.FindNode(AttrName);
  if XmlNode = nil then
    Exit(False);

  Str := UTF8ToString(XmlNode.TextContent);
  Result := Str <> '';
end; // ReadStringFromXml

function ReadStringFromXml(XmlNode: TDomNode; const AttrName: String): string; overload;
var Str: String = '';
begin
  if ReadStringFromXml(XmlNode, AttrName, Str) then
    Result := Str
end;

procedure ReadStringFromXml(XmlNode: TDomNode; const AttrName: String; var Str: String; const Default: String); overload;
begin
  if not ReadStringFromXml(XmlNode, AttrName, Str) then
    Str := Default;
end;

function ReadMultiStringFromXml(XmlNode: TDomNode; const AttrName: String; Delimiter: String): String;
var
  i: integer;
  XmlSubNode: TDomNode;
  S: String = '';
begin
  Result := '';

  for i := 0 to XmlNode.ChildNodes.Count - 1 do begin
    XmlSubNode := XmlNode.ChildNodes[i];
    if SameText(string(XmlSubNode.NodeName), AttrName) then begin
      ReadStringFromXml(XmlSubNode, '', S, '');
      if S <> '' then begin
        if Result <> '' then
          Result := Result + Delimiter;
        Result := Result + S;
      end;
    end;
  end;
end; // ReadStringFromXml

function ReadStringFromXmlOrParent(XmlNode: TDomNode; const AttrName: String; var Str: String): Boolean; overload;
begin
  if XmlNode = nil then
    Exit(False);
  Result := ReadStringFromXml(XmlNode, AttrName, Str);
  if (not Result) and (XmlNode.ParentNode <> nil) then
    Result := ReadStringFromXml(XmlNode.ParentNode, AttrName, Str);
end; // ReadStringFromXmlOrParent

function ReadStringFromXmlOrParent(XmlNode: TDomNode; const AttrName: String): string; overload;
var Str: String = '';
begin
  Result := '';
  if ReadStringFromXmlOrParent(XmlNode, AttrName, Str) then
    Result := Str
end; // ReadStringFromXmlOrParent

function ReadDoubleExpressionFromXml(XmlNode: TDomNode; const AttrName: String): extended;
begin
  Result := CalcExpression(ReadStringFromXml(XmlNode, AttrName));
end; // ReadDoubleExpressionFromXml

function ReadVectorFromXml(var Vec: TVector; XmlNode: TDomNode; const AttrName: String): Boolean;
var S: String;
begin
  if XmlNode = nil then
    exit(False);

  S := GetAttrFromXml(XmlNode, AttrName);
  if S <> '' then begin
    Vec.X := CalcExpression(Trim(TUtils.Parse(S, ';')));
    Vec.Y := CalcExpression(Trim(TUtils.Parse(S, ';')));
    Vec.Z := CalcExpression(Trim(TUtils.Parse(S, ';')));
    Result := True;
  end else begin
    //Result := TXmlReadWrite.VectorLoadFromXml(Vec, XmlNode.FindNode(AttrName));
    Vec.X := ReadDoubleExpressionFromXml(XmlNode, 'X');
    Vec.Y := ReadDoubleExpressionFromXml(XmlNode, 'Y');
    Vec.Z := ReadDoubleExpressionFromXml(XmlNode, 'Z');
    Result := True;
  end;
end; // ReadVectorFromXml

function ReadColorExpressionFromXml(XmlNode: TDomNode; const AttrName: String; Default: TColor = -1): TColor;

  function StrIsColor(S: String): Boolean;
  var i: integer;
  begin
    Result := False;
    if Length(S) <> 6 then
      exit;
    for i := 1 to 6 do
      if not CharInSet(S[i], ['0'..'9', 'a'..'f', 'A'..'F']) then
        exit;
    Result := True;
  end;

var
  V: TVar;
  Text: String;
begin // ReadColorExpressionFromXml
  Result := Default;
  Text := Trim(ReadStringFromXml(XmlNode, AttrName));
  if Text = '' then
    exit;

  if not StrIsColor(Text) then begin
    ExecuteScript('Result := ' + Text);

    if Calc.VarByName('Result', V) then
      Text := V.Value;
  end;

  Result := TUtils.StrToColor(Text);
end; // ReadColorStringExpressionFromXml

function ReadIntExpressionFromXml(XmlNode: TDomNode; const AttrName: String; Default: integer = 0): integer;
var Expression: String = '';
begin
  if ReadStringFromXml(XmlNode, AttrName, Expression) then
    if Expression <> '' then
      Exit(Round(CalcExpression(Expression, Default)));
  Result := Default;
end; // ReadIntExpressionFromXml

function ReadIntExpressionFromXmlOrParent(XmlNode: TDomNode; const AttrName: String; Default: integer = 0): integer;
var Expression: String = '';
begin
  if ReadStringFromXmlOrParent(XmlNode, AttrName, Expression) then
    if Expression <> '' then
      Exit(Round(CalcExpression(Expression, Default)));
  Result := Default;
end; // ReadIntExpressionFromXml

// ============================ TXmlDoc ========================================

//constructor TXmlDoc.Create(const FileName: String);
//begin
//  ReadXMLFile(FDoc, FileName);
//end;
//
//constructor TXmlDoc.Create(const Stream: TStream);
//begin
//  Stream.Seek(0, soBeginning);
//  ReadXMLFile(FDoc, Stream);
//end;
//
//destructor TXmlDoc.Destroy;
//begin
//  FDoc.Free;
//  inherited Destroy;
//end;

initialization
end.

