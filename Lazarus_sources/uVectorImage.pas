{************************************************************}
{                                                            }
{  Unit uVectorImage                                         }
{  2017-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uVectorImage;
{$mode delphi}

interface

uses Classes, Graphics, Generics.Collections;

type
  TIPoint = record
    x, y: integer;
  end;

type
  TVectorCommand = class
  protected
    procedure LoadFromStr(Text: string); virtual; abstract;
  end;

type
  TVectorCommandPenColor = class(TVectorCommand)
  private
    FPenColor: TColor;
  protected
    procedure LoadFromStr(Text: string); override;
  public
    property PenColor: TColor read FPenColor;
  end;

type
  TVectorCommandBrushColor = class(TVectorCommand)
  private
    FBrushColor: TColor;
  protected
    procedure LoadFromStr(Text: string); override;
  public
    property BrushColor: TColor read FBrushColor;
  end;

type
  TVectorCommandPolygon = class(TVectorCommand)
  private
    FPoints: TList<TIPoint>;
  protected
    procedure LoadFromStr(Text: string); override;
  public
    property Points: TList<TIPoint> read FPoints;

    constructor Create;
    destructor Destroy; override;
  end;

type
  TVectorImage = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  private
    FVectorCommands: TList<TVectorCommand>;
//    procedure AddCommand(Command: TVectorCommand);
    function ReadCommandFromString(const S: string): TVectorCommand;

  public
    Width, Height: integer;
    property VectorCommands: TList<TVectorCommand> read FVectorCommands;

    procedure LoadFromFile(FileName: String);

//    procedure AddText(Str: string);

  end;

implementation
uses uUtils,
     SysUtils;

//------------------------- TVectorCommandPenColor ----------------------------

procedure TVectorCommandPenColor.LoadFromStr(Text: string);
begin
  FPenColor := TUtils.StrToColor(Text);
end; // SetText

//----------------------- TVectorCommandBrushColor ----------------------------

procedure TVectorCommandBrushColor.LoadFromStr(Text: string);
begin
  FBrushColor := TUtils.StrToColor(Text);
end; // SetText

//------------------------- TVectorCommandPolygon -----------------------------

constructor TVectorCommandPolygon.Create;
begin
  inherited;
  FPoints := TList<TIPoint>.Create;
end;

destructor TVectorCommandPolygon.Destroy;
begin
  FPoints.Free;
end; // Destroy

procedure TVectorCommandPolygon.LoadFromStr(Text: string);
var p: TIPoint;
begin
  while Length(Text) > 0 do begin
    p.x := TUtils.ParseInt(Text);
    p.y := TUtils.ParseInt(Text);
    FPoints.Add(p);
  end;
end; // SetText

//----------------------------- TVectorImage ----------------------------------

constructor TVectorImage.Create;
begin
  inherited;
  Width := 1;
  Height := 1;
  FVectorCommands := TList<TVectorCommand>.Create;
end; // Create

destructor TVectorImage.Destroy;
begin
  Clear;
  FVectorCommands.Free;
  inherited;
end; // Destroy

procedure TVectorImage.Clear;
var vc: TVectorCommand;
begin
  for vc in FVectorCommands do
    vc.Free;
  FVectorCommands.Clear;
end; // Clear

function TVectorImage.ReadCommandFromString(const S: string): TVectorCommand;
var sCommand, S1: string;
begin
  S1 := S;
  sCommand := AnsiLowerCase(TUtils.Parse(S1, ' '));
  if sCommand = 'pencolor' then
    Result := TVectorCommandPenColor.Create
  else if sCommand = 'brushcolor' then
    Result := TVectorCommandBrushColor.Create
  else if sCommand = 'polygon' then
    Result := TVectorCommandPolygon.Create
  else
    Result := nil;

  if Result <> nil then
    Result.LoadFromStr(S1);
end; // ReadCommandFromString

procedure TVectorImage.LoadFromFile(FileName: String);
var
  i: integer;
  Command: TVectorCommand;
  sl: TStringList;
  S: string;
begin
  Clear;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    if sl.Count > 0 then
      S := Trim(sl[0])
    else
      S := '';
    TUtils.Parse(S, ' ');
    Width := StrToIntDef(TUtils.Parse(S, 'x'), 100);
    Height := StrToIntDef(S, 100);
    for i := 1 to sl.Count - 1 do begin
      Command := ReadCommandFromString(Trim(sl[i]));
      if Command <> nil then
        FVectorCommands.Add(Command);
    end;
  finally
    sl.Free;
  end;
end; // LoadFromFile

end.
