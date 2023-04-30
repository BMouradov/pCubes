{************************************************************}
{                                                            }
{  Unit uLog                                                 }
{  2023-2023                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit uLog;
{$mode delphi}

interface

uses
  Generics.Collections{Classes, SysUtils};

type
  TLogUpdatedEvent = procedure() of object;

type
  TLogRecord = record
    TimeStamp: TDateTime;
    LogStr: string;
  end;


type

  { TLog }

  TLog = class
  private
    FLogList: TList<TLogRecord>;
    FOnLogUpdated: TLogUpdatedEvent;
  public
    property OnLogUpdated: TLogUpdatedEvent write FOnLogUpdated;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(LogStr: String);
    function GetText(LastStrings: integer): String;
  end;


implementation

uses SysUtils;

{ TLog }

constructor TLog.Create;
begin
  inherited Create;
  FLogList := TList<TLogRecord>.Create;
  FOnLogUpdated := nil;
end;

destructor TLog.Destroy;
begin
  FreeAndNil(FLogList);
  inherited Destroy;
end;

procedure TLog.Clear;
begin
  FLogList.Free;
end;

procedure TLog.Add(LogStr: String);
var LogRec: TLogRecord;
begin
  LogRec.TimeStamp := Now;
  LogRec.LogStr := LogStr;
  FLogList.Add(LogRec);
  if Assigned(FOnLogUpdated) then
    FOnLogUpdated();
end;

function TLog.GetText(LastStrings: integer): String;
var FirstLine, i: integer;
begin
  Result := '';
  FirstLine := FLogList.Count - LastStrings;
  if (FirstLine < 0) or (LastStrings = -1) then
    FirstLine := 0;
  for i := FirstLine to FLogList.Count - 1 do
    Result := Result + TimeToStr(Now) + ': ' + FLogList[i].LogStr + {$IFDEF LIN}#10{$ELSE}#13#10{$ENDIF};
end; // GetText

end.

