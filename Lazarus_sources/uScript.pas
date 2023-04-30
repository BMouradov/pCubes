{************************************************************}
{                                                            }
{  Module uScript                                            }
{  Copyright (c) 2000 Alex Boiko                             }
{  2014-2020                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{  Script engine                                             }
{************************************************************}

{Based on PASCALC v 3.00 source}
{const
_pascalc: AnsiString = #10#10 +
'*************************************************'#10 +
'*      PASCALC interpreter v3.00 for Delphi     *'#10 +
'*    (c)2000 Alex Boiko  alexboiko@mtu-net.ru   *'#10 +
'*            http://alexboiko.da.ru             *'#10 +
'*************************************************'#10#10;}


unit uScript;

{$mode Delphi}

interface

uses classes, Generics.Collections;

type
  TToken =
    (tEMPTY, tVR, tCON, tTRUE, tFALSE,
    tEQU, tOR, tAND, tNOT, tXOR,
    tCOMMA, tLBL, tNEQ, tGT, tLS,
    tGTE, tLSE, tADD, tSUB, tMUL,
    tDIVIDE, tDIV, tMOD, tPWR, tLBR, tRBR, tLARR,
    tRARR, tSEMI, tREM, tREMB, tREME,
    tASSIGN, tBEGIN, tEND, tIF, tTHEN,
    tELSE, tFOR, tTO, tDOWNTO, tDO,
    tWHILE, tREPEAT, tUNTIL, tBREAK, tCONTINUE,
    tEXIT, tGOTO, tSHL, tSHR, tPROC,
    tFUNCT, tUSES, tINCLUDE, tCASE, tOF,
    tDOT2);

type
  TTokenSet = set of TToken;

const
  ResWords: array[TToken] of String =
  ('', '', '', 'TRUE', 'FALSE',
    '=', 'OR', 'AND', 'NOT', 'XOR',
    ',', ':', '<>', '>', '<',
    '>=', '<=', '+', '-', '*',
    '/', 'DIV', 'MOD', '^', '(', ')', '[',
    ']', ';', '//', '{', '}',
    ':=', 'BEGIN', 'END', 'IF', 'THEN',
    'ELSE', 'FOR', 'TO', 'DOWNTO', 'DO',
    'WHILE', 'REPEAT', 'UNTIL', 'BREAK', 'CONTINUE',
    'EXIT', 'GOTO', 'SHL', 'SHR', 'PROCEDURE',
    'FUNCTION', 'USES', 'INCLUDE', 'CASE', 'OF',
    '..');

const
  Alpha: set of AnsiChar = ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
  StrDelimiter: Char = '''';
  DecimalPoint: Char = '.';
  TokenDelimiter: Char = #127;

type
  TVar = record
    Name: String;
    Value: Variant;
  end;

type
  PIdleProc = procedure;

type
//  PFunction = function(Sender: TObject; Vars: TVarList; var Res: TVar): Boolean;
  TFunction = function(Vars: TList<TVar>; var Res: TVar): Boolean of Object;

type
  TFunc = record
    Name: String;
    Func: TFunction;
  end;

type
  TProcedure = record
    Name: String;
    Body: String;
    Params: String;
    Result: Boolean;
  end;

type
  TScript = class
    constructor Create;
    destructor Destroy; override;
    procedure ClearVars;
    procedure ClearProcs;
    procedure ClearFuncs;
    procedure DefineConsts;
    function VarCount: integer;
    function VarExists(const N: String): Boolean;
    procedure VarRemove(const N: String);
    function VarByName(const N: String; var V: TVar): Boolean;
    function SetVar(V: TVar): Boolean;
    function SetValue(N: String; V: variant): Boolean;
    function SetFunction(Name: String; F: TFunction): Boolean;
    procedure SetIdleProc(P: Pointer);
    procedure SetCallGlobalProc(P: TFunction);
    function Parse(S: String): String;
    function Calculate(S: String; var R: TVar): Boolean;
    function Execute(S: String): Boolean;
    procedure DisableInteractiveFunctions;
    procedure EnableInteractiveFunctions;
  private
    Expr: String;
    ExprIndex: integer;
    Token: String;
    TokenCode: TToken;

    BlockLevel: integer;
    BlockCmd: TToken;
    GotoLabel: String;

    VarList:  TDictionary<String, Variant>;
    FuncList: TDictionary<String, TFunc>;
    ProcList: TDictionary<String, TProcedure>;

    FInteractiveFunctionsEnabled: Boolean;

    // внешние функции
    IdleProc: PIdleProc;
    CallGlobalProc: TFunction;

    LastString: String;
    LastParsed: String;

    procedure Clear;
    procedure Process;
    procedure Error(Msg, Line: String; Code: integer);
    procedure Level1(var R: TVar);
    procedure Level2(var R: TVar);
    procedure Level3(var R: TVar);
    procedure Level4(var R: TVar);
    procedure Level5(var R: TVar);
    procedure Level6(var R: TVar);
    procedure Level7(var R: TVar);
    procedure Level8(var R: TVar);
    procedure Arith(o: TToken; var R, H: TVar);
    procedure Unary(o: TToken; var R: TVar);
    function GetIndex(S: String; var Index: integer; var T: TToken): String;
    function GetFuncParams(S: String; var Index: integer): String;
    function FindArray(const N: String): Boolean;
    procedure SetVarDirect(const R: TVar);
    function CallFunc(const FuncName: String; ParamString: String; var V: TVar): Boolean;
    function CallProc(ProcName: String; ParamString: String; var V: TVar): Boolean;
    function GetTextToken(S: String; var Index: integer; var Code: TToken): String;
    function TokenStr(T: TToken; S: String): String;
    function GetToken(S: String; var Index: integer; var Code: TToken): String;
    function GetTokenCode(S: String; var Index: integer; var Code: TToken): integer;
    function GetTokenLine(S: String; var Index: integer; var Code: TToken;
      StopToken: TTokenSet): String;
    function NextToken(S: String; Index: integer): TToken;
    function GetOperator(Txt: String; var Index: integer; EndToken: TTokenSet): String;
    function ParseOperator(Txt: String; var Cmd, Line, Lbl: String): TToken;
    function DelRemarks(S: String): String;
    function UnParse(S: String; Show: Boolean): String;
    function PreProcess(Txt: String): String;
    function Calc(S: String; var R: TVar): Boolean;
    procedure Exec(Txt: String);
    procedure DoSet(CmdLine, Cmd, Line: String);
    procedure DoIf(CmdLine, Line: String);
    procedure DoBegin(CmdLine, Line: String);
    procedure DoFor(CmdLine, Line: String);
    procedure DoBreak(CmdLine, Line: String);
    procedure DoContinue(CmdLine, Line: String);
    procedure DoExit(CmdLine, Line: String);
    procedure DoWhile(CmdLine, Line: String);
    procedure DoRepeat(CmdLine, Line: String);
    procedure DoGoto(CmdLine, Line: String);
    procedure DoCase(CmdLine, Line: String);

    procedure SetFunctions;
    function FuncShowMessage(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncLog(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncFloatToStr(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncIntToStr(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncStrToFloat(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncStrToInt(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncHexToInt(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncVal(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncCopy(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncPos(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncLength(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncInsert(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncDelete(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncTrim(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncTrimLeft(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncTrimRight(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncUpperCase(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncLowerCase(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncFormat(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncRequestNumbers(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncDateToStr(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncStrToDate(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncFormatDateTime(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncNow(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncDate(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncTime(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncStrToTime(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncTimeToStr(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncDayOfWeek(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncIncMonth(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncDecodeDate(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncDecodeTime(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncEncodeDate(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncEncodeTime(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncSin(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncCos(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncTan(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncArcSin(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncArcCos(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncArcTan(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncArcTan2(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncExp(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncLn(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncIntPower(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncSqr(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncSqrt(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncAbs(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncInt(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncFrac(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncRound(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncTrunc(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncCeil(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncFloor(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncMax(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncMin(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncInc(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncDec(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncSetVar(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncGetVar(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncVarExists(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncRandom(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncCallGlobal(Vars: TList<TVar>; var Res: TVar): Boolean;
    function FuncGetTickCount(Vars: TList<TVar>; var Res: TVar): Boolean;

  public
    Stop: Boolean;
    ErrCode: integer;
    ErrMsg: String;
    ErrLine: String;
  end;

//procedure SetFunctions(Script: TScript);

procedure ExecuteScript(Text: String);
function CalcExpression(Text: String; Default: extended = 0): extended;

var Calc: TScript;

implementation

uses fOrderDialog,
     uUtils,
     SysUtils, Variants, Types, Windows, Messages, Math, Dialogs, Controls, Forms;

const
  SpaceSet: set of AnsiChar = [' ', #9, #10, #13];

const
  errOK            = 0; // O.K.
  errExprSyntax    = 1; // Error in expression/statement
  errParentheses   = 2; // Unpaired parentheses
  errVarNotFound   = 3; // Variable not found
  errInvalidName   = 4; // Invalid variable/function/procedure name
  errTypeCast      = 5; // Invalid typecast
  errString        = 6; // Invalid string constant
  errCall          = 7; // Invalid function call
  errFuncNotFound  = 8; // Function not found
  errInvalidOp     = 9; // Invalid operator
  errEndExpected   = 10; // END expected
  errManyEnd       = 11; // Too many END
  errToExpected    = 12; // TO or DOWNTO expected
  errForVar        = 13; // FOR-loop variable expected
  errDoExpected    = 14; // DO expected
  errBreak         = 15; // BREAK/CONTINUE outside a loop
  errUntilExpected = 16; // UNTIL expected
  errManyUntil     = 17; // Too many UNTIL
  errLabelNotFound = 18; // Label not found
  errIndexRange    = 19; // Index out of range
  errValueRange    = 20; // Value out of range
  errRbrExpected   = 21; // ']' expected
  errManyRbr       = 22; // Too many '['
  errZeroDivide    = 23; // Division by zero
  errNameDup       = 24; // Variable or array name duplicated
  errFileOpen      = 25; // File оpen error
  errFuncResult    = 26; // Function must return result
  errOfExpected    = 27; // CASE without OF
  errManyElse      = 28; // Too many ELSE in CASE statement
  errCaseRange     = 29; // Case range expected

// Fast upcase function for Russian Win-1251 charset.
// For other charsets, уоu can modify it or replace
// function StUpCaseR with AnsiUpperCase


function DelChrs(s: String; c: Char): String;
var i: integer;
begin
  Result := '';
  for i:= 1 to Length(s) do
    if s[i] <> c then
      Result := Result + s[i];
end; // DelChrs


function ReplaceChrs(s: String; cFrom, cTo: Char): String;
var i: integer;
begin
  Result := s;
  for i := 1 to Length(s) do
    if s[i] = cFrom then
      Result[i] := cTo;
end; // ReplaceChrs


function StrToNum(S: String; var X: extended): Boolean;
var
  c: Char;
  st: String;
  i, nt, nz: integer;
begin
  Result := False;
  nz := 0;
  nt := 0;
  st := Trim(s);
  c := ' ';
  for i := 1 to length(st) do begin
    if not CharInSet(st[i], ['0'..'9', '+', '-', ',', '.', 'E']) then
      exit;
    if st[i] = '.' then begin
      inc(nt);
      c := '.';
    end;
    if st[i] = ',' then begin
      inc(nz);
      c := ',';
    end;
  end;

  if (c = '.') and (nt = 1) then
    st := DelChrs(st, ',');
  if (c = ',') and (nz = 1) then begin
    st := DelChrs(st, '.');
    st := ReplaceChrs(st, ',', '.');
  end;

  Val(st, x, i);
  Result := i = 0;
end; // StrToNum


function ExtToInt(X: extended): integer;
var negative: Boolean;
begin
  negative := x < 0;
  if negative then
    x := -x;
  Result := Trunc(x + 0.000000001);
  if negative then
    Result := -Result;
end; // ExtToInt


// сравнивает две строки, s1 с начала, s2 - с p-го символа, длиной L
function CmpStr(s1, s2: String; p, L: integer): Boolean;
var i, n: integer;
begin
  n := Length(s2);
  if p + L - 1 > n then
    Exit(False);
  for i := 1 to L do
    if s1[i] <> s2[p + i - 1] then
      Exit(False);
  Result := True;
end; // CmpStr


// буквы, цифры и подчерк
function IsAlpha(s: String): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 1 to Length(s) do
    if not CharInSet(s[i], Alpha) then
      Exit;
  Result := True;
end; // IsAlpha

// слова вида S, S[1], S[-1], S[1,1]
function ValidName(S: String): Boolean;
var
  i, n: integer;
  Arr: Boolean;
begin
  Result := False;
  if S = '' then
    Exit;
  if not CharInSet(S[1], Alpha) then
    Exit;
  if CharInSet(S[1], ['0'..'9']) then
    Exit;
  Arr := False;
  n := length(S);
  for i := 2 to n do begin
    if not Arr then begin
      if not CharInSet(S[i], Alpha) then begin
        if S[i] <> ResWords[tLARR][1] then
          Exit;
        Arr := True;
      end;
    end else begin
      if not CharInSet(S[i], ['0'..'9'])
         and (S[i] <> ResWords[tSUB][1])
         and (S[i] <> ResWords[tCOMMA][1])
         and (S[i] <> ResWords[tRARR][1]) then
        Exit;
      if (S[i] = ResWords[tRARR][1]) and (i <> n) then
        Exit;
      if (S[i] = ResWords[tRARR][1]) and (i = n) then
        Arr := False;
    end;
  end;
  if Arr then
    exit;

  Result := True;
end; // ValidName

function VarIsString(V: TVar): Boolean;
var
  t: integer;
begin
  t := VarType(V.Value);
  Result := (t = varString) or (t = varOleStr);
end; // VarIsString

function VarTypeName(V: variant): String;
var
  i: integer;
begin
  i := VarType(v);
  case i of
    varEmpty: Result := 'Empty';
    varNull: Result := 'Null';
    varSmallint: Result := 'SmallInt';
    varInteger: Result := 'Integer';
    varSingle: Result := 'Single';
    varDouble: Result := 'Double';
    varCurrency: Result := 'Currency ';
    varDate: Result := 'Date';
    varOleStr: Result := 'OleStr';
    varDispatch: Result := 'Dispatch';
    varError: Result := 'Error';
    varBoolean: Result := 'Boolean';
    varVariant: Result := 'Variant';
    varUnknown: Result := 'Unknown';
    varByte: Result := 'Byte';
    varString: Result := 'String';
    varTypeMask: Result := 'TypeMask';
    varArray: Result := 'Array';
  else
    Result := '';
  end;
end; // VarTypeName

{------------------------------- TScript ------------------------------------}

constructor TScript.Create;
begin
  inherited Create;

  ErrCode := errOK;
  IdleProc := nil;
  CallGlobalProc := nil;
  VarList := TDictionary<String, Variant>.Create;
  FuncList := TDictionary<String, TFunc>.Create;
  ProcList := TDictionary<String, TProcedure>.Create;
  LastString := '';
  LastParsed := '';

  SetFunctions;
  EnableInteractiveFunctions;
end; // Create

destructor TScript.Destroy;
begin
  VarList.Free;
  FuncList.Free;
  ProcList.Free;
end; // Destroy

procedure TScript.Clear;
begin
  BlockLevel := -1;
  BlockCmd := tEMPTY;
  Stop := False;
  ErrCode := errOK;
  ErrLine := '';
  ErrMsg := '';

//  VarList.ClearAll;
//  ProcList.ClearAll;
//  FuncList.ClearAll;
end; // Clear

procedure TScript.SetIdleProc(P: Pointer);
begin
  IdleProc := P;
end; // SetProcessProc

procedure TScript.SetCallGlobalProc(P: TFunction);
begin
  CallGlobalProc := P;
end; // SetCallProcessProc

procedure TScript.Process;
var
  Msg: TMsg;
begin // Process
  if Stop then
    BlockCmd := tEXIT;

  if Assigned(IdleProc) then begin
    IdleProc;
    Exit;
  end;

  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do begin
    if Msg.Message = WM_QUIT then begin
      BlockCmd := tEXIT;
      Exit;
    end;
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end; // Process

procedure TScript.ClearVars;
begin
  VarList.Clear;
end; // ClearVars

procedure TScript.ClearProcs;
begin
  ProcList.Clear;
end; // ClearVars

procedure TScript.ClearFuncs;
begin
  FuncList.Clear;
end; // ClearFuncs

procedure TScript.DefineConsts;
begin
  SetValue('PI', Pi);
end; // DefineConsts

procedure TScript.Error(Msg, Line: String; Code: integer);
begin
  if ErrCode <> errOK then
    Exit;
  ErrCode := Code;
  ErrMsg := Msg;
  ErrLine := UnParse(Line, True);
end; // Error

function TScript.VarCount: integer;
begin
  Result := VarList.Count;
end; // VarCount

function TScript.VarExists(const N: String): Boolean;
begin
  Result := VarList.ContainsKey(N);
end; // VarIndex

procedure TScript.VarRemove(const N: String);
begin
  VarList.Remove(N);
end; // VarIndex

function TScript.VarByName(const N: String; var V: TVar): Boolean;
var Name: String;
begin
  Name := UpperCase(Trim(N));
  Result := VarList.TryGetValue(Name, V.Value);
  if Result then
    V.Name := N;
//Result := VarList.VarByName(UpperCase(Trim(N)), V);
end; // VarByName

//function TScript.VarByIndex(idx: integer; var V: TVar): Boolean;
//begin
//  Result := (idx >= 0) and (idx < VarList.Count);
//  if Result then
//    V := VarList[idx];
//end; // VarByIndex

function TScript.SetVar(V: TVar): Boolean;
begin
  if VarList.ContainsKey(V.Name) then begin
    VarList.AddOrSetValue(V.Name, V.Value);
    Result := True;
  end else begin
    Result := ValidName(V.Name);
    if Result then
      VarList.AddOrSetValue(V.Name, V.Value);
  end;
end; // SetVar

function TScript.SetValue(N: String; V: variant): Boolean;
var VTemp: TVar;
begin
  VTemp.Name := UpperCase(Trim(N));
  VTemp.Value := V;
  Result := SetVar(VTemp);
end; // SetValue

function TScript.SetFunction(Name: String; F: TFunction): Boolean;
var Func: TFunc;
begin // SetFunction
  Name := UpperCase(Trim(Name));
  if not ValidName(Name) then
    Exit(False);
  Func.Name := Name;
  Func.Func := F;
  FuncList.AddOrSetValue(Name, Func);
  Result := True;
end; // SetFunction

function TScript.Calculate(S: String; var R: TVar): Boolean;
begin
  if Pos(TokenDelimiter, S) = 0 then begin
    if LastString <> S then
      LastParsed := Parse(S);
    LastString := S;
    Result := Calc(LastParsed, R);
  end else
    Result := Calc(S, R);
end; // Calculate

function TScript.Calc(S: String; var R: TVar): Boolean;
var
  ITmp: integer;
  TTmp: TToken;
  ETmp: String;
begin // Calc
  Result := False;
  VarClear(R.Value);
  ITmp := ExprIndex;
  ETmp := Expr;
  TTmp := TokenCode;

  Expr := S;
  ExprIndex := 1;

  Token := GetToken(Expr, ExprIndex, TokenCode);
  if TokenCode = tEmpty then begin
    Error('Empty string - not expression', Expr, errExprSyntax);
    Exit;
  end;

  Level1(R);

  Token := GetToken(Expr, ExprIndex, TokenCode);

  Result := (ErrCode = errOK) and (TokenCode = tEMPTY);
  TokenCode := TTmp;
  ExprIndex := ITmp;
  Expr := ETmp;
end; // Calc

procedure TScript.Level1(var R: TVar); (* логическое OR или XOR *)
var
  op: TToken;
  hold: TVar;
begin // Level1
  Level2(R);
  if ErrCode <> errOK then
    Exit;
  while (TokenCode = tOR) or (Tokencode = tXOR) do begin
    op := TokenCode;
    Token := GetToken(Expr, ExprIndex, TokenCode);
    Level2(hold);
    if ErrCode <> errOK then
      exit;
    Arith(op, R, hold);
  end;
end; // Level1

procedure TScript.Level2(var R: TVar); (* логическое И *)
var
  op: TToken;
  hold: TVar;
begin // Level2
  Level3(R);
  if ErrCode <> errOK then
    Exit;
  while TokenCode = tAND do begin
    op := TokenCode;
    Token := GetToken(Expr, ExprIndex, TokenCode);
    Level3(hold);
    if ErrCode <> errOK then
      exit;
    Arith(op, R, hold);
  end;
end; // Level2

procedure TScript.Level3(var R: TVar); (* сравнение     *)
var
  op: TToken;
  hold: TVar;
begin
  Level4(R);
  if ErrCode <> errOK then
    Exit;
  while (TokenCode = tLS) or (TokenCode = tGT)
    or (TokenCode = tLSE) or (TokenCode = tGTE)
    or (TokenCode = tEQU) or (TokenCode = tNEQ) do begin
    op := TokenCode;
    Token := GetToken(Expr, ExprIndex, TokenCode);
    level4(hold);
    if ErrCode <> errOK then
      exit;
    Arith(op, R, hold);
  end;
end; // Level3

procedure TScript.Level4(var R: TVar); (* сложение, вычитание *)
var
  op: TToken;
  hold: TVar;
begin
  Level5(R);
  if ErrCode <> errOK then
    Exit;
  while (TokenCode = tADD) or (TokenCode = tSUB) do begin
    op := TokenCode;
    Token := GetToken(Expr, ExprIndex, TokenCode);
    Level5(hold);
    if ErrCode <> errOK then
      exit;
    arith(op, R, hold);
  end;
end; // Level4

procedure TScript.Level5(var R: TVar); (* унарные операции *)
var
  op: TToken;
begin
  op := tEMPTY;
  if (TokenCode = tADD) or (TokenCode = tSUB) or (TokenCode = tNOT) then begin
    op := TokenCode;
    Token := GetToken(Expr, ExprIndex, TokenCode);
  end;
  Level6(R);
  if ErrCode <> errOK then
    Exit;
  if op > tEMPTY then
    Unary(op, R);
end; // Level5

procedure TScript.Level6(var R: TVar); (* умножение, деление, div, mod *)
var
  op: TToken;
  hold: TVar;
begin
  Level7(R);
  if ErrCode <> errOK then
    Exit;
  while TokenCode in [tMUL, tDIVIDE, tDIV, tMOD] do begin
    op := TokenCode;
    Token := GetToken(Expr, ExprIndex, TokenCode);
    Level7(hold);
    if ErrCode > 0 then
      Exit;
    Arith(op, R, hold);
  end;
end; // Level6

procedure TScript.Level7(var R: TVar); (* возв. в степень *)
var
  op: TToken;
  hold: TVar;
begin
  Level8(R);
  if ErrCode <> errOK then
    Exit;
  while (TokenCode = tPWR) or (TokenCode = tSHL) or (TokenCode = tSHR) do begin
    op := TokenCode;
    Token := GetToken(Expr, ExprIndex, TokenCode);
    Level8(hold);
    if ErrCode <> errOK then
      Exit;
    Arith(op, R, hold);
  end;
end; // Level7

procedure TScript.Level8(var R: TVar); (* круглые скобки *)
var
  T: TToken;
  i: integer;
  E: Extended;
  arr1, arr2: Boolean;
  S, Nm, ind1, ind2: String;
begin // Level8
  arr1 := False;
  arr2 := False;

  if TokenCode = tLBR then begin
    Token := GetToken(Expr, ExprIndex, TokenCode);
    Level1(R);
    if ErrCode <> errOK then
      Exit;
    if TokenCode <> tRBR then begin
      Error('''' + ResWords[tRBR] + ''' expected', Expr, errParentheses);
      Exit;
    end;
    Token := GetToken(Expr, ExprIndex, TokenCode);
  end else begin
    case TokenCode of

      tTRUE:
        begin
          R.Name := ResWords[tTRUE];
          R.Value := True;
        end;

      tFALSE:
        begin
          R.Name := ResWords[tFALSE];
          R.Value := False;
        end;

      tVR:
        begin
          Nm := Token;
          T := NextToken(Expr, ExprIndex);
          if (T = tLBR) or FuncList.ContainsKey(Nm) or ProcList.ContainsKey(Nm) then begin
            s := GetFuncParams(Expr, ExprIndex);

            if ProcList.ContainsKey(Nm) then begin
              if not CallProc(Nm, s, R) then begin
                Error('Error in procedure: ' + Nm, Expr, errCall);
                Exit;
              end;
            end else if not CallFunc(Nm, s, R) then begin
              Error('Error in function: ' + Nm, Expr, errCall);
              Exit;
            end;

          end else begin
            ind1 := '';
            ind2 := '';

            if T = tLARR then begin
              ind1 := GetIndex(Expr, ExprIndex, TokenCode);
              if ErrCode <> errOK then
                Exit;
              arr1 := True;
            end;

            T := NextToken(Expr, ExprIndex);
            if T = tLARR then begin
              ind2 := GetIndex(Expr, ExprIndex, TokenCode);
              if ErrCode <> errOK then
                Exit;
              arr2 := True;
            end;

            if arr1 and (not arr2) and VarExists(Nm) then begin
              arr1 := False;
              arr2 := True;
              ind2 := ind1;
            end;

            if arr1 then begin
              if VarExists(Nm) then begin
                Error('Variable with same name exist : ' + Nm, Expr, errNameDup);
                Exit;
              end;
              Nm := Nm + '[' + ind1 + ']';
            end else begin
              if FindArray(Nm) then begin
                Error('Array with same name exist : ' + Nm, Expr, errNameDup);
                Exit;
              end;
            end;

            if not VarList.TryGetValue(Nm, R.Value) then begin
              Error('Variable not found: ' + Nm, Expr, errVarNotFound);
              Exit;
            end;

            if arr2 then begin
              if not VarIsString(R) then begin
                Error('Invalid variable type: ' + Nm, Expr, errTypeCast);
                Exit;
              end;

              if ind2 = '' then begin
                Error('Empty string index', Expr, errIndexRange);
                Exit;
              end;

              if Pos(',', ind2) > 0 then begin
                Error('Invalid string index', Expr, errIndexRange);
                Exit;
              end;

              try
                i := StrToInt(ind2);
              except
                Error('Invalid string index : ' + ind2, Expr, errIndexRange);
                Exit;
              end;

              if (i < 1) or (i > Length(String(R.Value))) then begin
                Error('String index out of range: ' + ind2, Expr, errIndexRange);
                Exit;
              end;

              R.Value := String(R.Value)[i];
            end;
          end;

          Token := GetToken(Expr, ExprIndex, TokenCode);
        end;

      tCON:
        begin
          if (Token[1] <> StrDelimiter) then begin
            if not StrToNum(Token, E) then begin
              Error('Invalid number: ' + Token, Expr, errExprSyntax);
              Exit;
            end;

            if (Pos(DecimalPoint, Token) = 0)
              and (E < MaxInt) and (E > -MaxInt) then
              R.Value := ExtToInt(E)
            else
              R.Value := E;

          end else begin
            if (Length(Token) < 2) or (Token[Length(Token)] <> StrDelimiter) then begin
              Error('Unterminated string: ' + Token, Expr, errString);
              Exit;
            end;
            R.Value := copy(Token, 2, Length(Token) - 2);
          end;
          Token := GetToken(Expr, ExprIndex, TokenCode);
        end;
    else
      begin
        Error('Unknown or invalid operator in expression: ' + Token, Expr, errExprSyntax);
      end;
    end;
  end;
end; // Level8

procedure TScript.Arith(o: TToken; var R, H: TVar);
begin
  try
    case O of
      tOR:     R.Value := R.Value or H.Value;
      tAND:    R.Value := R.Value and H.Value;
      tXOR:    R.Value := R.Value xor H.Value;
      tEQU:    R.Value := R.Value = H.Value;
      tNEQ:    R.Value := R.Value <> H.Value;
      tGT:     R.Value := R.Value > H.Value;
      tLS:     R.Value := R.Value < H.Value;
      tGTE:    R.Value := R.Value >= H.Value;
      tLSE:    R.Value := R.Value <= H.Value;
      tADD:    R.Value := R.Value + H.Value;
      tSUB:    R.Value := R.Value - H.Value;
      tMUL:    R.Value := R.Value * H.Value;
      tDIVIDE: R.Value := R.Value / H.Value;
      tDIV:    R.Value := R.Value div H.Value;
      tMOD:    R.Value := R.Value mod H.Value;
      tPWR:    R.Value := Power(R.Value, H.Value);
      tSHL:    R.Value := R.Value shl H.Value;
      tSHR:    R.Value := R.Value shr H.Value;
    end;
  except
    Error('Invalid operand types for "' + ResWords[o] + '"', Expr, errTypeCast);
    Exit;
  end;
end; // Arith

procedure TScript.Unary(o: TToken; var R: TVar);
begin
  if o = tSUB then
    try
      R.Value := -R.Value;
      Exit;
    except
      Error('Invalid operand types for "' + ResWords[o] + '"', Expr, errTypeCast);
      Exit;
    end;

  if o = tNOT then
    try
      R.Value := not R.Value;
      Exit;
    except
      Error('Invalid operand types for "' + ResWords[o] + '"', Expr, errTypeCast);
      Exit;
    end;
end; // Unary

procedure TScript.SetVarDirect(const R: TVar);
begin
  SetVar(R);
end; // SetVarDirect

function TScript.FindArray(const N: String): Boolean;
var
  L: integer;
  s, key: String;
begin
  L := Length(N) + 1;
  s := N + '[';
  for key in VarList.Keys do
    if CmpStr(Key, S, 1, L) then
      Exit(True);
  Result := False;
end; // FindArray

function TScript.GetIndex(S: String; var Index: integer; var T: TToken): String;
var
  R: TVar;
  tt: TToken;
  i, i1, a, b: integer;
  ss, st, ind: String;
begin // GetIndex
  Result := '';

  if Index > Length(S) then begin
    Error('Invalid string index', S, errExprSyntax);
    Exit;
  end;

  Token := GetToken(S, Index, T);
  if T <> tLARR then begin
    Error(ResWords[tLARR] + ' expected', Expr, errManyRbr);
    Exit;
  end;

  ss := '';
  a := 1;
  repeat
    Token := GetToken(S, Index, T);
    if T = tLARR then
      inc(a);
    if T = tRARR then
      dec(a);
    if T = tEMPTY then
      break;
    if a = 0 then
      break;
    ss := ss + TokenStr(T, Token);
  until False;

  if a > 0 then begin
    Error(ResWords[tRARR] + ' expected', S, errRbrExpected);
    Exit;
  end;

  i1 := 1;
  a := 0;
  b := 0;
  st := '';
  ind := '';
  repeat
    st := GetToken(ss, i1, tt);
    if tt = tRBR then
      inc(b);
    if tt = tLBR then
      dec(b);
    if tt = tLARR then
      inc(a);
    if tt = tRARR then
      dec(a);

    if (a = 0) and (b = 0) and ((tt = tCOMMA) or (tt = tEMPTY)) then begin
      if not Calc(ind, R) then begin
        Error('Error in expression: ' + UnParse(ind, True), S, errExprSyntax);
        Exit;
      end;

      try
        i := R.Value;
      except
        Error('Invalid index: ' + UnParse(ind, True), S, errTypeCast);
        Exit;
//        i := 0; // fake, чтобы не было warnings
      end;

      if Result = '' then
        Result := IntToStr(i)
      else
        Result := Result + ',' + IntToStr(i);

      ind := '';
    end
    else
      ind := ind + TokenStr(tt, st);
    if tt = tEMPTY then
      break;
  until False;
end; // GetIndex

function TScript.GetFuncParams(S: String; var Index: integer): String;
var
  t: TToken;
  i, j: integer;
  st, ss, p: String;
begin // GetFuncParams
  Result := '';
  if Index > Length(s) then
    Exit;

  i := Index;
  Token := GetToken(s, Index, TokenCode);

  if TokenCode <> tLBR then begin
    Index := i;
    Exit;
  end;

  i := 1;
  st := '';
  repeat
    Token := GetToken(s, Index, TokenCode);
    if TokenCode = tLBR then
      inc(i);
    if TokenCode = tRBR then
      dec(i);
    if (TokenCode = tEMPTY) or (i = 0) then
      break;
    st := st + TokenStr(TokenCode, Token);
  until False;

  if i <> 0 then begin
    Error('Invalid function params: ' + UnParse(st, True), s, errExprSyntax);
    Exit;
  end;

  p := '';
  i := 1;
  j := 0;
  repeat
    ss := GetToken(st, i, t);
    if t = tEMPTY then
      break;
    if t = tRBR then
      inc(j);
    if t = tLBR then
      dec(j);
    if (j = 0) and (t = tCOMMA) then
      p := p + #13#10
    else
      p := p + TokenStr(t, ss);
  until False;
  Result := p;
end; // GetFuncParams

// здесь только встроенные функции и процедуры
function TScript.CallFunc(const FuncName: String; ParamString: String; var V: TVar): Boolean;
var
  i{, j}: integer;
  slParamStrings, slVars: TStringList;
  VarListLocals: TList<TVar>;
  VR: TVar;
  Param: TVar;
  s, err: String;
begin // CallFunc
  Result := False;

  if not FuncList.ContainsKey(FuncName) then begin
    Error('Unknown function: ' + FuncName, Expr, errFuncNotFound);
    Exit;
  end;

  slParamStrings := TStringList.Create;
  slVars := TStringList.Create;
  slParamStrings.Text := ParamString;
  VarListLocals := TList<TVar>.Create;

  // вычисляем параметры
  for i := 0 to slParamStrings.Count - 1 do
    if Calc(slParamStrings[i], VR) then begin
      Param := VR;
//      j := -1;
      s := UnParse(Trim(slParamStrings[i]), False);

      if ValidName(s) and VarExists(s) then begin
//        j := VarIndex(s);
//      if j >= 0 then
        Param.Name := 'VAR';
        slVars.Add(s);
      end else begin
        Param.Name := 'VALUE';
        slVars.Add('');
      end;
      VarListLocals.Add(Param);
    end else begin
      Error('Invalid expression in function params: ' + slParamStrings[i], Expr, errCall);
      slParamStrings.Free;
      slVars.Free;
      VarListLocals.Free;
      Exit;
    end;

  err := '';
  try
    // тело функции
    Result := FuncList[FuncName].Func(VarListLocals, V);
  except
    on E:Exception do
    begin
      Result := False;
//      err := 'Error in function ' + FuncName + ' (pascalc.pas, line:1594)';
      err := E.Message;
    end;
  end;

  if Result then begin
    // если параметры - VAR, то присваиваем значения
    for i := 0 to slParamStrings.Count - 1 do
      if slVars[i] <> '' then
        SetValue(slVars[i], VarListLocals[i].Value);
  end else begin
    s := 'Invalid parameters, function ' + FuncName;
    if err <> '' then
      s := s + ' : ' + err;
    Error(s, Expr, 7);
  end;

  slParamStrings.Destroy;
  slVars.Free;
  VarListLocals.Destroy;
  Result := True;
end; // CallFunc

{// здесь только встроенные функции и процедуры
function TScript.CallFunc(FuncName: String; ParamString: String; var V: TVar): Boolean;
var
  i, j, iFunc: integer;
  slParamStrings: PStrListEx;
  VarListLocals: TVarList;
  VR: TVar;
  P: PVar;
  s, err: String;
begin // CallFunc
  Result := False;
  iFunc := FindFunc(FuncName);

  if iFunc < 0 then begin
    Error('Unknown function: ' + FuncName, Expr, errFuncNotFound);
    Exit;
  end;

  slParamStrings := NewStrListEx;
  slParamStrings.Text := ParamString;
  VarListLocals := NewVarList;

  // вычисляем параметры
  for i := 0 to slParamStrings.Count - 1 do
    if Calc(slParamStrings.Items[i], VR) then begin
      New(P);
      P^ := VR;
      j := -1;
      s := UnParse(Trim(slParamStrings.Items[i]), False);

      if ValidName(s) then
        j := VarIndex(s);
      if j >= 0 then
        P^.Name := 'VAR'
      else
        P^.Name := 'VALUE';
      slParamStrings.Objects[i] := j;
      VarListLocals.Add(P);
    end else begin
      Error('Invalid expression in function params: ' + slParamStrings.Items[i], Expr, errCall);
      slParamStrings.Destroy;
      VarListLocals.Destroy;
      Exit;
    end;

  err := '';
  try
    // тело функции
    Result := PFunction(PFunc(FuncList.Items[iFunc])^.Func)(Self, VarListLocals, V);
  except
    on E:Exception do
    begin
      Result := False;
//      err := 'Error in function ' + FuncName + ' (pascalc.pas, line:1594)';
      err := E.Message;
    end;
  end;

  if Result then begin
    // если параметры - VAR, то присваиваем значения
    for i := 0 to slParamStrings.Count - 1 do begin
      j := integer(slParamStrings.Objects[i]);
      if (j >= 0) and (i < VarListLocals.Count) and (j < VarList.Count) then begin
        s := VarList.Items[j]^.Name;
        VarList.Items[j]^ := VarListLocals.Items[i]^;
        VarList.Items[j]^.Name := s;
      end;
    end;
  end else begin
    s := 'Invalid parameters, function ' + FuncName;
    if err <> '' then
      s := s + ' : ' + err;
    Error(s, Expr, 7);
  end;


  slParamStrings.Destroy;
  VarListLocals.Destroy;
end; // CallFunc}

// упрощенная процедура - без возврата данных и без локальных переменных
function TScript.CallProc(ProcName: String; ParamString: String; var V: TVar): Boolean;
var
  i: integer;
  PRoc: TProcedure;
  VR: TVar;
  slParamStrings: TStringList;
  slParams: TStringList;
begin // CallProc
  Result := False;
  if not ProcList.TryGetValue(ProcName, Proc) then begin
    Error('Unknown procedure: ' + ProcName, Expr, errFuncNotFound);
    Exit;
  end;

  // создаем переменные
  slParamStrings := TStringList.Create;
  slParams := TStringList.Create;
//  VarListTemp := TVarList.Create;
//  VarListTemp2 := TVarList.Create;

  try

    // сохраняем глобальные переменные во временный список
//    VarList.CopyTo(VarListTemp);

    // проверяем, совпадает ли количество параметров
    slParamStrings.Text := ParamString;
    slParams.Text := Proc.Params;
    if slParams.Count <> slParamStrings.Count then begin
      Error('Wrong parameters amount, procedure ' + ProcName, Expr, errCall);
      Exit;
    end;

    // Присваиваем параметры локальным переменным (SetVar(VR))
    for i := 0 to slParamStrings.Count - 1 do begin
      if Calc(slParamStrings[i], VR) then begin
        VR.Name := slParams[i];
        if not SetVar(VR) then begin
          Error('Duplicate variable name: ' + VR.Name, Expr, errNameDup);
//          VarList.ClearAll;
//          VarListTemp.CopyTo(VarList);
          Exit;
        end;
      end else begin
        Error('Invalid expression in function params: ' + slParamStrings[i], Expr, errCall);
//        VarList.ClearAll;
//        VarListTemp.CopyTo(VarList);
        Exit;
      end;
    end;

    // если это функция, удаляем Result (надо быть уверенным, что Result присвоен, и чтобы тип его был правильным)
    if Proc.Result then
      VarRemove('RESULT');

    // выполняем процедуру
    Exec(Proc.Body);

    // ???
    if BlockCmd <> tEMPTY then begin
      if BlockCmd = tEXIT then
        BlockCmd := tEMPTY
      else begin
        if BlockCmd in [tBREAK, tCONTINUE] then
          Error(ResWords[BlockCmd] + ' outside loop', Proc.Body, errBreak);

        if BlockCmd = tGOTO then
          Error('Label ' + GotoLabel + ' not exist or unreacheble', Proc.Body, errLabelNotFound);

        Exit;
      end;
    end;

    // восстанавливаем глобальные переменные
//    VarList.CopyTo(VarListTemp2);
//    VarList.ClearAll;
//    VarListTemp.CopyTo(VarList);

    // если параметры - VAR, то присваиваем значения
(*    for i := 0 to slParams.Count - 1 do
      if VarListTemp2.VarByName(slParams.Items[i], VR) then begin
        j := -1;
        s := UnParse(Trim(slParamStrings.Items[i]), False);
        if ValidName(s) then
          j := VarIndex(s);
        if j >= 0 then begin
          VR.Name := s;
          VarList.Items[j]^ := VR;
        end;
      end;*)

    // Для функций - присваиваем результат
    if Proc.Result then begin
//      if VarListTemp2.VarByName('RESULT', VR) then
      if VarByName('RESULT', VR) then
        V.Value := VR.Value
      else begin
        Error('Function must return result : ' + Proc.Name, Expr, errFuncResult);
//        VarList.ClearAll;
//        VarListTemp.CopyTo(VarList);
        Exit;
      end;
    end;

    Result := True;
  finally
    slParamStrings.Free;
//    VarListTemp.Free;
    slParams.Free;
//    VarListTemp2.Free;
  end;
end; // CallProc

{function TScript.CallProc(ProcName: String; ParamString: String; var V: TVar): Boolean;
var
  i, j: integer;
  s: String;
  PRoc: TProcedure;
  VR: TVar;
  VarListTemp: TVarList;
  VarListTemp2: TVarList;
  slParamStrings: PStrList;
  slParams: PStrList;
begin // CallProc
  Result := False;
  if not ProcList.ProcByName(ProcName, Proc) then begin
    Error('Unknown procedure: ' + ProcName, Expr, errFuncNotFound);
    Exit;
  end;

  // создаем переменные
  slParamStrings := NewStrList;
  slParams := NewStrList;
  VarListTemp := NewVarList;
  VarListTemp2 := NewVarList;

  try

    // сохраняем глобальные переменные во временный список
    VarList.CopyTo(VarListTemp);

    // проверяем, совпадает ли количество параметров
    slParamStrings.Text := ParamString;
    slParams.Text := Proc.Params;
    if slParams.Count <> slParamStrings.Count then begin
      Error('Wrong parameters amount, procedure ' + ProcName, Expr, errCall);
      Exit;
    end;

    // Присваиваем параметры локальным переменным (SetVar(VR))
    for i := 0 to slParamStrings.Count - 1 do begin
      if Calc(slParamStrings.Items[i], VR) then begin
        VR.Name := slParams.Items[i];
        if not SetVar(VR) then begin
          Error('Duplicate variable name: ' + VR.Name, Expr, errNameDup);
          VarList.ClearAll;
          VarListTemp.CopyTo(VarList);
          Exit;
        end;
      end else begin
        Error('Invalid expression in function params: ' + slParamStrings.Items[i], Expr, errCall);
        VarList.ClearAll;
        VarListTemp.CopyTo(VarList);
        Exit;
      end;
    end;

    // если это функция, удаляем Result (надо быть уверенным, что Result присвоен, и чтобы тип его был правильным)
    if Proc.Result then begin
      i := VarIndex('RESULT');
      if i >= 0 then begin
        Dispose(Varlist.Items[i]);
        VarList.Delete(i);
      end;
    end;

    // выполняем процедуру
    Exec(Proc.Body);

    // ???
    if BlockCmd <> tEMPTY then begin
      if BlockCmd = tEXIT then
        BlockCmd := tEMPTY
      else begin
        if BlockCmd in [tBREAK, tCONTINUE] then
          Error(ResWords[BlockCmd] + ' outside loop', Proc.Body, errBreak);

        if BlockCmd = tGOTO then
          Error('Label ' + GotoLabel + ' not exist or unreacheble', Proc.Body, errLabelNotFound);

        Exit;
      end;
    end;

    // восстанавливаем глобальные переменные
    VarList.CopyTo(VarListTemp2);
    VarList.ClearAll;
    VarListTemp.CopyTo(VarList);

    // если параметры - VAR, то присваиваем значения
    for i := 0 to slParams.Count - 1 do
      if VarListTemp2.VarByName(slParams.Items[i], VR) then begin
        j := -1;
        s := UnParse(Trim(slParamStrings.Items[i]), False);
        if ValidName(s) then
          j := VarIndex(s);
        if j >= 0 then begin
          VR.Name := s;
          VarList.Items[j]^ := VR;
        end;
      end;

    // Для функций - присваиваем результат
    if Proc.Result then begin
      if VarListTemp2.VarByName('RESULT', VR) then
        V.Value := VR.Value
      else begin
        Error('Function must return result : ' + Proc.Name, Expr, errFuncResult);
        VarList.ClearAll;
        VarListTemp.CopyTo(VarList);
        Exit;
      end;
    end;

    Result := True;
  finally
    slParamStrings.Free;
    VarListTemp.Free;
    slParams.Free;
    VarListTemp2.Free;
  end;
end; // CallProc }

function TScript.GetTextToken(S: String; var Index: integer; var Code: TToken): String;
var
  t: TToken;
  i, j, sl, tl: integer;
begin // GetTextToken
  Code := tEMPTY;
  Result := '';
  sl := Length(s);
  while (Index <= sl) and CharInSet(s[Index], SpaceSet) do
    inc(Index);

  if (Index > sl) then
    Exit;
  for t := Low(TToken) to High(TToken) do begin
    tl := Length(ResWords[t]);
    if tl = 0 then
      continue;
    if CmpStr(ResWords[t], s, Index, tl)
      and (tl > Length(Result))
      and ((index + tl > sl)
      or not CharInSet(s[index + tl], Alpha)
      or not IsAlpha(ResWords[t])) then begin
      Result := ResWords[t];
      Code := t;
    end;
  end;

  if Code <> tEMPTY then begin
    Index := Index + Length(Result);
    Exit;
  end;

  i := Index;

  if s[i] <> StrDelimiter then begin

    while (Index <= sl) do begin
      if CharInSet(s[Index], SpaceSet)
        or ((Index > i) and not CharInSet(s[Index], Alpha)
        and (s[Index] <> DecimalPoint)) then
        break;
      inc(Index);
    end;
    Result := Trim(copy(s, i, Index - i));

    //1.2E-16
    if (Result <> '') and CharInSet(Result[1], ['0'..'9']) and (Result[Length(Result)] = 'E') then begin
      while (Index <= sl) do begin
        if not CharInSet(s[Index], ['-', '+', '0'..'9']) then
          break;
        inc(Index);
      end;
      Result := Trim(copy(s, i, Index - i));
    end;


    if (Result <> '') and CharInSet(Result[1], ['0'..'9']) then begin
      j := Pos(ResWords[tDOT2], Result);
      if j > 0 then begin
        Result := Trim(copy(Result, 1, j - 1));
        Index := i + Length(Result);
      end;
    end;

  end else begin
    inc(Index);
    while (Index <= sl) and (s[Index] <> StrDelimiter) do
      inc(Index);
    Result := Trim(copy(s, i, Index - i + 1));
    inc(Index);
  end;

  if Result = '' then
    Exit;

  if ((Result[1] < '0') or (Result[1] > '9'))
    and (Result[1] <> StrDelimiter) then
    Code := tVR
  else
    Code := tCON;
end; // GetTextToken

function TScript.TokenStr(T: TToken; S: String): String;
begin
  Result := TokenDelimiter + Char(byte(T) + $40) + S;
end;

function TScript.DelRemarks(S: String): String;
var
  st, ss: String;
  i, rr, rb, re: integer;
  InString, InRem: Boolean;
begin
  st := UpperCase(S);
  InRem := False;
  InString := False;

  rr := Length(ResWords[tREM]);
  rb := Length(ResWords[tREMB]);
  re := Length(ResWords[tREME]);

  i := 1;
  while i <= Length(st) do begin
    if not InRem then
      if st[i] = StrDelimiter then
        InString := not InString;

    if not InString then begin
      if CmpStr(ResWords[tREM], st, i, rr) then
        InRem := True;
      if CmpStr(ResWords[tREMB], st, i, rb) then
        InRem := True;
      if CmpStr(ResWords[tREME], st, i, re) then begin
        InRem := False;
        i := i + re;
        continue;
      end;
      if (st[i] = #$A) or (st[i] = #$D) then
        InRem := False;
    end;
    if not InRem then
      if InString then
        ss := ss + S[i]
      else
        ss := ss + AnsiUpperCase(S[i]);
    inc(i);
  end;
  Result := ss;
end; // DelRemarks

function TScript.Parse(S: String): String;
var
  i: integer;
  T: TToken;
  ts, ss: String;
begin
  Result := '';
  i := 1;
  ss := DelRemarks(S);
  repeat
    TS := GetTextToken(ss, i, T);
    if T = tEMPTY then
      break;
    Result := Result + TokenStr(T, TS);
  until False;
end; // Parse

function TScript.UnParse(S: String; Show: Boolean): String;
var
  i: integer;
  T: TToken;
begin
  i := 1;
  Result := '';
  repeat
    Result := Result + GetToken(s, i, T);
    if Show then
      Result := Result + ' ';
    if t = tEMPTY then
      break;
  until False;
end; // UnParse

function TScript.GetToken(S: String; var Index: integer; var Code: TToken): String;
var
  sl, i: integer;
begin
  Code := tEMPTY;
  Result := '';
  sl := Length(s);
  if Index + 1 > sl then
    Exit;
  if s[Index] <> TokenDelimiter then
    Exit;
  inc(Index);
  Code := TToken(byte(s[Index]) - $40);
  i := Index + 1;
  while (Index <= sl) and (s[Index] <> TokenDelimiter) do
    inc(Index);
  Result := Result + copy(s, i, Index - i);
end; // GetToken

function TScript.GetTokenCode(S: String; var Index: integer; var Code: TToken): integer;
var
  sl: integer;
begin
  Result := Index;
  Code := tEMPTY;
  sl := Length(s);
  if Index + 1 > sl then
    Exit;
  if s[Index] <> TokenDelimiter then
    Exit;
  Result := Index;
  inc(Index);
  Code := TToken(byte(s[Index]) - $40);
  while (Index <= sl) and (s[Index] <> TokenDelimiter) do
    inc(Index);
end; // GetTokenCode

function TScript.GetTokenLine(S: String; var Index: integer; var Code: TToken;
  StopToken: TTokenSet): String;
var
  i, n: integer;
begin
  i := Index;
  repeat
    n := GetTokenCode(S, Index, Code);
    if Code = tEMPTY then
      break;
  until Code in StopToken;
  Result := copy(s, i, n - i);
end; // GetTokenLine

function TScript.NextToken(S: String; Index: integer): TToken;
var
  i: integer;
  t: tToken;
begin
  i := Index;
  GetTokenCode(S, i, t);
  Result := t;
end; // NextToken

function TScript.GetOperator(Txt: String; var Index: integer; EndToken: TTokenSet): String;
var
  t: TToken;
  s: String;
  Level, RLevel: integer;
begin
  Level := 0;
  RLevel := 0;
  Result := '';
  repeat
    s := GetToken(Txt, Index, t);
    if (t = tSEMI) and (Result = '') then
      continue;
    if (t = tEMPTY) or ((t in EndToken) and (Level = 0) and (RLevel = 0)) then
      break;
    if (t = tBEGIN) or (t = tCASE) then
      inc(Level);
    if t = tEND then
      dec(Level);
    if t = tREPEAT then
      inc(RLevel);
    if t = tUNTIL then
      dec(RLevel);
    Result := Result + TokenStr(t, s);
  until False;
  if Level > 0 then
    Error(ResWords[tEND] + ' expected', Result, errEndExpected);
  if Level < 0 then
    Error('Too many ' + ResWords[tEND], Result, errManyEnd);
  if RLevel > 0 then
    Error(ResWords[tUNTIL] + ' expected', Result, errUntilExpected);
  if Level < 0 then
    Error('Too many ' + ResWords[tUNTIL], Result, errManyUntil);
end; // GetOperator

function TScript.ParseOperator(Txt: String; var Cmd, Line, Lbl: String): TToken;
var
  i, n: integer;
  t: TToken;
  s: String;
begin
  s := Txt;
  Lbl := '';
  i := 1;
  Cmd := GetToken(s, i, t);
  Result := t;
  n := i;
  GetToken(s, n, t);
  if t = tLBL then begin
    i := n;
    Lbl := Cmd;
    Cmd := GetToken(s, i, t);
    Result := t;
  end;
  Line := copy(s, i, Length(s));
end; // ParseOperator

procedure TScript.Exec(Txt: String);
label
  EXECUTE;
var
  t: TToken;
  Ind: integer;
  c, s, l, CmdLine: String;
begin
  Ind := 1;
  Inc(BlockLevel);
  repeat
    if ErrCode <> errOK then
      break;
    CmdLine := GetOperator(Txt, Ind, [tSEMI]);
    if CmdLine = '' then
      break;
    t := ParseOperator(CmdLine, c, s, l);
    EXECUTE:
    case t of
      tVR: DoSet(CmdLine, c, s);
      tIF: DoIf(CmdLine, s);
      tBEGIN: DoBegin(CmdLine, s);
      tEND: Error('Too many ' + ResWords[tEND], CmdLine, errManyEnd);
      tELSE: Error(ResWords[tELSE] + ' without ' + ResWords[tIF], CmdLine, errInvalidOp);
      tFOR: DoFor(CmdLine, s);
      tWHILE: DoWhile(CmdLine, s);
      tREPEAT: DoRepeat(CmdLine, s);
      tBREAK: DoBreak(CmdLine, s);
      tCONTINUE: DoContinue(CmdLine, s);
      tEXIT: DoExit(CmdLine, s);
      tGOTO: DoGoto(CmdLine, s);
      tCASE: DoCase(CmdLine, s);
    else
      Error('Invalid operator: ' + c, CmdLine, errInvalidOp);
    end;

    Process;

    if BlockCmd <> tEMPTY then begin
      if (BlockLevel = 0) and (BlockCmd in [tBREAK, tCONTINUE]) then
        Error(ResWords[BlockCmd] + ' outside loop', CmdLine, errBreak);

      if BlockCmd = tGOTO then begin
        Ind := 1;
        repeat
          CmdLine := GetOperator(Txt, Ind, [tSEMI]);
          if CmdLine = '' then
            break;
          t := ParseOperator(CmdLine, c, s, l);
          if l = GotoLabel then begin
            BlockCmd := tEMPTY;
            goto EXECUTE;
          end;
        until False;
      end;
      break;
    end;
  until False;
  Dec(BlockLevel);
  if (BlockLevel < 0) and (BlockCmd = tGOTO) then
    Error('Label ' + GotoLabel + ' not exist or unreacheble', CmdLine, errLabelNotFound);
end; // Exec

function TScript.PreProcess(Txt: String): String;
var
  T, TT: TToken;
  Proc{, P}: TProcedure;
  Index, i, j: integer;
  h: THandle;
  sOperator, CmdLine, Op, Lbl: String;
  s: String;
  AnsiS: AnsiString;
  V: TVar;
begin // PreProcess
  Index := 1;
  Result := '';
  repeat
    sOperator := GetOperator(Txt, Index, [tSEMI]);
    if sOperator = '' then
      break;
    j := 1;
    GetTokenCode(sOperator, j, T);
    case T of
      tPROC, tFUNCT:
        begin
          ParseOperator(sOperator, CmdLine, Op, Lbl);

          i := 1;
          Proc.Name := AnsiUpperCase(GetToken(Op, i, TT));

          if TT <> tVR then begin
            Error('Invalid procedure name', sOperator, errInvalidName);
            Exit;
          end;

          // commented by BM on 28.11.2020 because of Collider Octahedron (twice LoadFrom)
//          if ProcList.ContainsKey(Proc.Name) then begin
//            Error('Duplicate procedure name', sOperator, errNameDup);
//            Exit;
//          end;

          GetToken(Op, i, TT);
          if TT <> tLBR then begin
            Error(ResWords[tLBR] + ' expected', sOperator, errParentheses);
            Exit;
          end;

          Proc.Body := '';
          Proc.Params := '';

          repeat
            s := GetToken(Op, i, TT);
            if TT = tRBR then
              break;
            if TT <> tVR then begin
              Error('Invalid function call', sOperator, errCall);
              Exit;
            end;
            if Proc.Params <> '' then
              Proc.Params := Proc.Params + #13#10;
            Proc.Params := Proc.Params + AnsiUpperCase(s);
            s := GetToken(Op, i, TT);
            if TT = tRBR then
              break;
            if TT <> tCOMMA then begin
              Error('Invalid function call', sOperator, errCall);
              Exit;
            end;
          until False;

          Proc.Body := GetOperator(Txt, Index, [tSEMI]);
          Proc.Result := T = tFUNCT;

          ProcList.AddOrSetValue(Proc.Name, Proc);
        end;

      tUSES, tINCLUDE:
        begin
          ParseOperator(sOperator, CmdLine, Op, Lbl);
          if not Calc(Op, V) then begin
            Error('Invalid filename "' + Op + '" in ' + ResWords[T] + ' statement',
              sOperator, errExprSyntax);
            Exit;
          end;

          try
            s := V.Value;
          except
            Error('Invalid filename "' + Op + '" in ' + ResWords[T] + ' statement',
              sOperator, errExprSyntax);
            Exit;
          end;

          h := FileOpen(s, fmOpenRead or fmShareDenyNone);
          if h = INVALID_HANDLE_VALUE then begin
            Error('Can''t open file "' + s + '", error code ' + IntToStr(GetLastError),
              sOperator, errFileOpen);
            Exit;
          end;

          i := FileSeek(h, 0, 2);
          SetLength(AnsiS, i + 1);
          FileSeek(h, 0, 0);
          FileRead(h, (@AnsiS[1])^, i);
          CloseHandle(h);
          AnsiS[i + 1] := #0;

          if t = tUSES then
            PreProcess(Parse(s))
          else
            Result := Result + PreProcess(Parse(String(AnsiS)));

          SetLength(AnsiS, 0);
        end;

    else
      Result := Result + sOperator + TokenStr(tSEMI, ResWords[tSEMI]);
    end;
  until False;
end; // PreProcess

function TScript.Execute(S: String): Boolean;
begin
  Clear;
//  ProcList.ClearAll; // !!!!!!!!!!
  if Pos(TokenDelimiter, S) = 0 then begin
    if LastString <> S then
      LastParsed := Parse(s);
    LastString := S;
    Exec(PreProcess(LastParsed));
  end else
    Exec(PreProcess(S));
  Result := ErrCode = errOK;
end; // Execute

procedure TScript.DoSet(CmdLine, Cmd, Line: String);
var
  R, RR: TVar;
  i, ind: integer;
  t: TToken;
  s, ss, v, ind1, ind2: String;
  arr1, arr2: Boolean;
begin // DoSet
  i := 1;
  arr1 := False;
  arr2 := False;
  Ind1 := '';
  Ind2 := '';
  V := Cmd;

  if NextToken(Line, i) = tLARR then begin
    ind1 := GetIndex(Line, i, T);
    if ErrCode <> errOK then
      Exit;
    arr1 := True;
  end;

  if NextToken(Line, i) = tLARR then begin
    ind2 := GetIndex(Line, i, T);
    if ErrCode <> errOK then
      Exit;
    arr2 := True;
  end;

  ss := GetToken(Line, i, t);

  if (t = tEQU) and (ResWords[tEQU] = ResWords[tASSIGN]) then
    t := tASSIGN;

  if (t <> tASSIGN) and (t <> tLBR) and (t <> tEMPTY) and (t <> tLARR) then begin
    Error('Found ' + ss + ', expected ' + ResWords[tASSIGN] + ', ' +
      ResWords[tLBR] + ', ' + ResWords[tLARR] + ' or ' +
      ResWords[tSEMI], Line, errExprSyntax);
    Exit;
  end;

  if t = tASSIGN then begin
    s := Trim(copy(Line, i, Length(Line)));
    v := Cmd;
  end else begin
    v := '';
    s := TokenStr(tVR, Cmd) + Line;
  end;

  if not Calc(s, R) then begin
    Error('Error in expression', s, errExprSyntax);
    Exit;
  end;

  if v <> '' then begin
    if arr1 and (not arr2) and VarExists(v) then begin
      arr1 := False;
      arr2 := True;
      ind2 := ind1;
    end;

    if not ValidName(V) then begin
      Error('Invalid variable name: ' + v, CmdLine, errInvalidName);
      Exit;
    end;

    if arr1 then begin
      if VarExists(v) then begin
        Error('Variable with same name exist : ' + v, CmdLine, errNameDup);
        Exit;
      end;
      v := v + '[' + ind1 + ']';
    end else if FindArray(v) then begin
      Error('Array with same name exist : ' + v, CmdLine, errNameDup);
      Exit;
    end;

    R.Name := V;

    if not arr2 then
      SetVarDirect(R)
    else begin
      if not VarByName(v, RR) then begin
        Error('Variable not found :' + v, CmdLine, errVarNotFound);
        Exit;
      end;

      if not VarIsString(RR) then begin
        Error('Invalid variable type: ' + v, CmdLine, errTypeCast);
        Exit;
      end;

      if ind2 = '' then begin
        Error('Empty string index', Expr, errIndexRange);
        Exit;
      end;

      if Pos(',', ind2) > 0 then begin
        Error('Invalid string index', Expr, errIndexRange);
        Exit;
      end;

      try
        ind := StrToInt(ind2);
      except
        Error('Invalid string index : ' + ind2, Expr, errIndexRange);
        Exit;
      end;

      if (ind > Length(String(RR.Value))) or (ind < 1) then begin
        Error('String index out of range: ' + ind2, CmdLine, errIndexRange);
        Exit;
      end;

      if not VarIsString(RR) then begin
        if Length(String(R.Value)) <> 1 then begin
          Error('Character expected: ''' + String(R.Value) + '''', CmdLine, errValueRange);
          Exit;
        end;
        s := String(RR.Value);
        s[ind] := String(R.Value)[1];
        RR.Value := s;
      end else begin
        try
          i := R.Value;
        except
          Error('Invalid variable type: ' + v, CmdLine, errTypeCast);
          Exit;
        end;
        if (i < 0) or (i > 255) then begin
          Error('Character value out of range: ' + IntToStr(i), CmdLine, errValueRange);
          Exit;
        end;
        s := String(RR.Value);
        s[ind] := Char(i);
        RR.Value := s;
      end;
      SetVarDirect(RR);
    end;
  end;
end; // DoSet

procedure TScript.DoIf(CmdLine, Line: String);
var
  R: TVar;
  i: integer;
  t: TToken;
  e: String;
  b: Boolean;
begin
  i := 1;
  e := GetTokenLine(Line, i, t, [tTHEN]);

  if not Calc(e, R) then begin
    Error('Error in expression', e, errExprSyntax);
    Exit;
  end;

  try
    b := R.Value;
  except
    Error('Invalid condition type', e, errTypeCast);
    Exit;
  end;

  if not b then begin
    GetOperator(Line, i, [tSEMI, tELSE]);
    e := copy(Line, i, Length(Line));
  end else
    e := GetOperator(Line, i, [tSEMI, tELSE]);

  Exec(e);
end; // DoIf

procedure TScript.DoBegin(CmdLine, Line: String);
var
  s: String;
  i: integer;
begin
  s := Trim(Line);

  if s = '' then begin
    Error(ResWords[tEND] + ' expected', CmdLine, errEndExpected);
    Exit;
  end;

  i := Length(s) - Length(TokenStr(tEND, ResWords[tEND]));

  if not CmpStr(TokenStr(tEND, ResWords[tEND]), s, i - 1,
    Length(TokenStr(tEND, ResWords[tEND]))) then begin
    s := copy(s, 1, i);
    Exec(s);
  end else begin
    Error(ResWords[tEND] + ' expected', CmdLine, errEndExpected);
    Exit;
  end;
end; // DoBegin

procedure TScript.DoFor(CmdLine, Line: String);
var
  R: TVar;
  i, j: integer;
  t: TToken;
  ForInc: Boolean;
  ForFrom, ForTo: integer;
  s, e, v: String;
begin
  i := 1;
  e := GetTokenLine(Line, i, t, [tTO, tDOWNTO]);

  if (t <> tTO) and (t <> tDOWNTO) then begin
    Error(ResWords[tTO] + ' or ' + ResWords[tDOWNTO] + ' expected', CmdLine, errToExpected);
    Exit;
  end;

  ForInc := t = tTO;

  j := 1;
  v := GetToken(e, j, t);
  if t <> tVR then begin
    Error(ResWords[tFOR] + ' control variable expected', CmdLine, errForVar);
    Exit;
  end;

  s := GetToken(e, j, t);
  if (t = tEQU) and (ResWords[tEQU] = ResWords[tASSIGN]) then
    t := tASSIGN;

  if t <> tASSIGN then begin
    Error('Invalid орerator: ' + s + ' (''' + ResWords[tFOR] + ''' expected ', CmdLine, errInvalidOp);
    Exit;
  end;

  e := Trim(copy(e, j, Length(e)));

  if not Calc(e, R) then begin
    Error('Error in expression', e, errExprSyntax);
    Exit;
  end;

  try
    ForFrom := R.Value;
  except
    Error('Invalid loop variable value ', e, errTypeCast);
    Exit;
  end;

  e := GetTokenLine(Line, i, t, [tDO]);

  if t <> tDO then begin
    Error(ResWords[tDO] + ' expected', CmdLine, errDoExpected);
    Exit;
  end;

  e := Trim(e);

  if not Calc(e, R) then begin
    Error('Error in expression', e, errExprSyntax);
    Exit;
  end;

  try
    ForTo := R.Value;
  except
    Error('Invalid loop variable value ', e, errTypeCast);
    Exit;
  end;

  s := copy(Line, i, Length(Line));

  i := ForFrom;
  repeat
    if (ForInc and (i > ForTo))
      or ((not ForInc) and (i < ForTo)) then
      break;

    if not SetValue(v, i) then begin
      Error('Invalid variable name: ' + v, CmdLine, errExprSyntax);
    end;

    Exec(s);
    if ErrCode <> errOK then
      break;

    Process;

    if BlockCmd <> tEMPTY then begin
      if (BlockCmd = tEXIT) or (BlockCmd = tGOTO) then
        break;
      if BlockCmd = tBREAK then begin
        BlockCmd := tEMPTY;
        break;
      end;
      if BlockCmd = tCONTINUE then
        BlockCmd := tEMPTY;
    end;
    if ForInc then
      inc(i)
    else
      dec(i);
  until False;
end; // DoFor

procedure TScript.DoBreak(CmdLine, Line: String);
var
  i: integer;
  t: TToken;
begin
  i := 1;
  GetToken(Line, i, t);
  if t <> tEMPTY then
    Error('Invalid ' + ResWords[tBREAK] + ' operator', CmdLine, errExprSyntax);
  BlockCmd := tBREAK;
end; // DoBreak

procedure TScript.DoExit(CmdLine, Line: String);
var
  i: integer;
  t: TToken;
begin
  i := 1;
  GetToken(Line, i, t);
  if t <> tEMPTY then
    Error('Invalid ' + ResWords[tEXIT] + ' operator', CmdLine, errExprSyntax);
  BlockCmd := tEXIT;
end; // DoExit

procedure TScript.DoContinue(CmdLine, Line: String);
var
  i: integer;
  t: TToken;
begin
  i := 1;
  GetToken(Line, i, t);
  if t <> tEMPTY then
    Error('Invalid ' + ResWords[tEXIT] + ' operator', CmdLine, errExprSyntax);
  BlockCmd := tCONTINUE;
end; // DoContinue

procedure TScript.DoWhile(CmdLine, Line: String);
var
  R: TVar;
  i: integer;
  t: TToken;
  s, e: String;
begin
  i := 1;
  e := GetTokenLine(Line, i, t, [tDO]);

  if t <> tDO then begin
    Error(ResWords[tDO] + ' expected', CmdLine, errDoExpected);
    Exit;
  end;

  e := Trim(e);
  s := Trim(copy(Line, i, Length(Line)));

  repeat
    if not Calc(e, R) then begin
      Error('Error in expression', e, errExprSyntax);
      Exit;
    end;

    try
      if not R.Value then
        break;
    except
      Error('Invalid condition type', e, errTypeCast);
      Exit;
    end;

    Exec(s);
    if ErrCode <> errOK then
      break;

    Process;

    if BlockCmd <> tEMPTY then begin
      if (BlockCmd = tEXIT) or (BlockCmd = tGOTO) then
        break;
      if BlockCmd = tBREAK then begin
        BlockCmd := tEMPTY;
        break;
      end;
      if BlockCmd = tCONTINUE then
        BlockCmd := tEMPTY;
    end;
  until False;
end; // DoWhile

procedure TScript.DoRepeat(CmdLine, Line: String);
var
  R: TVar;
  i: integer;
  t: TToken;
  s, e: String;
begin
  i := 1;
  s := GetTokenLine(Line, i, t, [tUNTIL]);

  if t <> tUNTIL then begin
    Error(ResWords[tUNTIL] + ' expected', CmdLine, errUntilExpected);
    Exit;
  end;

  s := Trim(s);
  e := Trim(copy(Line, i, Length(Line)));

  repeat
    Exec(s);
    if ErrCode <> errOK then
      break;

    if not Calc(e, R) then begin
      Error('Error in expression', e, errExprSyntax);
      Exit;
    end;

    try
      if R.Value then
        break;
    except
      Error('Invalid condition type', e, errTypeCast);
      Exit;
    end;

    Process;

    if BlockCmd <> tEMPTY then begin
      if (BlockCmd = tEXIT) or (BlockCmd = tGOTO) then
        break;
      if BlockCmd = tBREAK then begin
        BlockCmd := tEMPTY;
        break;
      end;
      if BlockCmd = tCONTINUE then
        BlockCmd := tEMPTY;
    end;
  until False;
end; // DoRepeat

procedure TScript.DoGoto(CmdLine, Line: String);
var
  i: integer;
  s: String;
  t: tToken;
begin
  i := 1;
  s := GetToken(Line, i, t);
  if (t <> tVR) and (t <> tCON) or not IsAlpha(s) then begin
    Error('Invalid label in ' + ResWords[tGOTO], CmdLine, errExprSyntax);
    Exit;
  end;

  GetToken(Line, i, t);
  if t <> tEMPTY then begin
    Error('Error in ' + ResWords[tGOTO] + ' statement', CmdLine, errExprSyntax);
    Exit;
  end;

  GotoLabel := s;
  BlockCmd := tGOTO;
end; // DoGoto

procedure TScript.DoCase(CmdLine, Line: String);

  function CmpVar(const x1, x2: TVar; var Cmp: integer): Boolean;
  begin
    Result := False;
    Cmp := 0;
    if VarIsString(x1) xor VarIsString(x2) then
      Exit;
    try
      if x1.Value > x2.Value then
        Cmp := 1;
      if x1.Value < x2.Value then
        Cmp := -1;
    except
      Exit;
    end;
    Result := True;
  end; // CmpVar

var
  i, j, k: integer;
  t: tToken;
  R, V, V1, V2: TVar;
  s, sOper, sCase, sElse, sRange: String;
  SL: TStringList;
begin // DoCase
  i := 1;
  s := GetTokenLine(Line, i, t, [tOF]);

  if t <> tOF then begin
    Error(ResWords[tOF] + ' expected', CmdLine, errDoExpected);
    Exit;
  end;

  if not Calc(s, R) then begin
    Error('Error in expression', s, errExprSyntax);
    Exit;
  end;

  SL := TStringlist.Create;

  sElse := '';
  repeat
    t := NextToken(Line, i);
    if t = tEND then
      break;
    if t = tEMPTY then begin
      Error('Error in ' + ResWords[tCASE] + ' statement', CmdLine, errExprSyntax);
      SL.Free;
      Exit;
    end;

    if t = tELSE then begin
      if sELSE <> '' then begin
        Error('Too many ' + ResWords[tELSE] + 'in ' + ResWords[tCASE] + ' statement', CmdLine, errManyElse);
        SL.Free;
        Exit;
      end;
      GetToken(Line, i, t);
      sElse := GetOperator(Line, i, [tSEMI]);
      continue;
    end;

    sRange := GetTokenLine(Line, i, t, [tLBL]);

    if t <> tLBL then begin
      Error(ResWords[tLBL] + ' expected', CmdLine, errCaseRange);
      SL.Free;
      Exit;
    end;

    if sRange = '' then begin
      Error('Range defininition expected', CmdLine, errCaseRange);
      SL.Free;
      Exit;
    end;

    sOper := GetOperator(Line, i, [tSEMI]);

    j := 1;
    SL.Text := GetFuncParams(TokenStr(tLBR, ResWords[tLBR]) + sRange +
      TokenStr(tRBR, ResWords[tRBR]), j);

    for k := 0 to SL.Count - 1 do begin
      sCase := SL[k];

      j := 1;
      s := GetTokenLine(sCase, j, t, [tDOT2]);

      if t <> tDOT2 then begin
        if not Calc(s, V1) then begin
          Error('Error in expression', s, errExprSyntax);
          SL.Free;
          Exit;
        end;
        V2 := V1;
      end else begin
        if not Calc(s, V1) then begin
          Error('Error in expression', s, errExprSyntax);
          SL.Free;
          Exit;
        end;
        s := Trim(copy(sCase, j, Length(sCase)));
        if not Calc(s, V2) then begin
          Error('Error in expression', s, errExprSyntax);
          SL.Free;
          Exit;
        end;
      end;

      if not CmpVar(V1, V2, j) then begin
        Error('Invalid typecast in ' + ResWords[tCASE] + ' range', sCase, errExprSyntax);
        SL.Free;
        Exit;
      end;

      if j > 0 then begin
        V := V1;
        V1 := V2;
        V2 := V;
      end;

      if not (VarIsString(R) xor VarIsString(V1)) then begin
        CmpVar(V1, R, j);
        if j > 0 then
          continue;
        CmpVar(R, V2, j);
        if j > 0 then
          continue;
        Exec(sOper);
        SL.Free;
        Exit;
      end;
    end;

  until False;

  if sElse <> '' then
    Exec(sElse);
  SL.Free;
end; // DoCase

// ---------------------------- functions ------------------------------------

function TScript.FuncFloatToStr(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  s: AnsiString;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Str(extended(Vars[0].Value): 0: 15, s);
  while s[Length(s)] = '0' do
    s := copy(s, 1, Length(s) - 1);
  if s[Length(s)] = '.' then
    s := copy(s, 1, Length(s) - 1);
  Res.Value := s;
  Result := True;
end; // fFloatToStr

function TScript.FuncIntToStr(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  i: integer;
  s: String;
begin
  Result := FuncFloatToStr(Vars, Res);
  if Result then begin
    s := String(Res.Value);
    i := Pos('.', s);
    if i > 0 then
      Res.Value := copy(s, 1, i - 1);
  end;
end; // fIntToStr

function TScript.FuncStrToFloat(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  i: integer;
  x: extended;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Val(Trim(Vars[0].Value), x, i);
  if i <> 0 then
    Exit;
  Res.value := x;
  Result := True;
end; // fStrToFloat

function TScript.FuncStrToInt(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  i: integer;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  i := StrToInt(Vars[0].Value);
  Res.value := i;
  Result := True;
end; // fStrToInt

function TScript.FuncHexToInt(Vars: TList<TVar>; var Res: TVar): Boolean;
var i: integer;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  i := TUtils.HexToInt(Vars[0].Value);
  Res.value := i;
  Result := True;
end; // fHexToInt

function TScript.FuncVal(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  x: extended;
  i: integer;
  VTemp: TVar;
begin
  Result := False;

  if (Vars.Count <> 3)
    or (Vars[1].Name <> 'VAR')
    or (Vars[2].Name <> 'VAR') then
    Exit;

  Val(Vars[0].Value, x, i);

  VTemp := Vars[1];
  VTemp.Value := x;
  Vars[1] := VTemp;

  VTemp := Vars[2];
  VTemp.Value := i;
  Vars[2] := VTemp;

  Result := True;
end; // fVal

function TScript.FuncCopy(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 3 then
    Exit;

  Res.Value := copy(Vars[0].Value,
    Round(extended(Vars[1].Value)),
    Round(extended(Vars[2].Value)));

  Result := True;
end; // fCopy

function TScript.FuncPos(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 2 then
    Exit;
  Res.Value := Pos(Vars[0].Value, Vars[1].Value);
  Result := True;
end; // fPos

function TScript.FuncLength(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Length(Vars[0].Value);
  Result := True;
end; // fLength

function TScript.FuncInsert(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  s: String;
  VTemp: TVar;
begin
  Result := False;

  if (Vars.Count <> 3)
    or (Vars[1].Name <> 'VAR') then
    Exit;

  s := Vars[1].Value;
  Insert(String(Vars[0].Value), s, Round(extended(Vars[2].Value)));
  VTemp := Vars[1];
  VTemp.Value := s;
  Vars[1] := VTemp;
  Res.Value := s;
  Result := True;
end; // fInsert

function TScript.FuncDelete(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  s: String;
  VTemp: TVar;
begin
  Result := False;

  if (Vars.Count <> 3)
    or (Vars[0].Name <> 'VAR') then
    Exit;


  s := Vars[0].Value;
  Delete(s, Round(extended(Vars[1].Value)), Round(extended(Vars[2].Value)));
  VTemp := Vars[0];
  VTemp.Value := s;
  Vars[0] := VTemp;

  Res.Value := s;
  Result := True;
end; // fDelete

function TScript.FuncTrim(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Trim(Vars[0].Value);
  Result := True;
end; // fTrim

function TScript.FuncTrimLeft(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := TrimLeft(Vars[0].Value);
  Result := True;
end; // fTrimLeft

function TScript.FuncTrimRight(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := TrimRight(Vars[0].Value);
  Result := True;
end; // fTrimRight

function TScript.FuncUpperCase(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := AnsiUpperCase(Vars[0].Value);
  Result := True;
end; // fUpperCase

function TScript.FuncLowerCase(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := AnsiLowerCase(Vars[0].Value);
  Result := True;
end; // fLowerCase

function TScript.FuncFormat(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  i, n: integer;
  ff: Boolean;
  fs, s, fmt: String;
begin
  Result := False;
  if (Vars.Count < 2) then
    Exit;
  s := '';
  fmt := '';
  ff := False;
  n := 0;
  fs := Vars[0].Value;
  for i := 1 to Length(fs) do begin
    if fs[i] = '%' then
      ff := True;
    if ff then
      fmt := fmt + fs[i]
    else
      s := s + fs[i];
    if ff and CharInSet(fs[i], ['A'..'Z', 'a'..'z']) then begin
      ff := False;
      inc(n);
      if n < Vars.Count then begin
        if not VarIsString(Vars[n]) then begin
          try
            s := s + Format(fmt, [Vars[n].Value]);
          except
            s := s + Format(fmt, [Round(extended(Vars[n].Value))]);
          end;
        end else
          s := s + Format(fmt, [Vars[n].Value]);
      end;
      fmt := '';
    end;
  end;
  Res.Value := s + fmt;
  Result := True;
end; // fFormat

function TScript.FuncShowMessage(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  if FInteractiveFunctionsEnabled then
    ShowMessage(Vars[0].Value);
  Result := True;
end; // fShowMessage

function TScript.FuncLog(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  if FInteractiveFunctionsEnabled then
    TUtils.Log(Vars[0].Value);
  Result := True;
end; // fLog

function TScript.FuncRequestNumbers(Vars: TList<TVar>; var Res: TVar): Boolean;
var VTemp: TVar;
begin
  Result := False;
  if not Vars.Count in [3, 5, 7] then
    exit;

  if OrderDialog = nil then
    Application.CreateForm(TOrderDialog, OrderDialog);

  case Vars.Count of
  3:
    begin
      OrderDialog.SetNumberOfOrders(1, Vars[0].Value, Vars[1].Value, '', '');
      OrderDialog.UpDown1.Position := Vars[2].Value;
    end;
  5:
    begin
      OrderDialog.SetNumberOfOrders(2, Vars[0].Value, Vars[1].Value, Vars[2].Value, '');
      OrderDialog.UpDown1.Position := Vars[3].Value;
      OrderDialog.UpDown2.Position := Vars[4].Value;
    end;
  7:
    begin
      OrderDialog.SetNumberOfOrders(3, Vars[0].Value, Vars[1].Value, Vars[2].Value, Vars[3].Value);
      OrderDialog.UpDown1.Position := Vars[4].Value;
      OrderDialog.UpDown2.Position := Vars[5].Value;
      OrderDialog.UpDown3.Position := Vars[6].Value;
    end;
  end;

  if FInteractiveFunctionsEnabled and (OrderDialog.ShowModal = mrOk) then begin
    case Vars.Count of
    3:
      begin
        VTemp := Vars[2];
        VTemp.Value := OrderDialog.UpDown1.Position;
        Vars[2] := VTemp;
      end;
    5:
      begin
        VTemp := Vars[3];
        VTemp.Value := OrderDialog.UpDown1.Position;
        Vars[3] := VTemp;

        VTemp := Vars[4];
        VTemp.Value := OrderDialog.UpDown2.Position;
        Vars[4] := VTemp;

        OrderDialog.SetNumberOfOrders(2, Vars[0].Value, Vars[1].Value, Vars[2].Value, '');
        OrderDialog.UpDown1.Position := Vars[3].Value;
        OrderDialog.UpDown2.Position := Vars[4].Value;
      end;
    7:
      begin
        VTemp := Vars[4];
        VTemp.Value := OrderDialog.UpDown1.Position;
        Vars[4] := VTemp;

        VTemp := Vars[5];
        VTemp.Value := OrderDialog.UpDown2.Position;
        Vars[5] := VTemp;

        VTemp := Vars[6];
        VTemp.Value := OrderDialog.UpDown3.Position;
        Vars[6] := VTemp;

      end;
    end;
    Res.Value := True;
  end else
    Res.Value := False;

  Result := True;
end; // fRequestNumbers

function TScript.FuncDateToStr(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := DateToStr(Vars[0].Value);
  Result := True;
end; // fDateToStr

function TScript.FuncStrToDate(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := StrToDate(Vars[0].Value);
  Result := True;
end; // fStrToDate

function TScript.FuncFormatDateTime(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 2 then
    Exit;
  Res.Value := FormatDateTime(Vars[0].Value, Vars[1].Value);
  Result := True;
end; // fFormatDateTime

function TScript.FuncNow(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 0 then
    Exit;
  Res.Value := Now;
  Result := True;
end; // fNow

function TScript.FuncDate(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 0 then
    Exit;
  Res.Value := Date;
  Result := True;
end; // fDate

function TScript.FuncTime(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 0 then
    Exit;
  Res.Value := Time;
  Result := True;
end; // fTime

function TScript.FuncStrToTime(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := StrToTime(Vars[0].Value);
  Result := True;
end; // fStrToTime

function TScript.FuncTimeToStr(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := TimeToStr(Vars[0].Value);
  Result := True;
end; // fTimeToStr

function TScript.FuncDayOfWeek(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := DayOfWeek(Vars[0].Value);
  Result := True;
end; // fDayOfWeek

function TScript.FuncIncMonth(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 2 then
    Exit;
  Res.Value := IncMonth(Vars[0].Value, Round(extended(Vars[1].Value)));
  Result := True;
end; // fIncMonth

function TScript.FuncDecodeDate(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  i: integer;
  D, M, Y: word;
  VTemp: TVar;
begin
  Result := False;
  if Vars.Count <> 4 then
    Exit;
  for i := 1 to 3 do
    if Vars[i].Name <> 'VAR' then
      Exit;
  DecodeDate(Vars[0].Value, Y, M, D);

  VTemp := Vars[1];
  VTemp.Value := Y;
  Vars[1] := VTemp;

  VTemp := Vars[2];
  VTemp.Value := M;
  Vars[2] := VTemp;

  VTemp := Vars[3];
  VTemp.Value := D;
  Vars[3] := VTemp;

  Res.Value := Vars[0].Value;
  Result := True;
end; // fDecodeDate

function TScript.FuncDecodeTime(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  i: integer;
  H, M, S, MS: word;
  VTemp: TVar;
begin
  Result := False;
  if Vars.Count <> 5 then
    Exit;
  for i := 1 to 4 do
    if Vars[i].Name <> 'VAR' then
      Exit;
  DecodeTime(Vars[0].Value, H, M, S, MS);
  VTemp := Vars[1];
  VTemp.Value := H;
  Vars[1] := VTemp;

  VTemp := Vars[2];
  VTemp.Value := M;
  Vars[2] := VTemp;

  VTemp := Vars[3];
  VTemp.Value := S;
  Vars[3] := VTemp;

  VTemp := Vars[4];
  VTemp.Value := MS;
  Vars[4] := VTemp;

  Res.Value := Vars[0].Value;
  Result := True;
end; // fDecodeTime

function TScript.FuncEncodeDate(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 3 then
    Exit;
  Res.Value := EncodeDate(Round(extended(Vars[0].Value)),
    Round(extended(Vars[1].Value)),
    Round(extended(Vars[2].Value)));
  Result := True;
end; // fEncodeDate

function TScript.FuncEncodeTime(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 4 then
    Exit;
  Res.Value := EncodeTime(Round(extended(Vars[0].Value)),
    Round(extended(Vars[1].Value)),
    Round(extended(Vars[2].Value)),
    Round(extended(Vars[3].Value)));
  Result := True;
end; // fEncodeTime

function TScript.FuncSin(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Sin(Vars[0].Value);
  Result := True;
end; // fSin

function TScript.FuncCos(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Cos(Vars[0].Value);
  Result := True;
end; // fCos

function TScript.FuncTan(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Tan(Vars[0].Value);
  Result := True;
end; // fTan

function TScript.FuncArcSin(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := ArcSin(Vars[0].Value);
  Result := True;
end; // fArcSin

function TScript.FuncArcCos(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := ArcCos(Vars[0].Value);
  Result := True;
end; // fArcCos

function TScript.FuncArcTan(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := ArcTan(Vars[0].Value);
  Result := True;
end; // fArcTan

function TScript.FuncArcTan2(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 2 then
    Exit;
  Res.Value := ArcTan2(Vars[0].Value, Vars[1].Value);
  Result := True;
end; // fArcTan2

function TScript.FuncExp(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Exp(Vars[0].Value);
  Result := True;
end; // fExp

function TScript.FuncLn(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Ln(Vars[0].Value);
  Result := True;
end; // fLn

function TScript.FuncIntPower(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 2 then
    Exit;
  Res.Value := IntPower(Vars[0].Value, Round(extended(Vars[1].Value)));
  Result := True;
end; // fIntPower

function TScript.FuncSqr(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Sqr(Vars[0].Value);
  Result := True;
end; // fSqr

function TScript.FuncSqrt(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Sqrt(Vars[0].Value);
  Result := True;
end; // fSqrt

function TScript.FuncAbs(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Abs(Vars[0].Value);
  Result := True;
end; // fAbs

function TScript.FuncInt(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Int(Vars[0].Value);
  Result := True;
end; // fInt

function TScript.FuncFrac(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Frac(Vars[0].Value);
  Result := True;
end; // fFrac

function TScript.FuncRound(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Integer(Round(extended(Vars[0].Value)));
  Result := True;
end; // fRound

function TScript.FuncTrunc(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Integer(Trunc(extended(Vars[0].Value)));
  Result := True;
end; // fTrunc

function TScript.FuncCeil(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Ceil(Vars[0].Value);
  Result := True;
end; // fCeil

function TScript.FuncFloor(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  Res.Value := Floor(Vars[0].Value);
  Result := True;
end; // fFloor

function TScript.FuncMax(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  i: integer;
  N: String;
begin
  Result := False;
  if Vars.Count = 0 then
    Exit;

  N := Res.Name;
  Res := Vars[0];
  Res.Name := N;

  for i := 0 to Vars.Count - 1 do begin
    if VarType(Res.Value) <> VarType(Vars[i].Value) then
      Exit;
    if Vars[i].Value > Res.Value then
      Res.Value := Vars[i].Value;
  end;
  Result := True;
end; // fMax

function TScript.FuncMin(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  i: integer;
  N: String;
begin
  Result := False;
  if Vars.Count = 0 then
    Exit;

  N := Res.Name;
  Res := Vars[0];
  Res.Name := N;

  for i := 0 to Vars.Count - 1 do begin
    if VarType(Res.Value) <> VarType(Vars[i].Value) then
      Exit;
    if Vars[i].Value < Res.Value then
      Res.Value := Vars[i].Value;
  end;
  Result := True;
end; // fMin

function TScript.FuncInc(Vars: TList<TVar>; var Res: TVar): Boolean;
var V: TVar;
begin
  if (Vars.Count < 1) or (Vars.Count > 2) then
    Exit(False);
  V := Vars[0];
  if Vars.Count = 1 then
    V.Value := V.Value + 1
  else
    V.Value := V.Value + Vars[1].Value;
  Vars[0] := V;
  Result := True;
end; // fInc

function TScript.FuncDec(Vars: TList<TVar>; var Res: TVar): Boolean;
var V: TVar;
begin
  if (Vars.Count < 1) or (Vars.Count > 2) then
    Exit(False);
  V := Vars[0];
  if Vars.Count = 1 then
    V.Value := V.Value - 1
  else
    V.Value := V.Value - Vars[1].Value;
  Vars[0] := V;
  Result := True;
end; // fDec

function TScript.FuncSetVar(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  s: String;
begin
  // Set simple variable (not array) value
  Result := False;
  if Vars.Count <> 2 then
    Exit;
  s := UpperCase(Vars[0].Value);
  Result := SetValue(s, Vars[1].Value);
end; // fSetVar

function TScript.FuncGetVar(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  // Get variable value
  if (Vars.Count <> 1) then
    Exit(False);
  Result := VarByName(AnsiUpperCase(Vars[0].Value), Res);
end; // fGetVar

function TScript.FuncVarExists(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  if Vars.Count <> 1 then
    Exit(False);

//  ShowMEssage(Vars[0].Value);

//  Res.Value := VarByName(AnsiUpperCase(Vars[0].Value), Res);
  Res.Value := VarExists(AnsiUpperCase(Vars[0].Value));
  Result := True;
end; // fSin

{function TScript.FuncDecode(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  i: integer;
  N: String;
  X: TVar;
begin
  Result := False;
  if Vars.Count < 3 then
    Exit;

  X.Assign(Vars[0]);
  N := Res.Name;

  i := 1;
  while i < Vars.Count - 1 do begin
    if Vars[i].Value = X.Value then begin
      Res.Assign(Vars[i + 1]);
      Res.Name := N;
      Result := True;
      Exit;
    end;
    i := i + 2;
  end;
  if not Odd(Vars.Count) then begin
    Res.Assign(Vars[Vars.Count - 1]);
    Res.Name := N;
    Result := True;
  end;
end;}

{function TScript.FuncYearDays(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  y, m, d: word;
begin
  Result := False;
  if Vars.Count <> 1 then
    Exit;
  DecodeDate(Vars[0].Value, y, m, d);
  if IsLeapYear(y) then
    Res.Value := 366
  else
    Res.Value := 365;
  Result := True;
end; // fYearDays}

{function TScript.FuncYearFrac(Vars: TList<TVar>; var Res: TVar): Boolean;
var
  x: extended;
  dt1, dt2, ds, de: TDateTime;
  y1, m1, d1, y2, m2, d2, i, n: word;
begin
  Result := False;
  if Vars.Count <> 2 then
    Exit;

  dt1 := Vars[0].Value;
  dt2 := Vars[1].Value;

  if dt1 > dt2 then begin
    dt1 := dt2;
    dt2 := Vars[0].Value;
  end;

  DecodeDate(dt1, y1, m1, d1);
  DecodeDate(dt2, y2, m2, d2);

  x := 0;
  for i := y1 to y2 do begin
    if i = y1 then
      ds := dt1
    else
      ds := EncodeDate(i, 1, 1);
    if i = y2 then
      de := dt2
    else
      de := EncodeDate(i + 1, 1, 1);
    if IsLeapYear(i) then
      n := 366
    else
      n := 365;
    x := x + (de - ds) / n;
  end;

  Res.Value := x;
  Result := True;
end; // fYearFrac}

function TScript.FuncRandom(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  if Vars.Count = 0 then
    Res.Value := Random
  else if Vars.Count = 1 then
    Res.Value := Random(Round(extended(Vars[0].Value)))
  else
    Exit(False);
  Result := True;
end; // fRandom

function TScript.FuncCallGlobal(Vars: TList<TVar>; var Res: TVar): Boolean;
var s: String;
begin
  if Vars.Count = 0 then
    Exit(False);
  if not Assigned(CallGlobalProc) then
    Exit(False);
  s := UpperCase(Vars[0].Value);
  Result := CallGlobalProc(Vars, Res);
end; // fSetGlobal

function TScript.FuncGetTickCount(Vars: TList<TVar>; var Res: TVar): Boolean;
begin
  if Vars.Count <> 0 then
    Exit(False);
  Res.Value := GetTickCount;
  Result := True;
end; // fGetTickCount

procedure TScript.SetFunctions;
begin
  // String routines
  SetFunction('Val',            FuncVal);
  SetFunction('IntToStr',       FuncIntToStr);
  SetFunction('StrToInt',       FuncStrToInt);
  SetFunction('FloatToStr',     FuncFloatToStr);
  SetFunction('StrToFloat',     FuncStrToFloat);
  SetFunction('HexToInt',       FuncHexToInt);
  SetFunction('Copy',           FuncCopy);
  SetFunction('Pos',            FuncPos);
  SetFunction('Length',         FuncLength);
  SetFunction('Insert',         FuncInsert);
  SetFunction('Delete',         FuncDelete);
  SetFunction('Trim',           FuncTrim);
  SetFunction('TrimLeft',       FuncTrimLeft);
  SetFunction('TrimRight',      FuncTrimRight);
  SetFunction('UpperCase',      FuncUpperCase);
  SetFunction('LowerCase',      FuncLowerCase);
  SetFunction('Format',         FuncFormat);
  SetFunction('ShowMessage',    FuncShowMessage);
  SetFunction('Log',            FuncLog);

  // Date/time routines
  SetFunction('Now',            FuncNow);
  SetFunction('Date',           FuncDate);
  SetFunction('Time',           FuncTime);
  SetFunction('DateToStr',      FuncDateToStr);
  SetFunction('StrToDate',      FuncStrToDate);
  SetFunction('TimeToStr',      FuncTimeToStr);
  SetFunction('StrToTime',      FuncStrToTime);
  SetFunction('FormatDateTime', FuncFormatDateTime);
  SetFunction('DayOfWeek',      FuncDayOfWeek);
  SetFunction('IncMonth',       FuncIncMonth);
  SetFunction('DecodeDate',     FuncDecodeDate);
  SetFunction('DecodeTime',     FuncDecodeTime);
  SetFunction('EncodeDate',     FuncEncodeDate);
  SetFunction('EncodeTime',     FuncEncodeTime);

  // Arithmetic routines
  SetFunction('Abs', FuncAbs);
  SetFunction('Int', FuncInt);
  SetFunction('Frac', FuncFrac);
  SetFunction('Round', FuncRound);
  SetFunction('Ceil', FuncCeil);
  SetFunction('Floor', FuncFloor);
  SetFunction('Trunc', FuncTrunc);
  SetFunction('Sin', FuncSin);
  SetFunction('Cos', FuncCos);
  SetFunction('Tan', FuncTan);
  SetFunction('ArcSin', FuncArcSin);
  SetFunction('ArcCos', FuncArcCos);
  SetFunction('ArcTan', FuncArcTan);
  SetFunction('ArcTan2', FuncArcTan2);
  SetFunction('Exp', FuncExp);
  SetFunction('Ln', FuncLn);
  SetFunction('IntPower', FuncIntPower);
  SetFunction('Sqr', FuncSqr);
  SetFunction('Sqrt', FuncSqrt);
  SetFunction('Inc', FuncInc);
  SetFunction('Dec', FuncDec);

  // PASCALC functions
  SetFunction('Min', FuncMin);
  SetFunction('Max', FuncMax);
  SetFunction('GetVar', FuncGetVar);
  SetFunction('SetVar', FuncSetVar);
  SetFunction('VarExists', FuncVarExists);
//  SetOFunction('Decode', FuncDecode);
//  SetOFunction('YearDays', FuncYearDays);
//  SetOFunction('YearFrac', FuncYearFrac);

  // Custom functions
  SetFunction('CallGlobal', FuncCallGlobal);
  SetFunction('Random', FuncRandom);
  SetFunction('GetTickCount', FuncGetTickCount);
  SetFunction('RequestNumbers', FuncRequestNumbers);
end; // SetFunctions

procedure TScript.DisableInteractiveFunctions;
begin
  FInteractiveFunctionsEnabled := False;
end;

procedure TScript.EnableInteractiveFunctions;
begin
  FInteractiveFunctionsEnabled := True;
end;

procedure ExecuteScript(Text: String);
//         var i:integer;
//         S: string;
begin
  if Text = '' then
    exit;

//  S := Text + #13#10;
//  for i  := 0 to Calc.VarList.Count - 1 do
//    S := S + Calc.VarList[i]^.Name + ' = ' + VarAsType(Calc.VarList[i]^.Value, varString) + #13#10;

//    ShowMessage(S);
  Calc.Execute(Text);

  if Calc.ErrCode <> 0 then
    raise Exception.Create(Calc.ErrMsg + ': ''' + Calc.ErrLine + '''');
//    ShowMessage(Calc.ErrMsg + ': ''' + Calc.ErrLine + ''''); // Memo2.Lines.Add(Calc.ErrLine)
end; // ExecuteScript

function CalcExpression(Text: String; Default: extended = 0): extended;
var V: TVar;
begin
  Result := Default;
  if Text = '' then
    exit;

  ExecuteScript('Result := ' + Text);

  if Calc.VarByName('Result', V) then
    Result := V.Value;
end; // CalcExpression

initialization

  Calc := TScript.Create;

finalization
  Calc.Free;
end.

