{************************************************************}
{                                                            }
{  Unit fMacro                                               }
{  2017-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit fMacro;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, StdCtrls, Grids, ExtCtrls,
  Buttons, Generics.Collections,
  uMacro;

type
  TMacroExecuteEvent = procedure(Macro: String) of object;

type
  TMacroForm = class(TForm)
    sgMacros: TStringGrid;
    pmMacro: TPopupMenu;
    miExecute: TMenuItem;
    Panel1: TPanel;
    bExecuteMacro: TSpeedButton;
    eMacro: TEdit;
    Label1: TLabel;

    procedure miExecuteClick(Sender: TObject);
    procedure sgMacrosDblClick(Sender: TObject);
    procedure sgMacrosDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgMacrosFixedCellClick(Sender: TObject; ACol, ARow: Integer);
    procedure sgMacrosMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure eMacroChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure bExecuteMacroClick(Sender: TObject);
    procedure pmMacroPopup(Sender: TObject);
    procedure sgMacrosKeyPress(Sender: TObject; var Key: Char);
  private
    FOnMacroExecute: TMacroExecuteEvent;
    FMacros: TList<TMacro>;

    procedure LoadIni;
    procedure SaveIni;
  public
  public
    property OnMacroExecute: TMacroExecuteEvent read FOnMacroExecute write FOnMacroExecute;

    procedure SetMacros(Macros: TList<TMacro>);
    procedure RefreshMacros;
  end;

var
  MacroForm: TMacroForm;

implementation
uses IniFiles, Math,
  uUtils;
{$R *.lfm}

procedure TMacroForm.FormCreate(Sender: TObject);
begin
  FOnMacroExecute := nil;

  sgMacros.ColCount := 4;

  sgMacros.ColWidths[0] := 30;
  sgMacros.ColWidths[1] := 60;
  sgMacros.ColWidths[2] := 200;
  sgMacros.ColWidths[3] := 100;

  sgMacros.Cells[1, 0] := 'Name';
  sgMacros.Cells[2, 0] := 'Description';
  sgMacros.Cells[3, 0] := 'Sequence';

  LoadIni;
end;

procedure TMacroForm.FormShow(Sender: TObject);
begin
//  FocusCurrent;

end;

procedure TMacroForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveIni;
end;

procedure TMacroForm.LoadIni;
var
  i, W: integer;
  Ini: TIniFile;
  ColWidths: String;
begin //LoadIni
  Ini := TIniFile.Create(TSettings.AppConfigDir + ProgramName + '.ini');
  try
    Left   := Max(Ini.ReadInteger('MacroForm', 'Left', 0), 0);
    Top    := Max(Ini.ReadInteger('MacroForm', 'Top', 0), 0);
    Width  := Ini.ReadInteger('MacroForm', 'Width', 800);
    Height := Ini.ReadInteger('MacroForm', 'Height', 580);
    SetRestoredBounds(Left, Top, Width, Height);
    if Ini.ReadBool('MacroForm', 'Maximized', True) then
      WindowState := wsMaximized;

    ColWidths := Ini.ReadString('MacroForm', 'ColWidths', '');
    for i := 0 to sgMacros.ColCount - 1 do begin
      W := StrToIntDef(TUtils.Parse(ColWidths, ';'), 0);
      if W <> 0 then
        sgMacros.ColWidths[i] := W;
    end;

  finally
    Ini.Free;
  end;
end; // LoadIni

procedure TMacroForm.SaveIni;
var
  i: integer;
  Ini: TIniFile;
  ColWidths: String;
begin //SaveIni
  Ini := TIniFile.Create(TSettings.AppConfigDir + ProgramName + '.ini');
  try
    if WindowState = wsMaximized then begin
      Ini.WriteInteger('MacroForm', 'Left',   RestoredLeft);
      Ini.WriteInteger('MacroForm', 'Top',    RestoredTop);
      Ini.WriteInteger('MacroForm', 'Width',  RestoredWidth);
      Ini.WriteInteger('MacroForm', 'Height', RestoredHeight);
    end else begin
      Ini.WriteInteger('MacroForm', 'Left',   Left);
      Ini.WriteInteger('MacroForm', 'Top',    Top);
      Ini.WriteInteger('MacroForm', 'Width',  Width);
      Ini.WriteInteger('MacroForm', 'Height', Height);
    end;
    Ini.WriteBool('MacroForm', 'Maximized', WindowState = wsMaximized);

    ColWidths := '';
    for i := 0 to sgMacros.ColCount - 1 do
      ColWidths := ColWidths + IntToStr(sgMacros.ColWidths[i]) + ';';
    Ini.WriteString('MacroForm', 'ColWidths', ColWidths);

//    Ini.WriteInteger('MacroForm', 'SortMode', Ord(TLibrary.Puzzles.SortMode));

  finally
    Ini.Free;
  end;
end; //SaveIni

procedure TMacroForm.SetMacros(Macros: TList<TMacro>);
begin
  FMacros := Macros;
end;

procedure TMacroForm.RefreshMacros;
var i: Integer;
begin
  sgMacros.RowCount := Max(2, FMacros.Count + 1);
  sgMacros.FixedRows := 1;
  if FMacros.Count > 0 then
    for i := 0 to FMacros.Count - 1 do begin
      sgMacros.Cells[0, i + 1] := IntToStr(i + 1);
      sgMacros.Cells[1, i + 1] := FMacros[i].Name;
      sgMacros.Cells[2, i + 1] := FMacros[i].Description;
      sgMacros.Cells[3, i + 1] := FMacros[i].Sequence;
    end
  else
    for i := 0 to sgMacros.ColCount - 1 do
      sgMacros.Cells[i, 1] := '';
end;

procedure TMacroForm.pmMacroPopup(Sender: TObject);
begin
  miExecute.Enabled := sgMacros.RowCount > 1;
end;

procedure TMacroForm.bExecuteMacroClick(Sender: TObject);
begin
  if Assigned(FOnMacroExecute) then
    FOnMacroExecute(eMacro.Text);
end;

procedure TMacroForm.eMacroChange(Sender: TObject);
begin
  //TLibrary.Puzzles.Filter := eFilter.Text;
  //RefreshMacros;
  //FocusCurrent;
end;

procedure TMacroForm.miExecuteClick(Sender: TObject);
begin
  if Assigned(FOnMacroExecute) then
    FOnMacroExecute(sgMacros.Cells[1, sgMacros.Row]);
  close;
end;

procedure TMacroForm.sgMacrosDblClick(Sender: TObject);
begin
  if Assigned(FOnMacroExecute) then
    FOnMacroExecute(sgMacros.Cells[1, sgMacros.Row]);
  close;
end;

procedure TMacroForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if FMacros.Count > 0 then begin
      if Assigned(FOnMacroExecute) then
        FOnMacroExecute(sgMacros.Cells[1, sgMacros.Row]);
      close;
    end;
  if Key = VK_ESCAPE then
    close;
end;

procedure TMacroForm.sgMacrosDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
//var
//  ARect: TRect;
//  Y: integer;
//  TempColor: TColor;
begin
//  if (ARow > 0) and (FMacros.Count > 0) then begin
//    with Sender as TStringGrid, Canvas do begin
//      TempColor := Brush.Color;
//      Brush.Color := clHighlight;
//      ARect := Rect;
//      ARect.Left := ARect.Left - 4;
//      FillRect(ARect);
//      SetBkMode(Handle, TRANSPARENT);
//
//      Y := Rect.Top + ((Rect.Bottom - Rect.Top - TextHeight('Wg')) div 2);
//      TextOut(Rect.Left + 2, Y, Cells[ACol, ARow]);
//      Brush.Color := TempColor;
//    end;
//  end;
end; // sgMacrosDrawCell

procedure TMacroForm.sgMacrosFixedCellClick(Sender: TObject; ACol, ARow: Integer);
begin
//  if (ARow = 0) and (ACol in [1..7]) then begin
//    case ACol of
//      1: TLibrary.Puzzles.Sort(SORT_BY_NAME);
//      2: TLibrary.Puzzles.Sort(SORT_BY_CLASS);
//      3: TLibrary.Puzzles.Sort(SORT_BY_MENU);
//      4: TLibrary.Puzzles.Sort(SORT_BY_FILE);
//      5: TLibrary.Puzzles.Sort(SORT_BY_INVERTOR);
//      6: TLibrary.Puzzles.Sort(SORT_BY_PROGRAMMER);
//      7: TLibrary.Puzzles.Sort(SORT_BY_ADDED);
//    end;
//    RefreshLibrary;
//    FocusCurrent;
//  end;
end; // sgMacrosFixedCellClick

procedure TMacroForm.sgMacrosKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Chr(VK_RETURN) then
    close;
end; // sgMacrosKeyPress

procedure TMacroForm.sgMacrosMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Col, Row: integer;
begin
  sgMacros.MouseToCell(x, y, Col, Row);
  if Row > 0 then
    sgMacros.Row := Row;
end;

end.
