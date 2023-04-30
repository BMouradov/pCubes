{************************************************************}
{                                                            }
{  Unit frLog                                                }
{  2023-2023                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit frLog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ExtCtrls;

type

  { TLogFrame }

  TLogFrame = class(TFrame)
    bSplitOrder: TButton;
    lScript: TLabel;
    mLog: TMemo;
    mPreScript: TMemo;
    pTopButtons: TPanel;
    pTop: TPanel;
    pBottomCaption: TPanel;
    pBottom: TPanel;
    sbLogClear: TSpeedButton;
    procedure bSplitOrderClick(Sender: TObject);
    procedure mPreScriptChange(Sender: TObject);
    procedure sbLogClearClick(Sender: TObject);
  private

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    // procedure UpdateLog(LogList: TStringList);
    procedure UpdateLog;
  end;

  { TLogFrame }
implementation
{$R *.lfm}
uses
  Messages, LCLType,
  fMain, uSplitOrder, uUtils;

constructor TLogFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TLogFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TLogFrame.sbLogClearClick(Sender: TObject);
begin
  MainForm.FLog.Clear;
  UpdateLog;
end;

procedure TLogFrame.bSplitOrderClick(Sender: TObject);
var so: TSplitOrder;
begin
  so := TSplitOrder.Create;
  try
    so.CalcSplitOrder(MainForm.CurrentPuzzle);
//  so.CalcCutPartsOrder(CurrentPuzzle);
  finally
    so.Free;
  end;
end;

procedure TLogFrame.mPreScriptChange(Sender: TObject);
begin
  TSettings.PreScript := mPreScript.Lines.Text;
end;

procedure TLogFrame.UpdateLog;
begin
//  if not Assigned(LogList) then
//    exit;

  mLog.Lines.Text := MainForm.FLog.GetText(500);
  mLog.Perform(WM_VScroll, SB_BOTTOM, 0);
end;

//procedure TLogFrame.UpdateLog(LogList: TStringList);
//begin
//  if not Assigned(LogList) then
//    exit;
//  mLog.Lines.Assign(LogList);
//  mLog.Perform(WM_VScroll, SB_BOTTOM, 0);
//end;
//
end.

