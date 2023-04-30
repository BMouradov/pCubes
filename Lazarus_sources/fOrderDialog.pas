{************************************************************}
{                                                            }
{  Unit fOrderDialog                                         }
{  2014-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit fOrderDialog;

{$MODE Delphi}

interface

uses Forms, StdCtrls, ComCtrls, Controls, Classes, ExtCtrls;

type
  TOrderDialog = class(TForm)
    Bevel1: TBevel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
  public
    procedure SetNumberOfOrders(Amount: integer; Caption, sName1: string; sName2: string = ''; sName3: string = '');
  end;

var
  OrderDialog: TOrderDialog;

implementation

{$R *.lfm}

procedure TOrderDialog.SetNumberOfOrders(Amount: integer; Caption, sName1: string; sName2: string = ''; sName3: string = '');
var d: integer;
begin
  Self.Caption := Caption;
  Label1.Caption := sName1;
  Label2.Caption := sName2;
  Label3.Caption := sName3;

  Label2.Visible := Amount > 1;
  Edit2.Visible := Amount > 1;
  UpDown2.Visible := Amount > 1;
  Label3.Visible := Amount = 3;
  Edit3.Visible := Amount = 3;
  UpDown3.Visible := Amount = 3;

  if Amount = 1 then
    d := 48
  else if Amount = 2 then
    d := 24
  else
    d := 0;
  Bevel1.Height := 97 - d;
  OKBtn.Top := 116 - d;
  CancelBtn.Top := 116 - d;
  Height := 185 - d;
end;

end.
