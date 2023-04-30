{************************************************************}
{                                                            }
{  Unit fLogo                                                }
{  2020-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit fLogo;

{$MODE Delphi}

interface

uses Forms, StdCtrls, ExtCtrls, Classes;

type

  { TLogoForm }

  TLogoForm = class(TForm)
    Image1: TImage;
    lStatus: TLabel;
    lVersion: TLabel;
    tCheckClose: TTimer;
    procedure FormClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tCheckCloseTimer(Sender: TObject);
  private
    FLPressed: integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LogoForm: TLogoForm;

implementation
{$R *.lfm}
uses Windows;

procedure TLogoForm.FormCreate(Sender: TObject);
begin
  lStatus.Caption := '';
  lVersion.Caption := '';
  FLPressed := -1;
  tCheckClose.Enabled := True;
end;

procedure TLogoForm.tCheckCloseTimer(Sender: TObject);
begin
  // catch global MouseUp
  if GetAsyncKeyState(VK_LBUTTON) = 0 then begin
    if FLPressed = 1 then
      close;
    FLPressed := 0;
  end else if FLPressed = 0 then
    FLPressed := 1;
end;

{ Это шаманское место. Application.Run в нашем случае вызывает программу деактивации.}

procedure TLogoForm.FormDeactivate(Sender: TObject);
begin
  Free;
end;

procedure TLogoForm.FormClick(Sender: TObject);
begin
  close;
end;

end.
