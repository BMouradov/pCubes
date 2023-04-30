{************************************************************}
{                                                            }
{  Unit uPuzzleHeader                                        }
{  2019-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{  Header                                                    }
{************************************************************}

unit uPuzzleHeader;

interface

type
  TPuzzleHeader = class
  public
    Name: String;
    Aliases: String;
    ClassString: String;
    MenuString: String;
    Inventor: String;
    Programmer: String;
    Added: String;
    Link: String;
    FileName: String;

    constructor Create; virtual;
    procedure Clear;
  end;

implementation

constructor TPuzzleHeader.Create;
begin
  inherited;
  Clear;
end;

procedure TPuzzleHeader.Clear;
begin
  Name        := '';
  Aliases     := '';
  ClassString := '';
  MenuString  := '';
  Inventor    := '';
  Programmer  := '';
  Added       := '';
  Link        := '';
  FileName    := '';
end;

end.
