{************************************************************}
{                                                            }
{  Unit uTexture                                             }
{  2017-2022                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{  Textures                                                  }
{************************************************************}

unit uTexture;
{$mode delphi}

interface

uses uVectorImage,
     Generics.Collections;

type
  TTexture = class
  private
//    FQuickPixels: TQuickPixels;
//    FPicture: TBitmap;
  public
    Name: string;
    FileName: string;
    VectorImage: TVectorImage;

    constructor Create;
    destructor Destroy; override;
    function Load(const PuzzleFileName: String): Boolean;
  end;

implementation
uses SysUtils;


// =============================== TTexture ====================================

constructor TTexture.Create;
begin
  inherited;
  Name := '';
  FileName := '';

  VectorImage := nil;
end;

function TTexture.Load(const PuzzleFileName: String): Boolean;

  function LoadVec(const FileName: String): Boolean;
  begin
    Result := FileExists(FileName);
    if Result then begin
      VectorImage := TVectorImage.Create;
      VectorImage.LoadFromFile(FileName);
    end;
  end;

begin
  Result := True;
  if VectorImage <> nil then
    exit;

  Result := False;
  if Pos('.vec', LowerCase(FileName)) <> 0 then begin
    Result := LoadVec(FileName);
    if not Result then
      Result := LoadVec(ExtractFilePath(PuzzleFileName) +  FileName);
  end;

  //  Result := False;
//  Picture := TPicture.Create;
//  try
//    Picture.LoadFromFile(FileName);
//  except
//    Picture.Free;
//    Picture := nil;
//    exit;
//  end;

//  FTexture := TBitmap.Create;
//  FTexture.Width := Picture.Width;
//  FTexture.Height := Picture.Height;
//  FTexture.PixelFormat := pf32bit;
//  FTexture.Canvas.Draw(0, 0, Picture.Graphic);
//  Picture.Free;

  //FTexture.Assign(Picture);

//  FQuickPixels := TQuickPixels.Create;
//  FQuickPixels.Attach(FTexture);
//              FQuickPixels[j, i] := clBlack;
end; // Load

destructor TTexture.Destroy;
begin
//  FTexture.Free;
//  FQuickPixels.Free;
  if VectorImage <> nil then
    VectorImage.Free;

  inherited;
end;

//function TTextures.AddTexture(const FileName: String): integer;
//var Texture: TTexture;
//begin
//  Result := -1;
//  if not FileExists(FileName) then
//    exit;
//  Result := FindByName(FileName);
//  if Result >= 0 then
//    exit;
//  Texture := TTexture.Create;
//  if Texture.Load(FileName) then begin
//    Result := Count;
//    Add(Texture);
//  end else
//    Texture.Free;
//end; // AddTexture
//
//function TTextures.AddVectorImage(const VectorImage: Pointer): integer;
//var VImage: TVectorImage;
//begin
//  VImage := VectorImage;
//  Result := Add(VImage);
//end;

end.
