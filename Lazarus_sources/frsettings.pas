{************************************************************}
{                                                            }
{  Unit frSettings                                           }
{  2023-2023                                                 }
{                                                            }
{  Author: Boris Mouradov                                    }
{                                                            }
{************************************************************}

unit frSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, Buttons, Dialogs;

type

  { TSettingsFrame }

  TSettingsFrame = class(TFrame)
    bBackgroundColor: TSpeedButton;
    bBodyColor: TSpeedButton;
    bEdgeColor: TSpeedButton;
    cbAntiAlias: TCheckBox;
    cbContourMode: TCheckBox;
    cbMinimizeToTray: TCheckBox;
    cbShowAxes: TCheckBox;
    cbShowNormals: TCheckBox;
    cbWired: TCheckBox;
    dBkColor: TColorDialog;
    gbColors: TGroupBox;
    gbCommon: TGroupBox;
    gbGraphicsEngine: TGroupBox;
    gbMouseClickMode: TGroupBox;
    gbVisualization: TGroupBox;
    Label2: TLabel;
    lBackgroundColor: TLabel;
    lBlend: TStaticText;
    lBodyColor: TLabel;
    lEdgeColor: TLabel;
    lLighting: TLabel;
    rbChangeColor: TRadioButton;
    rbGraphicsEngineBsp: TRadioButton;
    rbGraphicsEngineOpenGl: TRadioButton;
    rbGraphicsEngineZBuffer: TRadioButton;
    rbHidePart: TRadioButton;
    rbHideSticker: TRadioButton;
    rbRemovePart: TRadioButton;
    rbStickerColor: TRadioButton;
    rbTurn: TRadioButton;
    tbBlend: TTrackBar;
    tbLighting: TTrackBar;
    tbPenWidth: TTrackBar;
    procedure bBackgroundColorClick(Sender: TObject);
    procedure bBodyColorClick(Sender: TObject);
    procedure bEdgeColorClick(Sender: TObject);
    procedure cbAntiAliasClick(Sender: TObject);
    procedure cbContourModeClick(Sender: TObject);
    procedure cbMinimizeToTrayClick(Sender: TObject);
    procedure cbShowAxesClick(Sender: TObject);
    procedure cbShowNormalsClick(Sender: TObject);
    procedure cbWiredClick(Sender: TObject);
    procedure rbChangeColorClick(Sender: TObject);
    procedure rbGraphicsEngineBspClick(Sender: TObject);
    procedure rbGraphicsEngineOpenGlChange(Sender: TObject);
    procedure rbGraphicsEngineZBufferChange(Sender: TObject);
    procedure tbBlendChange(Sender: TObject);
    procedure tbLightingChange(Sender: TObject);
    procedure tbPenWidthChange(Sender: TObject);
  private
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TSettingsFrame }
implementation
{$R *.lfm}
uses fMain, uUtils;

constructor TSettingsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TSettingsFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TSettingsFrame.cbAntiAliasClick(Sender: TObject);
begin
  MainForm.SetAntiAlias(cbAntiAlias.Checked);
end; // cbAntiAliasClick

procedure TSettingsFrame.cbShowAxesClick(Sender: TObject);
begin
  MainForm.SetShowAxes(cbShowAxes.Checked);
end;

procedure TSettingsFrame.cbWiredClick(Sender: TObject);
begin
  MainForm.SetWired(cbWired.Checked);
end;

procedure TSettingsFrame.rbChangeColorClick(Sender: TObject);
begin
  TSettings.MouseMode := TRadioButton(Sender).Tag;
end;

procedure TSettingsFrame.cbShowNormalsClick(Sender: TObject);
begin
  MainForm.SetShowNormals(cbShowNormals.Checked);
end;

procedure TSettingsFrame.cbContourModeClick(Sender: TObject);
begin
  MainForm.SetContourMode(cbContourMode.Checked);
end;

procedure TSettingsFrame.tbLightingChange(Sender: TObject);
begin
  MainForm.GraphicsEngine.Lighting := tbLighting.Position;
end;

procedure TSettingsFrame.tbPenWidthChange(Sender: TObject);
begin
  MainForm.GraphicsEngine.SetPenWidth(tbPenWidth.Position);
//  MainForm.frmPuzzle3d.pb3d.Invalidate;
end;

procedure TSettingsFrame.cbMinimizeToTrayClick(Sender: TObject);
begin
  MainForm.SetMinimizeToTray(cbMinimizeToTray.Checked);
end;

procedure TSettingsFrame.tbBlendChange(Sender: TObject);
begin
  MainForm.AlphaBlend := tbBlend.Position <> 0;
  MainForm.AlphaBlendValue := 255 - tbBlend.Position * (255 div tbBlend.Max);

//SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
//SetWindowLong(pb3d.Canvas.Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
//SetLayeredWindowAttributes(pb3d.Canvas.Handle, 0, 255 - tbBlend.Position * 20, LWA_ALPHA);
//pb3d.Repaint;
  MainForm.Invalidate;
//  pb3d.Invalidate;
//  tbBlend.Position
end; // tbBlendChange

procedure TSettingsFrame.rbGraphicsEngineBspClick(Sender: TObject);
begin
  MainForm.SetGraphicsEngine(geBsp);
end;

procedure TSettingsFrame.rbGraphicsEngineOpenGlChange(Sender: TObject);
begin
  MainForm.SetGraphicsEngine(geOpenGl);
end;

procedure TSettingsFrame.rbGraphicsEngineZBufferChange(Sender: TObject);
begin
  MainForm.SetGraphicsEngine(geZBuffer);
end;

procedure TSettingsFrame.bBodyColorClick(Sender: TObject);
begin
  dBkColor.Color := MainForm.GraphicsEngine.BodyColor;
  if dBkColor.Execute then begin
    MainForm.GraphicsEngine.BodyColor := dBkColor.Color;
    bBodyColor.Color := MainForm.GraphicsEngine.BodyColor;
//    MainForm.frmPuzzle3d.pb3d.Invalidate;
  end;
end;

procedure TSettingsFrame.bEdgeColorClick(Sender: TObject);
begin
  dBkColor.Color := MainForm.GraphicsEngine.EdgeColor;
  if dBkColor.Execute then begin
    MainForm.GraphicsEngine.EdgeColor := dBkColor.Color;
    bEdgeColor.Color := MainForm.GraphicsEngine.EdgeColor;
//    MainForm.frmPuzzle3d.pb3d.Invalidate;
  end;
end;

procedure TSettingsFrame.bBackgroundColorClick(Sender: TObject);
begin
  dBkColor.Color := MainForm.GraphicsEngine.BackgroundColor;
  if dBkColor.Execute then begin
    MainForm.GraphicsEngine.BackgroundColor := dBkColor.Color;
    bBackgroundColor.Color := MainForm.GraphicsEngine.BackgroundColor;
//    MainForm.frmPuzzle3d.pb3d.Invalidate;
  end;
end;

end.

