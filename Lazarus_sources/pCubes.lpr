program pCubes;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  {$IFOPT D+}
  //heaptrc,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fMain, uVector, uMatrix, uPlane, uFace, uGeometryUtils, uLine,
  uFaceUtils, uPart, uPuzzleBase, uAxis, uLayer, uUtils, uTexture, uVectorImage,
  uPuzzleHeader, uPuzzle, uSolvedState, uMacro, uSelection, uTurn, uLibrary,
  uScript, fOrderDialog, uPuzzleUtils,
  uPartUtils, uStreamReadWrite, fLogo, uBaseGraphics, uSolvedChecker,
  uPuzzleScreen, uSplitOrder, uZBufferGraphics, uBspGraphics, uOpenGlGraphics,
  uBsp3, uBsp, fMacro, frLibrary, frPuzzle3d, frSettings, frLog, uLog,
  uXmlReadWrite;

{$R *.res}

var
  i: integer;
  HideLogo: Boolean;
begin
  RequireDerivedFormResource := True;
  HideLogo := False;
  for i := 1 to System.ParamCount do
    if ParamStr(i) = '-HideLogo' then
      HideLogo := True;

  Application.Scaled:=True;
  Application.Initialize;

  LogoForm := TLogoForm.Create(Application);
  if not HideLogo then
    LogoForm.Show;
  LogoForm.Update;
  Application.CreateForm(TMainForm, MainForm);

  LogoForm.Hide;
  LogoForm.Free;
  Application.Run;
end.

