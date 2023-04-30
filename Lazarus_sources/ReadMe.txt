To use sources you must install BGRABitmap package:
Package\Online package manager\BGRABitmap\Install

Command-line:
pCubes puzzle.xml -HideLogo -AppConfigDir=. 

-HideLogo - hides logo while starting
-AppConfigDir=dir - set directory where pCubes will save settings and temporary files ("." - program's folder)
-ReportMemoryLeaks - currently not used
-CheckFileAge - check age of files to recreate cache (otherwise check only size)
-ScanFiles - scans files in 'Figures' and 'new' subdirectories and creates file 'Library.xml'


Control now is very simple - right mouse rotates whole puzzle, left - only 
one layer. 

Press MouseWheel or Alt-Left to rotate whole puzzle over single axis.

Use MouseWheel to zoom.

Shift-Undo, Ctrl-Undo and Alt-Undo (and Redo) makes Undo 10, 100 and 1000 turns respectively.
Shift-Scramble, Ctrl-Scramble and Alt-Scramble makes Undo 10, 100 and 1000 scramble respectively.

Use Right mouse button to invoke context menu in library.

Saving automatically occures when You close simulator. File name is 
"Puzzle.cub". And simulator automatically loads this file while starting.

Click on moves or timer to reset it.

To save gif-animation first make some moves then some undo's (Mark|Undo to mark may be used).
Then adjust anti-alias and other options, size of window.
Click Commands/Save Capture and choose gif-type of file.
Wait while pCubes makes all Redo commands.

============================================================

Useful links
====== =====
Some instructions how to create puzzle
http://twistypuzzles.com/forum/viewtopic.php?p=359489#p359489
http://twistypuzzles.com/forum/viewtopic.php?p=361865#p361865
http://twistypuzzles.ru/forum/index.php/topic,953.msg15731.html#msg15731

Geometries Catalog
http://dmccooey.com/polyhedra/

Jumbling Geometries Catalog
http://twistypuzzles.com/forum/viewtopic.php?f=1&p=358806

======================================================
Description of xml structure
-------------
each file must be as follows:
<?xml version = "1.0" encoding = "windows-1251"?>
<xml Data_Version = "2" >
  ...body...
</xml>
-------------
body consist of sections:
    <Interface>
    <Script>
    <Axes>
    <Figure>
    <Procedures>
    <GraphicsOptions>
    <Undo>
-------------
    <Interface> consists of 
      - Name
      - Class
      - Inventor
      - Programmer
      - Menu - may be multiline
      - Added
      - Link - may be multiline
-------------
    <Axes> - there may be several Axes sections

    typical use:
    <Axes TurningAngles = "Pi / 2" PlaneDistances = "-D; D" >
        <Axis NormVector = "1; 0; 0" />
        <Axis NormVector = "0; 1; 0" />
    </Axes>

    Parameters in Axes are copied into every <Axis>. So you may write equivalent:
    <Axes>
        <Axis NormVector = "1; 0; 0" TurningAngles = "Pi / 2" PlaneDistances = "-D; D" />
        <Axis NormVector = "0; 1; 0" TurningAngles = "Pi / 2" PlaneDistances = "-D; D" />
    </Axes>

    - PlaneDistances - list of turning planes perpendicular to this axis. number is coordinate of plane's nearest point to zero.
    - TurningAngles - is an angle of possible turnings.
    - FixedLayers - list of layers that can't be turned.
    - TurnAxesWithLayer - use if axes system must turn with some layer (i.e. middle layer)
    - TurnAxesWithPartNo - use if axes system is locked with concrete part.
    - AvailableAngles - list of available angles of turn, used if angles are not divisible by 2*Pi.
    - JumbleAngle -> AvailableAngles = TurningAngles+-JumbleAngle

-------------
    <Figure>
    - LoadFrom - loads part of figure from external file.
    - Part - describes vertices and faces of 3d-part via "Vertices" and "Faces" subcommands
    - Macro/ExecMacro - repeating parts of code. <Macro Name = "mac">.../> <ExecMacro MacroName = "mac" [Repeat = "3"] AxisNo = "0" ... />
    - Circle - flat circle like in crazy cubes.
    - Cylinder - 3d cylinders (like in Time Mashine).
    - Script - script language, allowing to do complex calculations.
    - Split - splits visible parts with one plane specified by <Plane> directive or with "axis" parameter. Additional parameters - Color1, Color2, InvisibleEdges1 and InvisibleEdges2.
    - Cut - splits visible parts with plane, specified similar to Split command. Additional parameters - Color and InvisibleEdges.
    - SplitByAxes - splits visible parts in according to all axes' planes.
    - Turn - turns (visible) parts specified by "Axis", "Layers" and "Angle" (or "NormVector" and "Angle") parameters.
    - Hide - hides parts specified by "Axis", "From", "To" and "Layer" parameters.
    - HideContaining - hides parts specified by "Vector" parameter.
    - HideAll - hides all parts
    - ShowAll - shows all hidden parts.
    - InvertVisibility - inverts visibility of every part.
    - RemoveAllVisible - removes all visible parts.
    - RemoveGrayParts - removes parts that has no colored stickers.
    - RemoveContaining - removes parts specified by "Vector" parameter.
    - Undo [Repeat = "Count"] - undos last turns.
    - Connect [Axis = "0"] Layers = "1;3;5" - Layers 1, 3 and 5 will turn together.
    - SaveSolvedState - saves in memory coordinates and colors of the sides relative to the axes. There may be several directives in one figure.
    - Transfer Vector = " X; Y; Z" slides whole scene
-------------
        <Vertices Vectors = "vx; vy; vz" /> <!-- vx, vy, vz may be arrays -->
        -- or --
        <Vertices>
            <Add Vector = "vx[0];vy[0];vz[0]" />
            . . .
            <Add Vector = "vx[9];vy[9];vz[9]" />
        </Vertices>
-------------
        <Faces>
            <Face Color = "00FF00" VertexIndexes = "4;3;2;1;0" InvisibleEdges = "15" />
        </Faces>
    where InvisibleEdges is a bit-mask of what edges must be hidden, or "-1" to hide all edges.
    Color may be 6-digits, or "-1" (body-color), or "-2" (transparent).
-------------

    <Textures> - assignes names to files with texture. Only .vec files are supported now.
-------------
    <SetTextures> - assignes texture to faces. To get PartNo and FaceNo, Shift-click twice on empty place below "Axes" checkbox and then click on desired face.
-------------
    <Procedures CheckSolved = "..." >
    procedure to check if puzzle is solved. Available values are:
    - EachColorHasSameDirection (default)
    - EachColorHasSameOrOppositeDirection
    - CountUniquePlanes = N (counts only colored faces)
    - EachFaceConsistsOfDifferentColors - special procedure for some Sudoku puzzles
    - EachRowColDiagConsistsOfDifferentColors - special procedure for some Sudoku puzzles
    - CheckSavedStates. Compares current puzzle's state with state(s) set earlier via <SaveSolvedState/> directive. Procedure compares centers and normals of stickers. Colors may be exchanged.
    - EachPlaneHasOneColor
    - AdjoiningFacesHaveSameColors
    procedures EachColorHasSameDirection, EachColorHasSameOrOppositeDirection, EachPlaneHasOneColor don't consider faces with invisible edges.
-------------
    <Undo>
    in this section the last turns are described. May be visible when puzzle saved to xml-file.
    useful i.e. to view some combinations via Undo|Redo commands.
-------------
    Puzzle's classes:
    TPuzzle (default) - base for all classes and for most of puzzles.
    TAnimalCube - provides animal's interaction.
    TCrazy3 - layers and circles interaction. (used for Ultra Crazy)
    TDodep - layers interaction, only 5x5x5, if texture's name contains '1' then one layer turned, '2' - two, both - no layers turnable.
    TIrreversibleCube - layers turning.
    TJailBreak - layers turning, textures changing
    TLatch - layers turning. If Texture have "Lock CW" or "Lock CCW" in name then opposite side (or point) of this part have appropriate hook.
    TSquare1 - Special scrambling procedure.

    TBigChop - TMagnet with special turning angles and scrambling procedure.
    TChopasaurus - TMagnet with special turning angles and scrambling procedure.
    TSquare3d - TMagnet with special turning angles and scrambling procedure. Key angle is defined in pAngle variable.

    internal classes:
    TCuboid - used only for custom sizes, creation and parts interaction.
    TMagnet - base class for some puzzles. Provides AvailableAngles, finding new unusual axes, special scrambling, turning only by angles that provide following turnings.
    TRCube - creates default 3x3x3 Rubik's cube if no xml found.
======================================================

Description of the script language.
(Extracted from the description of the PascalC module)

- Variables do not need to be declared, they are created dynamically
  when assigninig. The type is determined by the last assigned value.
  The string characters can be assigned numeric values in the range
  of 0..255 or characters (lines of length 1).

- The following operations are supported in expressions:
    Arithmetic: +, -, *, /, ^ (exponentiation), SHL, SHR
    Operations with bits: BITOR, BITAND, BITXOR, BITNOT
    Logical:>, <,> =, <=, =, <>, AND, OR, NOT, the constants are TRUE and FALSE.
    You can also use parentheses. The order of operations is standard.
- The interpreter supports operators:
    BEGIN ... END
    IF ... THEN ... ELSE
    CASE
    FOR ... TO/DOWNTO ... DO
    WHILE ... DO
    REPEAT ... UNTIL
    CONTINUE
    BREAK
    EXIT
-  (Multidimensional) arrays supported: MyArr[ 1, 2] := 12. No declaration needed.
-  Procedures and functions are supported
    In the list of procedure and function parameters, you only need to list
    the names of the formal parameters. Use the variable "result" to return
    the value of the function.
- 
   // String functions (Delphi-like)

   Val
   IntToStr
   StrToInt
   FloatToStr
   StrToFloat
   HexToInt
   Copy
   Pos
   Length
   Insert
   Delete
   Trim
   TrimLeft
   TrimRight
   UpperCase
   LowerCase
   Format

   // Date-time functions

   Now
   Date
   Time
   DateToStr
   StrToDate
   TimeToStr
   StrToTime
   FormatDateTime
   DayOfWeek
   IncMonth
   DecodeDate
   DecodeTime
   EncodeDate
   EncodeTime

   // Math functions

   Abs
   Int
   Frac
   Round
   Ceil
   Floor
   Trunc
   Sin
   Cos
   Tan
   ArcSin
   ArcCos
   ArcTan
   Exp
   Ln
   IntPower
   Sqr
   Sqrt
   Min
   Max
   Inc
   Dec

   // System functions

   ShowMessage(V); // Displays the content on the screen. For copying, you can press Ctrl-C.
   VarExists('V'); // Checks if var V already assigned (to avoid exception message).
   RequestNumbers(Caption, Label1, N1);
   RequestNumbers(Caption, Label1, Label2, N1, N2);
   RequestNumbers(Caption, Label1, Label2, Label3, N1, N2, N3);
   include 'fileName.inc' - includes external file with script

======================================================

Description of .vec files
First line must be in form "Size NxN".
Available commands are:
PenColor XXXXXX (hex values RRGGBB)
BrushColor XXXXXX
Polygon x0,y0, x1,y1, ... (only integer coordinates allowed)
For editing you may use pVecEditor from http://pmetro.su/download/pMetroEditor.zip.
