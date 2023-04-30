        if not VarExists('W') then
            W := 0.5;
        if not VarExists('WX') then
            WX := W;
        if not VarExists('WY') then
            WY := W;
        if not VarExists('WZ') then
            WZ := W;
        if not VarExists('DWX') then
            DWX := 0;
        if not VarExists('DWY') then
            DWY := 0;
        if not VarExists('DWZ') then
            DWZ := 0;


        // Vertices (corners)

        Vx[0] :=-WX + DWX; Vy[0] :=-WY + DWY; Vz[0] :=-WZ + DWZ;
        Vx[1] :=-WX + DWX; Vy[1] :=-WY + DWY; Vz[1] := WZ + DWZ;
        Vx[2] :=-WX + DWX; Vy[2] := WY + DWY; Vz[2] :=-WZ + DWZ;
        Vx[3] :=-WX + DWX; Vy[3] := WY + DWY; Vz[3] := WZ + DWZ;
        Vx[4] := WX + DWX; Vy[4] :=-WY + DWY; Vz[4] :=-WZ + DWZ;
        Vx[5] := WX + DWX; Vy[5] :=-WY + DWY; Vz[5] := WZ + DWZ;
        Vx[6] := WX + DWX; Vy[6] := WY + DWY; Vz[6] :=-WZ + DWZ;
        Vx[7] := WX + DWX; Vy[7] := WY + DWY; Vz[7] := WZ + DWZ;


        // Centers
        Cx[0] :=-WX; Cy[0] :=  0; Cz[0] :=  0;
        Cx[1] := WX; Cy[1] :=  0; Cz[1] :=  0;
        Cx[2] :=  0; Cy[2] :=-WY; Cz[2] :=  0;
        Cx[3] :=  0; Cy[3] := WY; Cz[3] :=  0;
        Cx[4] :=  0; Cy[4] :=  0; Cz[4] :=-WZ;
        Cx[5] :=  0; Cy[5] :=  0; Cz[5] := WZ;


        // Edges

        Ex[ 0] :=  0; Ey[ 0] :=-WY; Ez[ 0] := WZ;
        Ex[ 1] :=  0; Ey[ 1] := WY; Ez[ 1] := WZ;
        Ex[ 2] :=-WX; Ey[ 2] :=  0; Ez[ 2] := WZ;
        Ex[ 3] := WX; Ey[ 3] :=  0; Ez[ 3] := WZ;
        Ex[ 4] := WX; Ey[ 4] :=-WY; Ez[ 4] :=  0;
        Ex[ 5] := WX; Ey[ 5] := WY; Ez[ 5] :=  0;
        Ex[ 6] :=  0; Ey[ 6] := WY; Ez[ 6] :=-WZ;
        Ex[ 7] :=  0; Ey[ 7] :=-WY; Ez[ 7] :=-WZ;
        Ex[ 8] := WX; Ey[ 8] :=  0; Ez[ 8] :=-WZ;
        Ex[ 9] :=-WX; Ey[ 9] :=  0; Ez[ 9] :=-WZ;
        Ex[10] :=-WX; Ey[10] := WY; Ez[10] :=  0;
        Ex[11] :=-WX; Ey[11] :=-WY; Ez[11] :=  0;
