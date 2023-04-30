        if not VarExists('W') then
          W := 1;

        // Vertices (corners)

        Vx[0] := -W; Vy[0] := -W; Vz[0] := -W; 
        Vx[1] := -W; Vy[1] := -W; Vz[1] :=  W; 
        Vx[2] := -W; Vy[2] :=  W; Vz[2] := -W; 
        Vx[3] := -W; Vy[3] :=  W; Vz[3] :=  W; 
        Vx[4] :=  W; Vy[4] := -W; Vz[4] := -W; 
        Vx[5] :=  W; Vy[5] := -W; Vz[5] :=  W; 
        Vx[6] :=  W; Vy[6] :=  W; Vz[6] := -W; 
        Vx[7] :=  W; Vy[7] :=  W; Vz[7] :=  W; 


        // Centers

        Cx[0] := -W; Cy[0] :=  0; Cz[0] :=  0; 
        Cx[1] :=  W; Cy[1] :=  0; Cz[1] :=  0; 
        Cx[2] :=  0; Cy[2] := -W; Cz[2] :=  0; 
        Cx[3] :=  0; Cy[3] :=  W; Cz[3] :=  0; 
        Cx[4] :=  0; Cy[4] :=  0; Cz[4] := -W; 
        Cx[5] :=  0; Cy[5] :=  0; Cz[5] :=  W; 


        // Edges

        Ex[ 0] :=  0; Ey[ 0] := -W; Ez[ 0] :=  W; 
        Ex[ 1] :=  0; Ey[ 1] :=  W; Ez[ 1] :=  W; 
        Ex[ 2] := -W; Ey[ 2] :=  0; Ez[ 2] :=  W; 
        Ex[ 3] :=  W; Ey[ 3] :=  0; Ez[ 3] :=  W; 
        Ex[ 4] :=  W; Ey[ 4] := -W; Ez[ 4] :=  0; 
        Ex[ 5] :=  W; Ey[ 5] :=  W; Ez[ 5] :=  0; 
        Ex[ 6] :=  0; Ey[ 6] :=  W; Ez[ 6] := -W; 
        Ex[ 7] :=  0; Ey[ 7] := -W; Ez[ 7] := -W; 
        Ex[ 8] :=  W; Ey[ 8] :=  0; Ez[ 8] := -W; 
        Ex[ 9] := -W; Ey[ 9] :=  0; Ez[ 9] := -W; 
        Ex[10] := -W; Ey[10] :=  W; Ez[10] :=  0; 
        Ex[11] := -W; Ey[11] := -W; Ez[11] :=  0; 
        
                                         
                                         