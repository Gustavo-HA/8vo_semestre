PROGRAM SECANTE
    !C
    !C Calcula la ra�z de una funci�n mediante el M�todo 
    !C de la secante
          DL = 1.0E-06
          WRITE (*,*)'Dame los extremos del intervaloque contienen la raiz:'
          READ(*,*)A,B
          DX = (B-A)/10.
          X0 = (A+B)/2.0
          CALL SECANT (DL,X0,DX,ISTEP)
          WRITE (6,999) ISTEP,X0,DX
          STOP
      999 FORMAT (I4,2F16.8)
    
          END
    !C
          SUBROUTINE SECANT (DL,X0,DX,ISTEP)
    !C
    !C Subroutina que implementa el m�todo
    !C
          ISTEP = 0
          X1 = X0 + DX
          DO    100  WHILE (ABS(DX).GT.DL)
            D  = F(X1) - F(X0)
            X2 = X1 - F(X1)*(X1-X0)/D
            X0 = X1
            X1 = X2
            DX = X1 - X0
            ISTEP = ISTEP + 1
      100 END DO
          RETURN
    
          END
    !C   Se define la funci�n
          FUNCTION F(X)
            F = -2*x**3 + 12*x**2 - 20*x + 8.5
          RETURN
    
          END