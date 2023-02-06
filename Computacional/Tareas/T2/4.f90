PROGRAM SECANTE

    DL = 1.0E-08
    WRITE (*,*)'Dame los extremos del intervaloque contienen la raiz:'
    READ(*,*)A,B
    DX = (B-A)/10.
    X0 = (A+B)/2.0
    CALL SECANT (DL,X0,DX,ISTEP)
    WRITE (6,999) ISTEP,X0,DX
    STOP
999 FORMAT (I4,2F16.8)
END

SUBROUTINE SECANT (DL,X0,DX,ISTEP)

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

FUNCTION F(X)
    real*4, parameter :: n = 0.44, re = 6000
    F = (1/x)-(4*log10(re*x**(1-0.5*n)))/(n**0.75)+0.4/(n**1.2)
    RETURN
END