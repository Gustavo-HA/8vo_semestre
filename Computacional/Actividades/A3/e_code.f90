PROGRAM NEWTON
    IMPLICIT NONE
    REAL*8 DF , dl , dx , F , x0 , x1
    INTEGER step

!  Modificación: 6/Junio/2017

!C Usa el metodo de Newton para encontrar la raiz de la funcion.
!C En este metodo se necesita conocer la derivada de la función.

!C     DL es la tolerancia
     dl = 1.0E-06
     WRITE (*,*)'***********************************'
     WRITE (*,*)'Dame la estimacion de la raiz (x0):'
     READ(*,*)x0   ! Estimation de la raíz
    dx=x0

    WRITE (*,*) 'F(x0):',  F(x0)
    WRITE (*,*) 'DF(x0):', DF(x0)
     step = 0


    Do 100 while (abs(DX).GT.DL)

       x1 = x0 - (F(x0)/DF(x0))
       dx = x1 - x0
       x0 = x1
     WRITE (*,*) x1
    step = step + 1
100 CONTINUE


    WRITE (*,*) 'Iteracion:   Raiz:  Intervalo:'
    WRITE (6,101) step , x0 , dx
101 FORMAT (I4,2F16.8)
!      PAUSE
    STOP
    END

! FUNCION
    FUNCTION F(X)
    IMPLICIT NONE
    REAL*8 F , X
    F = x*sin(x)-log(x)+x-2.6
    RETURN
    END
! DERIVADA DE LA FUNCION
    FUNCTION DF(X)
    IMPLICIT NONE
    REAL*8 DF , X
    DF = sin(x) + x*cos(x) - 1./x + 1
    RETURN
    END