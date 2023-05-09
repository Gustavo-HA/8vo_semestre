      PROGRAM EULER_MODIFICADO
      INTEGER N
      REAL Y,A,B

      OPEN(8,FILE='Aproximada.dat')
      ! Caso 1
      A=0. ! Limite inferior; es X de la condicion inicial
      B=1. ! Limite superior
      Y=0.2 !Condicion Inicial
      !Y = 1.0
      N=100 !Numero de intervalos

      CALL EULER(A,B,N,Y)
      

      END PROGRAM

      SUBROUTINE EULER(A,B,N,Y)
      INTEGER I,N
      REAL A,B,Y,F,F1,X
      WRITE(8,10)
      WRITE(*,10)
   10 FORMAT(/,2X,'XN',6X,'Y(EXACT)',3X,'EULER(YN)',2X,'EULER_MOD(YN)',
     & 2X,'ERR(EULER)',1X,'ERR(EULER_MOD)')
      H=(B-A)/N ! Tamano del Intervalo
      X=A       ! Limite inferior
      Y1=Y
      WRITE(8,11)X,F1(X),Y,Y1,ABS(F1(X)-Y),ABS(F1(X)-Y1)
   11 FORMAT(F10.6,5(F12.5))
      DO I=1,N
            Y=Y+(H*F(X,Y))  !Euler predictor
            Y1=Y1+(H*0.5)*(F(X,Y1)  +F(A+I*H, Y1+(H*F(X,Y1)))) !Solucion Aprox
            X=A+I*H
            WRITE(8,11)X,F1(X),Y,Y1,ABS(F1(X)-Y),ABS(F1(X)-Y1)
            WRITE(*,11)X,F1(X),Y,Y1,ABS(F1(X)-Y),ABS(F1(X)-Y1)
      END DO

      write(*,*)'Valor analitico de y(1): ',F1(x)
      write(*,*)'Valor numerico de y(1): ',Y1
      write(*,*)'Error relativo: ',abs((F1(X)-Y1)/F1(X))

      RETURN
      END SUBROUTINE

      FUNCTION F(X,Y) !Lado derecho
        F= 4.*x*y ! 1 xinicial = 0, xfinal = 1, 
      !  F= (4.*x*(y+y**0.5))/(1.+x**2) ! 2 xinicial = 0, xfinal = 1
      RETURN
      END

      FUNCTION F1(X) !Solucion particular
        F1= 0.2*exp(2*x**2)
      !  F1 = (2*x**2 +1)**2
      RETURN
      END
