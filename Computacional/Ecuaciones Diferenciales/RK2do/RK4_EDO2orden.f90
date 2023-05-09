      Program RK4Shooting

      real*8 K1y,K2y,K3y,K4y
      real*8 K1v,K2v,K3v,K4v
      real*8 t,yf,y,v, t0,h,y0,v0,yreal,vreal,vf,d1y,d2y
      integer n

      OPEN( 9,  FILE='funcion_analitica.txt' )
      OPEN( 10, FILE='fnumerica.txt' )
      OPEN( 11, FILE='derivada_analitica.txt' )
      OPEN( 12, FILE='derivada_num.txt' )
        
!      Condiciones iniciales
!***************   ejercicio 1  ***********************************
      t0=0.0  ! Indica el valor de t0 en y(t0)
      y=1.d0  !y(0) inicial
      v=0.d0  !y'(0) inicial
      n=20 !numero de intervalos
      h=0.1d0


!***********       CONDICIONES INICIALES   ***************************
      y0=y   ! Funcion ...asume que t0= 0.0
      v0=v   ! Derivada de la funcion ...asume que t0= 0.0
      contador=1
      write(*,*) '    t         Yreal          DYreal     Ycalc  DYcalc'

      do 10 i =1,n
            t= t0+h*float(i) !incrementos del tiempo
! ********************  Ejercicio 1 ******************************
            Yreal= (9./16.)*exp(2.*t)+(7./16.)*exp(-2.*t)-x/4. ! Funcion Analitica
            Vreal=(9./8.)*exp(2.*t)+(7./8.)*exp(-2.*t)-1./4. !Deriv. func. anali.
! ****************************************************************      
            K1y=d1y(t, y, v)
            K1v=d2y(t, y, v)
            K2y=d1y(t+h/2.,  y+h*K1y/2.,  v+h*K1v/2.)
            K2v=d2y(t+h/2.,  y+h*K1y/2.,  v+h*K1v/2.)
            K3y=d1y(t+h/2.,  y+h*K2y/2.,  v+h*K2v/2.)
            K3v=d2y(t+h/2.,  y+h*K2y/2.,  v+h*K2v/2.)
            K4y=d1y(t+h,   y+h*K3y,  v+h*K3v)
            K4v=d2y(t+h,   y+h*K3y,  v+h*K3v)
            yf= y0+(1./6.)*h*(K1y+   2.*K2y+  2.*K3y+  K4y)
            Vf= V0+(1./6.)*h*(K1v+   2.*K2v+  2.*K3v+  K4v)
            If(i.eq.contador)then
                  WRITE( *,20)t, yreal,vreal,yf,Vf
                  WRITE( 9,30)t, yf ! Valores de la funcion analitica
                  WRITE(10,30)t, yreal ! Valores de la funcion (numericos)
                  WRITE(11,30)t, Vf  ! Valores de la derivada analitica
                  WRITE(12,30)t, Vreal
                  contador=contador+1
            endif
            y=yf
            v=Vf
            y0=yf
            V0=Vf 
      10    continue
      20 FORMAT(7(1X,F14.8))
      30 FORMAT(2(1X,F14.8))
      close(9)
      close(10)
      close(11)
      close(12)
      stop
      end

      FUNCTION d1y(t,y,v)
      implicit none
      Real*8 d1y, v,t,y
      d1y=v
      RETURN
      END

      FUNCTION d2y(t,y,v)
      implicit none
      Real*8  d2y, t, y, v
      d2y=t+4.*y !Ejercicio 1
      RETURN
      END
