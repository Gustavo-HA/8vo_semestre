      Program RK4Shooting
! Se resuelve una ecuacion de segundo orden

! Se usa dos grupos de funciones.
! La posicion esta dada por: K1y,K2y,K3y,K4y
! La velocidad por: K1v, K2v, K3v y K4v 
! Aqui t es la variable independiente!!!

      real K1y,K2y,K3y,K4y
      real K1v,K2v,K3v,K4v
      real*8 t,yi,yf,c,y,pi,v, t0
      integer n

      OPEN( 9,  FILE='funcion_analitica.txt' )
      OPEN( 10, FILE='fnumerica.txt' )
      OPEN( 11, FILE='derivada_analitica.txt' )
      OPEN( 12, FILE='derivada_num.txt' )

c        WRITE(*,*) "Entra Velocidad inicial: "
c        READ(*,*) v
        
!      Condiciones iniciales
!***************   ejercicio 1  ***********************************
!      t0=0.0  ! Indica el valor de t0 en y(t0)
!      y=2.d0  !y(0) inicial
!      v=7.d0  !y'(0) inicial
!      n=20 !numero de intervalos
!      h=0.1d0
!***************   ejercicio 2  ***********************************
!      t0=0.0   ! Indica el valor de t0 en y(t0)
!      y=2.d0  !y(0) inicial
!      v=3.d0  !y'(0) inicial
!      n=10 !numero de intervalos
!      h=0.1d0
!***************   ejercicio 3  ***********************************
!      t0=0.0   ! Indica el valor de t0 en y(t0)
!      y=1.d0  !y(0) inicial
!      v=0.d0  !y'(0) inicial
!      n=20 !numero de intervalos
!      h=0.1d0
!***************   ejercicio 4  ***********************************
!      t0=1.0  ! Indica el valor de t0 en y(t0)
!      y=3.d0  ! y(1) inicial
!      v=1.d0  !y'(1) inicial
!      n=200 !numero de intervalos
!      h=0.01d0
!***************   ejercicio 5  ***********************************
!     t0=0.0  ! Indica el valor de t0 en y(t0)
!      y=1.d0/7.d0  !y(0) inicial
!      v=3.d0/5.d0  !y'(0) inicial
!      n=20 !numero de intervalos
!      h=0.1d0
!***************   ejercicio 6  ***********************************
!     t0=0.0  ! Indica el valor de t0 en y(t0)
!      y=4.d0  !y(0) inicial
!      v=1.d0  !y'(0) inicial
!      n=200 !numero de intervalos
!      h=0.01d0
!***************   ejercicio 7  ***********************************
!      t0=0.0   ! Indica el valor de t0 en y(t0)
!      y=1.d0  !y(0) inicial
!      v=1.d0  !y'(0) inicial
!      n=200 !numero de intervalos
!      h=0.01d0
!***************   ejercicio 8  ***********************************
!      t0=1.0   ! Indica el valor de t0 en y(t0)
!      y=1.d0  !y(0) inicial
!      v=1.d0  !y'(0) inicial
!      n=200 !numero de intervalos
!      h=0.01d0
!***************   ejercicio 9  ***********************************
!      t0=0.0   ! Indica el valor de t0 en y(t0)
 !     y=0.d0  !y(0) inicial
  !    v=0.89d0  !y'(0) inicial
   !   n=100 !numero de intervalos
    !  h=0.01d0
      !***************   ejercicio 10  ***********************************
!      t0=0.0   ! Indica el valor de t0 en y(t0)
!      y=1.d0      !y(0) inicial
!      v=0.0  !y'(0) inicial    Sol: -0.459d0
!      n=100 !numero de intervalos
!      h=0.01d0
 !***************   ejercicio 11  ***********************************
!      t0=0.0   ! Indica el valor de t0 en y(t0)
!      y=1.d0      !y(0) inicial
!      v=0.0  !y'(0) inicial    Sol: -0.459d0
!      n=100 !numero de intervalos
!      h=0.01d0
  !***************   ejercicio 12  ***********************************
      t0=0.0   ! Indica el valor de t0 en y(t0)
      y=0.d0   !y(0) inicial
      v=0.0  !y'(0) inicial    Sol:   -1.0
      n=200!numero de intervalos
      h=0.1d0

!***********       CONDICIONES INICIALES   ***************************
      y0=y   ! Funcion ...asume que t0= 0.0
      v0=v   ! Derivada de la funcion ...asume que t0= 0.0
      contador=1
      write(*,*) '    t         Yreal          DYreal     Ycalc  DYcalc'

!   Se hacen los ciclos con 2 ecuaciones de primer orden 
!   acopladas 
!   Recordar que la velocidad y la posici¢n son funcion del tiempo
!      V(t) es la primer derivada
!      Y (t) es la posicion

      do 10 i =1,n
      t= t0+h*float(i) !incrementos del tiempo
! ********************  Ejercicio 1 ******************************
C      Yreal= 2.*exp(2.*t)+3.*t*exp(2.*t) ! Funcion Analitica
C      Vreal=4.*exp(2.*t)+3.*(2.*t*exp(2.*t)+exp(2.*t)) !Deriv. func. anali.
! ********************  Ejercicio 2 *******************************
C      Yreal= 2.*cos(t)+3.*sin(t)
C      Vreal=-2.*sin(t)+3.*cos(t)
C      YError=Yreal-Yf



! ********************  Ejercicio 3 *******************************
C      Yreal= (3./5.)*exp(2.*t)+(2./5.)*exp(-3.*t) !Funcion analitica
C      Vreal= (6./5.)*exp(2.*t)-(6./5.)*exp(-3.*t) !derivada analitica
C      YError=Yreal-Yf
 ! ********************  Ejercicio 4 *******************************
C      Yreal= -t**3.d0+4.d0*t  !Funcion analitica
C      Vreal= -3.*t**2.d0+4.d0 !derivada analitica
C      YError=Yreal-Yf
! ********************  Ejercicio 5 *******************************
!      Yreal= (16./35.)*exp(2.*t)-(11./35.)*exp(t) !Funcion analitica
!      Vreal= (32./35.)*exp(2.*t)-(11./35.)*exp(t) !derivada analitica
!      YError=Yreal-Yf
 ! ********************  Ejercicio 6 *******************************
!      Yreal= 3.d0*exp(2.*t)+exp(-2.*t)-3.d0*t !Funcion analitica
!      Vreal= 6.d0*exp(2.*t)-2.d0*exp(-2.*t)-3.d0  !derivada analitica
!      YError=Yreal-Yf
 ! ********************  Ejercicio 7 *******************************
C      Yreal= log(t+1.d0)+1.d0 !Funcion analitica
C      Vreal= 1.d0/(t+1.d0)    !derivada analitica
C      yError=yreal-yf
 ! ********************  Ejercicio 8 *******************************
!      Yreal= 1.1392070132*t+(-0.039207/t**2.)-(0.3*sin(log(t)))-0.1d0
!     & *cos(log(t))  !Funcion analitica
!      Vreal= 1.d0/(t+1.d0)    !derivada analitica
!      yError=yreal-yf
 ! ********************  Ejercicio 9 *******************************
!      Yreal= 5.d0
!      Vreal= 3.d0    !derivada analitica
!      yError=yreal-yf
 ! ********************  Ejercicio 12 *******************************
C      Yreal= log(1.d0/t)+2.d0
C      Vreal= -1.d0/t    !derivada analitica
C      yError=yreal-yf
      
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
       WRITE(10,20)t, yreal,vreal,yf,Vf, YError
       WRITE(*,20)t, yreal,vreal,yf,Vf, YError
      contador=contador+1
!      print*,'   '
      endif
      y=yf
      v=Vf
      y0=yf
      V0=Vf 
      
   20 FORMAT(6(1X,F14.8))
   30 FORMAT(2(1X,F14.8))
10    continue

      open(2,file= 'graficaxdxd.txt')
      write(2,*) 'set xlabel "x"'
      write(2,*) 'set ylabel "y"'
      write(2,*) "plot 'fnumerica.txt' u 1:4 w l"
C      write(2,*) "replot 'fnumerica.txt' u 1:2 w l"
      write(2,*) 'replot 0 w l'
      close(2)
      
      call system('start gnuplot -p graficaxdxd.txt')



      Pause
      stop
      end

      ! VELOCIDAD
!Calcula dX/dt....primer derivada de y
      FUNCTION d1y(t,y,v)
      implicit none
      Real*8 d1y, t, y,v, g,L,pi
      d1y=v
      RETURN
      END

      ! ACELERACION
! Calcula dV/dt ....segunda derivada de y
! Esta es v'
      FUNCTION d2y(t,y,v)
      implicit none
      Real*8  d2y, t, y, v,g,L,pi, c, m
      g = 9.81
      m = 68.1
      c = 12.5
C      d2y=4.*v-4.*y !Ejercicio 1
C      d2y=-1.*y    !Ejercicio 2
C      d2y=-1.*v+6.*y !Ejercicio 3
C      d2y=-6.d0*t     ! Ejercicio 4
!      d2y=3.d0*v-2.d0*y    ! Ejercicio 5
!       d2y=12.d0*t+4.d0*y  ! Ejercicio 6 necesita h=0.01
C       d2y=-v**2.d0        ! Ejercicio 7
!      d2y=(-2./t)*v+(2./t**2.)*y+(sin(log (t))/t**2.) !Ejercicio 8
!       d2y=4.2d0*t-t*v+3.d0*y !9
!       d2y=y+v*t !10
      d2y = g - (c/m)*v
C       d2y=-v/t !12

      RETURN
      END
