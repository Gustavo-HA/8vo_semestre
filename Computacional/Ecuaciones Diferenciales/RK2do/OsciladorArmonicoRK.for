      Program OsciladorRK
! Se usa dos grupos de funciones.
! La posicion esta dada por: K1x,K2x,K3x,K4x
! La velocidad por: K1v, K2v, K3v y K4v
c    k y m son igual a 1. es decir que omega= 1.rad/s . 
! El periodoo es 2 pi.
c Condiciones iniciales son:
!  posicion x=1. y velocidad (v=0.).
! las soluciones analiticas son: x=cos(omega*t)  , v=-omega*sin(omega*t)
c     x es posicion , v es velocidad

      real K1x,K2x,K3x,K4x
      real K1v,K2v,K3v,K4v
      real k,m,t,Xf,amplitud 
      integer n
      common/ constantes/ k, m !  Parametros
      k=1.
      m=1.
      pi=2.*asin(1.)
!
      OPEN( 9,  FILE='Pos.txt' )
      OPEN( 10, FILE='vel.txt' )
      OPEN( 11, FILE='Pos_Vs_vel.txt' )
      OPEN( 12, FILE='Xerror.txt' )

!      Condiciones iniciales
      x=1.  !Posicion inicial (amplitud de oscilacion)
      v=0.  !Velocidad inicial
      amplitud=x
      n=1000 !numero de intervalos
      periodo=20.*Pi !Se grafica de 0-2pi
      X0=x
      V0=v

      omega=sqrt(k/m)

      contador=1
      h=periodo/(n*1.0) ! El delta t
! 
      write(*,*) '       t      Posicion   velocidad   PosReal    VReal'

!   Se hacen los ciclos con 2 ecuaciones de primer orden 
!   acopladas   
      do 10 i =1,n
      t=h*float(i) !incrementos del tiempo 

      K1x=d1x(t,x,v)
      K1v=d2x(t,x,v)

      K2x=d1x(t+h/2.,x+h*K1x/2.,v+h*K1v/2.)
      K2v=d2x(t+h/2.,x+h*K1x/2.,v+h*K1v/2.)

      K3x=d1x(t+h/2., x+h*K2x/2., v+h*K2v/2.)
      K3v=d2x(t+h/2., x+h*K2x/2., v+h*K2v/2.)

      K4x=d1x(t+h,x+h*K3x,v+h*K3v)
      K4v=d2x(t+h,x+h*K3x,v+h*K3v)

      Xf=X0+(1./6.)*h*(K1x+2.*K2x+2.*K3x+K4x)
      Vf=V0+(1./6.)*h*(K1v+2.*K2v+2.*K3v+K4v)

! La solucion analitica
      Xreal=amplitud*cos(omega*t)
      Vreal=amplitud*(-sin(omega*t))
      XError=Xreal-Xf
      If(i.eq.contador)then
       WRITE(*,20)t, Xf,Vf, xreal,Vreal
       WRITE(9,30)t, Xf
       WRITE(10,30)t, Vf
       WRITE(11,30)Xf, Vf
       WRITE(12,30)t, Xerror
      contador=contador+1
      print*,'   '
      endif
      x=Xf
      v=Vf
      X0=Xf
      V0=Vf 
      
   20 FORMAT(7(1X,F12.8))
   30 FORMAT(2(1X,F14.8))
10    continue
      stop
      end

!Calcula dX/dt....primer derivada de x
      FUNCTION d1x(t,x,v)
      implicit none
      Real d1x, t, x,v, k, m
      d1x=v   
      RETURN
      END

! Calcula dV/dt ....segunda derivada de x
      FUNCTION d2x(t,x,v)
      implicit none
      Real  d2x, t, x, v, k,m
      common/ constantes/ k, m
      d2x=-(k/m)*x !aceleracion
      RETURN
      END
