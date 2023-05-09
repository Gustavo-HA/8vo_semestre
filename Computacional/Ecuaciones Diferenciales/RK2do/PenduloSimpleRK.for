      Program PenduloSimpleRK
! Se usa dos grupos de funciones.
! La posicion (tetha) esta dada por: K1x,K2x,K3x,K4x
! La velocidad por: K1v, K2v, K3v y K4v 

      real*8 K1y,K2y,K3y,K4y
      real*8 K1v,K2v,K3v,K4v
      real*8 yError, periodo,y, yreal,vreal, d1y, d2y
      real*8 g,L,t,c,omega,yi,yf,amplitud,h,pi,v,v0,y0
      integer n
      common/constantes/g,L !  Parametros
!      g=9.81d0 ! m/s^2
      g=32.2d0 ! feet/s^2
      L=2.d0  !feet
!      L=1.d0  !metros
      pi=2.d0*asin(1.d0)
!
      OPEN( 9,  FILE='Pos.txt' )
      OPEN( 10, FILE='vel.txt' )
      OPEN( 11, FILE='Pos_Vs_vel.txt' )
      OPEN( 12, FILE='Xerror.txt' )

!      Condiciones iniciales
!      y=10.d0*(pi/180.0)  !angulo inicial en grados
      y=pi/4.d0
      v=0.d0     !Velocidad inicial
      n=100     !numero de intervalos
!      periodo=2.*Pi !Se grafica de 0-2pi
      y0=y
      v0=v
      contador=1

      h=0.05d0  ! Incremento
      write(*,*) '   t  AngRealPeq  VRealPeq    angulo    velocidad   '
!   Se hacen los ciclos con 2 ecuaciones de primer orden 
!   acopladas 
        
      do 10 i =1,n
      t=h*float(i) !incrementos del tiempo 

 !IMPORTANTE
! La solucion analitica lineal (angulos pequenos)
      yreal= y*dcos(sqrt(g/L)*t)
      Vreal=-y*sqrt(g/L)*dsin(sqrt(g/L)*t)
!      yError=yreal-yf

      K1y=d1y(t,y,v)
      K1v=d2y(t,y,v)

      K2y=d1y(t+h/2.d0,y+h*K1y/2.d0,v+h*K1v/2.d0)
      K2v=d2y(t+h/2.d0,y+h*K1y/2.d0,v+h*K1v/2.d0)

      K3y=d1y(t+h/2.d0, y+h*K2y/2.d0,  v+h*K2v/2.d0)
      K3v=d2y(t+h/2.d0, y+h*K2y/2.d0, v+h*K2v/2.d0)

      K4y=d1y(t+h,y+h*K3y,v+h*K3v)
      K4v=d2y(t+h,y+h*K3y,v+h*K3v)

      yf=y0+(1.d0/6.d0)*h*(K1y+2.d0*K2y+2.d0*K3y+K4y)
      Vf=V0+(1.d0/6.d0)*h*(K1v+2.d0*K2v+2.d0*K3v+K4v)
      
      If(i.eq.contador)then
       WRITE(*,20)t, yreal,Vreal,yf,Vf
       WRITE(9,30)t, yf
       WRITE(10,30)t, Vf
       WRITE(11,30)yf, Vf
       WRITE(12,30)t, yerror
      contador=contador+1
!      print*,'   '
      endif
      y=yf
      v=Vf
      y0=yf
      V0=Vf 
      
   20 FORMAT(7(1X,F12.8))
   30 FORMAT(2(1X,F14.8))
10    continue
      stop
      end

!Calcula dX/dt....primer derivada de x
      FUNCTION d1y(t,y,v)
      implicit none
      Real*8 d1y, t, y,v, g,L,pi
      d1y=v
      RETURN
      END

! Calcula dV/dt ....segunda derivada de x
      FUNCTION d2y(t,y,v)
      implicit none
      Real*8  d2y, t, y, v,g,L,pi
      common/constantes/g,L
      d2y= -1.*(g/L)*sin(y)! Para angulos pequenos aceleracion
 !     d2y= -(32.2/2.) *sin(y)
      RETURN
      END
