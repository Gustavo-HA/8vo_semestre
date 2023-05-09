      Program RK4ShootingnN
      
      real*8 K1y,K2y,K3y,K4y
      real*8 K1v,K2v,K3v,K4v
      real*8 K1w,K2w,K3w,K4w
      real*8 K1x,K2x,K3x,K4x
      real*8 t,yf,y,w,x,v, t0, dy, dv, dw, dx, x0,w0,y0,v0,vf,wf,xf
      integer n

      OPEN( 9,  FILE='derivada3_num.dat' )
      OPEN( 10, FILE='fnumerica.dat' )
      OPEN( 11, FILE='derivada2_num.dat' )
      OPEN( 12, FILE='derivada_num.dat' )
      
! Se debe dar un valor inicial de la derivada de la funci�n (v)
 !       WRITE(*,*) "Entra un valor de y' inicial: "
!        READ(*,*) v
!***************   ejercicio 6  ***********************************
      t0=0.0   ! Indica el valor de t0 en y(t0)
      y=0.0   !y(0) inicial
      v=0.0
      w=-80.0
      x=12.0
      n=50000 !numero de intervalos
      h=0.0000003


      y0=y   ! Funcion ...asume que t0= 0.0
      v0=v   ! Derivada de la funcion ...asume que t0= 0.0
      w0=w
      x0=x
      contador=1
      write(*,*) '    t         D2y          D3y     Ycalc  DYcalc'
!   Se hacen los ciclos con 2 ecuaciones de primer orden 
!   acopladas 
! Recordas que la velocidad y la posici�n son funcion del tiempo
!      V(t) es la primer derivada
!      Y (t) es la posicion
      do 10 i =1,n
      t= t0+h*float(i) !incrementos del tiempo
      
!      Yreal= 5.d0
 !     Vreal= 3.d0    !derivada analitica
 !     yError=yreal-yf

      K1y=dy(t, y, v, w, x)
      K1v=dv(t, y, v, w, x)
      K1w=dw(t, y, v, w, x)
      K1x=dx(t, y, v, w, x)
      
      K2y=dy(t+h/2.,  y+h*K1y/2.,  v+h*K1v/2.,  w+h*K1w/2.,  x+h*K1x/2.)
      K2v=dv(t+h/2.,  y+h*K1y/2.,  v+h*K1v/2.,  w+h*K1y/2.,  x+h*K1x/2.)
      K2w=dw(t+h/2.,  y+h*K1y/2.,  v+h*K1v/2.,  w+h*K1y/2.,  x+h*K1x/2.)
      K2x=dx(t+h/2.,  y+h*K1y/2.,  v+h*K1v/2.,  w+h*K1y/2.,  x+h*K1x/2.)

      K3y=dy(t+h/2.,  y+h*K2y/2.,  v+h*K2v/2.,  w+h*K2w/2.,  x+h*K2x/2.)
      K3v=dv(t+h/2.,  y+h*K2y/2.,  v+h*K2v/2.,  w+h*K2w/2.,  x+h*K2x/2.)
      K3w=dw(t+h/2.,  y+h*K2y/2.,  v+h*K2v/2.,  w+h*K2w/2.,  x+h*K2x/2.)
      K3x=dx(t+h/2.,  y+h*K2y/2.,  v+h*K2v/2.,  w+h*K2w/2.,  x+h*K2x/2.)
      
      K4y=dy(t+h,   y+h*K3y,  v+h*K3v,   w+h*K3w,  x+h*K3x)
      K4v=dv(t+h,   y+h*K3y,  v+h*K3v,   w+h*K3w,  x+h*K3x)
      K4w=dw(t+h,   y+h*K3y,  v+h*K3v,   w+h*K3w,  x+h*K3x)
      K4x=dx(t+h,   y+h*K3y,  v+h*K3v,   w+h*K3w,  x+h*K3x)

      yf= y0+(1./6.)*h*(K1y+   2.*K2y+  2.*K3y+  K4y)
      Vf= V0+(1./6.)*h*(K1v+   2.*K2v+  2.*K3v+  K4v)
      wf= w0+(1./6.)*h*(K1w+   2.*K2w+  2.*K3w+  K4w)
      xf= x0+(1./6.)*h*(K1x+   2.*K2x+  2.*K3x+  K4x)

      If(i.eq.contador)then
       WRITE( *,20)t, wf,xf,yf,Vf
       WRITE( 9,30)t, xf ! Valores de la funcion analitica
       WRITE(10,30)t, yf ! Valores de la funcion (numericos)
       WRITE(11,30)t, wf  ! Valores de la derivada analitica
       WRITE(12,30)t, vf
      contador=contador+1
!      print*,'   '
      endif
      y=yf
      v=Vf
      w=wf
      x=xf
      y0=yf
      V0=Vf
      w0=wf
      x0=xf
      
   20 FORMAT(7(1X,F16.8))
   30 FORMAT(2(1X,F16.8))
10    continue
      stop
      end

      FUNCTION dy(t,y,v,w,x)
      implicit none
      Real*8 dy, t, y,v, w, x
      dy=v
      RETURN
      END

      FUNCTION dv(t,y,v,w,x)
      implicit none
      Real*8  dv, t, y, v, w, x
       dv=w !Ejemplo 6
      RETURN
      END
      
      FUNCTION dw(t,y,v,w,x)
      implicit none
      Real*8 dw, t, y,v, w, x
      dw=x
      RETURN
      END

      FUNCTION dx(t,y,v,w,x)
      implicit none
      Real*8  dx, t, y, v, w, x
      dx= ((1.6*(10.**-14.)/(3.*(10.**-6.)-y)**2.)*(1.+0.65*((3.*(10.**-&
     &6.)-y)/(12.*(10.**-5.))))+((1.56*(10.**-31.))/(3.*(10.**-6.)-y)** &
     &4.))/(3040540.541)  !Ejemplo 6
      RETURN
      END
