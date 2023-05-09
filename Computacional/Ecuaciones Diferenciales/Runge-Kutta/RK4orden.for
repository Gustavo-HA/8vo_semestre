      program RungeKutta4toGrado 
      implicit none 
      real(8)::a,b,h,y_0,yreal 
      write(*,*)'Dame el valor del intervalo a,b' 
      read(*,*) a,b
      write(*,*)'Dame el valor de h y y(0)'
      read(*,*) h,y_0

      open(unit=1, file='RungeKutta.dat') 
      write(*,*)'         t=                   y=                yreal='
      call RungeKutta(y_0,a,b,h) 
      close(1) 
      
      contains 
      subroutine RungeKutta(y_0,a,b,h) 
      implicit none 
      real(8), intent(inout)::a,h,b, y_0 
      real(8):: y,t, F_1, F_2, F_3, F_4 
      t=a 
      y=y_0 
      do while(t<=b) 
      F_1=h*f(y,t) 
      F_2=h*f(y+(1d0/2d0)*F_1, t+h*(1d0/2d0))
      F_3=h*f(y+(1d0/2d0)*F_2, t+h*(1d0/2d0)) 
      F_4=h*f(y+F_3,t+h) 
      y=y+(F_1+2d0*F_2+2d0*F_3+F_4)/6d0 
      t=t+h ! Se incrementa t

!      yreal=exp(t*t)-1.d0/2.d0 !Funcion analitica
      yreal=exp(t**3/3.)
!      yreal=exp(t*t)

      write(1,*)t, y, yreal
      write(*,*)t, y, yreal
      
      end do 
      end subroutine 

!  ********    Lado derecho de la ec. diferencial  *****
      function f(y,t) 
      implicit none 
      real(8)::f,y,t
! y′(t)=−y(t)+sin(2πt) with the initial condition y(0)=1
!      pi=acos(-1d0) 
!      f=-y+sin(pi*2d0*t) 
!       F= 2.d0*t*y
!       f= 2.d0*t*y+t
       F= 6-y/25
      end function 
      end program
 
