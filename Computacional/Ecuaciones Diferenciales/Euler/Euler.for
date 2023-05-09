      program Euler
      implicit none
      real*16, dimension (0:2000)::x,y
      integer::i,LimSup
      real*16 :: h,f,error 
! Se lee condicion inicial
       x(0)=-2.d0
       y(0)=1.d0

       h=0.01d0 !incremento
       LimSup=-1.d0 !limite superior
       open(unit=9,file='Numerica.dat')

! Se hace el ciclo   
      do i=0,20000 !ilsup
            x(i+1)=x(i)+h
            y(i+1)=y(i)+h*f(x(i), y(i)) !Para la que depende de x y y   
            IF(x(i+1) .GT. LimSup) GO TO 20
            print*,"x=",x(i),"y=",y(i)
     &,error
            write(9,*)x(i+1),y(i+1) ! Se imprime al archivo la funcion
      enddo

      close(9)
      close(10)

20    STOP
      end

      real*16 function f(x,y) 
      implicit none
      real*16 ::x,y
      f = -1/y - 2*y/x
      end
      

