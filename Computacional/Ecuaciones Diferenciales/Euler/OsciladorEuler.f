       PROGRAM OsciladorEuler
c      implicit real*8 (a-h,o-z)
c
c  problema sacado del libro: An introduction
c  to computational physics. Tao Pang   
c 
c
c  Observa que al haber discretizado el 
c problema, se debe usar un numero grande
c de intervalos

      Parameter (n=10000)
      real*8, Dimension(n) :: x(n),v(n)
      Real*8 t1
      Real*8 DTimestep
      
c___Condiciones iniciales a t=0
c La Fuerza esta dada como  F=-kx
c k y m son igual a 1
      X(1)= 0.0
      V(1)= 1.0

c     Numero de intervalos
      t1= 1.0/n
c Tiempo total es 2*pi
c Dividiendo el Tiempo total entre el numero de
c intervalos obtengo el timestep
      DTimestep= t1*2.0*3.14159

c  t   x                v
c  0   0                1
c  1   0+1*times        1-0*1*times
c  2   times+1*2times   1-(times*2times)

       
c     Calculando a t(i+1)
      SUM=0.0
      OPEN( 9, FILE='posiciones.txt' )
      OPEN( 10, FILE='velocidades.txt' )
      OPEN( 11, FILE='ambos.txt' )
c Proceso Iterativo
      write (*,*)'tiempo(s),       posicion(m),    velocidad(m/s): '
      do i=1,n
      x(i+1) = X(i) + (V(i)*DTimestep)
      X(i+1)= SUM+X(i+1)
      
      v(i+1)= V(i) -  (X(i)*DTimestep)
      v(i+1)= SUM+v(i+1)

      write (*,100)(i)*DTimestep, X(i+1),V(i+1)
c Escribimos posiciones y velocidades por separado
      write (9,200)(i)*DTimestep, X(i+1)
      write (10,200)(i)*DTimestep,V(i+1)
      write (11,*)(i)*DTimestep, X(i+1), V(i+1)
c      write (*,100)(i)*Timestep, X(i+1),
      
      enddo
c imprimimos al 9
!      write (11,*)' '
!      do i=1,n
!      write (11,200)(i)*DTimestep,V(i+1)
!      enddo

  100 format (3(2x,f13.5)) 
  200 format (2(2x,f13.5))
c  100 format (3(2x,d13.5))
      close(9)
      close(10)
      close(11)
      stop
      end
