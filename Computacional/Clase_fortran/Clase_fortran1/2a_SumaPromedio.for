      program suma
      implicit none
C IMPLICIT NONE impone la necesidad de declarar variables
c Se usa Do y enddo
c DO 
c        ... 
c ENDDO

      integer n, i
      real sum
      double precision r
c
      sum = 0
      write (*,*) 'Cuantos terminos ?'
      read *, n
 
      do i = 1, n
        sum = sum + i
        r = sum /( i*1.0 )
c Convierte el numero en real
        write (*,*) 'i = ', i
        write (*,*) 'Sum = ', sum, ' su promedio es = ', r
        write (*,*) '     '
      enddo
      pause
      STOP
      end
