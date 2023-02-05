      PROGRAM Suma
c Este programa calcula la suma de numeros
c consecutivos

      integer i, n, sum
      
      Write(*,' (a)' ) 'Dame un numero'
      Read(*,*)n

      sum = 0

c  Ciclo DO
      do i = 1, n
      sum = sum + i
      write(*,*) 'i=',i, 'suma=', sum
c      write(*,*) 
      enddo
      
      PAUSE
c El pause solo es necesario en force
      stop
      end
