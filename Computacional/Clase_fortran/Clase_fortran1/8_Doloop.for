      PROGRAM DOloop
c Este programa imprime numeros
c consecutivos en orden ascendente
c o descendente

      integer k, l, suma
      
      Write(*,' (a)' ) 'Dame un numero'
      Read(*,*)l
      suma=0
c  Ciclo DO
c Imprime en orden ascendente 
c     DO 200 I = 1,n

c Imprime en orden ascendente de dos en dos
c      DO 200 I = 1,n-1,2

c Imprime en orden descendente iniciando desde n

      l = 50
      do 30 k = 0,l,1
            suma = suma + k
            write(*,*) 'Paso y resultado: ',k,suma
   30 continue
c     


      
      stop
      end
