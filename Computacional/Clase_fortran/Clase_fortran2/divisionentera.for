      PROGRAM DIVENTEROS
      REAL ivar3, k3
  
c     Division de numeros enteros
c     Definimos variables
      ivar1= 6
      ivar2= 4
      nvar3= 3

      ivar3=3.0

c  Se hacen las operaciones

      k1= ivar1/nvar3
      k2= ivar2/nvar3
      k3= ivar1/ivar3

c  Se escribe a la pantalla
      write (*,*) k1
      write (*,*) k2
      write (*,*) k3

      stop
      end
