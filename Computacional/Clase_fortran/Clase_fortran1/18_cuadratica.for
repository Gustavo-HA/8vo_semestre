c
c
c Este programa calcula las raices de una ecuacion de segundo
c grado. Todas las variables son del tipo REAL*4 y estan
c declaradas por defecto
c
c___________________________________________________________________
c___Entrada de los coeficientes desde teclado
      write (6,*) ' Entra el coeficiente cuadratico:'
      read (5,*) a
      write (6,*) ' Entra el coeficiente lineal:'
      read (5,*) b
      write (6,*) ' Entra el coeficiente independiente:'
      read (5,*) c
c___Calculo de las raices
      dis = b*b - 4.*a*c
      x1 = (-1.0*b + sqrt(dis)) / (2.0 * a)
      x2 = (-1.0*b - sqrt(dis)) / (2.0 * a)
c___Escritura de resultados en un archivo
      open (unit=12, file='resul.res')
c___status = 'old' es una lata, lo quitamos
      write (12,'(a)') ' Raices encontradas por fórmula general '
      write (12,100) ' Raíz X1 = ', x1
      write (12,100) ' Raíz X2 = ', x2
  100 format (2x,a20,2x,f15.8)
c____ Cuando imprime las raíces, imprime 2 espacios en blanco, la cadena, dos espacios y el numero
c____ f6.3 significan 6 casillas, 3 decimales, el punto decimal ocupa una casilla
       write (6,*) ' Se tiene el archivo resul.res'
      close (12)
      stop
      end
