c
c
c Este programa muestra como utilizar variables complejas
c
c___________________________________________________________________
c___Declaracion de las variables complejas
      complex*8 z1,z2,z3
      
c___Asignacion de una variable compleja
      z1 = ( 2.0 , 3.0 )
c___Calculo del conjugado de una variable compleja
      z2 = conjg(z1)
c___Calculo del producto de dos numeros complejos
      z3=z1*z2

c    La multiplicacion de 2 numeros complejos se
c realiza de la siguiente manera:
c
c  zz* = (x+iy) (x-iy)= x2 + iyx -iyx -i2 y2
c      = x2 + y2
c para el ejemplo tenemos:

c z1 = 2 + i*3
c z2=  2 - i*3
c
c z3= z1*z2 = 2*2 + 3*3
c z3= 13 

c___Calculo de las partes reales e imaginarias
      x1 = real (z1)
      y1 = aimag (z1)
      x2 = real (z2)
      y2 = aimag (z2)
      x3 = real (z3)
      y3 = aimag (z3)
c___Escritura por
      write (6,*)' z1 =', cmplx(x1,y1)
      write (6,*)' z2 =',cmplx(x2,y2)
      write (6,*)' z3 =',cmplx(x3,y3)



      write (6,*) ' x1 y y1 =', x1 , y1
      write (6,*) ' x2 y y2 =', x2 , y2
      write (6,*) ' x3 y y3 =', x3 , y3
      
      
      stop
      end
