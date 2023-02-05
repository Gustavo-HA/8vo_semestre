        program tipos
C En fortran existen enteros, cadenas y reales. Punto decimal reales, sin punto decimal enteros

C       Division de enteros y reales
       write (*,*) '    '
       write (*,*) '(5/2)=                 ', (5/2)
c Se trunca la parte decimal, resulta 2

C       Entero ente un real
       write (*,*) '(5/2.0)=               ', (5/2.0)
c cuando se hace una operacion entre un entero y un real el resultado es un real

       
       write (*,*) '(5/2 + 5/2.0)=           ', (5/2+5/2.0) ! resulta 4 
       write (*,*) '5/2*2=                  ', 5/2*2 ! Las operaciones son por jerarquia, de izquierda a derecha
       write (*,*) '5*2/2=                  ', 5*2/2 !5
       write (*,*) '2*(5/2)=                ', 2*(5/2)
       write (*,*) '2.0*(5/2)=              ', 2.0*(5/2)
       write (*,*) '    '
       write (*,*) '    '
C      Se puede convertir el tipo en las expresiones
       write (*,*) 'real(5)/real(2)*real(2)=', real(5)/real(2)*real(2)
       write (*,*) 'real(5)/2*2=            ', real(5)/2*2
       write (*,*) 'real(5/2*2)=            ', real(5/2*2)
       write (*,*) 'int(5/2.2)*5=           ', int(5/2.2)*5
       write (*,*) '    '
       PAUSE
       STOP
       end
