c      Metodo de Lagrange para interpolar
c      datos

c Se define un maximo de 20 parejas de datos
      dimension x(20),y(20)
      REAL L,s
      write(*,*) '  '
      write(*,*) 'Encuentra el valor de la funcion en x '
      write(*,*) 'usando la interpolacion de lagrange.'
      write(*,*) '  '
      write(*,*)'Cuantas parejas de datos son conocidas:'
      write(*,*) '  '
      read(*,*)n
      write(*,*)'Entra X(i) y Y(i) separados por coma:'
      
c     Se leen las parejas x, y
      DO 10 i=0,n-1
      read(*,*)x(i),y(i)
   10 continue
   
c Se pide el valor a interpolar
   45 write(*,*) '  '
      write(*,*)'Entra el valor de x que quieres conocer:'
      read(*,*)a
      s=0      
c Ciclo con dos indices
      write(*,*)'Terminos de los coeficientes de Lagrange :'
      DO 20 i=0,n-1
      p=1
c Se imprime la etiqueta del coeficiente
      write(*,520)i
  520 format(1x,'L',I2)
      DO 30 j=0,n-1
      
c Coeficientes de Lagrange  
c si i=j entonces no se imprime   

      IF(i.NE.j) then 

      L=(a-x(j))/(x(i)-x(j))
      write(*,610)j,i,j,X(j),X(i),X(j)
  610 format(1x,'{X-','(X(',I2,')/(X(',I2,')-(X',I2,')}= '
     & 1x,'(X-',F5.2,')/(',F5.2,'-',F5.2,')')

      p=p*L
      endif
      
   30 continue  
      write(*,200)i,y(i)
  200 format('Y(',I2,')= ',F7.4 )    
c  
c Sumando los terminos del polinomio de Lagrange      
      s=s+p*y(i)
     
      write(*,*)'----------------------------------'
   20 continue
      
       write(*,40)a,s
   40 format(1x,'El valor de la funcion en x =',F7.2,' es:',F7.4)
      write(*,*)'   '

      write(*,*)'Entra 1 para seguir, 0 para terminar'
      read(*,*)k
      if (k.eq.1) goto 45
      STOP
      END
