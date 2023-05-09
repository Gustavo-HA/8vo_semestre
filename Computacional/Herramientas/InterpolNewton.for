       PROGRAM newtonMethod 
c
      dimension x(20),y(20,20) 
      write(*,*)'Encontrar el valor de la funcion en a'
      write(*,*)' Interpola x usando el Metodo de diferencias
     & divididas de Newton'
      write(*,*) 
      write(*,*)'Numero de parejas de datos conocido:' 
      read(*,*)n 
      write(*,*)'Entra x(i) y y(i):' 
      DO 10 i=1,n 
      read(*,*)x(i),y(i,1) 
   10 continue 
      write(*,*)'valor de x a interpolar y(x)' 
      read(*,*)a 
      k=0 
      DO 20 j=2,n 
      k=k+1 
      DO 30 i=1,n-k 
      y(i,j)=(y(i+1,j-1)-y(i,j-1))/(x(i+k)-x(i)) 
   30 continue 
   20 continue 
      write(*,*)'La tabla de diferencias divididas es : ' 
      write(*,*) 
      DO 70 i=1,n 
      write(*,90)x(i) 
      DO 80 j=1,n-i+1 
      write(*,90)y(i,j) 
   90 format(1x,F10.3) 
   80 continue 
      write(*,*) 
      write(*,*) 
   70 continue 
      s=y(1,1) 
      DO 40 j=2,n 
      p=1 
      DO 50 i=1,j-1 
      p=p*(a-x(i)) 
   50 continue 
      s=s+p*y(1,j) 
   40 continue 
      write(*,60)s 
   60 format(1x,'El valor de la funcion es:',F10.3) 
      pause
      STOP
      END
