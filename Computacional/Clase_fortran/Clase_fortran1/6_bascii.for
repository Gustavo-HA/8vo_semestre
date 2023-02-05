      program ascii
!Me imprime el codigo ascii del 0 al 255.
      open(9,file='ascii.dat')
         
      DO 89 I=0,255
     	WRITE(9,2)I,CHAR(I)  
    2 format(1x,I3,3x,A6)
   89 CONTINUE
      stop
      end program

