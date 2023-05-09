C---  Program Suma0.7

c     Se suma 0.7 n veces para demostrar
c      como se va acumulando el error

      PROGRAM SumaZero7
      REAL*8 suma    !indicando doble precision
      REAL suma1
      suma= 1.0
      suma1= 1.0
      
      do 47 i = 1, 10000
      suma= suma+ 0.7
      suma1= suma1+ 0.7
   47 continue
   
      write(*,*)"Simple P.=",suma1 ,"Doble P.=",suma
      Pause
      STOP
      END 
