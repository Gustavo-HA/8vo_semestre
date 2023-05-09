C---  Programa que demuestra errores
c  por redondeo al sumar a 1.0
c diezmil veces 0.00001
c al final 10000*0.00001 debe dar 0.1
c 1 + 0.1= 1.1
c Si usamos doble precision:
c 1+0.1= 1.09999

      PROGRAM Numpeq
      REAL*8 suma    !indicando doble presicion
      REAL suma1
      suma= 1.0
      suma1= 1.0
      
      do 47 i = 1, 10000
      suma= suma+ 0.7
      suma1= suma1+ 0.7
   47 continue
   
      write(*,*)"Simple P.=",suma1 ,"Doble P.=",suma
      STOP
      END 
