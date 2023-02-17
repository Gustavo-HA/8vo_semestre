      PROGRAM Significativos
C  Se muestra como el error relativo aumenta cuando
C se restan 2 numeros muy cercanos.

      Real*8 x,y,z !double precision
      x=0.714251d0
      y=5.d0/7.d0 !Los numeros estan en doble precision
                  !Es valido dble(y)=dble(5.)/dble(7.)
      xs=0.714251
      ys=5./7. !Los numeros estan en simple precision
                  
      z= y-x       !Resta en doble precision
      Zs=ys-xs     !Resta en simple precision

       errorRelativo = abs(Z-Zs)/z  *100.
       
      write (*,10)x
      write (*,20) y
      write (*,30) z
      write (*,*) " "
      write (*,40) zs
      write (*,*) " "
      write (*,*) "error relativo" , errorRelativo
   10 Format('x con doble precision: ',F20.17)! Se imprimen 16 digitos significativos
   20 Format('y con doble precision: ',F20.17)!
       write (*,*) " "
   30 Format('y-x (doble precision)  :',F20.17)!
   40 Format('y-x (simple precision): ',F20.17)!
      Pause
      Stop
      end
