      Program ErrorRedondeo
! Se evalua una funci¢n que contiene resta de numeros cercanos
       Real*4 x, y, z   ! Precision simple
       Real*8 xd,yd,zd  !Doble precision, significa 8 bytes
                        ! Tiene una aprox. de 15-17 digitos
                        
!      PRUEBE usar 5 digitos y verifique que la 2da ec. da mejor approx.
      x=0.007
      y=(1.0-cos(x))/sin(x)  !Ec. original
      z=sin(x)/(1.0+cos(x))  ! 2da Ecuacion como una suma (mejor aprox.)
      
      yd=dble((1.0-cos(x))/sin(x)) !Usando doble precision
      zd=dble(sin(x)/(1.0+cos(x)))
      
      error=   abs(yd-y ) /yd
      errorSuma=   abs(zd-z )/zd
      
      write(*,10)y
   10 format ("Con resta= ", F27.21)

      write(*,20)z
   20 format ("Con suma=  ", F27.21)
       write(*,*)" "
        write(*,30)yd
   30 format ("Con resta Doble precision= ", F27.21)
      write(*,40)zd
   40 format ("Con suma Doble precision=  ", F27.21)
       write(*,*)" "
       write(*,50)error
   50 format ("Error resta relativo=  ", F27.21)
      write(*,60)errorSuma
   60 format ("Error suma relativo=   ", F27.21)
   
      pause
      stop
      end
