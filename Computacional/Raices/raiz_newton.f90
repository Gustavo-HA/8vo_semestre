program Newton

    implicit none
    
    integer :: i, imax
    real ::  x , xnuevo, toler,f,df,dx
    parameter (imax = 100)  ! Definimos las iteraciones maximas que debe correr el programa 
    parameter (toler = 1.e-6)  ! Tolerancia de error 
    
    print*, 'Valor inicial ' ; read*, x 
    ! Para empezar con la interacion y pasos 
    i = 0 ; dx = 1
        print*, '-----------------------------------------------------'
        print*, '| Pasos |    Aproximacion Raiz   |        f(x)       | ' 
    
    ! Metodo Newton (Si |x nuevo| > x )
    do while (i<imax.and.(abs(dx))>toler)
      i = i + 1                     ! Contador       
      xnuevo = x - f(x)/df(x)       ! Idea de Newton (y Raphson?)
      dx = xnuevo-x
      x = xnuevo                    ! Cambio de x 
      write(*,'(2x,i4,9x,f17.13,2x,f17.8)') i, xnuevo, f(xnuevo)
    end do
    
    ! Si llega a las iteraciones maximas, marcar error (Depende tolerancia)
    if (i>imax-2) then
    
        write(*,*) '# Error: Este metodo no converge, podrias reducir el valor de la tolerancia' 
        else
        print*, '-----------------------------------------------------'
        write(*,'(a14,f20.17)') 'Tolerancia : ', toler
        print*, '-----------------------------------------------------'
        write(*,'(a8,f30.25)') 'Raiz : ', xnuevo
        print*, '-----------------------------------------------------'
    
    endif
    
end program
    
function f(z)
    real ::  z, f    
    ! f = z*z*z - 8 !a)
    f = -0.8*z*z*z*z + 6.6*z*z*z - 16*z*z + 11.7*z + 10 !b)
    ! f = z*exp(0.5*z) + 1.2*z - 5 !c)
    ! f = cos(z)*cosh(z) + 1 !d)
    ! f = z - 2*exp(-z ) !e)
end
        
        ! Derivada de la funcion 
function df(z)
    real :: df,z
    ! df = 3*z*z !a)
    df = - 3.2*z*z*z + 19.8*z*z - 32*z + 11.7 !b)
    ! df = exp(0.5*z)*(0.5*z + 1) + 1.2 !c)
    ! df = sinh(z)*cos(z) - sin(z)*cosh(z) !d)
    ! df = 1 + 2*exp(-z) !e)
end