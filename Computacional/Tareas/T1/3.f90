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

    ! Funcion a evaluar 
    function f(x)
    real ::  x, f
    f = 100*(1+5.485e-3*x+6.65e-6*x**2+2.8055e-11*x**4-2.0e-17*x**6)-300 
    end
    
    ! Derivada de la funcion 
    function df(x)
    real :: df,x
    df = 100*(5.485e-3+2*6.65e-6*x+4*2.8055e-11*x**3-6*2.0e-17*x**5)
    end
    