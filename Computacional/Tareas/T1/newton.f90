program Newton
    implicit none
    integer :: i, imax
    real ::  x , xnuevo, toler,f,df,dx
    parameter (imax = 100) 
    parameter (toler = 1.e-6)  
    print*, 'Valor inicial ' ; read*, x 
    i = 0 ; dx = 1
        print*, '-----------------------------------------------------'
        print*, '| Pasos |    Aproximacion Raiz   |        f(x)       | ' 
    do while (i<imax.and.(abs(dx))>toler)
      i = i + 1                     !     
      xnuevo = x - f(x)/df(x)       
      dx = xnuevo-x
      x = xnuevo                    
      write(*,'(2x,i4,9x,f17.13,2x,f17.8)') i, xnuevo, f(xnuevo)
    end do
    if (i>imax-2) then
        write(*,*) 'No converge en 100 iteraciones.' 
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
    f = -0.8*z*z*z*z + 6.6*z*z*z - 16*z*z + 11.7*z + 10 
end
function df(z)
    real :: df,z
    df = - 3.2*z*z*z + 19.8*z*z - 32*z + 11.7 
end