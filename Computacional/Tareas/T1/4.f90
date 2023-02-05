program problema4
    real*8 :: dl = 1.e-9, a,b,c,dx,f
    write(*,*) 'Dame los extremos del intervalo:'
    read*,a,c
    dx = c-a ! Tamaño del intervalo
    istep = 0 ! Contador

    ! Se evalua la funcion en los extremos del intervalo
    write(*,20)a,f(a)
    write(6,20)c,f(c)
    20  format ('f(',f6.3,') = ',f13.8)

    do 100 while (abs(dx).gt.dl) ! Define cuando parar
        b = (a+c)/2. ! Se define b
        if ((f(a)*f(b)).lt.0.0) then
            c = b
            dx = c-a
        else
            a=b
            dx = c-a
        end if 
        istep = istep +1
    100 end do 
    
    write (*,666) 'Iteracion','Raiz','Tolerancia'
    write (6,999) istep,b,dx
    
    
    stop
    666 format(2x,a,10x,a,9x,a)
    999 format(3x,i4,5x,2f16.10,3x)



end

function f(theta)
    real*8 f , theta
    f = 60.*tan(theta)-(60.**2 * 32.2)/(2*50.**2 * cos(theta)**2)+6.5-7.
    return
end function 