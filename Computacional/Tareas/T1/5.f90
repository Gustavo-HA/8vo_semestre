program biseccion
    real*8 :: dl = 1.e-6
    write(*,*) 'Dame los extremos del intervalo:'
    read*,a,c
    dx = c-a ! Tamaño del intervalo
    istep = 0 ! Contador

    ! Se evalua la funcion en los extremos del intervalo
    write(*,20)a,f(a)
    write(6,20)c,f(c)
    20  format ('f(',f6.3,') = ',f11.8)

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
    999 format(3x,i4,5x,2f16.8,3x)
end

function f(x)
    f = 9.4*2.4*10*x*(1-x/(sqrt(x**2+0.1**2)))/(2*0.885) - 0.3
    return
end