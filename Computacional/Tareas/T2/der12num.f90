program der12num
    implicit none
    integer :: N
    real*8 :: x, der1,der2

    print*,'Orden de la derivada: ';read*,N
    print*,'Punto a evaluar: ';read*,X
    if (n==2) then 
        print*,'La segunda derivada de la funcion en el punto es: ',der2(x)
    else if (n==1) then 
        print*,'La primera derivada de la funcion en el punto es: ',der1(x)
    end if 
end

real*8 function f(x)
    real*8 :: x
    f = exp(x)
    return
end

real*8 function der1(x)
    real*8 :: x,h=1.0e-7,f
    der1 = (f(x+h)-f(x-h))/(2*h)
    return
end

real*8 function der2(x)
    real*8 :: x,h=1.0e-7,f
    der2 = (f(x+2*h)-2*f(x)+f(x-2*h))/(4*h**2)
    RETURN
END