program derivar
    real*8 :: x,der
    read*,x
    write(*,*)'La derivada de f(x) en el punto x es: ',der(x)
    write(*,*)'El angulo es de: ' ,atan(der(x))
    stop
end

function f(x)
    real*8 :: x,f
    f = exp(-x**2)
    return
end

function der(x)
    real*8 :: x,der,f,dx
    dx = 1.0e-9
    der = (f(x+dx)-f(x-dx))/(2*dx)
    return
end