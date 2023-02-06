program nderivada
    implicit none
    real*8 :: der, x
    integer :: N,i
    print*,'Orden de la derivada:' ; read*,N
    print*,'En el punto:' ; read*,x
    do 1 i=1,N
        write(*,10)i,x,der(i,x)
    1 continue
    10 format('La derivada de grado ',i2,' de f, evaluada en ',f9.4,' es igual a: ',f20.8)
end

real*8 function f(x)
real*8 :: x
f = exp(x)
return
end

real*8 recursive function der(n,x) result(nder)
    integer :: n
    real*8 :: x, h = 1.0e-6, f
    if (n.eq.1) then
    nder = (f(x+h)-f(x-h))/(2*h)
    return
    else if (n.eq.2) then
    nder = (f(x+h)-2*f(x)+f(x-h))/(h**2)
    return
    else if (n .gt. 2) then
    nder = (der(n-1,x+h)-der(n-1,x-h))/(2*h)
    return
    else
    print*,"ERROR, N DEBE SER ENTERO MAYOR O IGUAL A 1"
    end if
END