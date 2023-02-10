program trapezoidal
    implicit none
    integer :: n,j
    real*8 :: h,sum,a1,integral,f,xf,xi,x2
    print*,'Intervalo de la integral: ';read*,xi,xf
    n = 100000
    h = 1.0*(xf-xi)/n
    a1 = (0.5*h)*(f(xf)+f(xi))
    sum=0.0
    do j=1,n-1
        x2 = xi+j*h
        sum = sum+f(x2)
    enddo
    Integral = h*sum+a1
    print*,'La integral numerica trapezoidal evaluada: ',Integral
end

real*8 function f(x)
    real*8 :: x
    f = 1/(sqrt(5-7*x**2))
    return
end