program trapezoidal
    implicit none
    integer :: n,j
    real*8 :: h,sum,a1,integral,f,xf,xi,x2
    print*,'Intervalo de la integral: ';read*,xi,xf
    n = 100000
    h = 1.0*(xf-xi)/n
    a1 = (0.5*h)*abs(f(xf))+abs(f(xi))
    sum=0.0
    do j=1,n-1
        x2 = xi+j*h
        sum = sum+abs(f(x2))
    enddo
    Integral = h*sum+a1
    print*,'La integral numerica trapezoidal evaluada: ',Integral
end

real*8 function f(x)
    real*8 :: x
    f = 1/(x+1)**(2./3)
    return
end