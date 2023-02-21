program Simpsonintegral
    integer :: n,j
    real*4 :: x2,xi,xf,h,a1,sum,f,i

    print*,'Intervalo inferior y superior';read*,xi,xf
    n=1000
    h = 1.0*abs(xf-xi)/n
    a1 = (h/3.0)*(f(xi)+f(xf))
    i=0
    sum=0

    do j=1,n-1
        x2 = xi + h*j
        if (mod(j,2).eq.1) then
            sum = sum + 4.0*f(x2)
        else
            sum = sum + 2.0 * f(x2)
        end if
        i = h/3.0 * sum + a1
    end do
    write(*,*)'El area de la integral es:',i
    stop
end

function f(x)
    real :: x
    !1 f = sin(x**2.)
    !2 f = (5.*x+12.)/((x)*(x-4.))
    !3 f = x**2. * exp(x)
    !4 f = asin(x)
 ! f = (1.)/(3.*x**2.+6.*x-7.)
    f = sqrt(4.-x**2)/(x**2.)
    return
end