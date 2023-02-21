program Simpsonintegral
    integer :: n,j
    real*4 :: x2,xi,xf,h,a1,sum=0,f,i=0
    print*,'Intervalo inferior y superior';read*,xi,xf
    n=1000
    h = 1.0*abs(xf-xi)/n
    a1 = (h/3.0)*(f(xi)+f(xf))
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
! ***************** Funciones que acotan el área ******************
function g(x)
    g = 2*log(x)
    return
end
function p(x)
    p = log(x)
    return
end
! ***** Se define la función positiva del área |g(x)-p(x)| *******
function f(x)
    real :: x
    if (g(x).gt.p(x)) then
        f = g(x)-p(x)
    else
        f = p(x)-g(x)
    end if
    return
end
