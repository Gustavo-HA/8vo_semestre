program Simpsonintegral
    integer :: n,j
    real :: x2,xi,xf,h,a1,sum,f,i

    read*,xi
    read*,xf
    n = 100
    h = 1.0*abs(xf-xi)/n
    a1 = (h/3.0)*(f(xi)+f(xf))
    I=0
    sum=0.0

    do j=1,n-1
        x2 = xi+j*h
        if (mod(j,2).eq.1) then
            sum = sum+4.0*f(x2)
        else
            sum = sum+2.0*f(x2)
        end if
        I = h/3.0 * sum +a1
    end do

    write(*,*)'El área de la integral es: ',i
    stop
end

function f(x)
    f = exp(x)
    return
end
