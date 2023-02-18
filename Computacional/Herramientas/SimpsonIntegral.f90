program Simpsonintegral
    integer :: n,j
    real*4 :: x2,xi,xf,h,a1,sum,f,i

    print*,'Intervalo inferior y superior';read*,xi,xf
    print*,'Numero de intervalos';read*,n

    h = 1.0*abs(xf-xi)/n
    a1 = (h/3)*(f(xi)+f(xf))

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
    f= sqrt(1+x**2)
    return
end