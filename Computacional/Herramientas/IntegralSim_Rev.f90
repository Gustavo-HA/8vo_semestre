program Simpsonintegral
    integer :: n,j
    real*4 :: x2,xi,xf,h,a1,sum,g,i

    print*,'Intervalo inferior y superior';read*,xi,xf
    print*,'Numero de intervalos';read*,n

    h = 1.0*abs(xf-xi)/n
    a1 = (h/3)*(g(xi)+g(xf))

    do j=1,n-1
        x2 = xi + h*j
        if (mod(j,2).eq.1) then
            sum = sum + 4.0*g(x2)
        else
            sum = sum + 2.0 * g(x2)
        end if
        i = h/3.0 * sum + a1
    end do

    write(*,*)'El area de la integral es:',i
    stop
end

function g(x)
    g = sqrt(4-x**2)
    return
end

real*4 recursive function der(n,x) result(nder)
    integer :: n
    real :: x, h = 1.0e-4, g
    if (n.eq.1) then
    nder = (g(x+h)-g(x-h))/(2*h)
    return
    else if (n.eq.2) then
    nder = (g(x+h)-2*g(x)+g(x-h))/(h**2)
    return
    else if (n .gt. 2) then
    nder = (der(n-1,x+h)-der(n-1,x-h))/(2*h)
    return
    else
        
    print*,"ERROR, N DEBE SER ENTERO MAYOR O IGUAL A 1"
    end if
END


function f(x)
    real, parameter :: pi = 4.0*atan(1.0)
    f= 2*pi*g(x)*sqrt(1+(der(1,x))**2)
    return
end