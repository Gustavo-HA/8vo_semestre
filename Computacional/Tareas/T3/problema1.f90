program Simpsonintegral
    integer :: n,j
    real*4 :: x2,xi,xf,h,a1,sum,f,i,c

    print*,'Intervalo inferior y superior';read*,xi,xf
    print*,'Numero de intervalos';read*,n
    print*,'Valor de la constante a: ';read*,c
    h = 1.0*abs(xf-xi)/n
    a1 = (h/3.0)*(f(xi,c)+f(xf,c))
    i=0
    sum=0

    do j=1,n-1
        x2 = xi + h*j
        if (mod(j,2).eq.1) then
            sum = sum + 4.0*f(x2,c)
        else
            sum = sum + 2.0 * f(x2,c)
        end if
        i = h/3.0 * sum + a1
    end do
    write(*,*)'El area de la integral es:',i
    stop
end

function f(x,c)
    real :: x,c
    f = (1+c*(1-cos(x))**2)/((1+c*sin(x)**2)*sqrt(1+2*c*(1-cos(x))))
    return
end