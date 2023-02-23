program Simpsonintegral
    real*8 :: h,a1,xf,xi,f,x2,sum=0,i
    integer :: j,n

    print*,'Intervalo inferior y superior';read*,xi,xf
    print*,'Numero de intervalos';read*,n


    h = 1.0*abs(xf-xi)/n
    a1 = (h/3.d0)*(f(xi)+f(xf))

    do j=1,n-1
        x2 = xi + h*j
        if (mod(j,2).eq.1) then
            sum = sum + 4.0*f(x2)
        else
            sum = sum + 2.0 * f(x2)
        end if
    end do
    i = h/3.0 * sum + a1
    write(*,*)'El valor de la integral es:',i
    stop
end


real*8 function f(x)
    real*8 :: x
    f= x**9
    return
end