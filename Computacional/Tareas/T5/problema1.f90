program problema1
    implicit none
    integer :: n,j
    real*8 :: h,sum,a1,integral,f,xf,xi,x2,integral2, intverd = 8*4.0*atan(1.0)
    write(*,'(a)',advance='no')'Intervalo de la integral: ';read*,xi,xf

    print*,'---------------------- h arbitraria -----------------------'
    write(*,'(a)',advance='no')'Seleccione un valor para h: ';read*,h
    n = int((xf-xi)/h)
    a1 = (0.5*h)*(f(xf)+f(xi))
    sum=0.0
    x2 = xi
    do j=1,n-1
        x2 = x2+h
        sum = sum+f(x2)
    enddo
    Integral = h*sum+a1
    print*,'La integral numerica trapezoidal evaluada: ',Integral

    print*,'--------------------- n arbitraria ----------------------'
    write(*,'(a)',advance='no')'Seleccione un valor para n: ';read*,n
    h = (xf-xi)/n
    a1 = (0.5*h)*(f(xf)+f(xi))
    sum=0.0
    x2 = xi
    do j=1,n-1
        x2 = x2+h
        sum = sum+f(x2)
    enddo
    Integral2 = h*sum+a1
    print*,'La integral numerica trapezoidal evaluada: ',Integral2

    print*,'-------------------- Error absoluto -----------------------'
    write(*,'(a,f18.13)')'Error absoluto con h arbitraria: ',abs(Integral-Intverd)
    write(*,'(a,f18.13)')'Error absoluto con n arbitraria: ',abs(Integral2-Intverd)
end

real*8 function f(x)
    real*8 :: x
    f = sqrt(16.0-x**2)
    return
end