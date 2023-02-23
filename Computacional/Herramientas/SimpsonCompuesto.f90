program SimpsonCompuesto
    integer :: n=2, m
    real*8 :: xi,xf,dl=10,i,auxi=3.0e8,tolerancia

    write(*,'(a)',advance='no')'Extremos del intervalo inferior y superior: ' ;read*,xi,xf
    write(*,'(a)',advance='no')'Digitos decimales correctos: ';read*,m

    tolerancia = 10.0**(-m)
    do while (dl.gt.tolerancia)
        call simple(xi,xf,n,i)
        print*,n,i
        n=n*2
        dl = abs(i-auxi)
        auxi=i
    end do

    write(*,*)'El valor de la integral es:',i
    stop
end

real*8 function f(x)
    real*8 :: x
    f=exp(x)
    return
end

subroutine simple(xi,xf,n,i)
    
    implicit none
    real*8 :: h,a1,xf,xi,f,x2,sum=0.0,i
    integer :: j,n

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
    return
end subroutine