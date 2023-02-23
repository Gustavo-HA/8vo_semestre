program ndercentral
    implicit none
    integer, parameter :: qp = selected_real_kind(33, 4931)
    real(qp) :: x, nder
    integer :: n !, i
    print*,'Orden de la derivada: ';read*,n 
    print*,'Punto a evaluar: ';read*,x
    write(*,'(a,i2,a,f5.2,a,f50.33)')'La derivada de orden ',n,' en el punto ',x,' es igual a: ',nder(x,n)
end

function f(x)
    implicit none
    integer, parameter :: qp = selected_real_kind(33, 4931)
    real(qp) :: x, f
    f = sin(x)
    return
end

recursive function factorial(n) result(fact)
    implicit none
   integer :: N,fact
   if (n/=1.and.n/=0) then
   fact = factorial(n-1)*N
   return
   else
   fact = 1
    end if
   return
end

integer function combination(n,k)
    implicit none
    integer :: n,k,factorial
    combination = factorial(n)/(factorial(n-k)*factorial(k))
    return
end

function nder(x,n)
    implicit none
    integer :: n, k, combination
    integer, parameter :: qp = selected_real_kind(33, 4931)
    real(qp) :: f, x, sum = 0, nder
    real(qp), parameter :: h=1.0e-7
    do 1 k=0,n
        sum = sum + (-1)**(k) * combination(n,k) * f(x+(n-2*k)*h)
    1 continue
    nder = sum / ((2*h)**n)
    return
end