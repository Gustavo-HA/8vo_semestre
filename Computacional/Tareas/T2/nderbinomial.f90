program derivadabinomial
    implicit none
    integer :: n,i
    real*8 :: derivada,x

    print*,'Orden de la derivada a calcular: ';read*,N
    print*,'Punto donde se quiere calcular: ';read*,x
    do 2 i = 1,N
        write(*,*)'La derivada de grado ',i,' en el punto es: ',derivada(x,i)
    2 continue
    stop
end

real*8 function f(x)
implicit none
    real*8 :: x
    f = exp(x)
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

function combination(n,k)
    implicit none
    integer :: n,k,combination,factorial
    combination = factorial(n)/(factorial(n-k)*factorial(k))
    return
end

function derivada(x,n)
    implicit none
    integer :: n,k,combination
    real*8 :: x,derivada,h=1.0e-2 ,sum,f
    sum=0
    do 1 k=0,N
        sum = sum + ((-1)**(k+n))*combination(n,k)*f(x+k*h)
    1 continue    
    derivada = sum*(1/h**n)
    return
end