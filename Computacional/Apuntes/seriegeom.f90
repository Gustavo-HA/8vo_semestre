program geometrica
    real*8 :: sum=0,x, error, dif=10
    integer :: i,N
    write(*,'(a)',advance='no')'Punto a evaluar: ';read*,x    
    write(*,'(a)',advance='no')'Orden a evaluar: ';read*,n
    error = 10.d0**(-5)
    i=0
    do while (dif.gt.error)
        sum = sum + x**i
        i=i+1
        dif = x**i
        print*,sum
    enddo
    print*,sum
end program