program geometrica
    real*8 :: sum=0,x, error, dif=10
    integer :: i
    write(*,'(a)',advance='no')'Punto a evaluar: ';read*,x    
    error = 10.d0**(-2)
    i=0
    do while (abs(dif).gt.error)
        sum = sum + (-1)**(i) * x**(2*i)*2**(-i)
        dif = (-1)**(i) * x**(2*i)*2**(-i)
        i=i+1
        print*,i,sum
    enddo
end program
