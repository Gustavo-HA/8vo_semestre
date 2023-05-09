program geometrica
    real*8 :: sum=0,x, error, dif=10
    integer :: i
    write(*,'(a)',advance='no')'Punto a evaluar: ';read*,x    
    error = 10.d0**(-5)
    i=1
    do while(dif.ge.error)
        sum = sum + (-1)**(i+1) * x**(i)/gamma(real(i+1))
        dif = (-1)**(i+1) * x**(i)/gamma(real(i+1))
        i=i+1
        print*,i,sum
    enddo
end program
