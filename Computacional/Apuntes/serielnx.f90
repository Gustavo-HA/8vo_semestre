program serielnx
    real*8 :: sum=0,x, error, dif=10.0
    integer :: i
    write(*,'(a)',advance='no')'Punto a evaluar: ';read*,x    
    error = 10.d0**(-5)
    i=0
    do while(dif.ge.error)
        sum = sum + 2*(x-1)**(2*i+1.0)/((2.0*i+1.0)*(x+1.0)**(2*i+1))
        dif = 2*(x-1)**(2*i+1.0)/((2.0*i+1.0)*(x+1.0)**(2*i+1))
        i=i+1
        print*,i,sum
    enddo
    write(*,'(a,i2,a,f8.6)')'Se necesitaron',i,' terminos para obtener un error menor a: ',error
end program
