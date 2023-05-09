program regresionlineal
    real, dimension(:), allocatable :: x,y
    real :: sumx, sumy, a, sumxy, sumxx
    write(*,*)'Cuantos pares de puntos hay: ';read*,n
    write(*,*)'Escribalos:'
    allocate(x(n))
    allocate(y(n))

    sumx = 0
    sumy = 0
    sumxx = 0
    sumxy = 0

    do i = 1,n
        read*,x(i),y(i)
    enddo 

    write(*,'(8x,a,16x,a,14x,a,13x,a,11x,a,10x,a)')'X','Y','sumX','sumY','sum(X**2)','sum(X*Y)'
    do i = 1,n
        sumx = sumx + x(i)
        sumy = sumy + y(i)
        sumxy = sumxy + x(i)*y(i)
        sumxx = sumxx + x(i)**2
        write(*,*)x(i),y(i),sumx,sumy,sumxx,sumxy
    enddo 


    write(*,*)' '
    a = (n*sumxy - sumx*sumy)/(n*sumxx-sumx**2)
    write(*,*)'a = ',a
    write(*,*)'b = ',(sumy-a*sumx)/n
    write(*,*)' '
    

    open(10, file = 'regresionlineal.gpl')
    write(10,*)"set xlabel 'x'"
    write(10,*)"set ylabel 'y'"
    write(10,*)"unset key"
    write(10,*)"plot ",a,"*x + ",(sumy-a*sumx)/n,"lw 3 lc rgb 'blue',\"
    do i = 1, n-1
        write(10,*)"    '-' w p ls 7 ps 2,\"
    enddo
    write(10,*)"    '-' w p ls 7 ps 2"
    write(10,*)'   '
    do i = 1, n
        write(10,*)x(i),y(i)
        write(10,*)'e'
    enddo
    close(10)
    call execute_command_line('regresionlineal.gpl')
    write(*,*)'Presiona enter para cerrar.'
    read*,
end program