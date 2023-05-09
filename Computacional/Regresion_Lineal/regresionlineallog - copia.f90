program regresionlineallog
    real, dimension(:), allocatable :: x,y
    real :: sumx, sumy, a, sumxy, sumxx, M = 63.6, masa = 39.1, L = 30, D = 6, J = 18, alpha, beta, gama,g = 9.79
    alpha = 102.6763
    beta = M*(L/2.-j)**2 + masa*(L-j-D/2.)**2 + M*L**2 / 12. + masa*D**2 /12.
    gama = g*alpha
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
        x(i) = log10(x(i) + beta/(alpha*x(i)))
        y(i) = log10(y(i))
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
    write(*,*)'N = ',a
    write(*,*)'log(K) = ',(sumy-a*sumx)/n
    write(*,*)'K = ',10**((sumy-a*sumx)/n)
    write(*,*)' '
    

    open(10, file = 'regresionlineallog.gpl')
    write(10,*)"set xlabel 'x'"
    write(10,*)"set ylabel 'y'"
    write(10,*)"unset key"
    write(10,*)"plot ",10**((sumy-a*sumx)/n),"*x**", a ,"lw 3 lc rgb 'blue',\"
    do i = 1, n-1
        write(10,*)"    '-' w p ls 7 ps 2,\"
    enddo
    write(10,*)"    '-' w p ls 7 ps 2"
    write(10,*)'   '
    do i = 1, n
        write(10,*)10**x(i),10**y(i)
        write(10,*)'e'
    enddo
    close(10)
    call execute_command_line('regresionlineallog.gpl')
    write(*,*)'Presiona enter para cerrar.'
    read*,
end program