program intervaloraiz
    ! Dado un intervalo [a,b], hallar cuantas raices tiene la funcion.
    real*4 :: a,b, liminf(10), limsup(10),raiz(10),f ,sign! Imprime los intervalos de hasta 10 raices y 10 exactas
    integer :: n, count, i, ceros
    real*4, dimension(:), allocatable :: x! Vector dinamico, tendra dimension n
    
    write(*,*) '---------------------------'
    write(*,'(9x,a)')'PROBLEMA 6'
    write(*,*) '---------------------------'
    write(*,'(a)',ADVANCE='NO')'Inserte el intervalo [a,b]: '
    read*,a,b
    write(*,'(A)',advance='no')'Introduzca el valor de dx: '
    read*,dx
    write(*,*) '----------------------------'
    n = int((b-a)/dx) !Numero de pasos
    count = 0 ! Contador de intervalos encontrados
    m = 0 !Raices exactas encontradas

    allocate(x(n)) ! Aloja en la memoria el vector con n componentes

    ! Llenar el array
    x(1) = a
    do 1 j = 2,n
        x(j) = a + (j-1.)*dx
    1 continue

    ceros=0
    ! Verificamos el numero de raices
    do i=1,n-1
        sign = f(x(i))*f(x(i+1))
        if (sign.lt.0.) then
            count = count + 1
            liminf(count) = x(i)
            limsup(count) = x(i+1)
        else if (sign==0.0.and.f(x(i))==0.0) then
            ceros = ceros+1
            raiz(ceros) = x(i)
        endif
    enddo

    deallocate( x ) ! Se desaloja

    print*,'RESULTADO:'
    if (count+ceros /= 0) then
        write(*,10)count+ceros,a,b
        do 2 k = 1,count
            write(*,11)liminf(k),limsup(k)
        2 continue
        do 3 k = 1,ceros
            write(*,13)raiz(k)
        3 continue
    else
        write(*,12)a,b
    end if

    10  format('En total, se encontraron',1x,i1,1x,'raiz/ces en el intervalo [',f8.3,',',f8.3,'].')
    11  format('Se encontro una raiz en [',f8.3,',',f8.3,'].')
    12  format('No se han encontrado raices en el intervalo [',f8.3,',',f8.3,'].')
    13  format('Se ha encontrado una raiz exacta en x =',f13.8)

end program

function f(x)
    real*4 :: f,x
    f = -0.8*x*x*x*x + 6.6*x*x*x - 16*x*x + 11.7*x + 10
    return
end function 