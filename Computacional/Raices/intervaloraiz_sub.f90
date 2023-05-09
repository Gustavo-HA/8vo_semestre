program intervaloraiz

end program

function f(x)
    real*4 :: f,x
    f = -0.8*x*x*x*x + 6.6*x*x*x - 16*x*x + 11.7*x + 10
    return
end function 

subroutine intervaloraizsub(x00,xnn)

    real*4 :: a,b, liminf(10), limsup(10),raiz(10),f ,sign
    integer :: n, count, i, ceros
    real*4, dimension(:), allocatable :: x
    write(*,*)'Inserte el intervalo donde se encuentren las intersecciones[a,b]: '
    read*,a,b
    dx = 0.001
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

    x00 = limsup(count-1)
    xnn = liminf(count)

end subroutine