program areacurvas
    integer :: n,j
    real*4 :: x2,xi,xf,h,a1,sum=0,f,i=0
    call intervaloraizsub(xi,xf)
    write(*,'(a,f10.6,a1,f10.6,a2)')'El intervalo acotado es aproximadamente [',xi,',',xf,'].'
    n=1000
    h = 1.0*abs(xf-xi)/n
    a1 = (h/3.0)*(f(xi)+f(xf))
    do j=1,n-1
        x2 = xi + h*j
        if (mod(j,2).eq.1) then
            sum = sum + 4.0*f(x2)
        else
            sum = sum + 2.0 * f(x2)
        end if
        i = h/3.0 * sum + a1
    end do
    write(*,*)'El area de la integral es:',i
    stop
end
! ***************** Funciones que acotan el área ******************
function g(x)
    g = x**2.0-3.0
    return
end
function p(x)
    p = 1.0
    return
end
! ***** Se define la función positiva del área |g(x)-p(x)| *******
function f(x)
    real :: x
    if (g(x).gt.p(x)) then
        f = g(x)-p(x)
    else
        f = p(x)-g(x)
    end if
    return
end
! Definimos la subrutina para encontrar las intersecciones de la gráfica, hallando los
! intervalos donde la diferencia de las funciones son 0, con ayuda del programa para encontrar
! raíces dadas un intervalo.
subroutine intervaloraizsub(x00,xnn)
    real*4 :: a,b, liminf(10), limsup(10),raiz(10),g,p,sign
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
        sign = (g(x(i))-p(x(i)))*(g(x(i+1))-p(x(i+1)))
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

    ! Guarda el intervalo dadas las condiciones
    x00 = limsup(count-1)
    xnn = liminf(count)
end subroutine