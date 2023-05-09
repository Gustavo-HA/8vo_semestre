program serietaylor
    implicit none
    real*16 :: a,xf,xi,h=0.01,serief
    integer :: n 
    character(1000) :: serie='datos.dat', plot = 'serietaylor.gpl', xin, xfin

    ! Se definen los parámetros para la expansión en serie de taylor

    write(*,'(a)',advance='no')'Orden de la expansion: ';read*,n
    write(*,'(a)',advance='no')'Alrededor del punto: ';read*,a
    write(*,'(a)',advance='no')'Extremos del intervalo a graficar: ';read*,xi,xf
    write(xin,'(f8.3)')xi
    write(xfin,'(f8.3)')xf

    ! Escribe en un archivo los puntos de la expansión en serie

    open(10,file=serie)
    do while (xi.lt.xf)
        write(10,*)xi,serief(n,xi,a)
        xi = xi+h
    enddo
    close(10)

    ! Genera el script en gnuplot con el nombre serietaylor.gpl

    open(11,file=plot)
    write(11,*)'set xrange ['//trim(xin)//':'//trim(xfin)//']'
    write(11,*)'set mxtics 4'
    write(11,*)'set mytics 4'
    write(11,*)'set xlabel "x"'
    write(11,*)'set ylabel "y"'
    write(11,*)'set zeroaxis lt -1 lc -1'
    write(11,*)'plot "datos.dat" u 1:2 w l lw 2 t "Expansion en serie"'
    close(11)

end

! Expansión en serie de taylor, requiere de la subrutina de nderivada
function serief(n,x,a)
    implicit none
    real*16 :: x,serief,nderivada, a
    integer :: n, i
    serief = 0
    do i = 0, n
        call nder(a,i,nderivada)
        serief = serief + (nderivada/gamma(real(i+1)))*(x-a)**i
    enddo
    return
end function

! Nderivada, cuadruple precisión
subroutine nder(x,n,resultado)
    implicit none
    integer :: n, k
    real*16 :: f, x, sum, resultado, combination
    real*16, parameter :: h=1.0e-2
    sum=0
    do 1 k=0,n
        combination = gamma(real(n+1))/(gamma(real(n-k+1))*gamma(real(k+1)))
        sum = sum + (-1)**(k) * combination * f(x+(n-2*k)*h)
    1 continue
    resultado = sum / ((2*h)**n)
    return
end subroutine

