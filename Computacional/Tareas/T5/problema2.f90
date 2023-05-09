program vector
    implicit none
    real*16 :: x, y, magnitud1, magnitud2
    write(*,'(a)')'Componente x: ';read*,x
    write(*,'(a)')'Componente y: ';read*,y
    magnitud1 = sqrt(x**2 + y**2)
    magnitud2 = sqrt((x+y)**2-2*x*y)
    write(*,'(a)')'La magnitud del vector convencional es: ';write(*,'(f64.33)')magnitud1
    write(*,'(a)')'La magnitud del vector modificado es: ';write(*,'(f64.33)')magnitud2
end program