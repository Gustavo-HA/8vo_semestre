program error
    implicit none
    REAL*8 :: R, f
    ! Se pierden digitos significativos al sumar a un número grande un pequeño
    write(*,'(a)',advance='no')'Valor de x: ';read*,R
    print*,f(R)
end

real*8 function f(x)
    implicit none 
    real*8 :: x
    f = x-sin(x)
    !Se expande para que no aparezca el error cercano a 0
    return
end