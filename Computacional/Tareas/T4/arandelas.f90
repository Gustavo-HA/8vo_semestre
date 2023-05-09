program disco
    implicit none 
    real*8 :: f, pi = 4.d0*atan(1.d0), I=0, xi, xf, dx,x2
    integer :: n, j

    write(*,'(a)',advance='no')'Limite inferior y superior del intervalo: ';read*,xi,xf
    write(*,'(a)',advance='no')'Anchura de las arandelas: ';read*,dx
    
    n = int(abs(xf-xi)/dx)

    do j = 1, n
        x2 = xi + j*dx
        I = I + (f(x2)+f(x2-dx))*dx/2.d0  
    enddo
    I = I*pi
    write(*,'(a,f20.16)')'El volumen del solido de revolucion es: ',I
end

real*8 function f(x)
    real*8 :: x
    f = 1/x
    g = 0
    if (f.ge.g) f = f**2 - g**2
    if (f.lt.g) f = g**2 - g**2
    return
end
