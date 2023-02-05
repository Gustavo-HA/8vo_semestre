program derivar
    real*8 :: der,dx,a,b
    print*,'Intervalo donde se va a graficar:';read*,a,b
    print*,'Diferencial dx: ';read*,dx
    
    do while(a<b)
        if (der(a) .gt. -1.0e-6 .and. der(a).lt. 1.0e-6) then
            write(*,*)'Se encontro un maximo o minimo alrededor de:',a
        end if
    a = a+dx
    enddo
    
    stop
end

function f(x)
    real*8 :: x,f
    f = -0.5*x**4+4*x**3-10*x**2+8.5*x+1
    return
end

function der(x)
    real*8 :: x,der,f,h
    h = 1.0e-9
    der = (f(x+h)-f(x-h))/(2*h)
    return
end