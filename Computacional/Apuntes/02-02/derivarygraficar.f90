program derivar
    real*8 :: der,dx,a,b,f
    open(9,file = 'output.dat')
    print*,'Intervalo donde se va a graficar:';read*,a,b
    print*,'Diferencial dx: ';read*,dx
    
    do while(a<b)
        write(9,'(f16.8,2x,f16.8,2x,f16.8)')a,f(a),der(a)
        a = a+dx
    enddo

    call system("derivada.gpl")


    stop
end

function f(x)
    real*8 :: x,f
    f = log(x)
    return
end

function der(x)
    real*8 :: x,der,f,h
    h = 1.0e-9
    der = (f(x+h)-f(x-h))/(2*h)
    return
end