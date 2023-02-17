program error
    R = 0.00001
    ! Se pierden digitos significativos al sumar a un número grande un pequeño
    print*,f(R)
end

function f(x)
    f = (1-cos(x))/(x**2)
    return
end