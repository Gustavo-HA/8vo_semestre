program problema5
implicit none
    real*8 :: lambda, der
    write(*,'(a)',advance='no')'Longitud de onda a evaluar: ';read*,lambda

    if (der(lambda).gt.0.) write(*,*)'La velocidad aumenta'
    if (der(lambda).lt.0.) write(*,*)'La velocidad disminuye'
    if (der(lambda).eq.0.) write(*,*)'La velocidad se mantiene constante'

end program


real*8 function v2(la)
    real*8 :: g = 9.81, pi = 4.d0*atan(1.d0), h = 50, la
    v2 = (g*la)/(2*pi) * tanh(2*pi*h/la)
    return
end function

real*8 function der(la)
    real*8 :: v2, la, k = 1.0e-7
    der = (v2(la+k)-v2(la-k))/(2.0*k)
    return
end function
