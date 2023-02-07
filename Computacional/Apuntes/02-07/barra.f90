program maxmin
    real*8 :: dl,dx, a,b,x0,der2
    integer :: istep
    DL = 1.0E-07
    WRITE (*,*)'Dame los extremos del intervalo que contienen el maximo o minimo:'
    READ(*,*)A,B
    DX = (B-A)/10.
    X0 = (A+B)/2.0
    CALL SECANT (DL,X0,DX,ISTEP)
    if (der2(x0).gt.0) then
        print*,'Se encontro un minimo en x = ',x0
    else
        print*,'Se encontro un maximo en x = ',x0
    endif
    STOP
end

SUBROUTINE SECANT (DL,X0,DX,ISTEP)
    real*8 :: dl,x0,dx, x1,x2,d,der1
    integer :: ISTEP
    ISTEP = 0
    X1 = X0 + DX
    DO    100  WHILE (ABS(DX).GT.DL)
      D  = der1(X1) - der1(X0)
      X2 = X1 - der1(X1)*(X1-X0)/D
      X0 = X1
      X1 = X2
      DX = X1 - X0
      ISTEP = ISTEP + 1
 100 END DO
    RETURN
end

real*8 function f(x)
    real*8 :: acua,atri, x
    f = acua(x)+atri(x)
    return
end

real*8 function der1(x)
    real*8 :: x,h=1.0e-7,f
    der1 = (f(x+h)-f(x-h))/(2*h)
    return
end

real*8 function der2(x)
    real*8 :: x,h=1.0e-7,f
    der2 = (f(x+2*h)-2*f(x)+f(x-2*h))/(4*h**2)
    RETURN
END

real*8 function acua(c)
    real*8 :: c
    acua = (c/4)**2
    return
end

real*8 function atri(c)
    real*8 :: c
    real*8 :: pi = 4.d0*atan(1.d0)
    atri = ((2-c)/3)**2*sin(pi/3)/2
    return
end