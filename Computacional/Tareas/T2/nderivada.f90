program nderivada
    implicit none
    real*8 :: der, x
    integer :: N,i

    print*,'Orden de la derivada:' ; read*,N
    print*,'En el punto:' ; read*,x

    do 1 i=1,N
        write(*,10)i,x,der(i,x)
    1 continue
    10 format('La derivada de ',i2,' grado de f, evaluada en ',f9.4,' es igual a: ',f20.8)
end


real*8 function f(x)
real*8 :: x
f = exp(x)
return
end

REAL*8 FUNCTION DER(n,x)
    INTEGER :: n
    REAL*8 :: x, h = 1.0e-6, F, DDER
    IF (N.GE.2) THEN
    DER = (DdER(N-1,X+H)-DdER(N-1,X-H))/(2*h)
    RETURN
    ELSE IF (N .EQ. 1) THEN
    DER = (F(X+H)-F(X-H))/(2*H)
    RETURN
    ELSE
    PRINT*,"ERROR, N DEBE SER MAYOR O IGUAL A 1"
    END IF
END

REAL*8 FUNCTION DDER(n,x)
    INTEGER :: n
    REAL*8 :: x, h = 1.0e-6, F, DER
    IF (N.Gt.1) THEN
    DDER = (DER(N-1,X+H)-DER(N-1,X-H))/(2*h)
    RETURN
    ELSEIF (n.EQ.1) THEN
    DDER = (F(X+H)-F(X-H))/(2*H)
    RETURN
    ELSE
    PRINT*,"ERROR, N DEBE SER MAYOR O IGUAL A 1"
    END IF
END
