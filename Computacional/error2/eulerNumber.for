      PROGRAM EulerNumber

c Aproxima el número de Euler (e)
c e es irracional y su valor aproximado es:
c 2.7182818284590452353....
c Puede ser aproximado por la formula
c e = 1 + 1/1! + 1/2! + ... + 1/(n-1)! + 1/n!
c
c Pag. 5 Atkinson-Kendall

      IMPLICIT NONE
c Se inicializan valores de variables y tipos
        INTEGER :: n,k            !Numero de entrada del usuario
        INTEGER :: factor = 1  !factor en el denominador
        REAL*8 :: estimate = 1.0      !Estimacion de e
        REAL*8 e,errorRP, eraprox
        INTEGER :: l          !contador del lazo
        e=2.7182818284590452353
c Se obtiene un valor de entrada

        WRITE(*,*) "Entra el numero de terminos a calcular: "
        READ(*,*) n
c Calculo del error relativo porcentual
c error%= (VT-VA)/VT)*100%
c errorAproximacion= VT-VA
c donde VT es el valor verdadero
c       VA es valor aproximado

c Se hace un lazo DO

        DO k = 1, n, 1
                factor = factor*k
                estimate = estimate + 1.0/REAL(factor)
                eraprox=e-estimate
                errorRP= (e-estimate)/e*100.0
        END DO
        WRITE(*,*) " e es: ", e
        WRITE(*,*) "La estimacion de e es: ", estimate
        WRITE(*,*) "Error de aproximacion:", eraprox
        WRITE(*,*) "Error relativo porcentual: ", errorRP
        PAUSE
        STOP
        END 
