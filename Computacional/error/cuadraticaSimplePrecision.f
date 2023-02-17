      PROGRAM ecuadratica

! Declaraciones
      REAL :: a,b,c ! coeficientes de la ecuacion
      REAL :: discr ! discriminante de la ecuacion
      REAL :: x1,x2 ! variables para soluciones
      REAL :: term, den

! Entrada de datos

100   WRITE(*,*) "Ingrese coeficientes a,b,c, separados por comas"
      READ(*,*) a,b,c
! discriminante igual a cero
      IF( a == 0 ) THEN
      WRITE(*,*) "La ecuacion no tiene termino cuadratico"

      WRITE(*,*) "Ingrese 'a' diferente de cero"
      goto 100
      ENDIF
! Proceso
      discr =b**2 - 4.0*a*c
      WRITE(*,*) "discriminante= ", discr
      den = 2.0*a
      term = SQRT(ABS(discr))
      IF (discr .GT. 0 ) THEN
      WRITE(*,*) "dos soluciones"
      x1 = (-b+term)/den
      x2 = (-b-term)/den
      WRITE(*,10)x1
   10 format ("x1 = ", F27.21)
      WRITE(*,20)x2
   20 format ("x2 = ", F27.21)
      ELSE
      IF (discr == 0 ) THEN
      x1 = -b/den
      WRITE(*,*) "solucion unica"
      WRITE(*,*) "x1 = ", x1
      ELSE
      WRITE(*,*) "soluciones complejas"
      x1 = -b/den
      x2 = term
      WRITE(*,*) "x1 = (", x1, " ,", x2, ")"
      WRITE(*,*) "x2 = (", x1, " ,", -x2, ")"
      ENDIF
      endif

! Terminar
      pause
      STOP
      END

