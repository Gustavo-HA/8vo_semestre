      PROGRAM ecuadratica

! Declaraciones
      REAL*8 :: a,b,c ! coeficientes de la ecuacion
      REAL*8 :: discr ! discriminante de la ecuacion
      REAL*8 :: x1,x2 ! variables para soluciones
      REAL*8 :: term, den

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
      discr =dble( b**2 - 4.0*a*c  )
       WRITE(*,*) "discriminante= ", discr
      den = dble(2.0*a)
      term = dble(SQRT(ABS(discr)))
      IF (discr .GT. 0 ) THEN
      WRITE(*,*) "dos soluciones"
      x1 = dble((-b+term)/den  )
      x2 = dble((-b-term)/den )
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
      
      STOP
      END

