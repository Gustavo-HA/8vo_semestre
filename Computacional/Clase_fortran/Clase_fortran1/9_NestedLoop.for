      PROGRAM nested
! Demuestra como se obtiene
! que los 2 indices de una variable
! cambien

c---- FORMA DE COMENTAR -----
c identifica tabla multiplicar
! identifica A(i,j) elementos

      IMPLICIT NONE

      INTEGER :: i    !indice del lazo externo
      INTEGER :: j    !indice del lazo interno

      DO i = 1, 3, 1
         DO j = 1, 10, 1

c Util para tablas de multiplicar
c             WRITE(*,100) I,J, I*J

! Util para ver como cambian los elementos
! de la matriz A(i,j)
             WRITE(*,100) I,J
             
         END DO
      END DO

c  100 format(i3," * ",2x,i3," = ",i3 )

! Como elementos de una matriz
  100 format(" A( "i3,2x,i3,")")

      
      STOP
      END
