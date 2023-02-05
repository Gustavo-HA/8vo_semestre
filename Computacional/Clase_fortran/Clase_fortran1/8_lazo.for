      PROGRAM GOTOLP
! Calcula los cuadrados de los numeros
! comprendidos entre 1 y 100
!  Imprime a un archivo de salida

       OPEN( 9, FILE='salida.txt' )

	N=0
   55   N=N+1
	IF (N .GT. 100) GOTO 99
c_ goto indica que se dirige hacia la linea que contiene a la etiqueta
	M=N**2
	WRITE(9,33)N,M
	GOTO 55
   99    STOP
   33    FORMAT(11X,'N=',I3,'  M=',I6)
   




       PAUSE
       STOP
	END
