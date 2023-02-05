      PROGRAM GOTOLP
! Calcula los cuadrados de los numeros
! comprendidos entre 1 y 100

	N=0
   55   N=N+1
	IF (N .GT. 100) GOTO 99
	M=N**2
	WRITE(6,33)N,M
	GOTO 55
   99    STOP
   33    FORMAT(11X,'N=',I3,'  M=',I6)
	END
