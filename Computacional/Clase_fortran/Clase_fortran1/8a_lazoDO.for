       PROGRAM DLOOP1
       
	DO 89 N=1,100
	M=N**2
	WRITE(6,33)N,M
   89    CONTINUE
   33    FORMAT(11X,'N=',I3,'  M=',I6)
       PAUSE
       STOP
	END
