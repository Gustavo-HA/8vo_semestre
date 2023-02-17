      PROGRAM infinito
c Este programa demuestra que al no ser representado
c de manera correcta 0.1 en una maquina,
c el numero 1.0 nunca se obtiene.
c Esto produce un ciclo infinito.

      X= 2.0
      
   10 X= X-0.1
      PRINT *, X, SQRT(X)
      if (X. NE. 1.0) GO TO 10
    
      STOP
      END
 
