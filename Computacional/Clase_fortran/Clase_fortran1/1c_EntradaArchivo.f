      PROGRAM EntradaArchivo
      INTEGER H, J
     
c  III. Leemos de un archivo de entrada e imprimimos a un archivo de salida.
      Write (*,'(a)') ' '
      Write (*,'(a)') '************************* '
      Write (*,'(a)') 'Archivo que contiene un par de numeros'
      Write (*,'(a)') 'separados por comas'
      Write (*,'(a)') 'Llamado input.txt '
      Write (*,'(a)') ' '
       
c En este caso lo llamo input.txt


      OPEN( 8, FILE='input.txt' )
      OPEN( 10, FILE='output.txt' )

!--------------------------------------------------------------
      Read(8,*)H,J
      Write(10,*)H,J

      Write (*,'(a)') ' '
      Write (*,'(a)') 'Se tiene la salida en output.txt'
      PAUSE
      STOP
      END
