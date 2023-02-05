c Muestra formato de salida        
       program fmtio

       character chs*7
       
       write (*,*)'  '
       write (*,*)'Numero inicial: 1234.56'
       write (*,*)'  '
       write (*,*)'Escrito como F6.2:'
       write (*,FMT='(1X,F6.2)') 1234.56
        write (*,*)'  '
       write (*,*)'Como F7.2:'
       write (*,FMT='(1X,F7.2)') 1234.56
        write (*,*)'  '
       write (*,*)'Como F8.2:'
       write (*,FMT='(1x,F8.2)') 1234.56
        write (*,*)'  '
       write (*,*)'Como I6.6:'
       write (*,FMT='(1x,I6.6)') 1234
        write (*,*)'  '
       write (*,*)'Como I6.5:'
       write (*,FMT='(1x,I6.5)') 1234
        write (*,*)'  '
       write (*,*)'Como I6:'
       write (*,FMT='(1x,I6)')   1234
        write (*,*)'  '

C      'T' alinea si no se sabe el largo de la cadena
       write (*,*)'/////////////////////////////////////////////  '
       write (*,*)'Alineamos dos valores:'
       write (*,FMT='(1x,I5,T20,I6.6)') 1234,   1234
       write (*,*)'  '
       write (*,FMT='(1x,I6,T20,I6.6)') 123456, 1234

C      'TR' no es lo mismo que  'T'
       write (*,*)'  '
       write (*,*)'****   Uso de TR *******  '
       write (*,*)'  '
       write (*,FMT='(1x,I5,TR20,I6.6)') 1234,   1234
       write (*,*)'  '
       write (*,FMT='(1x,I6,TR20,I6.6)') 123456, 1234
       write (*,*)'  '
      

C      'nX' es igual que 'TRn'
       write (*,*)'  '
       write (*,*)'****   Uso de TRn *******  '
       write (*,FMT='(1x,I5,20X,I6.6)') 1234,   12344
       write (*,*)'  '
       write (*,FMT='(1x,I6,20X,I6.6)') 123456, 1234
       write (*,*)'  '

C      Se puede usar la cadena para definir el formato
C      Como la cadena tiene tres digitos, al usar I6 me imprime
c      3 blancos iniciales
       write (*,*)'  '
       chs = '(1x,I6)'
       write(*,FMT=chs) 123


C Notas:
C Integer                             rIw, rIw.m
C real, double precision, or comlex   rEw.d, rEw.dEe, rFw.d, rGw.d, rGw.dEe
C Logicla                             rLw
C Character                           rA, rAw
C
C w is the total field width.
C m is the minimum number of digits produced on output.
C d is the number of digits after the decimal point.
C e is the number of digits used for the exponent.
C r is a repeat (handy for arrays)
C Tn -- move to column n
C TRn -- Shift to the right by n columns
C TLn -- Shift to the left by n columns
C nX  -- Same as TRn
C SP  -- after all positive numbers have a +
C SS  --  after the + is suppresed
        PAUSE
        STOP

       end
