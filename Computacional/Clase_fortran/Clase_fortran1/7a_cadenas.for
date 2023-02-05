       program cadenas

C      cadenas de longitud 10
       character chs1*10
       character chs2*10

C    un arreglo of cadenas
       character charr(10)*22

C      cadenas
       chs1     = '0123456789'
       charr(1) = 'firstOne'
       charr(2) = 'secondOne'

C      Se indica una subcadena
       chs1(2:4) = 'BCD'

C      Se adicionan blancos a la derecha
c para ajustar el largo de la cadena
       chs2 = '123456789'
c0000000000000000000000000000000000000000000
       write (*,*) ' '
       write (*,*) 'Cadenas iniciales:'
       write (*,*) 'Cadena uno=',chs1
       write (*,*) 'Cadena dos=',charr(1)
       write (*,*) 'Cadena tres=', charr(2)
       write (*,*) 'Cadena cuatro=', chs1(2:4)
       write (*,*) 'Cadena cinco=',chs2 
       write (*,*) ' '
       

c
c    Se agrega x al inicio y final de chs1 y chs2
      write (*,*) 'Cadena uno con x al inicio y final '
      write (*,*) 'x', chs1, 'x'

c Como chs2 tiene 9 digitos, se imprime un blanco
c a la derecha
       write (*,*) ' '
       write (*,*) 'Cadena cinco con x inicial y final'
       write (*,*) 'x', chs2, 'x'
       write (*,*) ' '
C      FORTRAN sigue la siguiente notacion para cadenas
       write (*,*) 'Cadena cuatro con x inicial y final'
       write (*,*) 'x', chs1(2:4), 'x'
       write (*,*) ' '
       write (*,*) 'Subcadena uno con x inicial y final'
       write (*,*) 'x', chs1(:4),  'x'
       write (*,*) ' '
       write (*,*) 'Subcadena uno con x inicial y final'
       write (*,*) 'x', chs1(2:),  'x'
       write (*,*) ' '

C      Este es la subcadea comprendida entre segundo y cuarto
c caracter
       write (*,*) 'Subcadena de dos con x al inicio y final '
       write (*,*) 'x', charr(1)(2:4), 'x'
       write (*,*) ' '
C       // indica concatenacion de cadenas
       write (*,*) 'Concatenando cadena cinco con cadena uno'
       write (*,*) 'x', (chs2 // chs1), 'x'
       write (*,*) ' '
C      Comparacion de cadenas 
       write (*,*) 'Comparando cadenas'
       if ( .not. ('abc' .eq. 'ABC')) then
          write (*,*) 'abc .eq. ABC is FALSE'
       end if

C      FORTRAN especifica que las letras estan antes que los numero
c       A<B<..., 0<1<2..., 
c      y el  espacio esta antes que todo
C      
       if ('DEF' .gt. 'ABC') then
          write (*,*) 'DEF .gt. ABC is TRUE'
       end if
       
C      FORTRAN 77 tiene 4 funciones para determinar el orden de las cadenas
c      LGE, LGT, LLE, LLT.
       if (LGE('abc', 'ABC')) then
          write (*,*) 'LGE(abc, ABC) is TRUE'
       end if

C      ICHAR convierte el primer caracter de una cadena
c      en un numero 
       write (*,*) ' '
        write (*,*) 'Convierte caracter de la cadena a numero'
       write (*,*) 'ichar(a)=', ichar('a'), ' ichar(A)=', ichar('B')
       write (*,*) ' '
C      CHAR convierte un entero en una cadena
       write (*,*) 'Convierte numero a cadena'
       write (*,*) 'char(97)=', char(97), ' char(66)=', char(66)

C      Se busca con indice
       write (*,*) ' '
        write (*,*) 'Busqueda en la cadena '
       write (*,*) 'bar is at ', index('foobar', 'bar'), ' in foobar'

c La longitud de una cadena se encuentra con LEN
c se incluyen los balncos
       write (*,*) ' '
       write (*,*) 'Se da la longitud de las cadenas '
       write (*,*) 'Longitud de la cadena Uno=', len(chs1)
       write (*,*) 'Longitud de la cadena cinco=', len(chs2)

C      Se pasa una cadena a una subrutina
        write (*,*) ' '
        call excsub(chs1)

       PAUSE
       STOP
       end

c********************


      subroutine excsub(c)
c     Se declara una cadena sin especificar el tama¤o
c explicitamente

        character*(*) c
        write (*,*) 'En la subrutina excsub(): x', c, 'x'
        return
      end
