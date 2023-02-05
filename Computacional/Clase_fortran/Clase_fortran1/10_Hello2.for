       program hello_world2
       
c Hace uso de la subrutina Hello
       write (*,*) 'Hace uso de una subrutina'
       call hello
       call hello
       call hello

       STOP
       end

c Es el programa Hello.for
       subroutine hello
c
c      write (*,*) 'Hace uso de una subrutina'
       write (*,*) 'Hello World'
c El stop hace que la subrutina no se pueda correr otra vez
c      PAUSE
c       STOP
       end
