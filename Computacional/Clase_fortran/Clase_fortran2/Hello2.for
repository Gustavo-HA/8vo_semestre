       program hello_world2
       implicit none
c Hace uso de la subrutina Hello
       write (*,*) 'Hace uso de una subrutina'
       call hello
       call hello

       end

c Es el programa Hello.for
       subroutine hello
       implicit none
       character*32 text
c
c       write (*,*) 'Hace uso de una subrutina'
       write (*,*) ' '
       text = 'Hello World'
       write (*,*) text
c
       end