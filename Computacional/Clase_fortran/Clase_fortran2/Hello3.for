       program hello_world3
       implicit none
       character*32 text
c Se pasa una variable a una 
c subrutina:
       text = 'Hello World'
c
       call printtext(text)

       end
c
c Subrutina:
       subroutine printtext(tekst)
       implicit none
       character*32 tekst
c
       write (*,*) tekst
c
       end