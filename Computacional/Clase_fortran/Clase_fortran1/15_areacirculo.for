! Este programa lee un numero real r y muestra
!el area del c�rculo con radio r.
      real*8 :: pi = 3.1415926535897, r, area 
      write (*,*) 'Escribe el radio r:'
      read  (*,*) r
      area = pi*r*r
      write (*,*) 'Area = ', area

      stop
      end
