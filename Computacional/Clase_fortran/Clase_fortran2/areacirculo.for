! Este programa lee un numero real r y muestra
!el area del círculo con radio r.
      pi= 3.14159
      write (*,*) 'Escribe el radio r:'
      read  (*,*) r
      area = pi*r*r
      write (*,*) 'Area = ', area
      pause
      stop
      end
