      program Convert
      implicit none
! -----------------------------------------------Declare
      real*4 tempC, tempF, FACTOR !precision simple real*4
      integer*2 ZERO_SHIFT 
      parameter (ZERO_SHIFT = 32, FACTOR = 5./9.) ! Constante, no va a cambiar durante todo el programa
! -----------------------------------------------Input
      print*, "Entra la temperatura en Fahrenheit ..." !print es similar a write
      read*, tempF
! -----------------------------------------------Compute
      tempC = FACTOR * (tempF - ZERO_SHIFT)
! -----------------------------------------------Output
      print*, "La temperatura en Centigrados es: "
      print*, tempC, " grados"
      
      
      STOP
      end
