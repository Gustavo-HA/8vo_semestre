      program Convert
      implicit none
! -----------------------------------------------Declare
      real*4 tempC, tempF, FACTOR
      integer*2 ZERO_SHIFT
      parameter (ZERO_SHIFT = 32, FACTOR = 5./9.)
! -----------------------------------------------Input
      print*, "Entra la temperatura en Fahrenheit ..."
      read*, tempF
! -----------------------------------------------Compute
      tempC = FACTOR * (tempF - ZERO_SHIFT)
! -----------------------------------------------Output
      print*, "La temperatura en Centigrados es: "
      print*, tempC, " grados"
      end