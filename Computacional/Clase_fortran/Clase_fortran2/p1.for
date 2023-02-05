!     Last change:  BB    8 Oct 2002    5:28 pm
      program p1_2
      implicit none
!
!   This program demonstrates several types of data:
!   integers, real numbers, characters, strings &
!   one dimensional arrays.
!
      integer :: i=7
      REAL :: x
      REAL, DIMENSION(2) :: b
      CHARACTER(1) :: c
      CHARACTER(10) :: yourname
      write(*,*)' '
      WRITE(*,*)'Entra un numero real '
      READ(*,*)x
      WRITE(*,*)'Un entero por un real da un real'
      WRITE(*,*)'Asi, x=',x,' veces i=',i,' da ',x*i
      WRITE(*,*)' '
      WRITE(*,*)'Entra tu inicial '
      READ(*,*) c
      WRITE(*,*)' '
      WRITE(*,*)'Entra tu nombre '
      READ(*,*)yourname
      WRITE(*,*)' '
      WRITE(*,*)'Tu inicial es ',c,' y tu nombre es ',yourname
      WRITE(*,*)' '
      WRITE(*,*)'Entra dos numeros separados por comas '
      READ(*,*) b
      WRITE(*,*)' '
      WRITE(*,*)'Los componentes del vector b son ',b
      WRITE(*,*)' '
      WRITE(*,*)'Entra algo:  presiona una tecla y despues enter '
      READ(*,*)c !This is a way of producing a pause during execution.'
      WRITE(*,*)' '
      WRITE(*,*)'Otra forma de escribir los componentes es:'
      WRITE(*,*)' '
      WRITE(*,*)(b(i),i=1,2)
      WRITE(*,*)' '
      WRITE(*,*)'Entra algo '
      READ(*,*)c
      WRITE(*,*)' '
      WRITE(*,*)'Usando un do-loop para escribir los componentes'
      WRITE(*,*)' '
      do i=1,2
      WRITE(*,*)b(i)
      end do
      WRITE(*,*)' '
      WRITE(*,*)'Ves la diferencia?'
      stop
      end program p1_2
