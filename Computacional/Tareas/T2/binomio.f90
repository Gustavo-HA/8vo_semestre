program t1_2
  implicit none
  integer :: x , combi1
  integer*4 :: combi2
  real :: y
  print*, 'Valor de n : ' 
  read*, y 
  if (modulo(y,1.0) .eq. 0) then 
    select case (int(y))
      case (1 : )
        print*, 'Entero positivo'
        do x = 0 , int(y)
          print*, 'Combinacion', x, combi1(int(y), x)
        end do               
      case ( : -1)
        print*, 'Entero negativo'
        do x = 0 , int(-y)
          print*, 'Combinacion', x,  combi2(x, int(-y))
        end do 
      end select
  else 
    select case (int(y))
      case (1 : )
        print*, 'Real positivo'
        call combi3(y)
      case ( : -1)
        print*, 'Real negativo'
        call combi3(y)
    end select
  end if 
end program t1_2


function factorial (n)    ! no mas grande que 12 
  implicit none
  integer, intent (in) :: n
  integer :: factorial
  integer :: j
  factorial = product ((/(j, j = 1, n)/))
  return
end function factorial

function combi1 (n, k) result (res)
  implicit none
  integer, intent (in) :: n
  integer, intent (in) :: k
  integer :: res, factorial
  res = factorial (n) / (factorial (k) * factorial (n - k))
end function combi1

integer*4 function combi2(r, n) 
  implicit none 
  integer*4, intent(in):: r, n 
  integer*4 i, j/1/ 
  do i = max(n-r, r) + 1, n 
    j = j * i 
  enddo 
  do i = min(n - r, r), 2, -1 
    j = j / i 
  end do 
  combi2 = j 
  return 
end function combi2


subroutine combi3(r)
  real, intent(in) :: r
  real :: nume
  integer :: limsup, n, factorial
  print*, 'Orden de la expansion (0,1,...,inf): '
  read*,  limsup
  n=0
  do while (n.le.limsup)
    nume=1.0
    if (n==0) then
      combireal = 1
    else
      a = n-r+1
      do while (a.le.r)
        nume = nume*a
        a = a + 1
      enddo
      combireal = nume/(1.0*factorial(n))
    end if  
    write(*,*)'Combinacion', n , combireal
    n = n+1
  enddo
  return
end 