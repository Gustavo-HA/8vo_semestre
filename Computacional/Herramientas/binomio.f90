program t1_2
  implicit none
  integer :: x , combi1
  real :: y, punto=0.5
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
        call combi3(y,punto)
      end select
  else 
    print*,'Real'
    call combi3(y,punto)
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

subroutine combi3(r,y)
  real, intent(in) :: r
  real :: nume
  integer :: limsup, k, factorial
  print*, 'Orden de la expansion (0,1,...,inf): '
  read*,  limsup
  k=0
  do while (k.le.limsup)
    nume=1.0
    if (k==0) then
      combireal = 1
    else
      a = r
      do while (a.ge.(r-k+1))
        nume = nume*a
        a = a - 1
      enddo
      combireal = nume/(1.0*factorial(k))
    end if  
    write(*,*)'Termino', k , combireal, 'x^',k,y**k
    k = k+1
  enddo
  return
end 