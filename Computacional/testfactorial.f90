program testfactorial
    implicit none
    integer, parameter :: qp = selected_int_kind(4)
    integer(qp) :: x, nder
end

recursive function factorial(n) result(fact)
   implicit none
   integer, parameter :: qp = selected_int_kind(2)
   integer(qp) :: n, fact, factorial
   if (n/=1.and.n/=0) then
   fact = factorial(n-1)*N
   return
   else
   fact = 1
    end if
   return
end