program problema3
    implicit none
    real*8 :: epsilon = 1.084e-19, resultado,sum, error_rel
    integer :: i, k
    open(10, file='p3data.dat')
    do k = 1, 1000
        sum=0
        do i = 1, k
            sum = sum + 0.1
        end do
        resultado = k/10.0 - sum
        error_rel = abs(resultado-epsilon)/epsilon
        write(10,*)k,error_rel,resultado
    end do
    close(10)
end program