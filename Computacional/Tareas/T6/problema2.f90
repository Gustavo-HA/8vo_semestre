program problema2
    real*16 :: R(20,20), A, b, RS(20,20)
    integer :: n=20, k, m

    ! Menú 
    write(*,*)'Listado de funciones:'
    write(*,*)'1. exp(-x**3)'
    write(*,*)'2. cos(sqrt(x))'
    write(*,*)'3. sin(x)/x'
1    write(*,'(a)',advance='no')'Seleccion: ';read*,k
    if (k .lt. 1 .or. k .gt.3) then
        print*,'ERROR, seleccione una funcion entre 1 y 3.'
        goto 1
    end if 

    write(*,'(a)',advance='no')'Extremos del intervalo de integracion: ';read*,A,B
    write(*,'(a)',advance='no')'Orden de la expansion en series: ';read*, m

    call romberg(a,b,n,R,k)
    call RombergSerie(a,b,n,rs,k,m)
    write(*,'(a)',advance='no')'El valor de la integral mediante la funcion: ';print*,R(N,1)
    write(*,'(a)',advance='no')'El valor de la integral mediante la expansion: ';print*,RS(N,1)
    write(*,'(a)',advance='no')'Error absoluto: ';print*,abs(R(N,1)-RS(N,1))




end program

SUBROUTINE ROMBERG(A,B,N,R,l)
    REAL*16 ::R(20,20), f, S, A, B, H
    integer :: n 
    H=B-A
    R(1,1)=(H/2.0)*(F(A,l)+F(B,l))
    DO I=2,N
          S=0
          DO K=1,2**(I-2)
                S=S+F(A+((K-0.5)*H),l)
          END DO
          R(I,1)=0.5*(R(I-1,1) + (H*S))
          H=H/2.0
    END DO
    DO J=2,N
      DO I=J,N
          R(I,J)=R(I,J-1)+((R(I,J-1)-R(I-1,J-1))/(4**(J-1))-1)
      END DO
    END DO
END SUBROUTINE

SUBROUTINE RombergSerie(A,B,N,R,l,m)
    REAL*16 ::R(20,20), serie, S, A, B, H
    integer :: n , m
    H=B-A
    R(1,1)=(H/2.0)*(SERIE(A,l,m)+serie(B,l,m))
    DO I=2,N
          S=0
          DO K=1,2**(I-2)
                S=S+serie(A+((K-0.5)*H),l,m)
          END DO
          R(I,1)=0.5*(R(I-1,1) + (H*S)) 
          H=H/2.0
    END DO
    DO J=2,N
      DO I=J,N
          R(I,J)=R(I,J-1)+((R(I,J-1)-R(I-1,J-1))/(4**(J-1))-1)
      END DO
    END DO
END SUBROUTINE

real*16 function f(x,n)
    real*16 :: x
    integer :: n
    if (n.eq.1) f = exp(-x**3)
    if (n.eq.2) f = cos(sqrt(x))
    if (n.eq.3) f = sin(x)/x
    return
end function

real*16 function serie(x,k,n)
    real*16 :: x
    integer :: k , n
    serie = 0
    if (k.eq.1) then
        do i = 0, n
            serie = serie + (-1)**i *x**(3*i) / gamma(real(i+1)) 
        enddo
    else if (k.eq.2) then
        do i = 0, N
            serie = serie + (-1)**i * x**i / gamma(real(2*i + 1)) 
        enddo
    else
        do i = 0, N
            serie = serie + (-1)**i * x**(2*i) / gamma(real(2*i + 2))
        enddo
    end if
    return
end function