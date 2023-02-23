PROGRAM EulerConvergencia
   IMPLICIT NONE
   INTEGER :: n,k,j           
   real*8 :: factor = 1   ! Factorial
   REAL*8 :: estimate = 0.d0    ! Estimación inicial de e
   character (len = 20) :: archivo = 'resultado.txt' ! Nombre del archivo donde se depositarán los datos
   character(105) :: factorial ! Factorial string     <
   character(5) :: potencia ! Potencia string         <  Estas tres variables son para expresar el polinomio
   character (10000):: polinomio ! Polinomio string   <  como una cadena

   WRITE(*,*) "Orden de la aproximacion por Taylor: "
   READ(*,*) n
   open(10,file=archivo)
   write(10,'(a,10x,a)')'#Grado del polinomio','Polinomio Taylor'

   DO k = 0, n, 1
        factor = gamma(real(k+1))
        estimate = estimate + 1.d0/factor
        do j = 0, k
                write(factorial,'(f100.0)')gamma(real(j+1))
                write(potencia,'(i5)')j
                if (j==0) then
                        polinomio = trim('1')
                else
                        polinomio = trim(polinomio)//trim('+')//trim('1/')//trim(factorial)//trim('*x**')//trim(potencia)
                endif
        enddo
        call quitaespacio(polinomio)
        write(10,'(8x,i2,10x,a)')k,trim(polinomio)
   END DO
   close(10)

   ! Graficamos
        call execute_command_line('convergencia-10.gpl')
        call execute_command_line('convergencia4.gpl')
        call execute_command_line('convergencia10.gpl')
   STOP
   END 

subroutine quitaespacio(string)
        character(len=*) :: string
        integer :: longitudstring 
        integer :: ultima, actual  
        longitudstring = len (string)
        ultima = 1
        actual = 1

        do while (actual < longitudstring)
            if (string(ultima:ultima) == ' ') then
                actual = actual + 1
                string(ultima:ultima) = string(actual:actual)
                string(actual:actual) = ' '
            else
                ultima = ultima + 1
                if (actual < ultima) &
                    actual = ultima
            endif
        end do
end subroutine