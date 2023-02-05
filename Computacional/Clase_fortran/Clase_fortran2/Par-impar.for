        program Parimpar
        implicit none
        real num1
        print *,'dame un número par o impar'
        read *,num1
        if mod(num1,2).eq.1 then
	print *,'El número es par'
        else
        print *,'El número es impar'
        stop
        end
