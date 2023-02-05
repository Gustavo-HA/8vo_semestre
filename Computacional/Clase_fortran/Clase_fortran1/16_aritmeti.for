        program operac
        implicit none
        real num1,num2,resul,oper
        print *,'Introduzca los numeros:'
        read *,num1,num2
        print *,'______MENU______'
	print *,'1-Sumar.'
	print *,'2-Restar.'
	print *,'3-Multiplicar.'
	print *,'4-Dividir.'
1	print *,'Escoja una operacion.'
	read *,oper

	if (oper .eq. 1) then
		resul=num1+num2
	else if (oper .eq. 2) then
		resul=num1-num2
	else if (oper .eq. 3) then
		resul=num1*num2
	else if (oper .eq. 4) then
		resul=num1/num2
	else
		print *,'N�mero incorrecto.'
		goto 1
        end if
	print *,'El resultado es ',resul
	
        
        stop
        end
