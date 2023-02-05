          PROGRAM modfor
         parameter (n=1000)
         dimension A(n,n)
         integer i,j

c al usar la funcion mod(i,3), me da 0 para los multiplos de 3
c La funcion intrinseca modulo se define como:
c  MOD(x,y)
c  y cacula remainder x - INT(x/y)*y
c  Si se da como argumento reales da como salida reales
c si se usa enteros, da enteros.
c Ej.
c Mod(1,1)= 0
c Mod(2,1) da 2-int(2/1)*1=0
c Mod(1,2)  1-int(1/2)*2=1

	do 10  j=1,11
        do 20  i=1,11
		A(i,j)=mod(i,j)
		WRITE(*,30)i,j,A(i,j)
   20 continue
   10 continue

   30    FORMAT(1X,'Mod(',i3,',',i3,')=',f10.6)
		END
