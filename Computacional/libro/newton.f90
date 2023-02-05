       program newton  
!        METODO DE NEWTON PARA HALLAR UNA RAIZ REAL DE UN POLINOMIO P(X) = 0
       
       dimension a(10)
       data a/4.,4.,1.,7*0./
       read(5,3)x
       
       do 2 j = 1,20
        b=a(6)
        c=a(6)
        do 1 i=1,4
            k=6-i
            b = a(k)+x*b
    1       c = b+x*c 
            b = a(1)+x*b
            write(6,5) j,x,b,c
            deltax = b/c
            if (abs(deltax).lt. 1.e-5 .and. abs(b) .lt. 11.e-5) stop
    2       x=x-deltax
            write(6,6)
            stop
    3   format(e20.8)
    5   format(i5,3(1pe17.7))
    6   format('Failed to converge in 20 iterations')
        end



