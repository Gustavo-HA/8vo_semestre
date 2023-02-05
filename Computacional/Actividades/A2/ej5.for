       program biseccion
        
       write(*,*) 'Dame los extremos del intervalo:'
       read*,a,c
        dl = 1.e-6
       dx = c-a !tamaño del intervalo
       istep = 0 !contador

       !Se evalua la funcion en los extremos del intervalo
       write(*,20)f(a)
       write(6,20)f(c)
  20   format ('f(',f16.3,') = ',f16.8)

       do 100 while (abs(dx).gt.dl) !define cuando parar
        b = (a+c)/2. !se define b
        if ((f(a)*f(b)).lt.0.0) then
        c = b
        dx = c-a
       else
        a=b
        dx = c-a
       end if 
       istep = istep +1
100    end do 
       
       write (*,*) 'Iteracion, Raiz, Tolerancia'
       write (6,999) istep,b,dx
       
       
       stop
999    format(i4,2f16.8)
        end

       function f(x)
        f = 10**x-25*x+14 
        return
        end



       