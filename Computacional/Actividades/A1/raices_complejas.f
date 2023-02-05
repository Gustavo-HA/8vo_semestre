       PROGRAM RAICES
       !Este programa calcula las raices de una funci¢n cuadr tica
       
       REAL a, b, c, discr, x1, x2, parte_real, parte_imag
       
       WRITE(*,*) 'Ingresa el coeficiente cuadr tico'
       READ(*,*) a
       
       WRITE(*,*) 'Ingresa el coeficiente lineal'
       READ(*,*) b
       
       WRITE(*,*) 'Ingresa el coeficiente independiente'
       READ(*,*) c
       
       !Se calcula el discriminante
       discr= b**2-4.*a*c
       
       WRITE(*,*) 'El discriminante es:', discr
       
       !Resolvemos las ra¡ces dependiendo del valor del discriminate
       
       IF (discr > 0) THEN
       
       x1 = ( -b + sqrt(discr) ) / (2*a)
       x2=( -b - sqrt(discr) ) / (2*a)
       
       WRITE(*,*) 'Esta ecuaci¢n tiene dos ra¡ces reales y diferentes'
       WRITE(*,*)  'x1=', x1
       WRITE(*,*) 'x2=', x2
       
       ELSE IF (discr == 0.) THEN
       
       x1=(-b) / (2*a)
       
       WRITE(*,*) 'Esta ecuaci¢n tiene una sola ra¡z real'
       WRITE(*,*) 'x1=x2=', x1
       
       ELSE
       !El discriminante es negativo
       
       parte_real = ( -b ) / (2.*a)
       parte_imag = sqrt ( abs (discr) ) / (2.*a)
       
       WRITE(*,*) 'Tiene dos soluciones diferentes y complejas'
       WRITE(*,*) 'x1=', parte_real, '+i', parte_imag
       WRITE(*,*) 'x2=', parte_real, '-i', parte_imag
       
       END IF
       
       PAUSE
       END PROGRAM
