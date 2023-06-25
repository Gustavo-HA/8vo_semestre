set terminal gif animate delay 100 size 1300,600
set palette rgbformulae 22,13,-31
set output "vel_mag.gif"
set key below 
set view map
set dgrid3d

set xtics 0.5
set ytics 0.5

set xlabel " x "
set ylabel " h "
set title " Solucion Navier - Stokes "

# Tiene problemas ya que el mallado no es uniforme, pm3d funciona bien
# para mallados cuadrados uniformes.


do for [i=1:7] {
  
   splot sprintf('magvel_time%d.dat',i) u 1:2:3 with pm3d

 }



