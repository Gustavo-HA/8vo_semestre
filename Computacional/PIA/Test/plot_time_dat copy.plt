set terminal gif animate delay 100 size 1300,600
set palette rgbformulae 22,13,-31
set output "time_bump.gif"
set key below 
set pm3d map
set grid 

# set xrange [0:4]
set xtics 0.5

# set yrange [0:7]
set ytics 0.5

set cbrange [0.0:0.60]

set xlabel " x "
set ylabel " h "
set title " Solucion Navier - Stokes "



do for [i=1:7] {
  
   plot sprintf('time%d.dat', i) u 1:2:(0.35*$3):(0.35*$4):(sqrt($3**2+$4**2)) \
   w vec lw 1.7 lc palette title sprintf('tiempo %d', i)

 }



