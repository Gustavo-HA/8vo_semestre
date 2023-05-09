set termoption dashed
datafile = 'triag_0.dat'
stats datafile nooutput
plot for [IDX=0:STATS_blocks-1] \
    datafile \
    index IDX \
    using 1:2 \
    w lp pt '*' lc rgb 'dark-blue' \
    notitle
#pause 5 