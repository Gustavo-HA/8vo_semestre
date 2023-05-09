set termoption dashed
set size 1.4,0.6
datafile = 'triag_0.dat'
stats datafile nooutput
plot for [IDX=0:STATS_blocks-1] \
    datafile \
    index IDX \
    using 1:2 \
    w lp pt '*' lc 8 \
    notitle
#pause 5 