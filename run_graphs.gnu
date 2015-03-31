set terminal png
set grid
set key off

set xlabel 'Temp'
set ylabel 'Magnetization'
set title 'Magnetization'
set output 'MagVsT.png'
plot 'MagVsT.txt'

set xlabel 'Temp'
set ylabel 'Magnetic Susceptibility'
set title 'Magnetic Susceptibility'
set output 'chiVsT.png'
plot 'SuscVsT.txt'

set xlabel 'Temp'
set ylabel 'Internal Energy'
set title 'Internal Energy'
set output 'EVsT.png'
plot 'EnergyVsT.txt'

set xlabel 'Temp'
set ylabel 'Heat Capacity'
set title 'Heat Capacity'
set output 'C_vVsT.png'
plot 'C_vVsT.txt'

set xlabel 'log(Temp)'
set ylabel 'log(Magnetization)'
set title 'Critical Exponent (Beta)'
set output 'Beta.png'
plot 'beta.txt'

set xlabel 'log(Temp)'
set ylabel 'log(Magnetic Susceptibility)'
set title 'Critical Exponent (Gamma)'
set output 'Gamma.png'
plot 'gamma.txt'