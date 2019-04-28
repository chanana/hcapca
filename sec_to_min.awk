awk -v c=60 '{for (i=1; i <= NF; ++i) $i /= c } 1' CONVFMT='%.3g' data/Variables_t.dat > V_t.dat
