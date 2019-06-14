#!/bin/csh 
module load other/cdo
# input:
# fort.13 -> start date of forecast
# fort.12 -> 1 day before start date
# fort.11 -> 2 days before start date 

# output:
# fort.31 -> perturbation plus 
# fort.32 -> perturbation minus

set factor = 8.0

# U
cdo --silent -L replace fort.13 -add -selname,U fort.13 -divc,$factor -sub -selname,U fort.12 -selname,U fort.11 fort.1031
cdo --silent -L replace fort.13 -sub -selname,U fort.13 -divc,$factor -sub -selname,U fort.12 -selname,U fort.11 fort.1032

# V
cdo --silent -L replace fort.1031 -add -selname,V fort.13 -divc,$factor -sub -selname,V fort.12 -selname,V fort.11 fort.1131
cdo --silent -L replace fort.1032 -sub -selname,V fort.13 -divc,$factor -sub -selname,V fort.12 -selname,V fort.11 fort.1132

# PT
cdo --silent -L replace fort.1131 -add -selname,PT fort.13 -divc,$factor -sub -selname,PT fort.12 -selname,PT fort.11 fort.31
cdo --silent -L replace fort.1132 -sub -selname,PT fort.13 -divc,$factor -sub -selname,PT fort.12 -selname,PT fort.11 fort.32

wait

/bin/rm -f fort.1031
/bin/rm -f fort.1032
/bin/rm -f fort.1131
/bin/rm -f fort.1132
