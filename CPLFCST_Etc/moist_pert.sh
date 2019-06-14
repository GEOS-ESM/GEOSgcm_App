#!/bin/csh 
module load other/cdo
# input:
# fort.23 -> start date of forecast
# fort.22 -> 1 day before start date
# fort.21 -> 2 days before start date 

# output:
# fort.41 -> perturbation plus 
# fort.42 -> perturbation minus

set factor = 8.0

# Q
cdo --silent -L replace fort.23 -add -selname,Q fort.23 -divc,$factor -sub -selname,Q fort.22 -selname,Q fort.21 fort.41
cdo --silent -L replace fort.23 -sub -selname,Q fort.23 -divc,$factor -sub -selname,Q fort.22 -selname,Q fort.21 fort.42

