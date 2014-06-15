#!/bin/bash

# run this from the inst/ directory
# go to the root directory of the package
cd ..

vers=`grep Version: DESCRIPTION | awk '{print $2}'`

R --vanilla --quiet -e 'library(devtools); document()'

export R_RD4PDF=times,hyper
rm -f inst/tictoc_${vers}.pdf 
R CMD Rd2pdf -o inst/tictoc_${vers}.pdf .

