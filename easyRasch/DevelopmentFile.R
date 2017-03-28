
# Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("/Users/hyunjoooh/Desktop/2017_Stat_Prog/Midterm_2017") # change to match the directory

# This is run once when the package strcuture is first created

# At this point put the *.R files into the correct directories 
# and edit the DESCRIPTION file

# Let's look through the R directory in this order:
#‘EAP.R’,
#‘Likelihood.R’,
#‘Plot.R’,
#‘Prior.R’,
#‘Rasch-class.R’

# Now the NAMESPACE

## This can be run many times as the code is updates
current.code <- as.package("easyRasch")
load_all(current.code)
document(current.code)
test(current.code)

