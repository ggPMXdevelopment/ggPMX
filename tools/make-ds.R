# Dataset preparation
# -------------------
#
# UPDATE THIS FILE IF YOU INTEND TO INCLUDE DATASETS IN YOUR PACKAGE,
# DELETE OTHERWISE.
#
# The below functions prepare data sets for inclusion in the package and make 
# them accessible to either the project code only (internal data) or also to
# the user (external data). 
# 
# Data sets need to be included in /data/ as .rda files. If your data are in a 
# different format you can generate the .rda version of your data on the fly as 
# is done in the following examples.


# Pepare external example data set from csv input

make_ds <- function() {
  
  # If data are originally available as .csv, read them from data-raw/
  testdata <<- read.csv("data-raw/testdata.csv")
  # use_data expects it's data sets in the global env (which is why
  # we do <<-)
  
  use_data(testdata, # testdata2, testdata3, 
                     # Add further data sets here (separated by comma)
           overwrite=TRUE)
  # use_data will make a file called testdata.rda available in data/
  
}


# Pepare some internal example data sets 

make_internal_ds <- function() {
  
  if(!file.exists("inst/extra/testdata_internal.rda")) {
    
    # If data set does not yet exist, generate 
    # e.g. numbers from some publication
    # (could also be done by reading some data from file)
    dat1 <<- data.frame(dataset = 1, col1 = 1:5, col2 = 5:1)
    dat2 <<- data.frame(dataset = 2, col1 = 1:5, col2 = 1:5*10)
    save(list=c("dat1", "dat2"), file="inst/extra/testdata_internal.rda")
    
  }
    
  load("inst/extra/testdata_internal.rda", envir=.GlobalEnv)
  
  use_data(dat1, dat2,
           internal=TRUE, # Suppress external use: Data will be accessible to 
                          # the project code, but not to the users
           overwrite=TRUE)
  
}


library(devtools)

make_ds()
make_internal_ds()
