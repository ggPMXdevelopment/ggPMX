# Script to perform the Operation Qualification
# ---------------------------------------------
#
# EDIT THE TESTS TO BE PERFORMED AS DESCRIBED BELOW UNDER 'RUNNING PACKAGE TESTS'.

library(testthat)
library(ggPMX)

cat("TEST RUN DATE:", date(), "\n")

cat("TESTING PACKAGE:\n")
print(packageDescription("ggPMX"))

cat("RUNNING PACKAGE TESTS:\n")
# Run each section separately to get subsequent numbering per section
# of the TAP reporter; execution order must be aligned with steps described 
# in the vignette
for(test in c("examples1", # Step 1: Arithmetic operations
              "examples2", # Step 2: Data lookup
              # Add further tests as a comma-separated list here
              "misc"      # Additional tests
              )) {  
    test_package("ggPMX", filter=test, reporter="tap")
}

# Finally run all tests once more, but with the stop reporter. This
# ensures that the last line of this script is only displayed if and
# only if all tests run successful
test_package("ggPMX", reporter="stop")

cat("\n\nR SESSION INFO:\n")

print(sessionInfo())

cat("\nTEST FINISH DATE:", date(), "\n")
cat("\n\nALL TESTS SUCCESSFUL\n")

