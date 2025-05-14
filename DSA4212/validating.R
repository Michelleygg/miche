#' the function to extract the student number from a string
#' the package stringr is used
extract.student.number <- function(s) stringr::str_extract(s, 'A[0-9]{7}[A-Z]{1}')

## test case
# make sure you set the Working Directory to the folder of validating.R
# make sure test-y.csv is in the same folder of validating.R

submitted.file = 'A0000000A.csv'  # change this to the file name of your file to be submitted; also make sure it is in the same folder of validating.R
extract.student.number(submitted.file) # this should output your student number

# recheck your file if any error when running the follow code
pred <- read.csv(file=submitted.file)
stopifnot(!is.null(ncol(pred)))
stopifnot(ncol(pred)==1)
stopifnot(!is.na(nrow(pred)))
stopifnot(!is.null(nrow(pred)))
stopifnot(nrow(pred) == 10000)
stopifnot(!any(sapply(pred,is.na)))

