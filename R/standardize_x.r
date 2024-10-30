#Standardize continuous covariates


standardize_x <- function(x) { (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}

# standardize, providing the x, AND the mean and sd with respect to which we want to normalize
# the default values are identical to those provided by standardize_x
normalize_x <- function(x, 
                        meanval = mean(x, na.rm = TRUE),
                        sdval = sd(x, na.rm = TRUE)) { 
  (x - meanval) / sdval
}
