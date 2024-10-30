# Function to plot diagnostics of random effects of glmm models

diagnose_ranef <- function(model){
  nre <- length((ranef(model)))
  if(nre == 1){
    par(mfrow = c(1,1))
  }
  if(nre == 2){
    par(mfrow = c(1,2))
  }
  if(nre > 2){
    par(mfrow = c(2, floor(nre/2)))
  }
  
  for (i in seq_along(ranef(model))){
    qqnorm(ranef(model)[names(ranef(model))[i]][[1]][,1], main = names(ranef(model)[i]))
    qqline(ranef(model)[names(ranef(model))[i]][[1]][,1])
  }
  
  par(mfrow=c(1,1))
}