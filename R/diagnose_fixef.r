# Function to plot diagnostics of fixed effects of glmm models

diagnose_fixef <- function(model){
  
  par(mfrow=c(2,2))
  qqnorm(resid(model), main = "normal qq-plot, residuals")
  qqline(resid(model))
  
  plot(fitted(model), resid(model), xlab = 'fitted', ylab = 'residuals') 
  abline(h=0)
  
  plot(fitted(model), model@frame[,1], xlab = 'fitted', ylab = 'observed')
  abline(0,1)
  
  par(mfrow=c(1,1))
}