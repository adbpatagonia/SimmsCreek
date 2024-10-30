
panel.cor <- function(x, y, digits=2, prefix="", cex.cor = 1) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- round(cor(x, y, method = "pearson", use = "complete.obs"), 2)
  rs <- round(cor(x, y, method = "spearman", use = "complete.obs"), 2)
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  
  eq <- bquote(italic(r[pearson] == .(r)))
  eqs <- bquote(italic(r[spearman] == .(rs)))
  
  
  text(0.5, 0.6, eq, cex = cex.cor)
  text(0.5, 0.4, eqs, cex = cex.cor) 
 
}


panel.cor.pear <- function(x, y, digits=2, prefix="", cex.cor = 1.25) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- round(cor(x, y, method = "pearson", use = "complete.obs"), 2)
  rs <- round(cor(x, y, method = "spearman", use = "complete.obs"), 2)
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  
  eq <- bquote(italic(r[pearson] == .(r)))
  eqs <- bquote(italic(r[spearman] == .(rs)))
  
  
  text(0.5, 0.5, eq, cex = cex.cor)
  # text(0.5, 0.4, eqs, cex = cex.cor) 
  
}