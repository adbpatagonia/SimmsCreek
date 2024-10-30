
precision_kok <- function(m, nboot, extrayears){
  
  if (class(m) == "lm") {datframe <- m$model}
  if (grepl(x = class(m), pattern = 'lmer')) {datframe <- m@frame}
  
  # * obtain confidence intervals on baci effect ----
  cis <- confint(m, 
                 method = 'boot',  
              #       nsim = 20,
                 parm = dimnames(m@pp$X)[[2]][which(grepl(pattern = ':', x = dimnames(m@pp$X)[[2]]))],
                 oldNames = FALSE)
  #cis <- subset(cis, rownames(cis) %in%  attr(cis, 'dimnames')[[1]][grepl(x = attr(cis, 'dimnames')[[1]], pattern = ':')])
  meaneffects <- subset(coef(summary(m)),
                        rownames(coef(summary(m))) %in%  attr(coef(summary(m)), 'dimnames')[[1]][grepl(x = attr(coef(summary(m)), 'dimnames')[[1]], pattern = ':period')]
                        ,select = Estimate)
  meaneffects <- cbind(meaneffects, data.frame(nyears = rep(nrow(unique(datframe %>% filter(period == 'After') %>% select('year'))), times = nrow(meaneffects))))
  bacis <- cbind(meaneffects, cis)
  
  # * simulate extra years of sampling ----
  # ** define mean sample size by year ----
  sampsize <- datframe %>%
    filter(period == 'After') %>%
    group_by(year, site) %>%
    summarise(n = n()) %>% 
    ungroup() %>%
    summarise(mn = floor(mean(n)))
  sampsize <- sampsize[[1]]
  # ** define strata based on the variability structure of the model, sample with replacement ----
  widths <- data.frame(NULL)
  
  try(
    for (ey in unique(extrayears)) {
      for (i in 1:nboot) {
        
        nstrata <- datframe %>% filter(period == 'After') %>% select(site) %>% unique() %>% nrow()
        #  extrayears <- c(1, 3, 5)
        s <- strata(data = datframe %>% filter(period == 'After'),
                    stratanames = c('site'), 
                    size = rep(ey*sampsize, nstrata), 
                    method = 'srswr')
        # ** get simulated data ----
        newdat <- getdata(data = datframe %>% filter(period == 'After'), s)
        newdat <- newdat[, names(datframe)]
        newdat$year <- factor( rep(max(datframe$year) + 1:ey, times = nstrata ))
        # newdat$year <- factor( rep(max(datframe$year) + 1:ey, times = sampsize * length(unique(s$strata))))
        newdat <- droplevels(newdat)
        # ** add simulated data to observed data----
        dat <- rbind(datframe, newdat)
        
        # * update model with a simulated extra year of sampling ----
        newm <- update(m, data = dat)
        
        # * obtain confidence intervals on baci effect, model with a simulated extra year of sampling----
        newcis <- confint(newm, 
                          method = 'boot',  
                      #     nsim = 20,
                          parm = dimnames(newm@pp$X)[[2]][which(grepl(pattern = ':', x = dimnames(newm@pp$X)[[2]]))],
                          oldNames = FALSE)
        
        
        width <- data.frame(extrayears = NA, width = NA)
        width$extrayears <- ey
        width$width <- newcis[2] - newcis[1]
        
        widths <- rbind(widths, width)
        
      }
      
    }  
  )
  wdth <- data.frame(value = quantile(x = widths$extrayears, probs = c(0.025, 0.5, 0.975)), quant = c('llim', 'med', 'ulim'), nyears = c(5,5,5) )
  
  
  p <- c(0.025, 0.5, 0.975)
  p_names <- map_chr(p, ~paste0(.x*100, "%"))
  
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  
  
  
  wdth <-  widths %>%
    group_by(extrayears) %>%
    summarize_at(vars(width), funs(!!!p_funs))
  names(wdth) <- c('extrayears', 'llim', 'med', 'ulim')
  
  wdth <- rbind(wdth,
                data.frame(extrayears = 0,
                           llim = NA,
                           med = bacis[,'97.5 %'] - bacis[,'2.5 %'],
                           ulim = NA)
  ) %>%
    mutate(nyears = extrayears + bacis$nyears)
  
  
  return(wdth)
}





precision_kok_prof <- function(m, nboot, extrayears){
  
  if (class(m) == "lm") {datframe <- m$model}
  if (grepl(x = class(m), pattern = 'lmer')) {datframe <- m@frame}
  
  # * obtain confidence intervals on baci effect ----
  cis <- confint(m, 
                 method = 'profile',  
            #     nsim = 20,
                 parm = dimnames(m@pp$X)[[2]][which(grepl(pattern = ':', x = dimnames(m@pp$X)[[2]]))],
                 oldNames = FALSE)
  #cis <- subset(cis, rownames(cis) %in%  attr(cis, 'dimnames')[[1]][grepl(x = attr(cis, 'dimnames')[[1]], pattern = ':')])
  meaneffects <- subset(coef(summary(m)),
                        rownames(coef(summary(m))) %in%  attr(coef(summary(m)), 'dimnames')[[1]][grepl(x = attr(coef(summary(m)), 'dimnames')[[1]], pattern = ':period')]
                        ,select = Estimate)
  meaneffects <- cbind(meaneffects, data.frame(nyears = rep(nrow(unique(datframe %>% filter(period == 'After') %>% select('year'))), times = nrow(meaneffects))))
  bacis <- cbind(meaneffects, cis)
  
  # * simulate extra years of sampling ----
  # ** define mean sample size by year ----
  sampsize <- datframe %>%
    filter(period == 'After') %>%
    group_by(year, site) %>%
    summarise(n = n()) %>% 
    ungroup() %>%
    summarise(mn = floor(mean(n)))
  sampsize <- sampsize[[1]]
  # ** define strata based on the variability structure of the model, sample with replacement ----
  widths <- data.frame(NULL)
  
  try(
    for (ey in unique(extrayears)) {
      for (i in 1:nboot) {
        
        nstrata <- datframe %>% filter(period == 'After') %>% select(site) %>% unique() %>% nrow()
        #  extrayears <- c(1, 3, 5)
        s <- strata(data = datframe %>% filter(period == 'After'),
                    stratanames = c('site'), 
                    size = rep(ey*sampsize, nstrata), 
                    method = 'srswr')
        # ** get simulated data ----
        newdat <- getdata(data = datframe %>% filter(period == 'After'), s)
        newdat <- newdat[, names(datframe)]
        newdat$year <- factor( rep(max(datframe$year) + 1:ey, times = nstrata ))
        # newdat$year <- factor( rep(max(datframe$year) + 1:ey, times = sampsize * length(unique(s$strata))))
        newdat <- droplevels(newdat)
        # ** add simulated data to observed data----
        dat <- rbind(datframe, newdat)
        
        # * update model with a simulated extra year of sampling ----
        newm <- update(m, data = dat)
        
        # * obtain confidence intervals on baci effect, model with a simulated extra year of sampling----
        newcis <- confint(newm, 
                          method = 'profile',  
                   #       nsim = 20,
                          parm = dimnames(newm@pp$X)[[2]][which(grepl(pattern = ':', x = dimnames(newm@pp$X)[[2]]))],
                          oldNames = FALSE)
        
        
        width <- data.frame(extrayears = NA, width = NA)
        width$extrayears <- ey
        width$width <- newcis[2] - newcis[1]
        
        widths <- rbind(widths, width)
        
      }
      
    }  
  )
  wdth <- data.frame(value = quantile(x = widths$extrayears, probs = c(0.025, 0.5, 0.975)), quant = c('llim', 'med', 'ulim'), nyears = c(5,5,5) )
  
  
  p <- c(0.025, 0.5, 0.975)
  p_names <- map_chr(p, ~paste0(.x*100, "%"))
  
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  
  
  
  wdth <-  widths %>%
    group_by(extrayears) %>%
    summarize_at(vars(width), funs(!!!p_funs))
  names(wdth) <- c('extrayears', 'llim', 'med', 'ulim')
  
  wdth <- rbind(wdth,
                data.frame(extrayears = 0,
                           llim = NA,
                           med = bacis[,'97.5 %'] - bacis[,'2.5 %'],
                           ulim = NA)
  ) %>%
    mutate(nyears = extrayears + bacis$nyears)
  
  
  return(wdth)
}
