# set up analysis ----
mstable <- data.table()


gam_data <- juvs.simms %>%
  transform(Species = factor(Species, levels = c('CO', 'CT')))


## model G ----
# A single common (global) smoother for all observations
# two smoothers:
# 1. a Thin plate regression spline of year, and
# 2. a random effect for Species to model reach-specific intercepts
# The random effect smoother (bs="re") that we used for the Species factor
# always has a k value equal to the number of levels in the grouping variable
simms_modG <- gam(n.fish ~ s(Year, k = 7, bs = "tp") +
                 s(Species, k = length(levels(gam_data$Species)), bs = "re"),
               data = gam_data,
               method = "REML", family = "gaussian")
### Model check ----
k.check(simms_modG)
appraise(simms_modG)
concurvity(simms_modG)
concurvity(simms_modG, full = FALSE)
draw(simms_modG)
# visreg::visreg(simms_modG,"Year", by = "Species")
# overall concurvity
o_conc <- concrvity(simms_modG)
draw(o_conc)
# pairwise concurvity
p_conc <- concrvity(simms_modG, pairwise = TRUE)
draw(p_conc)

## model GS ----
# A single common smoother plus group-level smoothers that have the same wiggliness
# analogue to a GLMM with varying slopes
# two smoothers:
# 1. a Thin plate regression spline of year, and
# 2.  group-level smoother (reach-specific)

simms_modGS <- gam(n.fish ~ s(Year, k = 7, bs = "tp") +
                  s(Year, Species, k = 7, bs = "fs", m = 2),
                data = gam_data,
                method = "REML", family = "gaussian")

### Model check ----
k.check(simms_modGS)
appraise(simms_modGS)
concurvity(simms_modGS)
concurvity(simms_modGS, full = FALSE)
draw(simms_modGS)
# visreg::visreg(simms_modGS,"Year", by = "Species")
# overall concurvity
o_conc <- concrvity(simms_modGS)
draw(o_conc)
# pairwise concurvity
p_conc <- concrvity(simms_modGS, pairwise = TRUE)
draw(p_conc)

## model GI ----
# A single common smoother plus group-level smoothers that have their own level of wiggliness
# three smoothers:
# 1. a Thin plate regression spline of year, and
# 2.  group-level smoother (reach-specific). Diffrence from model GS: m = 1
# 3. a random effect for Species to model reach-specific intercepts
# The random effect smoother (bs="re") that we used for the Species factor
# always has a k value equal to the number of levels in the grouping variable

simms_modGI <- gam(n.fish ~ s(Year, k = 7, bs = "tp") +
                  s(Year, Species, k = 7, bs = "fs", m = 1) +
                  s(Species, k = length(levels(gam_data$Species)), bs = "re"),
                data = gam_data,
                method = "REML", family = "gaussian")

### Model check ----
k.check(simms_modGI)
appraise(simms_modGI)
concurvity(simms_modGI)
concurvity(simms_modGI, full = FALSE)
draw(simms_modGI)
# visreg::visreg(simms_modGI,"Year", by = "Species")
# overall concurvity
o_conc <- concrvity(simms_modGI)
draw(o_conc)
# pairwise concurvity
p_conc <- concrvity(simms_modGI, pairwise = TRUE)
draw(p_conc)

## model S ----
# Model S (shared smoothers) is model GS without the global smoother term
# This model assumes all groups have the same smoothness, but that the individual shapes of the smooth terms are not related
# If in a study there are very few data points in each grouping level (relative to the strength of the functional relationship of interest), estimates from model S will typically be much more variable than from model GS
# one smoother:
# 1.  group-level smoother (reach-specific)

simms_modS <- gam(n.fish ~ s(Year, Species, k = 7, bs = "fs", m = 2),
               data = gam_data,
               method = "REML", family = "gaussian")

### Model check ----
k.check(simms_modS)
appraise(simms_modS)
concurvity(simms_modS)
concurvity(simms_modS, full = FALSE)
draw(simms_modS)
# visreg::visreg(simms_modS,"Year", by = "Species")
# overall concurvity
o_conc <- concrvity(simms_modS)
draw(o_conc)
# pairwise concurvity
p_conc <- concrvity(simms_modS, pairwise = TRUE)
draw(p_conc)




## model I ----
# Model I is model GI without the first term
#  group-level smoothers that have their own level of wiggliness
# two smoothers:
# 1.  group-level smoother (reach-specific). Diffrence from model GS: m = 1
# 2. a random effect for Species to model reach-specific intercepts
# The random effect smoother (bs="re") that we used for the Species factor
# always has a k value equal to the number of levels in the grouping variable

simms_modI <- gam(n.fish ~ s(Year, Species, k = 7, bs = "fs", m = 1) +
                 s(Species, k = length(levels(gam_data$Species)), bs = "re"),
               data = gam_data,
               method = "REML", family = "gaussian")

### Model check ----
k.check(simms_modI)
appraise(simms_modI)
concurvity(simms_modI)
concurvity(simms_modI, full = FALSE)
draw(simms_modI)
# visreg::visreg(simms_modI,"Year", by = "Species")
# overall concurvity
o_conc <- concrvity(simms_modI)
draw(o_conc)
# pairwise concurvity
p_conc <- concrvity(simms_modI, pairwise = TRUE)
draw(p_conc)

## model selection -----
ms <- AIC(simms_modI,
          simms_modGI,
          simms_modS,
          simms_modGS,
          simms_modG) %>%
  arrange(AIC)
ms$deltaAIC <- ms$AIC - min(ms$AIC)
ms$wi <- exp(-0.5 * ms$deltaAIC)/sum(exp(-0.5 * ms$deltaAIC))
ms$er <- max(ms$wi)/ms$wi

ms.rb <- data.table(ms, keep.rownames = TRUE) %>%
  rename(model = rn)
mstable <- rbindlist(l = list(mstable, ms.rb))

## plot best model GI ----
startyr <- min(gam_data$Year)
lastyr <- max(gam_data$Year)
wd <- 0.4
### effects plot ----
p_effects_modGI <- draw(simms_modGI)
p_effectsYear_modGI <- p_effects_modGI[[2]] +
  scale_x_continuous(breaks = seq(startyr,lastyr,2))
p_effectsYear_tr_modGI <- p_effects_modGI[[3]] +
  scale_x_continuous(breaks = seq(startyr,lastyr,2)) +
  scale_colour_manual(name = '', values = viridis::viridis(3)[-3]) +
  theme(legend.position = "bottom")

### response plot -----
# setup prediction data
simms_modGI_pred <- with(gam_data,
                      expand.grid(Year = min(Year):max(Year),
                                  Species = levels(Species)))
# make the prediction, add this and a column of standard errors to the prediction data.frame.
simms_modGI_pred <- cbind(simms_modGI_pred,
                       predict(simms_modGI,
                               simms_modGI_pred,
                               se.fit = TRUE,
                               type = "response"))
# plot
p_bestmodel_modGI <- ggplot(data = gam_data, aes(x = Year, y = n.fish, color = Species, fill = Species)) +
  facet_grid(. ~ Species) +
  geom_ribbon(aes(ymin = (fit - 2*se.fit),
                  ymax = (fit + 2*se.fit),
                  x = Year,
                  fill = Species),
              color = "transparent",
              data = simms_modGI_pred,
              alpha = 0.15,
              inherit.aes = FALSE) +
  geom_line(aes(y = fit, color = Species, fill = Species), data = simms_modGI_pred) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0,
                                             dodge.width = wd), size = 2, alpha = 0.4) +
  scale_colour_manual(name = '', values = viridis::viridis(3)[-3]) +
  scale_fill_manual(name = '', values = viridis::viridis(3)[-3]) +
  scale_x_continuous(breaks = min(gam_data$Year, na.rm = TRUE):max(gam_data$Year, na.rm = TRUE)) +
  xlab("Year") +
  ylab('Number of fish') +
    theme(legend.position = 'none',
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 13),
        strip.text.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.background = element_blank()) +
  scale_x_continuous(breaks = seq(startyr,lastyr,2))

## plot best model I ----
### effects plot ----
p_effects_modI <- draw(simms_modI)
p_effectsYear_tr_modI <- p_effects_modI[[2]] +
  scale_x_continuous(breaks = seq(startyr,lastyr,2)) +
  scale_colour_manual(name = '', values = viridis::viridis(3)[-3]) +
  theme(legend.position = "bottom")

### response plot -----
# setup prediction data
simms_modI_pred <- with(gam_data,
                     expand.grid(Year = min(Year):max(Year),
                                 Species = levels(Species)))
# make the prediction, add this and a column of standard errors to the prediction data.frame.
simms_modI_pred <- cbind(simms_modI_pred,
                      predict(simms_modI,
                              simms_modI_pred,
                              se.fit = TRUE,
                              type = "response"))
# plot
p_bestmodel_modI <- ggplot(data = gam_data, aes(x = Year, y = n.fish, color = Species, fill = Species)) +
  facet_grid(. ~ Species) +
  geom_ribbon(aes(ymin = (fit - 2*se.fit),
                  ymax = (fit + 2*se.fit),
                  x = Year,
                  fill = Species),
              color = "transparent",
              data = simms_modI_pred,
              alpha = 0.15,
              inherit.aes = FALSE) +
  geom_line(aes(y = fit, color = Species, fill = Species), data = simms_modI_pred) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0,
                                             dodge.width = wd), size = 2, alpha = 0.4) +
    scale_colour_manual(name = '', values = viridis::viridis(3)[-3]) +
  scale_fill_manual(name = '', values = viridis::viridis(3)[-3]) +
  scale_x_continuous(breaks = min(gam_data$Year, na.rm = TRUE):max(gam_data$Year, na.rm = TRUE)) +
  xlab("Year") +
  ylab('Number of fish') +
  theme(legend.position = 'none',
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 13),
        strip.text.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.background = element_blank()) +
  scale_x_continuous(breaks = seq(startyr,lastyr,2))


### plot river smooths from the 2 best models ----
p_effectsYear_tr_data <- bind_rows(
  p_effectsYear_tr_modGI$data %>%
    mutate(Model = "GI"),
  p_effectsYear_tr_modI$data %>%
    mutate(Model = "I")
)

p_effectsYear_tr <- ggplot(p_effectsYear_tr_data, aes(x = Year, y = est, color = Species)) +
  geom_line(aes(linetype = Model)) +
  scale_colour_manual(name = 'Species', values = viridis::viridis(3)[-3]) +
  scale_x_continuous(breaks = min(gam_data$Year, na.rm = TRUE):max(gam_data$Year, na.rm = TRUE)) +
  xlab("Year") +
  ylab('Partial effect') +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        strip.text.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.background = element_blank()) +

  NULL

