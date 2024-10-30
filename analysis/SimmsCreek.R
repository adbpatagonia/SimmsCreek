# ADB
# 2024-10-30

# From Eric:
# What I want to try to see is whether there is a correlation between
# the number of juvenile CT that out migrate in the spring and
# the number of spawning CO the previous fall."
# The number of juvenile CT that move past the fence varies from a low of ~75 fish to a high of >750 fish. I am curious as to why.
# CO are more stable, ranging from ~350 to ~900
# It's a small semi-urban stream. I would assume there are density issues.
# fall trap counts all returning adults. Spring trap counts all outmigrating juveniles


# libraries -----
library(data.table)
library(tidyverse)
library(mgcv)
library(gratia)
library(stringr)
library(gam.hp)

# functions -----
source('R/plottheme.r')
ggplot2::theme_set(plottheme())

# read data ----
regional.dat <- fread('data/RegionalAbundance.csv')
simmscreek.dat <- fread('data/SimmsCreekData.csv')

# wrangle data -----
regional.dat[, V1 := NULL]
simmscreek.dat[, V1 := NULL]


regional.dat <- regional.dat[species %in% c("Coho" )]
simmscreek.dat <- simmscreek.dat[Species %in% c("CO", "CT" )]

simmscreek.dat[, ageclass := NA]
simmscreek.dat[Species  == "CT",
               ageclass := ifelse(Length >= 230, "adult", "juvenile")]



# EDA ----
simmscreek.dat %>%
  group_by(Species, Year, Period) %>%
  reframe(n_fish = n()) %>%
  filter(Period == "Spring") %>%
  ggplot(., aes(Year, n_fish)) +
  geom_point() +
  geom_line() +
  facet_wrap(Species ~ .)

# how were the spawners smoothed?
# looks weird
# I'll use the non-smoothed spawners
ggplot(regional.dat, aes(x = year, y = spawners)) +
  geom_point() +
  geom_line(aes( y = smoothedSpawners))

# out year is lagged by 1 to match it with the juveniles outmigrating
reg.dat <- regional.dat[, .(out.year = year - 1, species, spawners)] %>%
  mutate(species = "CO")

fish.dat <- simmscreek.dat %>%
  group_by(Species, Year, Period) %>%
  reframe(juveniles = n()) %>%
  filter(Period == "Spring") %>%
  select(out.year = Year,
         species = Species,
         juveniles) %>%
  left_join(reg.dat, by = c("out.year", "species")) %>%
  data.table()

left_join(
  fish.dat[species == "CT", .(out.year, species, juveniles)],
  fish.dat[species == "CO", .(out.year, species, spawners)],
  by = 'out.year') %>%
  ggplot(., aes(y = juveniles, x = spawners)) +
  geom_point() +
  geom_smooth() +
  xlab("Coho Spawners in year y-1") +
  ylab("Cutthroat Trout juveniles in year y")


left_join(
  fish.dat[species == "CT", .(out.year,  juveniles)],
  fish.dat[species == "CO", .(out.year,  spawners)],
  by = 'out.year') %>%
  mutate(spawners = spawners /1000) %>%
  rename(`thousands of spawners` = spawners) %>%
  pivot_longer(!out.year, names_to = "ageclass", values_to = "n.fish") %>%
  ggplot(., aes(out.year, n.fish, color = ageclass)) +
  geom_point() +
  geom_line()


fish.dat[species == "CO"] %>%
  ggplot(., aes(y = juveniles, x = spawners)) +
  geom_point() +
  geom_smooth() +
  xlab("Coho Spawners in year y-1") +
  ylab("Coho juveniles in year y")

left_join(
  simmscreek.dat %>%
    group_by(Species, Year, Period) %>%
    reframe(juv_CT = n()) %>%
    filter(Period == "Spring") %>%
    filter(Species == "CT"),

  simmscreek.dat %>%
    group_by(Species, Year, Period) %>%
    reframe(ad_CO = n()) %>%
    filter(Period != "Spring") %>%
    filter(Species == "CO") %>%
    mutate(Year = Year -1),
  by = "Year"
) %>%
  # filter(ad_CO < 200) %>%
  ggplot(., aes(ad_CO, juv_CT )) +
  geom_point()  +
  ggtitle("Juvenile CT as a function of Adult CO the previous fall. All data from Simms Creek")


left_join(
  simmscreek.dat %>%
    group_by(Species, Year, Period) %>%
    reframe(juv_CO = n()) %>%
    filter(Period == "Spring") %>%
    filter(Species == "CO"),

  simmscreek.dat %>%
    group_by(Species, Year, Period) %>%
    reframe(ad_CO = n()) %>%
    filter(Period != "Spring") %>%
    filter(Species == "CO") %>%
    mutate(Year = Year -1),
  by = "Year"
) %>%
  ggplot(., aes(ad_CO, juv_CO )) +
  geom_point()  +
  ggtitle("Juvenile CO as a function of Adult CO the previous fall. All data from Simms Creek")


simmscreek.dat %>%
  group_by(Species, Year, Period) %>%
  reframe(n.fish = n()) %>%
  filter(Period == "Spring") %>%
  pivot_wider(names_from = Species, values_from = n.fish) %>%
  ggplot(.,aes(x = CO, y = CT)) +
  geom_point() +
  geom_smooth()


simmscreek.dat %>%
  group_by(Species, Year, Period, ageclass) %>%
  reframe(n.fish = n()) %>%
  filter(Period == "Spring") %>%
  filter(ageclass %in% c(NA, "juvenile")) %>%
  # pivot_wider(names_from = Species, values_from = n.fish) %>%
  ggplot(.,aes(x = Year, y = n.fish, color = Species)) +
  geom_point() +
  geom_smooth()


simmscreek.dat %>%
  filter(Period == "Spring") %>%
  filter(Species == "CT") %>%
  ggplot(., aes(x=Length))+
  geom_histogram() +
  facet_wrap(Year ~ .)


CT.simms <- simmscreek.dat %>%
  filter(Period == "Spring") %>%
  group_by(Year, Species, ageclass) %>%
  filter(Species == "CT") %>%
  reframe(n.fish = n()
          ) %>%
  data.table()

CT.simms[ageclass == "adult", Year := Year -2]

CT.simms %>%
  pivot_wider(names_from = ageclass, values_from = n.fish) %>%
  ggplot(., aes(x = adult, y = juvenile)) +
  geom_point()

CT.simms[ageclass != "adult", n.fish := n.fish/100]

ggplot(CT.simms, aes(x = Year, y = n.fish, color = ageclass)) +
  geom_point() +
  geom_smooth()


ggplot(CT.simms, aes(x = Year, y = n.fish, color = ageclass)) +
  geom_point() +
  geom_smooth()

# HGAMs ------

juvs.simms <- simmscreek.dat %>%
  group_by(Species, Year, Period, ageclass) %>%
  reframe(n.fish = n()) %>%
  filter(Period == "Spring") %>%
  filter(ageclass %in% c(NA, "juvenile")) %>%
  select(-ageclass) %>%
  data.table()

source('analysis/SimmsCreek_HGAMs.R')
