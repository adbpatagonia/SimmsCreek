# ADB
# group helper functions
# taken from https://speakerdeck.com/lionelhenry/reusing-tidyverse-code?slide=38

require(dplyr)
require(magrittr)


# group mean ----
group_mean <- function(data, var, by){
  data %>%
    group_by({{ by }}) %>%
    dplyr::summarise(avg = mean({{ var }}, na.rm = TRUE))
}

# group count ----
group_count <- function(data, var, by){
  data %>%
    group_by({{ by }}) %>%
    dplyr::summarise(count = n())
}

# group summary ----
group_summary <- function(data, var, by){
  data %>%
    group_by({{ by }}) %>%
    dplyr::summarise(Count = n(),
      Mean = mean({{ var }}, na.rm = TRUE),
      Median = median({{ var }}, na.rm = TRUE),
      Min = min({{ var }}, na.rm = TRUE),
      Max = max({{ var }}, na.rm = TRUE),
      FirstQuantile = quantile({{ var }}, na.rm = TRUE, probs = 0.25),
      ThirdQuantile = quantile({{ var }}, na.rm = TRUE, probs = 0.75),
      NumNA = sum(is.na({{ var }})),
      SD = sd({{ var}}, na.rm = TRUE)) %>%
    mutate(SE = SD/sqrt(Count))
}
