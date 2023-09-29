library(tidyverse)

visitors <- read_csv("data/UK-visitor-numbers.csv")

visitors %>%
  filter(
    setting == "O",
    region == "Yorkshire and the Humber",
    admission == "Members",
    n_2022 >= 100000
    ) %>%
  count()

visitors %>%
  group_by(setting) %>%
  summarise(
    mean_2022 = mean(n_2022),
    med_2022 = median(n_2022)
  )

visitors %>%
  summarise(
    mean_2021 = mean(n_2021, na.rm = TRUE),
    med_2021 = median(n_2021, na.rm = TRUE)
  )

visitors_with_nations <- visitors %>% 
  mutate(
    nation = case_when(
      region == "Northern Ireland" ~ "Northern Ireland",
      region == "Scotland" ~ "Scotland",
      region == "Wales" ~ "Wales",
      TRUE ~ "England"
    )
  )

visitors_with_nations %>%
  group_by(nation) %>%
  summarise(
    IQR_2022 = IQR(n_2022, na.rm = TRUE)
  )

visitors_with_nations %>%
  group_by(nation) %>%
  count() %>%
  arrange(desc(n))

visitors_with_nations %>%
  filter(admission == "Free") %>%
  group_by(nation)