sc_raw <- read.csv("samp_sc.csv")
# summary(sc_raw); str(sc_raw)

library(tidyverse)

sc <- sc_raw %>%
  select(psa_level_raw, bmi, race, age, education, pros_enl, comorb, diab) %>%
  rename(
    psa_level     = psa_level_raw,
    BMI           = bmi,
    agecat        = age,
    pros_enlarged = pros_enl,
    comorbidity   = comorb,
    diabetes      = diab
  ) %>%
  mutate(
    BMI = case_when(
      BMI == 0 ~ "Underweight",
      BMI == 1 ~ "Normal",
      BMI == 2 ~ "Overweight",
      BMI == 3 ~ "Obese",
      BMI == 4 ~ "Morbidly Obese"
    ),
    agecat = case_when(
      agecat <  55 ~ 0L,
      agecat <  60 ~ 1L,
      agecat <  65 ~ 2L,
      agecat <  70 ~ 3L,
      agecat >= 70 ~ 4L
    )
  ) %>%
  filter(agecat != 0L, BMI != "Underweight") %>%
  mutate(across(!all_of(c("psa_level")), as.factor))


summary(sc)
mean(sc$psa_level)


