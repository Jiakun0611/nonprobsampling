sp_NHANES <- read.csv("NHANES_Harmonization_07302025.csv")
#summary(sp_NHANES)
#str(sp_NHANES)

library(tidyverse)
sp1 <- sp_NHANES %>%
  select(
    d_agecat, d_marital, d_race, d_educat, d_empstat,
    d_smoking, d_comorbidity, d_psa_level0, d_bmicat2,
    d_diabetes, d_pros_enlarged,
    SDMVSTRA, SDMVPSU, WTINT10YR
  )
sp1 <- sp1 %>% rename(
                        agecat = d_agecat, marital = d_marital, race = d_race, education = d_educat, employment = d_empstat,
                        smoking = d_smoking, comorbidity = d_comorbidity, psa_level = d_psa_level0, BMI = d_bmicat2,
                        diabetes = d_diabetes, pros_enlarged = d_pros_enlarged,
                        psu_sp1 = SDMVPSU, strata_sp1 = SDMVSTRA, wts_sp1 = WTINT10YR)

sp1 <- sp1 %>%
  mutate(BMI = case_when(
    BMI == 0 ~ "Underweight",
    BMI == 1 ~ "Normal",
    BMI == 2 ~ "Overweight",
    BMI == 3 ~ "Obese",
    BMI == 4 ~ "Morbidly Obese"
  )) %>%
  filter(BMI != "Underweight") %>%
  mutate(across(!all_of(c("psa_level", "strata_sp1", "psu_sp1", "wts_sp1")),
                as.factor))

summary(sp1)

true_mean_psa_sp1 <- weighted.mean(sp1$psa_level, sp1$wts_sp1, na.rm = TRUE)
true_mean_psa_sp1


