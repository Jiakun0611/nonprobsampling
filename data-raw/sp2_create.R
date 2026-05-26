sp_NHIS <- read.csv("NHIS_Harmonization_07302025.csv") %>% filter(SAMPWEIGHT != 0)

summary(sp_NHIS)



# sc has: psa_level_raw, bmi, race, age, education, pros_enl, comorb, diab;

sp2 <- sp_NHIS %>%
  select(d_agecat, d_marital, d_race, d_empstat,
         d_diabetes,
         d_bmicat2, d_smoking, d_comorbidity, SAMPWEIGHT,
         STRATA, PSU)

sp2 <- sp2 %>% rename(  agecat = d_agecat, marital = d_marital, race = d_race, employment = d_empstat,
                        diabetes = d_diabetes,
                        BMI = d_bmicat2, smoking = d_smoking, comorbidity = d_comorbidity,
                        wts_sp2 = SAMPWEIGHT, strata_sp2 = STRATA, psu_sp2 = PSU)

sp2 <- sp2 %>%
  mutate(BMI = case_when(
    BMI == 0 ~ "Underweight",
    BMI == 1 ~ "Normal",
    BMI == 2 ~ "Overweight",
    BMI == 3 ~ "Obese",
    BMI == 4 ~ "Morbidly Obese"
  )) %>%
  mutate(across(!all_of(c("wts_sp2", "strata_sp2", "psu_sp2")), as.factor))

summary(sp2)
