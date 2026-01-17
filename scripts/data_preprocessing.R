library(tidyverse)

setwd("~/module-3/group-project/")

patients <- read.csv(file = "data/patients_table.csv")

admissions <- read.csv(file = "data/patient_admissions.csv")

cat_cols <- c("race", "admission_type",
              "insurance", "language", "marital_status")

merged <- admissions %>%
  left_join(patients %>% select(subject_id, gender, anchor_year, anchor_age, dod),
            by = "subject_id") %>%
  mutate(nonenglish = case_when(
              language == "English" ~ 0,
              language != "English" ~ 1),
         gender = case_when(
           gender == "M" ~ 0,
           gender == "F" ~ 1),
         dod = ymd(dod), # date of death if applicable
         deathtime = ymd_hms(deathtime), # death time 
         admittime = ymd_hms(admittime),
         dischtime = ymd_hms(dischtime),
         LoS  = as.numeric(dischtime - admittime, units = "days"), # Hospital Length of Stay in DAYS
         edregtime = ymd_hms(edregtime),
         edouttime = ymd_hms(edouttime),
         LoS_ER  = as.numeric(edouttime - edregtime, units = "hours"), # ER Length of Stay in HOURS
         LoS_ER = ifelse(is.na(LoS_ER), 0, LoS_ER),
         current_age = year(admittime) - anchor_year + anchor_age) %>% # Age of patient at admission
  group_by(subject_id) %>%
  arrange(admittime, .by_group = TRUE) %>%
  mutate(admission_no = row_number()) %>% # Create admission number
  ungroup() %>%
  mutate(across(all_of(cat_cols), ~ replace_na(as.character(.), "Unknown"))) %>%
  mutate(across(all_of(cat_cols), as.factor)) %>%
  select(subject_id, hadm_id, admission_no, 
         LoS, LoS_ER, hospital_expire_flag, 
         deathtime, dod, current_age, gender, 
         race, admission_type, insurance, nonenglish, marital_status)

pca <- merged %>%
  select(LoS, LoS_ER, nonenglish, current_age, hospital_expire_flag, gender) %>%
  prcomp(center = T, scale = T)

pca_df <- pca %>%
  broom::augment(merged)

pca_rotations <- pca$rotation %>% 
  as.data.frame() %>% select(PC1, PC2) %>%
  rownames_to_column(var = "features")

plot.pca <- pca_df %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, color = gender)) +
  geom_point(alpha = 0.3, show.legend = F) +
  labs(title = "PCA Plot",
       x = "PC1",
       y = "PC2")

plot.pca +
  geom_segment(
    inherit.aes = F,
    data = pca_rotations,
    aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5)) +
  geom_text(
    inherit.aes = F,
    data = pca_rotations,
    aes(label = features,
        x = PC1 * 5, y = PC2 * 5)
  )

merged %>% 
  filter(LoS > 7) %>%
  ggplot(aes(x = LoS, fill = as.factor(insurance))) +
  geom_density(alpha = 0.7) +
  theme_bw()

merged %>%
  ggplot(aes(x = current_age, y = hospital_expire_flag)) +
  geom_point()

print(pca)

write_csv(merged, file = "data/cleaned.csv")

