rm(list = ls())

library(tidyverse)
library(fastDummies)
library(caret)

set.seed(67)

######## PREPARATION #########

# Read Data Files
patients <- read.csv(file = "data/patients_table.csv") %>% select(-X)

admissions <- read.csv(file = "data/patient_admissions.csv") %>% select(-X)

diagnoses <- read.csv(file = "data/grouped_diagnosis_byCSS.csv") 

######## INITIAL MERGING AND CLEANING #########

# Constructing a Long Merged Dataframe 
long <- admissions %>% # Start with Admissions
  left_join(patients , by = "subject_id")

# Fixing Empty Records and Merging Unknowns
long <- long %>%
  mutate(deathtime = case_when(
    deathtime == "" ~ NA,
    TRUE ~ deathtime),
    dod = case_when(
      dod == "" ~ NA,
      TRUE ~ dod))

categories <- c(
  "race", "admission_type", "admission_location", "discharge_location",
  "insurance", "language", "marital_status"
)

unknowns <- c(
  "unknown", "unable to obtain", "other"
)

pattern <- paste0("\\b(", paste(unknowns, collapse = "|"), ")\\b")

categories <- c(
  "race", "admission_type", "admission_location", "discharge_location",
  "insurance", "language", "marital_status"
)

unknowns <- c(
  "unknown", "unable to obtain", "other"
)

pattern <- paste0("\\b(", paste(unknowns, collapse = "|"), ")\\b")

for (col in categories) {
  
  x <- as.character(long[[col]])
  
  idx_unknown <- grepl(pattern, tolower(trimws(x)))
  idx_empty   <- trimws(x) == ""
  idx_na      <- is.na(x)
  
  # Replace all unknown/empty/NA with "Unknown/Other"
  x[idx_unknown | idx_empty | idx_na] <- "Unknown/Other"
  
  # Put it back into the data frame
  long[[col]] <- x
}


diagnoses <- diagnoses %>%
  mutate(ccs_code = case_when(
    ccs_code == "" ~ "Unknown_CCS",
    TRUE ~ ccs_code
    )
  )

# Setting up Categorical Features
long <- long %>% # Setting Gender as Numeric
  mutate(gender = case_when(
    gender == "M" ~ 0,
    gender == "F" ~ 1
  ))

# Parsing time data in lubridate
dates <- c( # Date type Features
  "admittime", "dischtime", "deathtime", "dod", 
  "anchor_year", "edregtime", "edouttime"
)

long <- long %>%
  mutate(across(all_of(dates),
      ~ parse_date_time(trimws(.),
      orders = c("Y", "ymd", "ymd HMS"), exact = FALSE)))

######## FEATURE ENGINEERING #########

# Removing discharge location as it provides immediate data leakage
long <- long

# Reducing Cardinality of Categorical Features
bin_rare_categories <- function(df, cols, threshold = 0.01) {
  n <- nrow(df)
  
  for (col in cols) {
    freq_table <- table(df[[col]])
    rare_levels <- names(freq_table[freq_table < threshold * n])
    
    df[[col]] <- ifelse(df[[col]] %in% rare_levels, "Unknown/Other", as.character(df[[col]]))
  }
  
  return(df)
}

threshold <- 0.01  # 1%
long <- bin_rare_categories(long, categories, threshold)

# Calculating Patient's Current Age
long <- long %>%
  mutate(age = year(admittime) - year(anchor_year) + anchor_age)

# Calculating Admissions Length of Stay Features
long <- long %>%
  mutate(LoS = # in-hospital LoS in DAYS
           as.numeric(dischtime - admittime, units = "days")) %>%
  mutate(ER_LoS = # ER LoS in HOURS
           case_when(
             is.na(edregtime) ~ NA,
             TRUE ~ as.numeric(edouttime - edregtime, units = "days")
           )
         )

# Calculating Patient Admission Features
long <- long %>%
  group_by(subject_id) %>%
  arrange(admittime, .by_group = TRUE) %>%
  mutate(admission_no = row_number(), # Create admission number
  days_since_discharge = # Days since discharged
    as.numeric(admittime - lag(dischtime), unit = "days") 
  ) %>%
  ungroup()

# Calculating Days until Death
long <- long %>%
  mutate(
    days_until_death = case_when(
      !is.na(dod)        ~ as.numeric(dod - admittime, unit = "days"),
      !is.na(deathtime)  ~ as.numeric(deathtime - admittime, unit = "days"),
      TRUE               ~ NA_real_
    )
  )

######## EMBEDDING CCS CODES #########

# Assigning ccs codes to integers
ccs_levels <- sort(unique(diagnoses$ccs_code))
diagnoses$ccs_index <- as.integer(factor(diagnoses$ccs_code, levels = ccs_levels)) - 1

# Initialize random embedding matrix
num_codes <- length(ccs_levels)
embedding_dim <- 30
#set.seed(67)
embedding_matrix <- matrix(runif(num_codes * embedding_dim, -0.1, 0.1),
                           nrow = num_codes, ncol = embedding_dim)
rownames(embedding_matrix) <- ccs_levels

# Creating Embedded CCS codes
ccs_embeddings <- diagnoses %>%
  group_by(hadm_id) %>%  # one group per admission
  summarise(
    embedding = list(colMeans(embedding_matrix[as.character(ccs_code), , drop = FALSE])),
    .groups = "drop"
  ) %>%
  unnest_wider(embedding, names_sep = "_") %>%
  rename_with(~ paste0("ccs_emb_", seq_along(.)), -hadm_id)

######## PIVOTING LONG TO WIDE  #########

# Create multi-hot / one-hot encoded columns
wide <- long %>%
  dummy_cols(select_columns = categories, 
             remove_selected = TRUE,  # Remove original categorical columns
             remove_first_dummy = FALSE)  # Keep all categories

# Incorporating CCS embeddings
wide <- wide %>%
  right_join(ccs_embeddings, by = "hadm_id")

######## TRAIN/TEST SPLIT AND SAVE  #########

# Removing Features
features_to_remove <- c(
  "admittime", "dischtime", "edregtime", "edouttime", "deathtime",
  "anchor_age", "anchor_year_group", "anchor_year", "admit_provider_id", "dod"
)
wide <- wide %>% select(-all_of(features_to_remove))

# One row per patient with label
patient_labels <- wide %>%
  group_by(subject_id) %>%
  summarise(hospital_expire_flag = max(hospital_expire_flag), .groups = "drop")

# Stratified patient split
train_patients <- patient_labels %>%
  group_by(hospital_expire_flag) %>%
  sample_frac(0.8) %>%
  pull(subject_id)

# Performing Split
train <- wide %>% filter(subject_id %in% train_patients)
test  <- wide %>% filter(!subject_id %in% train_patients)

# Writing to CSV file
write_csv(train, file = "data/train.csv")
write_csv(test, file = "data/test.csv")