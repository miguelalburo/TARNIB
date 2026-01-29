
# Setup  -------------------------------------------------------------------

rm(list = ls())

library(tidyverse) # da GOAT
library(Rtsne) # t-SNE 
library(factoextra) # clusters
library(ggpubr) # additional ggplot
library(EnhancedVolcano) # volcano plot
library(pheatmap) # heatmap

set.seed(67)


# Load Data ----------------------------------------------------------------

train <- read_csv(file = "data/train.csv") 
test <- read_csv(file = "data/test.csv")
embedded <- rbind(train,test)

train_wide <- read_csv(file = "data/train_wide.csv") 
test_wide <- read_csv(file = "data/test_wide.csv")
wide <- rbind(train_wide,test_wide)

merged <- read_csv(file = "data/merged.csv")

# wide <- embedded # TEST IF EMBEDDINGS HELP


# Normal/Poisson Histograms -----------------------------------------------
# Features which we should assume are normal or poisson adjacent

##### Per-admission #####
normals <- c("LoS", "ER_LoS", "days_since_discharge")
p.normals <- list()

for (f in normals) {
  df <- merged[merged[[f]] > 0,] %>% filter(!is.na(f))

  upper <- max(df[[f]], 0.95)
  
  p <- ggplot(df, aes(x = .data[[f]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", boundary = 0) +
    coord_cartesian(xlim = c(NA, upper)) +
    labs(title = paste("Histogram of", f), x = str_replace_all(f, "_", " "), y = "Count") +
    theme_bw()
  
  p.normals[[f]] <- p
}


##### Per-patient #####
index = 1
patients <- merged$subject_id
tot_admissions <- rep(NULL, length(patients))
ages <- rep(NULL, length(patients))

for (patient in patients) {
  subset <- merged[merged$subject_id == patient,]
  ages[index] <- max(subset[["age"]])
  tot_admissions[index] <- max(subset[["admission_no"]])
  
  index <- index + 1
}

p.normals[["admission_no"]] <- ggplot(tibble(x = tot_admissions), aes(x = x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", boundary = 0) +
  coord_cartesian(xlim = c(NA, upper)) +
  labs(title = "Histogram of per-patient admissions", x = "Admissions", y = "Count") +
  theme_bw()

p.normals[["age"]] <- ggplot(tibble(x = ages), aes(x = x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", boundary = 0) +
  coord_cartesian(xlim = c(NA, upper)) +
  labs(title = "Histogram of patient ages", x = "Age", y = "Count") +
  theme_bw()


# Socioeconomic Bars ------------------------------------------------------

p.dem <- list()

###### Economic Status #####

eco <- c("insurance", "admission_location", "marital_status")

for (f in eco) {
  
  # Count frequencies
  dat <- merged %>%
    count(!!sym(f)) %>%
    arrange(desc(n))
  top2 <- dat[[f]][1:2]
  
  # Collapse other categories into "Other"
  plot_data <- merged %>%
    mutate(main_cat = if_else(.data[[f]] %in% top2, 
                         as.character(.data[[f]]), 
                         "Other")) %>%
    mutate(sub_cat = if_else(main_cat == "Other", 
                        as.character(.data[[f]]), 
                        as.character(main_cat)))
  
  # Make factors so top categories come first
  plot_data$main_cat <- factor(plot_data$main_cat, levels = c(top2, "Other"))
  plot_data$sub_cat <- factor(plot_data$sub_cat)
  
  # Create stacked bar
  p.dem[[f]] <- ggplot(plot_data, aes(x = main_cat, fill = sub_cat)) +
    geom_bar() +
    labs(title = paste("Distribution of", f),
         x = "",
         y = "Count",
         fill = str_replace_all(f, "_", " ")) +
         theme_bw()
}

##### Socio Status #####

socio <- c("race", "language")

for (f in socio) {
  
  # Count frequencies
  dat <- merged %>%
    count(!!sym(f)) %>%
    arrange(desc(n))
  
  # Top 1 category
  top1 <- dat[[f]][1]
  
  # Collapse other categories into "Other"
  plot_data <- merged %>%
    mutate(main_cat = if_else(.data[[f]] == top1, 
                         as.character(.data[[f]]), 
                         "Other"),
           sub_cat = if_else(main_cat == "Other",
                         as.character(.data[[f]]),
                         as.character(main_cat)))
  
  # Make factors so top category comes first
  plot_data$main_cat <- factor(plot_data$main_cat, levels = c(top1, "Other"))
  plot_data$sub_cat <- factor(plot_data$sub_cat)
  
  # Create stacked bar
  p.dem[[f]] <- ggplot(plot_data, aes(x = main_cat, fill = sub_cat)) +
    geom_bar() +
    labs(title = paste("Distribution of", f),
         x = "",
         y = "Count",
         fill = str_replace_all(f, "_", " ")) +
    theme_bw()
}

##### Mortality Distribution #####
p.dem[["hospital_expire_flag"]] <- wide %>%
  mutate(x = as.character(hospital_expire_flag)) %>%
  ggplot(aes(
  x = x)) +
  geom_bar() +
  labs(title = "Distribution of in-hospital mortality per admission",
       x = "",
       y = "Count",
       fill = "Mortality") +
  theme_bw()

p.dem

# Label Correlations ------------------------------------------------------

##### Mortality Boxplots #####

p.box <- list()
boxables <- c("LoS", "ER_LoS", "days_since_discharge", "age")

for (f in boxables) {
  pval <- t.test(merged[[f]] ~ merged$hospital_expire_flag)$p.value %>%
    signif(2)
  
  p.box[[f]] <- merged %>% filter(!is.na(f)) %>%
    ggplot(aes(x = as.factor(hospital_expire_flag),
               fill = as.factor(hospital_expire_flag),
               y = .data[[f]])) +
    geom_jitter(alpha = 0.1, size = 0.2, width = 0.3) +
    geom_boxplot(outlier.shape = NA) +
    scale_fill_manual(values=c("#999999", "red"),labels=c('Survived', 'Died')) +
    labs(title = paste0("Histogram of mortality against ",
                        str_replace_all(f, "_", " "), " (p = ",pval,")"),
         x = "",
         y = str_replace_all(f, "_", " "),
         fill = "Mortality") +
    theme_bw()
}

p.box

##### Length of Stay Scatterplots #####

p.los <- list()
foi <- c("days_since_discharge", "ER_LoS", "age", "admission_no")

for (f in foi){
  p.los[[f]] <- wide %>%
    filter(!is.na(f)) %>% arrange(hospital_expire_flag) %>%
    ggplot(aes(y = LoS, x = .data[[f]],
               color = as.factor(hospital_expire_flag))) +
    geom_jitter(height = 0, width = 1) + 
    scale_color_manual(values=c("#999999", "red"),labels=c('Survived', 'Died')) +
    labs(title = paste0("Scatterplot showing Length of Stay against ",
                        str_replace_all(f, "_", " ")),
         y = "Length of Stay (days)",
         x = str_replace(f, "_", " "),
         color = "Mortality") +
    theme_bw()
}

p.los


# Unsupervised Learning and Dimensionality Reduction ---------------------------------------------------

p.dr <- list()

# softmax scaling for vis
softmax <- function(x, temperature = 1) { 
  x <- x / temperature
  ex <- exp(x - max(x, na.rm = TRUE))  # stability trick
  ex / sum(ex, na.rm = TRUE)
}

## Features to remove
remove <- c("ER_LoS", "days_until_death", "days_since_discharge", "hadm_id", 
            "subject_id", "hospital_expire_flag")

## Unsupervised Hierarchical Clustering
hclusters <- wide %>%
  select(!any_of(remove)) %>%
  dist(method = "euclidean") %>%
  hclust(method = "complete") %>%
  cutree(k = 4)

##### PCA #####

pca <- wide %>%
  mutate(hospital_expire_flag = as.integer(hospital_expire_flag)) %>%
  select(!all_of(remove)) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  broom::augment(wide) %>%
  mutate(cluster = hclusters)

p.dr[["pca"]] <- pca %>%
  arrange(hospital_expire_flag) %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, 
             color = as.factor(hospital_expire_flag),
             alpha = as.factor(hospital_expire_flag))) +
  geom_point(show.legend = c("alpha" = F, "color" = T)) +
  scale_alpha_discrete(range=c(0.4, 1)) +
  scale_color_manual(values=c("#999999", "red"),labels=c('Survived', 'Died')) +
  labs(title = "PCA Visualisation",
       x = "PC1", y = "PC2", color = "Mortality") +
  theme_bw()

##### t-SNE #####

tsne <- wide %>%
  select(!all_of(remove)) %>%
  scale() %>%
  Rtsne(dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500) %>%
  {data.frame(Dim1 = .$Y[,1], Dim2 = .$Y[,2])} %>%
  cbind(wide) %>%
  mutate(cluster = hclusters)

p.dr[["tsne"]] <- tsne %>%
  arrange(hospital_expire_flag) %>%
  mutate(size_sm = softmax(age, temperature = 0.5),
         size_plot = scales::rescale(size_sm, to = c(2, 10))) %>%
  ggplot(aes(x = Dim1, y = Dim2, 
             color = as.factor(hospital_expire_flag),
             alpha = as.factor(hospital_expire_flag),
             size = size_plot)) +
  geom_point(inherit.aes = T, show.legend = c("alpha" = F, "color" = T, size = F)) +
  scale_alpha_discrete(range=c(0.4, 1)) +
  scale_color_manual(values=c("#999999", "red"),labels=c('Survived', 'Died')) +
  labs(title = "t-SNE visualisation",
       x = "Dim 1", y = "Dim 2", color = "Mortality") +
  theme_bw()

# Attempted DIfferential Expression ---------------------------------------


ccs_codes <- wide %>%
  select(starts_with("'")) %>%
  colnames()

control <- wide$hospital_expire_flag == 0

# Initialize results
index = 1
results <- data.frame(
  ccs = ccs_codes,
  log2FC = NA,
  p_value = NA
)

for (ccs in ccs_codes) {
  ccs_counts <- wide[[ccs]]
  
  # Compute mean per group
  mean_control <- mean(ccs_counts[control])
  mean_treat   <- mean(ccs_counts[!control])
  
  # Log2 fold change
  results$log2FC[index] <- log2(mean_treat + 1) - log2(mean_control + 1)
  
  # t-test
  test <- t.test(ccs_counts[control], 
                 ccs_counts[!control])
  
  results$p_value[index] <- test$p.value
  index <- index + 1
}

# Adjust for multiple testing (FDR)
results$padj <- p.adjust(results$p_value, method = "BH")

# Sort by adjusted p-value
results <- results[order(results$padj), ]
results %>% head

##### Volcano Plot #####
EnhancedVolcano(results,
                lab = results$ccs,
                x = 'log2FC',
                y = 'padj',
                xlim = c(-1, 1),
                pCutoff = 10e-32,
                FCcutoff = 0.4)

##### Heatmap #####
size <- 1000 # > number of mortalities
mortalities <- sum(merged$hospital_expire_flag == 1)
pal7 <- c(
  "black",
  "red",
  "red",
  "orange",
  "orange",
  "yellow",
  "yellow"
)
breaks <- seq(0, 6, length.out = 8)

mat <- wide %>%
  arrange(hospital_expire_flag) %>%
  slice_head(n = size) %>%
  select(all_of(ccs_codes)) %>%
  as.matrix() %>% t()

pheatmap(mat, cluster_cols = F, 
         gaps_col = mortalities,
         color = pal7,
         breaks = breaks,
         show_rownames = F, show_colnames = F)


# Saving Plots ------------------------------------------------------------

save_plot_lists <- function(plot_lists, out_dir = "results/plots",
                            width = 7, height = 5, dpi = 300) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  for (i in seq_along(plot_lists)) {
    
    plot_group <- plot_lists[[i]]
    group_name <- names(plot_lists)[i]
    
    if (is.null(group_name)) {
      group_name <- paste0("group_", i)
    }
    
    for (j in seq_along(plot_group)) {
      
      p <- plot_group[[j]]
      plot_name <- names(plot_group)[j]
      
      if (is.null(plot_name)) {
        plot_name <- paste0("plot_", j)
      }
      
      file_name <- file.path(out_dir,
                             paste0(group_name, "_", plot_name, ".png"))
      
      ggsave(
        filename = file_name,
        plot = p,
        width = width,
        height = height,
        dpi = dpi
      )
    }
  }
}


plot_lists <- list(p.normals, p.box, p.dem, p.dr)

all_plots <- list(
  boxplots = p.box,
  demographics = p.dem,
  DR = p.dr,
  histogram = p.normals
)

save_plot_lists(all_plots,
                out_dir = "results/plots/",
                width = 8,
                height = 5)


