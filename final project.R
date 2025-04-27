setwd("C:/Users/ailsa/OneDrive - University of Edinburgh/Laptop/Desktop/Y4 Project/R studio")
library(openxlsx)  
library(readxl)
Baseline <- read.xlsx("Maybe gonna work.xlsx", sheet = 1)
Follow_up <- read.xlsx("Maybe gonna work.xlsx", sheet = 2)

Baseline_subset <- Baseline[, c("ID", "AGE", "SEX", "Average Schisto 1", 
                                "HOOKW1", "TRICH1", "ASCAR1", "FALCIP1", 
                                "OVALE1", "MALIAE1", "HAEM1", "FER1", 
                                "CRP1", "AGP1", "BMI")]

Follow_up_subset <- Follow_up[, c("ID","TREAT", "Average.Schisto.2", "HOOKW2", "TRICH2", "ASCAR2"
                                  ,"FALCIP2", "OVALE2", "MALIAE2", "HAEM2", "FER2", "CRP2", "AGP2")]

merged_data <- merge(Baseline_subset, Follow_up_subset, by = "ID", all.x = TRUE)

treated <- merged_data[merged_data$TREAT %in% c(1, 2, 3), ]
not_treated <- merged_data[!merged_data$TREAT %in% c(1, 2, 3), ]

infection_columns <- c("HOOKW1", "TRICH1", "ASCAR1", "FALCIP1", 
                       "OVALE1", "MALIAE1", "HAEM1")
pzq_groups$BMI <- merged_data$BMI[match(pzq_groups$ID, merged_data$ID)]

pzq_groups$HOOKW1 <- factor(pzq_groups$HOOKW1, levels = c(0, 1), labels = c("no", "yes"))
pzq_groups$TRICH1 <- factor(pzq_groups$TRICH1, levels = c(0, 1), labels = c("no", "yes"))
pzq_groups$ASCAR1 <- factor(pzq_groups$ASCAR1, levels = c(0, 1), labels = c("no", "yes"))
Baseline_subset$infection_count <- rowSums(Baseline_subset[, infection_columns] > 0, na.rm = TRUE)
Baseline_subset$infection_type <- ifelse(Baseline_subset$infection_count == 0, "none",
                                         ifelse(Baseline_subset$infection_count == 1, "single", "multiple")
                                         
infection_cols_followup <- c("HOOKW2", "TRICH2", "ASCAR2", "FALCIP2", 
                                                                      "OVALE2", "MALIAE2", "HAEM2")
Follow_up_subset$infection_count <- rowSums(Follow_up_subset[, infection_cols_followup] > 0, na.rm = TRUE)
Follow_up_subset$infection_type <- ifelse(Follow_up_subset$infection_count == 0, "none",
                                          ifelse(Follow_up_subset$infection_count == 1, "single", "multiple"))

mean_baseline <- mean(Baseline_subset$Average.Schisto.1, na.rm = TRUE)
mean_followup <- mean(Follow_up_subset$Average.Schisto.2, na.rm = TRUE)

merged_data$ERR_individual <- ifelse(merged_data$Average.Schisto.1 > 0,
                                     ((merged_data$Average.Schisto.1 - merged_data$Average.Schisto.2) /
                                        merged_data$Average.Schisto.1) * 100,
                                    NA)

treated <- merged_data[merged_data$TREAT %in% c(1, 2, 3), ]
not_treated <- merged_data[!merged_data$TREAT %in% c(1, 2, 3), ]

merged_data$HAEM1 <- merged_data$HAEM1
cor.test(merged_data$ERR_individual, merged_data$HAEM1, use = "complete.obs")

merged_data$Hb_change <- merged_data$HAEM2 - merged_data$HAEM1
cor.test(merged_data$ERR_individual, merged_data$Hb_change, use = "complete.obs")

treated <- merged_data[merged_data$TREAT %in% c(1, 2, 3), ]
not_treated <- merged_data[!merged_data$TREAT %in% c(1, 2, 3), ]

# List of baseline infection columns
infection_columns <- c("HOOKW1", "TRICH1", "ASCAR1", "FALCIP1", 
                       "OVALE1", "MALIAE1", "HAEM1")

# Count number of infections > 0
merged_data$infection_count <- rowSums(merged_data[, infection_columns] > 0, na.rm = TRUE)

# Categorize as none / single / multiple
merged_data$infection_type <- ifelse(merged_data$infection_count == 0, "none",
                                     ifelse(merged_data$infection_count == 1, "single", "multiple"))

# Convert to factor for modeling
merged_data$infection_type <- factor(merged_data$infection_type, levels = c("none", "single", "multiple"))

model <- lm(Hb_change ~ ERR_individual + infection_count + HAEM1 + AGE, data = merged_data)
summary(model)

pzq_groups <- merged_data[merged_data$TREAT %in% c(1, 2), ]

pzq_groups$anaemia_cat <- with(pzq_groups, ifelse(
  SEX == "2" & HAEM1 < 80, "severe",
  ifelse(SEX == "2" & HAEM1 < 110, "moderate",
         ifelse(SEX == "2" & HAEM1 < 120, "mild", 
                ifelse(SEX == "2", "normal",
                       
                       ifelse(SEX == "1" & HAEM1 < 80, "severe",
                              ifelse(SEX == "1" & HAEM1 < 110, "moderate",
                                     ifelse(SEX == "1" & HAEM1 < 130, "mild", "normal")))))))
)

# Turn into an ordered factor (optional, for plots/models)
pzq_groups$anaemia_cat <- factor(pzq_groups$anaemia_cat, 
                                 levels = c("severe", "moderate", "mild", "normal"),
                                 ordered = TRUE)

geohel_cols <- c("HOOKW1", "TRICH1", "ASCAR1")

pzq_groups$geohelminth_count <- rowSums(pzq_groups[, geohel_cols] > 0, na.rm = TRUE)

pzq_groups$geohelminth_cat <- cut(pzq_groups$geohelminth_count,
                                  breaks = c(-1, 0, 1, 2, 3, 4, 5),
                                  labels = c("none", "1", "2", "3", "4", "5"),
                                  right = TRUE)

pzq_groups$ERR_individual <- ifelse(
  pzq_groups$Average.Schisto.1 > 0,
  ((pzq_groups$Average.Schisto.1 - pzq_groups$Average.Schisto.2) / pzq_groups$Average.Schisto.1) * 100,
  NA  # Avoid division by 0
)

pzq_groups$anaemic_bin <- ifelse(
  (pzq_groups$SEX == "2" & pzq_groups$HAEM1 < 120) |
    (pzq_groups$SEX == "1" & pzq_groups$HAEM1 < 130),
  "anaemic", "non_anaemic"
)


model <- lm(ERR_individual ~ anaemia_cat * geohelminth + TREAT, data = pzq_groups)
summary(model)

model <- lm(ERR_individual ~ anaemia_cat + geohelminth_count + TREAT, data = pzq_groups)
summary(model)

model_all <- lm(ERR_individual ~ HOOKW1 + TRICH1 + ASCAR1 + anaemia_cat + TREAT, data = pzq_groups)


pzq_groups$has_hookworm <- ifelse(pzq_groups$HOOKW1 > 0, "yes", "no")
pzq_groups$has_trichuris <- ifelse(pzq_groups$TRICH1 > 0, "yes", "no")
pzq_groups$has_ascaris <- ifelse(pzq_groups$ASCAR1 > 0, "yes", "no")

pzq_groups$has_hookworm <- factor(pzq_groups$has_hookworm)
pzq_groups$has_trichuris <- factor(pzq_groups$has_trichuris)
pzq_groups$has_ascaris <- factor(pzq_groups$has_ascaris)

model <- lm(ERR_individual ~ has_hookworm + has_trichuris + has_ascaris + anaemia_cat + TREAT, 
            data = pzq_groups)
summary(model)


pzq_groups$anaemic_bin <- ifelse(
  (pzq_groups$SEX == "2" & pzq_groups$HAEM1 < 120) |
    (pzq_groups$SEX == "1" & pzq_groups$HAEM1 < 130),
  "anaemic", "non_anaemic"
)

# Make it a factor (important for regression)
pzq_groups$anaemic_bin <- factor(pzq_groups$anaemic_bin)

model_binary <- lm(ERR_individual ~ has_hookworm + has_trichuris + has_ascaris +
                     anaemic_bin + TREAT, data = pzq_groups)

summary(model_binary)


#looking at anaemia effect on PZQ

t.test(ERR_individual ~ anaemic_bin, data = pzq_groups)

# looking at parasite burden effect on PZQ 

cor.test(pzq_groups$HOOKW1, pzq_groups$HAEM1, use = "complete.obs")
cor.test(pzq_groups$TRICH1, pzq_groups$HAEM1, use = "complete.obs")
cor.test(pzq_groups$ASCAR1, pzq_groups$HAEM1, use = "complete.obs")

pzq_groups$ascaris_burden <- cut(pzq_groups$ASCAR1,
                                 breaks = c(-Inf, 0, 4999, 49999, Inf),
                                 labels = c("none", "light", "moderate", "heavy"),
                                 right = TRUE)

pzq_groups$trichuris_burden <- cut(pzq_groups$TRICH1,
                                   breaks = c(-Inf, 0, 999, 9999, Inf),
                                   labels = c("none", "light", "moderate", "severe"),
                                   right = TRUE)

pzq_groups$schisto_burden <- cut(pzq_groups$`Average.Schisto.1`,
                                 breaks = c(-Inf, 0, 49, Inf),
                                 labels = c("none", "light", "heavy"),
                                 right = TRUE)

pzq_groups$malaria_burden <- cut(pzq_groups$FALCIP1,  # or the right malaria column
                                 breaks = c(-Inf, 9, 49, 250, Inf),
                                 labels = c("low", "moderate", "high", "severe"),
                                 right = TRUE)

pzq_groups$hookworm_burden <- cut(pzq_groups$HOOKW1,
                                  breaks = c(-Inf, 0, 1999, 3999, Inf),
                                  labels = c("none", "light", "moderate", "heavy"),
                                  right = TRUE)

boxplot(HAEM1 ~ hookworm_burden, data = pzq_groups,
        main = "Hemoglobin by Hookworm Burden",
        xlab = "Hookworm Burden",
        ylab = "Hemoglobin (HAEM1)",
        col = "lightgreen")

anova_model <- aov(HAEM1 ~ hookworm_burden, data = pzq_groups)
summary(anova_model)

anova_model <- aov(HAEM1 ~ malaria_burden, data = pzq_groups)
summary(anova_model)

anova_model <- aov(HAEM1 ~ schisto_burden, data = pzq_groups)
summary(anova_model)

anova_model <- aov(HAEM1 ~ trichuris_burden, data = pzq_groups)
summary(anova_model)

anova_model <- aov(HAEM1 ~ ascaris_burden, data = pzq_groups)
summary(anova_model)

summary(aov(ERR_individual ~ hookworm_burden, data = pzq_groups))
TukeyHSD(aov(ERR_individual ~ ascaris_burden, data = pzq_groups))
model <- lm(ERR_individual ~ ascaris_burden + AGE + SEX, data = pzq_groups)
summary(model)


summary(aov(ERR_individual ~ malaria_burden, data = pzq_groups))
summary(aov(ERR_individual ~ schisto_burden, data = pzq_groups))
summary(aov(ERR_individual ~ trichuris_burden, data = pzq_groups))
summary(aov(ERR_individual ~ ascaris_burden, data = pzq_groups))


model_interaction <- lm(ERR_individual ~ ascaris_burden * anaemic_bin + AGE + SEX, data = pzq_groups)
summary(model_interaction)

t.test(ASCAR1 ~ anaemic_bin, data = pzq_groups)

table(pzq_groups$ascaris_burden, pzq_groups$anaemic_bin)
fisher.test(table(pzq_groups$ascaris_burden, pzq_groups$anaemic_bin))

table(pzq_groups$hookworm_burden, pzq_groups$anaemic_bin)
fisher.test(table(pzq_groups$hookworm_burden, pzq_groups$anaemic_bin))

table(pzq_groups$malaria_burden, pzq_groups$anaemic_bin)
fisher.test(table(pzq_groups$malaria_burden, pzq_groups$anaemic_bin))

table(pzq_groups$schisto_burden, pzq_groups$anaemic_bin)
fisher.test(table(pzq_groups$schisto_burden, pzq_groups$anaemic_bin))

table(pzq_groups$trichuris_burden, pzq_groups$anaemic_bin)
fisher.test(table(pzq_groups$trichuris_burden, pzq_groups$anaemic_bin))


model_anaemia_err <- lm(ERR_individual ~ anaemic_bin, data = pzq_groups)
summary(model_anaemia_err)

model_anaemia_adj <- lm(ERR_individual ~ anaemic_bin + AGE + SEX, data = pzq_groups)
summary(model_anaemia_adj)

summary(pzq_groups$CRP1)
summary(pzq_groups$AGP1)

model_crp_agp <- lm(ASCAR1 ~ CRP1 + AGP1, data = pzq_groups)
summary(model_crp_agp)

model_crp_agp <- lm(HOOKW1 ~ CRP1 + AGP1, data = pzq_groups)
summary(model_crp_agp)

model_crp_agp <- lm(TRICH1 ~ CRP1 + AGP1, data = pzq_groups)
summary(model_crp_agp)

model_crp_agp <- lm(Average.Schisto.1 ~ CRP1 + AGP1, data = pzq_groups)
summary(model_crp_agp)

model_crp_agp <- lm(FALCIP1 ~ CRP1 + AGP1, data = pzq_groups)
summary(model_crp_agp)

model_inflam_adj <- lm(ERR_individual ~ CRP1 + AGP1 + AGE + SEX, data = pzq_groups)
summary(model_inflam_adj)

model_full <- lm(ERR_individual ~ CRP1 + AGP1 + ascaris_burden * anaemic_bin + AGE + SEX, data = pzq_groups)

pzq_groups$treatment_failure <- ifelse(pzq_groups$ERR_individual < 90, 1, 0)
table(pzq_groups$treatment_failure)

model_malaria_adj <- glm(treatment_failure ~ FALCIP1 + AGE + SEX + anaemic_bin, data = pzq_groups, family = "binomial")
summary(model_malaria_adj)

exp(coef(model_malaria_adj))

#age groups 

pzq_groups$age_group <- cut(pzq_groups$AGE,
                            breaks = c(-Inf, 9, 14, Inf),
                            labels = c("<10", "10–14", "15+"))

summary(aov(ERR_individual ~ age_group, data = pzq_groups))

#co-infections 

pzq_groups$has_ascaris   <- as.numeric(pzq_groups$ASCAR1 > 0)
pzq_groups$has_trichuris <- as.numeric(pzq_groups$TRICH1 > 0)
pzq_groups$has_hookworm  <- as.numeric(pzq_groups$HOOKW1 > 0)
pzq_groups$has_schisto   <- as.numeric(pzq_groups$Average.Schisto.1 > 0)
pzq_groups$has_malaria   <- as.numeric(pzq_groups$FALCIP1 > 0)

pzq_groups$coinfection_count_all <- pzq_groups$has_ascaris +
  pzq_groups$has_trichuris +
  pzq_groups$has_hookworm +
  pzq_groups$has_schisto +
  pzq_groups$has_malaria

summary(lm(ERR_individual ~ coinfection_count_all + AGE + SEX, data = pzq_groups))

model_success_all <- glm(
  treatment_success ~ coinfection_count_all + AGE + SEX,
  data = pzq_groups,
  family = "binomial"
)
summary(model_success_all)

exp(coef(model_success_all))

#anaemia and burden 

aov_model <- aov(coinfection_count_all ~ anaemia_cat, data = pzq_groups)
summary(aov_model)

TukeyHSD(aov_model)

table(pzq_groups$anaemia_cat, pzq_groups$SEX)
pzq_groups <- pzq_groups %>%
  mutate(anaemia_cat = case_when(
    SEX == 1 & HAEM1 < 80 ~ "severe",                             # Males
    SEX == 1 & HAEM1 >= 80 & HAEM1 < 110 ~ "moderate",
    SEX == 1 & HAEM1 >= 110 & HAEM1 < 130 ~ "mild",
    SEX == 2 & HAEM1 < 80 ~ "severe",                             # Females
    SEX == 2 & HAEM1 >= 80 & HAEM1 < 110 ~ "moderate",
    SEX == 2 & HAEM1 >= 110 & HAEM1 < 120 ~ "mild",
    TRUE ~ "non_anaemic"
  ))

aov_model <- aov(coinfection_count_all ~ anaemia_cat, data = pzq_groups)
summary(aov_model)
library(dplyr)

pzq_groups <- pzq_groups %>%
  mutate(
    infection_combo = case_when(
      has_malaria == 1 & (has_ascaris + has_trichuris + has_schisto) > 0 ~ "malaria+helminth",
      has_malaria == 1 & (has_ascaris + has_trichuris + has_schisto) == 0 ~ "malaria_only",
      has_malaria == 0 & (has_ascaris + has_trichuris + has_schisto) > 0 ~ "helminth_only",
      has_malaria == 0 & (has_ascaris + has_trichuris + has_schisto) == 0 ~ "none"
    )
  )

table(pzq_groups$anaemia_cat, pzq_groups$infection_combo)
fisher.test(table(pzq_groups$anaemia_cat, pzq_groups$infection_combo))

table(pzq_groups$anaemic_bin, pzq_groups$infection_combo)
fisher.test(table(pzq_groups$anaemic_bin, pzq_groups$infection_combo))

model_adj <- lm(ERR_individual ~ infection_combo + AGE + SEX, data = pzq_groups)
summary(model_adj)

table(pzq_groups$ascaris_burden)

model_err <- lm(ERR_individual ~ malaria_burden + schisto_burden + AGE + SEX, data = pzq_groups)
summary(model_err)

library(nnet)

model_malaria <- multinom(malaria_burden ~ anaemic_bin + AGE + SEX +
                            coinfection_count_all + CRP1 + AGP1 + FER1,
                          data = pzq_groups)
summary(model_malaria)
table(pzq_groups$malaria_burden)

pzq_groups$malaria_burden <- droplevels(pzq_groups$malaria_burden)

z_malaria <- summary(model_malaria)$coefficients / summary(model_malaria)$standard.errors
p_values_malaria <- 2 * (1 - pnorm(abs(z_malaria)))
print(p_values_malaria)

model_schisto <- multinom(schisto_burden ~ anaemic_bin + AGE + SEX +
                            coinfection_count_all + CRP1 + AGP1 + FER1,
                          data = pzq_groups)

summary(model_schisto)

z_schisto <- summary(model_schisto)$coefficients / summary(model_schisto)$standard.errors
p_values_schisto <- 2 * (1 - pnorm(abs(z_schisto)))
print(p_values_schisto)

model_schisto_crp <- multinom(schisto_burden ~ CRP1, data = pzq_groups)
summary(model_schisto_crp)

z_schisto <- summary(model_schisto_crp)$coefficients / summary(model_schisto_crp)$standard.errors
p_values_schisto <- 2 * (1 - pnorm(abs(z_schisto)))
print(p_values_schisto)

summary(model_schisto_crp) 

z_schisto_crp <- summary(model_schisto_crp)$coefficients / summary(model_schisto_crp)$standard.errors
p_values_schisto_crp <- 2 * (1 - pnorm(abs(z_schisto_crp)))

z_schisto_crp <- summary(model_schisto_crp)$coefficients / summary(model_schisto_crp)$standard.errors
p_values_schisto_crp <- 2 * (1 - pnorm(abs(z_schisto_crp)))
print(p_values_schisto_crp)

# AGP1 model
model_schisto_agp <- multinom(schisto_burden ~ AGP1, data = pzq_groups)

# Summary
summary(model_schisto_agp)

# Z-scores and p-values
z_schisto_agp <- summary(model_schisto_agp)$coefficients / summary(model_schisto_agp)$standard.errors
p_values_schisto_agp <- 2 * (1 - pnorm(abs(z_schisto_agp)))
print(p_values_schisto_agp)

# FER1 model
model_schisto_fer <- multinom(schisto_burden ~ FER1, data = pzq_groups)

# Summary
summary(model_schisto_fer)

# Z-scores and p-values
z_schisto_fer <- summary(model_schisto_fer)$coefficients / summary(model_schisto_fer)$standard.errors
p_values_schisto_fer <- 2 * (1 - pnorm(abs(z_schisto_fer)))
print(p_values_schisto_fer)

model_schisto_combo <- multinom(schisto_burden ~ CRP1 + AGP1 + FER1, data = pzq_groups)
summary(model_schisto_combo)

z_combo <- summary(model_schisto_combo)$coefficients / summary(model_schisto_combo)$standard.errors
p_values_combo <- 2 * (1 - pnorm(abs(z_combo)))
print(p_values_combo)

# Create adjusted ferritin based on CRP and AGP levels
pzq_groups$FER_adj <- with(pzq_groups, FER1)  # Start with unadjusted FER1

# Adjust for both elevated CRP (>5) and AGP (>1)
pzq_groups$FER_adj[pzq_groups$CRP1 > 5 & pzq_groups$AGP1 > 1] <- 
  pzq_groups$FER1[pzq_groups$CRP1 > 5 & pzq_groups$AGP1 > 1] * 0.53

# Adjust for only elevated CRP
pzq_groups$FER_adj[pzq_groups$CRP1 > 5 & pzq_groups$AGP1 <= 1] <- 
  pzq_groups$FER1[pzq_groups$CRP1 > 5 & pzq_groups$AGP1 <= 1] * 0.77

# Adjust for only elevated AGP
pzq_groups$FER_adj[pzq_groups$CRP1 <= 5 & pzq_groups$AGP1 > 1] <- 
  pzq_groups$FER1[pzq_groups$CRP1 <= 5 & pzq_groups$AGP1 > 1] * 0.75

pzq_groups <- pzq_groups %>%
  mutate(CRP_cat = case_when(
    CRP1 <= 1 ~ "low",
    CRP1 > 1 & CRP1 <= 3 ~ "moderate",
    CRP1 > 3 & CRP1 <= 10 ~ "high",
    CRP1 > 10 ~ "severe"
  ))

pzq_groups <- pzq_groups %>%
  mutate(AGP_cat = case_when(
    AGP1 < 1 ~ "normal",
    AGP1 >= 1 ~ "inflammation"
  ))

pzq_groups <- pzq_groups %>%
  mutate(FER_cat = case_when(
    FER1 < 15 ~ "iron_deficient",
    FER1 >= 15 & FER1 < 30 ~ "low_stores",
    FER1 >= 30 & FER1 <= 300 ~ "normal",
    FER1 > 300 & FER1 <= 500 ~ "possible_inflammation",
    FER1 > 500 ~ "significant_inflammation"
  ))

table(pzq_groups$FER_cat, pzq_groups$schisto_burden)

pzq_groups <- pzq_groups %>%
  mutate(FER_cat = case_when(
    FER1 < 15 ~ "iron_deficient",
    FER1 >= 15 & FER1 < 300 ~ "adequate",
    FER1 >= 30 & FER1 <= 300 ~ "normal",
    FER1 > 300 ~ "high",
  ))

library(nnet)

model_schisto_cat <- multinom(
  schisto_burden ~ CRP_cat + AGP_cat + FER_cat,
  data = pzq_groups
)

summary(model_schisto_cat)

table(pzq_groups$CRP_cat, pzq_groups$schisto_burden)
table(pzq_groups$AGP_cat, pzq_groups$schisto_burden)
table(pzq_groups$FER_cat, pzq_groups$schisto_burden)

fisher.test(table(pzq_groups$CRP_cat_collapsed, pzq_groups$schisto_burden))

pzq_groups <- pzq_groups %>%
  mutate(CRP_cat_collapsed = case_when(
    CRP1 <= 1 ~ "low",
    CRP1 > 1 & CRP1 <= 10 ~ "moderate",  # includes moderate + high
    CRP1 > 10 ~ "severe"
  ))
pzq_groups$FER_cat_collapsed <- case_when(
    FER1 < 15 ~ "iron_deficient",
    FER1 >= 15 & FER1 < 300 ~ "adequate",
    FER1 >= 300 ~ "high"
  )

model_schisto_collapsed <- multinom(schisto_burden ~ CRP_cat_collapsed + AGP_cat + FER_cat_collapsed, data = pzq_groups)
summary(model_schisto_collapsed)

model_schisto_collapsed <- multinom(schisto_burden ~ CRP_cat_collapsed + AGP_cat + FER_cat, data = pzq_groups)
summary(model_schisto_collapsed)

table(pzq_groups$FER_cat_collapsed)

z_vals <- summary(model_schisto_collapsed)$coefficients / summary(model_schisto_collapsed)$standard.errors
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
print(p_vals)

pzq_groups <- pzq_groups %>%
  mutate(
    CRP_cat_collapsed = case_when(
      CRP1 <= 1 ~ "low",
      CRP1 > 1 & CRP1 <= 10 ~ "elevated",
      CRP1 > 10 ~ "elevated"
    ),
    
    pzq_groups <- pzq_groups %>%
      mutate(
        FER_cat_collapsed = case_when(
          FER1 < 15 ~ "iron_deficient",
          FER1 >= 15 & FER1 <= 300 ~ "adequate",  # merges low stores + normal
          FER1 > 300 ~ "high"
        )
      )
    summary(model_schisto_collapsed)

    model_schisto_collapsed <- multinom(
      formula = schisto_burden ~ CRP_cat_collapsed + AGP_cat + FER_cat_collapsed,
      data = pzq_groups
    )

    # Z-scores
    z_vals <- summary(model_schisto_collapsed)$coefficients / summary(model_schisto_collapsed)$standard.errors
    
    # P-values
    p_vals <- 2 * (1 - pnorm(abs(z_vals)))
    
    # Print p-values
    print(p_vals)


    model_crp <- lm(log(CRP1) ~ parasite_burden + malaria + coinfection + age, data = pzq_groups)

    str(pzq_groups)

    model_crp <- lm(CRP1 ~ parasite_burden + has_malaria + coinfection_cat + AGE, data = pzq_groups)
    summary(model_crp)
  
    model_crp <- lm(CRP1 ~ ascaris_burden + schisto_burden + malaria_burden + AGE, data = pzq_groups)
summary(model_crp)
library(nnet)
pzq_groups$CRP_cat <- factor(pzq_groups$CRP_cat)

model_crp_cat <- multinom(
  CRP_cat ~ ascaris_burden + trichuris_burden + schisto_burden + malaria_burden + hookworm_burden + AGE + SEX,
  data = pzq_groups
)
summary(model_crp_cat)

table(pzq_groups$CRP_cat)

pzq_groups$CRP_binary <- ifelse(pzq_groups$CRP_cat == "low", "low", "elevated")
pzq_groups$CRP_binary <- factor(pzq_groups$CRP_binary)
table(pzq_groups$CRP_binary)


model_crp_bin <- glm(
  CRP_binary ~ ascaris_burden + schisto_burden + malaria_burden + AGE + SEX,
  data = pzq_groups,
  family = "binomial"
)

summary(model_crp_bin)

library(readxl)

  Baseline <- read_excel("Maybe gonna work.xlsx", sheet = "Baseline")
  
  summary(pzq_groups$BMI)
  sum(is.na(pzq_groups$BMI)) 
  
  str(pzq_groups)
  
  
  model_err_all <- lm(
    ERR_individual ~ schisto_burden + ascaris_burden + hookworm_burden + trichuris_burden +
      malaria_burden + BMI + AGE + SEX,
    data = pzq_groups
  )
  summary(model_err_all)
  
  plot(model_err_all)

  model_interaction <- lm(
    ERR_individual ~ ascaris_burden * anaemic_bin + BMI + AGE + SEX,
    data = pzq_groups
  )
  summary(model_interaction)
    
  
  library(splines)
  
  model_spline <- lm(
    ERR_individual ~ ns(AGE, df = 3) + ascaris_burden + BMI + SEX,
    data = pzq_groups
  )
  summary(model_spline)
  
  pzq_groups$age_group <- factor(pzq_groups$age_group)
  
  model_age_cat <- lm(
    ERR_individual ~ age_group + ascaris_burden + BMI + SEX,
    data = pzq_groups
  )
  summary(model_age_cat)  
  
  model_bmi_anaemia <- lm(
    ERR_individual ~ anaemic_bin * BMI + AGE + SEX,
    data = pzq_groups
  )
  summary(model_bmi_anaemia) 
  
  pzq_groups$BMI_cat_adjusted <- with(pzq_groups, ifelse(
    (AGE == 9  & BMI < 14.5) |
      (AGE == 10 & BMI < 14.8) |
      (AGE == 11 & BMI < 15.0) |
      (AGE == 12 & BMI < 15.3) |
      (AGE == 13 & BMI < 15.5) |
      (AGE == 14 & BMI < 16.0) |
      (AGE == 15 & BMI < 16.5),
    "low", "normal"
  ))
  
  table(pzq_groups$BMI_cat_adjusted)
  pzq_groups$BMI_cat_adjusted <- factor(pzq_groups$BMI_cat_adjusted)
  
  
  model_cat_interaction <- lm(
    ERR_individual ~ anaemic_bin * BMI_cat_adjusted + AGE + SEX,
    data = pzq_groups
  )
  summary(model_cat_interaction)
  
  low_bmi <- subset(pzq_groups, BMI_cat_adjusted == "low")
  normal_bmi <- subset(pzq_groups, BMI_cat_adjusted == "normal")
  
  model_low_bmi <- lm(ERR_individual ~ anaemic_bin + AGE + SEX, data = low_bmi)
  summary(model_low_bmi) 
  
  model_normal_bmi <- lm(ERR_individual ~ anaemic_bin + AGE + SEX, data = normal_bmi)
  summary(model_normal_bmi)
  
  table(pzq_groups$treatment_failure)
  
  model_treat_fail <- glm(
    treatment_failure ~ has_malaria + anaemic_bin + coinfection_count + AGE + SEX + BMI,
    data = pzq_groups,
    family = "binomial"
  )
  summary(model_treat_fail)  
  
  exp(coef(model_treat_fail))  # Get odds ratios
  exp(confint(model_treat_fail)) 
  
  exp(coef(model_treat_fail))
  
  model_parasite_specific <- glm(
    treatment_failure ~ has_malaria + has_ascaris + has_hookworm + has_trichuris + has_schisto +
      anaemic_bin + AGE + SEX + BMI,
    data = pzq_groups,
    family = "binomial"
  )
  summary(model_parasite_specific)
  
  exp(coef(model_parasite_specific))  # Odds ratios
  exp(confint(model_parasite_specific))
  
  model_hb <- lm(
    HAEM1 ~ ascaris_burden + hookworm_burden + schisto_burden +
      malaria_burden + AGE + SEX + BMI,
    data = pzq_groups
  )
  summary(model_hb)
  library(MASS)
  model_burden_crp <- polr(
    ascaris_burden ~ CRP1 + AGP1 + AGE + SEX + BMI,
    data = pzq_groups,
    Hess = TRUE
  )
  summary(model_burden_crp)
  
  model_fer_err <- lm(
    ERR_individual ~ FER_adj + AGE + SEX + BMI + ascaris_burden + hookworm_burden + schisto_burden + malaria_burden + trichuris_burden,
    data = pzq_groups
  )
  summary(model_fer_err)
  
  model_fer_burden <- lm(
    FER_adj ~ ascaris_burden + hookworm_burden + schisto_burden + malaria_burden + trichuris_burden + AGE + SEX + BMI,
    data = pzq_groups
  )
  summary(model_fer_burden)
  
  model_anaemia_severity <- lm(
    HAEM1 ~ ascaris_burden + hookworm_burden + schisto_burden + malaria_burden + trichuris_burden +
      AGE + SEX + BMI + FER_adj + CRP1,
    data = pzq_groups
  )
  
  summary(model_anaemia_severity)
  
  model_3way <- lm(
    ERR_individual ~ ascaris_burden * anaemic_bin * age_group,
    data = pzq_groups
  )
  summary(model_3way)
  
  model_coinf_sex <- lm(
    ERR_individual ~ coinfection_count_all * anaemic_bin * SEX,
    data = pzq_groups
  )
  summary(model_coinf_sex)
  
  pzq_groups$high_inflammation <- ifelse(pzq_groups$CRP1 > 5 | pzq_groups$AGP1 > 1, 1, 0)
  
  pzq_groups$fer_status <- with(pzq_groups, ifelse(
    FER_adj < 15, "low",
    ifelse(FER_adj > 500, "inflamed", "normal")
  ))
  pzq_groups$fer_status <- factor(pzq_groups$fer_status, levels = c("normal", "low", "inflamed"))
  
  model_hook_ferritin3way <- lm(
    ERR_individual ~ hookworm_burden * anaemic_bin * fer_status,
    data = pzq_groups
  )
  
  summary(model_hook_ferritin3way)
  
  pzq_groups$hookworm_any <- ifelse(pzq_groups$hookworm_burden == "none", "none", "infected")
  pzq_groups$hookworm_any <- factor(pzq_groups$hookworm_any, levels = c("none", "infected"))
 
   # Collapse ferritin to 2 categories
  pzq_groups$fer_simple <- with(pzq_groups, ifelse(FER_adj < 15, "low", "normal"))
  pzq_groups$fer_simple <- factor(pzq_groups$fer_simple)
  
  model_simple <- lm(
    ERR_individual ~ hookworm_any * anaemic_bin * fer_simple,
    data = pzq_groups
  )
  summary(model_simple) 
  
  table(pzq_groups$hookworm_any, pzq_groups$anaemic_bin, pzq_groups$fer_simple)
  
  hist(pzq_groups$CRP1, breaks = 20) 
  pzq_groups$log_crp <- log10(pzq_groups$CRP1 + 0.01)
  hist(pzq_groups$log_crp, breaks = 20, col = "lightblue")  
  
  model_crp_schisto <- lm(
    ERR_individual ~ schisto_burden * anaemic_bin * log_crp,
    data = pzq_groups
  )
  summary(model_crp_schisto)
  hist(pzq_groups$AGP1, breaks = 20)
  
  pzq_groups$log_agp <- log10(pzq_groups$AGP1 + 0.01)
  
  model_agp_schisto <- lm(
    ERR_individual ~ schisto_burden * anaemic_bin * log_agp,
    data = pzq_groups
  )
  
  summary(model_agp_schisto)
  
  
  model_crp_anaemia <- lm(
    ERR_individual ~ anaemic_bin * log_crp,
    data = pzq_groups
  )
  
  summary(model_crp_anaemia)
  
  
  model_ascaris_fer_age <- lm(
    ERR_individual ~ ascaris_burden * fer_status * age_group,
    data = pzq_groups
  )
  
  summary(model_ascaris_fer_age)
  
  model_coinfx_age <- glm(
    treatment_failure ~ coinfection_count * age_group,
    data = pzq_groups,
    family = "binomial"
  )
  
  summary(model_coinfx_age)
  
  pzq_groups$age_simple <- ifelse(pzq_groups$AGE < 12, "<12", "12+")
  glm(treatment_failure ~ coinfection_count * age_simple, 
      data = pzq_groups, family = "binomial")
  
  summary(model_coinfx_simple)
  exp(coef(model_coinfx_simple)) 
  
  model_coinfx_simple <- glm(
    treatment_failure ~ coinfection_count * age_simple,
    data = pzq_groups,
    family = "binomial"
  )
  
  summary(model_coinfx_simple) 
  
  merged_data$delta_Hb <- merged_data$HAEM2 - merged_data$HAEM1
  merged_data$delta_CRP <- merged_data$CRP2 - merged_data$CRP1
 
  t.test(delta_Hb ~ treated, data = merged_data)
  lm(delta_Hb ~ parasite_burden + treated + AGE + SEX + BMI, data = merged_data)
  
  merged_data$treated <- ifelse(merged_data$TREAT %in% c(1, 2), 1, 0)
  merged_data$treatment_group <- case_when(
    merged_data$TREAT %in% c(1, 2) ~ "treated",
    merged_data$TREAT == 3 ~ "placebo",
    is.na(merged_data$TREAT) ~ "untreated"
  )
  merged_data$treatment_group <- factor(merged_data$treatment_group)
  library(dplyr)
  table(merged_data$treatment_group)
  
  names(merged_data)
  merged_data$delta_Hb <- merged_data$HAEM2 - merged_data$HAEM1
  merged_data$delta_CRP <- merged_data$CRP2 - merged_data$CRP1
  
  # Ascaris
  merged_data$ascaris_burden <- case_when(
    merged_data$ASCAR1 == 0 ~ "none",
    merged_data$ASCAR1 <= 4999 ~ "light",
    merged_data$ASCAR1 <= 49999 ~ "moderate",
    merged_data$ASCAR1 > 49999 ~ "heavy"
  )
  
  # Trichuris
  merged_data$trichuris_burden <- case_when(
    merged_data$TRICH1 == 0 ~ "none",
    merged_data$TRICH1 <= 999 ~ "light",
    merged_data$TRICH1 <= 9999 ~ "moderate",
    merged_data$TRICH1 > 9999 ~ "heavy"
  )
  
  # Hookworm (keep prior cutoffs or define if needed)
  merged_data$hookworm_burden <- case_when(
    merged_data$HOOKW1 == 0 ~ "none",
    merged_data$HOOKW1 <= 999 ~ "light",
    merged_data$HOOKW1 <= 3999 ~ "moderate",
    merged_data$HOOKW1 > 3999 ~ "heavy"
  )
  
  merged_data$anaemia_level <- case_when(
    merged_data$SEX == 1 & merged_data$HAEM1 >= 110 & merged_data$HAEM1 <= 129 ~ "mild",
    merged_data$SEX == 1 & merged_data$HAEM1 >= 80 & merged_data$HAEM1 < 110 ~ "moderate",
    merged_data$SEX == 1 & merged_data$HAEM1 < 80 ~ "severe",
    merged_data$SEX == 2 & merged_data$HAEM1 >= 110 & merged_data$HAEM1 <= 119 ~ "mild",
    merged_data$SEX == 2 & merged_data$HAEM1 >= 80 & merged_data$HAEM1 < 110 ~ "moderate",
    merged_data$SEX == 2 & merged_data$HAEM1 < 80 ~ "severe",
    TRUE ~ "normal"
  )
  merged_data$anaemia_level <- factor(merged_data$anaemia_level, levels = c("normal", "mild", "moderate", "severe"))  
  
  merged_data$coinfection_count <- rowSums(data.frame(
    merged_data$ascaris_burden != "none",
    merged_data$hookworm_burden != "none",
    merged_data$trichuris_burden != "none",
    merged_data$`Average Schisto 1` > 0,  # replace if you prefer a categorical schisto
    merged_data$MALIAE1 > 0 | merged_data$MALIAE2 > 0  # malaria PCR positivity
  ), na.rm = TRUE)
  
  merged_data$age_simple <- case_when(
    merged_data$AGE < 12 ~ "<12",
    merged_data$AGE >= 12 ~ "12+"
  )
  
  merged_data$ERR_schisto <- with(merged_data,
                                  ifelse(!is.na(`Average Schisto 1`) & `Average Schisto 1` > 0,
                                         ((`Average Schisto 1` - `Average.Schisto.2`) / `Average Schisto 1`) * 100,
                                         NA  # can't calculate if baseline = 0
                                  )
  )
  
  # ERR for Ascaris
  merged_data$ERR_ascaris <- with(merged_data,
                                  ifelse(!is.na(ASCAR1) & ASCAR1 > 0,
                                         ((ASCAR1 - ASCAR2) / ASCAR1) * 100,
                                         NA)
  )
  
  # ERR for Trichuris
  merged_data$ERR_trichuris <- with(merged_data,
                                    ifelse(!is.na(TRICH1) & TRICH1 > 0,
                                           ((TRICH1 - TRICH2) / TRICH1) * 100,
                                           NA)
  )
  
  # ERR for Hookworm
  merged_data$ERR_hookworm <- with(merged_data,
                                   ifelse(!is.na(HOOKW1) & HOOKW1 > 0,
                                          ((HOOKW1 - HOOKW2) / HOOKW1) * 100,
                                          NA)
  )
  
  anova_schisto <- aov(ERR_schisto ~ treatment_group, data = merged_data)
  summary(anova_schisto)
  
  TukeyHSD(anova_schisto)

  merged_clean <- merged_data %>%
    filter(!is.na(Average.Schisto.2)) %>%
    filter(complete.cases(ERR_schisto, treatment_group, AGE, SEX, BMI))
  anova_schisto_clean <- aov(ERR_schisto ~ treatment_group, data = merged_clean)
  summary(anova_schisto_clean)
  TukeyHSD(anova_schisto_clean)

  merged_clean %>%
    filter(treatment_group == "untreated") %>%
    summarise(
      baseline_mean = mean(`Average Schisto 1`, na.rm = TRUE),
      followup_mean = mean(Average.Schisto.2, na.rm = TRUE)
    )  
  t.test(
    merged_clean$`Average Schisto 1`[merged_clean$treatment_group == "untreated"],
    merged_clean$`Average.Schisto.2`[merged_clean$treatment_group == "untreated"],
    paired = TRUE
  )
  
  t.test(
    merged_clean$`Average Schisto 1`[merged_clean$treatment_group == "treated"],
    merged_clean$`Average.Schisto.2`[merged_clean$treatment_group == "treated"],
    paired = TRUE
  )
  
  t.test(
    merged_clean$`Average Schisto 1`[merged_clean$treatment_group == "placebo"],
    merged_clean$`Average.Schisto.2`[merged_clean$treatment_group == "placebo"],
    paired = TRUE
  )
  
  untreated_data <- merged_clean %>% 
    filter(treatment_group == "untreated") %>% 
    mutate(delta_schisto = `Average Schisto 1` - `Average.Schisto.2`)
  
  model_untreated <- lm(
    delta_schisto ~ AGE + SEX + BMI + CRP1 + AGP1 + FER1 + coinfection_count + anaemia_level,
    data = untreated_data
  )

  summary(model_untreated)
  
  model_treated <- lm(delta_schisto ~ AGE + SEX + BMI + CRP1 + AGP1 + FER1 + coinfection_count + anaemia_level,
                      data = merged_clean %>% filter(treatment_group == "treated"))
  summary(model_treated)
  
 
  
  merged_clean$delta_schisto <- with(merged_clean, `Average Schisto 1` - `Average.Schisto.2`)  

  model_treated <- lm(delta_schisto ~ AGE + SEX + BMI + CRP1 + AGP1 + FER1 + coinfection_count + anaemia_level,
                      data = merged_clean %>% filter(treatment_group == "treated"))
  summary(model_treated)
  
  merged_clean$anaemic_bin <- with(merged_clean, ifelse(
    (SEX == 2 & AGE >= 12 & HAEM1 < 120) |      # Female ≥12 → <120 g/L
      (SEX == 1 & AGE >= 15 & HAEM1 < 130) |      # Male ≥15 → <130 g/L
      (AGE < 5 & HAEM1 < 110) |                   # <5 years → <110 g/L
      (AGE >= 5 & AGE < 12 & HAEM1 < 115) |       # 5–11 years → <115 g/L
      (AGE >= 12 & AGE < 15 & HAEM1 < 120),       # 12–14 years → <120 g/L
    "anaemic", "non_anaemic"
  ))
  
  merged_clean$anaemic_bin <- factor(merged_clean$anaemic_bin, levels = c("non_anaemic", "anaemic"))
  
  model_interact <- lm(ERR_schisto ~ treatment_group * anaemic_bin, data = merged_clean)
  summary(model_interact) 
  
  model_interact_adjusted <- lm(
    ERR_schisto ~ treatment_group * anaemic_bin + AGE + SEX,
    data = merged_clean
  )
  summary(model_interact_adjusted)
  
  lm(delta_schisto ~ treatment_group * anaemic_bin + AGE + SEX + BMI, data = merged_clean) 
  summary(lm(delta_schisto ~ treatment_group * anaemic_bin + AGE + SEX + BMI, data = merged_clean))  
  
  anova_delta <- aov(delta_schisto ~ treatment_group, data = merged_clean)
  summary(anova_delta) 
  
  TukeyHSD(anova_delta)
  
  model_adj_delta <- lm(
    delta_schisto ~ treatment_group + `Average Schisto 1` + anaemic_bin +
      FER1 + CRP1 + AGP1 + AGE + SEX + BMI,
    data = merged_clean
  )
  summary(model_adj_delta)
  
  summary(model_adj_delta) 
  install.packages("emmeans")
  library(emmeans)
  
  # Run estimated marginal means
  emmeans(model_adj_delta, pairwise ~ treatment_group)
  
  
  
  model_age_sex_interact <- lm(
    delta_schisto ~ treatment_group * AGE + treatment_group * SEX +
      `Average Schisto 1` + anaemic_bin + FER1 + CRP1 + AGP1 + BMI,
    data = merged_clean
  )
  
  summary(model_age_sex_interact)
  library(emmeans)
  emmeans(model_age_sex_interact, pairwise ~ treatment_group | SEX)
  emmeans(
    model_age_sex_interact, 
    pairwise ~ treatment_group | AGE,
    at = list(AGE = c(8, 10, 12, 14))
  )
  
  install.packages("writexl")
  library(writexl)
  
  write_xlsx(merged_clean, "merged_clean_export.xlsx")
  
  install.packages("ggplot2")
  library(ggplot2)
  
  ggplot(merged_clean, aes(x = treatment_group, y = ERR_schisto, fill = treatment_group)) +
    geom_violin(trim = FALSE, alpha = 0.6) +        # violin plot
    geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +  # boxplot overlay
    labs(title = "Figure 1: Egg Reduction Rate (ERR) by Treatment Group",
         x = "Treatment Group",
         y = "Egg Reduction Rate (ERR)") +
    theme_minimal() +
    theme(legend.position = "none")
  
  
  library(ggplot2)
  
  ggplot(merged_clean, aes(x = treatment_group, y = ERR_schisto, fill = treatment_group)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_boxplot(width = 0.15, color = "black", outlier.shape = NA) +
    geom_jitter(width = 0.15, size = 1, alpha = 0.3, color = "gray30") +
    labs(
      title = "Figure 1: Egg Reduction Rate (ERR) by Treatment Group",
      x = "Treatment Group",
      y = "Egg Reduction Rate (%)"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none")
  
  table(merged_clean$treatment_group)

  # Load required packages
  library(dplyr)
  library(broom)
  library(gt)
  
  # Step 1: Filter to keep only 'treated' and 'placebo'
  merged_filtered <- merged_clean %>%
    filter(treatment_group %in% c("treated", "placebo")) %>%
    droplevels()
  
  # Step 2: Run adjusted linear model
  model_filtered <- lm(
    delta_schisto ~ treatment_group + `Average Schisto 1` + anaemic_bin +
      FER1 + CRP1 + AGP1 + AGE + SEX + BMI,
    data = merged_filtered
  )
  
  # Step 3: Tidy results and rename predictors
  tidy_filtered <- tidy(model_filtered) %>%
    mutate(
      term = recode(term,
                    "(Intercept)" = "Intercept",
                    "treatment_grouptreated" = "Treated (vs Placebo)",
                    "`Average Schisto 1`" = "Baseline Schisto",
                    "anaemic_binanaemic" = "Anaemic (vs Non-Anaemic)",
                    "FER1" = "Ferritin", "CRP1" = "CRP",
                    "AGP1" = "AGP", "AGE" = "Age",
                    "SEX" = "Sex (Male = 1)", "BMI" = "BMI"
      )
    )
  
  # Step 4: Display results in a clean table
  tidy_filtered %>%
    gt() %>%
    fmt_number(columns = estimate:statistic, decimals = 2) %>%
    fmt_number(columns = p.value, decimals = 4) %>%
    cols_label(
      term = "Predictor",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "t value",
      p.value = "p-value"
    ) %>%
    tab_header(
      title = "📊 Adjusted Linear Regression: ΔSchisto (Treated vs Placebo)"
    )
  
  library(gt)
  library(dplyr)
  library(broom)
  
  # Identify significance
  tidy_filtered <- tidy_filtered %>%
    mutate(significant = ifelse(p.value < 0.05, TRUE, FALSE))
  
  # Format GT table with bold for significant predictors
  tidy_filtered %>%
    gt() %>%
    fmt_number(columns = estimate:statistic, decimals = 2) %>%
    fmt_number(columns = p.value, decimals = 4) %>%
    cols_label(
      term = "Predictor",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "t value",
      p.value = "p-value"
    ) %>%
    tab_header(
      title = "Adjusted Linear Regression: ΔSchisto (Treated vs Placebo)"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = significant == TRUE
      )
    ) %>%
    cols_hide(columns = significant)
  
  colnames(tidy_filtered)
  
  library(gt)
  
  library(gt)
  install.packages("gtExtras")
  
  fmt_number(columns = p.value, decimals = 4)
  library(gtExtras)
  tidy_filtered %>%
    gt() %>%
    tab_header(
      title = "Adjusted Linear Regression: ΔSchisto (Treated vs Placebo)"
    ) %>%
    fmt_number(columns = estimate:statistic, decimals = 2) %>%
    fmt_pvalue(columns = p.value) %>%
    cols_label(
      term = "Predictor",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "t value",
      p.value = "p-value"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = estimate,
        rows = significant == TRUE
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = term,
        rows = significant == TRUE
      )
    )
  
  library(gt)
  
  tidy_filtered %>%
    gt() %>%
    tab_header(
      title = "Adjusted Linear Regression: ΔSchisto (Treated vs Placebo)"
    ) %>%
    fmt_number(columns = estimate:statistic, decimals = 2) %>%
    fmt_number(columns = p.value, decimals = 4) %>%  # fallback if fmt_pvalue() doesn't work
    cols_label(
      term = "Predictor",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "t value",
      p.value = "p-value"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = estimate,
        rows = significant == TRUE
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = term,
        rows = significant == TRUE
      )
    ) %>%
    # Add row group headers
    tab_row_group(
      label = "Treatment Arm",
      rows = c("Treated (vs Placebo)")
    ) %>%
    tab_row_group(
      label = "Baseline Infection",
      rows = c("Baseline Schisto")
    ) %>%
    tab_row_group(
      label = "Anaemia & Inflammation",
      rows = c("Anaemic (vs Non-Anaemic)", "Ferritin", "CRP", "AGP")
    ) %>%
    tab_row_group(
      label = "Demographics",
      rows = c("Age", "Sex (Male = 1)", "BMI")
    )

  tidy_filtered <- tidy_filtered %>%
    mutate(sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ))  
    
  install.packages("writexl")   # Run this only once
  library(writexl) 
    
  write_xlsx(pzq_groups, "pzq_groups_export.xlsx")

  library(broom)
  
  # Re-run your model (if needed)
  model_burden <- lm(
    ERR_individual ~ schisto_burden + ascaris_burden +
      hookworm_burden + trichuris_burden + malaria_burden +
      BMI + AGE + SEX,
    data = pzq_groups
  )
  
  # Get model summary with 95% Confidence Intervals
  tidy_burden <- tidy(model_burden, conf.int = TRUE, conf.level = 0.95)
  
  # View the result
  print(tidy_burden)  
  
  library(ggplot2)
  ggplot(tidy_burden, aes(x = estimate, y = reorder(term, estimate))) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(x = "Estimate (β)", y = "Predictor", title = "Predictors of ERR (Linear Model)") +
    theme_minimal()
  
  # Re-run logistic regression including BMI
  model_success_bmi <- glm(
    treatment_success ~ coinfection_count_all + AGE + SEX + BMI,
    data = pzq_groups,
    family = "binomial"
  )
  
  # Summary of the model
  summary(model_success_bmi)
  
  # Get odds ratios
  exp(coef(model_success_bmi))
  
  # Re-run the model using ERR_individual as the outcome
  model_ERR_bmi <- lm(
    ERR_individual ~ coinfection_count_all + AGE + SEX + BMI,
    data = pzq_groups
  )
  
  # Summary of the model
  summary(model_ERR_bmi)
  
  library(broom)
  tidy(model_ERR_bmi, conf.int = TRUE, conf.level = 0.95)
  
  library(ggplot2)
  
  ggplot(tidy(model_ERR_bmi, conf.int = TRUE), 
         aes(x = estimate, y = reorder(term, estimate))) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    theme_minimal() +
    labs(
      title = "Predictors of Egg Reduction Rate (ERR)",
      x = "Estimate (β)",
      y = "Predictor"
    )
  
  pzq_groups <- pzq_groups %>%
    mutate(
      schisto = ifelse(Average.Schisto.1 > 0, 1, 0),
      ascaris = ifelse(ascaris_burden != "none", 1, 0),
      hookworm = ifelse(hookworm_burden != "none", 1, 0),
      trichuris = ifelse(trichuris_burden != "none", 1, 0),
      malaria = ifelse(malaria_burden != "low", 1, 0)  # Adjust this based on your coding
    )
  
  pzq_groups <- pzq_groups %>%
    mutate(
      infection_pattern = paste0(
        "S", schisto,
        "_A", ascaris,
        "_H", hookworm,
        "_T", trichuris,
        "_M", malaria
      )
    )
  
  table(pzq_groups$infection_pattern)
  
  model_combo <- lm(delta_schisto ~ infection_combo + AGE + SEX + BMI, data = pzq_groups)
  summary(model_combo)
  
  pzq_groups <- pzq_groups %>%
    mutate(delta_schisto = Average.Schisto.1 - Average.Schisto.2)
  
  model_combo <- lm(delta_schisto ~ infection_pattern + AGE + SEX + BMI, data = pzq_groups)
  summary(model_combo)
  library(dplyr)
  pzq_groups <- pzq_groups %>%
    mutate(
      infection_pattern = case_when(
        schisto_burden > 0 & (ascaris_burden > 0 | hookworm_burden > 0 | trichuris_burden > 0) & malaria_burden > 0 ~ "schisto+geohelminth+malaria",
        schisto_burden > 0 & malaria_burden > 0 ~ "schisto+malaria",
        schisto_burden > 0 & (ascaris_burden > 0 | hookworm_burden > 0 | trichuris_burden > 0) ~ "schisto+geohelminth",
        malaria_burden > 0 & (ascaris_burden > 0 | hookworm_burden > 0 | trichuris_burden > 0) ~ "geohelminth+malaria",
        schisto_burden > 0 ~ "schisto_only",
        malaria_burden > 0 ~ "malaria_only",
        (ascaris_burden > 0 | hookworm_burden > 0 | trichuris_burden > 0) ~ "geohelminth_only",
        TRUE ~ "none"
      )
    )
  
  
  ggplot(pzq_groups, aes(x = coinfection_count_all, y = ERR_individual)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(
      x = "Number of Coinfections",
      y = "Egg Reduction Rate (ERR)",
      title = "Relationship Between Coinfection Count and ERR"
    ) +
    theme_classic()
  
  scale_y_continuous(trans = "sqrt")
  
  model_sqrt <- lm(sqrt(abs(ERR_individual)) ~ coinfection_count_all + AGE + SEX + BMI, 
                   data = pzq_groups)
  summary(model_sqrt)
  
  ggplot(data, aes(x = coinfection_count_all, y = ERR_individual)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    scale_y_sqrt() +  # or scale_y_continuous(trans = "sqrt")
    labs(
      title = "ERR vs Coinfection Count (sqrt-transformed Y)",
      x = "Number of Coinfections",
      y = "ERR (square root scale)"
    ) +
    theme_minimal()
  
  pzq_groups$age_group <- ifelse(pzq_groups$AGE < 12, "<12", "≥12")
  
  model_interact <- lm(ERR_individual ~ coinfection_count_all * age_group + SEX + BMI, 
                       data = pzq_groups)
  summary(model_interact)
  
  ggplot(pzq_groups, aes(x = coinfection_count_all, y = ERR_individual, color = age_group)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      title = "ERR vs Coinfection Count by Age Group",
      x = "Number of Coinfections",
      y = "Egg Reduction Rate (ERR)",
      color = "Age Group"
    ) +
    theme_minimal()
  
  pzq_groups$age_group <- ifelse(pzq_groups$AGE < 12, "<12", "≥12") 
  
  library(dplyr)
  
  pzq_groups %>%
    group_by(coinfection_count_all) %>%
    summarise(
      n = n(),
      mean_ERR = mean(ERR_individual, na.rm = TRUE),
      sd_ERR = sd(ERR_individual, na.rm = TRUE)
    ) 
  
  
  
  filter(pzq_groups, age_group == "<12" & coinfection_count_all == 4) %>%
    select(ERR_individual)
  
  
 pzq_groups %>%
    filter(coinfection_count_all == 4) %>%
    select(ERR_individual)
  
 ggplot(pzq_groups, aes(x = coinfection_count_all, y = ERR_individual, color = age_group)) +
   geom_point(position = position_jitter(width = 0.2), alpha = 0.6, size = 2) +
   geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +
   coord_cartesian(ylim = c(-500, 100)) +
   scale_color_manual(values = c("<12" = "#D55E00", "≥12" = "#0072B2")) +
   labs(
     title = "Decline in PZQ Egg Reduction Rate with Increasing Co-infections",
     subtitle = "Stratified by Age Group (<12 vs ≥12 years)",
     x = "Number of Co-infections",
     y = "Egg Reduction Rate (ERR)",
     color = "Age Group"
   ) +
   theme_minimal(base_size = 14)
 
 library(ggplot2)
 
 ggplot(pzq_groups, aes(x = coinfection_count_all, y = ERR_individual)) +
   geom_jitter(aes(color = age_group), width = 0.2, alpha = 0.4, size = 1.5) +
   geom_smooth(aes(color = age_group), method = "lm", se = TRUE, linetype = "dashed") +
   facet_wrap(~ age_group) +
   scale_y_continuous(limits = c(-100, 120)) +
   scale_color_manual(values = c("<12" = "#F8766D", "≥12" = "#00BFC4")) +
   labs(
     title = "Decline in PZQ Egg Reduction Rate with Increasing Co-infections",
     x = "Number of Co-infections",
     y = "Egg Reduction Rate (ERR)",
     color = "Age Group"
   ) +
   theme_minimal(base_size = 14) +
   theme(
     plot.title = element_text(face = "bold"),
     strip.text = element_text(face = "bold")
   )
 
 pzq_groups %>%
   select(coinfection_count_all, ERR_individual)
 
 library(dplyr)
 
 pzq_groups <- pzq_groups %>%
   mutate(coinfection_group = case_when(
     coinfection_count_all == 1 ~ "1",
     coinfection_count_all == 2 ~ "2",
     coinfection_count_all == 3 ~ "3",
     coinfection_count_all >= 4 ~ "4+"
   )) 
 
 pzq_groups <- pzq_groups %>%
   mutate(coinfection_group = factor(coinfection_group, levels = c("1", "2", "3", "4+")))
 
 library(dplyr)
 
 co_infection_summary_grouped <- pzq_groups %>%
   group_by(coinfection_group) %>%
   summarise(
     mean_ERR = mean(ERR_individual, na.rm = TRUE),
     sd_ERR = sd(ERR_individual, na.rm = TRUE),
     n = n()
   )
 
 print(co_infection_summary_grouped)
 
 pzq_groups %>%
   filter(coinfection_group == "4+") %>%
   select(coinfection_group, ERR_individual)
 
 library(dplyr)
 library(ggplot2)
 
 # Create required binary groups if needed
 data_heatmap <- merged_clean %>%
   mutate(
     ascaris_status = ifelse(ASCAR1 > 0 | ASCAR2 > 0, "Positive", "Negative"),
     anaemia_status = ifelse(anaemia_level == "non-anaemic", "Non-Anaemic", "Anaemic"),
     age_group = ifelse(AGE < 12, "<12", "≥12")
   ) %>%
   group_by(ascaris_status, anaemia_status, age_group) %>%
   summarise(
     mean_ERR = mean(ERR_schisto, na.rm = TRUE),
     .groups = "drop"
   )
 
 # Plot heatmap
 ggplot(data_heatmap, aes(x = ascaris_status, y = anaemia_status, fill = mean_ERR)) +
   geom_tile(color = "white") +
   geom_text(aes(label = round(mean_ERR, 1)), color = "black", size = 4) +
   scale_fill_gradient(low = "white", high = "red", name = "Mean ERR") +
   facet_wrap(~age_group) +
   labs(
     title = "Heatmap of Mean ERR by Ascaris Status × Anaemia × Age Group",
     x = "Ascaris Status",
     y = "Anaemia Status"
   ) +
   theme_minimal(base_size = 14)
 
 
 model_interaction <- lm(
   ERR_individual ~ ascaris_burden * anaemic_bin + AGE + SEX + BMI,
   data = pzq_groups
 )
 
 summary(model_interaction)
 
 library(ggplot2)
 
 ggplot(pzq_groups, aes(x = ascaris_burden, y = ERR_individual, color = anaemic_bin)) +
   stat_summary(fun = mean, geom = "point", size = 3) +
   stat_summary(fun = mean, geom = "line", aes(group = anaemic_bin)) +
   labs(
     title = "Interaction: Ascaris Burden × Anaemia on ERR",
     x = "Ascaris Burden",
     y = "Egg Reduction Rate (ERR)",
     color = "Anaemia Status"
   ) +
   theme_minimal()
 
 pzq_groups %>%
   group_by(ascaris_burden, anaemic_bin) %>%
   summarise(
     mean_ERR = mean(ERR_individual, na.rm = TRUE),
     sd_ERR = sd(ERR_individual, na.rm = TRUE),
     n = n()
   )
 
 library(dplyr)
 
 # Create a new variable where "moderate" and "heavy" become "mod_heavy"
 pzq_groups <- pzq_groups %>%
   mutate(ascaris_burden_combined = case_when(
     ascaris_burden %in% c("moderate", "heavy") ~ "mod_heavy",
     TRUE ~ as.character(ascaris_burden)
   ))
 
 # Now summarize by the new combined burden and anaemia status
 summary_combined <- pzq_groups %>%
   group_by(ascaris_burden_combined, anaemic_bin) %>%
   summarise(
     mean_ERR = mean(ERR_individual, na.rm = TRUE),
     sd_ERR = sd(ERR_individual, na.rm = TRUE),
     n = n(),
     .groups = "drop"
   )
 
 # View result
 print(summary_combined)
 
 mod_heavy_anaemic <- pzq_groups %>%
   filter(ascaris_burden %in% c("moderate", "heavy") & anaemic_bin == "anaemic")
 
 # View their ERR values
 mod_heavy_anaemic %>% select(ID, ascaris_burden, anaemic_bin, ERR_individual)
 
 library(broom)
 
 # Get tidy output with confidence intervals
 tidy_interaction <- tidy(model_interaction, conf.int = TRUE, conf.level = 0.95)
 
 # View the results
 print(tidy_interaction)
 
 library(ggplot2)
 library(dplyr)
 
 # Create summary data
 heat_data <- pzq_groups %>%
   group_by(ascaris_burden, anaemic_bin) %>%
   summarise(mean_ERR = mean(ERR_individual, na.rm = TRUE)) %>%
   ungroup()
 
 # Plot heatmap
 ggplot(heat_data, aes(x = ascaris_burden, y = anaemic_bin, fill = mean_ERR)) +
   geom_tile(color = "white") +
   scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                        name = "Mean ERR") +
   labs(title = "Mean ERR by Ascaris Burden and Anaemia Status",
        x = "Ascaris Burden",
        y = "Anaemia Status") +
   theme_minimal(base_size = 14)
 
 model_interaction_age <- lm(
   ERR_individual ~ ascaris_burden * anaemic_bin * age_group +
     BMI + SEX,
   data = pzq_groups
 )
 
 summary(model_interaction_age)
 
 library(broom)
 
 tidy_interaction_age <- tidy(model_interaction_age, conf.int = TRUE, conf.level = 0.95)
 
 # View results
 print(tidy_interaction_age)
 
 library(dplyr)
 
 heatmap_data <- pzq_groups %>%
   group_by(ascaris_burden, anaemic_bin, age_group) %>%
   summarise(mean_ERR = mean(ERR_individual, na.rm = TRUE),
             n = n())
 
 library(ggplot2)
 
 ggplot(heatmap_data, aes(x = ascaris_burden, y = anaemic_bin, fill = mean_ERR)) +
   geom_tile(color = "white") +
   facet_wrap(~age_group) +
   geom_text(aes(label = round(mean_ERR, 1)), color = "black", size = 4) +
   scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                        name = "Mean ERR") +
   labs(title = "Egg Reduction Rate by Ascaris Burden, Anaemia, and Age",
        x = "Ascaris Burden",
        y = "Anaemia Status") +
   theme_minimal(base_size = 14)
 
 model_err_malaria <- lm(ERR_individual ~ malaria_burden, data = pzq_groups)
 summary(model_err_malaria) 
 
 model_delta_malaria <- lm(delta_schisto ~ malaria_burden, data = pzq_groups)
 summary(model_delta_malaria)
 
 model_adj <- lm(ERR_individual ~ malaria_burden + AGE + SEX + coinfection_count_all,
                 data = pzq_groups)
 summary(model_adj)
 
 library(emmeans)
 emmeans(model_err_malaria, pairwise ~ malaria_burden)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
  