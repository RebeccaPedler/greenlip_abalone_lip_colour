# Install and load necessary package
install.packages(c("car", "dplyr", "tidyr", "rcompanion", "DescTools"))
library(car)
library(dplyr)
library(tidyr)
library(rcompanion)
library(DescTools)

#Load data and check structure
getwd()
setwd("C:/Users/RebeccaPedler/OneDrive - Yumbah/Documents/R&D/Industry PhD/Trials/Container Trial/R_datasets")

lip_data <- read.csv("lip_colour_data_all.csv", header = TRUE)
str(lip_data)

lip_data <- lip_data %>%
  mutate(
    diet       = factor(diet),
    water_temp = factor(water_temp),
    treatment  = factor(treatment)
  )

#Get summary statistics
summary_stats <- lip_data %>%
  group_by(treatment, measurement) %>%
  summarise(
    n = n(),
    mean = mean(result, na.rm = TRUE),
    se = sd(result, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )
print(summary_stats)

#Summary statistics (conditioning phase)
summary_stats_18 <- lip_data %>%
  filter(water_temp == "18") %>%  
  group_by(treatment, measurement) %>%
  summarise(
    n = n(),
    mean = mean(result, na.rm = TRUE),
    se = sd(result, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    mean_se = paste0(round(mean, 2), " \u00B1 ", round(se, 2))
  )
print(summary_stats_18, n = 42)

#Summary statistics (20 degrees)
summary_stats_20 <- lip_data %>%
  filter(water_temp == "20") %>% 
  group_by(treatment, measurement) %>%
  summarise(
    n = n(),
    mean = mean(result, na.rm = TRUE),
    se = sd(result, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    mean_se = paste0(round(mean, 2), " \u00B1 ", round(se, 2))
  )
print(summary_stats_20, n = 42)

#Summary statistics (25 degrees)
summary_stats_25 <- lip_data %>%
  filter(water_temp == "25") %>% 
  group_by(treatment, measurement) %>%
  summarise(
    n = n(),
    mean = mean(result, na.rm = TRUE),
    se = sd(result, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    mean_se = paste0(round(mean, 2), " \u00B1 ", round(se, 2))
  )
print(summary_stats_25, n = 42)

# Wakame ONE WAY ANOVA
wakame_18 <- lip_data %>%
  filter(diet %in% c("CONTROL", "WK"), 
  water_temp %in% c("18"))%>%
  mutate(inclusion = factor(inclusion))
str(wakame_18)

# L
wakame_L <- wakame_18 %>%
  filter(measurement == "L")

wakame_anova_model_L <- aov(result ~ inclusion, data = wakame_L)
summary(wakame_anova_model_L)

residuals_anova_WK_L <- residuals(wakame_anova_model_L)
shapiro.test(residuals_anova_WK_L)
qqnorm(residuals_anova_WK_L); qqline(residuals_anova_WK_L)
leveneTest(result ~ inclusion, data = wakame_L)

# a
wakame_a <- wakame_18 %>%
  filter(measurement == "a")

wakame_anova_model_a <- aov(result ~ inclusion, data = wakame_a)
summary(wakame_anova_model_a)

residuals_anova_WK_a <- residuals(wakame_anova_model_a)
shapiro.test(residuals_anova_WK_a)
qqnorm(residuals_anova_WK_a); qqline(residuals_anova_WK_a)
leveneTest(result ~ inclusion, data = wakame_a)

# b
wakame_b <- wakame_18 %>%
  filter(measurement == "b")

wakame_anova_model_b <- aov(result ~ inclusion, data = wakame_b)
summary(wakame_anova_model_b)

residuals_anova_WK_b <- residuals(wakame_anova_model_b)
shapiro.test(residuals_anova_WK_b)
qqnorm(residuals_anova_WK_b); qqline(residuals_anova_WK_b)
leveneTest(result ~ inclusion, data = wakame_b)

TukeyHSD(wakame_anova_model_b, "inclusion")

# Wakame TWO WAY ANOVA
wakame <- lip_data %>%
  filter(diet %in% c("CONTROL", "WK"), 
  water_temp %in% c("20", "25")) %>%
  mutate(inclusion = factor(inclusion))
str(wakame)

# L
wakame_L <- wakame %>%
  filter(measurement %in% "L")

wakame_anova_model_L <- aov(result ~ inclusion * water_temp, data = wakame_L)
summary(wakame_anova_model_L)

residuals_anova_WK_L <- residuals(wakame_anova_model_L)
shapiro.test(residuals_anova_WK_L)

qqnorm(residuals_anova_WK_L)
qqline(residuals_anova_WK_L)

leveneTest(result ~ inclusion * water_temp, data = wakame_L)

TukeyHSD(wakame_anova_model_L, "inclusion")
TukeyHSD(wakame_anova_model_L, "water_temp")

# a
wakame_a <- wakame %>%
  filter(measurement %in% "a")

wakame_anova_model_a <- aov(result ~ inclusion * water_temp, data = wakame_a)
summary(wakame_anova_model_a)

residuals_anova_WK_a <- residuals(wakame_anova_model_a)
shapiro.test(residuals_anova_WK_a)

qqnorm(residuals_anova_WK_a)
qqline(residuals_anova_WK_a)

leveneTest(result ~ inclusion * water_temp, data = wakame_a)

TukeyHSD(wakame_anova_model_a, "inclusion")
TukeyHSD(wakame_anova_model_a, "water_temp")

# b
wakame_b <- wakame %>%
  filter(measurement %in% "b")
print(wakame_b)

wakame_anova_model_b <- aov(result ~ inclusion * water_temp, data = wakame_b)
summary(wakame_anova_model_b)

residuals_anova_WK_b <- residuals(wakame_anova_model_b)
shapiro.test(residuals_anova_WK_b)

qqnorm(residuals_anova_WK_b)
qqline(residuals_anova_WK_b)

leveneTest(result ~ inclusion * water_temp, data = wakame_b)

TukeyHSD(wakame_anova_model_b, "inclusion")
TukeyHSD(wakame_anova_model_b, "water_temp")

# Spirulina ONE WAY ANOVA
spirulina_18 <- lip_data %>%
  filter(diet %in% c("CONTROL", "SP"), 
         water_temp %in% c("18"))
str(spirulina_18)

# L
spirulina_L <- spirulina_18 %>%
  filter(measurement == "L") %>%
  mutate(inclusion = factor(inclusion))

spirulina_anova_model_L <- aov(result ~ inclusion, data = spirulina_L)
summary(spirulina_anova_model_L)

residuals_anova_SP_L <- residuals(spirulina_anova_model_L)
shapiro.test(residuals_anova_SP_L)
qqnorm(residuals_anova_SP_L); qqline(residuals_anova_SP_L)
leveneTest(result ~ inclusion, data = spirulina_L)

TukeyHSD(spirulina_anova_model_L, "inclusion")

# a
spirulina_a <- spirulina_18 %>%
  filter(measurement == "a") %>%
  mutate(inclusion = factor(inclusion))

spirulina_anova_model_a <- aov(result ~ inclusion, data = spirulina_a)
summary(spirulina_anova_model_a)

residuals_anova_SP_a <- residuals(spirulina_anova_model_a)
shapiro.test(residuals_anova_SP_a)
qqnorm(residuals_anova_SP_a); qqline(residuals_anova_SP_a)
leveneTest(result ~ inclusion, data = spirulina_a)

TukeyHSD(spirulina_anova_model_a, "inclusion")

# b
spirulina_b <- spirulina_18 %>%
  filter(measurement == "b") %>%
  mutate(inclusion = factor(inclusion))

spirulina_anova_model_b <- aov(result ~ inclusion, data = spirulina_b)
summary(spirulina_anova_model_b)

residuals_anova_SP_b <- residuals(spirulina_anova_model_b)
shapiro.test(residuals_anova_SP_b)
qqnorm(residuals_anova_SP_b); qqline(residuals_anova_SP_b)
leveneTest(result ~ inclusion, data = spirulina_b)

TukeyHSD(spirulina_anova_model_b, "inclusion")

#Spirulina ANOVA
spirulina <- lip_data %>%
  filter(diet %in% c("CONTROL", "SP"), water_temp %in% c("20", "25")) %>%
  mutate(inclusion = factor(inclusion))

#L
spirulina_L <- spirulina %>%
  filter(measurement %in% "L") 

spirulina_anova_model_L <- aov(result ~ inclusion * water_temp, data = spirulina_L)
summary(spirulina_anova_model_L)

#Assumptions
# Extract residuals
residuals_anova_SP_L <- residuals(spirulina_anova_model_L)

# Shapiro-Wilk test
shapiro.test(residuals_anova_SP_L)

# Q-Q plot
qqnorm(residuals_anova_SP_L)
qqline(residuals_anova_SP_L)

#Homogeneity of variance
leveneTest(result ~ inclusion * water_temp, data = spirulina_L)

# Tukey HSD for the main effect of inclusion
TukeyHSD(spirulina_anova_model_L, "inclusion")

# Tukey HSD for the main effect of water_temp
TukeyHSD(spirulina_anova_model_L, "water_temp")

#a
spirulina_a <- spirulina %>%
  filter(measurement %in% "a")

spirulina_anova_model_a <- aov(result ~ inclusion * water_temp, data = spirulina_a)
summary(spirulina_anova_model_a)

#Assumptions
# Extract residuals
residuals_anova_SP_a <- residuals(spirulina_anova_model_a)

# Shapiro-Wilk test
shapiro.test(residuals_anova_SP_a)

# Q-Q plot
qqnorm(residuals_anova_SP_a)
qqline(residuals_anova_SP_a)

#Homogeneity of variance
leveneTest(result ~ inclusion * water_temp, data = spirulina_a)

# Tukey HSD for the main effect of inclusion
TukeyHSD(spirulina_anova_model_a, "inclusion")

# Tukey HSD for the main effect of water_temp
TukeyHSD(spirulina_anova_model_a, "water_temp")

#b
spirulina_b <- spirulina %>%
  filter(measurement %in% "b")

spirulina_anova_model_b <- aov(result ~ inclusion * water_temp, data = spirulina_b)
summary(spirulina_anova_model_b)

#Assumptions
# Extract residuals
residuals_anova_SP_b <- residuals(spirulina_anova_model_b)

# Shapiro-Wilk test
shapiro.test(residuals_anova_SP_b)

# Q-Q plot
qqnorm(residuals_anova_SP_b)
qqline(residuals_anova_SP_b)

#Homogeneity of variance
leveneTest(result ~ inclusion * water_temp, data = spirulina_b)

# Tukey HSD for the main effect of inclusion
TukeyHSD(spirulina_anova_model_b, "inclusion")

# Tukey HSD for the main effect of water_temp
TukeyHSD(spirulina_anova_model_b, "water_temp")
