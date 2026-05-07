# Abalone lip colour — commercial farm dataset
# Script 01: Data structuring, factor setup, and inspection

# Install packages and load libraries

install.packages(c("tidyverse", "ggplot2", "patchwork", "scales", "vcd", "lme4", "lmerTest", "emmeans", "performance", "car"))

library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(vcd)
library(lme4)        
library(lmerTest)    
library(emmeans)     
library(performance) 
library(car)  
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(car)       

### LOAD & CLEAN DATA

setwd("C:/Users/RebeccaPedler/OneDrive - Yumbah/Documents/R&D/Industry PhD/Trials/Commercial trial")

df_raw <- read.csv("lip_colour_commercial.csv")
str(df_raw)

names(df_raw) <- c("image_id", "date", "farm", "section", "tank", "yc", "age", "diet",
                   "shade_coverage", "section_coverage", "tank_position",
                   "abalone_number", "lightness", "a", "b", "length_mm", 
                   "width_mm", "area_mm2")

df_raw <- df_raw |>
  mutate(across(where(is.character), str_trim)) |>
  filter(!is.na(farm))

df_raw <- df_raw |> filter(!is.na(section_coverage)) # Delete NA
df_raw <- df_raw |> filter(lightness != 0) # Delete zero
df_raw <- df_raw |> filter(!is.na(length_mm)) # Delete NA

# Correct data types
df <- df_raw |>
  mutate(
    date = factor(date),

    # Ordered factors
    shade_coverage   = factor(shade_coverage,
                              levels  = c(1, 2, 3),
                              labels  = c("single", "double", "mixed"),
                              ordered = TRUE),

    section_coverage = factor(section_coverage,
                              levels  = c(1, 2),
                              labels  = c("single", "double"),
                              ordered = TRUE),

    # Unordered factors
    farm          = factor(farm),
    section       = factor(section),
    tank          = factor(tank),
    yc            = factor(yc, levels = c(21, 22)),
    age           = factor(age, levels = c(19, 31)),
    diet          = factor(diet),
    tank_position = factor(tank_position),

    # Numeric
    lightness  = as.numeric(lightness),
    a          = as.numeric(a),
    b          = as.numeric(b),
    length     = as.numeric(length_mm),
    width      = as.numeric(width_mm),
    area       = as.numeric(area_mm2),

    abalone_id = paste(tank, abalone_number, sep = "_")
  )

str(df)

### CLEAN DATA 

# Flag duplicate image IDs
dupes <- df_raw |> filter(duplicated(image_id) | duplicated(image_id, fromLast = TRUE))
if (nrow(dupes) > 0) {
  cat("WARNING: Duplicate image IDs found:\n")
  print(dupes[, c("image_id", "tank", "abalone_number")], row.names = FALSE)
} else {
  cat("No duplicate image IDs.\n")
}

# Check CIELAB metrics 
cat(sprintf("  L*: min=%.2f, max=%.2f  (expect 0-100)\n",
            min(df$lightness, na.rm=TRUE), max(df$lightness, na.rm=TRUE)))
cat(sprintf("  a*: min=%.2f, max=%.2f\n",
            min(df$a, na.rm=TRUE), max(df$a, na.rm=TRUE)))
cat(sprintf("  b*: min=%.2f, max=%.2f\n",
            min(df$b, na.rm=TRUE), max(df$b, na.rm=TRUE)))

# Flag any L* outside valid range
invalid_L <- df |> filter(lightness < 0 | lightness > 100)
if (nrow(invalid_L) > 0) {
  cat("WARNING: Invalid L* values:\n")
  print(invalid_L[, c("image_id", "tank", "lightness")], row.names = FALSE)
}

# Flag within-tank outliers (>3 SD from tank mean) for each colour metric
flag_outliers <- function(data, var) {
  data |>
    group_by(tank) |>
    mutate(
      tank_mean = mean(.data[[var]], na.rm = TRUE),
      tank_sd   = sd(.data[[var]],   na.rm = TRUE),
      z_within  = (.data[[var]] - tank_mean) / tank_sd,
      outlier   = abs(z_within) > 3
    ) |>
    ungroup() |>
    filter(outlier) |>
    select(image_id, tank, diet, yc, all_of(var), z_within)
}

for (var in c("lightness", "a", "b")) {
  out <- flag_outliers(df, var)
  cat(sprintf("\n  Within-tank outliers (|z|>3) for %s: %d rows\n", var, nrow(out)))
  if (nrow(out) > 0) print(as.data.frame(out), row.names = FALSE)
}

### OBSERVE LEVELS AND OBSERVATIONS FOR EACH FACTOR

factor_vars <- c("farm", "section", "age", "diet",
                 "section_coverage")

for (v in factor_vars) {
  cat(sprintf("\n--- %s ---\n", v))
  df |>
    count(.data[[v]], name = "n") |>
    mutate(pct = round(n / sum(n) * 100, 1)) |>
    as.data.frame() |>
    print(row.names = FALSE)
}

# Tank-level summaries
# Tanks per section_coverage
df |>
  distinct(tank, section_coverage) |>
  count(section_coverage) |>
  as.data.frame() |>
  print(row.names = FALSE)

# Abalone per tank
df |>
  count(tank, name = "n_abalone") |>
  summarise(mean = round(mean(n_abalone), 1),
            sd   = round(sd(n_abalone),   1),
            min  = min(n_abalone),
            max  = max(n_abalone)) |>
  as.data.frame() |>
  print(row.names = FALSE)

### GENERAL DATA OBSERVATIONS BY PLOTTING

# Create colour palettes for plotting
sec_cols <- c("single" = "#8DB4C8", "double" = "#2E6E9E")   # section_coverage
yc_cols  <- c("21"     = "#E07B39", "22"     = "#5B8FA8")   # year class
 
## Overall distributions and histograms — L*, a*, b*
hist_plot <- function(var, xlab, fill_col) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 40, fill = fill_col,
                   colour = "white", linewidth = 0.25, alpha = 0.85) +
    geom_density(colour = "#1B2A1C", linewidth = 0.6) +
    geom_rug(colour = fill_col, alpha = 0.3, linewidth = 0.3) +
    labs(x = xlab, y = "Density") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          axis.title.y     = element_text(size = 9, colour = "grey40"))
}

p_L <- hist_plot("lightness", "L* (lightness)",   "#5B8FA8")
p_a <- hist_plot("a",         "a* (green\u2013red)",   "#7A9E5A")
p_b <- hist_plot("b",         "b* (blue\u2013yellow)", "#C4A24A")

p_hist_all <- (p_L | p_a | p_b) +
  plot_annotation(
    title    = "Distribution of L*, a*, b* values",
    subtitle = paste0("n = ", nrow(df), " abalone (complete cases)"),
    theme    = theme(plot.title    = element_text(size = 13, face = "bold"),
                     plot.subtitle = element_text(size = 10, colour = "grey40"))
  )

# Print patched histograms 

print(p_hist_all)

## Inspect section_coverage (single or double shade cloth)

lab_long <- function(data) {
  data |>
    pivot_longer(cols = c(lightness, a, b),
                 names_to = "metric", values_to = "value") |>
    mutate(metric = factor(metric,
                           levels = c("lightness", "a", "b"),
                           labels = c("L*", "a*", "b*")))
}

# Histograms faceted by section_coverage
p_sec_hist <- df |>
  lab_long() |>
  ggplot(aes(x = value, fill = section_coverage)) +
  geom_histogram(bins = 30, colour = "white", linewidth = 0.2,
                 alpha = 0.8, position = "identity") +
  facet_grid(section_coverage ~ metric, scales = "free_x") +
  scale_fill_manual(values = sec_cols, guide = "none") +
  labs(x = "Value", y = "Count",
       title    = "L*, a*, b* by section coverage",
       subtitle = "Rows = coverage level · Columns = colour metric") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text       = element_text(size = 10),
        plot.title       = element_text(size = 13, face = "bold"),
        plot.subtitle    = element_text(size = 10, colour = "grey40"))

# Print plot
print(p_sec_hist)

# Boxplots by section_coverage (no YC)
p_sec_box <- df |>
  lab_long() |>
  ggplot(aes(x = section_coverage, y = value, fill = section_coverage)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5,
               colour = "grey30", linewidth = 0.4, alpha = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.07, size = 0.6, colour = "grey20") +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = sec_cols, guide = "none") +
  labs(x = "Section coverage", y = "Value",
       title    = "Colour metrics by section coverage",
       subtitle = "Jittered points = individual abalone; boxes = median \u00b1 IQR") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text       = element_text(size = 11),
        plot.title       = element_text(size = 13, face = "bold"),
        plot.subtitle    = element_text(size = 10, colour = "grey40"))

# a* vs b* scatter by section_coverage
p_scatter <- ggplot(df, aes(x = a, y = b, colour = section_coverage)) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_colour_manual(values = sec_cols, name = "Section\ncoverage") +
  labs(x = "a* (green\u2013red)", y = "b* (blue\u2013yellow)",
       title    = "a* vs b* by section coverage",
       subtitle = "Lines = linear trend per coverage level") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title       = element_text(size = 13, face = "bold"),
        plot.subtitle    = element_text(size = 10, colour = "grey40"),
        legend.position  = "right")


## Inspect section_coverage (single or double shade cloth) X YC (21 and 22)

# Boxplots: section_coverage × YC (dodged)
p_sec_yc_box <- df |>
  lab_long() |>
  ggplot(aes(x = section_coverage, y = value, fill = yc)) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5,
               colour = "grey30", linewidth = 0.4, alpha = 0.8,
               position = position_dodge(0.8)) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = yc_cols, name = "Year class") +
  labs(x = "Section coverage", y = "Value",
       title    = "Colour metrics by section coverage and year class",
       subtitle = "Dodged boxes = YC21 (orange) vs YC22 (blue)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text       = element_text(size = 11),
        plot.title       = element_text(size = 13, face = "bold"),
        plot.subtitle    = element_text(size = 10, colour = "grey40"),
        legend.position  = "right")

# Histograms: section_coverage × YC
p_sec_yc_hist <- df |>
  lab_long() |>
  ggplot(aes(x = value, fill = yc)) +
  geom_histogram(bins = 25, colour = "white", linewidth = 0.2,
                 alpha = 0.75, position = "identity") +
  facet_grid(section_coverage ~ metric, scales = "free_x") +
  scale_fill_manual(values = yc_cols, name = "Year class") +
  labs(x = "Value", y = "Count",
       title    = "L*, a*, b* by section coverage and year class",
       subtitle = "Rows = coverage level · Columns = colour metric · Colours = YC") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text       = element_text(size = 10),
        plot.title       = element_text(size = 13, face = "bold"),
        plot.subtitle    = element_text(size = 10, colour = "grey40"),
        legend.position  = "right")

# Print plot
print(p_sec_yc_hist)

# save all plots

ggsave("plot_01_histograms_Lab.png",      p_hist_all,     width = 10, height = 4,   dpi = 150)
ggsave("plot_02_sec_histograms.png",      p_sec_hist,     width = 10, height = 6,   dpi = 150)
ggsave("plot_03_sec_boxplots.png",        p_sec_box,      width = 10, height = 4.5, dpi = 150)
ggsave("plot_04_ab_scatter.png",          p_scatter,      width = 7,  height = 5.5, dpi = 150)
ggsave("plot_05_sec_yc_boxplots.png",     p_sec_yc_box,   width = 11, height = 5,   dpi = 150)
ggsave("plot_06_sec_yc_histograms.png",   p_sec_yc_hist,  width = 11, height = 6.5, dpi = 150)

# Create numeric summaries

df |>
  group_by(section_coverage) |>
  summarise(n      = n(),
            L_mean = round(mean(lightness), 2), L_sd = round(sd(lightness), 2),
            a_mean = round(mean(a), 2),         a_sd = round(sd(a), 2),
            b_mean = round(mean(b), 2),         b_sd = round(sd(b), 2),
            .groups = "drop") |>
  as.data.frame() |>
  print(row.names = FALSE)

df |>
  group_by(section_coverage, yc) |>
  summarise(n      = n(),
            L_mean = round(mean(lightness), 2),
            a_mean = round(mean(a), 2),
            b_mean = round(mean(b), 2),
            b_sd   = round(sd(b),   2),
            .groups = "drop") |>
  as.data.frame() |>
  print(row.names = FALSE)

df |>
  group_by(diet) |>
  summarise(n      = n(),
            b_mean = round(mean(b), 2),
            b_sd   = round(sd(b),   2),
            a_mean = round(mean(a), 2),
            .groups = "drop") |>
  arrange(desc(b_mean)) |>
  as.data.frame() |>
  print(row.names = FALSE)

df |>
  group_by(yc) |>
  summarise(n      = n(),
            b_mean = round(mean(b), 2),
            b_sd   = round(sd(b),   2),
            .groups = "drop") |>
  as.data.frame() |>
  print(row.names = FALSE)

### Script 02: Collinearity assessment — section_coverage, YC, diet

# Create separate dataframe to work on tank level
tank_df <- df |>
  distinct(tank, section_coverage, yc, diet, farm, section)

## PAIRWISE CROSSTABS WITH CRAMER'S V

cramer_v <- function(x, y) {
  tbl <- table(x, y)
  chi <- suppressWarnings(chisq.test(tbl))
  n   <- sum(tbl)
  k   <- min(nrow(tbl), ncol(tbl))
  v   <- sqrt(chi$statistic / (n * (k - 1)))
  p   <- chi$p.value
  list(V = round(as.numeric(v), 3), p = round(p, 4),
       chi2 = round(chi$statistic, 2), df = chi$parameter)
}

# Create pairs for moderators

pairs <- list(
  c("section_coverage", "yc"),
  c("section_coverage", "diet"),
  c("yc",              "diet")
)

for (pair in pairs) {
  res <- cramer_v(tank_df[[pair[1]]], tank_df[[pair[2]]])
  severity <- dplyr::case_when(
    res$V >= 0.7 ~ "SEVERE — do not include both",
    res$V >= 0.4 ~ "Moderate — include with caution",
    TRUE         ~ "Low — safe to include together"
  )
  cat(sprintf("%-20s x %-20s  V = %.3f  p = %.4f  [%s]\n",
              pair[1], pair[2], res$V, res$p, severity))
}

## CROSSTAB COUNTS (tank level)

# section_coverage x YC
table(tank_df$section_coverage, tank_df$yc) |>
  addmargins() |>
  print()

# section_coverage
table(tank_df$section_coverage, tank_df$diet) |>
  addmargins() |>
  print()

# YC x diet
table(tank_df$yc, tank_df$diet) |>
  addmargins() |>
  print()

# Three-way: section_coverage x YC x diet (n tanks)
tank_df |>
  count(section_coverage, yc, diet) |>
  arrange(section_coverage, yc, diet) |>
  as.data.frame() |>
  print(row.names = FALSE)

## VISUALISE CROSSING STRUCTURE

p_tile <- tank_df |>
  count(section_coverage, yc, diet) |>
  ggplot(aes(x = yc, y = diet, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = n), size = 3.5, fontface = "bold",
            colour = "white") +
  facet_wrap(~section_coverage, labeller = label_both) +
  scale_fill_gradient(low = "#BDD7EE", high = "#1F618D",
                      name = "Tanks (n)") +
  labs(x = "Year class", y = "Diet",
       title    = "Number of tanks per predictor combination",
       subtitle = "Each panel = one section_coverage level · Numbers = tank count\nEmpty cells = missing combinations (potential confounding)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid    = element_blank(),
        strip.text    = element_text(size = 11, face = "bold"),
        plot.title    = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 10, colour = "grey40"),
        legend.position = "right")

# Print crossing plot
print(p_tile)

## MOSAIC PLOTS (visual Cramer's V)

pdf("plot_07_mosaic_yc_diet.pdf", width = 7, height = 5)
  vcd::mosaic(~ yc + diet, data = tank_df,
              shade = TRUE, legend = TRUE,
              main  = "Mosaic: YC x diet (tank level)",
              sub   = "Blue = more tanks than expected under independence\nRed = fewer")
dev.off()

pdf("plot_08_mosaic_sec_diet.pdf", width = 7, height = 5)
  vcd::mosaic(~ section_coverage + diet, data = tank_df,
              shade = TRUE, legend = TRUE,
              main  = "Mosaic: section_coverage x diet (tank level)")
dev.off()

## VARIANCE INFLATION — manual check via linear probability models

tank_num <- tank_df |>
  mutate(
    sec_double = as.integer(section_coverage == "double"),
    yc_22      = as.integer(yc == "22"),
    diet_mari   = as.integer(diet == "Mari"),
    diet_pink   = as.integer(diet == "Pink"),
    diet_purple = as.integer(diet == "Purple"),
    diet_ridley = as.integer(diet == "Ridley")
    # Light blue = reference level
  )

calc_vif <- function(outcome, predictors, data) {
  f   <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  mod <- lm(f, data = data)
  r2  <- summary(mod)$r.squared
  vif <- 1 / (1 - r2)
  cat(sprintf("  VIF for %-14s = %.2f  (R² = %.3f)\n", outcome, vif, r2))
}

calc_vif("sec_double",  c("yc_22","diet_mari","diet_pink","diet_purple","diet_ridley"), tank_num)
calc_vif("yc_22",       c("sec_double","diet_mari","diet_pink","diet_purple","diet_ridley"), tank_num)
calc_vif("diet_mari",   c("sec_double","yc_22"), tank_num)
calc_vif("diet_pink",   c("sec_double","yc_22"), tank_num)
calc_vif("diet_purple", c("sec_double","yc_22"), tank_num)
calc_vif("diet_ridley", c("sec_double","yc_22"), tank_num)

# Save plots
ggsave("plot_07_predictor_crossing.png", p_tile, width = 9, height = 5, dpi = 150)

### Script 03: Linear Mixed Models — b*, L*, a* ~ section_coverage + yc + diet

## PREPARE MODELLING DATASET

df_mod <- df |>
  filter(!is.na(section_coverage), !is.na(yc), !is.na(diet),
         !is.na(b), !is.na(a), !is.na(lightness)) |>
  mutate(
    section_coverage = factor(section_coverage, ordered = FALSE),
    yc               = factor(yc,               ordered = FALSE),
    diet             = factor(diet),
    section_coverage = relevel(section_coverage, ref = "single"),
    yc               = relevel(yc,               ref = "22"),
    diet             = relevel(diet,             ref = "Pink")  
  )

# NULL MODELS & ICC 
# Fit intercept-only models (REML) to estimate ICC before any fixed effects

null_b <- lmer(b         ~ 1 + (1|tank), data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
null_L <- lmer(lightness ~ 1 + (1|tank), data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
null_a <- lmer(a         ~ 1 + (1|tank), data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))

icc_table <- data.frame(
  response = c("b*", "L*", "a*"),
  ICC      = round(c(performance::icc(null_b)$ICC_adjusted,
                     performance::icc(null_L)$ICC_adjusted,
                     performance::icc(null_a)$ICC_adjusted), 3),
  interpretation = c(
    ifelse(performance::icc(null_b)$ICC_adjusted > 0.1,
           "Mixed model warranted", "Minimal clustering"),
    ifelse(performance::icc(null_L)$ICC_adjusted > 0.1,
           "Mixed model warranted", "Minimal clustering"),
    ifelse(performance::icc(null_a)$ICC_adjusted > 0.1,
           "Mixed model warranted", "Minimal clustering")
  )
)

print(icc_table, row.names = FALSE)

## FORWARD SELECTION: FIT ALL MODEL COMBINATIONS AND COMPARE

# Order: null → + section_coverage → + yc → + diet (in order of hypothetic importance)

fit_steps <- function(response, data = df_mod) {
  f0  <- as.formula(paste(response, "~ 1                              + (1|tank)"))
  f1  <- as.formula(paste(response, "~ section_coverage               + (1|tank)"))
  f2  <- as.formula(paste(response, "~ section_coverage + yc          + (1|tank)"))
  f3  <- as.formula(paste(response, "~ section_coverage + yc + diet   + (1|tank)"))

  list(
    m0 = lmer(f0, data = data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
    m1 = lmer(f1, data = data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
    m2 = lmer(f2, data = data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
    m3 = lmer(f3, data = data, REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
  )
}

models_b <- fit_steps("b")
models_L <- fit_steps("lightness")
models_a <- fit_steps("a")

# AIC / BIC COMPARISON TABLE 

model_labels <- c(
  "m0: null (intercept only)",
  "m1: + section_coverage",
  "m2: + section_coverage + yc",
  "m3: + section_coverage + yc + diet"
)

aic_table <- function(mods, response) {
  aic_vals <- sapply(mods, AIC)
  bic_vals <- sapply(mods, BIC)
  loglik   <- sapply(mods, logLik)
  df_used  <- sapply(mods, function(m) attr(logLik(m), "df"))

  data.frame(
    response  = response,
    model     = model_labels,
    df        = df_used,
    logLik    = round(loglik,   2),
    AIC       = round(aic_vals, 2),
    dAIC      = round(aic_vals - min(aic_vals), 2),
    BIC       = round(bic_vals, 2),
    dBIC      = round(bic_vals - min(bic_vals), 2)
  )
}

aic_all <- bind_rows(
  aic_table(models_b, "b*"),
  aic_table(models_L, "L*"),
  aic_table(models_a, "a*")
)

# Print per response 
for (resp in c("b*", "L*", "a*")) {
  cat(sprintf("--- %s ---\n", resp))
  tbl <- aic_all |> filter(response == resp) |> select(-response)
  print(tbl, row.names = FALSE)

  best <- tbl$model[which.min(tbl$AIC)]
  cat(sprintf("  Best by AIC: %s\n", best))  # Flag best model

  competitive <- tbl |> filter(dAIC < 2)
  if (nrow(competitive) > 1) {
    cat("  Note: Multiple models within dAIC < 2 — consider parsimony.\n")
  }
  cat("\n")
}  

# LIKELIHOOD RATIO TESTS: LRT at each forward step — tests whether adding each predictor

lrt_sequential <- function(mods, response) {
  steps <- list(
    list(reduced = mods$m0, full = mods$m1, term_added = "section_coverage"),
    list(reduced = mods$m1, full = mods$m2, term_added = "yc"),
    list(reduced = mods$m2, full = mods$m3, term_added = "diet")
  )
  lapply(steps, function(s) {
    lt <- anova(s$reduced, s$full)
    data.frame(
      response   = response,
      term_added = s$term_added,
      model_prev = deparse(formula(s$reduced)),
      chi2       = round(lt$Chisq[2],        3),
      df         = lt$Df[2],
      p_value    = round(lt$`Pr(>Chisq)`[2], 4),
      sig        = case_when(
        lt$`Pr(>Chisq)`[2] < 0.001 ~ "***",
        lt$`Pr(>Chisq)`[2] < 0.01  ~ "**",
        lt$`Pr(>Chisq)`[2] < 0.05  ~ "*",
        lt$`Pr(>Chisq)`[2] < 0.1   ~ ".",
        TRUE                        ~ "ns"
      )
    )
  }) |> bind_rows()
}

lrt_all <- bind_rows(
  lrt_sequential(models_b, "b*"),
  lrt_sequential(models_L, "L*"),
  lrt_sequential(models_a, "a*")
)

for (resp in c("b*", "L*", "a*")) {
  cat(sprintf("--- %s ---\n", resp))
  tbl <- lrt_all |>
    filter(response == resp) |>
    select(term_added, chi2, df, p_value, sig)
  print(tbl, row.names = FALSE)
  cat("\n")
}

## REFIT SELECTED MODEL WITH REML 

final_formula_b <- b         ~ section_coverage + yc + diet + (1|tank)
final_formula_L <- lightness ~ section_coverage + yc + diet + (1|tank)
final_formula_a <- a         ~ section_coverage + yc + diet + (1|tank)

full_b <- lmer(final_formula_b, data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
full_L <- lmer(final_formula_L, data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
full_a <- lmer(final_formula_a, data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))

# Print full summaries
for (lst in list(list(full_b, "b*"),
                 list(full_L, "L*"),
                 list(full_a, "a*"))) {
  mod <- lst[[1]]; response <- lst[[2]]

  print(summary(mod))
  print(as.data.frame(VarCorr(mod))[, c("grp", "var1", "vcov", "sdcor")],
        row.names = FALSE)

  if (length(fixef(mod)) > 2) {             
    print(round(car::vif(mod), 3))
  } else {
    cat("  VIF: not computed (single fixed effect)\n")
  }

r2 <- performance::r2(mod)
  cat(sprintf("  R² marginal:    %.3f\n", r2$R2_marginal))
  cat(sprintf("  R² conditional: %.3f\n", r2$R2_conditional))
  cat("\n")

}

## CONSOLIDATED SUMMARY TABLE 

# AIC summary: just show dAIC column per response side by side
aic_wide <- aic_all |>
  select(response, model, AIC, dAIC) |>
  pivot_wider(names_from = response,
              values_from = c(AIC, dAIC),
              names_glue = "{response}_{.value}")

print(aic_wide, row.names = FALSE)

lrt_wide <- lrt_all |>
  select(response, term_added, chi2, df, p_value, sig) |>
  pivot_wider(names_from = response,
              values_from = c(chi2, p_value, sig),
              names_glue = "{response}_{.value}")
print(lrt_wide, row.names = FALSE)

## ESTIMATED MARGINAL MEANS & PAIRWISE CONTRASTS

for (lst in list(list(full_b, "b*"),
                 list(full_L, "L*"),
                 list(full_a, "a*"))) {
  mod <- lst[[1]]; response <- lst[[2]]
  
  emm <- emmeans(mod, specs = "section_coverage")
  print(emm)
  print(pairs(emm, adjust = "tukey"))
  
  if (response != "L*") {
    for (term in c("yc", "diet")) {
    cat(sprintf("\n--- %s: section_coverage ---\n", response))
    emm <- emmeans(mod, specs = "section_coverage")
    print(emm)
    print(pairs(emm, adjust = "tukey"))
    }
  }
}

## EXTRACT COEFFICIENTS FOR PLOTTING

extract_coefs <- function(mod, response) {
  coefs <- as.data.frame(coef(summary(mod)))
  coefs$term <- rownames(coefs)
  coefs |>
    rename(estimate = Estimate, se = `Std. Error`,
           t = `t value`, p = `Pr(>|t|)`) |>
    filter(term != "(Intercept)") |>
    mutate(
      response   = response,
      ci_lo      = estimate - 1.96 * se,
      ci_hi      = estimate + 1.96 * se,
      sig        = ifelse(p < 0.05, "p < 0.05", "p \u2265 0.05"),
      term_clean = case_when(
      term == "section_coveragedouble" ~ "Section coverage: double vs single",
      term == "yc21"                   ~ "Year class: 21 vs 22",        
      term == "dietLight blue"         ~ "Diet: Light blue vs Pink",    
      term == "dietMari"               ~ "Diet: Mari vs Pink",         
      term == "dietPurple"             ~ "Diet: Purple vs Pink",        
      term == "dietRidley"             ~ "Diet: Ridley vs Pink",        
      TRUE                             ~ term
      ),
      term_group = case_when(
        str_detect(term, "section") ~ "Light exposure",
        str_detect(term, "yc")      ~ "Year class",
        str_detect(term, "diet")    ~ "Diet",
        TRUE                        ~ "Other"
      )
    )
}

coef_df <- bind_rows(
  extract_coefs(full_b, "b*"),
  extract_coefs(full_L, "L*"),
  extract_coefs(full_a, "a*")
) |>
  mutate(response = factor(response, levels = c("b*", "L*", "a*")))

## FOREST PLOT

sig_cols <- c("p < 0.05" = "#1F618D", "p \u2265 0.05" = "#AED6F1")

p_forest <- ggplot(coef_df,
                   aes(x = estimate,
                       y = reorder(term_clean, estimate),
                       colour = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.25, linewidth = 0.7, alpha = 0.85) +
  geom_point(size = 3) +
  facet_wrap(~response, scales = "free_x", nrow = 1) +
  scale_colour_manual(values = sig_cols, name = NULL) +
  labs(
    x        = "Estimate (95% CI)",
    y        = NULL,
    title    = "Fixed effect estimates — LMM (b*, L*, a*)",
    subtitle = "Random: (1|tank) · Reference: single coverage, YC22, diet Pink\nBlue = p < 0.05"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.2, colour = "grey90"),
    strip.text         = element_text(size = 12, face = "bold"),
    plot.title         = element_text(size = 13, face = "bold"),
    plot.subtitle      = element_text(size = 9,  colour = "grey40"),
    legend.position    = "bottom",
    axis.text.y        = element_text(size = 10)
  )

print(p_forest)

## EMM PLOTS

emm_data <- function(mod, term, response) {
  emmeans(mod, specs = term) |>
    as.data.frame() |>
    mutate(response = response) |>
    rename(level = !!sym(term))
}

resp_cols <- c("b*" = "#C4A24A", "L*" = "#5B8FA8", "a*" = "#7A9E5A")

emm_plot <- function(emm_df, x_lab, title) {
  ggplot(emm_df,
         aes(x = level, y = emmean,
             ymin = lower.CL, ymax = upper.CL,
             colour = response)) +
    geom_errorbar(width = 0.18, linewidth = 0.8,
                  position = position_dodge(0.4)) +
    geom_point(size = 3.2, position = position_dodge(0.4)) +
    facet_wrap(~response, scales = "free_y", nrow = 1) +
    scale_colour_manual(values = resp_cols, guide = "none") +
    labs(x = x_lab, y = "Estimated marginal mean (95% CI)",
         title    = title,
         subtitle = "Adjusted for all other fixed effects") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text       = element_text(size = 11, face = "bold"),
      plot.title       = element_text(size = 12, face = "bold"),
      plot.subtitle    = element_text(size = 9,  colour = "grey40"),
      axis.text.x      = element_text(size = 10)
    )
}

emm_sec  <- bind_rows(emm_data(full_b,"section_coverage","b*"),
                      emm_data(full_L,"section_coverage","L*"),
                      emm_data(full_a,"section_coverage","a*"))
emm_yc   <- bind_rows(emm_data(full_b,"yc","b*"),
                      emm_data(full_a,"yc","a*"))
emm_diet <- bind_rows(emm_data(full_b,"diet","b*"),
                      emm_data(full_a,"diet","a*"))

p_emm_sec  <- emm_plot(emm_sec,  "Section coverage", "EMM: section coverage")
p_emm_yc   <- emm_plot(emm_yc,   "Year class",       "EMM: year class")
p_emm_diet <- emm_plot(emm_diet, "Diet",              "EMM: diet") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_emm_sec)
print(p_emm_yc)
print(p_emm_diet)

## ASSUMPTION DIAGNOSTICS

diag_plots <- function(mod, response, filename) {
  df_d <- data.frame(fitted = fitted(mod), resid = residuals(mod))

  p_rv <- ggplot(df_d, aes(x = fitted, y = resid)) +
    geom_point(alpha = 0.2, size = 0.8, colour = "#2E6E9E") +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_smooth(method = "loess", se = FALSE,
                colour = "#E07B39", linewidth = 0.8) +
    labs(x = "Fitted", y = "Residuals",
         title = paste0("Residuals vs fitted \u2014 ", response)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  p_qq <- ggplot(df_d, aes(sample = resid)) +
    stat_qq(alpha = 0.25, size = 0.7, colour = "#2E6E9E") +
    stat_qq_line(colour = "#E07B39", linewidth = 0.8) +
    labs(title = paste0("QQ plot \u2014 ", response),
         x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  ggsave(filename, p_rv | p_qq, width = 10, height = 4, dpi = 150)
  cat("  Saved:", filename, "\n")

  set.seed(42)
  sw <- shapiro.test(sample(residuals(mod),
                            min(length(residuals(mod)), 4999)))
  cat(sprintf("  Shapiro-Wilk (%s): W = %.4f, p = %.4f\n",
              response, sw$statistic, sw$p.value))
}

diag_plots(full_b, "b*", "plot_10_diagnostics_b.png")
diag_plots(full_L, "L*", "plot_11_diagnostics_L.png")
diag_plots(full_a, "a*", "plot_12_diagnostics_a.png")

### Script 04: Sensitivity test to substantiate dietary affect

## Ridley, Light blue and Purple are fully confounded with YC. 
## Repeat modelling steps with sensitive dataset restricting to Mari & Pink (both YCs represented)

# Create sensitive dataset

df_balanced <- df_mod |>
  filter(diet %in% c("Mari", "Pink"))

cat(sprintf("  Full dataset:     n = %d across %d tanks\n",
            nrow(df_mod), n_distinct(df_mod$tank)))
cat(sprintf("  Balanced subset:  n = %d across %d tanks (Mari + Pink only)\n\n",
            nrow(df_balanced), n_distinct(df_balanced$tank)))

# Confirm representation in dataset

df_balanced |>
  distinct(tank, yc, diet) |>
  count(yc, diet) |>
  as.data.frame() |>
  print(row.names = FALSE)

# ICC 

icc_sens <- data.frame(
  response = c("b*", "L*", "a*"),
  ICC = round(c(
    performance::icc(lmer(b         ~ 1 + (1|tank), data = df_balanced,
                          REML = TRUE, control = lmerControl(optimizer = "bobyqa")))$ICC_adjusted,
    performance::icc(lmer(lightness ~ 1 + (1|tank), data = df_balanced,
                          REML = TRUE, control = lmerControl(optimizer = "bobyqa")))$ICC_adjusted,
    performance::icc(lmer(a         ~ 1 + (1|tank), data = df_balanced,
                          REML = TRUE, control = lmerControl(optimizer = "bobyqa")))$ICC_adjusted
  ), 3)
)

print(icc_sens, row.names = FALSE)

## FORWARD MODEL SELECTION 

# Have reused fit_steps() and aic_table() from initial modelling script, applied to df_balanced

sens_models_b <- fit_steps("b",         data = df_balanced)
sens_models_L <- fit_steps("lightness", data = df_balanced)
sens_models_a <- fit_steps("a",         data = df_balanced)

aic_sens <- bind_rows(
  aic_table(sens_models_b, "b*"),
  aic_table(sens_models_L, "L*"),
  aic_table(sens_models_a, "a*")
)

for (resp in c("b*", "L*", "a*")) {
  cat(sprintf("--- %s ---\n", resp))
  tbl <- aic_sens |> filter(response == resp) |> select(-response)
  print(tbl, row.names = FALSE)
  best <- tbl$model[which.min(tbl$AIC)]
  cat(sprintf("  Best by AIC: %s\n", best))
  if (nrow(tbl |> filter(dAIC < 2)) > 1)
    cat("  Note: Multiple models within dAIC < 2 — consider parsimony.\n")
  cat("\n")
}

# Model selection is the same

## SEQUENTIAL LRTs 

lrt_sens <- bind_rows(
  lrt_sequential(sens_models_b, "b*"),
  lrt_sequential(sens_models_L, "L*"),
  lrt_sequential(sens_models_a, "a*")
)

for (resp in c("b*", "L*", "a*")) {
  cat(sprintf("--- %s ---\n", resp))
  print(
    lrt_sens |> filter(response == resp) |> select(term_added, chi2, df, p_value, sig),
    row.names = FALSE
  )
  cat("\n")
}

# REML FINAL MODELS 

sens_b <- lmer(b         ~ section_coverage + yc + diet + (1|tank),
               data = df_balanced, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
sens_L <- lmer(lightness ~ section_coverage                + (1|tank),
               data = df_balanced, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
sens_a <- lmer(a         ~ section_coverage + yc + diet + (1|tank),
               data = df_balanced, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))

for (lst in list(list(sens_b, "b*"),
                 list(sens_L, "L*"),
                 list(sens_a, "a*"))) {
  mod <- lst[[1]]; resp <- lst[[2]]
   print(summary(mod))

  print(as.data.frame(VarCorr(mod))[, c("grp", "var1", "vcov", "sdcor")],
        row.names = FALSE)

  # VIF only meaningful with 2+ fixed effects
  if (length(fixef(mod)) > 2) {
    cat("\n  VIF:\n")
    print(round(car::vif(mod), 3))
  } else {
    cat("\n  VIF: not computed (single fixed effect)\n")
  }

  r2 <- performance::r2(mod)
  cat(sprintf("\n  R² marginal:    %.3f\n", r2$R2_marginal))
  cat(sprintf("  R² conditional: %.3f\n\n", r2$R2_conditional))
}

# COEFFICIENT COMPARISON: full vs sensitivity 

extract_key_coefs <- function(mod, label) {
  coefs <- as.data.frame(coef(summary(mod)))
  coefs$term <- rownames(coefs)
  coefs |>
    filter(term != "(Intercept)") |>
    transmute(
      term,
      model    = label,
      estimate = round(Estimate,      3),
      se       = round(`Std. Error`,  3),
      p        = round(`Pr(>|t|)`,    4),
      sig      = case_when(
        `Pr(>|t|)` < 0.001 ~ "***",
        `Pr(>|t|)` < 0.01  ~ "**",
        `Pr(>|t|)` < 0.05  ~ "*",
        `Pr(>|t|)` < 0.1   ~ ".",
        TRUE               ~ "ns"
      )
    )
}

# b* comparison
coef_compare_b <- bind_rows(
  extract_key_coefs(full_b, "Full model (n=1265)"),
  extract_key_coefs(sens_b, "Balanced subset (n=697)")
) |>
  arrange(term, model)

print(coef_compare_b, row.names = FALSE)

# a* comparison
coef_compare_a <- bind_rows(
  extract_key_coefs(full_a, "Full model (n=1265)"),
  extract_key_coefs(sens_a, "Balanced subset (n=697)")
) |>
  arrange(term, model)

print(coef_compare_a, row.names = FALSE)


# Now have abalone width and length. Is this a better fixed effect than age?

### Script 05: Size metrics — collinearity with YC + LMM comparison (yc vs length)

# Are size metrics (length, width, area) correlated with each other?
# Does length_mm perform better than yc as a predictor of lip colour?

# Prepare data

# Add numeric yc for correlation purposes
df <- df |>
  mutate(yc_num = as.numeric(as.character(yc)))

str(df)
 
resp_cols <- c("b*" = "#C4A24A", "L*" = "#5B8FA8", "a*" = "#7A9E5A")
yc_cols   <- c("21" = "#E07B39", "22" = "#5B8FA8")
 
# HISTOGRAMS: size metrics coloured by YC

make_hist <- function(var, xlab, binwidth = NULL) {
  if (is.null(binwidth)) {
    rng      <- range(df[[var]], na.rm = TRUE)
    binwidth <- (rng[2] - rng[1]) / 40
  }
  summ <- df |>
    filter(!is.na(.data[[var]])) |>
    group_by(yc) |>
    summarise(med = median(.data[[var]], na.rm = TRUE), .groups = "drop")

  ggplot(df |> filter(!is.na(.data[[var]])),
         aes(x = .data[[var]], fill = yc, colour = yc)) +
    geom_histogram(binwidth = binwidth, alpha = 0.55,
                   position = "identity", linewidth = 0.2) +
    geom_density(aes(y = after_stat(density) *
                       nrow(df |> filter(!is.na(.data[[var]]))) * binwidth),
                 linewidth = 0.7, alpha = 0) +
    geom_vline(data = summ, aes(xintercept = med, colour = yc),
               linetype = "dashed", linewidth = 0.7) +
    scale_fill_manual(values = yc_cols, name = "Year class") +
    scale_colour_manual(values = yc_cols, name = "Year class") +
    labs(x = xlab, y = "Count",
         caption = "Dashed lines = median per year class") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "right",
          plot.caption     = element_text(size = 8, colour = "grey50"))
}

p_len  <- make_hist("length", "Length (mm)")
p_wid  <- make_hist("width",  "Width (mm)")
p_area <- make_hist("area",   "Area (mm²)", binwidth = 300)

p_size_hists <- (p_len / p_wid / p_area) +
  plot_annotation(
    title    = "Size metrics by year class",
    subtitle = paste0(
      "n = ", sum(!is.na(df$length)), " abalone with size data · ",
      "YC21 = orange, YC22 = blue"
    ),
    theme = theme(
      plot.title    = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, colour = "grey40")
    )
  )

print(p_size_hists)

ggsave("plot_size_histograms_by_yc.png", p_size_hists,
       width = 9, height = 10, dpi = 150)


# COLLINEARITY: size metrics × each other × YC

cor_matrix <- df |>
  select(length, width, area, yc_num) |>  
  cor(use = "complete.obs") |>           
  round(3)
print(cor_matrix)

# Flag pairs that exceed threshold
pairs_to_check <- combn(c("length", "width", "area", "yc_num"), 2, simplify = FALSE)

for (pair in pairs_to_check) {
  r        <- cor_matrix[pair[1], pair[2]]
  severity <- case_when(
    abs(r) > 0.85 ~ "SEVERE — cannot include both",
    abs(r) > 0.70 ~ "Moderate — include with caution",
    TRUE          ~ "Low — safe to include together"
  )
  cat(sprintf("  %-10s x %-10s  r = %+.3f  [%s]\n",
              pair[1], pair[2], r, severity))
}

## Correlation assessment confirms that YC and size cannot co-exist in same GLMM

## Determine if size is a better predictor by running models and comparing

# MODELLING DATASETS 

# Model A dataset: includes yc (Script 03 approach)
df_modA <- df |>
  filter(!is.na(section_coverage), !is.na(yc), !is.na(diet),
         !is.na(b), !is.na(a), !is.na(lightness),
         lightness != 0) |>
  mutate(
    section_coverage = factor(section_coverage, ordered = FALSE),
    yc               = factor(yc,               ordered = FALSE),
    diet             = factor(diet),
    section_coverage = relevel(section_coverage, ref = "single"),
    yc               = relevel(yc,               ref = "21"),
    diet             = relevel(diet,             ref = "Mari")
  )

# Model B dataset: includes length (must also have size data)
df_modB <- df |>
  filter(!is.na(section_coverage), !is.na(diet),
         !is.na(b), !is.na(a), !is.na(lightness),
         !is.na(length), lightness != 0) |>
  mutate(
    section_coverage = factor(section_coverage, ordered = FALSE),
    diet             = factor(diet),
    section_coverage = relevel(section_coverage, ref = "single"),
    diet             = relevel(diet,             ref = "Mari"),
    length_scaled    = scale(length)[, 1]   # z-score: 1 unit = 1 SD (~14mm)
  )

# Confirm size of datasets

cat(sprintf("  Model A (yc):    n = %d, tanks = %d\n",
            nrow(df_modA), n_distinct(df_modA$tank)))
cat(sprintf("  Model B (length): n = %d, tanks = %d\n",
            nrow(df_modB), n_distinct(df_modB$tank)))
cat(sprintf("  length SD = %.1f mm (1 unit of length_scaled = %.1f mm)\n\n",
            sd(df_modB$length), sd(df_modB$length)))

# FORWARD SELECTION: MODEL A (yc)

fit_A <- function(response) {
  list(
    m0 = lmer(as.formula(paste(response, "~ 1 + (1|tank)")),
              data = df_modA, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
    m1 = lmer(as.formula(paste(response, "~ section_coverage + (1|tank)")),
              data = df_modA, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
    m2 = lmer(as.formula(paste(response, "~ section_coverage + yc + (1|tank)")),
              data = df_modA, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
    m3 = lmer(as.formula(paste(response, "~ section_coverage + yc + diet + (1|tank)")),
              data = df_modA, REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
  )
}

# FORWARD SELECTION: MODEL B (length) 

fit_B <- function(response) {
  list(
    m0 = lmer(as.formula(paste(response, "~ 1 + (1|tank)")),
              data = df_modB, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
    m1 = lmer(as.formula(paste(response, "~ section_coverage + (1|tank)")),
              data = df_modB, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
    m2 = lmer(as.formula(paste(response, "~ section_coverage + length_scaled + (1|tank)")),
              data = df_modB, REML = FALSE, control = lmerControl(optimizer = "bobyqa")),
    m3 = lmer(as.formula(paste(response, "~ section_coverage + length_scaled + diet + (1|tank)")),
              data = df_modB, REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
  )
}

# Fit both models
modsA <- list(b = fit_A("b"), L = fit_A("lightness"), a = fit_A("a"))
modsB <- list(b = fit_B("b"), L = fit_B("lightness"), a = fit_B("a"))

# Generic AIC table function
make_aic <- function(mods, labels, response, model_name) {
  aic_vals <- sapply(mods, AIC)
  bic_vals <- sapply(mods, BIC)
  loglik   <- sapply(mods, logLik)
  df_used  <- sapply(mods, function(m) attr(logLik(m), "df"))
  data.frame(
    model_type = model_name,
    response   = response,
    step       = labels,
    df         = df_used,
    logLik     = round(loglik,   2),
    AIC        = round(aic_vals, 2),
    dAIC       = round(aic_vals - min(aic_vals), 2),
    BIC        = round(bic_vals, 2),
    dBIC       = round(bic_vals - min(bic_vals), 2)
  )
}

labelsA <- c("m0: null", "m1: + section_coverage",
             "m2: + section_coverage + yc",
             "m3: + section_coverage + yc + diet")
labelsB <- c("m0: null", "m1: + section_coverage",
             "m2: + section_coverage + length_mm",
             "m3: + section_coverage + length_mm + diet")

aic_A <- bind_rows(
  make_aic(modsA$b, labelsA, "b*", "A: yc"),
  make_aic(modsA$L, labelsA, "L*", "A: yc"),
  make_aic(modsA$a, labelsA, "a*", "A: yc")
)
aic_B <- bind_rows(
  make_aic(modsB$b, labelsB, "b*", "B: length"),
  make_aic(modsB$L, labelsB, "L*", "B: length"),
  make_aic(modsB$a, labelsB, "a*", "B: length")
)

# Print AIC tables
for (resp in c("b*", "L*", "a*")) {
  cat(sprintf("══ %s ══\n", resp))
  cat("  Model A (yc):\n")
  print(aic_A |> filter(response == resp) |> select(step, df, AIC, dAIC, BIC, dBIC),
        row.names = FALSE)
  cat(sprintf("    Best: %s\n\n",
              (aic_A |> filter(response == resp))$step[which.min(
                (aic_A |> filter(response == resp))$AIC)]))

  cat("  Model B (length):\n")
  print(aic_B |> filter(response == resp) |> select(step, df, AIC, dAIC, BIC, dBIC),
        row.names = FALSE)
  cat(sprintf("    Best: %s\n\n",
              (aic_B |> filter(response == resp))$step[which.min(
                (aic_B |> filter(response == resp))$AIC)]))
}

# Generic LRT function
make_lrt <- function(mods, terms, response, model_name) {
  mapply(function(r, f, term) {
    lt <- anova(r, f)
    data.frame(
      model_type = model_name,
      response   = response,
      term_added = term,
      chi2       = round(lt$Chisq[2], 3),
      df         = lt$Df[2],
      p_value    = round(lt$`Pr(>Chisq)`[2], 4),
      sig        = case_when(
        lt$`Pr(>Chisq)`[2] < 0.001 ~ "***",
        lt$`Pr(>Chisq)`[2] < 0.01  ~ "**",
        lt$`Pr(>Chisq)`[2] < 0.05  ~ "*",
        lt$`Pr(>Chisq)`[2] < 0.1   ~ ".",
        TRUE                        ~ "ns"
      )
    )
  },
  list(mods$m0, mods$m1, mods$m2),
  list(mods$m1, mods$m2, mods$m3),
  terms,
  SIMPLIFY = FALSE) |> bind_rows()
}

lrt_A <- bind_rows(
  make_lrt(modsA$b, c("section_coverage", "yc",     "diet"), "b*", "A: yc"),
  make_lrt(modsA$L, c("section_coverage", "yc",     "diet"), "L*", "A: yc"),
  make_lrt(modsA$a, c("section_coverage", "yc",     "diet"), "a*", "A: yc")
)
lrt_B <- bind_rows(
  make_lrt(modsB$b, c("section_coverage", "length_mm", "diet"), "b*", "B: length"),
  make_lrt(modsB$L, c("section_coverage", "length_mm", "diet"), "L*", "B: length"),
  make_lrt(modsB$a, c("section_coverage", "length_mm", "diet"), "a*", "B: length")
)

for (resp in c("b*", "L*", "a*")) {
  cat(sprintf("══ %s ══\n", resp))
  cat("  Model A (yc):\n")
  print(lrt_A |> filter(response == resp) |>
          select(term_added, chi2, df, p_value, sig),
        row.names = FALSE)
  cat("  Model B (length):\n")
  print(lrt_B |> filter(response == resp) |>
          select(term_added, chi2, df, p_value, sig),
        row.names = FALSE)
  cat("\n")
}

# RUN COMPARISON BETWEEN TWO MODELS

comparison <- bind_rows(lrt_A, lrt_B) |>
  filter(term_added %in% c("yc", "length_mm")) |>
  select(response, model_type, term_added, chi2, df, p_value, sig) |>
  arrange(response, model_type)

print(comparison, row.names = FALSE)

# REML FINAL MODELS 

# Model A — refit with REML
full_A_b <- lmer(b         ~ section_coverage + yc + diet + (1|tank),
                 data = df_modA, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
full_A_L <- lmer(lightness ~ section_coverage             + (1|tank),
                 data = df_modA, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
full_A_a <- lmer(a         ~ section_coverage + yc + diet + (1|tank),
                 data = df_modA, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))

# Model B — refit with REML
full_B_b <- lmer(b         ~ section_coverage + length_scaled + diet + (1|tank),
                 data = df_modB, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
full_B_L <- lmer(lightness ~ section_coverage                        + (1|tank),
                 data = df_modB, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
full_B_a <- lmer(a         ~ section_coverage + length_scaled + diet + (1|tank),
                 data = df_modB, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))

print_model <- function(mod, label) {
  cat(sprintf("\n%s\n--- %s ---\n%s\n", strrep("-", 55), label, strrep("-", 55)))
  print(summary(mod))
  cat("\n  Random effects:\n")
  print(as.data.frame(VarCorr(mod))[, c("grp", "var1", "vcov", "sdcor")],
        row.names = FALSE)
  if (length(fixef(mod)) > 2) {
    cat("\n  VIF:\n")
    print(round(car::vif(mod), 3))
  }
  r2 <- performance::r2(mod)
  cat(sprintf("\n  R² marginal:    %.3f\n", r2$R2_marginal))
  cat(sprintf("  R² conditional: %.3f\n\n", r2$R2_conditional))
}

for (lst in list(
  list(full_A_b, "Model A — b* (yc)"),
  list(full_B_b, "Model B — b* (length)"),
  list(full_A_L, "Model A — L* (yc, section only)"),
  list(full_B_L, "Model B — L* (length, section only)"),
  list(full_A_a, "Model A — a* (yc)"),
  list(full_B_a, "Model B — a* (length)")
)) print_model(lst[[1]], lst[[2]])

# COMPARISON TABLE

r2_compare <- lapply(list(
  list(full_A_b, "b*", "A: yc"),
  list(full_B_b, "b*", "B: length"),
  list(full_A_L, "L*", "A: yc"),
  list(full_B_L, "L*", "B: length"),
  list(full_A_a, "a*", "A: yc"),
  list(full_B_a, "a*", "B: length")
), function(x) {
  r2 <- performance::r2(x[[1]])
  data.frame(response = x[[2]], model = x[[3]],
             R2_marginal    = round(r2$R2_marginal,    3),
             R2_conditional = round(r2$R2_conditional, 3))
}) |> bind_rows()

print(r2_compare, row.names = FALSE)

# MARGINAL EFFECT PLOT — length 

pred_grid <- expand.grid(
  length         = seq(min(df_modB$length), max(df_modB$length), length.out = 100),
  section_coverage = factor("single", levels = levels(df_modB$section_coverage)),
  diet             = factor("Mari",   levels = levels(df_modB$diet))
) |>
  mutate(length_scaled = (length - mean(df_modB$length)) / sd(df_modB$length))

pred_df <- pred_grid |>
  mutate(
    b = predict(full_B_b, newdata = pred_grid, re.form = NA),
    L = predict(full_B_L, newdata = pred_grid, re.form = NA),
    a = predict(full_B_a, newdata = pred_grid, re.form = NA)
  ) |>
  pivot_longer(cols = c(b, L, a), names_to = "response", values_to = "predicted") |>
  mutate(response = factor(response,
                            levels = c("b", "L", "a"),
                            labels = c("b*", "L*", "a*")))

p_length_effect <- ggplot(pred_df,
                          aes(x = length, y = predicted, colour = response)) +
  geom_line(linewidth = 1) +
  facet_wrap(~response, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = resp_cols, guide = "none") +
  labs(x = "Shell length (mm)", y = "Predicted colour metric",
       title    = "Marginal effect of shell length on lip colour (Model B)",
       subtitle = "section_coverage = single, diet = Mari · population mean (re.form = NA)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text       = element_text(size = 11, face = "bold"),
        plot.title       = element_text(size = 13, face = "bold"),
        plot.subtitle    = element_text(size = 9,  colour = "grey40"))

print(p_length_effect)
ggsave("plot_length_marginal_effect.png", p_length_effect,
       width = 11, height = 4, dpi = 150)

### Conclusion: yc is a better predictor of CIELAB metrics than length 

