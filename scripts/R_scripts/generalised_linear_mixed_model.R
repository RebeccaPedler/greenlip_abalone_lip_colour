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

df_raw <- read.csv("lip_colour_commercial.csv", stringsAsFactors = FALSE)

names(df_raw) <- c("date", "farm", "section", "tank", "yc", "diet",
                   "shade_coverage", "section_coverage", "tank_position",
                   "abalone_number", "image_id",
                   "lightness", "a", "b", "hue", "saturation", "brightness",
                   "lip_score")

df_raw <- df_raw |>
  mutate(across(where(is.character), str_trim)) |>
  filter(!is.na(farm))

df_raw <- df_raw |> filter(!is.na(section_coverage)) # Delete NA
df_raw <- df_raw |> filter(lightness != 0) # Delete zero

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
    diet          = factor(diet),
    tank_position = factor(tank_position),

    # Numeric
    lightness  = as.numeric(lightness),
    a          = as.numeric(a),
    b          = as.numeric(b),

    abalone_id = paste(tank, abalone_number, sep = "_")
  )

str(df)

### OBSERVE LEVELS AND OBSERVATIONS FOR EACH FACTOR

factor_vars <- c("farm", "section", "yc", "diet",
                 "shade_coverage", "section_coverage")

for (v in factor_vars) {
  cat(sprintf("\n--- %s ---\n", v))
  df |>
    count(.data[[v]], name = "n") |>
    mutate(pct = round(n / sum(n) * 100, 1)) |>
    as.data.frame() |>
    print(row.names = FALSE)
}

# Tank-level summaries
cat("\n--- Tanks per section_coverage ---\n")
df |>
  distinct(tank, section_coverage) |>
  count(section_coverage) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n--- Tanks per section_coverage x shade_coverage (confounding check) ---\n")
df |>
  distinct(tank, section_coverage, shade_coverage) |>
  count(section_coverage, shade_coverage) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n--- Abalone per tank ---\n")
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

cat("\n=== Mean b* by section_coverage x YC ===\n")
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

cat("\n=== Mean b* by diet ===\n")
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

cat("\n=== Mean b* by YC ===\n")
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

cat("Tank-level dataset:", nrow(tank_df), "tanks\n\n")

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

pairs <- list(
  c("section_coverage", "yc"),
  c("section_coverage", "diet"),
  c("yc",              "diet")
)

cat("=== CRAMER'S V — PAIRWISE ASSOCIATIONS (tank level, n =",
    nrow(tank_df), ") ===\n\n")

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

cat("\n--- section_coverage x YC ---\n")
table(tank_df$section_coverage, tank_df$yc) |>
  addmargins() |>
  print()

cat("\n--- section_coverage x diet ---\n")
table(tank_df$section_coverage, tank_df$diet) |>
  addmargins() |>
  print()

cat("\n--- YC x diet ---\n")
table(tank_df$yc, tank_df$diet) |>
  addmargins() |>
  print()

cat("\n--- Three-way: section_coverage x YC x diet (n tanks) ---\n")
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

# SAVE PLOT

ggsave("plot_07_predictor_crossing.png", p_tile, width = 9, height = 5, dpi = 150)

cat("\nSaved: plot_07_predictor_crossing.png\n")
cat("Saved: plot_07_mosaic_yc_diet.pdf\n")
cat("Saved: plot_08_mosaic_sec_diet.pdf\n")

### Script 03: Linear Mixed Models — b*, L*, a* ~ section_coverage + yc + diet

# Fixed:  section_coverage + YC + diet
# Random: (1|section/tank) - multiple observations per tank

# Note: Purple fed only to YC22, Ridley only to YC21 

## PREPARE MODELLING DATASET

df_mod <- df |>
  filter(!is.na(section_coverage), !is.na(yc), !is.na(diet),
         !is.na(b), !is.na(a), !is.na(lightness)) |>
  mutate(
    section_coverage = factor(section_coverage, ordered = FALSE),
    yc               = factor(yc,               ordered = FALSE),
    section_coverage = relevel(section_coverage, ref = "single"),
    yc               = relevel(yc,               ref = "21"),
  )

print(as.data.frame(table(df_mod$diet)), row.names = FALSE)


## Determine ICC of null dataset (no moderators)

null_b <- lmer(b         ~ 1 + (1|tank), data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
null_L <- lmer(lightness ~ 1 + (1|tank), data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
null_a <- lmer(a         ~ 1 + (1|tank), data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))

data.frame(
  response = c("b*", "L*", "a*"),
  icc      = round(c(performance::icc(null_b)$ICC_adjusted,
                     performance::icc(null_L)$ICC_adjusted,
                     performance::icc(null_a)$ICC_adjusted), 3)
) |> as.data.frame() |> print(row.names = FALSE)

# ICC > 0.1 confirms mixed model necessary

## FIT FULL MODELS [Fixed:  section_coverage + yc & Random: (1|tank)]

# ML for likelihood ratio tests
full_b_ml <- lmer(b         ~ section_coverage + yc + (1|tank),
                  data = df_mod, REML = FALSE,
                  control = lmerControl(optimizer = "bobyqa"))
full_L_ml <- lmer(lightness ~ section_coverage + yc + (1|tank),
                  data = df_mod, REML = FALSE,
                  control = lmerControl(optimizer = "bobyqa"))
full_a_ml <- lmer(a         ~ section_coverage + yc + (1|tank),
                  data = df_mod, REML = FALSE,
                  control = lmerControl(optimizer = "bobyqa"))

# REML for final estimates
full_b <- lmer(b         ~ section_coverage + yc + (1|tank),
               data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
full_L <- lmer(lightness ~ section_coverage + yc + (1|tank),
               data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
full_a <- lmer(a         ~ section_coverage + yc + (1|tank),
               data = df_mod, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))


# Print model summaries

for (lst in list(list(full_b, "b*"),
                 list(full_L, "L*"),
                 list(full_a, "a*"))) {
  mod <- lst[[1]]; response <- lst[[2]]
  cat(sprintf("\n%s\n=== LMM: %s ===\n%s\n",
              strrep("-", 55), response, strrep("-", 55)))
  print(summary(mod))

  cat("\n--- Random effects variance components ---\n")
  print(as.data.frame(VarCorr(mod))[, c("grp","var1","vcov","sdcor")],
        row.names = FALSE)

  cat("\n--- VIF ---\n")
  print(round(car::vif(mod), 3))
}


## LIKELIHOOD RATIO TESTS

lrt_table <- function(full_ml, response) {
  lapply(c("section_coverage", "yc", "diet"), function(term) {
    reduced <- update(full_ml, as.formula(paste(". ~ . -", term)))
    lt      <- anova(reduced, full_ml)
    data.frame(
      response = response,
      term     = term,
      chi2     = round(lt$Chisq[2], 3),
      df       = lt$`Df`[2],
      p_value  = round(lt$`Pr(>Chisq)`[2], 4)
    )
  }) |> bind_rows()
}

lrt_all <- bind_rows(
  lrt_table(full_b_ml, "b*"),
  lrt_table(full_L_ml, "L*"),
  lrt_table(full_a_ml, "a*")
) |>
  mutate(sig = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    p_value < 0.1   ~ ".",
    TRUE            ~ ""
  ))

print(as.data.frame(lrt_all), row.names = FALSE)

## ESTIMATED MARGINAL MEANS & PAIRWISE CONTRASTS

for (lst in list(list(full_b, "b*"),
                 list(full_L, "L*"),
                 list(full_a, "a*"))) {
  mod <- lst[[1]]; response <- lst[[2]]
  for (term in c("section_coverage", "yc", "diet")) {
    cat(sprintf("\n--- %s: %s ---\n", response, term))
    emm <- emmeans(mod, specs = term)
    print(emm)
    cat("  Pairwise contrasts (Tukey):\n")
    print(pairs(emm, adjust = "tukey"))
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
        term == "yc22"                   ~ "Year class: 22 vs 21",
        term == "dietLight blue"         ~ "Diet: Light blue vs Mari",
        term == "dietPink"               ~ "Diet: Pink vs Mari",
        term == "dietPurple"             ~ "Diet: Purple vs Mari",
        term == "dietRidley"             ~ "Diet: Ridley vs Mari",
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
    subtitle = "Random: (1|section/tank) \u00b7 Reference: coverage single, YC21, diet Mari\nBlue = p < 0.05"
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
                      emm_data(full_L,"yc","L*"),
                      emm_data(full_a,"yc","a*"))
emm_diet <- bind_rows(emm_data(full_b,"diet","b*"),
                      emm_data(full_L,"diet","L*"),
                      emm_data(full_a,"diet","a*"))

p_emm_sec  <- emm_plot(emm_sec,  "Section coverage", "EMM: section coverage")
p_emm_yc   <- emm_plot(emm_yc,   "Year class",       "EMM: year class")
p_emm_diet <- emm_plot(emm_diet, "Diet",              "EMM: diet") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


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


## Save plots

ggsave("plot_08_forest.png",       p_forest,   width = 13, height = 6,  dpi = 150)
ggsave("plot_09a_emm_section.png", p_emm_sec,  width = 10, height = 4,  dpi = 150)
ggsave("plot_09b_emm_yc.png",      p_emm_yc,   width = 10, height = 4,  dpi = 150)
ggsave("plot_09c_emm_diet.png",    p_emm_diet, width = 11, height = 4,  dpi = 150)

write.csv(
  coef_df |>
    select(response, term_group, term_clean, estimate, se, ci_lo, ci_hi, t, p) |>
    mutate(across(where(is.numeric), ~round(.x, 4))),
  "lmm_coefficients.csv", row.names = FALSE
)
write.csv(as.data.frame(lrt_all), "lmm_lrt.csv", row.names = FALSE)
