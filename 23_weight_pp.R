rm(list = ls())

# Load --------------------------------------------------------------------

source("00_utils.R")
load("data/span2_clotstudy_p3_feasibility_data.Rdata")

# Output -----------------------------------------------------------------

tables <- list()
plots <- list()
pvalue_interaction <- list()

# Data Processing ----

dt <- dt_feasibility$pp |>
  rename(postop_d0_weight = srg_weight) |>
  mutate(across(c(postop_d0_weight, postop_d1_weight:postop_d8_weight), ~ 100*(.x - postop_d0_weight)/postop_d0_weight,
                .names = "perc_{.col}"))


# Tables ----

## Treatment effect ----

tables$outcome$overall <- dt |>
  select(perc_postop_d1_weight,
         perc_postop_d2_weight,
         perc_postop_d3_weight,
         perc_postop_d4_weight,
         perc_postop_d5_weight,
         perc_postop_d6_weight,
         perc_postop_d7_weight,
         perc_postop_d8_weight) |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$sex <- dt |>
  select(perc_postop_d1_weight,
         perc_postop_d2_weight,
         perc_postop_d3_weight,
         perc_postop_d4_weight,
         perc_postop_d5_weight,
         perc_postop_d6_weight,
         perc_postop_d7_weight,
         perc_postop_d8_weight,
         enro_sex) |>
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site <- dt |>
  select(perc_postop_d1_weight,
         perc_postop_d2_weight,
         perc_postop_d3_weight,
         perc_postop_d4_weight,
         perc_postop_d5_weight,
         perc_postop_d6_weight,
         perc_postop_d7_weight,
         perc_postop_d8_weight,
         site) |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$txas_reperfusion <- dt |>
  select(perc_postop_d1_weight,
         perc_postop_d2_weight,
         perc_postop_d3_weight,
         perc_postop_d4_weight,
         perc_postop_d5_weight,
         perc_postop_d6_weight,
         perc_postop_d7_weight,
         perc_postop_d8_weight,
         txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$clot_length <- dt |>
  select(perc_postop_d1_weight,
         perc_postop_d2_weight,
         perc_postop_d3_weight,
         perc_postop_d4_weight,
         perc_postop_d5_weight,
         perc_postop_d6_weight,
         perc_postop_d7_weight,
         perc_postop_d8_weight,
         clot_length) |>
  nt_describe(group = clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Plots  ----

data_labels$time <- "Time (days)"
data_labels$weight <- "Weight (grams)"

dp <- dt |> 
  select(enro_animal_id, 
         perc_postop_d0_weight,
         perc_postop_d1_weight,
         perc_postop_d2_weight,
         perc_postop_d3_weight,
         perc_postop_d4_weight,
         perc_postop_d5_weight,
         perc_postop_d6_weight,
         perc_postop_d7_weight,
         perc_postop_d8_weight,
         site,
         enro_sex,
         txas_reperfusion,
         clot_length) |>
  pivot_longer(cols = 
                 perc_postop_d0_weight:perc_postop_d8_weight,
               names_to = "time",
               values_to = "perc_weight",
               names_pattern= ("perc_postop_d(.)_weight")) |>
  mutate(time = as.numeric(time))

## Treatment ----

aux <-   dp |> 
  select(time, perc_weight, txas_reperfusion) |>
  nt_profileplot(time = time, group = txas_reperfusion,
                 labels = list(perc_weight = "Change in Weight (%)",
                               txas_reperfusion = "Study Arm",
                               time = "Time"),
                 std_fun_group = plot_weight,
                 colors = colors)

plots$overall$txas_reperfusion <- aux$perc_weight$plot + aux$perc_weight$n.table + 
  plot_layout(nrow = 2, heights = c(0.8, 0.2))


aux <-   dp |> 
  filter(clot_length == 3) |>
  select(time, perc_weight, txas_reperfusion) |>
  nt_profileplot(time = time, group = txas_reperfusion,
                 labels = list(perc_weight = "Change in Weight (%)",
                               txas_reperfusion = "Study Arm",
                               time = "Time"),
                 std_fun_group = plot_weight,
                 colors = colors)

plots$cl3$txas_reperfusion <- aux$perc_weight$plot + aux$perc_weight$n.table + 
  plot_layout(nrow = 2, heights = c(0.8, 0.2))


aux <-   dp |> 
  filter(clot_length == 4) |>
  select(time, perc_weight, txas_reperfusion) |>
  nt_profileplot(time = time, group = txas_reperfusion,
                 labels = list(perc_weight = "Change in Weight (%)",
                               txas_reperfusion = "Study Arm",
                               time = "Time"),
                 std_fun_group = plot_weight,
                 colors = colors)

plots$cl4$txas_reperfusion <- aux$perc_weight$plot + aux$perc_weight$n.table + 
  plot_layout(nrow = 2, heights = c(0.8, 0.2))

## Plot Length ---

aux <-   dp |> 
  select(time, perc_weight, clot_length) |>
  nt_profileplot(time = time, group = clot_length,
                 labels = list(perc_weight = "Change in Weight (%)",
                               clot_length = "Clot Length",
                               time = "Time"),
                 std_fun_group = plot_weight,
                 colors = colors)

plots$overall$clot_length <- aux$perc_weight$plot + aux$perc_weight$n.table + 
  plot_layout(nrow = 2, heights = c(0.8, 0.2))

# Test of Hypotheses ----

## Interaction ----

dm <- dt |> 
  select(enro_animal_id,
         perc_postop_d1_weight,
         perc_postop_d2_weight,
         perc_postop_d3_weight,
         perc_postop_d4_weight,
         perc_postop_d5_weight,
         perc_postop_d6_weight,
         perc_postop_d7_weight,
         perc_postop_d8_weight,
         site,
         enro_sex,
         txas_reperfusion,
         clot_length) |>
  pivot_longer(cols = 
                 perc_postop_d1_weight:perc_postop_d8_weight,
               names_to = "time",
               values_to = "perc_weight",
               names_pattern= ("perc_postop_d(.)_weight")) |>
  mutate(time = as.factor(time)) |>
  droplevels()

dm_cl3 <- dm |> filter(clot_length == 3)
dm_cl4 <- dm |> filter(clot_length == 4)

### Overall ----

aux <- list()

fit <- lmer(perc_weight ~ site + enro_sex + clot_length +
              txas_reperfusion*time + (1|enro_animal_id),
            data = dm, REML = FALSE)
fit0 <- lmer(perc_weight ~ site + enro_sex + clot_length +
              txas_reperfusion + time + (1|enro_animal_id),
            data = dm, REML = FALSE)
aux$txas_reperfusion <- 
  tibble(Comparison = "Study Arm x Time", 
             p.value = anova(fit, fit0)$`Pr(>Chisq)`[2])

fit <- lmer(perc_weight ~ site + enro_sex + txas_reperfusion +
              clot_length*time + (1|enro_animal_id),
            data = dm, REML = FALSE)
fit0 <- lmer(perc_weight ~ site + enro_sex + txas_reperfusion +
               clot_length + time + (1|enro_animal_id),
             data = dm, REML = FALSE)
aux$clot_length <- 
  tibble(Comparison = "Clot Length x Time", 
         p.value = anova(fit, fit0)$`Pr(>Chisq)`[2])

fit <- lmer(perc_weight ~ site + enro_sex + txas_reperfusion +
              clot_length*txas_reperfusion*time + (1|enro_animal_id),
            data = dm, REML = FALSE)
fit0 <- lmer(perc_weight ~ site + enro_sex + txas_reperfusion +
               clot_length*time + txas_reperfusion*time + (1|enro_animal_id),
             data = dm, REML = FALSE)
aux$txas_reperfusion_clot_length <- 
  tibble(Comparison = "Clot Length x Study Arm x Time", 
         p.value = anova(fit, fit0)$`Pr(>Chisq)`[2])

tables$lmer$overall$interaction <- bind_rows(aux) |>
         mutate(p.value = format_pvalue(p.value)) |>
  rename('p value' = p.value)

### Clot Length 3 ----



aux <- list()

fit <- lmer(perc_weight ~ site + enro_sex + 
              txas_reperfusion*time + (1|enro_animal_id),
            data = dm_cl3, REML = FALSE)
fit0 <- lmer(perc_weight ~ site + enro_sex + 
               txas_reperfusion + time + (1|enro_animal_id),
             data = dm_cl3, REML = FALSE)
tables$lmer$cl3$interaction <- 
  tibble(Comparison = "Study Arm x Time", 
         p.value = format_pvalue(anova(fit, fit0)$`Pr(>Chisq)`[2])) |>
  rename('p value' = p.value)

### Clot Length 4 ----

aux <- list()

fit <- lmer(perc_weight ~ site + enro_sex + 
              txas_reperfusion*time + (1|enro_animal_id),
            data = dm_cl4, REML = FALSE)
fit0 <- lmer(perc_weight ~ site + enro_sex + 
               txas_reperfusion + time + (1|enro_animal_id),
             data = dm_cl4, REML = FALSE)
tables$lmer$cl4$interaction <- 
  tibble(Comparison = "Study Arm x Time", 
         p.value = format_pvalue(anova(fit, fit0)$`Pr(>Chisq)`[2])) |>
  rename('p value' = p.value)

## At each time point ----

### Overall ----

fit <- lmer(perc_weight ~ site + enro_sex + clot_length +
              txas_reperfusion*time + (1|enro_animal_id),
            data = dm, REML = FALSE)

tables$lmer$overall$txas_reperfusion <-
  comparisons(fit, 
              variables = list(txas_reperfusion = "reference"), 
              by = "time",
              type = "response",
              conf_level = (1 - 0.05/8)) |>
  as.data.frame() |>
  mutate(contrast = str_remove_all(contrast, "mean|\\(|\\)"),
         estimate95 = 
           glue("{round(estimate, 2)} ({round(conf.low, 2)}, {round(conf.high, 2)})")) |>
  select(contrast, time, estimate95, p.value) |>
  group_by(contrast) |>
  mutate(p.value = p.adjust(p.value, method = "holm", n = 8),
         p.value = format_pvalue(p.value)) |>
  rename(Contrast = contrast,
         Time = time,
         'Difference (95%CI)' = estimate95,
         'p value' = p.value) 

fit <- lmer(perc_weight ~ site + enro_sex + txas_reperfusion +
              clot_length*time + (1|enro_animal_id),
            data = dm, REML = FALSE)

tables$lmer$overall$clot_length <-
  comparisons(fit, 
              variables = list(clot_length = "reference"), 
              by = "time",
              type = "response",
              conf_level = (1 - 0.05/8)) |>
  as.data.frame() |>
  mutate(contrast = str_remove_all(contrast, "mean|\\(|\\)"),
         estimate95 = 
           glue("{round(estimate, 2)} ({round(conf.low, 2)}, {round(conf.high, 2)})")) |>
  select(contrast, time, estimate95, p.value) |>
  group_by(contrast) |>
  mutate(p.value = p.adjust(p.value, method = "holm", n = 8),
         p.value = format_pvalue(p.value)) |>
  rename(Contrast = contrast,
         Time = time,
         'Difference (95%CI)' = estimate95,
         'p value' = p.value) 

### CL3 ----

fit <- lmer(perc_weight ~ site + enro_sex + 
              txas_reperfusion*time + (1|enro_animal_id),
            data = dm_cl3, REML = FALSE)

tables$lmer$cl3$txas_reperfusion <-
  comparisons(fit, 
              variables = list(txas_reperfusion = "reference"), 
              by = "time",
              type = "response",
              conf_level = (1 - 0.05/8)) |>
  as.data.frame() |>
  mutate(contrast = str_remove_all(contrast, "mean|\\(|\\)"),
         estimate95 = 
           glue("{round(estimate, 2)} ({round(conf.low, 2)}, {round(conf.high, 2)})")) |>
  select(contrast, time, estimate95, p.value) |>
  group_by(contrast) |>
  mutate(p.value = p.adjust(p.value, method = "holm", n = 8),
         p.value = format_pvalue(p.value)) |>
  rename(Contrast = contrast,
         Time = time,
         'Difference (95%CI)' = estimate95,
         'p value' = p.value) 

### CL4 ----

fit <- lmer(perc_weight ~ site + enro_sex + 
              txas_reperfusion*time + (1|enro_animal_id),
            data = dm_cl4, REML = FALSE)

tables$lmer$cl4$txas_reperfusion <-
  comparisons(fit, 
              variables = list(txas_reperfusion = "reference"), 
              by = "time",
              type = "response",
              conf_level = (1 - 0.05/8)) |>
  as.data.frame() |>
  mutate(contrast = str_remove_all(contrast, "mean|\\(|\\)"),
         estimate95 = 
           glue("{round(estimate, 2)} ({round(conf.low, 2)}, {round(conf.high, 2)})")) |>
  select(contrast, time, estimate95, p.value) |>
  group_by(contrast) |>
  mutate(p.value = p.adjust(p.value, method = "holm", n = 8),
         p.value = format_pvalue(p.value)) |>
  rename(Contrast = contrast,
         Time = time,
         'Difference (95%CI)' = estimate95,
         'p value' = p.value) 


# Saving output ----

rm(list=setdiff(ls(), c("file", 
                        "tables",
                        "plots")))
save.image("results/span2_clotstudy_p3_weight_analysis_pp.Rdata")
