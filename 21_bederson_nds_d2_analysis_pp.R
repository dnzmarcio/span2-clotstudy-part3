rm(list = ls())

# Load --------------------------------------------------------------------

source("00_utils.R")
load("data/span2_clotstudy_p3_feasibility_data.Rdata")

# Output -----------------------------------------------------------------

tables <- list()
plots <- list()
statistics <- list()
pim_results <- list()

ylabel <- "Bederson Neurodeficit score at Day 2"

# Data Processing ----

## MRI data ----

dt_mri <- read_csv(file = mri) |>
  clean_names() |> 
  select(enro_animal_id, p_mri_d2_fract_les)

## Feasibility data ----

dt <- dt_feasibility$pp  |>
  left_join(dt_mri,  by = "enro_animal_id")

dt_cl3 <- dt |> 
  filter(clot_length == "3")

dt_cl4 <- dt |> 
  filter(clot_length == "4")

dt_control <- dt |> 
  filter(txas_reperfusion == "Control")

dt_tnkase <- dt |> 
  filter(txas_reperfusion == "TNKase")

# Tables ----

## Feasibility ----

tables$feasibility$overall <- dt |>
  select(postop_d2_nds_score_conduct) |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$feasibility$sex <- dt |>
  select(postop_d2_nds_score_conduct,
         enro_sex) |>
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$feasibility$site <- dt |>
  select(postop_d2_nds_score_conduct, 
         site) |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$feasibility$txas_reperfusion <- dt |>
  select(postop_d2_nds_score_conduct, 
         txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$feasibility$clot_length <- dt |>
  select(postop_d2_nds_score_conduct,
         clot_length) |>
  nt_describe(group = clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


## Treatment ----

tables$outcome$overall <- dt |>
  select(postop_d2_nds_score) |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$sex$f <- dt |>
  filter(enro_sex == "Female") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$sex$m <- dt |>
  filter(enro_sex == "Male") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$ag <- dt |>
  filter(site == "AG") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site$dk <- dt |>
  filter(site == "DK") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$iw <- dt |>
  filter(site == "IW") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$mg <- dt |>
  filter(site == "MG") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site$sd <- dt |>
  filter(site == "SD") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$yl <- dt |>
  filter(site == "YL") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$txas_reperfusion <- dt |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$clot_length$cl3 <- dt |>
  filter(clot_length == "3") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$clot_length$cl4 <- dt |>
  filter(clot_length == "4") |>
  select(postop_d2_nds_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Plots ----

## Overall ----

dt_plot <- dt |>
  filter(!is.na(postop_d2_nds_score)) |>
  mutate(postop_d2_nds_score = droplevels(postop_d2_nds_score)) |>
  droplevels()

plots$outcome$overall$txas_reperfusion <-
  plot_treat_stackedbar(data = dt_plot,  
                 y = postop_d2_nds_score, 
                 ylabel = ylabel)

plots$outcome$overall$sex <-
  plot_sex_stackedbar(data = dt_plot,  
               y = postop_d2_nds_score, 
               ylabel = ylabel,
               size_perc = 2.5)

plots$outcome$overall$site <-
  plot_site_stackedbar(data = dt_plot,  
                y = postop_d2_nds_score, 
                ylabel = ylabel,
                size_perc = 2.5)

plots$outcome$overall$clot_length <-
  plot_clotlength_stackedbar(data = dt_plot,  
                              y = postop_d2_nds_score, 
                              ylabel = ylabel,
                              size_perc = 2.5)

## CL3 ----

dt_plot <- dt_cl3 |>
  filter(!is.na(postop_d2_nds_score)) |>
  mutate(postop_d2_nds_score = droplevels(postop_d2_nds_score)) |>
  droplevels()

plots$outcome$cl3$txas_reperfusion <-
  plot_treat_stackedbar(data = dt_plot,  
                        y = postop_d2_nds_score, 
                        ylabel = ylabel)

plots$outcome$cl3$sex <-
  plot_sex_stackedbar(data = dt_plot,  
                      y = postop_d2_nds_score, 
                      ylabel = ylabel,
                      size_perc = 2.5)

plots$outcome$cl3$site <-
  plot_site_stackedbar(data = dt_plot,  
                       y = postop_d2_nds_score, 
                       ylabel = ylabel,
                       size_perc = 2.5)

## CL4 ----

dt_plot <- dt_cl4 |>
  filter(!is.na(postop_d2_nds_score)) |>
  mutate(postop_d2_nds_score = droplevels(postop_d2_nds_score)) |>
  droplevels()

plots$outcome$cl4$txas_reperfusion <-
  plot_treat_stackedbar(data = dt_plot,  
                        y = postop_d2_nds_score, 
                        ylabel = ylabel)

plots$outcome$cl4$sex <-
  plot_sex_stackedbar(data = dt_plot,  
                      y = postop_d2_nds_score, 
                      ylabel = ylabel,
                      size_perc = 2.5)

plots$outcome$cl4$site <-
  plot_site_stackedbar(data = dt_plot,  
                       y = postop_d2_nds_score, 
                       ylabel = ylabel,
                       size_perc = 2.5)

# Treatment Comparisons ----

## PIM - No Missing Data Imputation ----

dm <- dt |>
  mutate(postop_d2_nds_score = as.numeric(postop_d2_nds_score) - 1) |>
  select(postop_d2_nds_score, site, enro_model, enro_sex, 
         clot_length, txas_reperfusion) |>
  droplevels()

dm_cl3 <- dt_cl3 |>
  mutate(postop_d2_nds_score = as.numeric(postop_d2_nds_score) - 1) |>
  select(postop_d2_nds_score, site, enro_model, enro_sex, 
         txas_reperfusion) |> 
  na.exclude() |>
  droplevels()

dm_cl4 <- dt_cl4 |>
  mutate(postop_d2_nds_score = as.numeric(postop_d2_nds_score) - 1) |>
  select(postop_d2_nds_score, site, enro_model, enro_sex, 
         txas_reperfusion) |>
  droplevels()

### Interaction ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm))

pim_results$noimputation$interaction <- tmp <-
  nt_multiple_pim(fit)

tables$pim$noimputation$interaction <-  
  pim_results$noimputation$interaction$pvalue[nrow(tmp)]

plots$pim$noimputation$interaction <- 
  plot_clotlength_stackedbar_interaction(dt, 
                                         y = postop_d2_nds_score,
                                         ylabel = ylabel, 
                                         pvalue = tables$pim$noimputation$interaction)

### Additive ----

#### Overall ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + clot_length + txas_reperfusion, data = na.omit(dm))

pim_results$noimputation$overall <- tmp <-
  nt_multiple_pim(fit)

tables$pim$noimputation$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$noimputation$overall$clot_length <-
  extract_clot_length(tmp)

tables$pim$noimputation$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$noimputation$overall$txas_reperfusion <- 
  plot_treat_sig_bar(data = dt,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$noimputation$overall$clot_length <- 
  plot_clot_length_sig_bar(data = dt,
                           y = postop_d1_nds_score, 
                           ylabel = ylabel, 
                           p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_cl3))

pim_results$noimputation$cl3 <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$noimputation$cl3$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$noimputation$cl3$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$noimputation$cl3 <- 
  plot_treat_sig_bar(data = dt_cl3,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$noimputation$overall <- 
  plot_clot_length_sig_bar(data = dt,
                           y = postop_d1_nds_score, 
                           ylabel = ylabel, 
                           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_cl4))

pim_results$noimputation$cl4 <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$noimputation$cl4$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$noimputation$cl4$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$noimputation$cl4 <- 
  plot_treat_sig_bar(data = dt_cl4,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)


## PIM - Missing Data Imputation - Overall Worst Rank Score ----

dm_imputed <- dt |> 
  mutate(postop_d2_nds_score = as.numeric(postop_d2_nds_score) - 1) |>
  mutate(postop_d2_nds_score = 
           ifelse(postop_d2_nds_score_conduct == "Yes", postop_d2_nds_score,
                  ifelse(eos_day_diff_srg_death < 1, 
                         max(postop_d2_nds_score, na.rm = TRUE),
                         NA)))|> 
  droplevels() |> 
  select(postop_d2_nds_score, 
         site, enro_sex, enro_model,
         clot_length, txas_reperfusion)

dm_imputed_cl3 <- dm_imputed  |>
  filter(clot_length == "3")

dm_imputed_cl4 <- dm_imputed  |>
  filter(clot_length == "4")

### Interaction ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_overall$interaction <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$imputation_ws_overall$interaction <-  
  pim_results$imputation_ws_overall$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_ws_overall$interaction <- 
  plot_clotlength_stackedbar_interaction(dt, 
                                         y = postop_d2_nds_score,
                                         ylabel = ylabel, 
                                         pvalue = 
                                           tables$pim$imputation_ws_overall$interaction)


### Additive ----

#### Overall ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + clot_length + txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_overall$overall <- tmp <-
  nt_multiple_pim(fit)

tables$pim$imputation_ws_overall$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_overall$overall$clot_length <-
  extract_clot_length(tmp)

tables$pim$imputation_ws_overall$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_overall$overall$txas_reperfusion <- 
  plot_treat_sig_bar(data = dt,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_overall$overall$clot_length <- 
  plot_clot_length_sig_bar(data = dt,
                           y = postop_d1_nds_score, 
                           ylabel = ylabel, 
                           p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl3))

pim_results$imputation_ws_overall$cl3 <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$imputation_ws_overall$cl3$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_overall$cl3$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_overall$cl3 <- 
  plot_treat_sig_bar(data = dt_cl3,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl4))

pim_results$imputation_ws_overall$cl4 <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$imputation_ws_overall$cl4$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_overall$cl4$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_overall$cl4 <- 
  plot_treat_sig_bar(data = dt_cl4,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)


## PIM - Missing Data Imputation - Clot Size and Sex ----

dm_imputed <- dt |> 
  mutate(postop_d2_nds_score = as.numeric(postop_d2_nds_score) - 1) |>
  group_by(clot_length, enro_sex) |> 
  mutate(postop_d2_nds_score = 
           ifelse(postop_d2_nds_score_conduct == "Yes", postop_d2_nds_score,
                  ifelse(eos_day_diff_srg_death < 1, 
                         max(postop_d2_nds_score, na.rm = TRUE),
                         NA)))|> 
  droplevels() |> 
  select(postop_d2_nds_score, 
         site, enro_sex, enro_model,
         clot_length, txas_reperfusion)

dm_imputed_cl3 <- dm_imputed  |>
  filter(clot_length == "3")

dm_imputed_cl4 <- dm_imputed  |>
  filter(clot_length == "4")

### Interaction ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_cls$interaction <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$imputation_ws_cls$interaction <-  
  pim_results$imputation_ws_cls$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_ws_cls$interaction <- 
  plot_clotlength_stackedbar_interaction(dt, 
                                         y = postop_d2_nds_score,
                                         ylabel = ylabel, 
                                         pvalue = 
                                           tables$pim$imputation_ws_cls$interaction)

### Additive ----

#### Overall ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + clot_length + txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_cls$overall <- tmp <-
  nt_multiple_pim(fit)

tables$pim$imputation_ws_cls$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_cls$overall$clot_length <-
  extract_clot_length(tmp)

tables$pim$imputation_ws_cls$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_cls$overall$txas_reperfusion <- 
  plot_treat_sig_bar(data = dt,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_cls$overall$clot_length <- 
  plot_clot_length_sig_bar(data = dt,
                           y = postop_d1_nds_score, 
                           ylabel = ylabel, 
                           p_values = pvalue_treatment)



#### Clot length 3cm ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl3))

pim_results$imputation_ws_cls$cl3 <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$imputation_ws_cls$cl3$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_cls$cl3$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_cls$cl3 <- 
  plot_treat_sig_bar(data = dt_cl3,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl4))

pim_results$imputation_ws_cls$cl4 <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$imputation_ws_cls$cl4$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_cls$cl4$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_cls$cl4 <- 
  plot_treat_sig_bar(data = dt_cl4,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

## PIM - Missing Data Imputation - Clot Size and Sex and Study Arm ----

dm_imputed <- dt |> 
  mutate(postop_d2_nds_score = as.numeric(postop_d2_nds_score) - 1) |>
  group_by(clot_length, enro_sex, txas_reperfusion) |> 
  mutate(postop_d2_nds_score = 
           ifelse(postop_d2_nds_score_conduct == "Yes", postop_d2_nds_score,
                  ifelse(eos_day_diff_srg_death < 1, 
                         max(postop_d2_nds_score, na.rm = TRUE),
                         NA)))|> 
  droplevels() |> 
  select(postop_d2_nds_score, 
         site, enro_sex, enro_model,
         clot_length, txas_reperfusion)

dm_imputed_cl3 <- dm_imputed  |>
  filter(clot_length == "3")

dm_imputed_cl4 <- dm_imputed  |>
  filter(clot_length == "4")

### Interaction ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_clst$interaction <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$imputation_ws_clst$interaction <-  
  pim_results$imputation_ws_clst$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_ws_clst$interaction <- 
  plot_clotlength_stackedbar_interaction(dt, 
                                         y = postop_d2_nds_score,
                                         ylabel = ylabel, 
                                         pvalue = 
                                           tables$pim$imputation_ws_clst$interaction)

### Additive ----

#### Overall ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + clot_length + txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_clst$overall <- tmp <-
  nt_multiple_pim(fit)

tables$pim$imputation_ws_clst$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_clst$overall$clot_length <-
  extract_clot_length(tmp)

tables$pim$imputation_ws_clst$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_clst$overall$txas_reperfusion <- 
  plot_treat_sig_bar(data = dt,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_clst$overall$clot_length <- 
  plot_clot_length_sig_bar(data = dt,
                           y = postop_d1_nds_score, 
                           ylabel = ylabel, 
                           p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl3))

pim_results$imputation_ws_clst$cl3 <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$imputation_ws_clst$cl3$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_clst$cl3$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_clst$cl3 <- 
  plot_treat_sig_bar(data = dt_cl3,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(postop_d2_nds_score ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl4))

pim_results$imputation_ws_clst$cl4 <- tmp <- 
  nt_multiple_pim(fit)

tables$pim$imputation_ws_clst$cl4$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_clst$cl4$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_clst$cl4 <- 
  plot_treat_sig_bar(data = dt_cl4,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

## PIM - Missing Data Imputation - Multiple Imputation ----

dt_imputed <- dt |> 
  mutate(postop_d2_nds_score = as.numeric(postop_d2_nds_score) - 1) |>
  select(postop_d2_nds_score, 
         all_of(mi_late_variables),
         enro_model, enro_sex, site,
         clot_length, txas_reperfusion) |>
  droplevels() |>
  mice(m = m, maxit = maxit, meth='pmm',seed=500)

dt_imputed_cl3 <- dt |> 
  mutate(postop_d2_nds_score = as.numeric(postop_d2_nds_score) - 1) |>
  filter(clot_length == "3") |>
  select(postop_d2_nds_score, 
         all_of(mi_late_variables),
         enro_model, enro_sex, site,
         clot_length, txas_reperfusion) |>
  droplevels() |>
  mice(m = m, maxit = maxit, meth='pmm',seed=500)

dt_imputed_cl4 <- dt |> 
  mutate(postop_d2_nds_score = as.numeric(postop_d2_nds_score) - 1) |>
  filter(clot_length == "4") |>
  select(postop_d2_nds_score, 
         all_of(mi_late_variables),
         enro_model, enro_sex, site,
         clot_length, txas_reperfusion) |>
  droplevels() |>
  mice(m = m, maxit = maxit, meth='pmm',seed=500)


### Interaction ----

fit <- with(dt_imputed, pim(postop_d2_nds_score ~ site + 
                              enro_sex + clot_length*txas_reperfusion))

pim_results$imputation_mi$interaction <- tmp <- 
  nt_multiple_pim(fit, mi = TRUE)

tables$pim$imputation_mi$interaction <-  
  pim_results$imputation_mi$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_mi$interaction <- 
  plot_clotlength_stackedbar_interaction(dt, 
                                         y = postop_d2_nds_score,
                                         ylabel = ylabel, 
                                         pvalue = 
                                           tables$pim$imputation_mi$interaction)

### Additive ----

#### Overall ----

fit <- with(dt_imputed, pim(postop_d2_nds_score ~ site + 
                              enro_sex + clot_length + txas_reperfusion))

pim_results$imputation_mi$overall <- tmp <-
  nt_multiple_pim(fit, mi = TRUE)

tables$pim$imputation_mi$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_mi$overall$clot_length <-
  extract_clot_length(tmp)

tables$pim$imputation_mi$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_mi$overall$txas_reperfusion <- 
  plot_treat_sig_bar(data = dt,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_mi$overall$clot_length <- 
  plot_clot_length_sig_bar(data = dt,
                           y = postop_d1_nds_score, 
                           ylabel = ylabel, 
                           p_values = pvalue_treatment)

#### Clot length 3cm ----

fit <- with(dt_imputed_cl3, pim(postop_d2_nds_score ~ site + 
                                  enro_sex + txas_reperfusion))

pim_results$imputation_mi$cl3 <- tmp <- 
  nt_multiple_pim(fit, mi = TRUE)

tables$pim$imputation_mi$cl3$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_mi$cl3$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_mi$cl3 <- 
  plot_treat_sig_bar(data = dt_cl3,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- with(dt_imputed_cl4, pim(postop_d2_nds_score ~ site + 
                                  enro_sex + txas_reperfusion))

pim_results$imputation_mi$cl4 <- tmp <- 
  nt_multiple_pim(fit, mi = TRUE)

tables$pim$imputation_mi$cl4$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_mi$cl4$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_mi$cl4 <- 
  plot_treat_sig_bar(data = dt_cl4,
               y = postop_d2_nds_score, 
               ylabel = ylabel, 
               p_values = pvalue_treatment)


# Saving output ----

rm(list=setdiff(ls(), c("tables", 
                        "plots",
                        "pim_results")))
save.image("results/span2_clotstudy_p3_bederson_nds_d2_analysis_pp.Rdata")