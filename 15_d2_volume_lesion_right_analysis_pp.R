rm(list = ls())

# Load --------------------------------------------------------------------

source("00_utils.R")
load("data/span2_clotstudy_p3_mri_d2_data.Rdata")

# Output -----------------------------------------------------------------

tables <- list()
plots <- list()
pim_results <- list()

ylabel <- "Day 3 Volume Fraction Lesion of the Right Hemisphere"

# Data Processing ----

dt <- left_join(dt_conduct_pp, dt_outcome_pp, by = "enro_animal_id")

dt_cl3 <- dt |> 
  filter(clot_length == "3")

dt_cl4 <- dt |> 
  filter(clot_length == "4")

dt_control <- dt |> 
  filter(txas_reperfusion == "Control")

dt_tnkase <- dt |> 
  filter(txas_reperfusion == "TNKase")

ylower = min(dt$p_mri_d2_fract_lesion_right, na.rm = TRUE)
yupper = max(dt$p_mri_d2_fract_lesion_right, na.rm = TRUE)

# Tables ----

## Feasibility ----

tables$feasibility$overall <- dt |>
  select(mri_d2_conduct, mri_d2_conduct_rsn, 
         p_mri_d2_ind,p_mri_d2_rsn_1:p_mri_d2_rsn_9
  ) |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$feasibility$sex <- dt |>
  select(mri_d2_conduct, mri_d2_conduct_rsn, 
         p_mri_d2_ind,p_mri_d2_rsn_1:p_mri_d2_rsn_9,
         enro_sex) |>
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$feasibility$site <- dt |>
  select(mri_d2_conduct, mri_d2_conduct_rsn, 
         p_mri_d2_ind,p_mri_d2_rsn_1:p_mri_d2_rsn_9,
         site) |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$feasibility$txas_reperfusion <- dt |>
  select(mri_d2_conduct, mri_d2_conduct_rsn, 
         p_mri_d2_ind,p_mri_d2_rsn_1:p_mri_d2_rsn_9,
         txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$feasibility$clot_length <- dt |>
  select(mri_d2_conduct, mri_d2_conduct_rsn, 
         p_mri_d2_ind,p_mri_d2_rsn_1:p_mri_d2_rsn_9,
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
  select("p_mri_d2_fract_lesion_right") |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$sex$f <- dt |>
  filter(enro_sex == "Female") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$sex$m <- dt |>
  filter(enro_sex == "Male") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site$overall <- dt |>
  select("p_mri_d2_fract_lesion_right", site) |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site$ag <- dt |>
  filter(site == "AG") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site$dk <- dt |>
  filter(site == "DK") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$iw <- dt |>
  filter(site == "IW") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$mg <- dt |>
  filter(site == "MG") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site$sd <- dt |>
  filter(site == "SD") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$yl <- dt |>
  filter(site == "YL") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$clot_length$cl3 <- dt |>
  filter(clot_length == "3") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$clot_length$cl4 <- dt |>
  filter(clot_length == "4") |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$txas_reperfusion <- dt |>
  select("p_mri_d2_fract_lesion_right", txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Plots ----


## Overall ----

dt_plot = dt

plots$outcome$overall$txas_reperfusion$d2 <-
  plot_treat(data = dt_plot, 
             y = p_mri_d2_fract_lesion_right,
             ylabel = ylabel,
             lower = ylower,
             upper = yupper)

plots$outcome$overall$clot_length$d2 <-
  plot_clotlength(data = dt_plot, 
                  y = p_mri_d2_fract_lesion_right,
                  ylabel = ylabel,
                  lower = ylower,
                  upper = yupper)

plots$outcome$overall$sex$d2 <- 
  plot_sex(data = dt_plot, 
           y = p_mri_d2_fract_lesion_right,
           ylabel = ylabel,
           lower = ylower,
           upper = yupper)

plots$outcome$overall$site$d2 <- 
  plot_site(data = dt_plot, 
            y = p_mri_d2_fract_lesion_right,
            ylabel = ylabel,
            lower = ylower,
            upper = yupper)


## Clot Length 3 ----

plots$outcome$cl3$txas_reperfusion <-
  plot_treat(data = dt_cl3,  
             y = p_mri_d2_fract_lesion_right, 
             ylabel = ylabel,
             lower = ylower,
             upper = yupper)

plots$outcome$cl3$sex <-
  plot_sex(data = dt_cl3,  
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel,
           lower = ylower,
           upper = yupper)

plots$outcome$cl3$site <-
  plot_site(data = dt_cl3,  
            y = p_mri_d2_fract_lesion_right, 
            ylabel = ylabel,
            lower = ylower,
            upper = yupper)

## Clot Length 4 ----

plots$outcome$cl4$txas_reperfusion <-
  plot_treat(data = dt_cl4,  
             y = p_mri_d2_fract_lesion_right, 
             ylabel = ylabel,
             lower = ylower,
             upper = yupper)

plots$outcome$cl4$sex <-
  plot_sex(data = dt_cl4,  
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel,
           lower = ylower,
           upper = yupper)

plots$outcome$cl4$site <-
  plot_site(data = dt_cl4,  
            y = p_mri_d2_fract_lesion_right, 
            ylabel = ylabel,
            lower = ylower,
            upper = yupper)

# Treatment Comparisons ----


## PIM - No Missing Data Imputation ----

dm <- dt |>
  select(p_mri_d2_fract_lesion_right, site, enro_model, enro_sex, 
         clot_length, txas_reperfusion) |>
  droplevels()

dm_cl3 <- dt_cl3 |>
  select(p_mri_d2_fract_lesion_right, site, enro_model, enro_sex, 
         txas_reperfusion) |> 
  na.exclude() |>
  droplevels()

dm_cl4 <- dt_cl4 |>
  select(p_mri_d2_fract_lesion_right, site, enro_model, enro_sex, 
         txas_reperfusion) |>
  droplevels()

### Interaction ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm))

pim_results$noimputation$interaction <- tmp <-
  nt_multiple_pim(fit, oneminus = TRUE)

tables$pim$noimputation$interaction <-  
  pim_results$noimputation$interaction$pvalue[nrow(tmp)]

plots$pim$noimputation$interaction <- 
  plot_clotlength_interaction(dt, y = p_mri_d2_fract_lesion_right,
                              ylabel = ylabel, 
                              pvalue = tables$pim$noimputation$interaction)

### Additive ----

#### Overall ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + clot_length + txas_reperfusion, data = na.omit(dm))

pim_results$noimputation$overall <- tmp <-
  nt_multiple_pim(fit, oneminus = TRUE)

tables$pim$noimputation$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$noimputation$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$noimputation$overall <- 
  plot_sig(data = dt,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_cl3))

pim_results$noimputation$cl3 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

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
  plot_sig(data = dt_cl3,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_cl4))

pim_results$noimputation$cl4 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

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
  plot_sig(data = dt_cl4,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)


## PIM - Missing Data Imputation - Overall Worst Rank Score ----

dm_imputed <- dt |> 
  mutate(p_mri_d2_fract_lesion_right = 
           ifelse(mri_d2_conduct == "Yes", p_mri_d2_fract_lesion_right,
                  ifelse(mri_d2_conduct_rsn == "Animal death", 
                         max(p_mri_d2_fract_lesion_right, na.rm = TRUE),
                         NA)))|> 
  droplevels() |> 
  select(p_mri_d2_fract_lesion_right, 
         site, enro_sex, enro_model,
         clot_length, txas_reperfusion)

dm_imputed_cl3 <- dm_imputed  |>
  filter(clot_length == "3")

dm_imputed_cl4 <- dm_imputed  |>
  filter(clot_length == "4")

### Interaction ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_overall$interaction <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

tables$pim$imputation_ws_overall$interaction <-  
  pim_results$imputation_ws_overall$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_ws_overall$interaction <- 
  plot_clotlength_interaction(dt, y = p_mri_d2_fract_lesion_right,
                              ylabel = ylabel, 
                              pvalue = 
                                tables$pim$imputation_ws_overall$interaction)


### Additive ----

#### Overall ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + clot_length + txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_overall$overall <- tmp <-
  nt_multiple_pim(fit, oneminus = TRUE)

tables$pim$imputation_ws_overall$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_overall$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_overall$overall <- 
  plot_sig(data = dt,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl3))

pim_results$imputation_ws_overall$cl3 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

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
  plot_sig(data = dt_cl3,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl4))

pim_results$imputation_ws_overall$cl4 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

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
  plot_sig(data = dt_cl4,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)


## PIM - Missing Data Imputation - Clot Size and Sex ----

dm_imputed <- dt |> 
  group_by(clot_length, enro_sex) |> 
  mutate(p_mri_d2_fract_lesion_right = 
           ifelse(mri_d2_conduct == "Yes", p_mri_d2_fract_lesion_right,
                  ifelse(mri_d2_conduct_rsn == "Animal death", 
                         max(p_mri_d2_fract_lesion_right, na.rm = TRUE),
                         NA)))|> 
  droplevels() |> 
  select(p_mri_d2_fract_lesion_right, 
         site, enro_sex, enro_model,
         clot_length, txas_reperfusion)

dm_imputed_cl3 <- dm_imputed  |>
  filter(clot_length == "3")

dm_imputed_cl4 <- dm_imputed  |>
  filter(clot_length == "4")

### Interaction ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_cls$interaction <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

tables$pim$imputation_ws_cls$interaction <-  
  pim_results$imputation_ws_cls$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_ws_cls$interaction <- 
  plot_clotlength_interaction(dt, y = p_mri_d2_fract_lesion_right,
                              ylabel = ylabel, 
                              pvalue = 
                                tables$pim$imputation_ws_cls$interaction)

### Additive ----

#### Overall ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + clot_length + txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_cls$overall <- tmp <-
  nt_multiple_pim(fit, oneminus = TRUE)

tables$pim$imputation_ws_cls$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_cls$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_cls$overall <- 
  plot_sig(data = dt,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl3))

pim_results$imputation_ws_cls$cl3 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

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
  plot_sig(data = dt_cl3,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl4))

pim_results$imputation_ws_cls$cl4 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

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
  plot_sig(data = dt_cl4,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)

## PIM - Missing Data Imputation - Clot Size and Sex and Study Arm ----

dm_imputed <- dt |> 
  group_by(clot_length, enro_sex, txas_reperfusion) |> 
  mutate(p_mri_d2_fract_lesion_right = 
           ifelse(mri_d2_conduct == "Yes", p_mri_d2_fract_lesion_right,
                  ifelse(mri_d2_conduct_rsn == "Animal death", 
                         max(p_mri_d2_fract_lesion_right, na.rm = TRUE),
                         NA)))|> 
  droplevels() |> 
  select(p_mri_d2_fract_lesion_right, 
         site, enro_sex, enro_model,
         clot_length, txas_reperfusion)

dm_imputed_cl3 <- dm_imputed  |>
  filter(clot_length == "3")

dm_imputed_cl4 <- dm_imputed  |>
  filter(clot_length == "4")

### Interaction ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_clst$interaction <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

tables$pim$imputation_ws_clst$interaction <-  
  pim_results$imputation_ws_clst$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_ws_clst$interaction <- 
  plot_clotlength_interaction(dt, y = p_mri_d2_fract_lesion_right,
                              ylabel = ylabel, 
                              pvalue = 
                                tables$pim$imputation_ws_clst$interaction)

### Additive ----

#### Overall ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + clot_length + txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_clst$overall <- tmp <-
  nt_multiple_pim(fit, oneminus = TRUE)

tables$pim$imputation_ws_clst$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_ws_clst$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_clst$overall <- 
  plot_sig(data = dt,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl3))

pim_results$imputation_ws_clst$cl3 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

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
  plot_sig(data = dt_cl3,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(p_mri_d2_fract_lesion_right ~ site + 
             enro_sex + txas_reperfusion, data = na.omit(dm_imputed_cl4))

pim_results$imputation_ws_clst$cl4 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE)

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
  plot_sig(data = dt_cl4,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)

## PIM - Missing Data Imputation - Multiple Imputation ----

dt_imputed <- dt |> 
  select(p_mri_d2_fract_lesion_right, 
         all_of(mi_early_variables),
         enro_model, enro_sex, site,
         clot_length, txas_reperfusion) |>
  droplevels() |>
  mice(m = m, maxit = maxit, meth='pmm',seed=500)

dt_imputed_cl3 <- dt |> 
  filter(clot_length == "3") |>
  select(p_mri_d2_fract_lesion_right, 
         all_of(mi_early_variables),
         enro_model, enro_sex, site,
         clot_length, txas_reperfusion) |>
  droplevels() |>
  mice(m = m, maxit = maxit, meth='pmm',seed=500)

dt_imputed_cl4 <- dt |> 
  filter(clot_length == "4") |>
  select(p_mri_d2_fract_lesion_right, 
         all_of(mi_early_variables),
         enro_model, enro_sex, site,
         clot_length, txas_reperfusion) |>
  droplevels() |>
  mice(m = m, maxit = maxit, meth='pmm',seed=500)


### Interaction ----

fit <- with(dt_imputed, pim(p_mri_d2_fract_lesion_right ~ site + 
                              enro_sex + clot_length*txas_reperfusion))

pim_results$imputation_mi$interaction <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE, mi = TRUE)

tables$pim$imputation_mi$interaction <-  
  pim_results$imputation_mi$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_mi$interaction <- 
  plot_clotlength_interaction(dt, y = p_mri_d2_fract_lesion_right,
                              ylabel = ylabel, 
                              pvalue = 
                                tables$pim$imputation_mi$interaction)

### Additive ----

#### Overall ----

fit <- with(dt_imputed, pim(p_mri_d2_fract_lesion_right ~ site + 
                              enro_sex + clot_length + txas_reperfusion))

pim_results$imputation_mi$overall <- tmp <-
  nt_multiple_pim(fit, oneminus = TRUE, mi = TRUE)

tables$pim$imputation_mi$overall$txas_reperfusion <-
  extract_treatment(tmp, oneminus = TRUE)

tables$pim$imputation_mi$overall$full_output <- format_full_output(tmp)

##### Significance Level Plots ----

tmp.pval <- tmp |> 
  filter(str_detect(variable, "txas_reperfusion")) |> 
  mutate(variable = str_replace(variable, "txas_reperfusion", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_mi$overall <- 
  plot_sig(data = dt,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)

#### Clot length 3cm ----

fit <- with(dt_imputed_cl3, pim(p_mri_d2_fract_lesion_right ~ site + 
                                  enro_sex + txas_reperfusion))

pim_results$imputation_mi$cl3 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE, mi = TRUE)

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
  plot_sig(data = dt_cl3,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- with(dt_imputed_cl4, pim(p_mri_d2_fract_lesion_right ~ site + 
                                  enro_sex + txas_reperfusion))

pim_results$imputation_mi$cl4 <- tmp <- 
  nt_multiple_pim(fit, oneminus = TRUE, mi = TRUE)

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
  plot_sig(data = dt_cl4,
           y = p_mri_d2_fract_lesion_right, 
           ylabel = ylabel, 
           p_values = pvalue_treatment)


# Saving output ----

rm(list=setdiff(ls(), c("tables",
                        "plots",
                        "pim_results")))
save.image("results/span2_clotstudy_p3_mri_d2_volume_lesion_right_analysis_pp.Rdata")
