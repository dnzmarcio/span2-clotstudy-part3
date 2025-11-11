rm(list = ls())

# Load --------------------------------------------------------------------

load("data/span2_clotstudy_p3_neuro_d30_data.Rdata")
source("00_utils.R")

# Output -----------------------------------------------------------------

tables <- list()
plots <- list()
statistics <- list()
pim_results <- list()
tmp_summary <- list()

ylabel <- "Simple SPAN Score at Day 30"

# Data Processing ----

dt <- left_join(dt_conduct_pp, dt_outcome_pp)


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
  select(behav_d30_conduct, behav_d30_conduct_rsn) |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$feasibility$sex <- dt |>
  select(behav_d30_conduct, behav_d30_conduct_rsn,
         enro_sex) |>
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$feasibility$site <- dt |>
  select(behav_d30_conduct, behav_d30_conduct_rsn,
         site) |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$feasibility$txas_reperfusion <- dt |>
  select(behav_d30_conduct, behav_d30_conduct_rsn,
         txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$feasibility$clot_length <- dt |>
  select(behav_d30_conduct, behav_d30_conduct_rsn,
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
  select(simple_span_score) |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$sex$f <- dt |>
  filter(enro_sex == "Female") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$sex$m <- dt |>
  filter(enro_sex == "Male") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site$ag <- dt |>
  filter(site == "AG") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site$dk <- dt |>
  filter(site == "DK") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$iw <- dt |>
  filter(site == "IW") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$mg <- dt |>
  filter(site == "MG") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$site$sd <- dt |>
  filter(site == "SD") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$outcome$site$yl <- dt |>
  filter(site == "YL") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$clot_length$cl3 <- dt |>
  filter(clot_length == "3") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$clot_length$cl4 <- dt |>
  filter(clot_length == "4") |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$outcome$txas_reperfusion <- dt |>
  select(simple_span_score, txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Plots ----

## Overall ----

plots$outcome$overall$txas_reperfusion <-
  plot_treat(data = dt,  
             y = simple_span_score, 
             ylabel = ylabel)

plots$outcome$overall$clot_length <-
  plot_clotlength(data = dt,  
                  y = simple_span_score, 
                  ylabel = ylabel)

plots$outcome$overall$sex <-
  plot_sex(data = dt,  
           y = simple_span_score, 
           ylabel = ylabel)

plots$outcome$overall$site <-
  plot_site(data = dt,  
            y = simple_span_score, 
            ylabel = ylabel)

## Clot Length 3 ----

plots$outcome$cl3$txas_reperfusion <-
  plot_treat(data = dt_cl3,  
             y = simple_span_score, 
             ylabel = ylabel)

plots$outcome$cl3$sex <-
  plot_sex(data = dt_cl3,  
           y = simple_span_score, 
           ylabel = ylabel)

plots$outcome$cl3$site <-
  plot_site(data = dt_cl3,  
            y = simple_span_score, 
            ylabel = ylabel)

## Clot Length 4 ----

plots$outcome$cl4$txas_reperfusion <-
  plot_treat(data = dt_cl4,  
             y = simple_span_score, 
             ylabel = ylabel)

plots$outcome$cl4$sex <-
  plot_sex(data = dt_cl4,  
           y = simple_span_score, 
           ylabel = ylabel)

plots$outcome$cl4$site <-
  plot_site(data = dt_cl4,  
            y = simple_span_score, 
            ylabel = ylabel)


# Treatment Comparisons ----


## PIM - No Missing Data Imputation ----

dm <- dt |>
  select(simple_span_score, site, enro_model, enro_sex, 
         clot_length, txas_reperfusion) |>
  droplevels()

dm_cl3 <- dt_cl3 |>
  select(simple_span_score, site, enro_model, enro_sex, 
         txas_reperfusion) |> 
  na.exclude() |>
  droplevels()

dm_cl4 <- dt_cl4 |>
  select(simple_span_score, site, enro_model, enro_sex, 
         txas_reperfusion) |>
  droplevels()

### Interaction ----

fit <- pim(simple_span_score ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm))

pim_results$noimputation$interaction <- tmp <-
 nt_multiple_pim(fit)

tables$pim$noimputation$interaction <-  
  pim_results$noimputation$interaction$pvalue[nrow(tmp)]

plots$pim$noimputation$interaction <- 
  plot_clotlength_interaction(dt, y = simple_span_score,
                              ylabel = ylabel, 
                              pvalue = tables$pim$noimputation$interaction)

### Additive ----

#### Overall ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt,
                 y = simple_span_score, 
                 ylabel = ylabel,
                 p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$noimputation$overall$clot_length <- 
  plot_clot_length_sig(data = dt,
                 y = simple_span_score, 
                 ylabel = ylabel,
                 p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl3,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl4,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)


## PIM - Missing Data Imputation - Overall Worst Rank Score ----

dm_imputed <- dt |> 
  mutate(simple_span_score = 
           ifelse(behav_d30_conduct == "Yes", simple_span_score,
                  ifelse(behav_d30_conduct_rsn == "Animal death", 
                         max(simple_span_score, na.rm = TRUE),
                         NA)))|> 
  droplevels() |> 
  select(simple_span_score, 
         site, enro_sex, enro_model,
         clot_length, txas_reperfusion)

dm_imputed_cl3 <- dm_imputed  |>
  filter(clot_length == "3")

dm_imputed_cl4 <- dm_imputed  |>
  filter(clot_length == "4")

### Interaction ----

fit <- pim(simple_span_score ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_overall$interaction <- tmp <- 
 nt_multiple_pim(fit)

tables$pim$imputation_ws_overall$interaction <-  
  pim_results$imputation_ws_overall$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_ws_overall$interaction <- 
  plot_clotlength_interaction(dt, y = simple_span_score,
                              ylabel = ylabel, 
                              pvalue = 
                                tables$pim$imputation_ws_overall$interaction)


### Additive ----

#### Overall ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt,
                 y = simple_span_score, 
                 ylabel = ylabel,
                 p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_overall$overall$clot_length <- 
  plot_clot_length_sig(data = dt,
                       y = simple_span_score, 
                       ylabel = ylabel,
                       p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl3,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl4,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)


## PIM - Missing Data Imputation - Clot Size and Sex ----

dm_imputed <- dt |> 
  group_by(clot_length, enro_sex) |> 
  mutate(simple_span_score = 
           ifelse(behav_d30_conduct == "Yes", simple_span_score,
                  ifelse(behav_d30_conduct_rsn == "Animal death", 
                         max(simple_span_score, na.rm = TRUE),
                         NA)))|> 
  droplevels() |> 
  select(simple_span_score, 
         site, enro_sex, enro_model,
         clot_length, txas_reperfusion)

dm_imputed_cl3 <- dm_imputed  |>
  filter(clot_length == "3")

dm_imputed_cl4 <- dm_imputed  |>
  filter(clot_length == "4")

### Interaction ----

fit <- pim(simple_span_score ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_cls$interaction <- tmp <- 
 nt_multiple_pim(fit)

tables$pim$imputation_ws_cls$interaction <-  
  pim_results$imputation_ws_cls$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_ws_cls$interaction <- 
  plot_clotlength_interaction(dt, y = simple_span_score,
                              ylabel = ylabel, 
                              pvalue = 
                                tables$pim$imputation_ws_cls$interaction)

### Additive ----

#### Overall ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt,
                 y = simple_span_score, 
                 ylabel = ylabel,
                 p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_cls$overall$clot_length <- 
  plot_clot_length_sig(data = dt,
                       y = simple_span_score, 
                       ylabel = ylabel,
                       p_values = pvalue_treatment)



#### Clot length 3cm ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl3,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl4,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)

## PIM - Missing Data Imputation - Clot Size and Sex and Study Arm ----

dm_imputed <- dt |> 
  group_by(clot_length, enro_sex, txas_reperfusion) |> 
  mutate(simple_span_score = 
           ifelse(behav_d30_conduct == "Yes", simple_span_score,
                  ifelse(behav_d30_conduct_rsn == "Animal death", 
                         max(simple_span_score, na.rm = TRUE),
                         NA)))|> 
  droplevels() |> 
  select(simple_span_score, 
         site, enro_sex, enro_model,
         clot_length, txas_reperfusion)

dm_imputed_cl3 <- dm_imputed  |>
  filter(clot_length == "3")

dm_imputed_cl4 <- dm_imputed  |>
  filter(clot_length == "4")

### Interaction ----

fit <- pim(simple_span_score ~ site + 
             enro_sex + clot_length*txas_reperfusion, data = na.omit(dm_imputed))

pim_results$imputation_ws_clst$interaction <- tmp <- 
 nt_multiple_pim(fit)

tables$pim$imputation_ws_clst$interaction <-  
  pim_results$imputation_ws_clst$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_ws_clst$interaction <- 
  plot_clotlength_interaction(dt, y = simple_span_score,
                              ylabel = ylabel, 
                              pvalue = 
                                tables$pim$imputation_ws_clst$interaction)

### Additive ----

#### Overall ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt,
                 y = simple_span_score, 
                 ylabel = ylabel,
                 p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_ws_clst$overall$clot_length <- 
  plot_clot_length_sig(data = dt,
                       y = simple_span_score, 
                       ylabel = ylabel,
                       p_values = pvalue_treatment)


#### Clot length 3cm ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl3,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl4,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)

## PIM - Missing Data Imputation - Multiple Imputation ----

dt_imputed <- dt |> 
  select(simple_span_score, 
         all_of(mi_late_variables),
         enro_model, enro_sex, site,
         clot_length, txas_reperfusion) |>
  droplevels() |>
  mice(m = m, maxit = maxit, meth='pmm',seed=500)

dt_imputed_cl3 <- dt |> 
  filter(clot_length == "3") |>
  select(simple_span_score, 
         all_of(mi_late_variables),
         enro_model, enro_sex, site,
         clot_length, txas_reperfusion) |>
  droplevels() |>
  mice(m = m, maxit = maxit, meth='pmm',seed=500)

dt_imputed_cl4 <- dt |> 
  filter(clot_length == "4") |>
  select(simple_span_score, 
         all_of(mi_late_variables),
         enro_model, enro_sex, site,
         clot_length, txas_reperfusion) |>
  droplevels() |>
  mice(m = m, maxit = maxit, meth='pmm',seed=500)


### Interaction ----

fit <- with(dt_imputed, pim(simple_span_score ~ site + 
             enro_sex + clot_length*txas_reperfusion))

pim_results$imputation_mi$interaction <- tmp <- 
  nt_multiple_pim(fit, mi = TRUE)

tables$pim$imputation_mi$interaction <-  
  pim_results$imputation_mi$interaction$pvalue[nrow(tmp)]

plots$pim$imputation_mi$interaction <- 
  plot_clotlength_interaction(dt, y = simple_span_score,
                              ylabel = ylabel, 
                              pvalue = 
                                tables$pim$imputation_mi$interaction)

### Additive ----

#### Overall ----

fit <- with(dt_imputed, pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt,
                 y = simple_span_score, 
                 ylabel = ylabel,
                 p_values = pvalue_treatment)

tmp.pval <- tmp |> 
  filter(str_detect(variable, "clot_length")) |> 
  mutate(variable = str_replace(variable, "clot_length", "")) |> 
  select(variable, pvalue)
pvalue_treatment = format_pvalue(tmp.pval$pvalue)

plots$pim$imputation_mi$overall$clot_length <- 
  plot_clot_length_sig(data = dt,
                       y = simple_span_score, 
                       ylabel = ylabel,
                       p_values = pvalue_treatment)

#### Clot length 3cm ----

fit <- with(dt_imputed_cl3, pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl3,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)

#### Clot length 4cm ----

fit <- with(dt_imputed_cl4, pim(simple_span_score ~ site + 
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
  plot_treat_sig(data = dt_cl4,
           y = simple_span_score, 
           ylabel = ylabel,
           p_values = pvalue_treatment)

# Summary ----

## Treatment Overall ----

tmp_summary$tx$overall <- list(
  noimputation        = tables$pim$noimputation$overall$txas_reperfusion,
  imputation_ws_overall   = tables$pim$imputation_ws_overall$overall$txas_reperfusion,
  imputation_ws_cls       = tables$pim$imputation_ws_cls$overall$txas_reperfusion,
  imputation_ws_clst      = tables$pim$imputation_ws_clst$overall$txas_reperfusion,
  imputation_mi           = tables$pim$imputation_mi$overall$txas_reperfusion
)

tmp <- tmp_summary$tx$overall

plots$summary$tx$overall <- plot_summary(tmp, comparison = "treatment")

## Clot Length Overall ----

tmp_summary$clot_length$overall <- list(
  noimputation        = tables$pim$noimputation$overall$clot_length,
  imputation_ws_overall   = tables$pim$imputation_ws_overall$overall$clot_length,
  imputation_ws_cls       = tables$pim$imputation_ws_cls$overall$clot_length,
  imputation_ws_clst      = tables$pim$imputation_ws_clst$overall$clot_length,
  imputation_mi           = tables$pim$imputation_mi$overall$clot_length
)

tmp <- tmp_summary$clot_length$overall

plots$summary$clot_length$overall <- plot_summary(tmp, comparison = "clot_length")

## CL3 ----
tmp_summary$cl3 <- list(
  noimputation        = tables$pim$noimputation$cl3$txas_reperfusion,
  imputation_ws_overall   = tables$pim$imputation_ws_overall$cl3$txas_reperfusion,
  imputation_ws_cls       = tables$pim$imputation_ws_cls$cl3$txas_reperfusion,
  imputation_ws_clst      = tables$pim$imputation_ws_clst$cl3$txas_reperfusion,
  imputation_mi           = tables$pim$imputation_mi$cl3$txas_reperfusion
)

tmp <- tmp_summary$cl3

plots$summary$cl3 <- plot_summary(tmp, comparison = "treatment")

## CL4 ----
tmp_summary$cl4 <- list(
  noimputation        = tables$pim$noimputation$cl4$txas_reperfusion,
  imputation_ws_overall   = tables$pim$imputation_ws_overall$cl4$txas_reperfusion,
  imputation_ws_cls       = tables$pim$imputation_ws_cls$cl4$txas_reperfusion,
  imputation_ws_clst      = tables$pim$imputation_ws_clst$cl4$txas_reperfusion,
  imputation_mi           = tables$pim$imputation_mi$cl4$txas_reperfusion
)

tmp <- tmp_summary$cl4 

plots$summary$cl4 <- plot_summary(tmp, comparison = "treatment")

# Saving output ----

rm(list=setdiff(ls(), c("tables", 
                        "plots",
                        "pim_results")))

save.image("results/span2_clotstudy_p3_simple_span_score_d30_analysis_pp.Rdata")
