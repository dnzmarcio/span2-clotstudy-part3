rm(list = ls())

# Load --------------------------------------------------------------------

source("00_utils.R")
load("data/span2_clotstudy_p3_feasibility_data.Rdata")

# Output -----------------------------------------------------------------

tables <- list()
plots <- list()

# mITT ----

aux <- dt_feasibility$comp_mitt 

## Tables ----

tables$comp_mitt$overall <- aux |>
  select(compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$comp_mitt$sex <- aux |> 
  select(enro_sex, 
         compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$comp_mitt$site <- aux |>
  select(site, 
         compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$comp_mitt$txas_reperfusion <- aux |>
  select(txas_reperfusion, 
         compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$comp_mitt$clot_length <- aux |>
  select(clot_length, 
         compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(group = clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Per Protocol ----

aux <- dt_feasibility$comp_pp

## Tables ----

tables$comp_pp$overall <- aux |>
  select(compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$comp_pp$sex <- aux |> 
  select(enro_sex, 
         compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$comp_pp$site <- aux |>
  select(site, 
         compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$comp_pp$txas_reperfusion <- aux |>
  select(txas_reperfusion, 
         compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$comp_pp$clot_length <- aux |>
  select(clot_length, 
         compliance_vial_id, compliance_tnk_adm,
         compliance_tnk_dose, compliance_clot_length,
         compliance_donor_id, compliance_donor_sex, 
         compliance_clot_draw_srg_date) |>
  nt_describe(group = clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Saving output ----

rm(list=setdiff(ls(), c("tables",
                        "plots")))
save.image("results/span2_clotstudy_p3_compliance_analysis.Rdata")
