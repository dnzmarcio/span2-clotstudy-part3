rm(list = ls())


# Load --------------------------------------------------------------------

source("00_utils.R")
load("data/span2_clotstudy_p3_feasibility_data.Rdata")

# Output -----------------------------------------------------------------

tables <- list()

# Enrolled ----

aux <- dt_feasibility$enrolled |>
  select(-enro_animal_id) 

tables$enrolled$overall <- aux |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$enrolled$site <- aux |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$enrolled$sex <- aux |> 
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2)) 


# Ineligible for Randomization ----

aux <- dt_feasibility$ineligible_for_randomization |>
  select(-enro_animal_id) 

tables$ineligible_for_randomization$overall <- aux |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$ineligible_for_randomization$site <- aux |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$ineligible_for_randomization$sex <- aux |> 
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2)) 


# ITT ----

aux <- dt_feasibility$itt |>
  select(-enro_animal_id,  -rand_conduct) 


tables$itt$overall <- aux |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))
tables$itt$sex <- aux |> 
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))
tables$itt$site <- aux |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$itt$txas_reperfusion <- aux |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$itt$clot_length <- aux |>
  nt_describe(group = rand_clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

# Exclusion from Treatment ----

# aux <- dt_feasibility$exclusion_from_treatment |>
#   select(-enro_animal_id,  -rand_conduct)
# 
# 
# tables$exclusion_from_treatment$overall <- aux |>
#   nt_describe(labels = data_labels,
#               measures_ql = list(helper_perc_count2),
#               measures_qt = list(helper_mean_sd,
#                                  helper_median_iqr,
#                                  helper_median_range,
#                                  helper_missing2))
# 
# tables$exclusion_from_treatment$sex <- aux |>
#   nt_describe(group = enro_sex,
#               labels = data_labels,
#               measures_ql = list(helper_perc_count2),
#               measures_qt = list(helper_mean_sd,
#                                  helper_median_iqr,
#                                  helper_median_range,
#                                  helper_missing2))
# 
# tables$exclusion_from_treatment$site <- aux |>
#   nt_describe(group = site,
#               labels = data_labels,
#               measures_ql = list(helper_perc_count2),
#               measures_qt = list(helper_mean_sd,
#                                  helper_median_iqr,
#                                  helper_median_range,
#                                  helper_missing2))
# 
# tables$exclusion_from_treatment$txas_reperfusion <- aux |>
#   nt_describe(group = txas_reperfusion,
#               labels = data_labels,
#               measures_ql = list(helper_perc_count2),
#               measures_qt = list(helper_mean_sd,
#                                  helper_median_iqr,
#                                  helper_median_range,
#                                  helper_missing2))
# 
# tables$exclusion_from_treatment$clot_length <- aux |>
#   nt_describe(group = rand_clot_length,
#               labels = data_labels,
#               measures_ql = list(helper_perc_count2),
#               measures_qt = list(helper_mean_sd, 
#                                  helper_median_iqr, 
#                                  helper_median_range,
#                                  helper_missing2))


# Procedural Dropouts ----


aux <- dt_feasibility$procedural_dropout |>
  select(-enro_animal_id, -rand_conduct , -srg_conduct, -successful_surgery) 

tables$procedural_dropout$overall <- aux |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$procedural_dropout$sex <- aux |> 
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$procedural_dropout$site <- aux |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))



tables$procedural_dropout$txas_reperfusion <- aux |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$procedural_dropout$clot_length <- aux |>
  nt_describe(group = srg_clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

# mITT ----


aux <- dt_feasibility$mitt |>
  select(-enro_animal_id, -rand_conduct , -srg_conduct, -successful_surgery,
         -c(postop_d1_nds_score_conduct:animal_death_before_conduct_d30)) 


tables$mitt$overall <- aux |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))
tables$mitt$sex <- aux |> 
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$mitt$site <- aux |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$mitt$txas_reperfusion <- aux |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$mitt$clot_length <- aux |>
  nt_describe(group = clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Partial Treatment ----


aux <- dt_feasibility$partial_treatment |>
  select(-enro_animal_id, -rand_conduct , -srg_conduct, -successful_surgery) 


tables$partial_treatment$overall <- aux |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$partial_treatment$sex <- aux |> 
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$partial_treatment$site <- aux |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$partial_treatment$txas_reperfusion <- aux |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$partial_treatment$clot_length <- aux |>
  nt_describe(group = srg_clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

# Per Protocol ----


aux <- dt_feasibility$pp |>
  select(-enro_animal_id, -rand_conduct , -srg_conduct, -successful_surgery,
         -c(postop_d1_nds_score_conduct:animal_death_before_conduct_d30))


tables$pp$overall <- aux |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$pp$sex <- aux |> 
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$pp$site <- aux |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$pp$txas_reperfusion <- aux |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$pp$clot_length <- aux |>
  nt_describe(group = clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Loss Follow up ----


aux <- dt_feasibility$loss_followup |>
  select(-enro_animal_id, -rand_conduct , -srg_conduct, -successful_surgery,
         -c(postop_d1_nds_score_conduct:eos_nds_score_conduct))


tables$loss_followup$overall <- aux |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$loss_followup$sex <- aux |> 
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$loss_followup$site <- aux |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$loss_followup$txas_reperfusion <- aux |>
  nt_describe(group = txas_reperfusion_actual,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$loss_followup$clot_length <- aux |>
  nt_describe(group = srg_clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Full Analysis Population ----

aux <- dt_feasibility$full_data |>
  select(-enro_animal_id, -rand_conduct , -srg_conduct, -successful_surgery,
         -c(postop_d1_nds_score_conduct:animal_death_before_conduct_d30))


tables$full_data$overall <- aux |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$full_data$sex <- aux |> 
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$full_data$site <- aux |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


tables$full_data$txas_reperfusion <- aux |>
  nt_describe(group = txas_reperfusion_actual,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))

tables$full_data$clot_length <- aux |>
  nt_describe(group = srg_clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd, 
                                 helper_median_iqr, 
                                 helper_median_range,
                                 helper_missing2))


# Saving output ----

rm(list=setdiff(ls(), "tables"))
save.image("results/span2_clotstudy_p3_feasibility_descriptive_analysis.Rdata")
