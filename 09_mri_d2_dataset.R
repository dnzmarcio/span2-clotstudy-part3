# Author: Karni Bedirian, Marcio Augusto Diniz

# Setup

rm(list = ls())

# Libraries
source("00_utils.R")

# D2 MRI ----
dataset <- read_csv(file = mri) |>
  clean_names() 

dt_outcome <- dataset |> 
  select(enro_animal_id, p_mri_d2_mid_shift_indx, 
         p_mri_d2_fract_csf, p_mri_d2_fract_csf_left, p_mri_d2_fract_csf_right,
         p_mri_d2_fract_tis, p_mri_d2_fract_tis_left, p_mri_d2_fract_tis_right, 
         p_mri_d2_fract_les, p_mri_d2_fract_les_left, p_mri_d2_fract_les_right) |>
  rename(p_mri_d2_fract_tissue = p_mri_d2_fract_tis,
         p_mri_d2_fract_tissue_left = p_mri_d2_fract_tis_left,
         p_mri_d2_fract_tissue_right = p_mri_d2_fract_tis_right,
         p_mri_d2_fract_lesion = p_mri_d2_fract_les,
         p_mri_d2_fract_lesion_left = p_mri_d2_fract_les_left,
         p_mri_d2_fract_lesion_right = p_mri_d2_fract_les_right)

dt_conduct <- dataset |> 
  select(enro_animal_id,
         mri_d2_conduct,mri_d2_conduct_rsn, p_mri_d2_ind,
         p_mri_d2_rsn_1:p_mri_d2_rsn_9) |>
  mutate(mri_d2_conduct = factor(mri_d2_conduct,
                                 levels = 0:1,
                                 labels = c("No", "Yes")),
         mri_d2_conduct_rsn = factor(mri_d2_conduct_rsn,
                                     levels = 1:3,
                                     labels = c("Animal death",
                                                "Equipment Failure",
                                                "Other")),
         animal_death_d2 = ifelse(mri_d2_conduct == "Yes", "No",
                                  ifelse(mri_d2_conduct_rsn ==
                                           "Animal death",
                                         "Yes", "No")),
         # mri_d2_comments = factor(mri_d2_comments,
         #                          levels = 0:4,
         #                          labels = c("None",
         #                                     "Animal could not complete test",
         #                                     "Scan equipment malfunction",
         #                                     "Disruption during scan",
         #                                     "Other")),
         p_mri_d2_ind = factor(p_mri_d2_ind, 
                                      levels = 0:1,
                                      labels = c("No", "Yes")),
         across(p_mri_d2_rsn_1:p_mri_d2_rsn_9, ~ 
                  ifelse(p_mri_d2_ind == "Yes", NA, .x)),
         p_mri_d2_rsn_1 = factor(p_mri_d2_rsn_1,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         p_mri_d2_rsn_2 = factor(p_mri_d2_rsn_2,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         p_mri_d2_rsn_3 = factor(p_mri_d2_rsn_3,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         p_mri_d2_rsn_4 = factor(p_mri_d2_rsn_4,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         p_mri_d2_rsn_5 = factor(p_mri_d2_rsn_5,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         p_mri_d2_rsn_6 = factor(p_mri_d2_rsn_6,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         p_mri_d2_rsn_7 = factor(p_mri_d2_rsn_7,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         p_mri_d2_rsn_8 = factor(p_mri_d2_rsn_8,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         p_mri_d2_rsn_9 = factor(p_mri_d2_rsn_9,
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         .after = enro_animal_id) |>
  select(enro_animal_id,p_mri_d2_ind,p_mri_d2_rsn_1:p_mri_d2_rsn_9,
         mri_d2_conduct, mri_d2_conduct_rsn, animal_death_d2)

## Feasibility ----

file2 <- file
load("data/span2_clotstudy_p3_feasibility_data.Rdata")
file <- file2
rm(file2)

### Modified ITT ----
dt_outcome_mitt <- left_join(dt_feasibility$mitt, dt_outcome,  by = "enro_animal_id") |>
  select(enro_animal_id, contains("mri"))

dt_conduct_mitt <- left_join(dt_feasibility$mitt, dt_conduct,  by = "enro_animal_id") |>
  select(enro_animal_id, site, enro_model, enro_sex,
         txas_reperfusion,clot_length,
         srg_animal_age, srg_weight, 
         mri_d2_conduct, mri_d2_conduct_rsn,
         animal_death_d2,p_mri_d2_ind,
         contains("mri"))


### Full Treatment ----
dt_outcome_pp <- left_join(dt_feasibility$pp, dt_outcome, by = "enro_animal_id") |>
  select(enro_animal_id, contains("mri")) 

dt_conduct_pp <- left_join(dt_feasibility$pp, dt_conduct,  by = "enro_animal_id") |>
  select(enro_animal_id, site, enro_model, enro_sex,
         txas_reperfusion, clot_length,
         srg_animal_age, srg_weight, 
         mri_d2_conduct, mri_d2_conduct_rsn,
         animal_death_d2,p_mri_d2_ind,
         contains("mri"))

# Saving output ----

rm(list=setdiff(ls(), c("data_labels",
                        "dt_outcome_mitt", "dt_conduct_mitt",
                        "dt_outcome_pp", "dt_conduct_pp")))
save.image("data/span2_clotstudy_p3_mri_d2_data.Rdata")
