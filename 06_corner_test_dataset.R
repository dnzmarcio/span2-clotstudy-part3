# Author: Marcio Augusto Diniz

# Setup

rm(list = ls())

# Libraries
source("00_utils.R")

# Corner test index ----
dataset <- read_csv(file = behavior) |>
  clean_names()

dt_outcome <- dataset |>
  select(enro_animal_id,
         crt_bl_right_s9:crt_d30_left_s9) |>
  mutate(alternative_corner_index_d0 = abs(crt_bl_left_s9-crt_bl_right_s9)/
           (crt_bl_left_s9+crt_bl_right_s9),
         alternative_corner_index_d7 = abs(crt_d7_left_s9-crt_d7_right_s9)/
           (crt_d7_left_s9+crt_d7_right_s9),
         alternative_corner_index_d30 = abs(crt_d30_left_s9-crt_d30_right_s9)/
           (crt_d30_left_s9+crt_d30_right_s9)
  ) |>
  select(enro_animal_id,
         alternative_corner_index_d0,
         alternative_corner_index_d7,
         alternative_corner_index_d30)

## Conduct data from corner test ----

dt_conduct <- dataset |> 
  select(enro_animal_id,
         corner_bl_conduct,
         corner_bl_conduct_rsn,
         corner_d7_conduct,
         corner_d7_conduct_rsn,
         corner_d30_conduct,
         corner_d30_conduct_rsn,
         behav_d30_conduct,
         behav_d30_conduct_rsn)  |>
  mutate(corner_bl_conduct = factor(corner_bl_conduct,
                                    levels = 0:1,
                                    labels = c("No", "Yes")),
         corner_bl_conduct_rsn = factor(corner_bl_conduct_rsn,
                                        levels = 1:4,
                                        labels = c("Animal death",
                                                   "Equipment Failure",
                                                   "Not in Experimental Protocol",
                                                   "Other")),
         corner_d7_conduct = factor(corner_d7_conduct,
                                    levels = 0:1,
                                    labels = c("No", "Yes")),
         corner_d7_conduct_rsn = factor(corner_d7_conduct_rsn,
                                        levels = 1:4,
                                        labels = c("Animal death",
                                                   "Equipment Failure",
                                                   "Not in Experimental Protocol",
                                                   "Other")),
         #animal_death_d7 = ifelse(corner_d7_conduct_rsn == "Animal death", "Yes", "No"),
         corner_d30_conduct = ifelse(is.na(corner_d30_conduct), 0, corner_d30_conduct),
         corner_d30_conduct = factor(corner_d30_conduct,
                                     levels = 0:1,
                                     labels = c("No", "Yes")),
         corner_d30_conduct_rsn = factor(corner_d30_conduct_rsn,
                                         levels = 1:3,
                                         labels = c("Animal death",
                                                    "Equipment Failure",
                                                    "Other")),
         # behav_d30_conduct = if_else(is.na(behav_d30_conduct), 0,
         #                             behav_d30_conduct),
         # behav_d30_conduct = factor(behav_d30_conduct,
         #                            levels = 0:1,
         #                            labels = c("No", "Yes")),
         # behav_d30_conduct_rsn = factor(behav_d30_conduct_rsn,
         #                                levels = 1:3,
         #                                labels = c("Animal death",
         #                                           "Equipment Failure",
         #                                           "Other")),
         # corner_d30_conduct_rsn = behav_d30_conduct_rsn,
         # animal_death_d30 = ifelse(corner_d30_conduct_rsn == "Animal death", 
         #                           "Yes", "No"),
         .after = enro_animal_id)

## Feasibility ----

load("data/span2_clotstudy_p3_feasibility_data.Rdata")

## MRI data ----

dt_mri <- read_csv(file = mri) |>
  clean_names() |> 
  select(enro_animal_id, p_mri_d2_fract_les)

### Modified ITT ----
dt_outcome_mitt <- left_join(dt_feasibility$mitt, 
                             dt_outcome,  by = "enro_animal_id") |>
  select(enro_animal_id, contains("corner_index"))

dt_conduct_mitt <- left_join(dt_feasibility$mitt, 
                             dt_conduct,  by = "enro_animal_id") |>
  left_join(dt_mri,  by = "enro_animal_id") |>
  select(enro_animal_id, site, enro_model, enro_sex,
         srg_animal_age, srg_weight, 
         p_mri_d2_fract_les,
         txas_reperfusion, clot_length,
         animal_death_d7 = animal_death_before_conduct_d7, 
         animal_death_d30 = animal_death_before_conduct_d30,
         corner_bl_conduct, corner_bl_conduct_rsn,
         corner_d7_conduct, corner_d7_conduct_rsn,
         corner_d30_conduct, corner_d30_conduct_rsn)


### Full Treatment ----
dt_outcome_pp <- left_join(dt_feasibility$pp, 
                           dt_outcome,  
                           by = "enro_animal_id") |>
  select(enro_animal_id, contains("corner_index")) 

dt_conduct_pp <- left_join(dt_feasibility$pp,
                           dt_conduct,  by = "enro_animal_id")   |>
  left_join(dt_mri,  by = "enro_animal_id") |>
  select(enro_animal_id, site, enro_model, enro_sex,
         srg_animal_age, srg_weight, 
         p_mri_d2_fract_les,
         txas_reperfusion, clot_length, 
         animal_death_d7 = animal_death_before_conduct_d7, 
         animal_death_d30  = animal_death_before_conduct_d30, 
         behav_d30_conduct, behav_d30_conduct_rsn,
         corner_bl_conduct, corner_bl_conduct_rsn,
         corner_d7_conduct, corner_d7_conduct_rsn,
         corner_d30_conduct, corner_d30_conduct_rsn)

# Saving output ----

rm(list=setdiff(ls(), c("data_labels",
                        "dt_outcome_mitt", "dt_conduct_mitt",
                        "dt_outcome_pp", "dt_conduct_pp")))
save.image("data/span2_clotstudy_p3_corner_test_data.Rdata")