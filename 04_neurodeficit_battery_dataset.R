# Author: Marcio Augusto Diniz

# Setup

rm(list = ls())

# Libraries
source("00_utils.R")

# Neurodeficit scores ----
dataset <- read_csv(file = behavior, 
                    na = c("", "NA", "99")) |>
  clean_names()

dt_outcome <- dataset |> 
  select(enro_animal_id, contains("neuro")) |>
  mutate(n_item =
           as.numeric(!is.na(neuro_d30_spont)) +
           as.numeric(!is.na(neuro_d30_circling)) +
           as.numeric(!is.na(neuro_d30_symmetry)) +
           as.numeric(!is.na(neuro_d30_outstretching))  +
           as.numeric(!is.na(neuro_d30_trunk)) +
           as.numeric(!is.na(neuro_d30_vibrissae)) +
           as.numeric(!is.na(neuro_d30_face)) +
           as.numeric(!is.na(neuro_d30_beam)) +
           as.numeric(!is.na(neuro_d30_climb)),
         denom_duke_score =
           as.numeric(!is.na(neuro_d30_circling)) +
           as.numeric(!is.na(neuro_d30_symmetry)) +
           as.numeric(!is.na(neuro_d30_trunk)) +
           as.numeric(!is.na(neuro_d30_vibrissae)) +
           as.numeric(!is.na(neuro_d30_face)) +
           as.numeric(!is.na(neuro_d30_beam)) +
           as.numeric(!is.na(neuro_d30_climb)),
         denom_mgh_score =
           3*as.numeric(!is.na(neuro_d30_spont)) +
           3*as.numeric(!is.na(neuro_d30_circling)) +
           3*as.numeric(!is.na(neuro_d30_symmetry)) +
           3*as.numeric(!is.na(neuro_d30_outstretching)) +
           3*as.numeric(!is.na(neuro_d30_trunk)) +
           3*as.numeric(!is.na(neuro_d30_vibrissae)) +
           3*as.numeric(!is.na(neuro_d30_climb)),
         denom_reduced_span_score =
           0.504*3*as.numeric(!is.na(neuro_d30_symmetry)) +
           0.507*3*as.numeric(!is.na(neuro_d30_outstretching)) +
           0.109*3*as.numeric(!is.na(neuro_d30_trunk)) +
           0.311*3*as.numeric(!is.na(neuro_d30_vibrissae)) +
           0.276*3*as.numeric(!is.na(neuro_d30_face)) +
           0.216*3*as.numeric(!is.na(neuro_d30_beam)),
         o_neuro_d30_spont = neuro_d30_spont,
         o_neuro_d30_circling = neuro_d30_circling,
         o_neuro_d30_symmetry = neuro_d30_symmetry,
         o_neuro_d30_outstretching = neuro_d30_outstretching,
         o_neuro_d30_trunk = neuro_d30_trunk,
         o_neuro_d30_vibrissae = neuro_d30_vibrissae,
         o_neuro_d30_face = neuro_d30_face,
         o_neuro_d30_beam = neuro_d30_beam,
         o_neuro_d30_climb = neuro_d30_climb,
         across(neuro_d30_spont:neuro_d30_climb, ~
                  ifelse(n_item > 1 & n_item < 9,
                         ifelse(is.na(.x), 0, .x), .x)),
         full_span_score = neuro_d30_spont + neuro_d30_circling +
           neuro_d30_symmetry + neuro_d30_outstretching  +
           neuro_d30_trunk + neuro_d30_vibrissae +
           neuro_d30_face + neuro_d30_beam +
           neuro_d30_climb,
         simple_span_score = as.numeric(neuro_d30_spont > 0) +
           as.numeric(neuro_d30_circling > 0) +
           as.numeric(neuro_d30_symmetry > 0) +
           as.numeric(neuro_d30_outstretching  > 0) +
           as.numeric(neuro_d30_trunk > 0) +
           as.numeric(neuro_d30_vibrissae > 0) +
           as.numeric(neuro_d30_face > 0) +
           as.numeric(neuro_d30_beam > 0) +
           as.numeric(neuro_d30_climb > 0),
         duke_score = as.numeric(neuro_d30_circling > 0) +
           as.numeric(neuro_d30_symmetry > 0) +
           as.numeric(neuro_d30_trunk > 0) +
           as.numeric(neuro_d30_vibrissae > 0) +
           as.numeric(neuro_d30_face > 0) +
           as.numeric(neuro_d30_beam > 0) +
           as.numeric(neuro_d30_climb > 0),
         mgh_score = neuro_d30_spont +
           neuro_d30_circling +
           neuro_d30_symmetry +
           neuro_d30_outstretching  +
           neuro_d30_trunk +
           neuro_d30_vibrissae +
           neuro_d30_climb,
         reduced_span_score = neuro_d30_symmetry*0.504 +
           neuro_d30_outstretching*0.507 +
           neuro_d30_trunk*0.109 +
           neuro_d30_vibrissae*0.311 +
           neuro_d30_face*0.276 +
           neuro_d30_beam*0.216,
         adjusted_full_span_score = ifelse(n_item > 1 & n_item < 9,
                                           (27*full_span_score)/(3*n_item),
                                           full_span_score),
         adjusted_simple_span_score = ifelse(n_item > 1 & n_item < 9,
                                             (9*simple_span_score)/n_item,
                                             simple_span_score),
         adjusted_duke_score = ifelse(n_item > 1 & n_item < 9,
                                      (7*duke_score)/denom_duke_score,
                                      duke_score),
         adjusted_mgh_score = ifelse(n_item > 1 & n_item < 9,
                                     (21*mgh_score)/denom_mgh_score,
                                     mgh_score),
         adjusted_reduced_span_score = ifelse(n_item > 1 & n_item < 9,
                                              (5.769*reduced_span_score)/denom_reduced_span_score,
                                              reduced_span_score),
         neuro_d30_spont = o_neuro_d30_spont,
         neuro_d30_circling = o_neuro_d30_circling,
         neuro_d30_symmetry = o_neuro_d30_symmetry,
         neuro_d30_outstretching = o_neuro_d30_outstretching,
         neuro_d30_trunk = o_neuro_d30_trunk,
         neuro_d30_vibrissae = o_neuro_d30_vibrissae,
         neuro_d30_face = o_neuro_d30_face,
         neuro_d30_beam = o_neuro_d30_beam,
         neuro_d30_climb = o_neuro_d30_climb) |>
  select(enro_animal_id,
         full_span_score = adjusted_full_span_score,
         simple_span_score = adjusted_simple_span_score,
         duke_score = adjusted_duke_score, 
         mgh_score = adjusted_mgh_score, 
         reduced_span_score)

## Conduct data from corner test ----

dt_conduct <- dataset |> 
  select(enro_animal_id,
         behav_d30_conduct,
         behav_d30_conduct_rsn)  |>
  mutate(behav_d30_conduct = if_else(is.na(behav_d30_conduct), 0,
                                     behav_d30_conduct),
         behav_d30_conduct = factor(behav_d30_conduct,
                                     levels = 0:1,
                                     labels = c("No", "Yes")),
         behav_d30_conduct_rsn = factor(behav_d30_conduct_rsn,
                                         levels = 1:3,
                                         labels = c("Animal death",
                                                    "Equipment Failure",
                                                    "Other")),
         .after = enro_animal_id)

## Feasibility ----

load("data/span2_clotstudy_p3_feasibility_data.Rdata")

# MRI data ----

dt_mri <- read_csv(file = mri) |>
   clean_names() |> 
   select(enro_animal_id, p_mri_d2_fract_les)

### Modified ITT ----
dt_outcome_mitt <- left_join(dt_feasibility$mitt, 
                             dt_outcome,  by = "enro_animal_id") |>
  select(enro_animal_id, contains("score"))

dt_conduct_mitt <- left_join(dt_feasibility$mitt, 
                             dt_conduct,  by = "enro_animal_id") |>
  left_join(dt_mri,  by = "enro_animal_id") |>
  select(enro_animal_id, site, enro_model, enro_sex,
         srg_animal_age, srg_weight, 
         p_mri_d2_fract_les,
         txas_reperfusion, clot_length,
         animal_death_before_conduct_d3, 
         animal_death_before_conduct_d30, 
         behav_d30_conduct, behav_d30_conduct_rsn)

### Per-Protocol ----
dt_outcome_pp <- left_join(dt_feasibility$pp, 
                           dt_outcome,  
                           by = "enro_animal_id") |>
  select(enro_animal_id, contains("score")) 

dt_conduct_pp <- left_join(dt_feasibility$pp,
                           dt_conduct,  by = "enro_animal_id")   |>
  left_join(dt_mri,  by = "enro_animal_id") |>
  select(enro_animal_id, site, enro_sex, enro_model, 
         srg_animal_age, srg_weight, 
         p_mri_d2_fract_les,
         txas_reperfusion, clot_length,
         animal_death_before_conduct_d3, 
         animal_death_before_conduct_d30,
         behav_d30_conduct, behav_d30_conduct_rsn)

# Saving output ----

rm(list=setdiff(ls(), c("dt_outcome_mitt",
                        "dt_conduct_mitt",
                        "dt_outcome_pp",
                        "dt_conduct_pp")))
save.image("data/span2_clotstudy_p3_neuro_d30_data.Rdata")

