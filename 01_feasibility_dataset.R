#Authors: Karni Bedirian, Marcio Augusto Diniz

# Setup

rm(list = ls())


subDir <- "data_checks"
if (!dir.exists(file.path(subDir))) {
  dir.create(file.path(subDir))
}
save <- FALSE



# Libraries
source("00_utils.R")


## Feasibility ----
dataset_feas <- 
  read_csv(file = feasibility) |>
  clean_names()

dataset_mri <- 
  read_csv(file = mri) |>
  clean_names() |>
  filter(!is.na(enro_animal_id)) |>
  select(enro_animal_id, mri_d2_conduct, p_mri_d2_fract_les_right)

# Filter animal ids that have lesion farction of right < 0.03 
tmp_ids_lesion_cutoff <- dataset_mri |>
  filter(p_mri_d2_fract_les_right < 0.03) |>
  select(enro_animal_id)

dataset_mri <- dataset_mri |>
  select(enro_animal_id, mri_d2_conduct)

dataset_behav <- 
  read_csv(file = behavior) |>
  clean_names() |>
  filter(!is.na(enro_animal_id)) |>
  select(enro_animal_id, corner_d7_conduct)

dataset_feas$postop_d4_weight[dataset_feas$enro_animal_id == "RQ9447"] <- 195.7

dataset <- left_join(dataset_feas, dataset_mri, by = "enro_animal_id") |>
  left_join(dataset_behav, by = "enro_animal_id")

# Exclude animals that have lesion farction of right < 0.03 
dataset <- dataset |>
  filter(!(enro_animal_id %in% tmp_ids_lesion_cutoff$enro_animal_id))


## Data labeling ----

dt <- dataset %>%
  filter(!is.na(enro_animal_id)) |>
  mutate(site = str_to_upper(redcap_data_access_group),
         .after = enro_animal_id) |>
  mutate(enro_sex = factor(enro_sex, levels = 0:3,
                           labels = c("Female", "Male", 
                                      "Ovariectomized Female", 
                                      "Neutered Maled")),
         enro_model = factor(enro_model, levels = c(1, 2, 3, 4, 5),
                             labels = c("SHR", "OIH",
                                        "Aged", "YHM", "YHR")),
         rand_conduct = factor(rand_conduct,
                               levels = 0:1,
                               labels = c("No", "Yes")),
         rand_conduct_rsn = factor(rand_conduct_rsn,
                                   levels = 1:3,
                                   labels = c("Animal death",
                                              "Equipment Failure",
                                              "Other")),
         txas_reperfusion = factor(txas_reperfusion,
                                   levels = c(2, 1),
                                   labels = c("Control", "TNKase")),
         txas_reperfusion_actual = factor(txas_reperfusion_actual,
                                       levels = c(2, 1),
                                       labels = c("Control", "TNKase")),
         rand_clot_length = as.factor(rand_clot_length),
         srg_conduct = factor(srg_conduct,
                              levels = 0:1,
                              labels = c("No", "Yes")),
         srg_conduct_rsn = factor(srg_conduct_rsn,
                                  levels = 1:3,
                                  labels = c("Animal death",
                                             "Equipment Failure",
                                             "Other")),
         postop_d1_nds_score_conduct = 
           ifelse(is.na(postop_d1_nds_score), "No", "Yes"),
         postop_d2_nds_score_conduct = 
           ifelse(is.na(postop_d2_nds_score), "No", "Yes"),
         srg_nds_score = as.factor(srg_nds_score),
         postop_d1_nds_score = as.factor(postop_d1_nds_score),
         postop_d2_nds_score = as.factor(postop_d2_nds_score),
         postop_d1_nds_score_conduct = as.factor(postop_d1_nds_score_conduct),
         postop_d2_nds_score_conduct = as.factor(postop_d2_nds_score_conduct),
         eos_nds_score = as.factor(eos_nds_score),
         eos_nds_score_conduct =
           ifelse(is.na(eos_nds_score), "No", "Yes"),
         eos_nds_score_conduct = as.factor(eos_nds_score_conduct),
         srg_concom_meds_sq = factor(srg_concom_meds_sq, levels = 0:1,
                                     labels = c("No", "Yes")),
         srg_concom_meds_lrs_nacl = factor(srg_concom_meds_lrs_nacl, levels = 0:1,
                                      labels = c("No", "Yes")),
         srg_comments_1 = factor(srg_comments_1,
                                   levels = 0:1,
                                   labels = c("No", "Yes")),
         srg_comments_2 = factor(srg_comments_2,
                                   levels = 0:1,
                                   labels = c("No", "Yes")),
         srg_comments_3 = factor(srg_comments_3,
                                   levels = 0:1,
                                   labels = c("No", "Yes")),
         srg_comments_4 = factor(srg_comments_4,
                                   levels = 0:1,
                                   labels = c("No", "Yes")),
         srg_comments_5 = factor(srg_comments_5,
                                   levels = 0:1,
                                   labels = c("No", "Yes")),
         srg_comments_6 = factor(srg_comments_6,
                                   levels = 0:1,
                                   labels = c("No", "Yes")),
         srg_comments_7 = factor(srg_comments_7,
                                   levels = 0:1,
                                   labels = c("No", "Yes")),
         srg_comments_8 = factor(srg_comments_8,
                                   levels = 0:1,
                                   labels = c("No", "Yes")),
         srg_comments_9 = factor(srg_comments_9,
                                   levels = 0:1,
                                   labels = c("No", "Yes"))) |>  
  mutate(successful_surgery = ifelse(!is.na(srg_reperfsn_vial), 
                                     "Yes", "No"),
         full_dose = if_else((srg_reperfsn_dose >= 
                                0.75*rand_reperfusion_dose & 
                                srg_reperfsn_dose <= 
                                1.25*rand_reperfusion_dose), "Yes", "No"),
         .after = srg_exact_occ) |>
  mutate(
    compliance_vial_id = case_when(
      rand_reperfusion_vial == srg_reperfsn_vial ~ "Yes",
      rand_reperfusion_vial != srg_reperfsn_vial ~ "No",
      .default = NA
    ),
    compliance_donor_id = case_when(
      enro_animal_id != srg_donor_eartag ~ "Yes",
      enro_animal_id == srg_donor_eartag ~ "No",
      .default = NA
    ),
    compliance_tnk_adm = 
      case_when(
        srg_reperfsn_dose == rand_reperfusion_dose ~ "Yes",
        srg_reperfsn_dose != rand_reperfusion_dose ~ "No",
        .default = NA
      ),
    compliance_tnk_dose = 
      case_when(
        (srg_reperfsn_dose >= (rand_reperfusion_dose * (1 - 0.25)) & 
        srg_reperfsn_dose <= (rand_reperfusion_dose * (1 + 0.25))) ~ "Yes",
        !(srg_reperfsn_dose >= (rand_reperfusion_dose * (1 - 0.25)) & 
           srg_reperfsn_dose <= (rand_reperfusion_dose * (1 + 0.25))) ~ "No",
        .default = NA
      ),
    srg_clot_length = round(srg_clot_length),
    compliance_clot_length = 
      case_when(
        srg_clot_length == rand_clot_length ~ "Yes",
        srg_clot_length != rand_clot_length ~ "No",
        .default = NA
      ),
    srg_embolic_draw_sex = factor(srg_embolic_draw_sex, levels = 0:3,
                                  labels = c("Female", "Male", 
                                             "Ovariectomized Female", 
                                             "Neutered Maled")),
    compliance_donor_sex = 
      case_when(
        srg_embolic_draw_sex == enro_sex ~ "Yes",
        srg_embolic_draw_sex != enro_sex ~ "No",
        .default = NA
      ),
    srg_actual_surg_dt = 
      as.POSIXct(srg_actual_surg_dt, format = "%m/%d/%Y"),
    srg_embolic_draw_dt =
      as.POSIXct(srg_embolic_draw_dt, format = "%m/%d/%Y %H:%M"),
    srg_embolic_draw_time_diff = 
      difftime(as.POSIXct(srg_actual_surg_dt, format = "%m/%d/%Y"), 
               as.POSIXct(srg_embolic_draw_dt, format = "%m/%d/%Y"), 
               units = "days"),
    srg_embolic_draw_time_diff = as.numeric(srg_embolic_draw_time_diff),
    compliance_clot_draw_srg_date = 
      case_when(
        srg_actual_surg_dt != srg_embolic_draw_dt ~ "Yes",
        srg_actual_surg_dt == srg_embolic_draw_dt ~ "No",
        .default = NA
      )) |> 
  mutate(
    animal_death_before_conduct_d30 = ifelse(behav_d30_conduct == 0 &
                                               eos_day_diff_srg_death <= 30,
                                             "Yes", "No"),
    animal_death_before_conduct_d7 = ifelse(corner_d7_conduct == 0 &
                                              eos_day_diff_srg_death <= 7, 
                                            "Yes", "No"),
    animal_death_before_conduct_d3 = ifelse(mri_d2_conduct == 0 &
                                               eos_day_diff_srg_death <= 3, 
                                            "Yes", "No")) |>
  select(-contains("redcap")) |>
  mutate(srg_clot_length = as.factor(round(srg_clot_length, 0)))

# Full data ----

# txas_group for mITT population
# txas_reperfusion_actual for partial treatment, PP, loss follow up, full data.

dt_feasibility <- list()

aux <- dt

dt_feasibility$all <- aux |>
  mutate(enrolled = if_else(!is.na(enro_animal_id),
                            "Yes", "No"),
         ineligible_for_randomization =
           if_else(rand_conduct == "No", 
                   "Yes", "No"),
         itt = ifelse(rand_conduct == "Yes", 
                      "Yes", "No"),
         exclusion_from_treatment =
           if_else(rand_conduct == "Yes" &
                     srg_conduct == "No",
                   "Yes", "No"),
         procedural_dropout =
           if_else(rand_conduct == "Yes" &
                     srg_conduct == "Yes" & 
                     successful_surgery == "No", "Yes", "No"),
         mitt =
           if_else(rand_conduct == "Yes" &
                     srg_conduct == "Yes" & 
                     successful_surgery == "Yes", "Yes", "No"),
         pp =
           if_else(rand_conduct == "Yes" &
                     srg_conduct == "Yes" & 
                     successful_surgery == "Yes" & 
                     full_dose == "Yes",
                   "Yes", "No"),
         loss_followup = 
           if_else(rand_conduct == "Yes" &
                     srg_conduct == "Yes" & 
                     successful_surgery == "Yes" &
                     full_dose == "Yes" &
                     behav_d30_conduct == 0,
                   "Yes", "No"),
         full_data =
           if_else(rand_conduct == "Yes" &
                     srg_conduct == "Yes" & 
                     successful_surgery == "Yes" &
                     full_dose == "Yes" &
                     behav_d30_conduct == 1,
                   "Yes", "No")
         )

dt_feasibility$enrolled <- aux |>
  filter(!is.na(enro_animal_id)) |>
  select(enro_animal_id, 
         site, enro_model, enro_sex)

tmp <- dt_feasibility$enrolled %>% 
  select(enro_animal_id)

if (save)
write.csv(tmp, paste0("data_checks/id_enrolled ", Sys.Date(), ".csv"))

dt_feasibility$ineligible_for_randomization <- aux |> 
  filter(rand_conduct == "No") |>
  select(enro_animal_id, site, enro_sex, enro_model, 
         rand_conduct, rand_conduct_rsn, rand_conduct_rsn_oth)

tmp <- dt_feasibility$ineligible_for_randomization %>% 
  select(enro_animal_id, rand_conduct, rand_conduct_rsn_oth)

if (save)
write.csv(tmp, paste0("data_checks/id_ineligible_for_randomization ", Sys.Date(), ".csv"))


dt_feasibility$itt <- aux |>
  filter(rand_conduct == "Yes") |>
  select(enro_animal_id, site, enro_model, enro_sex,
         rand_conduct, txas_reperfusion, rand_clot_length
         ) 

tmp <- dt_feasibility$itt %>% 
  select(enro_animal_id, rand_conduct)

if (save)
write.csv(tmp, paste0("data_checks/id_itt", Sys.Date(), ".csv"))

dt_feasibility$exclusion_from_treatment <- aux |>
  filter(rand_conduct == "Yes" & srg_conduct == "No") |>
  select(enro_animal_id, site, enro_model, enro_sex,
         rand_conduct, srg_conduct, srg_conduct_rsn, srg_conduct_rsn_oth,
         txas_reperfusion, rand_clot_length) 

tmp <- dt_feasibility$exclusion_from_treatment %>% 
  select(enro_animal_id, rand_conduct, 
         srg_conduct, srg_conduct_rsn, srg_conduct_rsn_oth,)

if (save)
write.csv(tmp, paste0("data_checks/id_exclusion_from_treatment ", Sys.Date(), ".csv"))

dt_feasibility$procedural_dropout <- aux |>
  filter(rand_conduct == "Yes" &
           srg_conduct == "Yes" &
           successful_surgery == "No"
         ) |>
  select(enro_animal_id, rand_conduct, 
         site, enro_model, enro_sex,
         rand_conduct, srg_conduct, 
         successful_surgery, contains("comments"),
         txas_reperfusion, srg_clot_length) 

tmp <- dt_feasibility$procedural_dropout %>%
  select(enro_animal_id, rand_conduct, srg_conduct,
         successful_surgery, contains("comments"))

if (save)
write.csv(tmp, paste0("data_checks/id_procedural_dropout ", Sys.Date(), ".csv"))

dt_feasibility$mitt <- aux |>
  filter(rand_conduct == "Yes" & 
           srg_conduct == "Yes" &
           successful_surgery == "Yes"
         ) |>
  select(enro_animal_id, 
         site, enro_model, enro_sex, 
         rand_conduct, srg_conduct, successful_surgery,
         txas_reperfusion, rand_clot_length, 
         srg_animal_age, srg_weight, 
         srg_length_surgery, srg_exact_occ,
         srg_concom_meds_sq, srg_concom_meds_lrs_nacl,
         enro_weight, srg_weight,
         postop_d1_weight, postop_d2_weight,
         postop_d3_weight, postop_d4_weight, 
         postop_d5_weight, postop_d6_weight,
         postop_d7_weight, postop_d8_weight,
         eos_weight, srg_nds_score,
         postop_d1_nds_score, postop_d2_nds_score, eos_nds_score,
         postop_d1_nds_score_conduct, 
         postop_d2_nds_score_conduct,
         eos_nds_score_conduct,
         eos_day_diff_srg_death, 
         animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30)

tmp <- dt_feasibility$mitt %>% 
  select(enro_animal_id, 
         rand_conduct, srg_conduct, successful_surgery, 
         txas_reperfusion, rand_clot_length)

if (save)
write.csv(tmp, paste0("data_checks/id_mitt ", Sys.Date(), ".csv"))


dt_feasibility$partial_treatment <- aux |>
  filter(rand_conduct == "Yes" &
           srg_conduct == "Yes" &
           successful_surgery == "Yes"  &
           full_dose == "No") |> 
  select(enro_animal_id, 
         site, enro_model, enro_sex, 
         rand_conduct, srg_conduct, successful_surgery,
         txas_reperfusion, srg_clot_length, 
         rand_reperfusion_dose, srg_reperfsn_dose, full_dose)

tmp <- dt_feasibility$partial_treatment %>%
  select(enro_animal_id, 
         rand_conduct, srg_conduct,  successful_surgery,
         rand_reperfusion_dose, srg_reperfsn_dose, full_dose)

if (save)
write.csv(tmp, paste0("data_checks/id_partial_treatment ", Sys.Date(), ".csv"))


dt_feasibility$pp <- aux |>
  filter(rand_conduct == "Yes" &
           srg_conduct == "Yes" &
           successful_surgery == "Yes"  &
           full_dose == "Yes") |> 
  select(enro_animal_id, 
         site, enro_model, enro_sex, 
         rand_conduct, srg_conduct, successful_surgery,
         txas_reperfusion_actual,
         srg_clot_length,
         srg_animal_age, srg_weight, 
         srg_length_surgery, srg_exact_occ,
         srg_concom_meds_sq, srg_concom_meds_lrs_nacl,
         enro_weight, srg_weight,
         postop_d1_weight, postop_d2_weight,
         postop_d3_weight, postop_d4_weight, 
         postop_d5_weight, postop_d6_weight,
         postop_d7_weight, postop_d8_weight,
         eos_weight, srg_nds_score,
         postop_d1_nds_score, postop_d2_nds_score, eos_nds_score,
         postop_d1_nds_score_conduct, 
         postop_d2_nds_score_conduct,
         eos_nds_score_conduct,
         eos_day_diff_srg_death, 
         animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30)

tmp <- dt_feasibility$pp %>%
  select(enro_animal_id, 
         rand_conduct, srg_conduct,
         successful_surgery, 
         txas_reperfusion_actual, 
         srg_clot_length)

if (save)
write.csv(tmp, paste0("data_checks/id_per_protocol ", Sys.Date(), ".csv"))

dt_feasibility$loss_followup <- aux |>
  filter(rand_conduct == "Yes" &
           srg_conduct == "Yes" &
           successful_surgery == "Yes"  &
           full_dose == "Yes" &
           behav_d30_conduct == 0) |> 
  select(enro_animal_id, 
         site, enro_model, enro_sex, 
         rand_conduct, srg_conduct, successful_surgery,
         txas_reperfusion_actual,
         srg_clot_length,
         srg_animal_age, srg_weight, 
         srg_length_surgery, srg_exact_occ,
         srg_concom_meds_sq, srg_concom_meds_lrs_nacl,
         enro_weight, srg_weight,
         postop_d1_weight, postop_d2_weight,
         postop_d3_weight, postop_d4_weight, 
         postop_d5_weight, postop_d6_weight,
         postop_d7_weight, postop_d8_weight,
         eos_weight, srg_nds_score,
         postop_d1_nds_score, postop_d2_nds_score, eos_nds_score,
         postop_d1_nds_score_conduct, 
         postop_d2_nds_score_conduct,
         eos_nds_score_conduct,
         eos_day_diff_srg_death, 
         animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30)

tmp <- dt_feasibility$loss_followup %>%
  select(enro_animal_id, 
         rand_conduct, srg_conduct,
         successful_surgery, 
         txas_reperfusion_actual, 
         srg_clot_length,
         eos_day_diff_srg_death)

if (save)
write.csv(tmp, paste0("data_checks/id_loss_followup ", Sys.Date(), ".csv"))


dt_feasibility$full_data <- aux |>
  filter(rand_conduct == "Yes" &
           srg_conduct == "Yes" &
           successful_surgery == "Yes"  &
           full_dose == "Yes" &
           behav_d30_conduct == 1) |>
  select(enro_animal_id, 
         site, enro_model, enro_sex, 
         rand_conduct, srg_conduct, successful_surgery,
         txas_reperfusion_actual,
         srg_clot_length,
         srg_animal_age, srg_weight, 
         srg_length_surgery, srg_exact_occ,
         srg_concom_meds_sq, srg_concom_meds_lrs_nacl,
         enro_weight, srg_weight,
         postop_d1_weight, postop_d2_weight,
         postop_d3_weight, postop_d4_weight, 
         postop_d5_weight, postop_d6_weight,
         postop_d7_weight, postop_d8_weight,
         eos_weight, srg_nds_score,
         postop_d1_nds_score, postop_d2_nds_score, eos_nds_score,
         postop_d1_nds_score_conduct, 
         postop_d2_nds_score_conduct,
         eos_nds_score_conduct,
         eos_day_diff_srg_death, 
         animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30)

tmp <- dt_feasibility$full_data %>%
  select(enro_animal_id, 
         rand_conduct, srg_conduct,
         successful_surgery, 
         txas_reperfusion_actual, 
         srg_clot_length,
         eos_day_diff_srg_death)

if (save)
write.csv(tmp, paste0("data_checks/id_full_data ", Sys.Date(), ".csv"))

dt_feasibility$comp_mitt <- aux |>
  filter(rand_conduct == "Yes" &
           srg_conduct == "Yes" &
           successful_surgery == "Yes") |>
  select(enro_animal_id, site, enro_sex, enro_model, 
         txas_reperfusion, rand_clot_length,
         compliance_vial_id, compliance_tnk_adm, compliance_tnk_dose, 
         srg_clot_length, compliance_clot_length, 
         srg_donor_eartag, compliance_donor_id,
         srg_embolic_draw_sex, compliance_donor_sex, 
         srg_embolic_draw_time_diff, compliance_clot_draw_srg_date)

dt_feasibility$comp_pp <- aux |>
  filter(rand_conduct == "Yes" &
           srg_conduct == "Yes" &
           successful_surgery == "Yes"  &
           full_dose == "Yes") |>
  select(enro_animal_id, site, enro_sex, enro_model, 
         txas_reperfusion_actual, rand_clot_length,
         compliance_vial_id, compliance_tnk_adm, compliance_tnk_dose, 
         srg_clot_length, compliance_clot_length, 
         srg_donor_eartag, compliance_donor_id,
         srg_embolic_draw_sex, compliance_donor_sex, 
         srg_embolic_draw_time_diff, compliance_clot_draw_srg_date)

# Renaming variables ----

dt_feasibility$mitt <- 
dt_feasibility$mitt |>
  rename(clot_length = rand_clot_length)

dt_feasibility$pp <- 
dt_feasibility$pp |>
  rename(clot_length = srg_clot_length,
         txas_reperfusion = txas_reperfusion_actual)

dt_feasibility$comp_mitt <- 
  dt_feasibility$comp_mitt |>
  rename(clot_length = rand_clot_length)

dt_feasibility$comp_pp <- 
  dt_feasibility$comp_pp |>
  rename(clot_length = srg_clot_length,
         txas_reperfusion = txas_reperfusion_actual)

# Saving output ----

rm(list=setdiff(ls(), "dt_feasibility"))
save.image("data/span2_clotstudy_p3_feasibility_data.Rdata")

