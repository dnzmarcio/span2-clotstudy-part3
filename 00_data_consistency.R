#Author: Marcio Augusto Diniz, Karni Bedirian

# Setup ----

rm(list = ls())
file <- "span2_clotstudy_data_checks"

# Libraries ----
source("00_utils.R")

# Data ----

dataset_feas <-
  read_csv(file = feasibility) |>
  clean_names() |>
  filter(!is.na(enro_animal_id))

dataset_bhv <-
  read_csv(file = behavior) |>
  clean_names() |>
  filter(!is.na(enro_animal_id))

dataset_mri <-
  read_csv(file = mri) |>
  clean_names() |>
  filter(!is.na(enro_animal_id))

# Feasibility dataset ----

load("data/span2_clotstudy_feasibility_data.Rdata")

### Are partial dose and full dose conflicting? ----
aux <- dt_feasibility$mitt |>
  select(enro_animal_id, stage, site,
         partial_dose, full_dose, rand_conduct, srg_conduct,
         successful_surgery, srg_admin_typ) |>
  filter(rand_conduct == "No")

dose_yesyes <-
  aux |> filter(partial_dose == "Yes" &
                  full_dose == "Yes")
#print(dose_yesyes)
print(nrow(dose_yesyes))

if(nrow(dose_yesyes) > 0)
write_csv(dose_yesyes, file = "partial_dose_yes_full_dose_yes.csv")

dose_nono <-
  aux |> filter(partial_dose == "No" &
                  full_dose == "No")
#print(dose_nono)
print(nrow(dose_nono))

if(nrow(dose_nono) > 0)
write_csv(dose_nono, file = "partial_dose_no_full_dose_no.csv")

### Is rand_conduct missing? ----
aux <- dataset_feas
aux <-  aux |> filter(is.na(rand_conduct))
#print(aux)
print(nrow(aux))

if (nrow(aux) > 0)
write.csv(aux, file = "rand_conduct_missing.csv")

# NeuroDeficit Score Dataset ----

### Is conduct variable consistent with the NeuroDeficit values? ----

aux <- dataset_bhv

#### Conduct: Yes ----

dt_conduct_yes <- aux  %>%
  filter(behav_d30_conduct == 1)

print(nrow(dt_conduct_yes))

dt_conduct_yes_val_yes <- aux  %>%
  filter(behav_d30_conduct == 1 &
           !is.na(neuro_d30_spont) &
           !is.na(neuro_d30_circling) &
           !is.na(neuro_d30_symmetry) &
           !is.na(neuro_d30_outstretching) &
           !is.na(neuro_d30_trunk) &
           !is.na(neuro_d30_vibrissae) &
           !is.na(neuro_d30_face) &
           !is.na(neuro_d30_beam) &
           !is.na(neuro_d30_climb))

print(nrow(dt_conduct_yes_val_yes))


dt_conduct_yes_val_no <- aux  %>%
  filter(behav_d30_conduct == 1 &
           !(!is.na(neuro_d30_spont) &
           !is.na(neuro_d30_circling) &
           !is.na(neuro_d30_symmetry) &
           !is.na(neuro_d30_outstretching) &
           !is.na(neuro_d30_trunk) &
           !is.na(neuro_d30_vibrissae) &
           !is.na(neuro_d30_face) &
           !is.na(neuro_d30_beam) &
           !is.na(neuro_d30_climb))) |>
  select(enro_animal_id, behav_d30_conduct,
         neuro_d30_spont:neuro_d30_climb)


#print(dt_conduct_yes_val_no$enro_animal_id)
print(nrow(dt_conduct_yes_val_no))

if (nrow(dt_conduct_yes_val_no) > 0)
  write.csv(dt_conduct_yes_val_no,
          paste0("neurodeficit_d30_conduct_yes_values_no_stage1",
                 Sys.Date(),
                 ".csv"))

#### Conduct: No ----

dt_conduct_no <- aux  %>%
  filter(behav_d30_conduct == 0)

#print(dt_conduct_yes_val_yes$enro_animal_id)
print(nrow(dt_conduct_no))

dt_conduct_no_val_no <- aux  %>%
  filter(behav_d30_conduct == 0 &
           is.na(neuro_d30_spont) &
           is.na(neuro_d30_circling) &
           is.na(neuro_d30_symmetry) &
           is.na(neuro_d30_outstretching) &
           is.na(neuro_d30_trunk) &
           is.na(neuro_d30_vibrissae) &
           is.na(neuro_d30_face) &
           is.na(neuro_d30_beam) &
           is.na(neuro_d30_climb))

#print(dt_conduct_no_val_no$enro_animal_id)
print(nrow(dt_conduct_no_val_no))

dt_conduct_no_val_yes <- aux  %>%
  filter(behav_d30_conduct == 0 &
           !(is.na(neuro_d30_spont) &
           is.na(neuro_d30_circling) &
           is.na(neuro_d30_symmetry) &
           is.na(neuro_d30_outstretching) &
           is.na(neuro_d30_trunk) &
           is.na(neuro_d30_vibrissae) &
           is.na(neuro_d30_face) &
           is.na(neuro_d30_beam) &
           is.na(neuro_d30_climb))) |>
  select(enro_animal_id, behav_d30_conduct,
         neuro_d30_spont:neuro_d30_climb)

#print(dt_conduct_no_val_yes$enro_animal_id)
print(nrow(dt_conduct_no_val_yes))

if (nrow(dt_conduct_no_val_yes) > 0)
  write.csv(dt_conduct_no_val_yes,
          paste0("neurodedeficit_d30_conduct_no_values_yes_stage1",
                 Sys.Date(),
                 ".csv"))

# Create the table
tab <- matrix(c(nrow(dt_conduct_yes_val_yes), nrow(dt_conduct_yes_val_no),
                nrow(dt_conduct_no_val_yes), nrow(dt_conduct_no_val_no)), nrow=2, ncol=2)
rownames(tab) <- c("Value Yes", "Value No")
colnames(tab) <- c("Conduct Yes", "Conduct No")

# Create table
kable(tab)

# Corner Score Dataset ----

### Is conduct variable consistent with the outcome values? ----

aux <- dataset_bhv

#### Day 0 Conduct: Yes ----

dt_conduct_yes <- aux  %>%
  filter(corner_bl_conduct == 1)

print(nrow(dt_conduct_yes))

dt_conduct_yes_val_yes <- aux  %>%
  filter(corner_bl_conduct == 1 &
           !is.na(crt_bl_left_s9) &
           !is.na(crt_bl_right_s9))

print(nrow(dt_conduct_yes_val_yes))


dt_conduct_yes_val_no <- aux  %>%
  filter(corner_bl_conduct == 1 &
           !(!is.na(crt_bl_left_s9) &
           !is.na(crt_bl_right_s9))) |>
  select(enro_animal_id, corner_bl_conduct,
         crt_bl_left_s9, crt_bl_right_s9)


#print(dt_conduct_yes_val_no$enro_animal_id)
print(nrow(dt_conduct_yes_val_no))

if (nrow(dt_conduct_yes_val_no) > 0)
write.csv(dt_conduct_yes_val_no,
          paste0("cornertest_d0_conduct_yes_values_no_stage1",
                 Sys.Date(),
                 ".csv"))

#### Day 0 Conduct: No ----

dt_conduct_no <- aux  %>%
  filter(corner_bl_conduct == 0)

#print(dt_conduct_yes_val_yes$enro_animal_id)
print(nrow(dt_conduct_no))

dt_conduct_no_val_no <- aux  %>%
  filter(corner_bl_conduct == 0 &
           is.na(crt_bl_left_s9) &
           is.na(crt_bl_right_s9))

#print(dt_conduct_no_val_no$enro_animal_id)
print(nrow(dt_conduct_no_val_no))

dt_conduct_no_val_yes <- aux  %>%
  filter(corner_bl_conduct == 0 &
         !(is.na(crt_bl_left_s9) &
           is.na(crt_bl_right_s9))) |>
  select(enro_animal_id, corner_bl_conduct,
         crt_bl_left_s9, crt_bl_right_s9)

#print(dt_conduct_no_val_yes$enro_animal_id)
print(nrow(dt_conduct_no_val_yes))

if (nrow(dt_conduct_no_val_yes) > 0)
  write.csv(dt_conduct_no_val_yes,
            paste0("cornertest_d0_conduct_no_values_yes_clotstudy",
                   Sys.Date(),
                   ".csv"))

# Create the table
tab <- matrix(c(nrow(dt_conduct_yes_val_yes), nrow(dt_conduct_yes_val_no),
                nrow(dt_conduct_no_val_yes), nrow(dt_conduct_no_val_no)), nrow=2, ncol=2)
rownames(tab) <- c("Value Yes", "Value No")
colnames(tab) <- c("Conduct Yes", "Conduct No")

# Create table
kable(tab)

#### Day 7 Conduct: Yes ----

dt_conduct_yes <- aux  %>%
  filter(corner_d7_conduct == 1)

print(nrow(dt_conduct_yes))

dt_conduct_yes_val_yes <- aux  %>%
  filter(corner_d7_conduct == 1 &
           !is.na(crt_d7_left_s9) &
           !is.na(crt_d7_right_s9))

print(nrow(dt_conduct_yes_val_yes))


dt_conduct_yes_val_no <- aux  %>%
  filter(corner_d7_conduct == 1 &
           !(!is.na(crt_d7_left_s9) &
               !is.na(crt_d7_right_s9))) |>
  select(enro_animal_id, corner_d7_conduct,
         crt_d7_left_s9, crt_d7_right_s9)


#print(dt_conduct_yes_val_no$enro_animal_id)
print(nrow(dt_conduct_yes_val_no))

if (nrow(dt_conduct_yes_val_no) > 0)
write.csv(dt_conduct_yes_val_no,
          paste0("cornertest_d7_conduct_yes_values_no_stage1",
                 Sys.Date(),".csv"))

#### Day 7 Conduct: No ----

dt_conduct_no <- aux  %>%
  filter(corner_d7_conduct == 0)

#print(dt_conduct_yes_val_yes$enro_animal_id)
print(nrow(dt_conduct_no))

dt_conduct_no_val_no <- aux  %>%
  filter(corner_d7_conduct == 0 &
           is.na(crt_d7_left_s9) &
           is.na(crt_d7_right_s9))

#print(dt_conduct_no_val_no$enro_animal_id)
print(nrow(dt_conduct_no_val_no))

dt_conduct_no_val_yes <- aux  %>%
  filter(corner_d7_conduct == 0 &
         !(is.na(crt_d7_left_s9) &
             is.na(crt_d7_right_s9))) |>
  select(enro_animal_id, corner_d7_conduct,
         crt_d7_left_s9, crt_d7_right_s9)

#print(dt_conduct_no_val_yes$enro_animal_id)
print(nrow(dt_conduct_no_val_yes))

if (nrow(dt_conduct_no_val_yes) > 0)
write.csv(dt_conduct_no_val_yes,
          paste0("cornertest_d7_conduct_no_values_yes_stage1",
                 Sys.Date(),".csv"))

# Create the table
tab <- matrix(c(nrow(dt_conduct_yes_val_yes), nrow(dt_conduct_yes_val_no),
                nrow(dt_conduct_no_val_yes), nrow(dt_conduct_no_val_no)), nrow=2, ncol=2)
rownames(tab) <- c("Value Yes", "Value No")
colnames(tab) <- c("Conduct Yes", "Conduct No")

# Create table
kable(tab)


#### Day 30 Conduct: Yes ----

dt_conduct_yes <- aux  %>%
  filter(corner_d30_conduct == 1)

print(nrow(dt_conduct_yes))

dt_conduct_yes_val_yes <- aux  %>%
  filter(corner_d30_conduct == 1 &
           !is.na(crt_d30_left_s9) &
           !is.na(crt_d30_right_s9))

print(nrow(dt_conduct_yes_val_yes))


dt_conduct_yes_val_no <- aux  %>%
  filter(corner_d30_conduct == 1 &
           !(!is.na(crt_d30_left_s9) &
               !is.na(crt_d30_right_s9))) |>
  select(enro_animal_id, corner_d30_conduct,
         crt_d30_left_s9, crt_d30_right_s9)


#print(dt_conduct_yes_val_no$enro_animal_id)
print(nrow(dt_conduct_yes_val_no))

if (nrow(dt_conduct_yes_val_no) > 0)
write.csv(dt_conduct_yes_val_no,
          paste0("cornertest_d30_conduct_yes_values_no_stage1",
                 Sys.Date(),".csv"))

#### Day 30 Conduct: No ----

dt_conduct_no <- aux  %>%
  filter(corner_d30_conduct == 0)

#print(dt_conduct_yes_val_yes$enro_animal_id)
print(nrow(dt_conduct_no))

dt_conduct_no_val_no <- aux  %>%
  filter(corner_d30_conduct == 0 &
           is.na(crt_d30_left_s9) &
           is.na(crt_d30_right_s9))

#print(dt_conduct_no_val_no$enro_animal_id)
print(nrow(dt_conduct_no_val_no))

dt_conduct_no_val_yes <- aux  %>%
  filter(corner_d30_conduct == 0 &
           !(is.na(crt_d30_left_s9) &
               is.na(crt_d30_right_s9))) |>
  select(enro_animal_id, corner_d30_conduct,
         crt_d30_left_s9, crt_d30_right_s9)

#print(dt_conduct_no_val_yes$enro_animal_id)
print(nrow(dt_conduct_no_val_yes))

if (nrow(dt_conduct_no_val_yes) > 0)
write.csv(dt_conduct_no_val_yes,
          paste0("cornertest_d30_conduct_no_values_yes_stage1",
                 Sys.Date(),".csv"))

# Create the table
tab <- matrix(c(nrow(dt_conduct_yes_val_yes), nrow(dt_conduct_yes_val_no),
                nrow(dt_conduct_no_val_yes), nrow(dt_conduct_no_val_no)), nrow=2, ncol=2)
rownames(tab) <- c("Value Yes", "Value No")
colnames(tab) <- c("Conduct Yes", "Conduct No")

# Create table
kable(tab)

# MRI Dataset ----

### Is MRI conduct consistent with the MRI values? ----

aux <- dataset_mri
with(aux, table(mri_d2_conduct, p_mri_d2_ind))

#### Conduct: Yes

dt_conduct_yes <- aux  %>%
  filter(mri_d2_conduct == 1 & p_mri_d2_ind == 1)

print(nrow(dt_conduct_yes))

dt_conduct_yes_val_yes <- aux  %>%
  filter(mri_d2_conduct == 1 &
           !is.na(p_mri_d2_fract_csf) &
           !is.na(p_mri_d2_fract_csf_left) &
           !is.na(p_mri_d2_fract_csf_right) &
           !is.na(p_mri_d2_fract_tis) &
           !is.na(p_mri_d2_fract_tis_left) &
           !is.na(p_mri_d2_fract_tis_right) &
           !is.na(p_mri_d2_fract_les) &
           !is.na(p_mri_d2_fract_les_left) &
           !is.na(p_mri_d2_fract_les_right))

#print(dt_conduct_yes_val_yes$enro_animal_id)
print(nrow(dt_conduct_yes_val_yes))

dt_conduct_yes_val_no <- aux  %>%
  filter(mri_d2_conduct == 1 & p_mri_d2_ind == 1 &
           !(!is.na(p_mri_d2_fract_csf) &
           !is.na(p_mri_d2_fract_csf_left) &
           !is.na(p_mri_d2_fract_csf_right) &
           !is.na(p_mri_d2_fract_tis) &
           !is.na(p_mri_d2_fract_tis_left) &
           !is.na(p_mri_d2_fract_tis_right) &
           !is.na(p_mri_d2_fract_les) &
           !is.na(p_mri_d2_fract_les_left) &
           !is.na(p_mri_d2_fract_les_right))) |>
  select(enro_animal_id, mri_d2_conduct, p_mri_d2_ind,
         p_mri_d2_fract_csf:p_mri_d2_fract_les_right)

# dt_conduct_yes_pip_no_val_no <- aux  %>%
#   filter(mri_d2_conduct == 1 & p_mri_d2_ind == 0 &
#            !(!is.na(p_mri_d2_fract_csf) &
#                !is.na(p_mri_d2_fract_csf_left) &
#                !is.na(p_mri_d2_fract_csf_right) &
#                !is.na(p_mri_d2_fract_tis) &
#                !is.na(p_mri_d2_fract_tis_left) &
#                !is.na(p_mri_d2_fract_tis_right) &
#                !is.na(p_mri_d2_fract_les) &
#                !is.na(p_mri_d2_fract_les_left) &
#                !is.na(p_mri_d2_fract_les_right))) |>
#   select(enro_animal_id, mri_d2_conduct, p_mri_d2_ind,
#          p_mri_d2_fract_csf:p_mri_d2_fract_les_right)


#print(dt_conduct_yes_val_no$enro_animal_id)
print(nrow(dt_conduct_yes_val_no))

# This discrepancy is because the pipeline was not successful
table(dt_conduct_yes_val_no$p_mri_d2_ind)

if (nrow(dt_conduct_yes_val_no) > 0)
write.csv(dt_conduct_yes_val_no,
          paste0("mri_d2_conduct_yes_values_no_stage1",
                 Sys.Date(),".csv"))

#### Conduct: No

dt_conduct_no <- aux  %>%
  filter(mri_d2_conduct == 0 | p_mri_d2_ind == 0)

#print(dt_conduct_yes_val_yes$enro_animal_id)
print(nrow(dt_conduct_no))

dt_conduct_no_val_no <- aux  %>%
  filter((mri_d2_conduct == 0 | p_mri_d2_ind == 0) &
           is.na(p_mri_d2_fract_csf) &
           is.na(p_mri_d2_fract_csf_left) &
           is.na(p_mri_d2_fract_csf_right) &
           is.na(p_mri_d2_fract_tis) &
           is.na(p_mri_d2_fract_tis_left) &
           is.na(p_mri_d2_fract_tis_right) &
           is.na(p_mri_d2_fract_les) &
           is.na(p_mri_d2_fract_les_left) &
           is.na(p_mri_d2_fract_les_right))

#print(dt_conduct_no_val_no$enro_animal_id)
print(nrow(dt_conduct_no_val_no))

dt_conduct_no_val_yes <- aux  %>%
  filter(mri_d2_conduct == 0 & p_mri_d2_ind == 1 &
           (!is.na(p_mri_d2_fract_csf) |
           !is.na(p_mri_d2_fract_csf_left) |
           !is.na(p_mri_d2_fract_csf_right) |
           !is.na(p_mri_d2_fract_tis) |
           !is.na(p_mri_d2_fract_tis_left) |
           !is.na(p_mri_d2_fract_tis_right) |
           !is.na(p_mri_d2_fract_les) |
           !is.na(p_mri_d2_fract_les_left) |
           !is.na(p_mri_d2_fract_les_right))) |>
  select(enro_animal_id, mri_d2_conduct, p_mri_d2_ind,
         p_mri_d2_fract_csf:p_mri_d2_fract_les_right)

#print(dt_conduct_no_val_yes$enro_animal_id)
print(nrow(dt_conduct_no_val_yes))

if (nrow(dt_conduct_no_val_yes) > 0)
write.csv(dt_conduct_no_val_yes,
          paste0("mri_d2_conduct_no_values_yes_stage1",
                 Sys.Date(),".csv"))

# Create the table
tab <- matrix(c(nrow(dt_conduct_yes_val_yes), nrow(dt_conduct_yes_val_no),
                nrow(dt_conduct_no_val_yes), nrow(dt_conduct_no_val_no)), nrow=2, ncol=2)
rownames(tab) <- c("Value Yes", "Value No")
colnames(tab) <- c("Conduct Yes", "Conduct No")

# Create table
kable(tab)

