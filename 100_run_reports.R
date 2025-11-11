# Run reports to send ----

subDir <- "results"
if (!dir.exists(file.path(subDir))) {
  dir.create(file.path(subDir))
}

subDir <- "reports"
if (!dir.exists(file.path(subDir))) {
  dir.create(file.path(subDir))
}


## Feasibility ----

### Data ----

source("01_feasibility_dataset.R")

#### Checking sample size ----

fname <- paste0("report_sample_size ", Sys.Date(), ".html")
quarto_render(input = "01_report_sample_size.qmd", output_file = fname)
# move file into reports/
file.rename(fname, file.path("reports", fname))


#### Descriptive Analysis ----

source("02_feasibility_analysis.R")

fname <- paste0("report_feasibility ", Sys.Date(), ".html")
quarto_render(input = "02_report_feasibility.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

#### Compliance ----

source("03_compliance_analysis.R")

fname <- paste0("report_compliance ",Sys.Date(), ".html")
quarto_render(input = "03_report_compliance.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

### SPAN Full Score ----

##### Data ----

source("04_neurodeficit_battery_dataset.R")

##### mITT analysis ----

source("05_full_span_neuroscore_analysis_mitt.R")

fname <- paste0("report_full_span_score_mitt ", Sys.Date(), ".html")
quarto_render(input = "05_report_full_span_score_mitt.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

##### PP analysis ----

source("05_full_span_neuroscore_analysis_pp.R")

fname <- paste0("report_full_span_score_pp ",Sys.Date(), ".html")
quarto_render(input = "05_report_full_span_score_pp.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

### Alternative Corner Test ----

##### Data ----

source("06_corner_test_dataset.R")

##### mITT analysis ----

source("07_d30_corner_test_analysis_mitt.R")

fname <- paste0("report_d30_corner_test_mitt ", Sys.Date(), ".html")
quarto_render(input = "07_report_d30_corner_test_mitt.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

source("08_d7_corner_test_analysis_mitt.R")

fname <- paste0("report_d7_corner_test_mitt ",Sys.Date(), ".html")
quarto_render(input = "08_report_d7_corner_test_mitt.qmd",output_file = fname)
file.rename(fname, file.path("reports", fname))

##### PP analysis ----

source("07_d30_corner_test_analysis_pp.R")

fname <- paste0("report_d30_corner_test_pp ", Sys.Date(), ".html")
quarto_render(input = "07_report_d30_corner_test_pp.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

source("08_d7_corner_test_analysis_pp.R")

fname <- paste0("report_d7_corner_test_pp ", Sys.Date(), ".html")
quarto_render(input = "08_report_d7_corner_test_pp.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

### Day 2 MRI ----

##### Data ----

source("09_mri_d2_dataset.R")

#### Volume Fraction CSF - Left Hemisphere ----

# ###### mITT analysis ----
# 
# source("09_d2_volume_csf_left_analysis_mitt.R")
# quarto_render(input = "report_d2_volume_csf_left_analysis_mitt.qmd",
#               output_file = paste0("reports/report_d2_volume_csf_left_analysis_mitt ",
#                                    Sys.Date(), ".html"))
# ###### PP analysis ----
# 
# source("09_d2_volume_csf_left_analysis_ft.R")
# quarto_render(input = "report_d2_volume_csf_left_analysis_pp.qmd",
#               output_file = paste0("reports/report_d2_volume_csf_left_analysis_pp ",
#                                    Sys.Date(), ".html"))

#### Volume Fraction CSF - Right Hemisphere ----

# ###### mITT analysis ----
# 
# source("10_d2_volume_csf_right_analysis_mitt.R")
# quarto_render(input = "report_d2_volume_csf_right_analysis_mitt.qmd",
#               output_file = paste0("reports/report_d2_volume_csf_right_analysis_mitt ",
#                                    Sys.Date(), ".html"))
# ###### PP analysis ----
# 
# source("10_d2_volume_csf_right_analysis_ft.R")
# quarto_render(input = "report_d2_volume_csf_right_analysis_pp.qmd",
#               output_file = paste0("reports/report_d2_volume_csf_right_analysis_pp ",
#                                    Sys.Date(), ".html"))

#### Volume Fraction Tissue - Left Hemisphere ----

# ###### mITT analysis ----
# 
# source("11_d2_volume_tissue_left_analysis_mitt.R")
# quarto_render(input = "report_d2_volume_tissue_left_analysis_mitt.qmd",
#               output_file = paste0("reports/report_d2_volume_tissue_left_analysis_mitt ",
#                                    Sys.Date(), ".html"))
# ###### PP analysis ----
# 
# source("11_d2_volume_tissue_left_analysis_ft.R")
# quarto_render(input = "report_d2_volume_tissue_left_analysis_pp.qmd",
#               output_file = paste0("reports/report_d2_volume_tissue_left_analysis_pp ",
#                                    Sys.Date(), ".html"))

#### Volume Fraction Tissue - Right Hemisphere ----

# ###### mITT analysis ----
# 
# source("12_d2_volume_tissue_right_analysis_mitt.R")
# quarto_render(input = "report_d2_volume_tissue_right_analysis_mitt.qmd",
#               output_file = paste0("reports/report_d2_volume_tissue_right_analysis_mitt ",
#                                    Sys.Date(), ".html"))
# ###### PP analysis ----
# 
# source("12_d2_volume_tissue_right_analysis_ft.R")
# quarto_render(input = "report_d2_volume_tissue_right_analysis_pp.qmd",
#               output_file = paste0("reports/report_d2_volume_tissue_right_analysis_pp ",
#                                    Sys.Date(), ".html"))

#### Volume Fraction Lesion - Left Hemisphere ----

# ###### mITT analysis ----
# 
# source("13_d2_volume_lesion_left_analysis_mitt.R")
# quarto_render(input = "report_d2_volume_lesion_left_analysis_mitt.qmd",
#               output_file = paste0("reports/report_d2_volume_lesion_left_analysis_mitt ",
#                                    Sys.Date(), ".html"))
# ###### PP analysis ----
# 
# source("13_d2_volume_lesion_left_analysis_pp.R")
# quarto_render(input = "report_d2_volume_lesion_left_analysis_ft.qmd",
#               output_file = paste0("reports/report_d2_volume_lesion_left_analysis_pp ",
#                                    Sys.Date(), ".html"))

#### Volume Fraction Lesion - Right Hemisphere ----

###### mITT analysis ----

source("15_d2_volume_lesion_right_analysis_mitt.R")

fname <- paste0("report_d2_volume_lesion_right_analysis_mitt ", Sys.Date(), ".html")
quarto_render(input = "15_report_d2_volume_lesion_right_analysis_mitt.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

###### PP analysis ----

source("15_d2_volume_lesion_right_analysis_pp.R")

fname <- paste0("report_d2_volume_lesion_right_analysis_pp ", Sys.Date(), ".html")
quarto_render(input = "15_report_d2_volume_lesion_right_analysis_pp.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

#### Midline Shift Index ----

###### mITT analysis ----

source("16_d2_mid_shift_indx_analysis_mitt.R")

fname <- paste0("report_d2_mid_shift_indx_analysis_mitt ", Sys.Date(), ".html")
quarto_render(input = "16_report_d2_mid_shift_indx_analysis_mitt.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

###### PP analysis ----

source("16_d2_mid_shift_indx_analysis_pp.R")

fname <- paste0("report_d2_mid_shift_indx_analysis_pp ", Sys.Date(), ".html")
quarto_render(input = "16_report_d2_mid_shift_indx_analysis_pp.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

### Simple SPAN Score ----

# ##### Data ----
# 
# source("04_neurodeficit_battery_dataset.R")
# 
# ##### mITT analysis ----
# 
# source("16_simple_span_neuroscore_analysis_mitt.R")
# quarto_render(input = "report_simple_span_score_mitt.qmd",
#               output_file = paste0("reports/report_simple_span_score_mitt ",
#                                    Sys.Date(), ".html"))
# ##### FT analysis ----
# 
# source("16_simple_span_neuroscore_analysis_ft.R")
# quarto_render(input = "report_simple_span_score_ft.qmd",
#               output_file = paste0("reports/report_simple_span_score_ft ",
#                                    Sys.Date(), ".html"))

### MGH Score ----

# ##### Data ----
# 
# source("04_neurodeficit_battery_dataset.R")
# 
# ##### mITT analysis ----
# 
# source("17_mgh_neuroscore_analysis_mitt.R")
# quarto_render(input = "report_mgh_score_mitt.qmd",
#               output_file = paste0("reports/report_mgh_score_mitt ",
#                                    Sys.Date(), ".html"))
# ##### FT analysis ----
# 
# source("17_mgh_neuroscore_analysis_ft.R")
# quarto_render(input = "report_mgh_score_ft.qmd",
#               output_file = paste0("reports/report_mgh_score_ft ",
#                                    Sys.Date(), ".html"))


### Duke Score ----

# ##### Data ----
# 
# source("04_neurodeficit_battery_dataset.R")
# 
# ##### mITT analysis ----
# 
# source("18_duke_neuroscore_analysis_mitt.R")
# quarto_render(input = "report_duke_score_mitt.qmd",
#               output_file = paste0("reports/report_duke_score_mitt ",
#                                    Sys.Date(), ".html"))
# ##### FT analysis ----
# 
# source("18_duke_neuroscore_analysis_ft.R")
# quarto_render(input = "report_duke_score_ft.qmd",
#               output_file = paste0("reports/report_duke_score_ft ",
#                                    Sys.Date(), ".html"))
              
### Mortality ----

#### mITT analysis ----

source("19_mortality_analysis_mitt.R")

fname <- paste0("report_mortality_analysis_mitt ", Sys.Date(), ".html")
quarto_render(input = "19_report_mortality_analysis_mitt.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

#### FT analysis -----

source("19_mortality_analysis_pp.R")

fname <- paste0("report_mortality_analysis_ft ", Sys.Date(), ".html")
quarto_render(input = "19_report_mortality_analysis_pp.qmd", output_file = fname)  
file.rename(fname, file.path("reports", fname))

### Bederson Neurodeficit score at day 1 ----

#### mITT analysis ----

source("20_bederson_nds_d1_analysis_mitt.R")

fname <- paste0("report_d1_bederson_nds_analysis_mitt ", Sys.Date(), ".html")
quarto_render(input = "20_report_d1_bederson_nds_analysis_mitt.qmd", output_file = fname)   
file.rename(fname, file.path("reports", fname))

#### FT analysis ----

source("20_bederson_nds_d1_analysis_pp.R")

fname <- paste0("report_d1_bederson_nds_analysis_pp ",Sys.Date(), ".html")
quarto_render(input = "20_report_d1_bederson_nds_analysis_pp.qmd", output_file = fname)  
file.rename(fname, file.path("reports", fname))

### Bederson Neurodeficit score at day 2 ----

#### mITT analysis ----

source("21_bederson_nds_d2_analysis_mitt.R")

fname <- paste0("report_d2_bederson_nds_analysis_mitt ", Sys.Date(), ".html")
quarto_render(input = "21_report_d2_bederson_nds_analysis_mitt.qmd", output_file = fname)  
file.rename(fname, file.path("reports", fname))

#### FT analysis ----

source("21_bederson_nds_d2_analysis_pp.R")

fname <- paste0("report_d2_bederson_nds_analysis_pp ", Sys.Date(), ".html")
quarto_render(input = "21_report_d2_bederson_nds_analysis_pp.qmd", output_file = fname)  
file.rename(fname, file.path("reports", fname))

### Bederson Neurodeficit score at day 30 ----

#### mITT analysis ----

source("22_bederson_nds_d30_analysis_mitt.R")

fname <- paste0("report_d30_bederson_nds_analysis_mitt ",Sys.Date(), ".html")
quarto_render(input = "22_report_d30_bederson_nds_analysis_mitt.qmd",output_file = fname) 
file.rename(fname, file.path("reports", fname))

#### FT analysis

source("22_bederson_nds_d30_analysis_pp.R")

fname <- paste0("report_d30_bederson_nds_analysis_pp ",Sys.Date(), ".html")
quarto_render(input = "22_report_d30_bederson_nds_analysis_pp.qmd", output_file = fname) 
file.rename(fname, file.path("reports", fname))

### Weight ----

#### mITT analysis ----

source("23_weight_mitt.R")
fname <- paste0("report_weight_analysis_mitt ",Sys.Date(), ".html")
quarto_render(input = "23_report_weight_analysis_mitt.qmd",output_file = fname)
file.rename(fname, file.path("reports", fname))

#### PP analysis

source("23_weight_pp.R") 
fname <- paste0("report_weight_analysis_pp ", Sys.Date(), ".html")
quarto_render(input = "23_report_weight_analysis_pp.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

### Simple SPAN Score ----

##### mITT analysis ----

source("24_simple_span_neuroscore_analysis_mitt.R")

fname <- paste0("report_simple_span_score_mitt ", Sys.Date(), ".html")
quarto_render(input = "24_report_simple_span_score_mitt.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

##### PP analysis ----

source("24_simple_span_neuroscore_analysis_pp.R")

fname <- paste0("report_simple_span_score_pp ",Sys.Date(), ".html")
quarto_render(input = "24_report_simple_span_score_pp.qmd", output_file = fname)
file.rename(fname, file.path("reports", fname))

# ### Clot Draw ----
# 
# #### mITT analysis ----
# 
# source("24_clot_draw_analysis_mitt.R")
# quarto_render(input = "report_clot_draw_analysis_mitt.qmd",
#               output_file = paste0("reports/report_clot_draw_analysis_mitt ",
#                                    Sys.Date(), ".html"))  
# 
# source("24_clot_draw_analysis_ft.R")
# quarto_render(input = "report_clot_draw_analysis_ft.qmd",
#               output_file = paste0("reports/report_clot_draw_analysis_ft ",
#                                    Sys.Date(), ".html"))  
# 
# ### Combined plots ----
# quarto_render(input = "report_combined_plots.qmd",
#               output_file = paste0("reports/report_combined_plots_mitt ",
#                                    Sys.Date(), ".html"))  

