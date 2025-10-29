rm(list = ls())

# Load --------------------------------------------------------------------

source("00_utils.R")
load("data/span2_clotstudy_p3_feasibility_data.Rdata")

# Output -----------------------------------------------------------------

tables <- list()
plots <- list()
cox_results <- list()
pval_prop_hazard <- list()

# Data Processing ----

dt <- dt_feasibility$mitt |>
  mutate(status = 
           ifelse(eos_day_diff_srg_death < 30 & 
                    animal_death_before_conduct_d30 == "Yes", 1, 0),
         time = 
           ifelse(eos_day_diff_srg_death < 30, eos_day_diff_srg_death, 30)) |>
  select(time, status, site, enro_sex, enro_model,
         txas_reperfusion, clot_length,
         animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30) |>
  droplevels()

dt_cl3 <- dt |>
  filter(clot_length == "3")

dt_cl4 <- dt |>
  filter(clot_length == "4")

# Tables ----

tables$descriptive$overall <- dt |>
  select(animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30) |>
  nt_describe(labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd,
                                 helper_median_iqr,
                                 helper_median_range,
                                 helper_missing2))

tables$descriptive$sex <- dt |>
  select(animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30,
         enro_sex) |>
  nt_describe(group = enro_sex,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd,
                                 helper_median_iqr,
                                 helper_median_range,
                                 helper_missing2))

tables$descriptive$site <- dt |>
  select(animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30,
         site) |>
  nt_describe(group = site,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd,
                                 helper_median_iqr,
                                 helper_median_range,
                                 helper_missing2))

tables$descriptive$txas_reperfusion <- dt |>
  select(animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30,
         txas_reperfusion) |>
  nt_describe(group = txas_reperfusion,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd,
                                 helper_median_iqr,
                                 helper_median_range,
                                 helper_missing2))

tables$descriptive$clot_length <- dt |>
  select(animal_death_before_conduct_d3,
         animal_death_before_conduct_d7,
         animal_death_before_conduct_d30,
         clot_length) |>
  nt_describe(group = clot_length,
              labels = data_labels,
              measures_ql = list(helper_perc_count2),
              measures_qt = list(helper_mean_sd,
                                 helper_median_iqr,
                                 helper_median_range,
                                 helper_missing2))

# Plots ----

### Overall ----

fit <- survfit(Surv(time, status) ~ txas_reperfusion, data = dt)

# Perform the log-rank test
surv_object <- Surv(time = dt$time, event = dt$status)
log_rank_test <- survdiff(surv_object ~ txas_reperfusion, data = dt)
pval.log_rank_test = log_rank_test$pvalue

plots$txas_reperfusion$overall$all_days <- 
  ggsurvplot(fit,
             palette = colors[2:1],
             conf.int = FALSE,
             risk.table = TRUE,
             legend.title = "Study Arm",
             xlab = "Time (days)",
             legend.labs = c("Control", "TNKase"),
             pval = paste0("p = ",pvalue(pval.log_rank_test, prefix = c("< ", "", "")))
  ) 

plots$txas_reperfusion$overall$seven_days <- 
  ggsurvplot(fit,
             palette = colors[2:1],
             conf.int = FALSE,
             risk.table = TRUE,
             legend.title = "Study Arm",
             xlab = "Time (days)",
             legend.labs = c("Control", "TNKase"),
             pval = paste0("p = ",pvalue(pval.log_rank_test, prefix = c("< ", "", ""))),
             xlim = c(0, 7),
             break.time.by = 1
  )


### CL3 ----

fit <- survfit(Surv(time, status) ~ txas_reperfusion, data = dt_cl3)

# Perform the log-rank test
surv_object <- Surv(time = dt_cl3$time, event = dt_cl3$status)
log_rank_test <- survdiff(surv_object ~ txas_reperfusion, data = dt_cl3)
pval.log_rank_test = log_rank_test$pvalue

plots$txas_reperfusion$cl3$all_days <- 
  ggsurvplot(fit,
             palette = colors[2:1],
             conf.int = FALSE,
             risk.table = TRUE,
             legend.title = "Study Arm",
             xlab = "Time (days)",
             legend.labs = c("Control", "TNKase"),
             pval = paste0("p = ",pvalue(pval.log_rank_test, prefix = c("< ", "", "")))
  )

plots$txas_reperfusion$cl3$seven_days <- 
  ggsurvplot(fit,
             palette = colors[2:1],
             conf.int = FALSE,
             risk.table = TRUE,
             legend.title = "Study Arm",
             xlab = "Time (days)",
             legend.labs = c("Control", "TNKase"),
             pval = paste0("p = ",pvalue(pval.log_rank_test, prefix = c("< ", "", ""))),
             xlim = c(0, 7),
             break.time.by = 1
  )


### CL4 ----

fit <- survfit(Surv(time, status) ~ txas_reperfusion, data = dt_cl4)

# Perform the log-rank test
surv_object <- Surv(time = dt_cl4$time, event = dt_cl4$status)
log_rank_test <- survdiff(surv_object ~ txas_reperfusion, data = dt_cl4)
pval.log_rank_test = log_rank_test$pvalue

plots$txas_reperfusion$cl4$all_days <- 
  ggsurvplot(fit,
             palette = colors[2:1],
             conf.int = FALSE,
             risk.table = TRUE,
             legend.title = "Study Arm",
             xlab = "Time (days)",
             legend.labs = c("Control", "TNKase"),
             pval = paste0("p = ",pvalue(pval.log_rank_test, prefix = c("< ", "", "")))
  )

plots$txas_reperfusion$cl4$seven_days <- 
  ggsurvplot(fit,
             palette = colors[2:1],
             conf.int = FALSE,
             risk.table = TRUE,
             legend.title = "Study Arm",
             xlab = "Time (days)",
             legend.labs = c("Control", "TNKase"),
             pval = paste0("p = ",pvalue(pval.log_rank_test, prefix = c("< ", "", ""))),
             xlim = c(0, 7),
             break.time.by = 1
  )


# Test of Hypotheses ----


## Interaction ----

fit <- coxph(Surv(time, status) ~ strata(site) + enro_sex + 
               + txas_reperfusion + clot_length +
               txas_reperfusion*clot_length,
             data = dt)
sm <- summary(fit)

tables$cox$interaction <- 
  sm$coefficients["txas_reperfusionTNKase:clot_length4", "Pr(>|z|)"]

cox_results$interaction <- 
  nt_multiple_cox(fit, labels = data_labels,
                  table_reference = FALSE)$effect |>
  mutate(`p value` = `Wald p value`)

aux <- dt |>
  mutate(txas_reperfusion_cl = 
           paste0(txas_reperfusion, " and ", clot_length, "cm")) |>
  mutate(txas_reperfusion_cl = 
           factor(txas_reperfusion_cl, levels = 
                  c("Control and 3cm", 
                    "TNKase and 3cm", 
                    "Control and 4cm", 
                    "TNKase and 4cm")
                  )
         )

fit <- survfit(Surv(time, status) ~ txas_reperfusion_cl, data = aux)

gp <- 
  ggsurvplot(fit,
             #palette = rep(colors[2:1], 2),
             linetype = rep(1:2, each = 2),
             conf.int = FALSE,
             risk.table = TRUE,
             legend.title = "Study Arm and Clot Length",
             xlab = "Time (days)",
             legend.labs = c("Control and 3cm", 
                             "TNKase and 3cm", 
                             "Control and 4cm", 
                             "TNKase and 4cm"),
             #pval = paste0("p = ",pvalue(pval.log_rank_test, prefix = c("< ", "", "")))
  )
tmp <- format_pvalue(tables$cox$interaction)

gp$plot <- gp$plot +
  guides(color = guide_legend(order = 1,nrow = 2, byrow = TRUE),
         linetype = guide_legend(order = 1, nrow = 2, byrow = TRUE)) +
  scale_color_manual(values = rep(colors[2:1], 2)) +
  labs(caption = glue("p-value for interaction = {tmp}")) 
gp$table$theme$axis.text.y <- NULL
gp$table <- gp$table +
  labs(y = "") +
  theme(axis.text.y = element_text(color = "black", size = 12))

plots$cox$interaction <- 
  gp$plot + gp$table + plot_layout(nrow = 2, heights = c(0.8, 0.2))

## Additive ----

### Overall ----
fit <- coxph(Surv(time, status) ~ strata(site) + enro_sex + 
                + clot_length + txas_reperfusion,
             data = dt)

cox_results$overall <- 
  nt_multiple_cox(fit, labels = data_labels,
                  table_reference = FALSE)$effect |>
  mutate(`p value` = `Wald p value`)
  
  
tables$cox$overall <- cox_results$overall |>
  slice(2:3) |>
  select(Variable, 
         Ratio = HR,
         `HR (95% CI)` = `Estimate (95% CI)`,
         `p value` = `Wald p value`)

### Proportional hazards ----
zph_results <- cox.zph(fit)
print(zph_results)
pval_prop_hazard$overall <- round(zph_results$table["txas_reperfusion", "p"],3)

## CL3 ----
fit <- coxph(Surv(time, status) ~ strata(site) + enro_sex + 
               + txas_reperfusion,
             data = dt_cl3)

cox_results$cl3 <- 
  nt_multiple_cox(fit, labels = data_labels,
                  table_reference = FALSE)$effect |>
  mutate(`p value` = `Wald p value`)


tables$cox$cl3 <- cox_results$cl3 |>
  slice(2) |>
  select(Ratio = HR,
         `HR (95% CI)` = `Estimate (95% CI)`,
         `p value` = `Wald p value`)

### Proportional hazards ----
zph_results <- cox.zph(fit)
print(zph_results)
pval_prop_hazard$cl3 <- round(zph_results$table["txas_reperfusion", "p"],3)

## CL4 ----

### Overall ----
fit <- coxph(Surv(time, status) ~ strata(site) + enro_sex + 
               + txas_reperfusion,
             data = dt_cl4)

cox_results$cl4 <- 
  nt_multiple_cox(fit, labels = data_labels,
                  table_reference = FALSE)$effect

tables$cox$cl4 <- cox_results$cl4 |>
  slice(2) |>
  select(Ratio = HR,
         `HR (95% CI)` = `Estimate (95% CI)`,
         `p value` = `Wald p value`)

### Proportional hazards ----
zph_results <- cox.zph(fit)
print(zph_results)
pval_prop_hazard$cl4 <- round(zph_results$table["txas_reperfusion", "p"],3)


# Saving output ----

rm(list=setdiff(ls(), c("cox_results",
                        "tables",
                        "plots",
                        "pval_prop_hazard")))
save.image("results/span2_clot_study_p3_mortality_analysis_mitt.Rdata")

