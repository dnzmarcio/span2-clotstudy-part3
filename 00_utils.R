
#in case the libraries are not install already-
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(janitor, gt, ntimes, ggplot2, RColorBrewer, pim, ggsignif, 
# glue, scales, tidyverse, , stringr, quarto)

# test branch check

library(janitor)
library(gt)
library(ntimes) #devtools::install_github("dnzmarcio/ntimes")
library(ggplot2)
library(RColorBrewer)
library(pim) # devtools::install_github("dnzmarcio/pim")
library(ggsignif)
library(glue)
library(scales)
library(tidyverse)
library(stringr)
library(quarto)
library(survival)
library(survminer)
library(patchwork)
library(lme4)
library(lmerTest)
library(marginaleffects)
library(mice)
library(hms)
library(cowplot)
library(ggbeeswarm)
library(ggtext)
library(ggprism)

# Data ----

mri <- "data/SPAN2EmbolicClotStud-MRIDay3DataStatsRepo_DATA_2025-12-15_1159.csv"
feasibility <- "data/SPAN2EmbolicClotStud-ClotModelStageFeasib_DATA_2025-10-08_1704.csv"
behavior <- "data/SPAN2EmbolicClotStud-ClotModelStageBehavi_DATA_2025-10-08_1704.csv"

# Treatment routes ----

tnk_treatments <- c("Control", "TNKase")

# Bederson nds score argument (show percentages = "yes" else "no")

show_perc_val = "yes"

# Multiple imputation ----
m <- 25
maxit <- 50
mi_early_variables <- c('srg_weight', 'srg_animal_age') #
mi_late_variables <- c('srg_weight', 'srg_animal_age',
                       'p_mri_d2_fract_les')

options(scipen = 999)

# Color Palette ----
colors <- palette.colors(palette = "Set1") 

# Helpful Functions ----


format_pvalue <- label_pvalue(prefix = c("<", "", ""))

# Descriptive Measures
helper_perc_count2 <- function(var, digits, ...){
  
  ldots <- list(...)
  h <- var
  lh <- levels(h)
  
  count <- tapply(h, h, length)
  count <- ifelse(is.na(count), 0, count)
  n <- length(h)
  perc <- 100*prop.table(count)
  perc <- ifelse(!is.finite(perc), NA, 
                 format(round(perc, digits), nsmall = digits))
  
  perc_count <- paste0(count, " (", perc, ")")
  
  lh <- c(lh, "Effective Sample Size")
  perc_count <- c(perc_count, sum(!is.na(h)))
  
  lh <- paste0("\t ", lh)
  
  out <- list(name = lh, value = perc_count)
}

helper_missing2 <- function(var, ...){
  
  ldots <- list(...)
  
  out <- list(name = "\t Effective Sample Size",
              value = sum(!is.na(var)))
  
  return(out)
}

plot_weight <- 
  function (var, time, group, var.label, time.label, group.label,
            ...) {
    
    group <- droplevels(group)
    
    ldots <- list(...)
    
    if(length(levels(group)) == 2){ #3
      index <- c(2, 1) #c(9, 6, 8)
    } else if(length(levels(group)) == 5){
      index <- c(1, 2, 3, 4, 7)
    } else {
      index <- c(1, 2, 3, 4, 7, 9, 6) #c(1, 2, 3, 4, 7, 9, 6, 8)
    }
    tmp <- as.character(ldots$colors[index])
    
    if (!is.null(ldots$labels)){
      
      dp <- na.exclude(data.frame(time, group, var, facet = ldots[[1]])) |>
        group_by(group, time, facet) |>
        summarize(mean = mean(var, na.rm = TRUE), 
                  se = sd(var, na.rm = TRUE)/sqrt(n()),
                  n = sum(!is.na(var)))
      
      plot <- ggplot(dp, aes(x = .data$time, color = .data$group, 
                             y = .data$mean)) + 
        geom_point(position = position_dodge(0.05)) + 
        geom_line(aes(group = group), position = position_dodge(0.05)) + 
        geom_errorbar(aes(ymin = .data$mean - .data$se, ymax = .data$mean + 
                            .data$se), width = 0.2, position = position_dodge(0.05)) +
        facet_grid(. ~ facet)  + 
        labs(y = var.label, x = time.label) + 
        theme_classic(base_size = 18) + 
        theme(legend.position = "top") + 
        scale_color_manual(group.label, palette = "Set1")  +
        scale_x_continuous(breaks = 0:8)
      
      x.ticks <- ggplot_build(plot)$layout$panel_params[[1]]$x$breaks
      colors <- unique(ggplot_build(plot)$data[[1]]["colour"])
      
      ## Basic plot
      n.table <- ggplot(dp, aes(x = time, y = group)) +
        geom_text(aes_string(label = "n"), size = 3.5)  +
        facet_grid(~ facet, labeller = as_labeller(labels)) + 
        theme_classic() +
        labs(x = "Time (days)", y = "", title = "Sample Size") +
        scale_y_discrete(labels = rep("-", nlevels(dp$group)))  +
        scale_x_continuous(breaks = 0:8) +
        theme(title = element_text(size = 9),
              axis.text.y = element_text(colour = colors[[1]],
                                         face = "bold",
                                         size = 48,
                                         vjust = 0.3),
              axis.ticks.y = element_blank(),
              strip.background = element_blank(),
              strip.text = element_blank())
      
    } else {
      
      dp <- na.exclude(data.frame(time, group, var)) |>
        group_by(group, time) |>
        summarize(mean = mean(var, na.rm = TRUE), 
                  se = sd(var, na.rm = TRUE)/sqrt(n()),
                  n = sum(!is.na(var)))
      
      plot <- ggplot(dp, aes(x = .data$time, color = .data$group, 
                             y = .data$mean)) + 
        geom_point(position = position_dodge(0.05)) + 
        geom_line(aes(group = group), position = position_dodge(0.05)) + 
        geom_errorbar(aes(ymin = .data$mean - .data$se, ymax = .data$mean + 
                            .data$se), width = 0.2, 
                      position = position_dodge(0.05))  + 
        labs(y = var.label, x = time.label) + 
        theme_classic(base_size = 18) + 
        theme(legend.position = "top") + 
        scale_color_manual(group.label, values = tmp)  +
        scale_x_continuous(breaks = 0:8)
      
      x.ticks <- ggplot_build(plot)$layout$panel_params[[1]]$x$breaks
      colors <- unique(ggplot_build(plot)$data[[1]]["colour"])
      
      
      ## Basic plot
      n.table <- ggplot(dp, aes(x = time, y = group)) +
        geom_text(aes_string(label = "n"), size = 3.5)  +
        labs(x = "Time (days)", y = "", title = "Sample Size") +
        theme_classic() +
        scale_y_discrete(labels = rep("-", nlevels(dp$group)))  +
        scale_x_continuous(breaks = 0:8) +
        theme(title = element_text(size = 9),
              axis.text.y = element_text(colour = colors[[1]],
                                         face = "bold",
                                         size = 48,
                                         vjust = 0.3),
              axis.ticks.y = element_blank(),
              strip.background = element_blank(),
              strip.text = element_blank())
    }
    
    out <- list(plot = plot, n.table = n.table)
    
    return(out)
  }

hush <- function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}

# Summarizing model regression results
nt_multiple_pim <- function(fit, mi = FALSE){
  
  if (!mi){
    aux <- fit@formula@predictors
    
    beta <- as.numeric(fit@coef)
    se <- sqrt(diag(fit@vcov))
    pvalue <- summary(fit)@pr

    prob_index <- plogis(fit@coef)
    q <- qnorm(1 - (0.05/2), 0, 1)
    lower <- plogis(beta - q*se) 
    upper <- plogis(beta + q*se)
    
  } else {
    tmp <- fit$analyses[[1]]
    df <- tmp@penv@nobs - nrow(tidy(tmp))
    aux <- summary(pool(fit, dfcom = df))
    
    variable <- aux$term
    beta <- aux$estimate
    se <- aux$std.error 
    pvalue <- aux$p.value 
    
    prob_index <- plogis(beta)
    names(prob_index) <- variable
    q <- qnorm(1 - (0.05/2), 0, 1)
    lower <- plogis(beta - q*se) 
    upper <- plogis(beta + q*se)
  }
  
  labels <- as.list(paste0(aux,":"))
  labels <- setNames(labels, aux)
  
  out <- data.frame(prob_index,
                    lower, upper,
                    coef = beta, 
                    se =  se,
                    zscore = beta/se,
                    pvalue = round(pvalue, 3)) |>   
    rownames_to_column(var = "variable") 

  return(out)
}

# Format output of descriptive tables

nt_format <- function(tab){
  
  if (any(str_detect(colnames(tab), "All")) & ncol(tab) == 2){
    
    gt(tab) |>
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = c(Variable),
          rows = !str_detect(Variable, "\t"))
      ) 
    
  } else {
    
    title <- str_extract(colnames(tab), "[^:]*: ") |>
      na.exclude() |>
      str_remove(": ") |>
      unique()
    
    colnames(tab) <- str_remove(colnames(tab), "[^:]*: ")
    
    gt(tab) |>
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_body(
          columns = c(Variable),
          rows = !str_detect(Variable, "\t"))
      ) |>
      tab_spanner(
        label = title,
        columns = 2:ncol(tab)
      )
  }
  
}

# Table of test statistics

table_db <-function(statistic, route){
  if (route == "ip"){
    out <- statistic |> as_tibble() |>
      mutate(across(contains("stage"), .fns = ~ -.x)) |>
      mutate('Treatment' = c("L", "M"), .before = "stage1")
  }
  if (route == "iv"){
    out <- statistic |> as_tibble()|>
      mutate(across(contains("stage"), .fns = ~ -.x)) |>
      mutate('Treatment' = c("H", "I", "J", "K"), .before = "stage1")
  }
  
  out <- out |> rename_with(.fn = ~ str_replace(str_to_title(.x), "ge", "ge "))
  
}

# Plots

plot_db <- function(statistic, route){
  
  limits <- list()
  limits$iv$upper <- c(3.303, 2.803, 2.670, 2.643) #c(2.970, 2.520, 2.401, 2.376)
  limits$iv$lower <- c(-0.661, 0.934, 1.907, 2.643) #c(-0.594, 0.840, 1.714, 2.376)
  limits$ip$upper <- c(3.028, 2.569, 2.448, 2.422) #c(2.67, 2.27, 2.16, 2.14)
  limits$ip$lower <- c(-0.605, 0.856, 1.748, 2.422) #c(-0.535, 0.757, 1.546, 2.14)
  
  statistic <- unlist(statistic)
  if(route == "ip"){
    index <- c(6, 8)
  } else if(route == "iv"){
    index <- c(2, 3, 4, 7)
  } 
  tmp <- as.character(colors[index])
  
  if (route == "ip"){
    group <- c("L", "M")
    stage = rep(c(1, 2, 3, 4), each = length(group))
    group <- rep(group, 4)
    statistic <- c(-statistic, rep(NA, (8 - length(statistic))))
    dp_statistic_ip <- data.frame(group, stage, statistic)
    
    dp_limits_ip <- data.frame(lower = limits$ip$lower, 
                               upper = limits$ip$upper,
                               stage = 1:4) %>%
      pivot_longer(cols = -stage, names_to = "type", values_to = "limits") %>%
      mutate(label = round(limits, 2), design = "triangular")
    
    out <- ggplot(dp_limits_ip, aes(x = stage, y = limits)) +
      geom_point() + 
      geom_line(aes(group = type), linetype = 2) + 
      theme_prism() +
      scale_x_continuous("Stage", breaks = 1:4) +
      labs(y = "Test statistic") +
      geom_text(aes(label = label), nudge_x = 0.1, size = 5) +
      geom_jitter(data = dp_statistic_ip, 
                  aes(x = stage, y = statistic, color = group), size = 5,
                  width = 0.05, height = 0) +
      scale_fill_manual("Study Arm", values = tmp) +
      theme(text = element_text(size=16), legend.position = "top")
    
  }
  
  if (route == "iv"){
    group <- c("H", "I", "J", "K")
    stage = rep(c(1, 2, 3, 4), each = length(group))
    group <- rep(group, 4)
    statistic <- c(-statistic, rep(NA, (16 - length(statistic))))
    dp_statistic_iv <- data.frame(group, stage, statistic)
    
    dp_limits_iv <- data.frame(lower = limits$iv$lower, 
                               upper = limits$iv$upper,
                               stage = 1:4) %>%
      pivot_longer(cols = -stage, names_to = "type", values_to = "limits") %>%
      mutate(label = round(limits, 2), design = "triangular")
    
    out <- ggplot(dp_limits_iv, aes(x = stage, y = limits)) +
      geom_point() + 
      geom_line(aes(group = type), linetype = 2) + 
      theme_prism() +
      scale_x_continuous("Stage", breaks = 1:4) +
      labs(y = "Test statistic") +
      geom_text(aes(label = label), nudge_x = 0.1, size = 5) +
      geom_jitter(data = dp_statistic_iv, 
                  aes(x = stage, y = statistic, color = group), 
                  size = 5,
                  width = 0.05, height = 0) +
      scale_fill_manual("Study Arm", values = tmp) +
      theme(text = element_text(size=16), legend.position = "top")
  }
  
  return(out)
}


plot_treat <- function(data, y, ylabel,
                       lower = NULL, upper = NULL){
  endpoint <- data |> pull({{y}})
  
  if(length(levels(data$txas_reperfusion)) == 2){ #3
    index <- c(2, 1) #c(9, 6, 8)
  } else if(length(levels(data$txas_reperfusion)) == 5){
    index <- c(1, 2, 3, 4, 7)
  } else {
    index <- c(1, 2, 3, 4, 7, 9, 6) #c(1, 2, 3, 4, 7, 9, 6, 8)
  }

  tmp <- as.character(colors[index])
  
  if(is.null(lower) & is.null(upper)){
    lower <- min(endpoint, na.rm = TRUE) - 0.1*abs(min(endpoint, na.rm = TRUE))
    upper <- max(endpoint, na.rm = TRUE) + 0.1*abs(max(endpoint, na.rm = TRUE))
  } else{
    lower = lower
    upper = upper
  }
  ggplot(data, aes(x = txas_reperfusion, y = {{y}},
                   fill = txas_reperfusion)) +
    geom_beeswarm(aes(fill = txas_reperfusion), shape = 21, size = 2, cex = 2) +
    geom_boxplot(width=0.2, alpha=0.2,
                 position = position_dodge(width = 1),
                 outlier.shape = NA) +
    #geom_violin(position = position_dodge(width = 1)) +
    labs(x = "Treatment", y = ylabel) +
    theme_bw(base_size = 16) +
    scale_fill_manual("Treatment", values = tmp) +
    theme(legend.position = "none",
          axis.title.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 11)) +
    scale_y_continuous(limits = c(lower,upper))
}

plot_treat_bar <- function(data, y, ylabel){
  
  dt_sum <- data %>%
    group_by(txas_reperfusion, {{y}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(txas_reperfusion) %>%
    mutate(perc_value = (count / sum(count)) * 100)
  
  # Ensure all combinations of txas_reperfusion and postop_d1_nds_score exist
  dt_sum_complete <- dt_sum |>
    ungroup() |>
    mutate({{y}} := factor({{y}}, levels = 0:4)) |>
    complete(txas_reperfusion, {{y}},
             fill = list(perc_value = 0),
             explicit = TRUE) |>
    mutate(perc_value = ifelse(perc_value == 0, 0.1, perc_value))
  
  index <- c(1, 2, 3, 4, 7)
  tmp <- as.character(colors[index])
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    theme_prism(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100))
  
}

plot_treat_stackedbar <- function(data, y, ylabel, size_perc = 4){
  
  dt_sum <- data %>%
    group_by(txas_reperfusion, {{y}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(txas_reperfusion) %>%
    mutate(perc_value = round_percentages((count / sum(count)) * 100))
  
  # Ensure all combinations of txas_reperfusion and postop_d1_nds_score exist
  dt_sum_complete <- dt_sum |>
    ungroup() |>
    mutate({{y}} := factor({{y}}, levels = c(4, 3, 2, 1, 0))) |>
    complete(txas_reperfusion, {{y}},
             fill = list(perc_value = 0),
             explicit = TRUE) 
  
  index <- c(1, 2, 3, 4, 7)
  #tmp <- as.character(colors[index])
  
  tmp <- c(c(
    "4" = "#D55E00", 
    "3" = "#009E73",
    "2" = "#56B4E9",
    "1" = "#E69F00",
    "0" = "#CACACA"   
  ))
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = "stack", width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    geom_text(data = dt_sum_complete %>% filter(perc_value != 0),
              aes(label=paste0(sprintf("%1.1f", perc_value),"%")),
              position=position_stack(vjust=0.5),
              size = size_perc
    ) +
    theme_bw(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp, 
                      guide = guide_legend(reverse = TRUE)
    ) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100.2))
  
}

plot_sex <- function(data, y, ylabel,
                     lower = NULL, upper = NULL,
                     bar_plot = NULL){
  
  endpoint <- data |> pull({{y}})
  
  if(length(levels(data$txas_reperfusion)) == 2){ #3
    index <- c(2, 1) #c(9, 6, 8)
  } else if(length(levels(data$txas_reperfusion)) == 5){
    index <- c(1, 2, 3, 4, 7)
  } else {
    index <- c(1, 2, 3, 4, 7, 9, 6) #c(1, 2, 3, 4, 7, 9, 6, 8)
  }
  
  if(is.null(lower) & is.null(upper)){
    lower <- min(endpoint, na.rm = TRUE) - 0.1*abs(min(endpoint, na.rm = TRUE))
    upper <- max(endpoint, na.rm = TRUE) + 0.1*abs(max(endpoint, na.rm = TRUE))
  } else{
    lower = lower
    upper = upper
  }
  
  
  tmp <- as.character(colors[index])
  
  ggplot(data, aes(x = txas_reperfusion, y = {{y}},
                   fill = txas_reperfusion)) +
    geom_beeswarm(aes(fill = txas_reperfusion), shape = 21, size = 2, cex = 2) +
    geom_boxplot(width=0.2, alpha=0.2,
                 position = position_dodge(width = 1),
                 outlier.shape = NA) +
    labs(x = "Treatment", y = ylabel) +
    theme_bw(base_size = 16) +
    scale_fill_manual("Treatment", values = tmp) +
    theme(legend.position = "top",
          axis.title.y = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 11)) +
    scale_y_continuous(limits = c(lower,upper)) +
    facet_wrap(~ enro_sex, ncol = 3, nrow = 2)
}

plot_sex_bar <- function(data, y, ylabel){
  
  dt_sum <- data %>%
    group_by(txas_reperfusion, enro_sex, {{y}}) %>%  # Group by treatment and score
    summarise(count = n(), .groups = "drop") %>%     # Count occurrences
    group_by(txas_reperfusion, enro_sex) %>%                      # Group again by treatment
    mutate(perc_value = (count / sum(count)) * 100)
  
  dt_sum_complete <- dt_sum |>
    ungroup() |>
    mutate({{y}} := factor({{y}}, levels = 0:4)) |>
    group_by(txas_reperfusion, enro_sex) |>
    complete({{y}},
             fill = list(perc_value = 0),
             explicit = TRUE) |>
    mutate(perc_value = ifelse(perc_value == 0, 0.1, perc_value))
  
  index <- c(1, 2, 3, 4, 7)
  tmp <- as.character(colors[index])
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    theme_bw(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100)) +
    facet_wrap(~ enro_sex, ncol = 3, nrow = 2)
  
}

plot_sex_stackedbar <- function(data, y, ylabel, size_perc = 4){
  
  dt_sum <- data %>%
    group_by(txas_reperfusion, enro_sex, {{y}}) %>%  # Group by treatment and score
    summarise(count = n(), .groups = "drop") %>%     # Count occurrences
    group_by(txas_reperfusion, enro_sex) %>%                      # Group again by treatment
    mutate(perc_value = round_percentages((count / sum(count)) * 100))
  
  dt_sum_complete <- dt_sum |>
    ungroup() |>
    mutate({{y}} := factor({{y}}, levels = c(4,3,2,1,0))) |>
    group_by(txas_reperfusion, enro_sex) |>
    complete({{y}},
             fill = list(perc_value = 0),
             explicit = TRUE)
  
  index <- c(1, 2, 3, 4, 7)
  #tmp <- as.character(colors[index])
  
  tmp <- c(c(
    "4" = "#D55E00", 
    "3" = "#009E73",
    "2" = "#56B4E9",
    "1" = "#E69F00",
    "0" = "#CACACA"   
  ))
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = "stack", width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    geom_text(data = dt_sum_complete %>% filter(perc_value != 0),
              aes(label=paste0(sprintf("%1.1f", perc_value),"%")),
              position=position_stack(vjust=0.5),
              size = size_perc
    ) +
    theme_bw(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp,
                      guide = guide_legend(reverse = TRUE)
    ) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100.2)) +
    facet_wrap(~ enro_sex, ncol = 3, nrow = 2)
  
}

plot_clotlength <- function(data, y, ylabel,
                            lower = NULL, upper = NULL){
  
  endpoint <- data |> pull({{y}})
  
  if(length(levels(data$txas_reperfusion)) == 2){ 
    index <- c(2, 1)
  }
  
  if(is.null(lower) & is.null(upper)){
    lower <- min(endpoint, na.rm = TRUE) - 0.1*abs(min(endpoint, na.rm = TRUE))
    upper <- max(endpoint, na.rm = TRUE) + 0.1*abs(max(endpoint, na.rm = TRUE))
  } else{
    lower = lower
    upper = upper
  }
  
  tmp <- as.character(colors[index])
  
  ggplot(data, aes(x = txas_reperfusion, y = {{y}},
                   fill = txas_reperfusion)) +
    geom_beeswarm(aes(fill = txas_reperfusion), shape = 21, size = 2, cex = 2) +
    geom_boxplot(width=0.2, alpha=0.2,
                 position = position_dodge(width = 1),
                 outlier.shape = NA) +
    labs(x = "Treatment", y = ylabel) +
    theme_bw(base_size = 16) +
    scale_fill_manual("Treatment", values = tmp) +
    theme(legend.position = "top",
          axis.title.y = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 11)) +
    scale_y_continuous(limits = c(lower,upper)) +
    facet_wrap(~ clot_length, ncol = 2, nrow = 1,
               labeller = 
                 labeller(clot_length = 
                            as_labeller(c("3" = "3cm", "4" = "4cm"))))
}

plot_clotlength_stackedbar <- function(data, y, ylabel, size_perc = 4){
  
  dt_sum <- data %>%
    group_by(txas_reperfusion, clot_length, {{y}}) %>%  # Group by treatment and score
    summarise(count = n(), .groups = "drop") %>%     # Count occurrences
    group_by(txas_reperfusion, clot_length) %>%                      # Group again by treatment
    mutate(perc_value = round_percentages((count / sum(count)) * 100))
  
  dt_sum_complete <- dt_sum |>
    ungroup() |>
    mutate({{y}} := factor({{y}}, levels = c(4,3,2,1,0))) |>
    group_by(txas_reperfusion, clot_length) |>
    complete({{y}},
             fill = list(perc_value = 0),
             explicit = TRUE)
  
  index <- c(1, 2, 3, 4, 7)
  #tmp <- as.character(colors[index])
  
  tmp <- c(c(
    "4" = "#D55E00", 
    "3" = "#009E73",
    "2" = "#56B4E9",
    "1" = "#E69F00",
    "0" = "#CACACA"   
  ))
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = "stack", width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    geom_text(data = dt_sum_complete %>% filter(perc_value != 0),
              aes(label=paste0(sprintf("%1.1f", perc_value),"%")),
              position=position_stack(vjust=0.5),
              size = size_perc
    ) +
    theme_bw(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp,
                      guide = guide_legend(reverse = TRUE)
    ) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100.2)) +
    facet_wrap(~ clot_length, ncol = 3, nrow = 2)
  
}


plot_clotlength_interaction <- 
  function(data, y, ylabel,
           pvalue,
           lower = NULL, upper = NULL){
  
  endpoint <- data |> pull({{y}})
  
  if (pvalue < 0.001){
    pvalue <- format_pvalue(pvalue)
  } else {
    pvalue <- paste0("= ", format_pvalue(pvalue))
  }
  
  if(length(levels(data$txas_reperfusion)) == 2){ 
    index <- c(2, 1)
  }
  
  if(is.null(lower) & is.null(upper)){
    lower <- min(endpoint, na.rm = TRUE) - 0.1*abs(min(endpoint, na.rm = TRUE))
    upper <- max(endpoint, na.rm = TRUE) + 0.1*abs(max(endpoint, na.rm = TRUE))
  } else{
    lower = lower
    upper = upper
  }
  
  tmp <- as.character(colors[index])
  
  ggplot(data, aes(x = clot_length, y = {{y}},
                   fill = txas_reperfusion)) +
    geom_beeswarm(aes(fill = txas_reperfusion), shape = 21, size = 2, cex = 2, dodge.width = 1) +
    geom_boxplot(width=0.2, alpha=0.2,
                 position = position_dodge(width = 1),
                 outlier.shape = NA) +
    labs(x = "Clot Length", y = ylabel, 
         caption = glue("p-value for interaction {pvalue}")) +
    theme_prism(base_size = 16) +
    scale_fill_manual("Treatment", values = tmp) +
    theme(legend.position = "top",
          axis.title.y = element_text(size = 13),
          # axis.title.x = element_blank(),
          # axis.text.x = element_blank(),
          # axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 11)) +
    scale_y_continuous(limits = c(lower,upper))
}

plot_clotlength_stackedbar_interaction <- 
  function(data, y, ylabel, pvalue, size_perc = 4){
  
  dt_sum <- data %>%
    select(txas_reperfusion, clot_length, {{y}}) |>
    na.omit() |> 
    group_by(txas_reperfusion, clot_length, {{y}}) %>%  # Group by treatment and score
    summarise(count = n(), .groups = "drop") %>%     # Count occurrences
    group_by(txas_reperfusion, clot_length) %>%                      # Group again by treatment
    mutate(perc_value = round_percentages((count / sum(count)) * 100))
  
  dt_sum_complete <- dt_sum |>
    ungroup() |>
    mutate({{y}} := factor({{y}}, levels = c(4,3,2,1,0))) |>
    group_by(txas_reperfusion, clot_length) |>
    complete({{y}},
             fill = list(perc_value = 0),
             explicit = TRUE) |>
    mutate(txas_reperfusion_cl = 
             paste0(txas_reperfusion, " and ", clot_length, "cm"),
           txas_reperfusion_cl = factor(txas_reperfusion_cl,
                                        levels = c("Control and 3cm",
                                                   "TNKase and 3cm",
                                                   "Control and 4cm",
                                                   "TNKase and 4cm")))
  
  index <- c(1, 2, 3, 4, 7)
  #tmp <- as.character(colors[index])
  
  tmp <- c(
    "4" = "#D55E00", 
    "3" = "#009E73",
    "2" = "#56B4E9",
    "1" = "#E69F00",
    "0" = "#CACACA"   
  )
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion_cl, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = "stack", width = 0.8) +
    labs(
      x = "Treatment Group and Clot Length",
      y = "Percentage (%)",
      fill = ylabel, 
      caption = glue("p-value for interaction = {pvalue}")
    ) +
    geom_text(data = dt_sum_complete %>% filter(perc_value != 0),
              aes(label=paste0(sprintf("%1.1f", perc_value),"%")),
              position=position_stack(vjust=0.5),
              size = size_perc
    ) +
    theme_prism(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp,
                      guide = guide_legend(reverse = TRUE)
    ) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100.2))
  
}

plot_site <- function(data, y, ylabel, lower = NULL, upper = NULL){
  endpoint <- data |> pull({{y}})
  if(is.null(lower) & is.null(upper)){
    lower <- min(endpoint, na.rm = TRUE) - 0.1*abs(min(endpoint, na.rm = TRUE))
    upper <- max(endpoint, na.rm = TRUE) + 0.1*abs(max(endpoint, na.rm = TRUE))
  } else{
    lower = lower
    upper = upper
  }
  if(length(levels(data$txas_reperfusion)) == 2){ #3
    index <- c(2, 1) #c(9, 6, 8)
  } else if(length(levels(data$txas_reperfusion)) == 5){
    index <- c(1, 2, 3, 4, 7)
  } else {
    index <- c(1, 2, 3, 4, 7, 9, 6) #c(1, 2, 3, 4, 7, 9, 6, 8)
  }
  tmp <- as.character(colors[index])
  
  ggplot(data, aes(x = txas_reperfusion, y = {{y}},
                   fill = txas_reperfusion)) +
    geom_beeswarm(aes(fill = txas_reperfusion), shape = 21, size = 2, cex = 2) +
    geom_boxplot(width=0.2, alpha=0.2,
                 position = position_dodge(width = 1),
                 outlier.shape = NA) +
    labs(x = "Treatment", y = ylabel) +
    theme_bw(base_size = 16) +
    scale_fill_manual("Treatment", values = tmp) +
    theme(legend.position = "top",
          axis.title.y = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 10)) +
    scale_y_continuous(limits = c(lower,upper)) +
    facet_wrap(~ site, ncol = 3, nrow = 2)
}

plot_site_bar <- function(data, y, ylabel){
  
  dt_sum <- data %>%
    group_by(txas_reperfusion, site, {{y}}) %>%  # Group by treatment and score
    summarise(count = n(), .groups = "drop") %>%     # Count occurrences
    group_by(txas_reperfusion, site) %>%                      # Group again by treatment
    mutate(perc_value = (count / sum(count)) * 100)
  
  dt_sum_complete <- dt_sum %>%
    ungroup() %>%
    mutate({{y}} := factor({{y}}, levels = 0:4)) |>
    group_by(txas_reperfusion, site) %>%
    complete(
      {{y}},
      fill = list(perc_value = 0),
      explicit = TRUE
    ) %>%
    mutate(perc_value = ifelse(perc_value == 0, 0.2, perc_value))  # Minimal value for visibility
  
  index <- c(1, 2, 3, 4, 7)
  tmp <- as.character(colors[index])
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    theme_bw(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100)) +
    facet_wrap(~ site, ncol = 3, nrow = 2)
  
}
plot_site_stackedbar <- function(data, y, ylabel, size_perc = 4){
  
  dt_sum <- data %>%
    group_by(txas_reperfusion, site, {{y}}) %>%  # Group by treatment and score
    summarise(count = n(), .groups = "drop") %>%     # Count occurrences
    group_by(txas_reperfusion, site) %>%                      # Group again by treatment
    mutate(perc_value = round_percentages((count / sum(count)) * 100))
  
  dt_sum_complete <- dt_sum %>%
    ungroup() %>%
    mutate({{y}} := factor({{y}}, levels = c(4, 3, 2, 1, 0))) |>
    group_by(txas_reperfusion, site) %>%
    complete(
      {{y}},
      fill = list(perc_value = 0, count = 0),
      explicit = TRUE
    )
  
  index <- c(1, 2, 3, 4, 7)
  #tmp <- as.character(colors[index])
  
  tmp <- c(
    "4" = "#D55E00", 
    "3" = "#009E73",
    "2" = "#56B4E9",
    "1" = "#E69F00",
    "0" = "#CACACA"   
  )
  
  #print(dt_sum_complete |> filter(site == "AG"))
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = "stack", width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    geom_text(data = dt_sum_complete %>% filter(perc_value != 0),
              aes(label=paste0(sprintf("%1.1f", perc_value),"%")),
              position=position_stack(vjust=0.5),
              size = size_perc
    ) +
    theme_bw(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp,
                      guide = guide_legend(reverse = TRUE)
    ) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100.2)) +
    facet_wrap(~ site, ncol = 3, nrow = 2)
  
}

plot_enro_model <- function(data, y, ylabel, lower = NULL, upper = NULL){
  endpoint <- data |> pull({{y}})
  if(is.null(lower) & is.null(upper)){
    lower <- min(endpoint, na.rm = TRUE) - 0.1*abs(min(endpoint, na.rm = TRUE))
    upper <- max(endpoint, na.rm = TRUE) + 0.1*abs(max(endpoint, na.rm = TRUE))
  } else{
    lower = lower
    upper = upper
  }
  if(length(levels(data$txas_reperfusion)) == 2){ #3
    index <- c(2, 1) #c(9, 6, 8)
  } else if(length(levels(data$txas_reperfusion)) == 5){
    index <- c(1, 2, 3, 4, 7)
  } else {
    index <- c(1, 2, 3, 4, 7, 9, 6) #c(1, 2, 3, 4, 7, 9, 6, 8)
  }
  tmp <- as.character(colors[index])
  
  ggplot(data, aes(x = txas_reperfusion, y = {{y}},
                   fill = txas_reperfusion)) +
    geom_beeswarm(aes(fill = txas_reperfusion), shape = 21, size = 2, cex = 2) +
    geom_boxplot(width=0.2, alpha=0.2,
                 position = position_dodge(width = 1),
                 outlier.shape = NA) +
    labs(x = "Treatment", y = ylabel) +
    theme_prism(base_size = 16) +
    scale_fill_manual("Treatment", values = tmp) +
    theme(legend.position = "top",
          axis.title.y = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 11)) +
    scale_y_continuous(limits = c(lower,upper)) +
    facet_wrap(~ enro_model, ncol = 2, nrow = 2)
}

plot_enro_model_bar <- function(data, y, ylabel){
  
  dt_sum <- data %>%
    group_by(txas_reperfusion, enro_model, {{y}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(txas_reperfusion, enro_model) %>%
    mutate(perc_value = (count / sum(count)) * 100)
  
  dt_sum_complete <- dt_sum %>%
    ungroup() %>%
    mutate({{y}} := factor({{y}}, levels = 0:4)) |>
    group_by(txas_reperfusion, enro_model) %>%
    complete(
      {{y}},
      fill = list(perc_value = 0),
      explicit = TRUE
    ) %>%
    mutate(perc_value = ifelse(perc_value == 0, 0.2, perc_value))
  
  index <- c(1, 2, 3, 4, 7)
  tmp <- as.character(colors[index])
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    theme_prism(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100)) +
    facet_wrap(~ enro_model, ncol = 2, nrow = 2)
}

plot_enro_model_stackedbar <- function(data, y, ylabel, size_perc = 4){
  
  dt_sum <- data %>%
    group_by(txas_reperfusion, enro_model, {{y}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(txas_reperfusion, enro_model) %>%
    mutate(perc_value = round_percentages((count / sum(count)) * 100))
  
  dt_sum_complete <- dt_sum %>%
    ungroup() %>%
    mutate({{y}} := factor({{y}}, levels = c(4,3,2,1,0))) |>
    group_by(txas_reperfusion, enro_model) %>%
    complete(
      {{y}},
      fill = list(perc_value = 0),
      explicit = TRUE
    )
  
  index <- c(1, 2, 3, 4, 7)
  #tmp <- as.character(colors[index])
  
  tmp <- c(c(
    "4" = "#D55E00", 
    "3" = "#009E73",
    "2" = "#56B4E9",
    "1" = "#E69F00",
    "0" = "#CACACA"   
  ))
  
  ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = "stack", width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    geom_text(data = dt_sum_complete %>% filter(perc_value != 0),
              aes(label=paste0(sprintf("%1.1f", perc_value),"%")),
              position=position_stack(vjust=0.5),
              size = size_perc
    ) +
    theme_prism(base_size = 15) +
    scale_fill_manual(ylabel, values = tmp,
                      guide = guide_legend(reverse = TRUE)
    ) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0,100.2)) +
    facet_wrap(~ enro_model, ncol = 2, nrow = 2)
}

format_full_output <- function(df){
  
  out <- df |>
    mutate(coef = round(coef, 3), 
           se = round(se, 3),
           pvalue = pvalue(pvalue, prefix = c("< ", "", ""))) |>
    rename(Covariate = variable,
           Coefficient = coef, SE = se, "p value" = pvalue) |>
    select(Covariate, Coefficient, SE, 'p value')
  
  return(out)
  
}

format_pi_table <- function(df){
  
  df <- df |>
    mutate(group = as.factor(group))
  
  if(all(levels(df$group) %in% 
         c("overall", "yhm", "aging", "oih", "yhr", "shr"))){
    df <- df |> 
      mutate(group = factor(group,
                            levels = c("overall", "yhm", "aging",
                                       "oih", "yhr", "shr"),
                            labels = c("Overall", "YHM",
                                       "Aging", "OIH", "YHR", "SHR")))
  }
  
  if(all(levels(df$group) %in% c("overall", "female", "male"))){
    df <- df |> 
      mutate(group = factor(group,
                            levels = c("overall", "female", "male"),
                            labels = c("Overall", "Female", "Male")))
  }
  
  if (!is.null(df$pvalue)){
    out <- df |> 
      mutate(ci = glue("{round(estimate,2)} ({round(lower,2)}; {round(upper,2)})")) |>
      arrange(group) |>
      select(group, ci, pvalue) |>
      dplyr::rename('Subgroup' = group, '95% CI' = ci, 'p value' = pvalue)
  } else {
    out <- df |> 
      mutate(ci = glue("{round(estimate,2)} ({round(lower,2)}; {round(upper,2)})")) |>
      arrange(group) |>
      select(group, ci) |>
      dplyr::rename('Subgroup' = group, '95% CI' = ci)
  }
  
  return(out)
}

extract_stratified_analysis <- function(list){
  map(list,
      .f = ~ .x |> filter(str_detect(variable, 
                                     "txas_reperfusion"))) |>
    enframe() |> 
    unnest(cols = c(value)) |>
    select(-variable) |>
    rename(group = name, estimate = prob_index) |>
    mutate(pvalue = pvalue(pvalue, prefix = c("< ", "", ""))) 
}

extract_treatment <- function(list, oneminus = FALSE){
  
  out <- list %>%
    filter(str_detect(variable, "txas_reperfusion")) %>%
    select(-coef, -se) %>%
    rename(group = variable, estimate = prob_index) %>%
    mutate(group = case_when(
      oneminus == TRUE ~ str_c(str_replace(group, "txas_reperfusion", ""), " < Control"),
      oneminus == FALSE ~ str_c("Control < ", str_replace(group, "txas_reperfusion", ""))
      
    ))
  
  if (oneminus){
    out <- out %>%
    mutate(ci = glue("{round(1 - estimate, 2)} ({round(1 - upper, 2)}; {round(1 - lower, 2)})"),
           pvalue = pvalue(pvalue, prefix = c("< ", "", "")))
  } else {
    out <- out %>%
    mutate(ci = glue("{round(estimate, 2)} ({round(lower, 2)}; {round(upper, 2)})"),
           pvalue = pvalue(pvalue, prefix = c("< ", "", "")))
  }
  
  out <- out %>%
    select(group, ci, pvalue) %>%
    rename('Comparison' = group, 'PI (95% CI)' = ci, 'p value' = pvalue)
}

extract_clot_length <- function(list, oneminus = FALSE){
  
  out <-
  list %>%
    filter(str_detect(variable, "clot_length")) %>%
    select(-coef, -se) %>%
    rename(group = variable, estimate = prob_index) %>%
    mutate(group = case_when(
      oneminus == TRUE ~ str_c(str_replace(group, "clot_length", ""), "cm < 3cm"),
      oneminus == FALSE ~ str_c("3cm < ", str_replace(group, "clot_length", ""), "cm")
      
    )) 
  
  if (oneminus){
    out <- out %>%
      mutate(ci = glue("{round(1 - estimate, 2)} ({round(1 - upper, 2)}; {round(1 - lower, 2)})"),
             pvalue = pvalue(pvalue, prefix = c("< ", "", "")))
  } else {
    out <- out %>%
      mutate(ci = glue("{round(estimate, 2)} ({round(lower, 2)}; {round(upper, 2)})"),
             pvalue = pvalue(pvalue, prefix = c("< ", "", "")))
  }
  
  out <- out %>%
    select(group, ci, pvalue) %>%
    rename('Comparison' = group, 'PI (95% CI)' = ci, 'p value' = pvalue)
}


plot_pi <- function(list, route, oneminus){
  dp <- list %>%
    filter(str_detect(variable, "txas_reperfusion")) %>%
    select(-coef, -se) %>%
    rename(group = variable, estimate = prob_index) #%>%
  
  #check for treatment group
  group_letter <- sub("txas_reperfusion(.*)", "\\1", dp$group)
  
  if(route == "all"){
    dp <- dp |>
      mutate(group = case_when(
        (group_letter %in% iv_treatments) & oneminus == TRUE ~ str_c(str_replace(group, "txas_reperfusion", ""), " < IV Control"),
        (group_letter %in% iv_treatments) & oneminus == FALSE ~ str_c(str_replace(group, "txas_reperfusion", ""), " > IV Control"),
        (group_letter %in% ip_treatments) & oneminus == TRUE ~ str_c(str_replace(group, "txas_reperfusion", ""), " < IP Control"),
        (group_letter %in% ip_treatments) & oneminus == FALSE ~ str_c(str_replace(group, "txas_reperfusion", ""), " > IP Control"),
        (group_letter %in% tnk_treatments) & oneminus == TRUE ~ str_c(str_replace(group, "txas_reperfusion", ""), " < Saline  Control"),
        (group_letter %in% tnk_treatments) & oneminus == FALSE ~ str_c(str_replace(group, "txas_reperfusion", ""), " > Saline  Control")
      ),
      pvalue = pvalue(pvalue, prefix = c("< ", "", "")))
  }else {
    dp <- dp |>
      mutate(group = case_when(
        route == "IV" & oneminus == TRUE ~ str_c(str_replace(group, "txas_reperfusion", ""), " < IV Control"),
        route == "IV" & oneminus == FALSE ~ str_c(str_replace(group, "txas_reperfusion", ""), " > IV Control"),
        route == "IP" & oneminus == TRUE ~ str_c(str_replace(group, "txas_reperfusion", ""), " < IP Control"),
        route == "IP" & oneminus == FALSE ~ str_c(str_replace(group, "txas_reperfusion", ""), " > IP Control"),
        route == "None" & oneminus == TRUE ~ str_c(str_replace(group, "txas_reperfusion", ""), " < Saline Control"),
        route == "None" & oneminus == FALSE ~ str_c(str_replace(group, "txas_reperfusion", ""), " > Saline Control")
      ),
      pvalue = pvalue(pvalue, prefix = c("< ", "", "")))
  }
  
  if(length(dp$group) == 1){ #2
    index <- c(6) #c(6, 8)
  } else if(length(dp$group) == 4){
    index <- c(7, 4, 3, 2)
  } else if(length(dp$group) == 5){
    index <- c(6, 7, 4, 3, 2)
  }
  tmp <- as.character(colors[index])
  
  # Reorder data
  dp <- dp[order(grepl("IP", dp$group)), ]
  
  # Update factor levels (so IP groups come last)
  dp$group <- factor(dp$group, levels = rev(dp$group))
  
  ggplot(dp, aes(x = estimate, y = group, color = group)) +
    geom_point(size= 3) +
    scale_color_manual("Treatment", values = tmp) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3, linewidth = 1) +
    geom_vline(xintercept = 0.5, linetype = 2) +
    geom_text(aes(label = pvalue), x = 0.95, color = "black") +
    annotate("text", x = 0.95, y = (nrow(dp) + 0.5), label = "p value")+
    scale_x_continuous(limits = c(0, 1)) +
    labs(x = "Probabilistic Index", y = "") +
    theme_prism(base_size = 16) +
    theme(legend.position = "none")
}


extract_statistic <- function(fit, route = "iv", mi = FALSE){
  
  if (!mi){
    
    if (route == "iv"){
      sm <- summary(fit)
      coef_names <- names(sm@zval)
      index <- which(str_detect(coef_names, "txas_reperfusion"))
      index_name <- str_remove(
        coef_names[str_detect(coef_names, "txas_reperfusion")],
        "txas_reperfusion")
      
      out <- rep(NA, 4)
      names(out) <- c("H", "I", "J", "K")
      out[index_name] <- sm@zval[index]
    }
    
    if (route == "ip") {
      sm <- summary(fit)
      coef_names <- names(sm@zval)
      index <- which(str_detect(coef_names, "txas_reperfusion"))
      index_name <- str_remove(
        coef_names[str_detect(coef_names, "txas_reperfusion")],
        "txas_reperfusion")
      
      out <- rep(NA, 2)
      names(out) <- c("L", "M")
      out[index_name] <- sm@zval[index]
    }
    
  } else {
    sm <- summary(pool(fit))
    
    if (route == "iv"){
      coef_names <- sm$term
      index <- which(str_detect(coef_names, "txas_reperfusion"))
      index_name <- str_remove(
        coef_names[str_detect(coef_names, "txas_reperfusion")],
        "txas_reperfusion")
      
      out <- rep(NA, 4)
      names(out) <- c("H", "I", "J", "K")
      out[index_name] <- sm$statistic[index]
    }
    
    if (route == "ip") {
      coef_names <- sm$term
      index <- which(str_detect(coef_names, "txas_reperfusion"))
      index_name <- str_remove(
        coef_names[str_detect(coef_names, "txas_reperfusion")],
        "txas_reperfusion")
      
      out <- rep(NA, 2)
      names(out) <- c("L", "M")
      out[index_name] <- sm$statistic[index]
    }
  }
  
  return(out)
}

plot_treat_sig <- function(data, y, ylabel, p_values, 
                     lower = NULL, upper = NULL) {
  
  endpoint <- data |> pull({{y}})
  if(is.null(lower) & is.null(upper)){
    lower <- min(endpoint, na.rm = TRUE) - 0.3*abs(min(endpoint, na.rm = TRUE))
    upper <- max(endpoint, na.rm = TRUE) + 0.4*abs(max(endpoint, na.rm = TRUE))
  } else {
    lower <- lower - 0.05 *(lower)
    upper <- upper + 0.4*(upper)
  }
  
  # Define comparisons for geom_signif
  comparisons <- list(c("Control", "TNKase"))
  upper_sig <- upper*c(0.75) 
  index <- c(2, 1) 
  
  tmp <- as.character(colors[index])
  
  ggplot(data, aes(x = txas_reperfusion, y = {{y}},
                   fill = txas_reperfusion)) +
    geom_beeswarm(aes(fill = txas_reperfusion), shape = 21, size = 2, cex = 2) +
    geom_boxplot(width=0.2, alpha=0.2,
                 position = position_dodge(width = 1),
                 outlier.shape = NA) +
    labs(x = "Treatment", y = ylabel) +
    theme_prism(base_size = 16) +
    scale_fill_manual("Treatment", values = tmp) +
    theme(legend.position = "top",
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 11)) +
    scale_y_continuous(limits = c(lower, upper)) +
    geom_signif(
      comparisons = comparisons,
      map_signif_level = TRUE,
      annotations = p_values,
      y_position = upper_sig,  # Adjust the y_position as needed
      tip_length = 0.02,
      textsize = 4,
      color = "black"
    )
  
}

plot_clot_length_sig <- function(data, y, ylabel, p_values, 
                                       lower = NULL, upper = NULL) {
  
  endpoint <- data |> pull({{y}})
  if(is.null(lower) & is.null(upper)){
    lower <- min(endpoint, na.rm = TRUE) - 0.3*abs(min(endpoint, na.rm = TRUE))
    upper <- max(endpoint, na.rm = TRUE) + 0.4*abs(max(endpoint, na.rm = TRUE))
  } else {
    lower <- lower - 0.05 *(lower)
    upper <- upper + 0.4*(upper)
  }
  
  # Define comparisons for geom_signif
  comparisons <- list(c("3", "4"))
  upper_sig <- upper*c(0.75) 
  index <- c(2, 1) 
  
  tmp <- as.character(colors[index])
  
  ggplot(data, aes(x = clot_length, y = {{y}},
                   fill = clot_length)) +
    geom_beeswarm(aes(fill = clot_length), shape = 21, size = 2, cex = 2) +
    geom_boxplot(width=0.2, alpha=0.2,
                 position = position_dodge(width = 1),
                 outlier.shape = NA) +
    labs(x = "Clot Length", y = ylabel) +
    theme_prism(base_size = 16) +
    scale_fill_manual("Clot Length", values = tmp) +
    theme(legend.position = "top",
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 11)) +
    scale_y_continuous(limits = c(lower, upper)) +
    geom_signif(
      comparisons = comparisons,
      map_signif_level = TRUE,
      annotations = p_values,
      y_position = upper_sig,  # Adjust the y_position as needed
      tip_length = 0.02,
      textsize = 4,
      color = "black"
    )
  
}

plot_treat_sig_bar <- function(data, y, ylabel, p_values, 
                         lower = NULL, upper = NULL,
                         size_perc = 4) {
  
  dt_sum <- data %>%
    select(txas_reperfusion, {{y}}) |>
    na.omit() |>
    group_by(txas_reperfusion, {{y}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(txas_reperfusion) %>%
    mutate(perc_value = (count / sum(count)) * 100)
  
  upper_max = 100
  
  upper_sig = c(upper_max + 6)
  upper_lim <- upper_max + 6
  comparisons <- list(
    c("Control", "TNKase"))

  
  # Ensure all combinations of txas_reperfusion and postop_d1_nds_score exist
  dt_sum_complete <- dt_sum |>
    ungroup() |>
    mutate({{y}} := factor({{y}}, levels = c(4, 3, 2, 1, 0))) |>
    complete(txas_reperfusion, {{y}},
             fill = list(perc_value = 0),
             explicit = TRUE) 
  
  index <- c(1, 2, 3, 4, 7)
  #tmp <- as.character(colors[index])
  
  tmp <- c(c(
    "4" = "#D55E00", 
    "3" = "#009E73",
    "2" = "#56B4E9",
    "1" = "#E69F00",
    "0" = "#CACACA"   
  ))
  
  g <- ggplot(dt_sum_complete, aes(x = txas_reperfusion, y = perc_value,
                              fill = {{y}})) +
    geom_col(position = "stack", width = 0.8) +
    labs(
      x = "Treatment Group",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    geom_text(data = dt_sum_complete %>% filter(perc_value != 0),
              aes(label=paste0(sprintf("%1.1f", perc_value),"%")),
              position=position_stack(vjust=0.5),
              size = size_perc
    ) +
    theme_prism(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp, 
                      guide = guide_legend(reverse = TRUE)
    ) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + 
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                         limits = c(0,110)) +
    geom_signif(
      comparisons = comparisons,
      map_signif_level = TRUE,
      annotations = p_values,
      y_position = upper_sig,  # Adjust the y_position as needed
      tip_length = 0.02,
      textsize = 4,
      color = "black"
    )
  
  return(g)
}

plot_clot_length_sig_bar <- function(data, y, ylabel, p_values, 
                                           lower = NULL, upper = NULL,
                                           size_perc = 4) {
  
  dt_sum <- data %>%
    select(clot_length, {{y}}) |>
    na.omit() |>
    group_by(clot_length, {{y}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(clot_length) %>%
    mutate(perc_value = (count / sum(count)) * 100)
  
  upper_max = 100
  
  upper_sig = c(upper_max + 6)
  upper_lim <- upper_max + 6
  comparisons <- list(
    c("3", "4"))
  
  
  # Ensure all combinations of txas_reperfusion and postop_d1_nds_score exist
  dt_sum_complete <- dt_sum |>
    ungroup() |>
    mutate({{y}} := factor({{y}}, levels = c(4, 3, 2, 1, 0))) |>
    complete(clot_length, {{y}},
             fill = list(perc_value = 0),
             explicit = TRUE) 
  
  index <- c(1, 2, 3, 4, 7)
  #tmp <- as.character(colors[index])
  
  tmp <- c(c(
    "4" = "#D55E00", 
    "3" = "#009E73",
    "2" = "#56B4E9",
    "1" = "#E69F00",
    "0" = "#CACACA"   
  ))
  
  g <- ggplot(dt_sum_complete, aes(x = clot_length, y = perc_value,
                                   fill = {{y}})) +
    geom_col(position = "stack", width = 0.8) +
    labs(
      x = "Clot Length",
      y = "Percentage (%)",
      fill = ylabel
    ) +
    geom_text(data = dt_sum_complete %>% filter(perc_value != 0),
              aes(label=paste0(sprintf("%1.1f", perc_value),"%")),
              position=position_stack(vjust=0.5),
              size = size_perc
    ) +
    theme_prism(base_size = 16) +
    scale_fill_manual(ylabel, values = tmp, 
                      guide = guide_legend(reverse = TRUE)
    ) +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 11),
      legend.position = "top"
    ) + 
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                       limits = c(0,110)) +
    geom_signif(
      comparisons = comparisons,
      map_signif_level = TRUE,
      annotations = p_values,
      y_position = upper_sig,  # Adjust the y_position as needed
      tip_length = 0.02,
      textsize = 4,
      color = "black"
    )
  
  return(g)
}

round_percentages <- function(x, digits = 10) {
  x_scaled <- x / sum(x) * 100
  
  multiplier <- 10^digits
  
  x_floor <- floor(x_scaled * multiplier) / multiplier
  
  target_total <- round(sum(x_scaled), digits = digits)
  diff_units <- round((target_total - sum(x_floor)) * multiplier)
  
  residuals <- x_scaled - x_floor
  
  add_index <- order(residuals, decreasing = TRUE)[seq_len(diff_units)]
  x_floor[add_index] <- x_floor[add_index] + 1 / multiplier
  
  return(x_floor)
}

plot_summary <- function(list, comparison = "treatment"){
  
    dp <- map_dfr(names(list), ~{
      df <- list[[.x]]
      
      pi_str <- df[["PI (95% CI)"]]
      pval_raw <- df[["p value"]]
      
      # Extract numbers from PI
      parts <- str_match(pi_str, "([0-9.]+) \\(([0-9.]+); ([0-9.]+)\\)")
      
      # Handle p-value formatting
      if (is.character(pval_raw) && str_detect(pval_raw, "<")) {
        pval_label <- pval_raw
        pval_num <- as.numeric(str_remove(pval_raw, "<\\s*"))
      } else {
        pval_num <- as.numeric(pval_raw)
        pval_label <- pvalue(pval_num, prefix = c("< ", "", ""))
      }
      
      data.frame(
        group = .x,
        estimate = as.numeric(parts[2]),
        lower = as.numeric(parts[3]),
        upper = as.numeric(parts[4]),
        pval_num = pval_num,
        pval_label = pval_label
      )
    }) |> as.data.frame()

  dp <- dp |>
    mutate(group = factor(group,
                          levels = c("noimputation","imputation_ws_overall","imputation_ws_cls","imputation_ws_clst","imputation_mi"),
                          labels = c("No Imputation","Worst Score Overall","Worst Score CLS","Worst Score CLST","Multiple Imputation")))
  
  # define final order 
  y_levels <- c("No Imputation","Worst Score Overall","Worst Score CLS","Worst Score CLST","Multiple Imputation")

  # assign to factor
  dp$group <- factor(as.character(dp$group), levels = y_levels)
  
  if (comparison == "treatment"){
    main_label = "TNKase"
  } else {
    main_label = "3cm"
  }
  # ggplot
  ggplot(dp, aes(x = estimate, y = group)) +
    geom_point(size = 3, na.rm = TRUE, color = "black") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), orientation = "y",
                   color = "black", height = 0.3, linewidth = 1, 
                   na.rm = TRUE) +
    geom_vline(xintercept = 0.5, linetype = 2) +
    geom_text(
      aes(label = pval_label, color = pval_num <= 0.05),
      x = 0.95, size = 4, na.rm = TRUE
    ) +
    scale_color_manual(values = c(`TRUE` = "#E41A1C", `FALSE` = "black"), guide = "none") +
    annotate("text", x = 0.95, y = length(y_levels) + 0.4, label = "p value", size = 4) +
    scale_y_discrete(limits = rev(y_levels),  # reverse order here
                     drop = FALSE) +
    scale_x_continuous(limits = c(0, 1)) +
    labs(x = "Probabilistic Index", y = "") +
    theme_prism(base_size = 16) +   
    theme(legend.position = "none",
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          plot.margin = ggplot2::margin(t = 40, r = 10, b = 10, l = 10)) +
    coord_cartesian(clip = "off") +
    #arrow + label above plot 
    # arrow line in npc (relative) coordinates
    annotation_custom(
      grob = grid::linesGrob(
        x = grid::unit(c(0.5, 0.7), "npc"),  
        y = grid::unit(1.05, "npc"),          # just above the panel
        arrow = grid::arrow(type = "closed", length = grid::unit(6, "pt")),
        gp = grid::gpar(lwd = 2, fill = "black", col = "black")
      )
    ) +
    annotation_custom(
      grob = grid::textGrob(
        paste(main_label, " better"),
        x = grid::unit(0.72, "npc"),  # just after arrow
        y = grid::unit(1.05, "npc"),
        hjust = 0, gp = grid::gpar(cex = 0.9, col = "black",
                                   fontface = "bold")
      )
    )
}
# Blurbs ----

interpretation <- list()
footnote <- list()

interpretation$treat <-glue("Effect size calculated from this semiparametric regression model is known as probability index (PI). The PI is defined as the probability that animals in the treatment group have better outcome when compared to animals in the control group. If PI = 0.5, there is no differences between treatment groups; If PI > 0.5, then treatment group X has more animals with better score than control group; If PI < 0.5, then treatment group X has less animals with better score than the control group.")
interpretation$model <- glue("A semiparametric probabilistic index model was fitted with treatment as main effect adjusted by other confounding covariates including sex, clot size, and site.")

interpretation$imp <- 
  glue("Two data imputation methods are performed in the analysis: WS and MI. WS is known as the Worst Rank Score Imputation that replaces missing values with the worst possible rank score within strata that could be defined as (1) overall, (2) Clot Size and Sex and (3) Clot Size, Sex and Study Arm. MI is known as Multiple Imputation that involves generating several plausible imputed datasets. This method uses Predictive Mean Matching, where plausible values are obtained by randomly selecting an animal with observed data that matches covariates such as treatment group, site, sex, and clot size.")

interpretation$mortality <-
  
  glue("Kaplan-Meier curves were plotted for treatment vs control. Hazard ratios were estimated using Cox Regression Models. The hazard ratio (HR) represents the risk of death from Treament group relative to the risk of death from the Control grup at any time in the trial. When HR = 1, we do not see evidence of different mortality risk between the study arms.

The models were adjusted for sex and clot length and stratified by site.")

#Models where the proportional hazards (PH) assumption for the main treatment effect was not met were also stratified by animal model. 
# The PH assumption for treatment was assessed based on Schoenfeld residuals. Hazard ratios for models where the PH p value < 0.05 (PH assumption violated) are not valid. Hazard ratios with 95% C.I. from 0 to Infinity mean that the model did not converge and are also not valid. 
footnote$ci <- "No multiplicity correction" #glue("Ajustment for multiplicity (Bonferroni Correction).")
footnote$pvals <- "No multiplicity correction" #glue("Ajustment for multiplicity (Holm-Bonferroni Correction).")
footnote$pi <- glue("PI = Probability Index")
footnote$hr <- glue("HR = Hazard Ratio")
# Labels ----

data_labels <- 
  list(site = "Site",
       enro_sex = "Sex",
       enro_model = "Enrollment model",   
       enro_vendor = "Vendor",
       enro_animal_source = "Source",
       enro_strain = "Strain",
       enro_species = "Species",
       rand_conduct = "Was randomization conducted?",
       rand_conduct_rsn = "Reason for failure in randomization",
       rand_conduct_rsn_oth = "Reason for failure in randomization - Other",
       rand_occlusion = "Occlusion Period",
       rand_dose = "Dose of intervention at randomization (mls)",
       txas_reperfusion_actual = "Actual Study Arm", 
       txas_reperfusion = "Study Arm", 
       srg_conduct = "Was surgery conducted?",
       srg_conduct_rsn = "Reason for failure in surgery",
       successful_surgery = "Successful Surgery",
       srg_tx_dose = "Dose of intervention at surgery (mls)",
       srg_comments_1 = "Surgery: Excessive bleeding during surgery",
       srg_comments_2 = "Surgery: No drop in Doppler flow",
       srg_comments_3 = "Surgery: Doppler flow returned to baseline shortly after occlusion",
       srg_comments_4 = "Surgery: Animal briefly stopped breathing during surgery",
       srg_comments_5 = "Surgery: Animal died during surgery",
       srg_comments_6 = "Surgery: Unable to advance occlusion suture",
       srg_comments_7 = "Surgery: Animal died during occlusion period",
       srg_comments_8 = "Surgery: Animal died during recovery after reperfusion",
       srg_comments_9 = "Surgery: Other",
       srg_animal_age = "Age on surgery day (months)",
       srg_length_surgery = "Length of surgery (minutes)",
       srg_exact_occ = "Exact occlusion period (minutes)",
       srg_ldf_drop = "Post occlusion drop in doppler reading (% dropped)",
       srg_concom_meds_sq = "SQ bupivacaine administered",
       srg_concom_meds_lrs = "1.0ml/100g warmed LRS administered",
       srg_occlu_sleep_ind = "Did the animal remain asleep during occlusion period?", 
       srg_weight = "Weight at surgery (g)",
       postop_d1_weight = "Post Op Weight at day 1 (g)",
       postop_d2_weight = "Post Op Weight at day 2 (g)",
       postop_d3_weight = "Post Op Weight at day 3 (g)",
       postop_d4_weight = "Post Op Weight at day 4 (g)",
       postop_d5_weight = "Post Op Weight at day 5 (g)",
       postop_d6_weight = "Post Op Weight at day 6 (g)",
       postop_d7_weight = "Post Op Weight at day 7 (g)",
       postop_d8_weight = "Post Op Weight at day 8 (g)",
       perc_postop_d1_weight = "Percent change Post Op Weight at day 1 from Surgery",
       perc_postop_d2_weight = "Percent change Post Op Weight at day 2 from surgery",
       perc_postop_d3_weight = "Percent change Post Op Weight at day 3 from surgery",
       perc_postop_d4_weight = "Percent change Post Op Weight at day 4 from surgery",
       perc_postop_d5_weight = "Percent change Post Op Weight at day 5 from surgery",
       perc_postop_d6_weight = "Percent change Post Op Weight at day 6 from surgery",
       perc_postop_d7_weight = "Percent change Post Op Weight at day 7 from surgery",
       perc_postop_d8_weight = "Percent change Post Op Weight at day 8 from surgery",
       postop_d1_neuro_score = "Neurological Deficit Score at day 1",
       postop_d2_neuro_score = "Neurological Deficit Score at day 2",
       eos_nds_score = "Neurological Deficit Score - Sum of score at the end of the study",
       eos_nds_normal = "Neurological Deficit Score: Normal",
       srg_width_suture = "Filament Size - Width",
       srg_length_suture = "Filament Size - Length",
       srg_embolic_draw_time_diff = "Time difference between clot draw and surgery dates",
       srg_embolic_draw_enro_sex = "Is the same sex between donor and receiver of clot?",
       rand_clot_length = "Randomized Clot Length",
       srg_clot_length = "Surgery Clot Length",
       clot_length = "Clot Length",
       eos_day_diff_srg_death = "Survival",
       eos_conduct = "Did the animal survival to the end of the trial?",
       eos_weight = "Weight at the end of the study (g)",
       bank_conduct = "Was tissue banked for this animal?",
       compliance_vial_id = "Compliance of Vial ID as randomized",
       compliance_tnk_adm = "Compliance TNK administration as randomized",
       compliance_tnk_dose = "Compliance of TNK dose as randomized",
       compliance_clot_length = "Compliance of clot length as randomized",
       compliance_donor_id = "Compliance of donor ID and receiver ID are different", 
       compliance_donor_sex = "Compliance of sex of the donor and recepient are different",
       compliance_clot_draw_srg_date = "Compliance of clot drawn and actual surgery dates different",
       stage = "Stage",
       srg_nds_score = "Neurological Deficit Score - Sum of score at surgery", 
       enro_weight = "Weight at enrollment (g)",
       srg_concom_meds_lrs_nacl = "Was 1.0ml/100g warmed LRS or 0.9% NaCl administered?",
       srg_concom_meds_sq = "Was SQ bupivacaine administered?",
       srg_reperfsn_time_suture = "Date and time of reperfusion",
       srg_awake_min = "Length of occlusion (minutes)",
       txas_reperfusion = "Treatment",
       full_span_score = "Full SPAN Score at Day 30",
       animal_death_before_conduct_d3 = "Animal Death at Day 3",
       animal_death_before_conduct_d7 = "Animal Death at Day 7",
       animal_death_before_conduct_d30 = "Animal Death at Day 30",
       behav_d30_conduct = "Conduct behavior tests at Day 30", 
       behav_d30_conduct_rsn = "Reason for not conducting behavior tests at Day 30",
       corner_bl_conduct = "Conduct corner test at Day 0",
       corner_bl_conduct_rsn = "Reason for not conducting corner test at Day 0",
       corner_d7_conduct = "Conduct corner test at Day 7",
       corner_d7_conduct_rsn = "Reason for not conducting corner test at Day 7",
       corner_d30_conduct = "Conduct corner test at Day 30",
       corner_d30_conduct_rsn = "Reason for not conducting corner test at Day 30",
       alternative_corner_index_d0 = "Alternative Corner Index at Baseline",
       alternative_corner_index_d7 = "Alternative Corner Index at Day 7",
       alternative_corner_index_d30 = "Alternative Corner Index at Day 30",
       p_mri_d2_ind = "Was the scan successfully processed through the
pipeline?",
       p_mri_d2_ind = "Was the scan successfully processed through the pipeline?",
       p_mri_d2_rsn_1 = "Reason scan not successfully processed through pipeline: Missing sequence(s)",
       p_mri_d2_rsn_2 = "Reason scan not successfully processed through pipeline: Motion artifact",
       p_mri_d2_rsn_3 = "Reason scan not successfully processed through pipeline: Scanner placement issue",
       p_mri_d2_rsn_4 = "Reason scan not successfully processed through pipeline: Failed nifty conversion",
       p_mri_d2_rsn_5 = "Reason scan not successfully processed through pipeline: Bad image quality",
       p_mri_d2_rsn_6 = "Reason scan not successfully processed through pipeline: Ghosting artifact",
       p_mri_d2_rsn_7 = "Reason scan not successfully processed through pipeline: Masking threshold error",
       p_mri_d2_rsn_8 = "Reason scan not successfully processed through pipeline: Atlas registration failed",
       p_mri_d2_rsn_9 = "Reason scan not successfully processed through pipeline: Other",
       mri_d2_conduct = "MRI conducted at Day 3",
       mri_d2_conduct_rsn = "Reasons for not conducting MRI at Day 3",
       p_mri_d2_mid_shift_indx = "Day 3 Midline Shift Index",
       p_mri_d2_fract_lesion_left = "Day 3 Volume Fraction Lesion of the Left Hemisphere", 
       p_mri_d2_fract_lesion_right = "Day 3 Volume Fraction Lesion of the Right Hemisphere",
       p_mri_d2_fract_csf_left = "Day 3 Volume Fraction CSF of the Left Hemisphere",
       p_mri_d2_fract_csf_right = "Day 3 Volume Fraction CSF of the Right Hemisphere",
       p_mri_d2_fract_tissue_left = "Day 3 Volume Fraction Tissue of the Left Hemisphere",
       p_mri_d2_fract_tissue_right = "Day 3 Volume Fraction Tissue of the Right Hemisphere",
       simple_span_score = "Simple SPAN Score",  
       duke_score = "Duke Score",
       mgh_score= "MGH Score",
       postop_d1_nds_score = "Bederson Neurodeficit at Day 1",
       postop_d1_nds_score_conduct = "Conduct Bederson Neurodeficit at Day 1",
       postop_d2_nds_score = "Bederson Neurodeficit at Day 2",
       postop_d2_nds_score_conduct = "Conduct Bederson Neurodeficit at Day 2",
       eos_nds_score = "Bederson Neurodeficit at Day 30",
       eos_nds_score_conduct = "Conduct Bederson Neurodeficit at Day 30",
       rand_dose1 = "Dose 1 of intervention treatment (mls) - Randomized",
       rand_dose2 = "Dose 2 of intervention treatment (mls) - Randomized",
       rand_dose3 = "Dose 3 of intervention treatment (mls) - Randomized",
       rand_dose4 = "Dose 4 of intervention treatment (mls) - Randomized",
       rand_dose5 = "Dose 5 of intervention treatment (mls) - Randomized",
       srg_tx1_dose = "Dose 1 of intervention (mls) - Surgical",
       srg_tx2_dose = "Dose 2 of intervention (mls) - Surgical",
       srg_tx3_dose = "Dose 3 of intervention (mls) - Surgical for IV",
       ip_pop_dose3 = "Dose 3 of intervention (mls) - Surgical for IP",
       ip_pop_dose4 = "Dose 4 of intervention (mls) - Surgical",
       ip_pop_dose5 = "Dose 5 of intervention (mls) - Surgical"
  )