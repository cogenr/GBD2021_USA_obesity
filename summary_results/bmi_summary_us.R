#------------------------------------------------------------
# Purpose: compile GBD2021 US results for high BMI
#------------------------------------------------------------

rm(list = ls())
source('../subnational_mapping.R')
source("../stgpr/api/public.R")
source("../get_model_results.R")
source("../get_age_metadata.R")
source("../get_location_metadata.R")
source("../get_population.R")
source("../get_crosswalk_version.R")
source("../make_aggregates.R")
source("../get_outputs.R")
source("../get_draws.R")
source("../get_cause_metadata.R")

library(data.table)
library(tidyverse)
save_data <- T
prevalence <- T
results_fp <- ""

# results directories
ow_not_ob <- "../prev_ow_not_ob_custom/"
ow_and_ob <- "../prev_ow_custom/"
obesity <- "../prev_ob_custom/"
models <- data.table(ow_not_ob, ow_and_ob, obesity)

# use release_id 9 for all the machinery results
locs <- get_location_metadata(release_id = 9, location_set_id = 35)
ages <- get_age_metadata(release_id = 9)
locs_of_interest <- locs[parent_id==102 | location_id==102, .(location_id, ihme_loc_id, location_name)]

# pull population estimates
pops <- get_population(release_id = 9, age_group_id = c(34, 6:20, 30:32, 235), sex_id = c(1,2,3),
                       year_id = c(1980:2021), location_id = locs_of_interest$location_id)

if(dir.exists(results_fp) == FALSE){
  dir.create(results_fp)
} 

if(prevalence){
  for(m in names(models)){
    print(m)
    results_dir <- models[[m]]
    
    prev_draws <- lapply(paste0(results_dir, locs_of_interest$location_id, ".csv"), fread) %>%
      rbindlist(use.names = TRUE)
    
    prev_draws <- prev_draws[year_id %in% c(1990:2021)]
    
    # reshape the draws to wide
    prev_draws[,draw:=paste0("draw_", draw)]
    prev_draws <- dcast.data.table(prev_draws[,.(location_id, year_id, age_group_id, sex_id, draw, value)], 
                                   location_id + year_id + age_group_id + sex_id ~ draw, value.var = "value")
    
    # create estimates for children (2-24, 5-24) and adults (25+) separately
    for(age_group in c("children")){
      if(age_group == "children"){
        #age_grps <- c(34,6:9)
        age_grps <- c(6,7)
        age_name <- "5_14"
      } else {
        age_grps <- c(10:20, 30:32, 235)
        age_name <- "25_125"
      }
      print(paste0("age group: ",age_group))
      print(age_grps)
      prev_draws_wide <- prev_draws[age_group_id %in% age_grps]
      
      # Defining names for new columns
      vars <- paste0("draw_", seq(0, 999, 1)) 
      vars_AS <- paste0("draw_AS_", seq(0, 999, 1)) 
      count_cols <- paste0("draw_count_", seq(0, 999, 1)) 
      vars_AA <- paste0("draw_AA_", seq(0, 999, 1))
      p_change_2021 <- paste0("draw_2021_", seq(0, 999, 1))
      p_change_1990 <- paste0("draw_1990_", seq(0, 999, 1))
      p_num_chg_2021 <- paste0("draw_num_chg_2021_", seq(0, 999, 1))
      p_num_chg_1990 <- paste0("draw_num_chg_1990_", seq(0, 999, 1))
      pct_point_chg_2021 <- paste0("draw_pct_point_chg_2021_", seq(0, 999, 1))
      pct_point_chg_1990 <- paste0("draw_pct_point_chg_1990_", seq(0, 999, 1))
      
      # Merging population and age weights onto prevalence draws
      current <- merge(prev_draws_wide, ages[,.(age_group_id, age_group_name,age_group_weight_value)], by = "age_group_id", all.x = T)
      current <- merge(current[year_id >= 1980], pops[, .(age_group_id, location_id, year_id, sex_id, population)], by = c("location_id", "year_id", "age_group_id", "sex_id"), all.x = T)
      
      if(nrow(current[is.na(age_group_id)]) > 0) stop("Missing age groups")
      if(nrow(current[is.na(population)]) > 0) stop("Missing populations")
      
      print("Dataset prepared")
      
      # Creating both sexes sample
      both_sexes <- copy(current)
    
      # Getting age-standardized estimates by sex-year-location
      current[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("location_id", "year_id", "sex_id")]
      
      # Getting age-standardized estimates by year-location
      both_sexes[, both_sex_population := sum(population), by = c("location_id", "year_id", "age_group_id")]
      both_sexes[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "sex_id", "age_group_id")]
      both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("location_id", "year_id", "age_group_id")]
      
      both_sexes[, (vars) := lapply(.SD, function(x) x/both_sex_population), .SDcols = count_cols, by = c("location_id", "year_id", "age_group_id")]
      both_sexes$population <- NULL
      
      print(nrow(both_sexes))
      both_sexes <- both_sexes[sex_id == 1]
      both_sexes$sex_id <- NULL
      print(nrow(both_sexes))
      
      both_sexes[, (vars_AS) := lapply(.SD, function(x) sum(x*age_group_weight_value/sum(age_group_weight_value))), .SDcols = vars, by = c("location_id", "year_id")]
      
      # Getting the number of ow/ob population by sex-year-location
      current[, (count_cols) := lapply(.SD, function(x) x*population), .SDcols = vars, by = c("location_id", "year_id", "sex_id", "age_group_id")]
      current[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("location_id", "year_id", "sex_id")]
      
      # Getting the number of ow/ob population by year-location
      both_sexes[, (count_cols) := lapply(.SD, function(x) sum(x)), .SDcols = count_cols, by = c("location_id", "year_id")]
      
      # Getting all-ages prevalence by sex-year-location
      current[, all_pop := sum(population), by = c("location_id", "year_id", "sex_id")]
      current[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("location_id", "sex_id", "year_id")]
      
      # Getting all-ages prevalence by year-location
      both_sexes[, all_pop := sum(both_sex_population), by = c("location_id", "year_id")]
      both_sexes[, (vars_AA) := lapply(.SD, function(x) x/all_pop), .SDcols = count_cols, by = c("location_id", "year_id")]
      
      #---------------prev estimates----------------
      print("Getting prevalence estimates!")
      
      # For age-specific:
      current[, mean_prev := rowMeans(.SD), .SD = vars]
      current[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      current[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      both_sexes[, mean_prev := rowMeans(.SD), .SD = vars]
      both_sexes[, lower_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars]
      both_sexes[, upper_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars]
      
      # For age-standardized:
      current[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      current[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      current[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      both_sexes[, mean_AS_prev := rowMeans(.SD), .SD = vars_AS]
      both_sexes[, lower_AS_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AS]
      both_sexes[, upper_AS_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AS]
      
      # For number of ow/ob population: 
      current[, mean_count := rowMeans(.SD), .SD = count_cols]
      current[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      current[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      both_sexes[, mean_count := rowMeans(.SD), .SD = count_cols]
      both_sexes[, lower_count := apply(.SD, 1, quantile, c(.025)), .SDcols = count_cols]
      both_sexes[, upper_count := apply(.SD, 1, quantile, c(.975)), .SDcols = count_cols]
      
      # For all-ages prevalence: 
      current[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      current[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      current[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      both_sexes[, mean_AA_prev := rowMeans(.SD), .SD = vars_AA]
      both_sexes[, lower_AA_prev := apply(.SD, 1, quantile, c(.025)), .SDcols = vars_AA]
      both_sexes[, upper_AA_prev := apply(.SD, 1, quantile, c(.975)), .SDcols = vars_AA]
      
      #---------percentage change of prevalence---------
      print("Onto percentage change of prevalence!")
      
      # Percent change of AS prevalence between 2021 and 1990 by sex-location-age
      current_2021 <- copy(current[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      current_1990 <- copy(current[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      current_p <- merge(current_2021, current_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_p[, ..p_change_2021]-current_p[, ..p_change_1990])/current_p[, ..p_change_1990]
      current_p[, (p_change_2021) := temp]
      
      # Percent change of AS prevalence between 2021 and 1990 by location-age
      both_sexes_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      both_sexes_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      both_sexes_p <- merge(both_sexes_2021, both_sexes_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_sexes_p[, ..p_change_2021]-both_sexes_p[, ..p_change_1990])/both_sexes_p[, ..p_change_1990]
      both_sexes_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age-sex-location
      current_by_age_2021 <- copy(current[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      current_by_age_1990 <- copy(current[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      current_by_age_p <- merge(current_by_age_2021, current_by_age_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_by_age_p[, ..p_change_2021]-current_by_age_p[, ..p_change_1990])/current_by_age_p[, ..p_change_1990]
      current_by_age_p[, (p_change_2021) := temp]
      
      # Percentage change of age-specific prevalence between 2021 and 1990 by age-location
      both_by_age_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      both_by_age_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      both_by_age_p <- merge(both_by_age_2021, both_by_age_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_by_age_p[, ..p_change_2021]-both_by_age_p[, ..p_change_1990])/both_by_age_p[, ..p_change_1990]
      both_by_age_p[, (p_change_2021) := temp]
      
      print("Summarizing percentage change!")
      
      # For percentage change
      current_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      current_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      current_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      both_sexes_p[, mean_AS_change := rowMeans(.SD), .SD = p_change_2021]
      both_sexes_p[, lower_AS_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      both_sexes_p[, upper_AS_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      current_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      current_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      current_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      both_by_age_p[, mean_change := rowMeans(.SD), .SD = p_change_2021]
      both_by_age_p[, lower_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      both_by_age_p[, upper_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      print("Onto percentage point change of prevalence!") # newly added by XD!
      
      # Percent point change of AS prevalence between 2021 and 1990 by sex-location-age
      current_pct_point_2021 <- copy(current[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      current_pct_point_1990 <- copy(current[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      current_pct_point <- merge(current_pct_point_2021, current_pct_point_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_pct_point[, ..p_change_2021]-current_pct_point[, ..p_change_1990])
      current_pct_point[, (p_change_2021) := temp]
      
      # Percent point change of AS prevalence between 2021 and 1990 by location-age
      both_sexes_pct_point_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars_AS]
      both_sexes_pct_point_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars_AS]
      
      both_sexes_pct_point <- merge(both_sexes_pct_point_2021, both_sexes_pct_point_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_sexes_pct_point[, ..p_change_2021]-both_sexes_p[, ..p_change_1990])
      both_sexes_pct_point[, (p_change_2021) := temp]
      
      # Percentage point change of age-specific prevalence between 2021 and 1990 by age-sex-location
      current_pct_point_by_age_2021 <- copy(current[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      current_pct_point_by_age_1990 <- copy(current[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      current_pct_point_by_age <- merge(current_pct_point_by_age_2021, current_pct_point_by_age_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_pct_point_by_age[, ..p_change_2021]-current_pct_point_by_age[, ..p_change_1990])
      current_pct_point_by_age[, (p_change_2021) := temp]
      
      # Percentage point change of age-specific prevalence between 2021 and 1990 by age-location
      both_pct_point_by_age_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = vars]
      both_pct_point_by_age_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = vars]
      
      both_pct_point_by_age <- merge(both_pct_point_by_age_2021, both_pct_point_by_age_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_pct_point_by_age[, ..p_change_2021]-both_pct_point_by_age[, ..p_change_1990])
      both_pct_point_by_age[, (p_change_2021) := temp]
      
      print("Summarizing percentage point change!")
      
      # For percentage change
      current_pct_point[, mean_AS_pct_point_change := rowMeans(.SD), .SD = p_change_2021]
      current_pct_point[, lower_AS_pct_point_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      current_pct_point[, upper_AS_pct_point_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      both_sexes_pct_point[, mean_AS_pct_point_change := rowMeans(.SD), .SD = p_change_2021]
      both_sexes_pct_point[, lower_AS_pct_point_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      both_sexes_pct_point[, upper_AS_pct_point_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      current_pct_point_by_age[, mean_pct_point_change := rowMeans(.SD), .SD = p_change_2021]
      current_pct_point_by_age[, lower_pct_point_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      current_pct_point_by_age[, upper_pct_point_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      both_pct_point_by_age[, mean_pct_point_change := rowMeans(.SD), .SD = p_change_2021]
      both_pct_point_by_age[, lower_pct_point_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      both_pct_point_by_age[, upper_pct_point_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      ## added pct point change done ##
      
      #--------absolute change in number of ow/ob population---------
      print("Onto absolute change in number of ow/ob population")
      
      # absolute change between 2021 and 1990 by sex-location
      current_2021 <- copy(current[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      current_1990 <- copy(current[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      current_count <- merge(current_2021, current_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_count[, ..p_num_chg_2021]-current_count[, ..p_num_chg_1990])
      current_count[, (p_num_chg_2021) := temp]
      
      # Percent change between 2021 and 1990 by location
      both_sexes_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_num_chg_2021) := .SD, .SDcols = count_cols]
      both_sexes_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_num_chg_1990) := .SD, .SDcols = count_cols]
      
      both_sexes_count <- merge(both_sexes_2021, both_sexes_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_sexes_count[, ..p_num_chg_2021]-both_sexes_count[, ..p_num_chg_1990])
      both_sexes_count[, (p_num_chg_2021) := temp]
      
      print("Summarizing absolute change in number of ow/ob population!")
      
      # For percentage change
      current_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      current_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      current_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]
      
      both_sexes_count[, mean_abs_count_change := rowMeans(.SD), .SD = p_num_chg_2021]
      both_sexes_count[, lower_abs_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_num_chg_2021]
      both_sexes_count[, upper_abs_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_num_chg_2021]
      
      #------- percentage change in number of ow/ob population--------
      print("Onto percentage change in number of ow/ob population")
      
      # Percent change between 2021 and 1990 by sex-location
      current_2021 <- copy(current[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      current_1990 <- copy(current[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      current_p_count <- merge(current_2021, current_1990, by = c("location_id", "age_group_id", "sex_id"))
      
      temp <- (current_p_count[, ..p_change_2021]-current_p_count[, ..p_change_1990])/current_p_count[, ..p_change_1990]
      current_p_count[, (p_change_2021) := temp]
      
      # Percent change between 2021 and 1990 by location
      both_sexes_2021 <- copy(both_sexes[year_id == 2021]) %>% .[, (p_change_2021) := .SD, .SDcols = count_cols]
      both_sexes_1990 <- copy(both_sexes[year_id == 1990]) %>% .[, (p_change_1990) := .SD, .SDcols = count_cols]
      
      both_sexes_p_count <- merge(both_sexes_2021, both_sexes_1990, by = c("location_id", "age_group_id"))
      
      temp <- (both_sexes_p_count[, ..p_change_2021]-both_sexes_p_count[, ..p_change_1990])/both_sexes_p_count[, ..p_change_1990]
      both_sexes_p_count[, (p_change_2021) := temp]
      
      print("Summarizing percentage change in number of ow/ob population!")
      
      # For percentage change
      current_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      current_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      current_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      both_sexes_p_count[, mean_count_change := rowMeans(.SD), .SD = p_change_2021]
      both_sexes_p_count[, lower_count_change := apply(.SD, 1, quantile, c(.025)), .SDcols = p_change_2021]
      both_sexes_p_count[, upper_count_change := apply(.SD, 1, quantile, c(.975)), .SDcols = p_change_2021]
      
      #-------final stretch----------
      print("In the final stretch!")
      
      # Get summary values for AS and AA prevalence
      both_summary <- unique(both_sexes[, .(year_id, location_id, 
                                            mean_count, lower_count, upper_count, 
                                            mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                            mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      
      summary <- unique(current[, .(year_id, location_id, sex_id,
                                    mean_count, lower_count, upper_count, 
                                    mean_AS_prev, lower_AS_prev, upper_AS_prev, 
                                    mean_AA_prev, lower_AA_prev, upper_AA_prev)])
      
      # Get summary values for age-specific prevalence
      both_by_age <- unique(both_sexes[, .(year_id, location_id, age_group_id, 
                                           mean_prev, lower_prev, upper_prev)])
      
      by_age <- unique(current[, .(year_id, location_id, age_group_id, sex_id,
                                   mean_prev, lower_prev, upper_prev)])
      
      # Get summary values of pct change and pct point change of AS prevalence
      # by sex and location
      p_summary <- unique(current_p[, .(sex_id, location_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      p_both_summary <- unique(both_sexes_p[, .(location_id, mean_AS_change, lower_AS_change, upper_AS_change)])
      
      p_point_summary <- unique(current_pct_point[, .(sex_id, location_id, mean_AS_pct_point_change, lower_AS_pct_point_change, upper_AS_pct_point_change)])
      p_point_both_summary <- unique(both_sexes_pct_point[, .(location_id, mean_AS_pct_point_change, lower_AS_pct_point_change, upper_AS_pct_point_change)])
      
      # Get summary values of pct change and pct point change of age-specific prevalence
      # by sex and by location
      p_by_age <- unique(current_by_age_p[, .(sex_id, age_group_id, location_id, mean_change, lower_change, upper_change)])
      p_point_by_age <- unique(current_pct_point_by_age[, .(sex_id, age_group_id, location_id, mean_pct_point_change, lower_pct_point_change, upper_pct_point_change)])
      
      # for both sexes by location
      p_both_by_age <- unique(both_by_age_p[, .(location_id, age_group_id, mean_change, lower_change, upper_change)])
      p_point_both_by_age <- unique(both_pct_point_by_age[, .(location_id, age_group_id, mean_pct_point_change, lower_pct_point_change, upper_pct_point_change)])
      
      # Get summary values of total count abs change
      # by location by sex and for both sexes
      count <- unique(current_count[, .(sex_id, location_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      both_count <- unique(both_sexes_count[, .(location_id, mean_abs_count_change, lower_abs_count_change, upper_abs_count_change)])
      
      # Get summary values of total count percent change
      # by location
      p_count <- unique(current_p_count[, .(sex_id, location_id, mean_count_change, lower_count_change, upper_count_change)])
      p_both_count <- unique(both_sexes_p_count[, .(location_id, mean_count_change, lower_count_change, upper_count_change)])
      
      # Combine all of the datasets
      both_summary[, sex_id := 3]
      both_by_age[, sex_id := 3]
      p_both_summary[, sex_id := 3]
      p_point_both_summary[, sex_id := 3]
      p_both_by_age[, sex_id := 3]
      p_point_both_by_age[, sex_id := 3]
      both_count[, sex_id := 3]
      p_both_count[, sex_id := 3]
      
      summary <- rbindlist(list(both_summary, summary), use.names = T, fill = T)
      
      by_age <- rbindlist(list(both_by_age, by_age), use.names = T, fill = T)
      
      p_summary <- rbindlist(list(p_summary, p_both_summary), use.names = T, fill = T)
      
      p_point_summary <- rbindlist(list(p_point_summary, p_point_both_summary), use.names = T, fill = T)
      
      p_by_age <- rbindlist(list(p_by_age, p_both_by_age), use.names = T, fill = T)
      
      p_point_by_age <- rbindlist(list(p_point_by_age, p_point_both_by_age), use.names = T, fill = T)
      
      count <- rbindlist(list(count, both_count), use.names = T, fill = T)
      
      p_count <- rbindlist(list(p_count, p_both_count), use.names = T, fill = T)
      
      # merge the results
      summary <- merge(summary, p_summary, by = c("sex_id", "location_id"), all = T)
      summary <- merge(summary, count, by = c("sex_id", "location_id"), all = T)
      summary <- merge(summary, p_count, by = c("sex_id", "location_id"), all = T)
      summary <- merge(summary, p_point_summary, by = c("sex_id", "location_id"), all = T)
      by_age <- merge(by_age, p_by_age, by = c("sex_id", "age_group_id", "location_id"), all = T)
      by_age <- merge(by_age, p_point_by_age, by = c("sex_id", "age_group_id", "location_id"), all = T)
      
      # Merge with locations and names
      summary <- merge(summary, locs[,.(location_id, location_name)], by = "location_id", all.x = T)
      by_age <- merge(by_age, locs[,.(location_id, location_name)], by = "location_id", all.x = T)
      by_age <- merge(by_age, ages[, .(age_group_id, age_group_name)], by = "age_group_id", all.x = T)
      
      # Clean up columns
      summary[, Sex := ifelse(sex_id == 1, "Male", 
                              ifelse(sex_id == 2, "Female", "Both"))]
      by_age[, Sex := ifelse(sex_id == 1, "Male", 
                             ifelse(sex_id == 2, "Female", "Both"))]
      
      
      summary[, location_level := ifelse(location_id == 102, "Country", "State")]
                                         
      by_age[, location_level := ifelse(location_id == 102, "Country", "State")]
      
      # save the results
      print("save the results!")
      fwrite(summary, paste0(results_fp,"gbd_", age_name, "_", m, "_summary_usa.csv"), row.names = F)
      fwrite(by_age, paste0(results_fp, "gbd_", age_name, "_", m, "_by_age_usa.csv"), row.names = F)
    }
  }
  print("prevalence results done!")
}
