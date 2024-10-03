
rm(list = ls())
library(data.table)
library(tidyverse)

#------------------------------------------------------------- 
# pull the GBD and FHS results together by children and adults
#-------------------------------------------------------------

results_dir <- "../final_results_us/"

# pull the age specific results together
for(m in c("obesity","ow_and_ob","ow_not_ob")){
  
  if(m == "obesity"){
    fhs_m <- "obesity"
  } else if(m == "ow_and_ob"){
    fhs_m <- "overweight"
  } else if(m == "ow_not_ob"){
    fhs_m <- "overweight_less_obesity"
  }
  
  for(age_grp in c("2_24","25_125")){
    # read in the GBD results
    gbd <- fread(paste0(results_dir,"gbd_",age_grp,"_",m,"_by_age_usa.csv")) %>% .[, version := "gbd"] %>% .[,metric := fhs_m]
    
    # read in the FHS results, only keep 2022 and later
    fhs <- fread(paste0(results_dir,"fhs_",age_grp,"_",fhs_m,"_by_age_usa.csv")) %>% .[, version := "fhs"] %>% .[year_id>=2022] %>% .[,metric := fhs_m]

    # pull the results together
    final <- rbindlist(list(gbd, fhs), use.names = T)
    
    # sort the results by location, year, sex, and age
    setcolorder(final, c(2,4,3,1,5:ncol(final)))
    final <- final[order(location_id, year_id, sex_id, age_group_id), ]
    
    # calculate annualized rate of change (ARC)
    final[year_id <= 2021, mean_arc := mean_change/(2021-1990)]
    final[year_id > 2021, mean_arc := mean_change/(2050-2021)]
    
    final[year_id <= 2021, lower_arc := lower_change/(2021-1990)]
    final[year_id > 2021, lower_arc := lower_change/(2050-2021)]
    
    final[year_id <= 2021, upper_arc := upper_change/(2021-1990)]
    final[year_id > 2021, upper_arc := upper_change/(2050-2021)]
    
    # save the final combined results
    fwrite(final, paste0(results_dir,"combined_",age_grp,"_",m,"_by_age_usa_arc.csv"), row.names = F)
    
  }
}

# pull the summary results together
for(m in c("obesity","ow_and_ob","ow_not_ob")){
  
  if(m == "obesity"){
    fhs_m <- "obesity"
  } else if(m == "ow_and_ob"){
    fhs_m <- "overweight"
  } else if(m == "ow_not_ob"){
    fhs_m <- "overweight_less_obesity"
  }
  
  all_data <- NULL
  for(age_grp in c("2_14","2_24","5_14","5_24","15_24","25_125")){
    # read in the GBD results
    gbd <- fread(paste0(results_dir,"gbd_",age_grp,"_",m,"_summary_usa.csv")) %>% .[, version := "gbd"] %>% .[,metric := fhs_m] %>% .[, age_group := age_grp]
    
    # read in the FHS results, only keep 2022 and later
    fhs <- fread(paste0(results_dir,"fhs_",age_grp,"_",fhs_m,"_summary_usa.csv")) %>% .[, version := "fhs"] %>% .[year_id>=2022] %>% .[,metric := fhs_m] %>% .[, age_group := age_grp]

    # pull the results together
    final <- rbindlist(list(gbd, fhs), use.names = T)
    
    # sort the results by location
    setcolorder(final, c(1,3,2,ncol(final),4:(ncol(final)-1)))
    final <- final[order(location_id, year_id, sex_id, age_group),]
    
    # combind results together
    all_data <- rbindlist(list(all_data, final), use.names = T)
  }
  
  # calculate annualized rate of change (ARC)
  all_data[year_id <= 2021, mean_arc := mean_AS_change/(2021-1990)]
  all_data[year_id > 2021, mean_arc := mean_AS_change/(2050-2021)]
  
  all_data[year_id <= 2021, lower_arc := lower_AS_change/(2021-1990)]
  all_data[year_id > 2021, lower_arc := lower_AS_change/(2050-2021)]
  
  all_data[year_id <= 2021, upper_arc := upper_AS_change/(2021-1990)]
  all_data[year_id > 2021, upper_arc := upper_AS_change/(2050-2021)]
  
  # save the final combined results
  fwrite(all_data, paste0(results_dir,"combined","_",m,"_summary_usa_arc.csv"), row.names = F)
}
  
