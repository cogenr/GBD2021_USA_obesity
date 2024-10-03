#--------------------------------------------------
# purpose: create figures for the BMI USA papers
#--------------------------------------------------
#-------------------------------------------------------------------------------
# Figures/tables to use in the BMI US paper for adults and children, respectively
# Figure 1. Map of AS prevalence for adults and children from 1990 to 2021
# Figure 2. Map of change of AS prevalence between 1990 and 2021 and between 2021 and 2050
# Figure 3. Line plots with the annual rate of change over time
#-------------------------------------------------------------------------------

## SET-UP ##
rm(list = ls())
library(data.table)
library(ggplot2)
library(tidyverse)
library(rhdf5)
library(scales)
library(Hmisc)
library(knitr)
library(ggridges)
library(RColorBrewer)
library(openxlsx)
library(ggrepel)
library(grid)
library(treemapify)
options(max.print=2000)
# load sources
source("../gbd2023_map.R")
source('../subnational_mapping.R')
source("../graphing_labels_tables.R")
source("../stgpr/r_functions/utilities/utility.r")
source("../utils.R")
source("../stgpr/api/public.R")
source("../US_mapping/us_mapping.R")
invisible(sapply(list.files("../r/", full.names = T), source))

# main dir
main_dir <- "FILEPATH"

# Round specific variables
gbd_id <- 7
release <- 9
decomp <- "iterative"

# Getting location metadata
locs <- get_location_metadata(22, release_id = release)
locs_usa <- locs[location_id==102 | parent_id==102, location_id]

# Getting age metadata
ages <- get_age_metadata(release_id = release)
ages[,defined := paste0(age_group_years_start,"-",age_group_years_end)]

# Getting population metadata
# pull forecasted population for years 2022 to 2050 (do not include 2021)
pops <- get_population(release_id = 9, age_group_id = c(6:20, 30:32, 235), sex_id = c(1,2,3),
                       year_id = c(2022:2050), location_id = locs_usa, forecasted_pop = T)
pops_1_4 <- get_population(release_id = 9, age_group_id = 5, sex_id = c(1,2,3),
                           year_id = c(2022:2050), location_id = locs_usa, forecasted_pop = T)

# pull 2021 pop to calculate the ratio between 2-4 and 1-4
pop_2021 <- get_population(release_id = 9, year_id = 2021, age_group_id = c(34,5), sex_id = c(1,2,3), location_id = locs_usa)

# dcast for age group 5 and 34 to 
pop_2021 <- dcast.data.table(pop_2021, location_id + year_id + sex_id ~ age_group_id, value.var = "population")
setnames(pop_2021, c("5", "34"), c("pop_1_4", "pop_2_4"))
pop_2021[, ratio := pop_2_4/pop_1_4]

# merge ratio to pops_1_4 and calculate the population for 2-4
pops_2_4 <- merge(pops_1_4, pop_2021[,.(location_id, sex_id, ratio)], by = c("location_id", "sex_id"), all.x = T)
pops_2_4[, population := population*ratio]
pops_2_4[, age_group_id := 34]

# append pops_2_4 with pops
pops <- rbindlist(list(pops, pops_2_4[, -c("ratio")]), use.names = T)

# get 1990 to 2021 population from GBD 2021 results
pops_2021_all <- get_population(release_id = 9, age_group_id = c(34,6:20, 30:32, 235), sex_id = c(1,2,3),
                                year_id = 1990:2021, location_id = locs_usa)

# combine 2021 (run_id 359) and 2022-2050 (run_id 6) population
pops <- rbindlist(list(pops_2021_all, pops), use.names = T)

#---------------------------------------------------------------------------------
# Figure 1. Map of AS prevalence for adults and children from 1990 to 2021
#---------------------------------------------------------------------------------
color_palette <- colorRampPalette(brewer.pal(11, "RdYlBu"))(11)
color_palette <- rev(color_palette)

# age_groups <- c("2_14","5_14","2_24","5_24","15_24","25_125")
age_groups <- c("15_24")
metrics <- c("ow_and_ob") #"obesity", "ow_and_ob", "ow_not_ob"

for (age_grp in age_groups){    
  for(m in metrics){
    print(paste0("Plotting ", m, " for age group ", age_grp))
    # load data, keep countries only
    prev <- fread(paste0(main_dir, "combined_", m, "_summary_usa.csv")) %>% .[location_level=="State"] %>% .[age_group==age_grp]
    
    # plot age-standardized prev
    prev[, mapvar := mean_AS_prev * 100]
    
    years <- c(1990,2000,2010,2021,2050) # change this line to only plot 2021 results
    #years <- c(2021)
    sex <- c(1,2,3)
    
    minval <- min(prev[year_id %in% years,mapvar], na.rm = T) %>% floor
    maxval <- max(prev[year_id %in% years,mapvar], na.rm = T) %>% ceiling
    
    if(m=="ow_and_ob"){
      plot_title <- "prevalence overweight and obese"
      plot_y <- "Prevalence overweight and obese (%)"
    } else if(m=="obesity") {
      plot_title <- "Prevalence obese"
      plot_y <- "Prevalence obese (%)"
    } else if(m=="ow_not_ob") {
      plot_title <- "Prevalence overweight"
      plot_y <- "Prevalence overweight (%)"
    }
    
    pdf(paste0("FILEPATH"),width=20,height=10)
    for(y in years){
      for(s in sex){
        sex_name <- ifelse(s==1, "Males", 
                           ifelse(s==2,"Females", "Both sexes"))
        main_title <- paste0("Age-standardized ",plot_title," among ", age_grp,", ", sex_name, ", " , y)
        
        # p_map <- us_mapping(prev, valname = "mapvar", superpose = NULL, sex=s, year=y, agetext = age_grp, map0=smap, 
        #                     states=NULL, main = main_title, legend = TRUE, legname = plot_y,
        #                     boundcol='grey', scalecols = color_palette, logt=FALSE, minAnchor = minval, 
        #                     maxAnchor = maxval, greybound = 0.,  nacol = 'white', sub = NULL, tsize = 13)
        
        p_map <- us_mapping_label(prev, valname = "mapvar", superpose = NULL, sex=s, year=y, agetext = age_grp, map0=smap, 
                            states=NULL, main = main_title, legend = TRUE, legname = plot_y,
                            boundcol='grey', scalecols = color_palette, logt=FALSE, minAnchor = minval, 
                            maxAnchor = maxval, greybound = 0.,  nacol = 'white', sub = NULL, tsize = 13)
        print(p_map)
      }
    }
    dev.off()
  }
}

#-------------------------------------------------------------------------------------------------
# Figure 2. Map of change of AS prevalence between 1990 and 2021 and between 2021 and 2050
#-------------------------------------------------------------------------------------------------

age_groups <- c("2_14","5_14","2_24","5_24","15_24","25_125")
metrics <- c("obesity", "ow_and_ob", "ow_not_ob")

for (age_grp in age_groups){    
  for(m in metrics){
    print(paste0("Plotting ", m, " for age group ", age_grp))
    # load data, keep countries only
    prev_full <- fread("FILEPATH") %>% .[location_level=="State"] %>% .[age_group==age_grp]
    
    for(y in c(2021, 2050)){
      prev <- unique(prev_full[year_id==y,.(year_id, location_id, location_name, sex_id, age_group, mean_AS_change, lower_AS_change, upper_AS_change,
                                               mean_abs_count_change, lower_abs_count_change, upper_abs_count_change,
                                               mean_count_change, lower_count_change, upper_count_change)])
      # plot percent change in age-standardized prev
      prev[, mapvar := mean_AS_change * 100]
      
      sex <- c(1,2,3)
      
      minval <- min(prev[,mapvar], na.rm = T) %>% floor
      maxval <- max(prev[,mapvar], na.rm = T) %>% ceiling
      
      if(m=="ow_and_ob"){
        if(y==2021){
          plot_title <- "Percent change of proportion overweight and obese between 1990 and 2021"
        } else if(y==2050){
          plot_title <- "Percent change of proportion overweight and obese between 2021 and 2050"
        }
        plot_y <- "Percent change of overweight and obese (%)"
      } else if(m=="obesity") {
        if(y==2021){
          plot_title <- "Percent change of proportion obese between 1990 and 2021"
        } else if(y==2050){
          plot_title <- "Percent change of proportion obese between 2021 and 2050"
        }
        plot_y <- "Percent change of obese (%)"
      } else if(m=="ow_not_ob") {
        if(y==2021){
          plot_title <- "Percent change of proportion overweight between 1990 and 2021"
        } else if(y==2050){
          plot_title <- "Percent change of proportion overweight between 2021 and 2050"
        }
        plot_y <- "Percent change of overweight (%)"
      }
      
      pdf(paste0("FILEPATH"),width=20,height=10)
      for(s in sex){
        sex_name <- ifelse(s==1, "Males", 
                           ifelse(s==2,"Females", "Both sexes"))
        main_title <- paste0(plot_title," among ", age_grp,", ", sex_name)
        
        p_map <- us_mapping_label(prev, valname = "mapvar", superpose = NULL, sex=s, year=y, agetext = age_grp, map0=smap, 
                            states=NULL, main = main_title, legend = TRUE, legname = plot_y,
                            boundcol='grey', scalecols = color_palette, logt=FALSE, minAnchor = minval, 
                            maxAnchor = maxval, greybound = -100.,  nacol = 'white', sub = NULL, tsize = 13)
        
        print(p_map)
      }
      dev.off()
    }
  }
}

#-------------------------------------------------------------------------
# Figure 3. Line plots with the annual rate of change over time
#-------------------------------------------------------------------------

age_groups <- c("2_14","5_14","2_24","5_24","15_24","25_125")
metrics <- c("obesity", "ow_and_ob", "ow_not_ob")

states_dt <- data.table(
  location_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                    "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", 
                    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
                    "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                    "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                    "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  loc_name = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", 
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
               "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
               "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
               "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)

# Plot all states in one graph. 
for (age_grp in age_groups){    
  for(m in metrics){
    
    #yearly cahnge
    prev <- fread(paste0(main_dir, age_grp, "_", m, "_summary_usa.csv"))
    prev[, yearly_change := (mean_AS_prev - lag(mean_AS_prev))*100, by = c("location_name", "Sex")]
    prev[, yearly_change_lead := lead(yearly_change), by = c("location_name", "Sex")]
    prev <- prev[is.na(yearly_change), yearly_change := yearly_change_lead]
    prev[, mean_AS_prev := mean_AS_prev*100]
    
    #make colour scheme for gradient
    col_s <- c("darkblue","cornflowerblue","springgreen4","gold","red", "darkred")
    inv_col_s <- c("red", "gold","springgreen4" ,"cornflowerblue","darkblue")
    
    sex <- c(1,2,3)
    
    pdf(paste0("FILEPATH"),width=16,height=10)
    for(s in sex){
      
      set.seed(135)
      sex_name <- ifelse(s==1, "Males", 
                         ifelse(s==2,"Females", "Both sexes"))
      yc_min <- prev[, yearly_change] %>% min
      yc_max <- prev[, yearly_change] %>% max
      
      country_data <- prev %>% filter(sex_id == s, location_level == "Country") %>% data.table
      state_data <- prev %>% filter(sex_id == s, location_level == "State") %>% data.table
      
      # Create a data table with US states, DC and their abbreviations
      state_data <- merge(state_data, states_dt, by = "location_name", all.x = TRUE)
      
      # Create the plot
      p <- ggplot() +
        geom_line(data = country_data, aes(x = year_id, y = mean_AS_prev, color=yearly_change), size = 3) +
        geom_line(data = state_data, aes(x = year_id, y = mean_AS_prev, color=yearly_change, group = location_name), 
                  size = 0.5, alpha = 0.3) +
        theme_bw()+
        geom_text_repel(data = prev[sex_id==s & year_id == 1990],force=0.01,direction="y",
                        aes(label = location_name, y=mean_AS_prev, x = 1989) , hjust = 1, fontface = "bold", color = "#888888", size = 3.5)+
        geom_text_repel(data = prev[sex_id==s & year_id == 2021], force=0.01,direction="y",
                        aes(label = location_name, y=mean_AS_prev, x = 2022) , hjust = 0, fontface = "bold", color = "#888888", size = 3.5)+
        labs(x = "Year",
             y = "Prevalence Estimate (%)",
             title = paste0("Prevalence ",  m, " by state"),
             subtitle = paste0(age_grp,", ",sex_name)) +
        theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
        theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
        theme(axis.text.x      = element_text(size=14)) +
        theme(axis.text.y      = element_text(size=14)) +
        scale_color_gradientn(colors=col_s, name="Yearly change in prevalence (%)", breaks = round(seq(yc_min, yc_max, length.out=4), digits = 1))+
        scale_x_continuous(breaks = c(1990,2000,2010,2021), expand = c(0.5, 0.5)) +
        coord_cartesian(clip = 'off') +
        theme(legend.position = 'bottom',
              panel.border = element_blank(),
              plot.margin = margin(0.1, 5, 0.1, 0.1, "cm")) #top, right, bottom, and left margins
      
      print(p)
    }
    dev.off()
  }
}

# plot only country level results of ow+ob, ob and ow by sex from 1990 to 2050 with UI
for (age_grp in age_groups){
  # combine data for all metrics
  ow <- fread("FILEPATH") %>% .[age_group == age_grp]
  ob <- fread("FILEPATH") %>% .[age_group == age_grp]
  ow_ob <- fread("FILEPATH") %>% .[age_group == age_grp]
  
  prev <- rbindlist(list(ow,ob,ow_ob), use.names = T)
  
  # change metric name
  prev[, metric_name := ifelse(metric == "overweight", "Overweight and Obesity", 
                               ifelse(metric == "obesity", "Obesity", "Overweight"))]
  
  #yearly change
  prev[, yearly_change := (mean_AS_prev - lag(mean_AS_prev))*100, by = c("location_name", "Sex", "age_group", "metric")]
  prev[, yearly_change_lead := lead(yearly_change), by = c("location_name", "Sex", "age_group", "metric")]
  prev <- prev[is.na(yearly_change), yearly_change := yearly_change_lead]
  prev[, mean_AS_prev := mean_AS_prev*100]
  prev[, lower_AS_prev := lower_AS_prev*100]; prev[, upper_AS_prev := upper_AS_prev*100]
  
  prev_country <- prev[location_level=="Country"]
  
  #make colour scheme for gradient
  col_s <- c("darkblue","cornflowerblue","springgreen4","gold","red", "darkred")
  inv_col_s <- c("red", "gold","springgreen4" ,"cornflowerblue","darkblue")
  
  sex <- c(1,2,3)
  
  pdf(paste0("FILEPATH"),width=16,height=10)
  for(s in sex){
    
    set.seed(135)
    sex_name <- ifelse(s==1, "Males", 
                       ifelse(s==2,"Females", "Both sexes"))
    yc_min <- prev_country[, yearly_change] %>% min
    yc_max <- prev_country[, yearly_change] %>% max
    
    
    p <- ggplot(prev_country[sex_id==s],aes(x=year_id, y=mean_AS_prev, group=metric))+
      geom_ribbon(aes(ymin = lower_AS_prev, ymax = upper_AS_prev), fill = "grey", alpha = 0.2) + 
      geom_line(aes(color=yearly_change),size=1.5) + geom_vline(xintercept = 2021, linetype = "dotted", size = 1) +
      theme_bw()+
      geom_text_repel(data = prev_country[sex_id==s & year_id == 1990],force=0.01,direction="y",
                      aes(label = metric_name, x = 1989) , hjust = 1, fontface = "bold", color = "#888888", size = 3.5)+
      geom_text_repel(data = prev_country[sex_id==s & year_id == 2050], force=0.01,direction="y",
                      aes(label = metric_name, x = 2051) , hjust = 0, fontface = "bold", color = "#888888", size = 3.5)+
      labs(x = "Year",
           y = "Prevalence Estimate (%)",
           title = paste0("Prevalence Estimates from 1990 to 2021 and Forecasts to 2050"),
           subtitle = paste0(age_grp,", ",sex_name)) +
      theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
      theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
      theme(axis.text.x      = element_text(size=14)) +
      theme(axis.text.y      = element_text(size=14)) +
      scale_color_gradientn(colors=col_s, name="Yearly change in prevalence (%)", breaks = round(seq(yc_min, yc_max, length.out=4), digits = 1))+
      scale_x_continuous(breaks = c(1990,2000,2010,2021,2030,2040,2050), expand = c(0.5, 0.5)) +
      coord_cartesian(clip = 'off') +
      theme(legend.position = 'bottom',
            panel.border = element_blank(),
            plot.margin = margin(0.1, 5, 0.1, 0.1, "cm")) #top, right, bottom, and left margins
    print(p)
  }
  dev.off()
}
