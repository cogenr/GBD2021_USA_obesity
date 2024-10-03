#### Create figures and tables for GBD2021 USA obesity paper
#### Cleaned for publication 10/2024
## 1. Final overweight and obese trends (Figure)
## 2. birth cohorts by super region (Figure)
## 3. Sources used (Table)
## 4. Counts overweight (Table)
## 5. Prevalence overweight by 5-year age group (Table)
## 6. Prevalence obese by 5-year age group (Table)

## Load in libraries and dependencies
invisible(sapply(list.files('FILEPATH', full.names = T), source))
source('FILEPATH')
library(plyr)
library(dplyr)
library(bit64)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(ggpubr)
library(rvest)
library(kableExtra)

## Load in location and age information
locs <- get_location_metadata(location_set_id=22, release_id=16)[,c("location_id", "location_name_short", "super_region_id", "super_region_name",
                                                                   "region_id", "region_name", "ihme_loc_id","location_type")]
locs[, country_id := as.integer(as.factor(substr(ihme_loc_id, 1, 3)))]
locs[, country_name := substr(ihme_loc_id, 1, 3)]
countries <- unique(locs[!grepl("_", ihme_loc_id) & (location_type=="admin0"| location_type=="nonsovereign")])

ages <- get_age_metadata(age_group_set_id=24, release_id=16)[, .(age_group_id, age_start = age_group_years_start, age_end = age_group_years_end-1, 
                                                                age_mid = .5*(age_group_years_start + age_group_years_end))]
ages$age_start <- round(ages$age_start)
ages$age_end <- round(ages$age_end)
ages[, age_label := paste0(age_start, "-", age_end, " years (", age_group_id, ")")]
ages$age_label <- factor(ages$age_label, levels = c("2-4 years (34)", "5-9 years (6)", "10-14 years (7)", "15-19 years (8)", "20-24 years (9)", "25-29 years (10)", 
                                                    "30-34 years (11)", "35-39 years (12)", "40-44 years (13)", "45-49 years (14)", "50-54 years (15)", "55-59 years (16)", 
                                                    "60-64 years (17)", "65-69 years (18)", "70-74 years (19)", "75-79 years (20)", "80-84 years (30)", 
                                                    "85-89 years (31)", "90-94 years (32)", "95-124 years (235)"))

## Load in data
out_path <- 'FILEPATH'
usa <- unique(locs[grepl("USA", country_name)])

prev_ow <- fread('FILEPATH')
prev_ow <- rbind(prev_ow, fread('FILEPATH'), fill=T)
prev_ow <- prev_ow[location_id==102]
prev_ow <- rbind(prev_ow, fread('FILEPATH'), fill=T)
prev_ow <- rbind(prev_ow, fread('FILEPATH'), fill=T)

prev_ob <- fread('FILEPATH')
prev_ob <- rbind(prev_ob, fread('FILEPATH'), fill=T)
prev_ob <- prev_ob[location_id==102]
prev_ob <- rbind(prev_ob, fread('FILEPATH'), fill=T)
prev_ob <- rbind(prev_ob, fread('FILEPATH'), fill=T)

prev_ow$age_group_name <- factor(prev_ow$age_group_name, levels = c("2 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                                                                    "30 to 34", "35 to 39", "40 to 44","45 to 49", "50 to 54", "55 to 59", 
                                                                    "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84" ,"85 to 89", "90 to 94",  "95 plus"))

prev_ob$age_group_name <- factor(prev_ob$age_group_name, levels = c("2 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                                                                    "30 to 34", "35 to 39", "40 to 44","45 to 49", "50 to 54", "55 to 59", 
                                                                    "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84" ,"85 to 89", "90 to 94",  "95 plus"))

#### 1. Final overweight and obese trends ####

  prev_ow_plot <- prev_ow[year_id>1990 & year_id<=2021 & sex_id %in% c(1,2) & age_group_id %in% c(6:20,235)]
  prev_ob_plot <- prev_ob[year_id>1990 & year_id<=2021 & sex_id %in% c(1,2) & age_group_id %in% c(6:20,235)]
  prev_ow_plot[age_group_name=="95 plus",age_group_name:="80+"]
  prev_ob_plot[age_group_name=="95 plus",age_group_name:="80+"]
  prev_ow_plot[,age_group_name:=gsub(" to ", "-", age_group_name)]
  prev_ob_plot[,age_group_name:=gsub(" to ", "-", age_group_name)]
  
  prev_ow_plot$age_group_name <- factor(prev_ow_plot$age_group_name, levels = c("2-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                                                      "30-34", "35-39", "40-44","45-49", "50-54", "55-59", 
                                                                      "60-64", "65-69", "70-74", "75-79", "80+"))
  
  prev_ob_plot$age_group_name <- factor(prev_ob_plot$age_group_name, levels = c("2-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                                                      "30-34", "35-39", "40-44","45-49", "50-54", "55-59", 
                                                                      "60-64", "65-69", "70-74", "75-79", "80+"))
  
  prev_ow_plot[,label:=paste0(location_name, ", ", Sex)]
  prev_ob_plot[,label:=paste0(location_name, ", ", Sex)]
  ages_plot <- unique(prev_ob_plot$age_group_name)
  custom_labels <- ifelse(seq_along(levels(prev_ob_plot$age_group_name)) %% 5 == 1, levels(prev_ob_plot$age_group_name), "")
  
  pdf(paste0('FILEPATH'),width=15,height=10)
  loc <- "United States of America"
      
  g1 <- ggplot()  +
        geom_line(data=prev_ob_plot[location_name==loc & year_id==2021], aes(x=age_group_name, y=mean_prev*100, group=as.factor(label), color=as.factor(label)), alpha=0.7, linewidth=1) +
        geom_ribbon(data=prev_ob_plot[location_name==loc & year_id==2021],aes(x=age_group_name, y=mean_prev*100, ymin=lower_prev*100, ymax=upper_prev*100, group=as.factor(label), fill=as.factor(Sex)), alpha=0.1) +
        # ylim(0,1) + 
        scale_fill_manual(values=c("slateblue","red3")) +
        scale_color_manual(values=c("slateblue","red3")) +
        scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.05))) + 
        theme_classic() + theme(axis.text.x = element_text(angle = 60, vjust = 1.15, hjust=1.25), plot.title = element_text(hjust = 0.5), 
                                legend.position = "inside", legend.justification=c(0.9,0.05), panel.border = element_rect(fill = NA), axis.ticks.length = unit(0.25, "cm"),
                                axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = 1)) +  
        guides(size = guide_legend(override.aes = list(color="black",fill="black")), fill="none") +
        labs(x="Age", y = "Prevalence (%)", title = "Obesity (BMI>=30)",color=element_blank())
      print(g1)
      
      
    g2 <- ggplot()  +
        geom_line(data=prev_ow_plot[location_name==loc & year_id==2021], aes(x=age_group_name, y=mean_prev*100, group=as.factor(Sex), color=as.factor(Sex)), alpha=0.7, linewidth=1) +
        geom_ribbon(data=prev_ow_plot[location_name==loc & year_id==2021],aes(x=age_group_name, y=mean_prev*100, ymin=lower_prev*100, ymax=upper_prev*100, group=as.factor(Sex), fill=as.factor(Sex)), alpha=0.1) +
        # ylim(0,1) + 
        scale_fill_manual(values=c("slateblue","red3")) +
        scale_color_manual(values=c("slateblue","red3")) +
        scale_x_discrete(expand = c(0, 0)) + scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.05))) + 
        theme_classic() + theme(axis.text.x = element_text(angle = 60, vjust = 1.15, hjust=1.25), legend.position="bottom", plot.title = element_text(hjust = 0.5), 
                                panel.border = element_rect(fill = NA), axis.ticks.length = unit(0.25, "cm"), 
                                axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = 1)) +   
        guides(size = guide_legend(override.aes = list(color="black",fill="black")), fill="none") + 
        labs(x="Age", y = "Prevalence (%)", title = "Overweight and obesity (BMI>=25)",color=element_blank())
      print(g2)
      
    dev.off()


#### 2. birth cohorts by super region   ####
release <- 9 ## GBD 2021 versions
  
# # Getting location metadata
locs_3 <- get_location_metadata(22, release_id = release)
locs_3 <- locs_3[level==3, location_id]

# # Getting age metadata
ages2 <- get_age_metadata(24, release_id = release)
ages2[,defined := paste0(age_group_years_start,"-",age_group_years_end)]

## Load forecasting data
fhs_ow <- fread('FILEPATH')
fhs_ob <- fread('FILEPATH')
    
fhs_ow <- fhs_ow[age_group_id %in% c(34,6:20,30)]
fhs_ob <- fhs_ob[age_group_id %in% c(34,6:20,30)]
fhs_ow[age_group_name=="80-85",age_group_name:="80+"][age_group_id==30, age_start := 80][age_group_id==30, age_end := 85]
fhs_ob[age_group_name=="80-85",age_group_name:="80+"][age_group_id==30, age_start := 80][age_group_id==30, age_end := 85]

fhs_ow$age_group_name <- factor(fhs_ow$age_group_name, levels = c("2 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                                                                    "30 to 34", "35 to 39", "40 to 44","45 to 49", "50 to 54", "55 to 59", 
                                                                    "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80+"))

fhs_ob$age_group_name <- factor(fhs_ob$age_group_name, levels = c("2 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
                                                                    "30 to 34", "35 to 39", "40 to 44","45 to 49", "50 to 54", "55 to 59", 
                                                                    "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80+"))

all_cohort_data <- data.table()
for(meas in c("ow","ob")){ 
  current <- get(paste0("fhs_",meas))
  current <- current[sex_id!=3]
  
  # calculate birth years (birth_year_end should be used as the start of birth cohort)
  current <- merge(current, ages2, by="age_group_id")
  current[, birth_year_start := year_id - age_group_years_start]
  current[, birth_year_end := year_id - age_group_years_end]
  current[, birth_cohort := paste0(birth_year_end, "-", birth_year_start)]
  
  # keep only 5 year interval for plotting
  current_5 <- current[year_id %in% seq(1990,2050,5)]
  
  # country estimates only
  current_5 <- current_5[location_id==102]
  setnames(current_5, c("mean_prev", "lower_prev", "upper_prev"), c("prevalence", "lower", "upper"))
  
  # collapse by year, sex, age group and SDI group
  current_pop <- current_5[, lapply(.SD, mean), by=c("year_id", "sex_id", "age_group_id"), .SDcols=c("prevalence","lower","upper")]
  
  current_pop <- merge(current_pop, ages2[,.(age_group_id, age_group_name, age_group_years_start, age_group_years_end)], by="age_group_id")
  current_pop[, birth_year_start := year_id - age_group_years_start]
  current_pop[, birth_year_end := year_id - age_group_years_end]
  current_pop[, birth_cohort := paste0(birth_year_end, "-", birth_year_start)]
  current_pop[, sex := ifelse(sex_id==1, "Male", "Female")]

  # plots birth cohort after 1900
  current_pop <- current_pop[birth_year_start>=1900]
  current_pop[age_group_id==235,age_group_name:="80+"][age_group_id==235, age_group_years_start := 80][age_group_id==235, age_group_years_end := 85]

  col_grad <- colorRampPalette(c("#9E0142", "#F46D43", "#FEE08B", "#E6F598", "#66C2A5", "#5E4FA2"), space = "rgb")
  num <- current_pop[age_group_id %in% c(6:20,30:32, 235), birth_year_start] %>% unique %>% length
  
  # plot results using SDI in 2024
  if(meas=="ow"){
    plot_title <- "A: Overweight and obesity (BMI>=25)"
    plot_y <- "Prevalence overweight"
    save_name <- 'FILEPATH'
  } else if(meas=="ob"){
    plot_title <- "A: Obesity (BMI>=30)"
    plot_y <- "Prevalence obese"
    save_name <- 'FILEPATH'
  } 
  
  ## Save a dataset of all data that goes into cohort plots
  all_cohort_data <- rbind(all_cohort_data, current_pop[, measure := meas])
  
  ##Birth cohort by year
  for(s in c("Male","Female")){
    figure_1_2020 <- ggplot()+
      geom_line(data = current_pop[age_group_id %in% c(6:20,30, 235) & sex==s], aes(x = age_group_years_start, y = prevalence*100, group = factor(birth_year_start),
                                                                                      color=factor(birth_year_start)), linetype = 1)+
      geom_point(data = current_pop[age_group_id %in% c(6:20,30, 235)& sex==s], aes(x = age_group_years_start, y = prevalence*100, group = factor(birth_year_start),
                                                                                       color=factor(birth_year_start)), shape=15) +
      scale_colour_manual(values=col_grad(num), name="Birth year",
                          guide = guide_legend(ncol=1), drop = T) +
      theme_classic() +  theme(panel.border = element_rect(fill = NA)) + #plot.title = element_text(hjust = 0.5)
      scale_x_continuous(breaks=seq(10,120,10)) + scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.05)))+
      labs(title=paste0(ifelse(s=="Female", "(B) Women", "(A) Men")), 
           y="Prevalence (%)", x="age")
    
    assign(paste0(meas, "_", s, "_fhs"), figure_1_2020)
  }
  
}

##save dataset
all_cohort_data[measure == "ow", measure:= "Prevalence overweight"][measure == "ob", measure:= "Prevalence obese"]
fwrite(all_cohort_data, paste0('FILEPATH'))

## combine cohort plots together
pdf(paste0('FILEPATH'),width=15,height=10)
  gg2 <- ggarrange(ob_Male_fhs, ob_Female_fhs, nrow = 1, ncol=2, common.legend = T, legend="right")
  gg2 <- ggarrange(ow_Male_fhs, ow_Female_fhs, nrow = 1, ncol=2, common.legend = T, legend="right")
  print(gg2)
dev.off()


# Load in data for tables
ow_xwalk <- get_crosswalk_version('FILEPATH')
ow_xwalk <- ow_xwalk[is_outlier==0]

#### 3. Sources used   ####
ow_counts <- unique(ow_xwalk[, c("nid", "field_citation_value", "diagnostic")])
ow_counts <- ow_counts[, .N, by=c("nid")]
ow_counts <- ow_counts[N>1]

ow_counts2 <- unique(ow_xwalk[, c("nid", "field_citation_value", "location_id")])
ow_counts2[, nat:=0][location_id %in% countries$location_id, nat:=1]
ow_counts2 <- ow_counts2[, .N, by=c("nid","nat")]
ow_counts3 <- ow_counts2[, .N, by=c("nid")]
ow_both <- ow_counts3[N>1, nid]
ow_nat <- ow_counts2[nat==1 & !nid %in% ow_both, nid] 
ow_subnat <- ow_counts2[nat==0 & N>=1 & !nid %in% ow_both, nid]

ow_sources <- unique(ow_xwalk[, .(ss = sum(sample_size)),c("nid", "field_citation_value", "year_start", "year_end", "location_id", "diagnostic")])
ow_sources[nid %in% ow_counts$nid, diagnostic := "measured"]
ow_sources[nid %in% ow_nat, represent := "National"][nid %in% ow_subnat, represent := "State"][nid %in% ow_both, represent := "State + National"]
ow_sources <- merge(ow_sources, locs[, c("location_id","country_name")], all.x=T)
ow_sources[, Years := paste0(year_start, "-", year_end)]
ow_sources <- unique(ow_sources[, .(ss = sum(ss)), c("nid", "field_citation_value", "Years", "country_name","diagnostic","represent")])
ow_sources <- aggregate(data=ow_sources,Years~.,FUN=paste,collapse=",\n") %>% as.data.table()
ow_sources <- ow_sources[order(-Years,nid),]
ow_sources[, field_citation_value := gsub("<.*?>", "", field_citation_value)]
setnames(ow_sources, c("nid", "field_citation_value", "country_name","diagnostic","represent","ss"), 
         c("NID", "Citation", "Country","Data type","Representativeness","Sample size"))
ow_sources <- ow_sources[, c("NID","Citation","Country","Representativeness","Sample size", "Years","Data type")]
fwrite(ow_sources[Country=="USA"], paste0('FILEPATH'))


#### 4. Counts overweight   ####
# Pull in child, adolescent, and adult estimates for 1990, 2021, and 2050
ow_2_14 <- fread('FILEPATH')
ow_2_14 <- rbind(ow_2_14, fread('FILEPATH')[year_id!=2021], fill=T)
ow_15_24 <- fread('FILEPATH')
ow_15_24 <- rbind(ow_15_24, fread('FILEPATH')[year_id!=2021], fill=T)
ow_25up <- fread('FILEPATH')
ow_25up <- rbind(ow_25up, fread('FILEPATH')[year_id!=2021], fill=T)

## Table SM 5
years <- c(1990,2021,2050)
ow_2_14 <- ow_2_14[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","mean_count","lower_count","upper_count")]
ow_15_24 <- ow_15_24[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","mean_count","lower_count","upper_count")]
ow_25up <- ow_25up[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","mean_count","lower_count","upper_count")]

ow_2_14[, mean_count := formatC(round(mean_count,1), format="d", big.mark=",")][, lower_count := formatC(round(lower_count,1), format="d", big.mark=",")][, upper_count := formatC(round(upper_count,1), format="d", big.mark=",")]
ow_15_24[, mean_count := formatC(round(mean_count,1), format="d", big.mark=",")][, lower_count := formatC(round(lower_count,1), format="d", big.mark=",")][, upper_count := formatC(round(upper_count,1), format="d", big.mark=",")]
ow_25up[, mean_count := formatC(round(mean_count,1), format="d", big.mark=",")][, lower_count := formatC(round(lower_count,1), format="d", big.mark=",")][, upper_count := formatC(round(upper_count,1), format="d", big.mark=",")]


## Pivot wider
ow_2_14_w <- pivot_wider(ow_2_14, names_from = year_id, values_from = c(mean_count,lower_count, upper_count)) %>% as.data.table()
ow_15_24_w <- pivot_wider(ow_15_24, names_from = year_id, values_from = c(mean_count,lower_count, upper_count)) %>% as.data.table()
ow_25up_w <- pivot_wider(ow_25up, names_from = year_id, values_from = c(mean_count,lower_count, upper_count)) %>% as.data.table()

##Format 14-24 age group
comb_14_24 <- merge(ow_15_24_w[Sex=="Male", -c("Sex","age_group.x", "age_group.y","age_group")], ow_15_24_w[Sex=="Female", -c("Sex","age_group.x", "age_group.y","age_group")], by=c("location_name"))
comb_14_24[location_name =="United States of America", location_name :="USA"]
comb_14_24 <- rbind(comb_14_24[location_name =="USA",], comb_14_24[location_name !="USA",])
comb_14_24[, ci_1990.x := paste0("(", lower_count_1990.x, "-", upper_count_1990.x, ")")][, ci_1990.y := paste0("(", lower_count_1990.y, "-", upper_count_1990.y, ")")]
comb_14_24[, ci_2021.x := paste0("(", lower_count_2021.x, "-", upper_count_2021.x, ")")][, ci_2021.y := paste0("(", lower_count_2021.y, "-", upper_count_2021.y, ")")]
comb_14_24[, ci_2050.x := paste0("(", lower_count_2050.x, "-", upper_count_2050.x, ")")][, ci_2050.y := paste0("(", lower_count_2050.y, "-", upper_count_2050.y, ")")]
comb_14_24 <- comb_14_24[, c("location_name","mean_count_1990.x","ci_1990.x","mean_count_2021.x","ci_2021.x","mean_count_2050.x","ci_2050.x",
                             "mean_count_1990.y","ci_1990.y","mean_count_2021.y","ci_2021.y","mean_count_2050.y","ci_2050.y")]

##Format 25+ age group
comb_25 <- merge(ow_25up_w[Sex=="Male", -c("Sex","age_group.x", "age_group.y","age_group")], ow_25up_w[Sex=="Female", -c("Sex","age_group.x", "age_group.y","age_group")], by=c("location_name"))
comb_25[location_name =="United States of America", location_name :="USA"]
comb_25 <- rbind(comb_25[location_name =="USA",], comb_25[location_name !="USA",])
comb_25[, ci_1990.x := paste0("(", lower_count_1990.x, "-", upper_count_1990.x, ")")][, ci_1990.y := paste0("(", lower_count_1990.y, "-", upper_count_1990.y, ")")]
comb_25[, ci_2021.x := paste0("(", lower_count_2021.x, "-", upper_count_2021.x, ")")][, ci_2021.y := paste0("(", lower_count_2021.y, "-", upper_count_2021.y, ")")]
comb_25[, ci_2050.x := paste0("(", lower_count_2050.x, "-", upper_count_2050.x, ")")][, ci_2050.y := paste0("(", lower_count_2050.y, "-", upper_count_2050.y, ")")]
comb_25 <- comb_25[, c("location_name","mean_count_1990.x","ci_1990.x","mean_count_2021.x","ci_2021.x","mean_count_2050.x","ci_2050.x",
                       "mean_count_1990.y","ci_1990.y","mean_count_2021.y","ci_2021.y","mean_count_2050.y","ci_2050.y")]

## Save tables
write.xlsx(comb_14_24, 'FILEPATH')
write.xlsx(comb_25, 'FILEPATH')

#### 5. Prevalence overweight by 5-year age group ####

# Pull in child, adolescent, and adult estimates for 1990, 2021, and 2050
ow_2_24 <- fread('FILEPATH')
ow_25up <- fread('FILEPATH')

ow_2_24[, prev:=paste0(round(mean_prev*100,1), " (", round(lower_prev*100, 1), "-", round(upper_prev*100,1), ")")]
ow_25up[, prev:=paste0(round(mean_prev*100,1), " (", round(lower_prev*100, 1), "-", round(upper_prev*100,1), ")")]

years <- c(1990,2021,2050)
ow_2_24 <- ow_2_24[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","prev","age_group_id")]
ow_25up <- ow_25up[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","prev","age_group_id")]

## Pivot wider
ow_2_24_w <- pivot_wider(ow_2_24, names_from = year_id, values_from = prev) %>% as.data.table()
ow_25up_w <- pivot_wider(ow_25up, names_from = year_id, values_from = prev) %>% as.data.table()

## Add in age group information
ow_2_24_w <- merge(ow_2_24_w, ages[,c("age_group_id", "age_label")], by="age_group_id", all.x=T)
ow_25up_w <- merge(ow_25up_w, ages[,c("age_group_id", "age_label")], by="age_group_id", all.x=T)
ow_2_24_w[, age_label2:= gsub("years.*", "", age_label)]
ow_25up_w[, age_label2:= gsub("years.*", "", age_label)]

comb <- rbind(ow_2_24_w,ow_25up_w, fill=T)
comb <- comb[order(age_label)]
comb <- rbind(comb[location_name =="United States of America",], comb[location_name !="United States of America",])

title <- "SM Table 4: Prevalence of overweight and obesity by 5-year age group and sex in 1990, 2021 and 2050 at the national level, across 50 states and Washington DC"

for(ag in levels(comb$age_label)){
  df <- comb[age_label==ag]
  
  comb2 <- merge(df[Sex=="Male", -c("Sex","age_group_id","age_label","age_label2")], df[Sex=="Female", -c("Sex","age_group_id","age_label","age_label2")], by=c("location_name"))
  comb2[location_name =="United States of America", location_name :="USA"]
  comb2 <- rbind(comb2[location_name =="USA",], comb2[location_name !="USA",])  
  
  try({kable(comb2, align = 'lrrrrrr', caption=title,
             col.names = c("Location","1990","2021","2050","1990","2021","2050"))  %>%
      kable_classic() %>% add_header_above(c(paste0(unique(df$age_label2), "years"), "Male" = 3, "Female" = 3)) %>%
      column_spec(c(1,4), border_right = T) %>%
      kable_styling("striped", font_size=10) %>%
      save_kable(paste0('FILEPATH'), density=500)
  })
}


#### 6. Prevalence obese by 5-year age group ####
# Pull in child, adolescent, and adult estimates for 1990, 2021, and 2050
ob_2_24 <- fread('FILEPATH')
ob_25up <- fread('FILEPATH')

ob_2_24[, prev:=paste0(round(mean_prev*100,1), " (", round(lower_prev*100, 1), "-", round(upper_prev*100,1), ")")]
ob_25up[, prev:=paste0(round(mean_prev*100,1), " (", round(lower_prev*100, 1), "-", round(upper_prev*100,1), ")")]

years <- c(1990,2021,2050)
ob_2_24 <- ob_2_24[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","prev","age_group_id")]
ob_25up <- ob_25up[year_id %in% years & Sex!="Both", c("location_name","Sex","year_id","prev","age_group_id")]

## Pivot wider
ob_2_24_w <- pivot_wider(ob_2_24, names_from = year_id, values_from = prev) %>% as.data.table()
ob_25up_w <- pivot_wider(ob_25up, names_from = year_id, values_from = prev) %>% as.data.table()

## Add in sage group information
ob_2_24_w <- merge(ob_2_24_w, ages[,c("age_group_id", "age_label")], by="age_group_id", all.x=T)
ob_25up_w <- merge(ob_25up_w, ages[,c("age_group_id", "age_label")], by="age_group_id", all.x=T)
ob_2_24_w[, age_label2:= gsub("years.*", "", age_label)]
ob_25up_w[, age_label2:= gsub("years.*", "", age_label)]

comb <- rbind(ob_2_24_w,ob_25up_w, fill=T)
comb <- comb[order(age_label)]
comb <- rbind(comb[location_name =="United States of America",], comb[location_name !="United States of America",])

title <- "SM Table 5: Prevalence of obesity by 5-year age group and sex in 1990, 2021 and 2050 at the national level, across 50 states and Washington DC"

for(ag in levels(comb$age_label)){
  df <- comb[age_label==ag]
  
  comb2 <- merge(df[Sex=="Male", -c("Sex","age_group_id","age_label","age_label2")], df[Sex=="Female", -c("Sex","age_group_id","age_label","age_label2")], by=c("location_name"))
  comb2[location_name =="United States of America", location_name :="USA"]
  comb2 <- rbind(comb2[location_name =="USA",], comb2[location_name !="USA",])  
  
  try({kable(comb2, align = 'lrrrrrr', caption=paste0(title), booktabs=T,
             col.names = c("Location","1990","2021","2050","1990","2021","2050"))  %>%
      kable_classic() %>% add_header_above(c(paste0(unique(df$age_label2), "years"), "Male" = 3, "Female" = 3)) %>% 
      column_spec(c(1,4), border_right = T) %>%
      kable_styling("striped", font_size=10) %>%
      save_kable(paste0('FILEPATH'), density=500)
  })

}
