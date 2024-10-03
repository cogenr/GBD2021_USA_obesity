# US mapping functions
# Map USA, states, with DC inset
# Cleaned for GBD2021 USA obesity publication 10/2024

smap <- readRDS("FILEPATH")
cmap <- readRDS("FILEPATH")
library(RColorBrewer)


us_mapping = function(dat0, valname = 'val', superpose = NULL, sex=2, year=2021, agetext = '', map0=smap, legname = NULL,
                      states=NULL, main = NULL, legend = TRUE, boundcol='white', scalecols = c('lightcyan','darkblue'), 
                      logt=FALSE, maxAnchor = NULL, minAnchor = NULL, greybound = 0.,  nacol = 'white', sub = NULL, tsize = 13)
{
  ##### Arguments
  # dat0: data.table with estimates. Must have IHME-coded variables named location_id, measure_id, metric_id; 
  #            race, sex, year (with or without '_id'); 
  #            either cause_id and acause or rei_id and rei; and val
  # valname: name of y variable to plot (string)
  
  # rei, cause: rei_id or cause_id to be mapped. If 'rei' is provided then 'cause' will be ignored.
  # sex, year: sex_id and year(_id) to plot
  # race: race_id per USHD codes (1-all, 2-Latine, 4-Black, 5-White, 6-AIAN, 7-AsianPI)
  #         * Note: if dataset has no race or race_id variable, then function will autogenerate it as code 1 (all)
  # ages, agetext: age_group_id and its description to go in the plot title (unless specific title is provided)
  # map0: the map shapefile, expected to be in sf (simple features) format.
  # superpose: an optional additional map to superpose boundaries from (typically state map when 'map0' is county)
  # vartype, metric: measure_id and metric_id
  # states: optional, names of states to plot (map will include *only those states). If left as NULL the entire US will map
  
  # main: plot title. If left as NULL, will be generated from sex/age/raceth/year info, plus acause/rei
  # legend: whether to plot a color-scale legend
  # boundcol: the color for boundary between polygons. Use NA to get zero-width boundaries.
  # scalecols: the colors for the continuous scale of val. Must be at least 2 for bottom and top.
  # logt: whether to log-transform val (more relevant when using counts)
  # zeroAnchor: whether the color scale is anchored at zero (rather than at the minimum value). Recommended when plotting rates.
  # topAnchor: an optional anchor for the top of the scale. Useful when plotting several maps w/the same legend.
  # greybound: the value below which polygons will be greyed out. Default 0.
  # sub: optional subtitle
  # tsize: relative text size. Default 13 (pt)  
  
  require(plyr)
  require(data.table)
  require(ggplot2)
  require(sf)
  require(tools)
  theme_set(theme_void(tsize))
  
  dat = copy(dat0)
  
  if(valname != 'val') 
  {
    if ('val' %in% names(dat)) setnames(dat, 'val', 'zzz')
    setnames(dat, valname, 'val')
  }
  
  # Auto-convert numerical code (user specifies raceth as string)
  tmp = dat[sex_id==sex & year_id==year, ]
  
  # Can do just a selection of states!
  if(!is.null(states)) map0 = map0[map0$state_name %in% states,]
  
  map0$measure = tmp$val[match(map0$location_id, tmp$location_id)]
  map0$measure[map0$measure < greybound] = NA
  ymax = ifelse(is.null(maxAnchor), ceiling(max(map0$measure, na.rm=TRUE)), maxAnchor)
  ymin = ifelse(is.null(minAnchor), floor(min(map0$measure, na.rm=TRUE)), minAnchor)
  
  ### Input-dependent titles and legends:
  
  if(is.null(main)) main = paste(valname, 'among', agetext,
                                 mapvalues(tmp$sex_id[1], 1:3, c('Men', 'Women', 'All Sexes'), warn_missing = FALSE),',', year)
  ## Add on 2 letter state code
  map0 <- merge(map0, locs[,c("location_id","local_id")], by="location_id")
  map0$local_id <- gsub("US-","",map0$local_id)
  map0$color_text <- ifelse(map0$measure<40, "white","black")
  map0$local_id[9] <- NA
  
  p1 <- ggplot(map0) + geom_sf(color=boundcol, aes(fill=measure), size=0.1)  +
    geom_sf_text(aes(label=local_id, color="black"), size=6) +
    scale_color_manual(values=c("black"="black","white"="white")) +
    labs(fill = legname, title = main, subtitle = sub) + 
    scale_fill_gradientn(colors=scalecols, trans=ifelse(logt,'log10','identity'), limits = c(ymin, ymax), na.value = nacol) +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(!is.null(superpose)) p1 <- p1 + geom_sf(data=superpose, color='black', fill = NA)
  
  if(!legend) p1 <- p1 + guides(fill='none')
  
  return(p1)
}


### Same function, includes label for each state
us_mapping_label = function(dat0, valname = 'val', superpose = NULL, sex=2, year=2021, agetext = '', map0=smap, legname = NULL,
                      states=NULL, main = NULL, legend = TRUE, boundcol='white', scalecols = c('lightcyan','darkblue'), 
                      logt=FALSE, maxAnchor = NULL, minAnchor = NULL, greybound = 0.,  nacol = 'white', sub = NULL, tsize = 13)
{
  ##### Arguments
  # dat0: data.table with estimates. Must have IHME-coded variables named location_id, measure_id, metric_id; 
  #            race, sex, year (with or without '_id'); 
  #            either cause_id and acause or rei_id and rei; and val
  # valname: name of y variable to plot (string)
  
  # rei, cause: rei_id or cause_id to be mapped. If 'rei' is provided then 'cause' will be ignored.
  # sex, year: sex_id and year(_id) to plot
  # race: race_id per USHD codes (1-all, 2-Latine, 4-Black, 5-White, 6-AIAN, 7-AsianPI)
  #         * Note: if dataset has no race or race_id variable, then function will autogenerate it as code 1 (all)
  # ages, agetext: age_group_id and its description to go in the plot title (unless specific title is provided)
  # map0: the map shapefile, expected to be in sf (simple features) format.
  # superpose: an optional additional map to superpose boundaries from (typically state map when 'map0' is county)
  # vartype, metric: measure_id and metric_id
  # states: optional, names of states to plot (map will include *only those states). If left as NULL the entire US will map
  
  # main: plot title. If left as NULL, will be generated from sex/age/raceth/year info, plus acause/rei
  # legend: whether to plot a color-scale legend
  # boundcol: the color for boundary between polygons. Use NA to get zero-width boundaries.
  # scalecols: the colors for the continuous scale of val. Must be at least 2 for bottom and top.
  # logt: whether to log-transform val (more relevant when using counts)
  # zeroAnchor: whether the color scale is anchored at zero (rather than at the minimum value). Recommended when plotting rates.
  # topAnchor: an optional anchor for the top of the scale. Useful when plotting several maps w/the same legend.
  # greybound: the value below which polygons will be greyed out. Default 0.
  # sub: optional subtitle
  # tsize: relative text size. Default 13 (pt)  
  
  require(plyr)
  require(data.table)
  require(ggplot2)
  require(sf)
  require(tools)
  theme_set(theme_void(tsize))
  
  dat = copy(dat0)
  
  if(valname != 'val') # very brute-forcey
  {
    if ('val' %in% names(dat)) setnames(dat, 'val', 'zzz')
    setnames(dat, valname, 'val')
  }
  
  # Auto-convert numerical code (user specifies raceth as string)
  tmp = dat[sex_id==sex & year_id==year, ]
  
  # Can do just a selection of states!
  if(!is.null(states)) map0 = map0[map0$state_name %in% states,]
  
  map0$measure = tmp$val[match(map0$location_id, tmp$location_id)]
  map0$measure[map0$measure < greybound] = NA
  ymax = ifelse(is.null(maxAnchor), ceiling(max(map0$measure, na.rm=TRUE)), maxAnchor)
  ymin = ifelse(is.null(minAnchor), floor(min(map0$measure, na.rm=TRUE)), minAnchor)
  
  ### Input-dependent titles and legends:
  
  if(is.null(main)) main = paste(valname, 'among', agetext,
                                 mapvalues(tmp$sex_id[1], 1:3, c('Men', 'Women', 'All Sexes'), warn_missing = FALSE),',', year)
  ## Add on 2 letter state code
  map0 <- merge(map0, locs[,c("location_id","local_id")], by="location_id")
  map0$local_id <- gsub("US-","",map0$local_id)
  map0$color_text <- ifelse(map0$measure<40, "white","black")
  map0$local_id[9] <- NA
  
  p1 <- ggplot(map0) + geom_sf(color=boundcol, aes(fill=measure), size=0.1)  +
    geom_sf_text(aes(label=local_id), color="black", size=6) + 
    scale_color_manual(values=c("black"="black","white"="white")) +
    labs(fill = legname, title = main, subtitle = sub) + 
    scale_fill_gradientn(colors=scalecols, trans=ifelse(logt,'log10','identity'), limits = c(ymin, ymax), na.value = nacol) +
    theme(plot.title = element_text(hjust = 0.5))
  
  if(!is.null(superpose)) p1 <- p1 + geom_sf(data=superpose, color='black', fill = NA)
  
  if(!legend) p1 <- p1 + guides(fill='none')
  
  return(p1)
}
