################################################################################
## Global map with insets for smaller regions
## Description: mapping function that takes the following arguments:
##
##  REQUIRED INPUT:
##    data        ->  A data frame or table with two variables, 'location_id' and 'mapvar'.
##    limits      ->  A vector that defines the bins for the map. This should be
##                    the minimum value, each of the break points in ascending order
##                    and then the maximum value. By default, the bins are currently
##                    assigned such that a value equal to a breakpoint is included in
##                    the bin where that breakpoint is the lower bound.
##
##  OPTIONAL INPUT:
##    sub_nat ->      Prints map with capstone level subnationals("capstone"), topic paper level subnations ("topic"), all subnationals for vetting ("all"), or without subnationals("none").
##                    Defaults to map with capstone level subnationals
##    legend ->      Logical. Prints map with or without legend. Defaults to map with legend.
##    inset ->       Logical. Prints map with or without inset at bottom. Defaults to map without insets
##    labels      ->  Vector of the labels to use in the legend for each group.
##                    If this is not specified, these are assigned automatically
##                    based on the provided values in 'limits'.
##    pattern     ->  A vector, with length equal to the number of bins, of
##                    of patterns to be used for shading in countries. 0 = no
##                    shading (the default), 1=horizontal lines, 2=positively
##                    sloping lines at a 45 degree angle, 3 = vertical lines,
##                    4 = negatively sloping lines at a 45 degree angle.
##    col         ->  Either a string specifying the name of a color palette from
##                    ColorBrewer () or a vector of colors.
##    col.reverse ->  Logical. Should the order of the colors be reversed (this
##                    may be useful when specifying a palette from ColorBrewer)
##    na.color    ->  The color to be used for countries not in the dataset
##                    (defaults to grey)
##    title       ->  A string specifying the title (defaults to "")
##    title.cex   ->  Multiplier for the font size of the title
##    fname       ->  The filename for saving the map. Currently supports '.tif'
##                    '.eps', and '.pdf'. If nothing is specifed, the map is
##                    printed to screen so long as there is not a pdf device
##                    already open (if there is a pdf device already open, the
##                    map is saved to this device; this is to allow for making
##                    pdfs with multiple figures using the onefile=T option in
##                    the pdf function)
##    legend.title -> The title for the legend
##    legend.columns -> The number of columns in the lengend. Defaults to 1 for
##                    7 or less groups and 2 otherwise.
##    legend.cex   -> Multiplier for the size of the legend
##    legend.shift -> Shift the legend (vector of size 2: horizontal shift and
##                    vertical shift)
##  OVERRIDES col:
##    midpoint     -> Set a midpoint of your data to set neutral colors
##    col1         -> If midpoint is set, sets a color scheme for below midopints
##    col2         -> If midpoint is set, sets a color scheme for above midopints
################################################################################


gbd_map <- function(data, limits, sub_nat="capstone", legend=TRUE, inset=TRUE,
                    labels=NULL,
                    pattern=NULL,
                    col="RdYlBu", col.reverse=FALSE, na.color = "grey20",
                    title="", fname=NULL,
                    legend.title=NULL, legend.columns = NULL, legend.cex=1, legend.shift=c(0,0)) {

  ## load libraries
  library(RColorBrewer)
  library(sf)

  ## set default for legend and inset and sub_nat
  if(missing(legend)) {
    legend==TRUE
  }

  if(missing(inset)) {
    inset==TRUE
  }

  if(missing(sub_nat)) {
    sub_nat=="capstone"
  }

  ## test for consistency & give warnings
  if (length(limits)-1 != length(labels) & !is.null(labels)) stop("length(limits) must equal length(labels) + 1")
  if (length(limits)-1 != length(col) & length(col)>1) stop("number of colors does not match number of groups")
  if (length(limits)-1 != length(pattern) & !is.null(pattern)) stop("number of patterns does not match number of groups")
  if (!"location_id" %in% names(data) | !"mapvar" %in% names(data)) stop("data is missing location_id or mapvar variables")

  ## set how device opens
  if (is.null(fname)) {
    if (!"pdf" %in% names(dev.list())) dev.new(width=7.5,height=4.15)
  } else if (grepl(".tif", fname, fixed=T)) {
    tiff(fname, width=7.5, height=4.15, units="in", res=1000)
  } else if (grepl(".eps", fname, fixed=T)) {
    postscript(fname)
  } else if (grepl(".pdf", fname, fixed=T)) {
    pdf(fname, width=14, height=8)
  } else {
    stop("invalid file type: must be .tif, .eps, or .pdf")
  }

  data <- as.data.frame(data)
  data$loc_id <- data$location_id
  data$location_id <- NULL

  ## load shapefile and prep data and inset data
  load(paste('FILEPATH'))
  main_data <- merge(data, map[,c("loc_id", "ID")], by="loc_id", sort= TRUE, all.y=T)

  if(sub_nat=="none") {
    load(paste('FILEPATH'))
    nat_map <- merge(data, nosubmap[,c("loc_id", "ID")],by="loc_id", sort= TRUE, all.y=T)
  }

  if(sub_nat=="topic") {
    load(paste('FILEPATH'))
    topic_map <- merge(data, topicmap[,c("loc_id", "ID")],by="loc_id", sort= TRUE, all.y=T)
  }

  if(sub_nat=="topic_wIND") {
    load(paste('FILEPATH'))
    topic_map_wIND <- merge(data, topicmap_wIND[,c("loc_id", "ID")],by="loc_id", sort= TRUE, all.y=T)
  }
  
  if(sub_nat=="all") {
    load(paste('FILEPATH'))
    all_map <- merge(data, allmap[,c("loc_id", "ID")],by="loc_id", sort= TRUE, all.y=T)
  }

  load(paste('FILEPATH'))
  if(sub_nat=="none") caribbean_central_america_background <- caribbean_central_america_background[caribbean_central_america_background$level ==  3,] #these lines control subnational plotting in the insets
  caribbean_central_america_data <- merge(data, caribbean_central_america_background[, c("loc_id", "ID")], by = "loc_id", sort = TRUE, all.y=T)
    
  load(paste('FILEPATH'))
  w_africa_data <- merge(data, w_africa_background[, c("loc_id", "ID")], by="loc_id", all.y=TRUE, sort= TRUE)

  load(paste('FILEPATH'))
  if(sub_nat=="none") persian_gulf_background <- persian_gulf_background[persian_gulf_background$level ==  3,]
  persian_gulf_data <- merge(data, persian_gulf_background[, c("loc_id", "ID")], by="loc_id", sort= TRUE, all.y=T)
    
  load(paste('FILEPATH'))
  if(sub_nat=="none" | sub_nat =="topic" | sub_nat == "topic_wIND" | sub_nat == "capstone") balkan_background <- balkan_background[balkan_background$level ==  3,]
  balkan_data <- merge(data, balkan_background[, c("loc_id", "ID")], by="loc_id", all.y=T, sort= TRUE)
 
  load(paste('FILEPATH'))
  if(sub_nat=="none") se_asia_background <- se_asia_background[se_asia_background$level ==  3,]
  se_asia_data <- merge(data, se_asia_background[, c("loc_id", "ID")], by="loc_id", all.y=T, sort= TRUE)
  
  load(paste('FILEPATH'))
  if(sub_nat =="topic" | sub_nat == "topic_wIND" | sub_nat == "capstone") n_europe_background <- n_europe_background[n_europe_background$parent_id !=  51,]
  if(sub_nat == "none") n_europe_background <- n_europe_background[n_europe_background$level ==  3,]
  n_europe_data <- merge(data, n_europe_background[, c("loc_id", "ID")], by="loc_id", all.y=T, sort= TRUE)
 
  load(paste('FILEPATH'))
  e_mediterranean_data <- merge(data, e_mediterranean_background[, c("loc_id", "ID")], by="loc_id", sort= TRUE, all.y=T)


  ## assign colors and patterns
  main_data$color <- main_data$pattern <- NA
  n <- length(limits) - 1

  if (length(col)==1) {
    if (brewer.pal.info[col,1] >= n) mapcolors <- brewer.pal(n, col)
    else mapcolors <- colorRampPalette(brewer.pal(brewer.pal.info[col,1], col))(n)
  } else {
    mapcolors <- col
  }
  if (col.reverse) mapcolors <- mapcolors[n:1]
  if (is.null(pattern)) pattern <- rep(0, n)

  for (g in 1:n) {
    ii <- (main_data$mapvar >= limits[g] & main_data$mapvar <= limits[g+1])
    main_data$color[ii] <- mapcolors[g]
    main_data$pattern[ii] <- pattern[g]
  }

  main_data$color[is.na(main_data$color)] <- na.color
  main_data$pattern[is.na(main_data$pattern)] <- 0

  main_data$density <- ifelse(main_data$pattern==0, 0, 30)
  main_data$angle <- as.numeric(as.character(factor(main_data$pattern, levels=1:4, label=c(0,45,90,135))))

  ### assign colors and patterns for nosubmap
  if(sub_nat=="none") {
    for (g in 1:n) {
      ii <- (nat_map$mapvar >= limits[g] & nat_map$mapvar <= limits[g+1])
      nat_map$color[ii] <- mapcolors[g]
      nat_map$pattern[ii] <- pattern[g]
    }

    nat_map$color[is.na(nat_map$color)] <- na.color
    nat_map$pattern[is.na(nat_map$pattern)] <- 0

    nat_map$density <- ifelse(nat_map$pattern==0, 0, 30)
    nat_map$angle <- as.numeric(as.character(factor(nat_map$pattern, levels=1:4, label=c(0,45,90,135))))
  }

  if(sub_nat=="topic") {
    for (g in 1:n) {
      ii <- (topic_map$mapvar >= limits[g] & topic_map$mapvar <= limits[g+1])
      topic_map$color[ii] <- mapcolors[g]
      topic_map$pattern[ii] <- pattern[g]
    }

    topic_map$color[is.na(topic_map$color)] <- na.color
    topic_map$pattern[is.na(topic_map$pattern)] <- 0

    topic_map$density <- ifelse(topic_map$pattern==0, 0, 30)
    topic_map$angle <- as.numeric(as.character(factor(topic_map$pattern, levels=1:4, label=c(0,45,90,135))))
  }
  
  if(sub_nat=="topic_wIND") {
    for (g in 1:n) {
      ii <- (topic_map_wIND$mapvar >= limits[g] & topic_map_wIND$mapvar <= limits[g+1])
      topic_map_wIND$color[ii] <- mapcolors[g]
      topic_map_wIND$pattern[ii] <- pattern[g]
    }
    
    topic_map_wIND$color[is.na(topic_map_wIND$color)] <- na.color
    topic_map_wIND$pattern[is.na(topic_map_wIND$pattern)] <- 0
    
    topic_map_wIND$density <- ifelse(topic_map_wIND$pattern==0, 0, 30)
    topic_map_wIND$angle <- as.numeric(as.character(factor(topic_map_wIND$pattern, levels=1:4, label=c(0,45,90,135))))
  }

  if(sub_nat=="all") {
    for (g in 1:n) {
      ii <- (all_map$mapvar >= limits[g] & all_map$mapvar <= limits[g+1])
      all_map$color[ii] <- mapcolors[g]
      all_map$pattern[ii] <- pattern[g]
    }

    all_map$color[is.na(all_map$color)] <- na.color
    all_map$pattern[is.na(all_map$pattern)] <- 0

    all_map$density <- ifelse(all_map$pattern==0, 0, 30)
    all_map$angle <- as.numeric(as.character(factor(all_map$pattern, levels=1:4, label=c(0,45,90,135))))
  }
  ## assign colors and patterns and location for inset

  caribbean_central_america_data$color <- caribbean_central_america_data$pattern <- NA
  for (g in 1:n) {
    ii <- (caribbean_central_america_data$mapvar >= limits[g] & caribbean_central_america_data$mapvar <= limits[g+1])
    caribbean_central_america_data$color[ii] <- mapcolors[g]
    caribbean_central_america_data$pattern[ii] <- pattern[g]
  }
  
  caribbean_central_america_data$color[is.na(caribbean_central_america_data$color)] <- na.color
  caribbean_central_america_data$pattern[is.na(caribbean_central_america_data$pattern)] <- 0
  
  caribbean_central_america_data$density <- ifelse(caribbean_central_america_data$pattern==0, 0, 30)
  caribbean_central_america_data$angle <- as.numeric(as.character(factor(caribbean_central_america_data$pattern, levels=1:4, label=c(0,45,90,135))))
  
  balkan_data$color <- balkan_data$pattern <- NA
  for (g in 1:n) {
    ii <- (balkan_data$mapvar >= limits[g] & balkan_data$mapvar <= limits[g+1])
    balkan_data$color[ii] <- mapcolors[g]
    balkan_data$pattern[ii] <- pattern[g]
  }
  
  balkan_data$color[is.na(balkan_data$color)] <- na.color
  balkan_data$pattern[is.na(balkan_data$pattern)] <- 0
  
  balkan_data$density <- ifelse(balkan_data$pattern==0, 0, 30)
  balkan_data$angle <- as.numeric(as.character(factor(balkan_data$pattern, levels=1:4, label=c(0,45,90,135))))
  
  n_europe_data$color <- n_europe_data$pattern <- NA
  
  for (g in 1:n) {
    ii <- (n_europe_data$mapvar >= limits[g] & n_europe_data$mapvar <= limits[g+1])
    n_europe_data$color[ii] <- mapcolors[g]
    n_europe_data$pattern[ii] <- pattern[g]
  }
  
  n_europe_data$color[is.na(n_europe_data$color)] <- na.color
  n_europe_data$pattern[is.na(n_europe_data$pattern)] <- 0
  
  n_europe_data$density <- ifelse(n_europe_data$pattern==0, 0, 30)
  n_europe_data$angle <- as.numeric(as.character(factor(n_europe_data$pattern, levels=1:4, label=c(0,45,90,135))))
  
  persian_gulf_data$color <- persian_gulf_data$pattern <- NA
  for (g in 1:n) {
    ii <- (persian_gulf_data$mapvar >= limits[g] & persian_gulf_data$mapvar <= limits[g+1])
    persian_gulf_data$color[ii] <- mapcolors[g]
    persian_gulf_data$pattern[ii] <- pattern[g]
  }
  
  persian_gulf_data$color[is.na(persian_gulf_data$color)] <- na.color
  persian_gulf_data$pattern[is.na(persian_gulf_data$pattern)] <- 0
  
  persian_gulf_data$density <- ifelse(persian_gulf_data$pattern==0, 0, 30)
  persian_gulf_data$angle <- as.numeric(as.character(factor(persian_gulf_data$pattern, levels=1:4, label=c(0,45,90,135))))
  
  e_mediterranean_data$color <- e_mediterranean_data$pattern <- NA
  
  for (g in 1:n) {
    ii <- (e_mediterranean_data$mapvar >= limits[g] & e_mediterranean_data$mapvar <= limits[g+1])
    e_mediterranean_data$color[ii] <- mapcolors[g]
    e_mediterranean_data$pattern[ii] <- pattern[g]
  }
  
  e_mediterranean_data$color[is.na(e_mediterranean_data$color)] <- na.color
  e_mediterranean_data$pattern[is.na(e_mediterranean_data$pattern)] <- 0
  
  e_mediterranean_data$density <- ifelse(e_mediterranean_data$pattern==0, 0, 30)
  e_mediterranean_data$angle <- as.numeric(as.character(factor(e_mediterranean_data$pattern, levels=1:4, label=c(0,45,90,135))))
  
  se_asia_data$color <- se_asia_data$pattern <- NA
  for (g in 1:n) {
    ii <- (se_asia_data$mapvar >= limits[g] & se_asia_data$mapvar <= limits[g+1])
    se_asia_data$color[ii] <- mapcolors[g]
    se_asia_data$pattern[ii] <- pattern[g]
  }
  
  se_asia_data$color[is.na(se_asia_data$color)] <- na.color
  se_asia_data$pattern[is.na(se_asia_data$pattern)] <- 0
  
  se_asia_data$density <- ifelse(se_asia_data$pattern==0, 0, 30)
  se_asia_data$angle <- as.numeric(as.character(factor(se_asia_data$pattern, levels=1:4, label=c(0,45,90,135))))
  
  w_africa_data$color <- w_africa_data$pattern <- NA
  
  for (g in 1:n) {
    ii <- (w_africa_data$mapvar >= limits[g] & w_africa_data$mapvar <= limits[g+1])
    w_africa_data$color[ii] <- mapcolors[g]
    w_africa_data$pattern[ii] <- pattern[g]
  }
  
  w_africa_data$color[is.na(w_africa_data$color)] <- na.color
  w_africa_data$pattern[is.na(w_africa_data$pattern)] <- 0
  
  w_africa_data$density <- ifelse(w_africa_data$pattern==0, 0, 30)
  w_africa_data$angle <- as.numeric(as.character(factor(w_africa_data$pattern, levels=1:4, label=c(0,45,90,135))))
  
  ####disputed locations
  load(paste('FILEPATH'))
  disputed_data <- merge(data, disputed[,c("ADM0_NAME", "ID", "DISP_AREA")], all.y=T)

  load(paste('FILEPATH'))
  disputed_border$linetype <- ifelse(disputed_border$DISP_AREA==1,2,1)
  disputed_border_non <- disputed_border[disputed_border$DISP_AREA==0,]
  disputed_border <-disputed_border[disputed_border$DISP_AREA==1,]


  ## generate labels if necessary
  if (is.null(labels)) {
    for (g in 1:n) {
      labels <- c(labels, paste(limits[g], " to ", limits[g+1]))
    }
  }

  # graphics.off()
  par("mar")
  par(mar=c(1,1,1,1))

  ### plot if sub nat & inset are true

  if(sub_nat=="capstone")
  {

    if(inset==TRUE) {
      par(fig = c(.07, .31, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25)
      plot(st_geometry(caribbean_central_america_background), col= caribbean_central_america_data$color[order(caribbean_central_america_data$ID)])
      mtext("Caribbean and Central America", side= 3, line = 0, adj = 0.5)
      rect(-92, 7, -58, 27)

      par(fig = c(.30, .45, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(persian_gulf_background), col=  persian_gulf_data$color[order(persian_gulf_data$ID)])
      mtext("Persian Gulf",side= 3, line = 0, adj = 0.5)
      rect(46, 21, 56, 31)

      par(fig = c(.78, .85, .15, .27),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(w_africa_background), col= w_africa_data$color[order(w_africa_data$ID)])
      mtext("West Africa", side= 3, line = 0, adj = 0.5)
      rect(-17.52, 7, -9, 16)

      par(fig = c(.42, .63, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, xpd= NA, new=TRUE)
      plot(st_geometry(balkan_background), col= balkan_data$color[order(balkan_data$ID)])
      mtext("The Balkans", side= 3, line = 0, adj = 0.5)
      rect(13.5, 34.809, 32, 50)

      par(fig = c(.84, .93, 0.15, .27),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(e_mediterranean_background), col= e_mediterranean_data$color[order(e_mediterranean_data$ID)])
      mtext("Eastern \n Mediterranean", side= 3, line = 0, adj = 0.5)
      rect(31.630, 28.855, 38.630, 35.855)

      par(fig = c(.60, .79, 0, .25),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(se_asia_background), col= se_asia_data$color[order(se_asia_data$ID)])
      mtext("Southeast Asia", side= 3, line = 0, adj = 0.5)
      rect(95, -9.643, 119, 9)

      par(fig = c(.78, .92, 0, .132),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(n_europe_background), col= n_europe_data$color[order(n_europe_data$ID)])
      mtext("Northern Europe", side= 3, line = 0, adj = 0.5, padj = .50)
      rect(5, 48, 27.89, 59)

      ### plot main map
      par(fig  = c(0, 1, .25, 1), lwd=0.25, mai=c(.02,.02,.4,.02), new = TRUE)
      plot(st_geometry(map), col=main_data$color[order(main_data$ID)], cex=0.5)
      plot(st_geometry(map), density=main_data$density[order(main_data$ID)], angle=main_data$angle[order(main_data$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
    ###plot if subnat = true and inset is false
    if (inset==FALSE)
    {
      ###plot only mainmap
      par(lwd=0.1, mai=c(.02,.02,.4,.02))
      plot(st_geometry(map), col=main_data$color[order(main_data$ID)], cex=0.5)
      plot(st_geometry(map), density=main_data$density[order(main_data$ID)], angle=main_data$angle[order(main_data$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
  }
  ###plot  if sub nat is none and inset is true
  if  (sub_nat=="none") {
    if(inset==TRUE) {
      par(fig = c(.07, .31, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25)
      plot(st_geometry(caribbean_central_america_background), col= caribbean_central_america_data$color[order(caribbean_central_america_data$ID)])
      mtext("Caribbean and Central America", side= 3, line = 0, adj = 0.5, padj = 1., cex =  0.8)
      rect(-92, 7, -58, 27)

      par(fig = c(.30, .45, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(persian_gulf_background), col=  persian_gulf_data$color[order(persian_gulf_data$ID)])
      mtext("Persian Gulf",side= 3, line = 0, adj = 0.5, padj = 1., cex =  0.8)
      rect(46, 21, 56, 31)

      par(fig = c(.78, .85, .14, .26),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(w_africa_background), col= w_africa_data$color[order(w_africa_data$ID)])
      mtext("West Africa", side= 3, line = 0, adj = 0.5, padj = .75, cex =  0.8)
      rect(-17.52, 7, -9, 16)

      par(fig = c(.42, .63, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, xpd= NA, new=TRUE)
      plot(st_geometry(balkan_background), col= balkan_data$color[order(balkan_data$ID)])
      mtext("The Balkans", side= 3, line = 0, adj = 0.5, padj = 1., cex =  0.8)
      rect(13.5, 34.809, 32, 50)

      par(fig = c(.84, .93, 0.14, .26),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(e_mediterranean_background), col= e_mediterranean_data$color[order(e_mediterranean_data$ID)])
      mtext("Eastern \n Mediterranean", side= 3, line = 0, adj = 0.5, padj = .25, cex =  0.8)
      rect(31.630, 28.855, 38.630, 35.855)

      par(fig = c(.60, .79, 0, .25),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(se_asia_background), col= se_asia_data$color[order(se_asia_data$ID)])
      mtext("Southeast Asia", side= 3, line = 0, adj = 0.5, padj = 1., cex =  0.8)
      rect(95, -9.643, 119, 9)

      par(fig = c(.78, .92, 0, .132),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(n_europe_background), col= n_europe_data$color[order(n_europe_data$ID)])
      mtext("Northern Europe", side= 3, line = 0, adj = 0.5, padj = 0.7, cex = 0.8)
      rect(5, 48, 27.89, 59)

      ### plot main map
      par(fig  = c(0, 1, .25, 1), lwd=0.1, mai=c(.02,.02,.4,.02), new = TRUE)
      plot(st_geometry(nosubmap), col=nat_map$color[order(nat_map$ID)], cex=0.5)
      plot(st_geometry(nosubmap), density=nat_map$density[order(nat_map$ID)], angle=nat_map$angle[order(nat_map$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
    ###plot if subnat is  none and so is inset
    if (inset==FALSE)
    {
      ###plot only mainmap
      par(lwd=0.1, mai=c(.02,.02,.4,.02))
      plot(st_geometry(nosubmap), col=nat_map$color[order(nat_map$ID)], cex=0.5)
      plot(st_geometry(nosubmap), density=nat_map$density[order(nat_map$ID)], angle=nat_map$angle[order(nat_map$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
  }
  ###plot for topic papers
  if  (sub_nat=="topic") {
    if(inset==TRUE) {
      par(fig = c(.07, .31, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25)
      plot(st_geometry(caribbean_central_america_background), col= caribbean_central_america_data$color[order(caribbean_central_america_data$ID)])
      mtext("Caribbean and central America", side= 3, line = 0, adj = 0.5)
      rect(-92, 7, -58, 27)

      par(fig = c(.30, .45, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(persian_gulf_background), col=  persian_gulf_data$color[order(persian_gulf_data$ID)])
      mtext("Persian Gulf",side= 3, line = 0, adj = 0.5)
      rect(46, 21, 56, 31)

      par(fig = c(.78, .85, .14, .26),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(w_africa_background), col= w_africa_data$color[order(w_africa_data$ID)])
      mtext("West Africa", side= 3, line = 0, adj = 0.5)
      rect(-17.52, 7, -9, 16)

      par(fig = c(.42, .63, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, xpd= NA, new=TRUE)
      plot(st_geometry(balkan_background), col= balkan_data$color[order(balkan_data$ID)])
      mtext("The Balkans", side= 3, line = 0, adj = 0.5)
      rect(13.5, 34.809, 32, 50)

      par(fig = c(.84, .93, 0.14, .26),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(e_mediterranean_background), col= e_mediterranean_data$color[order(e_mediterranean_data$ID)])
      mtext("Eastern \n Mediterranean", side= 3, line = 0, adj = 0.5)
      rect(31.630, 28.855, 38.630, 35.855)

      par(fig = c(.60, .79, 0, .25),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(se_asia_background), col= se_asia_data$color[order(se_asia_data$ID)])
      mtext("Southeast Asia", side= 3, line = 0, adj = 0.5)
      rect(95, -9.643, 119, 9)

      par(fig = c(.78, .92, 0, .132),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(n_europe_background), col= n_europe_data$color[order(n_europe_data$ID)])
      mtext("Northern Europe", side= 3, line = 0, adj = 0.50, padj = .50)
      rect(5, 48, 27.89, 59)

      ### plot main map
      par(fig  = c(0, 1, .25, 1), lwd=0.1, mai=c(.02,.02,.4,.02), new = TRUE)
      plot(st_geometry(topicmap), col=topic_map$color[order(topic_map$ID)], cex=0.5)
      plot(st_geometry(topicmap), density=topic_map$density[order(topic_map$ID)], angle=topic_map$angle[order(topic_map$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
    ###plot if subnat is  false and so is inset
    if (inset==FALSE)
    {
      ###plot only mainmap
      par(lwd=0.1, mai=c(.02,.02,.4,.02))
      plot(st_geometry(topicmap), col=topic_map$color[order(topic_map$ID)], cex=0.5)
      plot(st_geometry(topicmap), density=topic_map$density[order(topic_map$ID)], angle=topic_map$angle[order(topic_map$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
  }
  
  ###plot for topic papers w/IND
  if  (sub_nat=="topic_wIND") {
    if(inset==TRUE) {
      par(fig = c(.07, .31, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25)
      plot(st_geometry(caribbean_central_america_background), col= caribbean_central_america_data$color[order(caribbean_central_america_data$ID)])
      mtext("Caribbean and central America", side= 3, line = 0, adj = 0.5)
      rect(-92, 7, -58, 27)
      
      par(fig = c(.30, .45, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(persian_gulf_background), col=  persian_gulf_data$color[order(persian_gulf_data$ID)])
      mtext("Persian Gulf",side= 3, line = 0, adj = 0.5)
      rect(46, 21, 56, 31)
      
      par(fig = c(.78, .85, .14, .26),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(w_africa_background), col= w_africa_data$color[order(w_africa_data$ID)])
      mtext("West Africa", side= 3, line = 0, adj = 0.5)
      rect(-17.52, 7, -9, 16)
      
      par(fig = c(.42, .63, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, xpd= NA, new=TRUE)
      plot(st_geometry(balkan_background), col= balkan_data$color[order(balkan_data$ID)])
      mtext("The Balkans", side= 3, line = 0, adj = 0.5)
      rect(13.5, 34.809, 32, 50)
      
      par(fig = c(.84, .93, 0.14, .26),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(e_mediterranean_background), col= e_mediterranean_data$color[order(e_mediterranean_data$ID)])
      mtext("Eastern \n Mediterranean", side= 3, line = 0, adj = 0.5)
      rect(31.630, 28.855, 38.630, 35.855)
      
      par(fig = c(.60, .79, 0, .25),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(se_asia_background), col= se_asia_data$color[order(se_asia_data$ID)])
      mtext("Southeast Asia", side= 3, line = 0, adj = 0.5)
      rect(95, -9.643, 119, 9)
      
      par(fig = c(.78, .92, 0, .132),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(n_europe_background), col= n_europe_data$color[order(n_europe_data$ID)])
      mtext("Northern Europe", side= 3, line = 0, adj = 0.50, padj = .50)
      rect(5, 48, 27.89, 59)
      
      ### plot main map
      par(fig  = c(0, 1, .25, 1), lwd=0.1, mai=c(.02,.02,.4,.02), new = TRUE)
      plot(st_geometry(topicmap_wIND), col=topic_map_wIND$color[order(topic_map_wIND$ID)], cex=0.5)
      plot(st_geometry(topicmap_wIND), density=topic_map_wIND$density[order(topic_map_wIND$ID)], angle=topic_map_wIND$angle[order(topic_map_wIND$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
    ###plot if subnat is  false and so is inset
    if (inset==FALSE)
    {
      ###plot only mainmap
      par(lwd=0.1, mai=c(.02,.02,.4,.02))
      plot(st_geometry(topicmap_wIND), col=topic_map_wIND$color[order(topic_map_wIND$ID)], cex=0.5)
      plot(st_geometry(topicmap_wIND), density=topic_map_wIND$density[order(topic_map_wIND$ID)], angle=topic_map_wIND$angle[order(topic_map_wIND$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
  }
  

  ###plot for vetting papers
  if  (sub_nat=="all") {
    if(inset==TRUE) {
      par(fig = c(.07, .31, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25)
      plot(st_geometry(caribbean_central_america_background), col= caribbean_central_america_data$color[order(caribbean_central_america_data$ID)])
      mtext("Caribbean and central America", side= 3, line = 0, adj = 0.5)
      rect(-92, 7, -58, 27)

      par(fig = c(.30, .45, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(persian_gulf_background), col=  persian_gulf_data$color[order(persian_gulf_data$ID)])
      mtext("Persian Gulf",side= 3, line = 0, adj = 0.5)
      rect(46, 21, 56, 31)

      par(fig = c(.78, .85, .14, .26),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(w_africa_background), col= w_africa_data$color[order(w_africa_data$ID)])
      mtext("West Africa", side= 3, line = 0, adj = 0.5)
      rect(-17.52, 7, -9, 16)

      par(fig = c(.42, .63, 0, .25), mar=c(0, 0, 0, 0), lwd=0.25, xpd= NA, new=TRUE)
      plot(st_geometry(balkan_background), col= balkan_data$color[order(balkan_data$ID)])
      mtext("The Balkans", side= 3, line = 0, adj = 0.5)
      rect(13.5, 34.809, 32, 50)

      par(fig = c(.84, .93, 0.14, .26),mar=c(0, 0, 0, 0), lwd=0.25, new=TRUE)
      plot(st_geometry(e_mediterranean_background), col= e_mediterranean_data$color[order(e_mediterranean_data$ID)])
      mtext("Eastern \n Mediterranean", side= 3, line = 0, adj = 0.5)
      rect(31.630, 28.855, 38.630, 35.855)

      par(fig = c(.60, .79, 0, .25),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(se_asia_background), col= se_asia_data$color[order(se_asia_data$ID)])
      mtext("Southeast Asia", side= 3, line = 0, adj = 0.5)
      rect(95, -9.643, 119, 9)

      par(fig = c(.78, .92, 0, .132),mar=c(0, 0, 0, 0),lwd=0.25, new=TRUE)
      plot(st_geometry(n_europe_background), col= n_europe_data$color[order(n_europe_data$ID)])
      mtext("Northern Europe", side= 3, line = 0, adj = 0.5, padj = .50)
      rect(5, 48, 27.89, 59)

      ### plot main map
      par(fig  = c(0, 1, .25, 1), lwd=0.1, mai=c(.02,.02,.4,.02), new = TRUE)
      plot(st_geometry(allmap), col=all_map$color[order(all_map$ID)], cex=0.5)
      plot(st_geometry(allmap), density=all_map$density[order(all_map$ID)], angle=all_map$angle[order(all_map$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
    ###plot if subnat is  false and so is inset
    if (inset==FALSE)
    {
      ###plot only mainmap
      par(lwd=0.1, mai=c(.02,.02,.4,.02))
      plot(st_geometry(allmap), col=all_map$color[order(all_map$ID)], cex=0.5)
      plot(st_geometry(allmap), density=all_map$density[order(all_map$ID)], angle=all_map$angle[order(all_map$ID)], add=T)
      plot(st_geometry(disputed),border="white", lty = 1, cex=0.5, lwd=0.3, add= T)
      plot(st_geometry(disputed_border), col="white", cex=0.5,lty=1,lwd=0.3, add=T)
      plot(st_geometry(disputed),border="grey29", lty = 2, cex=0.5, lwd=0.2,  add=T)
      plot(st_geometry(disputed_border), col="grey29", cex=0.5,lty=2,lwd=0.2, add=T)
      plot(st_geometry(disputed_border_non),cex=0.5,lty=1, col = "black", lwd=0.25, add=T)
    }
  }

  ## plot legend
  angle <- as.numeric(as.character(factor(pattern, levels=1:4, label=c(0,45,90,135))))
  legend <- as.logical(legend)
  if (legend==TRUE) {
    if (n <= 6) {
      if (is.null(legend.columns)) legend.columns <- 1
      legend(-170+legend.shift[1], 10+legend.shift[2], labels, fill=mapcolors, border=mapcolors,
             cex=0.75*legend.cex, bg="white", box.col="white", ncol=legend.columns, title=legend.title)
      if (sum(pattern)>0) legend(-170+legend.shift[1], 10+legend.shift[2], labels, fill=NA, border=NA, density=ifelse(pattern==0, 0, 60), angle=angle,
                                 cex=0.75*legend.cex, box.col="white", ncol=legend.columns, title=legend.title)
    } else {
      if (is.null(legend.columns)) legend.columns <- 2
      legend(-180+legend.shift[1], 10+legend.shift[2], labels, fill=mapcolors, border=mapcolors,
             cex=0.6*legend.cex, bg="white", box.col="white", ncol=legend.columns, title=legend.title)
      if (sum(pattern)>0) legend(-180+legend.shift[1], 10+legend.shift[2], labels, fill=NA, border=NA, density=ifelse(pattern==0, 0, 60), angle=angle,
                                 cex=0.6*legend.cex, box.col="white", ncol=legend.columns, title=legend.title)
    } }

  title(main=title, cex=0.95)

  ## close device
  if(!is.null(fname)) dev.off()
  if(sub_nat=="all") warning("This map is for internal viewing only, please do not share results externally")

}
