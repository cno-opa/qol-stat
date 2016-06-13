#
# depts.R
#
# =================
#
# Makes charts based on department-submitted, not source, data
#
# =================
#

#plot fn
plotCumulativeLine <- function(data, measure, target, cum = TRUE, lower_title = TRUE) {
  d <- filter(data, variable == measure, !is.na(value))
  d$month <- month(as.Date(as.yearmon(d$date)), label = TRUE)
  d$year <- as.character(year(as.Date(as.yearmon(d$date))))
  
  #subset to most recent three years
  yrs_n <- length(unique(d$year))
  yrs <- unique(d$year)[(yrs_n - 2):yrs_n]
  d <- filter(d, year %in% yrs)
  
  #title stuff
  if(lower_title == TRUE) {
    title_measure <- tolower(measure)
  } else {
    title_measure <- measure
  }
  
  #add target
  if(hasArg(target)) {
    for(i in 1:12) {
      m <- month(i, label = TRUE)
      r <- list(NA, measure, (target/ 12), m, "Target")
      d <- rbind(d, r)
    }
  }
  
  #turn target into cumulative sum if cum == FALSE
  if(cum == FALSE) {
    r <- cumsum(d$value[d$year == "Target"])
    d$value[d$year == "Target"] <- r
  }
  
  d <- group_by(d, year) %>%
    mutate (annual_cum_sum = cumsum(value))
  
  # changes made to fix subset function deprecation 
  
  d1 = data.frame()
  #assign(d1, d[d$date == r_period,], envir=globalenv())
  
  d1 = d[d$date == r_period,]
  d1 <- na.omit(d1)
  assign("d1", d[d$date == r_period,], envir=globalenv())
  
  #custom color scale
  n <- length(unique(d$year)) - 2
  gray_highlight <- c(colorRampPalette(c("gray77", "gray55"))(n), darkBlue, "grey10")
  names(gray_highlight) <- unique(d$year)
  gray_highlight_scale <- scale_colour_manual(name = "year", values = gray_highlight)
  
  gray_highlight_no_target <- c(colorRampPalette(c("gray77", "gray55"))(n + 1), darkBlue)
  names(gray_highlight_no_target) <- unique(d$year)
  gray_highlight_scale_no_target <- scale_colour_manual(name = "year", values = gray_highlight_no_target)
  
  rep_n <- length(unique(d$year)) - 1
  
  
  if(cum == TRUE & hasArg(target)) {
    p <- lineOPA(d, "month", "annual_cum_sum", paste("Cumulative number of", title_measure), group = "year", linetype = c(rep("solid", length(unique(data$year)) - 1), "dashed")) +
      gray_highlight_scale +
      geom_text(data = d1, aes(month,annual_cum_sum, label= format(annual_cum_sum, big.mark = ",", scientific = FALSE)), size = 3, colour = "grey33", hjust = -0.2, vjust = -1)
  } else if(cum == TRUE & !hasArg(target)) {
    p <- lineOPA(d, "month", "annual_cum_sum", paste("Cumulative number of", title_measure), group = "year") +
      gray_highlight_scale_no_target +
      geom_text(data = d1, aes(month,annual_cum_sum,label = format(annual_cum_sum, big.mark = ",", scientific = FALSE)), size = 3, colour = "grey33", hjust = -0.2, vjust = -1)
  } else if(cum == FALSE & !hasArg(target)) {
    p <- lineOPA(d, "month", "value", paste("Cumulative number of", title_measure), group = "year") +
      gray_highlight_scale_no_target +
      geom_text(data = d1, aes(month,value, label = format(value, big.mark = ",", scientific = FALSE)), size = 3, colour = "grey33", hjust = -0.2, vjust = -1)
  } else if(cum == FALSE & hasArg(target)) {
    p <- lineOPA(d, "month", "value", paste("Cumulative number of", title_measure), group = "year", linetype = c(rep("solid", length(unique(data$year)) - 1), "dashed")) +
      gray_highlight_scale +
      geom_text(data = d1, aes(month,value, label = format(value, big.mark = ",", scientific = FALSE)), size = 3, colour = "grey33", hjust = -0.2, vjust = -1)
  }
  
  p <- buildChart(p)
  ggsave( file = paste("./output/dept", measure, "cumulative.png"), plot = p, width = 7.42, height = 5.75 )
}

clusterBarYear <- function(data, measure) {
  d <- filter(data, variable == measure)
  d$month <- month(as.Date(as.yearmon(d$date)), label = TRUE)
  d$year <- as.character(year(as.Date(as.yearmon(d$date))))
  
  colors_highlight <- c(colorRampPalette( c("gray77", "gray55") )(length(unique(d$year)) - 1), darkBlue)
  
  p <- barOPA(d, "month", "value", measure, fill = "year", position = "dodge") +
    scale_fill_manual(values = colors_highlight)
  p <- buildChart(p)
  
  ggsave( file = paste("./output/", measure, "cluster bar.png"), plot = p, width = 7.42, height = 5.75 )
}

calcKPIs <- function(data, r_period) {
  load("./data/kpis.Rdata")
  
  #subset data YTD based on r_period
  month_range <- unique(data$date[year(dateFromYearMon(data$date)) == year(dateFromYearMon(r_period))])
  d <- filter(data, date %in% month_range)
  
  summary_all_depts <- group_by(d, variable) %>%
    summarise(sum = sum(value))
  
  kpi_potholes_filled <- filter(summary_all_depts, variable == "Potholes filled")$sum
  kpi_outages_restored <- filter(summary_all_depts, variable == "Outages restored")$sum
  kpi_catch_basins <- filter(summary_all_depts, variable == "Catch basins cleaned")$sum
  kpi_acres_mowed <- filter(summary_all_depts, variable == "Acres mowed")$sum
  kpi_trims <- filter(summary_all_depts, variable == "Tree trims and removals")$sum
  kpi_dumping <- filter(summary_all_depts, variable == "Illegal dumping sites cleared")$sum
  kpi_abo <- filter(summary_all_depts, variable == "ABO filings")$sum
  kpi_mosquito <- filter(summary_all_depts, variable == "Avg days to mosquito request")$sum/length(month_range)
  kpi_rodent <- filter(summary_all_depts, variable == "Avg days to rodent request")$sum/length(month_range)
  
  kpis <- rbind(
    kpis,
    c("DPW - Potholes filled", kpi_potholes_filled),
    c("DPW - Percent of streetlights functioning", filter(d, variable == "Streetlights functioning", date == r_period)$value),
    c("DPW - Number of outages restored", kpi_outages_restored),
    c("DPW - Number of permanent traffic signs installed", filter(d, variable == "Traffic signs installed", date == r_period)$value),
    c("DPW - Number of street name signs installed", filter(d, variable == "Street name signs installed", date == r_period)$value),
    c("DPW - Number of catch basins cleaned", kpi_catch_basins),
    c("DPW - Percent of catch basins cleaned", kpi_catch_basins/68092),
    c("PP - Number of acres mowed", kpi_acres_mowed),
    c("PP - Number of tree trims and removals", kpi_trims),
    c("SAN - Number of illegal dumping sites cleared", kpi_dumping),
    c("SAN - Percentage of households registered for recycling", filter(d, variable == "Houses registered for recycling", date == r_period)$value/136746),
    c("LAW - Number of ABO filings", kpi_abo),
    c("MTCB - Average days to close mosquito service request", kpi_mosquito),
    c("MTCB - Average days to close rodent request", kpi_rodent)
  )
  
  return(kpis)
}

#load and melt
data1 <- read.csv("./data/depts.csv", header = TRUE)
data <- melt(data1, id.vars = "date")
data$date <- as.factor(as.yearmon(data$date))
data$variable <- gsub(".", " ", data$variable, fixed = TRUE)

#subset to two years back from reporting period
# date_max <- length(levels(data$date))
# date_min <- length(levels(data$date)) - 24
# date_range <- levels(data$date)[date_min:date_max]
# data <- filter(data, date %in% date_range)

#plot calls

theme_set(theme_opa( base_size = 12 ))

plotCumulativeLine(data, "Potholes filled", 70000)
plotCumulativeLine(data, "Traffic signs installed", 3000, cum = FALSE)
plotCumulativeLine(data, "Street name signs installed", 1500, cum = FALSE)
plotCumulativeLine(data, "Catch basins cleaned", 4500)
plotCumulativeLine(data, "Tree trims and removals", 3000)
plotCumulativeLine(data, "Emergency tree requests")
plotCumulativeLine(data, "Illegal dumping sites cleared", 2000)
plotCumulativeLine(data, "ABO filings", 100, lower_title = FALSE)

#percent of streetlights functioning
p_sl <- lineOPA(filter(data, variable == "Streetlights functioning"), "date", "value", "Percent of street lights functioning", percent = TRUE, labels = "percent(value)", lab.size = 2.5 )
p_sl <- buildChart(p_sl)
ggsave( file = "./output/dept percent of streetlights on.png", plot = p_sl, width = 7.42, height = 5.75 )

#number of streetlight outages restored
p_outage <- barOPA(filter(data, variable == "Outages restored"), "date", "value", "Number of streetlight outages restored", labels = "value")
p_outage <- buildChart(p_outage)
ggsave( file = "./output/streetlight outages restored.png", plot = p_outage, width = 7.42, height = 5.75 )

#acres mowed
clusterBarYear(data, "Acres mowed")

#bandit signs
p_bandit <- lineOPA(filter(data, variable == "PP sign removal" | variable == "Sanitation sign removal", !is.na(value)), "date", "value", "Bandit signs removed", group = "variable", legend.labels = c("Parks and Parkways", 'Sanitation'), labels = "value")
p_bandit <- buildChart(p_bandit)
ggsave( file = "./output/bandit sign removal.png", plot = p_bandit, width = 7.42, height = 5.75 )

#sanitation inspections
p_san_insp <- lineOPA(filter(data, variable == "Sanitation inspections"), "date", "value", "Sanitation inspections", labels = "value")
p_san_insp <- buildChart(p_san_insp)
ggsave( file = "./output/sanitation inspections.png", plot = p_san_insp, width = 7.42, height = 5.75 )

#tires removed
p_tires <- lineOPA(filter(data, variable == "Tires removed"), "date", "value", "Tires removed", labels = "value")
p_tires <- buildChart(p_tires)
ggsave( file = "./output/tires removed.png", plot = p_tires, width = 7.42, height = 5.75 )

#houses registered for recycling
p_recycling <- lineOPA(filter(data, variable == "Houses registered for recycling", !is.na(value)), "date", "value", "Houses registered for recycling", labels = "value")
p_recycling <- buildChart(p_recycling)
ggsave( file = "./output/houses recycling.png", plot = p_recycling, width = 7.42, height = 5.75 )

#avg days to mosquito request
p_mosq <- lineOPA(filter(data, variable == "Avg days to mosquito request", !is.na(value)), "date", "value", "Average days to close mosquito request", labels = "value")
p_mosq <- buildChart(p_mosq)
ggsave( file = "./output/avg days mosquito.png", plot = p_mosq, width = 7.42, height = 5.75 )

#avg days to rodent request
p_rod <- lineOPA(filter(data, variable == "Avg days to rodent request", !is.na(value)), "date", "value", "Average days to close rodent request", labels = "value")
p_rod <- buildChart(p_rod)
ggsave( file = "./output/avg days rodent.png", plot = p_rod, width = 7.42, height = 5.75 )

#nopd complaints and summonses
p_nopd <- lineOPA(filter(data, variable == "Complaints received by NOPD" | variable == "Summonses issued by NOPD", !is.na(value)), "date", "value", "Complaints received and summonses issued by NOPD quality of life officers", group = "variable", labels = "value")
p_nopd <- buildChart(p_nopd)
ggsave( file = "./output/nopd.png", plot = p_nopd, width = 7.42, height = 5.75 )

#calc kpis and print
kpis <- calcKPIs(data, r_period)
print(kpis)
write.csv(kpis, "./data/kpi_table.csv")