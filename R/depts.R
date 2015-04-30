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
plotCumulativeLine <- function(data, measure, target) {
  d <- filter(data, variable == measure)
  d$month <- month(as.Date(as.yearmon(d$date)), label = TRUE)
  d$year <- as.character(year(as.Date(as.yearmon(d$date))))

  #add target
  if(hasArg(target)) {
    for(i in 1:12) {
      m <- month(i, label = TRUE)
      r <- list(NA, measure, (target/ 12), m, "Target")
      d <- rbind(d, r)
    }
  }

  d <- group_by(d, year) %>%
       mutate (annual_cum_sum = cumsum(value))

  #custom color scale
  n <- length(unique(d$year)) - 2
  gray_highlight <- c(colorRampPalette(c("gray77", "gray55"))(n), darkBlue, red)
  names(gray_highlight) <- unique(d$year)
  gray_highlight_scale <- scale_colour_manual(name = "year", values = gray_highlight)

  p <- lineOPA(d, "month", "annual_cum_sum", paste("Cumulative number of", tolower(measure)), group = "year") +
       gray_highlight_scale +
       geom_text(data = filter(d, date == r_period), aes(label = format(annual_cum_sum, big.mark = ",", scientific = FALSE), y = annual_cum_sum), size = 4, colour = "grey33", hjust = -0.2, vjust = -1)

  p <- buildChart(p)
  ggsave( file = paste("./output/dept", measure, "cumulative.png"), plot = p, width = 7.42, height = 5.75 )
}

clusterBarYear <- function(data, measure) {
  d <- filter(data, variable == measure)
  d$month <- month(as.Date(as.yearmon(d$date)), label = TRUE)
  d$year <- as.character(year(as.Date(as.yearmon(d$date))))

  p <- barOPA(d, "month", "value", measure, fill = "year", position = "dodge")
  p <- buildChart(p)

  ggsave( file = paste("./output/", measure, "cluster bar.png"), plot = p, width = 7.42, height = 5.75 )
}

#load and melt
data <- read.csv("./data/depts.csv", header = TRUE)
data <- melt(data, id.vars = "date")
data$date <- as.factor(as.yearmon(data$date))
data$variable <- gsub(".", " ", data$variable, fixed = TRUE)

#plot calls

theme_set(theme_opa())

plotCumulativeLine(data, "Potholes filled", 40000)
plotCumulativeLine(data, "Traffic signs installed", 20000)
plotCumulativeLine(data, "Street name signs installed", 1000)
plotCumulativeLine(data, "Catch basins cleaned", 3000)
plotCumulativeLine(data, "Tree trims and removals", 3000)
plotCumulativeLine(data, "Emergency tree removals")
plotCumulativeLine(data, "Illegal dumping sites cleared", 2000)
plotCumulativeLine(data, "ABO filings", 250)

#percent of streetlights functioning
p_sl <- lineOPA(filter(data, variable == "Streetlights functioning"), "date", "value", "Percent of street lights functioning", percent = TRUE, labels = "percent(value)")
p_sl <- buildChart(p_sl)
ggsave( file = "./output/dept percent of streetlights on.png", plot = p_sl, width = 7.42, height = 5.75 )

#number of streetlight outages restored
p_outage <- barOPA(filter(data, variable == "Outages restored"), "date", "value", "Number of streetlight outages restored", labels = "value")
p_outage <- buildChart(p_outage)
ggsave( file = "./output/streetlight outages restored.png", plot = p_outage, width = 7.42, height = 5.75 )

#acres mowed
clusterBarYear(data, "Acres mowed")

#bandit signs
p_bandit <- lineOPA(filter(data, variable == "PP sign removal" | variable == "Sanitation sign removal", !is.na(value)), "date", "value", "Bandit sign removal", group = "variable", legend.labels = c("Parks and Parkways", 'Sanitation'), labels = "value")
p_bandit <- buildChart(p_bandit)
ggsave( file = "./output/bandit sign removal.png", plot = p_bandit, width = 7.42, height = 5.75 )

#sanitation inspections
p_san_insp <- lineOPA(filter(data, variable == "Sanitation inspections"), "date", "value", "Sanitation inspections", labels = "value")
p_san_insp <- buildChart(p_san_insp)
ggsave( file = "./output/sanitation inspections.png", plot = p_san_insp, width = 7.42, height = 5.75 )

#tires removed
p_tires <- barOPA(filter(data, variable == "Tires removed"), "date", "value", "Tires removed", labels = "value")
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
p_rod <- lineOPA(filter(data, variable == "Avg days to rodent request", !is.na(value)), "date", "value", "Average days to close mosquito request", labels = "value")
p_rod <- buildChart(p_rod)
ggsave( file = "./output/avg days rodent.png", plot = p_rod, width = 7.42, height = 5.75 )

#nopd complaints and summonses
p_nopd <- lineOPA(filter(data, variable == "Complaints received by NOPD" | variable == "Summonses issued by NOPD", !is.na(value)), "date", "value", "Complaints received and summonses issued by NOPD quality of life officers", group = "variable", labels = "value")
p_nopd <- buildChart(p_nopd)
ggsave( file = "./output/nopd.png", plot = p_nopd, width = 7.42, height = 5.75 )
