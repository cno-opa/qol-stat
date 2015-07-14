#
# 311.R
#
# =================
#
# Calculates 311 service request stats from 311 source data
#
# =================
#

#clean
cleanSource <- function(data) {
  names(data) <- slugify(names(data))
  data$x_1 <- NULL
  data$x_2 <- NULL
  data$x_3 <- NULL
  data$x_4 <- NULL
  data$x_5 <- NULL
  data$x_6 <- NULL
  data$x_7 <- NULL
  data$x_8 <- NULL
  data$open_dt <- mdy(data$open_dt)
  data$closed_dt <- mdy(data$closed_dt)
  data$month_start <- as.factor(as.yearmon(data$open_dt))
  data$month_end <- as.factor(as.yearmon(data$closed_dt))
  data$age__calendar <- as.numeric(as.character(data$age__calendar))
  return(data)
}

#calculate summary tables
makeSummary <- function(data, service_request) {

  countOpen <- function(data, month) {
    u <- dateFromYearMon(month)
    x <- filter(data, open_dt <= u, closed_dt > u | is.na(closed_dt))
    return(nrow(x))
  }

  countNet <- function(data, month) {
    o <- nrow(filter(data, as.character(month_start) == as.character(month)))
    c <- nrow(filter(data, as.character(month_end) == as.character(month)))
    return( o - c )
  }

  countOpened <- function(data, month) {
    o <- nrow(filter(data, as.character(month_start) == as.character(month)))
    return(o)
  }

  countClosed <- function(data, month) {
    c <- nrow(filter(data, as.character(month_end) == as.character(month)))
    return(c)
  }

  d <- filter(data, type == service_request)
  month_range <- unique(d$month_start)

  output <- data.frame(type = service_request, date = month_range)
  output$open <- sapply(output$date, countOpen, data = d)
  output$Opened <- sapply(output$date, countOpened, data = d)
  output$Closed <- sapply(output$date, countClosed, data = d)
  output$net <- sapply(output$date, countNet, data = d)
  output$shade <- ifelse(output$net > 0, "bad","good")
  output <- melt(output, id.vars = c("type", "date", "shade"))

  return(output)
}

#calculate number of cases closed within target (in days) by month
inTarget <- function(data, month, service_request, target) {
  u <- dateFromYearMon(month)
  l <- dateFromYearMon(month, eom = FALSE)
  d <- filter(data, type == service_request, closed_dt >= l & closed_dt <= u)

  p <- nrow(d[d$age__calendar <= target,])/nrow(d)
  result <- data.frame(type = service_request, date = month, target = p)
  return(result)
}

#calculate and save kpis
calcKPIs <- function(data, r_period) {
  #get YTD based on r_period and date request was closed
  d <- filter(data, closed_dt >= ymd(paste(year(dateFromYearMon(r_period)), "01", "01", sep = "-")))

  #percent of streetlight requests closed within 90 days
  kpi_lights_90_days <- nrow(filter(d, type == "Street Light", age__calendar <= 90))/nrow(filter(d, type == "Street Light"))

  #percent of abandoned vehicle requests closed in 30 days
  kpi_vehicles_30_days <- nrow(filter(d, type == "Abandoned Vehicle Reporting/Removal", age__calendar <= 30))/nrow(filter(d, type == "Abandoned Vehicle Reporting/Removal"))

  #percent of illegal dumping requests closed in 30 days
  kpi_dumping_30_days <- nrow(filter(d, type == "Illegal Dumping Reporting", age__calendar <= 30))/nrow(filter(d, type == "Illegal Dumping Reporting"))

  kpis <- rbind(
            c("DPW - Percent of 311 street light requests closed in 90 days", kpi_lights_90_days),
            c("DPW - Percent of 311 abandoned vehicle requests closed in 30 days", kpi_vehicles_30_days),
            c("SAN - Percent of 311 illegal dumping requests closed in 30 days", kpi_dumping_30_days)
          )

  save(kpis, file = "./data/kpis.Rdata")
}

#plot
plot311NetLog <- function(data, service_request) {

  d <- filter(data, type == service_request)

  #filter for last two years by yearmon factor
  range_u <- length(levels(d$date))
  range_l <- range_u - 24
  date_range <- levels(d$date)[range_l:range_u]

  d <- filter(d, date %in% date_range)

  p_line <- lineOPA(filter(d, variable == "open"), "date", "value", title = paste(service_request, "service requests open at end of month"), labels = "format(value, big.mark = \",\", scientific = FALSE)")
  #p_bar <- barOPA(filter(d, variable == "net"), "date", "value", title = paste(service_request, "service requests net per month"), fill = "shade", labels = "format(value, big.mark = \",\", scientific = FALSE)") +
          #  scale_y_continuous(breaks = 0) +
          #  good_bad_scale +
          #  theme(axis.text.y = element_blank(), legend.position = "none")

  p_bar <- barOPA(filter(d, variable == "Opened" | variable == "Closed"), "date", "value", title = paste(service_request, "service requests net per month"), fill = "variable", position = "identity") + scale_fill_manual(values = alpha(c("red", "blue"), .3))

  #p_net_line <- lineOPA(filter(d, variable == "Opened" | variable == "Closed"), "date", "value", title = paste(service_request, "service requests opened and closed per month"), group = "variable", labels = "value")

  p_line <- buildChart(p_line)
  #p_net_line <- buildChart(p_net_line)
  p_bar <- buildChart(p_bar)

  #remove any backslashes that will break save call
  service_request <- gsub("/", " ", service_request, fixed = TRUE)

  ggsave( file = paste("./output/311", service_request, "backlog.png"), plot = p_line, width = 7.42, height = 5.75)
  ggsave( file = paste("./output/311", service_request, "opened and closed.png"), plot = p_bar, width = 7.42, height = 5.75)
}

plot311Target <- function(data, service_request, target) {
  d <- filter(data, type == service_request)

  #filter for last two years by yearmon factor
  range_u <- length(levels(d$date))
  range_l <- range_u - 24
  date_range <- levels(d$date)[range_l:range_u]

  d <- filter(d, date %in% date_range)

  #remove any backslashes that will break save call
  service_request <- gsub("/", " ", service_request, fixed = TRUE)

  p <- lineOPA(d, "date", "target", paste("Percent of", service_request, "resolved in", target, "days"), percent = TRUE, labels = "percent(target)")
  p <- buildChart(p)
  ggsave( file = paste("./output/311", service_request, "in target.png"), plot = p, width = 7.42, height = 5.75 )
}

mapNewSR <- function(data, service_request) {
  d <- filter(data, type == service_request, month_start == r_period)
  dsp <- toSpatialPoints(d, "x", "y")

  #remove any backslashes that will break save call
  service_request <- gsub("/", " ", service_request, fixed = TRUE)

  map <- mapOPAPoints(dsp, "x", "y", map_source = "google", map_type = "terrain-lines", fill = "tomato", size =2.5, title = paste("New", service_request, "service requests,", r_period, sep = " "))
  map <- buildChart(map)
  ggsave( file = paste("./output/map", service_request, ".png"), plot = map, width = 7.42, height = 5.75)
}

#load and run
data <- read.csv("./data/311-source.csv", header = TRUE)
data <- cleanSource(data)
month_range <- sort(unique(as.factor(as.yearmon(data$open_dt))))

summary_table <- rbind(
                 makeSummary(data, "Pothole/Roadway Surface Repair"),
                 makeSummary(data, "Street Light"),
                 makeSummary(data, "Traffic Sign"),
                 makeSummary(data, "Street Name Sign"),
                 makeSummary(data, "Abandoned Vehicle Reporting/Removal"),
                 makeSummary(data, "Street Flooding/Drainage"),
                 makeSummary(data, "Tree Service Emergency"),
                 makeSummary(data, "Tree Service"),
                 makeSummary(data, "Illegal Dumping Reporting"),
                 makeSummary(data, "Residential Recycling Programs"),
                 makeSummary(data, "Mosquito Control"),
                 makeSummary(data, "Rodent Complaint")
                 )

target_table <- rbind(
                do.call("rbind", lapply(month_range, inTarget, data = data, service_request = "Abandoned Vehicle Reporting/Removal", target = 30)),
                do.call("rbind", lapply(month_range, inTarget, data = data, service_request = "Illegal Dumping Reporting", target = 30))
                )

theme_set(theme_opa( base_size = 12 ))

service_requests <- as.character(unique(summary_table$type))
target_service_requests <- as.character(unique(target_table$type))

sapply(service_requests, plot311NetLog, data = summary_table)
sapply(target_service_requests, plot311Target, data = target_table, target = "30")
sapply(service_requests, mapNewSR, data = data)

calcKPIs(data, r_period)
