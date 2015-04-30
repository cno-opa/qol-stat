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
    o <- nrow(filter(data, month_start == month))
    c <- nrow(filter(data, month_end == month))
    return( o - c )
  }

  d <- filter(data, type == service_request)
  month_range <- unique(d$month_start)

  output <- data.frame(type = service_request, date = month_range)
  output$open <- sapply(output$date, countOpen, data = d)
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

#plot
plot311NetLog <- function(data, service_request) {

  d <- filter(data, type == service_request)

  #filter for last two years by yearmon factor
  range_u <- length(levels(d$date))
  range_l <- range_u - 24
  date_range <- levels(d$date)[range_l:range_u]

  d <- filter(d, date %in% date_range)

  p_line <- lineOPA(filter(d, variable == "open"), "date", "value", title = paste(service_request, "service requests open at end of month"), labels = "format(value, big.mark = \",\", scientific = FALSE)")
  p_bar <- barOPA(filter(d, variable == "net"), "date", "value", title = paste(service_request, "service requests net per month"), fill = "shade", labels = "format(value, big.mark = \",\", scientific = FALSE)") +
           scale_y_continuous(breaks = 0) +
           good_bad_scale +
           theme(axis.text.y = element_blank(), legend.position = "none")

  p_line <- buildChart(p_line)
  p_bar <- buildChart(p_bar)

  #remove any backslashes that will break save call
  service_request <- gsub("/", " ", service_request, fixed = TRUE)

  ggsave( file = paste("./output/311", service_request, "backlog.png"), plot = p_line, width = 7.42, height = 5.75)
  ggsave( file = paste("./output/311", service_request, "net.png"), plot = p_bar, width = 7.42, height = 5.75)
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

theme_set(theme_opa())

service_requests <- as.character(unique(summary_table$type))
target_service_requests <- as.character(unique(target_table$type))

sapply(service_requests, plot311NetLog, data = summary_table)
sapply(target_service_requests, plot311Target, data = target_table, target = "30")
