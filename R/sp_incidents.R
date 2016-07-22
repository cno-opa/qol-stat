# Load dependencies
library(dplyr)
library(ggplot2)
library(grDevices)
library(grid)
library(gridExtra)
library(RODBC)
library(scales)
library(tidyr)
library(zoo)

stat_colors <- colorRampPalette(c("#005983", "#72d0ff"))

channel <- odbcDriverConnect(connection = "Driver={SQL Server};Server=cno-sqlreport01;Database=Lama_Rpt")

query <- paste(readLines(con = "P:/Safety and Permits/sp_compliance.sql"), collapse = "\t")

# Query database
df <- sqlQuery(channel, query, as.is = TRUE)

# Close database connection
close(channel)

incidents <- df

names(incidents) <- tolower(names(incidents))

incidents <- incidents %>%
    filter(daystoinspect >= 0) %>%
    mutate(filingdate = as.Date(filingdate),
           inspectiondate = as.Date(inspectiondate),
           m = as.yearmon(inspectiondate),
           m_filed = as.yearmon(filingdate))

days_to_inspect_by_month <- incidents %>%
    group_by(type, m) %>%
    summarize(mean = mean(daystoinspect),
              median = median(daystoinspect),
              max = max(daystoinspect),
              num_insp = n())

for (item in unique(days_to_inspect_by_month$type)){
    
    plot_data <- days_to_inspect_by_month %>% filter(type == item)
    
    p <- ggplot(data = plot_data, aes(x = m)) +
        geom_line(aes(y = mean, color = "Mean"), size = 1.5) +
        geom_line(aes(y = median, color = "Median"), size = 1.5) +
        scale_x_yearmon(breaks = plot_data$m) +
        xlab(NULL) +
        ylab(NULL) +
        labs(color = NULL) +
        ggtitle(paste0(item, ": ", "Days to initial inspection")) +
        scale_color_manual(values = stat_colors(2))
    
    item <- tolower(gsub("[ -/]+", "_", item))
    
    print(p)
    
    ggsave(paste0("./output/", "sp_", item, "_days_to_inspect.png"), plot = p, width = 7.5, height = 5.5)
    
}

cases_filed_by_month <- incidents %>%
    filter(filingdate >= '2014-06-01' & filingdate < '2016-07-01') %>%
    group_by(type, m_filed) %>%
    summarize(num_filed = n())

for (item in unique(cases_filed_by_month$type)){
    
    plot_data <- cases_filed_by_month %>% filter(type == item)
    
    p <- ggplot(data = plot_data, aes(x = m_filed, y = num_filed)) +
        geom_bar(stat = "identity", fill = "#005983") +
        scale_x_yearmon(breaks = plot_data$m_filed) +
        labs(color = NULL) +
        xlab(NULL) +
        ylab(NULL) +
        ggtitle(paste0(item, ": ", "Violation cases filed by month"))
    
    item <- tolower(gsub("[ -/]+", "_", item))
    
    print(p)
    
    ggsave(paste0("./output/", "sp_", item, "_cases_filed.png"), plot = p, width = 7.5, height = 5.5)
    
}


