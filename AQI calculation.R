library(ggplot2)

# Read the data
data <- read.csv("D:/math project/All citys data/Amaravati.csv", header = TRUE, sep = ",")

# Convert date column to Date format
data$Date <- as.Date(data$Date)

# Create a list to store the plots
plots <- list()

# Loop through each year
for (year in unique(format(data$Date, "%Y"))) {
  
  # Filter data for current year
  data_year <- subset(data, format(Date, "%Y") == year)
  
  # Calculate AQI
  data_year$AQI <- NA
  for (i in 1:nrow(data_year)) {
    if (is.na(data_year$PM2.5[i])) {
      data_year$AQI[i] <- NA
    } else {
      c <- data_year$PM2.5[i]
      if (c <= 12) {
        data_year$AQI[i] <- 50/12 * c
      } else if (c <= 35.4) {
        data_year$AQI[i] <- 50/(35.4-12) * (c-12) + 50
      } else if (c <= 55.4) {
        data_year$AQI[i] <- 100/(55.4-35.4) * (c-35.4) + 100
      } else if (c <= 150.4) {
        data_year$AQI[i] <- 150/(150.4-55.4) * (c-55.4) + 150
      } else if (c <= 250.4) {
        data_year$AQI[i] <- 200/(250.4-150.4) * (c-150.4) + 200
      } else if (c <= 350.4) {
        data_year$AQI[i] <- 300/(350.4-250.4) * (c-250.4) + 300
      } else {
        data_year$AQI[i] <- 400/(500.4-350.4) * (c-350.4) + 400
      }
    }
  }
  
  # Create AQI level column
  data_year$AQI_level <- cut(data_year$AQI, breaks = c(0, 50, 100, 150, 200, 300, 400, Inf), labels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous", "Beyond Index"))
  
  # Create plot
  p <- ggplot(data_year, aes(x = Date, y = AQI, color = AQI_level)) +
    geom_point(size = 2) +
    geom_line() +
    scale_color_manual(values = c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023", "#000000")) +
    ggtitle(paste("Amaravati Air Quality Index -", year)) +
    xlab("Date") +
    ylab("AQI") +
    theme_bw()
  
  # Add plot to list
  plots[[year]] <- p
}

# Plot all years together using grid.arrange function from the gridExtra package
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2)
