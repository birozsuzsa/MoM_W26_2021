#install and load packages
# Package names
packages <- c("rvest", "ggplot2", "readxl","readr", "dplyr", "tidyr", "reshape", "reshape2", "knitr", "lubridate", "dslabs", "stringr", "tidyverse", "RColorBrewer","viridis","ggExtra")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#read in the data from data.world
data_birth <- read.csv("https://query.data.world/s/kauwemd3g2f5254itcp6lmlfwwuter", header=TRUE, stringsAsFactors=FALSE);

#organize and group the data by the date in the month
data_mth_date <- data_birth 
data_mth_date$month <- factor(month.abb[data_mth_date$month],levels=month.abb)
data_mth_date <- data_mth_date %>%
  group_by(month,date_of_month) %>%
  summarise(sum=sum(births),avg=mean(births))

#which birthday was the most popular?
max <- data_mth_date[which.max(data_mth_date$avg),]
#which birthday was the least popular?
min <- data_mth_date[which.min(data_mth_date$avg),]

least_popular <- paste(min[,2],min[,1], sep="/")
most_popular <- paste(max[,2],max[,1], sep="/")

plot1 <-ggplot(data_mth_date,aes(month,date_of_month,fill=avg))+
  geom_tile(color= "white",size=0.25) +
  theme_minimal()+
  scale_y_continuous(trans = "reverse", breaks = unique(data_mth_date$date_of_month))+
  scale_x_discrete(position = "top")+
  scale_fill_distiller(palette = "Spectral") +
  theme(strip.background = element_rect(colour = "white"))+
  removeGrid()+
  labs(title = "Which are the most common birthdays?",
       subtitle = "based on U.S. average daily births between 1994-2014",
       x = "Month", y="Day",
       caption = "#MakeoverMonday W26 2021 | Source: U.S.National Center for Health Statistics & U.S Social Security Administration | Viz: @ZsuBiro")+
  theme(legend.position = "right",
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size=10),
        plot.caption = element_text(size = 7,colour = "grey"),
        axis.text.x  = element_text(size=8),
        axis.text.y  = element_text(size=8))

plot1


