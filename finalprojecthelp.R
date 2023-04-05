library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(cowplot)
library(plm)

# URL of the file to download
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"

# Download the file and save it in the current working directory
download.file(url, destfile = "owid-covid-data.csv", mode = "wb")

# Read in the CSV file using read.csv
df <- read.csv("owid-covid-data.csv")

# Getting the number of rows and columns
num_rows <- nrow(df)
num_cols <- ncol(df)

cat("The dataset has", num_cols, "columns and", num_rows, "rows.\n")

# Printing the columns (variables) names
data.frame(colnames(df))

cat("Do all the observations in 'date' follow the pattern 'yyyy-mm-dd'?", all(grepl("\\d{4}-\\d{2}-\\d{2}", df$date)))

df$date <- as.Date(df$date)

df <- df %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date)) %>%
  select(iso_code, continent, location, date, year, month, day, everything())

#View(df)

unique(df$iso_code)

df_countries <- subset(df, !grepl("^OWID_", iso_code))

df_countries %>%
  distinct(iso_code, location) %>%
  select(iso_code, location)

#View(df_countries)


#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

# Aggregate new_cases by continent and date
df_newcases <- aggregate(new_cases ~ continent + date, data = df_countries, sum, na.action = na.omit)

#View(df_newcases)

# Create the plot
ggplot(df_newcases, aes(x = date, y = new_cases, color = continent)) +
  geom_line(size=0.8) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), breaks = seq(0, max(df_newcases$new_cases), by = 1e6))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(x = "Date", y = "New Cases (millions)", color = "Continent") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_line(size = 0.1, color = "grey80", linetype = "dotted"),
        panel.grid.major.y = element_line(size = 0.2, color = "grey80", linetype = "dashed"))
  
#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

# Aggregate new_deaths by continent and date
df_newdeaths <- aggregate(new_deaths ~ continent + date, data = df_countries, sum, na.action = na.omit)

#View(df_newdeaths)

# Create the plot
ggplot(df_newdeaths, aes(x = date, y = new_deaths, color = continent)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(x = "Date", y = "New Deaths", color = "Continent") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_line(size = 0.1, color = "grey80", linetype = "dotted"),
        panel.grid.major.y = element_line(size = 0.2, color = "grey80", linetype = "dashed"))


#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

# Aggregate stringency_index by continent and date
df_stringencyindex <- aggregate(stringency_index ~ continent + date, data = df_countries, mean, na.action = na.omit)

#View(df_stringencyindex)

# Create the plot
ggplot(df_stringencyindex, aes(x = date, y = stringency_index, color = continent)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(x = "Date", y = "Stringency Index", color = "Continent") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_line(size = 0.1, color = "grey80", linetype = "dotted"),
        panel.grid.major.y = element_line(size = 0.2, color = "grey80", linetype = "dashed"))


#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

df_finalsample <- df_countries %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2022-11-30"))

#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

str(df_finalsample)

#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

df_plot1 <- subset(df_finalsample, !is.na(stringency_index) & !is.na(new_cases))

#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

df_plot2 <- subset(df_finalsample, !is.na(stringency_index) & !is.na(new_deaths))

#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

plot1 <- ggplot(df_plot1, aes(x = stringency_index, y = new_cases)) +
  geom_point(stat='identity', position='identity', aes(colour=continent),size=1.5) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), breaks = seq(0, 2e6, by = 0.5e6)) +
  labs(x = "Stringency Index", y = "New Cases") +
  ggtitle("Stringency Index and New Cases")

plot1

plot2 <- ggplot(df_plot2, aes(x = stringency_index, y = new_deaths)) +
  geom_point(stat='identity', position='identity', aes(colour=continent),size=1.5) +
  labs(x = "Stringency Index", y = "New Deaths") +
  ggtitle("Stringency Index and New Deaths")

plot2

plot_grid(plot1, plot2, nrow = 1)

#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

# Create a unique identifier for each observation
df_finalsample$id <- paste(df_finalsample$location, df_finalsample$date, sep = "_")

# Check for duplicates
any(duplicated(df_finalsample$id))

table(df_finalsample$location, df_finalsample$year) #unbalanced

df_finalsample <- pdata.frame(df_finalsample,index = c('location', 'date'))

str(df_finalsample)

df_finalsample <- as.data.frame(df_finalsample)

#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

df_finalsample_2021 <- df_finalsample %>%
  filter(year == 2021)

df_finalsample_2022 <- df_finalsample %>%
  filter(year == 2022)

#-----------------------------#-----------------------------#-----------------------------#-----------------------------#

# Fit linear model
model1 <- plm(new_deaths ~ stringency_index, data = df_finalsample_2022, model="within", effect = "twoways", index = c('location', 'date'))

summary(model1)


# Filter data for 30/11/2022
df_filtered_map <- subset(df_finalsample, date == "2021-01-01")

# Get map data
map_data <- map_data("europe")

# Merge data and map data
df_map <- merge(map_data, df_filtered_map, by.x = "region", by.y = "location", all.x = TRUE)

# Create plot
ggplot(df_map, aes(x = long, y = lat, group = region, fill = stringency_index)) +
  geom_map(map = df_map, aes(map_id = region)) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_void() +
  ggtitle("Total Deaths on 30/11/2022") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(df_map, aes(x = long, y = lat, group = group, fill = total_deaths)) +
  geom_map(map = df_map, aes(map_id = region)) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_void() +
  ggtitle("Total Deaths on 30/11/2022") +
  theme(plot.title = element_text(hjust = 0.5))




plot1 <- ggplot(df_plot1, aes(x = stringency_index, y = new_cases)) +
  geom_point(stat='identity', position='identity', aes(colour=continent),size=1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), breaks = seq(0, 2e6, by = 0.5e6)) +
  labs(x = "Stringency Index", y = "New Cases") +
  ggtitle("Stringency Index and New Cases") +
  scale_color_manual(values = c("#999F55","#FF0000","#00FF00","#0000FF","#00FFFF","#9553FF"), guide = "none")

plot2 <- ggplot(df_plot2, aes(x = stringency_index, y = new_deaths)) +
  geom_point(stat='identity', position='identity', aes(colour=continent),size=1) +
  labs(x = "Stringency Index", y = "New Deaths") +
  ggtitle("Stringency Index and New Deaths") +
  scale_color_manual(values = c("#999F55","#FF0000","#00FF00","#0000FF","#00FFFF","#9553FF"), guide = "none")

legend <- ggplot() +
  geom_point(aes(x = 1, y = 1, colour = "Africa"), size = 3) +
  geom_point(aes(x = 1, y = 2, colour = "Asia"), size = 3) +
  geom_point(aes(x = 1, y = 3, colour = "Europe"), size = 3) +
  geom_point(aes(x = 1, y = 3, colour = "North America"), size = 3) +
  geom_point(aes(x = 1, y = 3, colour = "South America"), size = 3) +
  geom_point(aes(x = 1, y = 3, colour = "Oceania"), size = 3) +
  scale_color_manual(values = c("#999F55","#FF0000","#00FF00","#0000FF","#00FFFF","#9553FF"), guide = "none") +
  guides(colour = guide_legend(title = "Continent", ncol = 1, override.aes = list(size = 3)))

plot_grid(plot1, plot2, legend, ncol = 2)



library(cowplot)

plot1 <- ggplot(df_plot1, aes(x = stringency_index, y = new_cases)) +
  geom_point(stat='identity', position='identity', aes(colour=continent),size=1) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), breaks = seq(0, 2e6, by = 0.5e6)) +
  labs(x = "Stringency Index", y = "New Cases") +
  ggtitle("Stringency Index and New Cases") +
  scale_color_manual(values = c("#999F55","#FF0000","#00FF00","#0000FF","#00FFFF","#9553FF"), guide = "none")

plot2 <- ggplot(df_plot2, aes(x = stringency_index, y = new_deaths)) +
  geom_point(stat='identity', position='identity', aes(colour=continent),size=1) +
  labs(x = "Stringency Index", y = "New Deaths") +
  ggtitle("Stringency Index and New Deaths") +
  scale_color_manual(values = c("#999F55","#FF0000","#00FF00","#0000FF","#00FFFF","#9553FF"), guide = "none")

legend <- get_legend(plot1 + theme(legend.position = "bottom"))

plot_grid(plot1 + theme(legend.position = "none"), plot2 + theme(legend.position = "right"), legend, ncol = 3, rel_widths = c(3, 3, 0.8), align = "h")

