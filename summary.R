library(dplyr)
library(ggplot2)
library(tidyr)

# get incarceration data
data = read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# select year, total_pop, total_jail_pop, and region for time efficiency
region_jail_percent_data <- data %>% select("year", "total_pop", "total_jail_pop", "region")
region_jail_percent_data[is.na(region_jail_percent_data)] <- 0

# get regional average incarceration percentage over time
region_mean_data <- region_jail_percent_data %>% group_by(region, year) %>% summarize(total_percentage = sum(total_jail_pop)/sum(total_pop)*100)

# get overall average of incarceration percentage
average_jail_percentage_by_region <- region_mean_data %>% group_by(region) %>% summarize(mean_jail_percentage = mean(total_percentage))

# plot
region_jail_percentage_over_time_plot <- ggplot(data = region_mean_data) + geom_line(mapping = aes(x = year, y = total_percentage, group=region, color = region)) +
  labs(title = "Percent of Inmates in Population by Region", x = "Year", y = "Percentage of Inmates in Population")

# midwest with the lowest average jail percentage
# south with the highest average jail percentage

# get columns needed for juvenile incarceration percentage
juvenile_jail_data <- data %>% select("year", "region", "total_jail_pop", "female_juvenile_jail_pop", "male_juvenile_jail_pop") %>% filter(region == "Midwest" | region == "South")
juvenile_jail_data[is.na(juvenile_jail_data)] <- 0

# get regional juvenile incarceration percentage over time
juvenile_jail_data <- juvenile_jail_data %>% group_by(region, year) %>% summarize(juvenile_jail_percentage = (sum(male_juvenile_jail_pop) + sum(female_juvenile_jail_pop))/sum(total_jail_pop) * 100) %>% filter(juvenile_jail_percentage <= 100 & juvenile_jail_percentage > 0)
juvenile_jail_data[is.na(juvenile_jail_data)] <- 0

#plot
juvenile_jail_percentage_plot <- ggplot(data = juvenile_jail_data, aes(x = year, y = juvenile_jail_percentage, fill = region)) + geom_bar(position = "fill", stat = "identity") + 
  labs(title = "Percent of Juveniles in Inmates Midwest vs South", x = "Year", y = "Percentage of Juveniles in Inmates")

# summary statistics for midwest and south region
juvenile_jail_percentage_data <- juvenile_jail_data %>% group_by(region) %>% summarize(mean_juvenile_percentage = mean(juvenile_jail_percentage), max_juvenile_percentage = max(juvenile_jail_percentage))

# calculate max juvenile incarceration percentage year
max_juvenile_year_data <- juvenile_jail_data %>% group_by(region) %>% filter(juvenile_jail_percentage == max(juvenile_jail_percentage)) 


#map data of juvenile
library(scales)

# get columns needed for the map
np_state_data <- data %>% select("year", "state", "total_jail_pop", "female_juvenile_jail_pop", "male_juvenile_jail_pop")
np_state_data[is.na(np_state_data)] <- 0

# calculate juvenile incarceration percentage by state
np_state_data <- np_state_data %>% group_by(state) %>% summarize(Percentage_of_Juvenile = (sum(male_juvenile_jail_pop) + sum(female_juvenile_jail_pop)) / sum(total_jail_pop) * 100)

# get state data and combine with juvenile incarceration data
state_shape <- map_data("state")
state_abbrevs <- data.frame(state.abb, state.name)

np_state_data <- left_join(np_state_data, state_abbrevs, by=c('state' = 'state.abb'))

np_state_data <- np_state_data %>% mutate(region = tolower(state.name))

state_shape <- left_join(state_shape, np_state_data)

# plot
juvenile_percentage_by_state_plot <- ggplot(state_shape) + geom_polygon(mapping = aes(x=long, y=lat, group = group, fill = Percentage_of_Juvenile)) + scale_fill_continuous(low = 'black', high = 'red') + coord_map() + 
  labs(title = "Percentage of Juvenile in Inmates by States")