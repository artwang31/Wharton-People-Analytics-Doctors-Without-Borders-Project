# Loading in packages
library(tidyverse)
library(data.table)
library(ggplot2)
# library(rworldmap)

# Reading in data
raw_data <- fread(paste0("Wharton People Analytics Data Final.csv"), header = T, stringsAsFactors = F, data.table = T)
data <- raw_data

# Coverting time to Posix formart
data$departure_date <-  as.POSIXct(data$departure_date, format = "%m/%d/%Y")
data$return_date <- as.POSIXct(data$return_date, format = "%m/%d/%Y")

# Creating two new columns, one for difference of intervals in days, and year of departure
data <- data %>% 
  mutate(duration = difftime(data$return_date,data$departure_date, units = "days")) %>%
  mutate(depart_date_year = substr(data$departure_date, start = 1,stop = 4)) 

# Calculating the number of assignments per year 
data$n <- 1
assignment <- data %>% group_by(depart_date_year) %>% summarize(assignment = sum(n))

# Visualization of assignments per year
ggplot(assignment, aes(x = depart_date_year, y = assignment, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point()

# Total number of assignments per country
country_total_assignments <- data %>% group_by(assignment_country) %>% summarize(n = sum(n))
# Ordering totals 
country_total_assignments <- country_total_assignments[order(country_total_assignments$n, decreasing = TRUE),]

# Visualizing total assignments per country
ggplot(country_total_assignments[1:20,], aes(x = reorder(assignment_country, n), y = n, fill = -n)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  xlab("Country") + ggtitle("Total Assignments per Country")

# Filtering only the assignment right before someone turned MEDCO
assignment_before_medcos <- data[data$pool == "MEDCO", 1]
assignment_before_medcos <- data %>% filter(staff_id %in% assignment_before_medcos) %>% filter(!pool == "MEDCO") %>% 
  group_by(assignment_country) %>% summarise(n = sum(n))
# Ordering total medcos
assignment_before_medcos <- assignment_before_medcos[order(assignment_before_medcos$n, decreasing = TRUE), ]

# Visualizing countries before the step of MEDCOs per country
ggplot(assignment_before_medcos[1:20,], aes(x = reorder(assignment_country,n), y = n, fill = -n))+
  geom_bar(stat = "identity") + 
  coord_flip() +
  xlab("Country") + ggtitle("Total MEDCOs per Country")

# MEDCOs only
medcos_only <- data %>% filter(pool == "MEDCO") %>% group_by(assignment_country) %>% summarise(n = sum(n))
# Ordering by number
medcos_assignment <- medcos_only[order(medcos_only$n, decreasing = TRUE), ]

# Visualizing countries with most MEDCOs
ggplot(medcos_assignment[1:20,], aes(x = reorder(assignment_country,n), y = n, fill = -n))+
  geom_bar(stat = "identity") + 
  coord_flip() +
  xlab("Country") + ggtitle("Total MEDCOs per Country")

# Exploratory Data Analysis
median_duration <- data %>% group_by(pool) %>% summarize(days = median(duration))
mean_duration <- data %>% group_by(pool) %>% summarize(days = mean(duration))

# Ordering
medco_mean_duration <- data %>% filter(pool == "MEDCO") %>% group_by(assignment_country) %>%
                                summarize(duration = mean(duration))
medco_mean_duration <- medco_mean_duration[order(medco_mean_duration$duration, decreasing = TRUE), ]

# Plotting the median and mean number of days per position
ggplot(median_duration, aes(x = pool, y = days)) +
  geom_bar(stat = "identity", aes(fill = pool)) +
  coord_flip() +
  theme(legend.position = "top")

ggplot(mean_duration, aes(x = pool, y = days)) +
  geom_bar(stat = "identity", aes(fill = pool)) +
  coord_flip() +
  theme(legend.position = "top")
# 
# 
# 
# ggplot(medco_mean_duration[1:20,], aes(x = assignment_country, y = duration) +
#          geom_bar(stat = "identity", aes(fill = assignment_country)) +
#          coord_flip() +
#          theme(legend.position = "top")
# 
# 
# #bulgaria, mozambique, 
# map.world<-map_data("world")
# mapped_data <- joinCountryData2Map(population_data, joinCode = "ISO2", 
#                                    nameJoinColumn = "Country.Code")
# #decision tree
# tidy<-function(x){
#   number<-x %>%
#     group_by(staff_id) %>%
#     summarise(tot_assign = max(assignment_number))
#   
#   duration<-x %>% 
#     group_by(staff_id) %>%
#     summarise(tot_days = sum(duration_days))
#   
#   df<-x %>% select(staff_id,sex,first_departure,pool)
#   number %>% 
#     left_join(duration, by = 'staff_id') %>%
#     left_join(df, by = 'staff_id') 
#   
# }
# d.tree<-tidy(data) %>% distinct(staff_id, .keep_all = TRUE) 
# d.tree<-na.omit(d.tree)
# d.tree$pool<-ifelse(d.tree$pool == "MEDCO", "MEDCO", "NONMEDCO")
# #decision tree
# 
# set.seed(123)
# split = sample.split(d.tree, SplitRatio = 0.75)
# training_set = subset(d.tree, split == TRUE)
# test_set = subset(d.tree, split == FALSE)
# 
# tree<-rpart(formula = pool ~., data = training_set)
# 
# y_pred = predict(tree, newdata = test_set[-6], type = 'class')
# 
# 
# rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)