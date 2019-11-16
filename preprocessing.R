library(tidyverse)
library(lubridate)
library(ggplot2)
library(DataExplorer)
library(summarytools)
library(reshape2)
library(GGally)
library(gridExtra)


data <- readRDS("input/train_small.rds")
str(data)

# Drop position column since it is not used in test data
data$listing_position <- NULL

length(unique(data$search_id)) # 95842 unique searches
length(unique(data$site_id)) # 34 unique website POS
length(unique(data$user_country_id)) # Total 192 countries.
length(unique(data$listing_country_id)) # Total 166 destination countries

plot_missing(data)
jpeg(file = "output/plots/missing-stage1.jpeg")
plot_missing(data)
dev.off()

# Drop all competitor data columns
most_complete <- data[, -grep("competitor*", names(data))]
plot_missing(most_complete)
jpeg(file = "output/plots/missing-stage2.jpeg")
plot_missing(most_complete)
dev.off()

remove_columns <-
  c(
    "log_click_proportion",
    "user_hist_paid",
    "user_hist_stars",
    "booking_value",
    "distance_to_dest"
  )
most_complete <-
  most_complete[,!names(most_complete) %in% remove_columns]
plot_missing(most_complete)
jpeg(file = "output/plots/missing-stage3.jpeg")
plot_missing(most_complete)
dev.off()

# Analyzing missing values in location_score2
summary(most_complete$location_score2)
cor(na.omit(as.matrix(most_complete[, c("location_score1", "location_score2")])))

most_complete$location_score2 <-
  NULL # Since there is 43% correlation with location_score1 dropping this variable entirely


##Remove observations with Value o in listing_stars and listing_review_score
summary(as.factor(as.character(most_complete$listing_stars)))
81744 / nrow(most_complete) * 100 # Remove this 3.4 % data
most_complete <- most_complete[!most_complete$listing_stars == 0, ]

summary(as.factor(as.character(most_complete$listing_review_score)))
96805 / nrow(most_complete) * 100 # Remove this 4.2 % data
most_complete <-
  most_complete[!most_complete$listing_review_score == 0, ]
most_complete <-
  most_complete[!(is.na(most_complete$listing_review_score)), ] # Remove NA as well

length(unique(most_complete$search_id)) # Now 95792 unique searches are available
plot_missing(most_complete)
names(most_complete)


#### Seperate data in to a set of features understood from frequency tables
id_columns_with_date <-
  c(
    "search_id",
    "timestamp",
    "site_id",
    "user_country_id",
    "listing_country_id",
    "listing_id",
    "destination_id"
  )

possible_target_variables <-
  c("search_id",
    "timestamp",
    "clicked",
    "booked")

known_categorical <-
  c("is_brand",
    "has_promotion",
    "stay_on_saturday",
    "random_sort")


#### Some quick eda before creating datasets
freq(most_complete$user_country_id, order = "freq")
freq(most_complete$listing_country_id, order = "freq")
#freq(most_complete)


id_data <- most_complete[, id_columns_with_date]
freq(id_data$user_country_id, order = "freq")

# Create top 6 user_country_id and listing_country_id
top6_user_countries <-
  as.numeric(row.names(head(
    freq(id_data$user_country_id, order = "freq")
  )))
id_data$user_country_id[!(id_data$user_country_id %in% top6_user_countries)] <-
  0
id_data$user_country_id <- as.character(id_data$user_country_id)
id_data$user_country_id <-
  paste0("country_", id_data$user_country_id)
id_data$user_country_id[id_data$user_country_id %in% c("country_0")] <-
  "country_other"
summary(as.factor(id_data$user_country_id))
id_data$user_country_id <- as.factor(id_data$user_country_id)

freq(id_data$listing_country_id, order = "freq")
top6_listing_countries <-
  as.numeric(row.names(head(
    freq(id_data$listing_country_id, order = "freq")
  )))
id_data$listing_country_id[!(id_data$listing_country_id %in% top6_listing_countries)] <-
  0
id_data$listing_country_id <-
  as.character(id_data$listing_country_id)
id_data$listing_country_id <-
  paste0("listing_", id_data$listing_country_id)
id_data$listing_country_id[id_data$listing_country_id %in% c("listing_0")] <-
  "listing_other"
summary(as.factor(id_data$listing_country_id))
id_data$listing_country_id <- as.factor(id_data$listing_country_id)

# Create top 6 pos
freq(id_data$site_id, order = "freq")
top6_site_id <-
  as.numeric(row.names(head(freq(
    id_data$site_id, order = "freq"
  ))))
id_data$site_id[!(id_data$site_id %in% top6_site_id)] <- 0
id_data$site_id <- as.character(id_data$site_id)
id_data$site_id <- paste0("pos_", id_data$site_id)
id_data$site_id[id_data$site_id %in% c("pos_0")] <- "pos_other"
summary(as.factor(id_data$site_id))
id_data$site_id <- as.factor(id_data$site_id)




### Create date dataset
target_data <- most_complete[, possible_target_variables]
target_data$date <- date(target_data$timestamp)
target_data$day <- day(target_data$date)
target_data$week <- week(target_data$date)
target_data$month <- month(target_data$date)
target_data$weekday <- weekdays(target_data$date)


table(target_data$month) # Data not available for months July- October



### Do some plotting with month
test <-
  melt(target_data[, c("month", "clicked", "booked")], id = "month")

month_plot <-
  ggplot(test, aes(x = variable, y = value, fill = factor(month))) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = position_dodge(1)) +
  scale_color_discrete("Month")
stat_summary(
  fun.ymin = min,
  fun.ymax = max,
  geom = "errorbar",
  color = "grey80",
  position = position_dodge(1),
  width = .2
)

month_plot
ggsave("output/plots/month_plot.jpeg", month_plot, device = "jpeg")

### Do some plotting with weekdays
test2 <-
  melt(target_data[, c("weekday", "clicked", "booked")], id = "weekday")

weekday_plot <-
  ggplot(test2, aes(
    x = variable,
    y = value,
    fill = factor(weekday)
  )) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = position_dodge(1)) +
  scale_color_discrete("weekday")
stat_summary(
  fun.ymin = min,
  fun.ymax = max,
  geom = "errorbar",
  color = "grey80",
  position = position_dodge(1),
  width = .2
)
weekday_plot
ggsave("output/plots/weekday_plot.jpeg", weekday_plot, device = "jpeg")

target_data$weekday <- as.factor(as.character(target_data$weekday))




## Create possible numeric columns
possible_numeric_columns <-
  names(most_complete)[!(names(most_complete) %in% unique(
    c(
      id_columns_with_date,
      possible_target_variables,
      known_categorical
    )
  ))]
names(most_complete)
possible_numeric_data <- most_complete[, possible_numeric_columns]
correlations <- cor(possible_numeric_data)
#View(correlations)

jpeg(file = "output/tables/correlations_numeric.jpeg")
grid.table(correlations)
dev.off()

saveRDS(possible_numeric_data,
        "output/data/temp/possible_numeric_data.rds")


##### Normality analysis
str(possible_numeric_data)
possible_numeric_data_summary <-
  descr(possible_numeric_data, transpose = T)

# price_usd,num_rooms,length_of_stay has skewness of over +3. suggesting non-normality
descr(possible_numeric_data$price_usd)
tail(sort(possible_numeric_data$price_usd), 150)


### Outlier Analysis can be done to remove outliers
possible_numeric_data <-
  within(possible_numeric_data, price_usd_quantile <-
           as.integer(cut(
             price_usd, quantile(price_usd, probs = 0:4 / 4), include.lowest = TRUE
           )))
possible_numeric_data$price_usd <- NULL

categorical_columns <- c("price_usd_quantile")

descr(possible_numeric_data$num_rooms)
rooms_plot <-
  ggplot(possible_numeric_data, aes(x = num_rooms)) + geom_histogram()
rooms_plot

### This is poisson distribution. Convert this to categorical variable.
categorical_columns <- c(categorical_columns, "num_rooms")

descr(possible_numeric_data$length_of_stay)
los_plot <-
  ggplot(possible_numeric_data, aes(x = length_of_stay)) + geom_histogram()
los_plot

categorical_columns <- c(categorical_columns, "length_of_stay")



jpeg(file = "output/plots/rooms_los.jpeg")
grid.arrange(rooms_plot, los_plot, ncol = 2)
dev.off()


preprocessed_data <-
  cbind.data.frame(possible_numeric_data, most_complete[, known_categorical])
str(preprocessed_data)
categorical_columns <- c(known_categorical, categorical_columns)

for (i in categorical_columns) {
  preprocessed_data[, i] <-
    as.factor(as.character(preprocessed_data[, i]))
}

str(preprocessed_data)

## Merge with created columns from id_data and target_data

preprocessed_data <-
  cbind.data.frame(possible_numeric_data, id_data[, c("listing_id","user_country_id", "listing_country_id", "site_id")])
preprocessed_data <-
  cbind.data.frame(preprocessed_data, target_data[, "weekday"])
preprocessed_data <-
  cbind.data.frame(target_data[, c("search_id", "timestamp")], preprocessed_data)



preprocessed_data <-
  cbind.data.frame(preprocessed_data, target_data[, c("clicked", "booked")])

saveRDS(preprocessed_data, "output/data/preprocessed_data.rds")
print("code executed successfully")