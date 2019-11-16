library(tidyverse)
library(reshape2)
library(dplyr)
library(summarytools)
library(GGally)
library(gridExtra)
library(ggpubr)

input_data <- readRDS("output/data/preprocessed_data.rds")

# Create property data before anything
property_data <-
  unique(input_data[, c("listing_id", "listing_stars", "listing_review_score")])
sum(duplicated(property_data$listing_id))

# Calculate number of clicks and click through rate
temp <- as.data.frame(table(input_data[, c("listing_id", "clicked")]))
click_data_listing <-
  dcast(temp, formula = listing_id ~ clicked, value.var = "Freq")
click_data_listing$ctr <-
  click_data_listing[, "1"] / (click_data_listing[, "0"] + click_data_listing[, "1"]) *
  100

# Find number of booked first and then using number of clicks calculate conversion_rate
temp2 <- as.data.frame(table(input_data[, c("listing_id", "booked")]))
book_data_listing <-
  dcast(temp2, formula = listing_id ~ booked, value.var = "Freq")
names(book_data_listing) <- c("listing_id", "not_booked", "booked")

book_data_listing$clicks_recieved <- click_data_listing[, "1"]
book_data_listing$conversion_rate <-
  book_data_listing$booked / book_data_listing$clicks_recieved * 100


property_data$ctr <- click_data_listing$ctr
property_data$conversion_rate <- book_data_listing$conversion_rate

ctr_data <-
  property_data[, c("listing_id", "listing_stars", "listing_review_score", "ctr")]
conversion_data <-
  property_data[!is.nan(property_data$conversion_rate), c("listing_id",
                                                          "listing_stars",
                                                          "listing_review_score",
                                                          "conversion_rate")]


######################## Analysis of effects of listing_stars on ctr and conversion_rate

# Assume listing_stars as categorical first
ctr_data$listing_stars <-
  factor(ctr_data$listing_stars, ordered = TRUE)

ctr_data %>%
  group_by(listing_stars) %>%
  summarise(
    count_stars = n(),
    mean_ctr = mean(ctr, na.rm = TRUE),
    sd_ctr = sd(ctr, na.rm = TRUE)
  )

ctr_plot <-
  ggplot(ctr_data, aes(x = listing_stars, y = ctr, fill = listing_stars)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_classic()

ctr_plot


one_way_anova_ctr <- aov(ctr ~ listing_stars, data = ctr_data)
summary(one_way_anova_ctr)

tukey_test_ctr <- TukeyHSD(one_way_anova_ctr)
tukey_test_ctr
plot(tukey_test_ctr)

jpeg("output/plots/star_ctr.jpeg")
plot(tukey_test_ctr)
dev.off()


###
conversion_data$listing_stars <-
  factor(conversion_data$listing_stars, ordered = TRUE)

conversion_data %>%
  group_by(listing_stars) %>%
  summarise(
    count_stars = n(),
    mean_conv = mean(conversion_rate, na.rm = TRUE),
    sd_conv = sd(conversion_rate, na.rm = TRUE)
  )

conversion_plot <-
  ggplot(conversion_data,
         aes(x = listing_stars, y = conversion_rate, fill = listing_stars)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_classic()

conversion_plot


one_way_anova_conversion_rate <-
  aov(conversion_rate ~ listing_stars, data = conversion_data)
summary(one_way_anova_conversion_rate)

tukey_test_conv <- TukeyHSD(one_way_anova_conversion_rate)
tukey_test_conv
plot(tukey_test_conv)

jpeg("output/plots/star_conv.jpeg")
plot(tukey_test_conv)
dev.off()


######################## Analysis of effects of review_score on ctr and conversion_rate

# ctr
ctr_tau <-
  cor.test(ctr_data$listing_review_score, ctr_data$ctr, method = "kendall")
ctr_tau

# conv
conv_tau <-
  cor.test(conversion_data$listing_review_score,
           conversion_data$conversion_rate,
           method = "kendall")
conv_tau
ggqqplot(ctr_data$ctr, ylab = "ctr")

jpeg("output/plots/review_score_quantiles.jpeg")
ggqqplot(conversion_data$listing_review_score, ylab = "review_score")
dev.off()

#
jpeg("output/plots/conversion_rate_quantiles.jpeg")
ggqqplot(conversion_data$conversion_rate, ylab = "conversion_rate")
dev.off()
