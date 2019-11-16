library(GGally)
temp_data <- readRDS("output/data/temp/possible_numeric_data.rds")

correlation_plot <- ggpairs(temp_data)
ggsave("output/plots/correlation_plot.jpeg",correlation_plot)