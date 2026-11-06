# Load packages
library(tidyverse)
library(DataExplorer)
library(rstatix)

# Set up the file we'll be using

ds <- read_csv("data-raw/raw_data.csv")
demo <- readxl::read_excel("data-raw/participant_demo.xlsx", sheet = "Data") %>% rename_all(janitor::make_clean_names)
ds <- ds %>% pivot_longer(-id, names_to = "variable", values_to = "value") %>% 
  mutate(condition = ifelse(str_detect(variable, "walk"), "walk", "search"),
         variable = str_remove_all(variable, "walk_"),
         variable = str_remove_all(variable, "search_"))
ds <- ds %>% pivot_wider(id_cols = c("id", "condition"), names_from = "variable", values_from = "value")
ds <- left_join(ds, demo, by = c("id" = "subject_number"))
ds <- ds %>% relocate(test_date:humidity, .after = condition) %>% mutate(humidity = as.numeric(humidity))

# Example data for today

glimpse(ds)

# Things we already know how to do

# Filter based on expected ranges ----
ds %>% select(id, age) %>% filter(age < 18 | age > 22)
# Filter based a set of possible values ----
ds %>% select(id, handedness) %>% filter(!(handedness %in% c("Right", "Left")))

# Print conditional warnings and errors ----

# Throw a warning for unexpected ages
check_age <- ds %>% select(id, age) %>% filter(age < 18 | age > 22)
if (nrow(check_age) > 0) print(str_glue("WARNING: {nrow(check_age)} participants have ages out of the expected range"))

# Explicit pass/fail for handedness
check_handedness <- ds %>% select(id, handedness) %>% filter(!(handedness %in% c("Right", "Left")))
if (nrow(check_handedness) > 0) {
  print("Handedness check FAILED")
  print(check_handedness)
} else {
  print("Handedness check PASSED")
}

# Check the number of participant ids ----
if (length(unique(ds$id)) != 30) print("ERROR: Incomplete Dataset")

# Count
count(ds, sex)
ds$sex <- tolower(ds$sex)

# Count NAs
sum(is.na(ds$temperature))

# Get summary stats
ds %>% drop_na(temperature) %>% summarize(temp_min = min(temperature), temp_mean = mean(temperature), temp_max = max(temperature))

summary(ds$temperature) #Also a nice option

# rstatix functions ----

# Get summary stats ----
ds %>% get_summary_stats(temperature) #Note that it drops NAs
ds %>% group_by(target, sex) %>% get_summary_stats(temperature)

# Specify multiple variables and types of output stats
ds %>% get_summary_stats(c("temperature", "humidity"), type = "common")
ds %>% get_summary_stats(starts_with("eyex"), type = "mean_sd")

# rstatix::freq_table() ----
ds %>% freq_table(sex)
ds %>% freq_table(target, sex)

# rstatix::identify_outliers() ----
if (nrow(identify_outliers(ds, temperature)) == 0) print("No temperature outliers")
ds %>% identify_outliers(eyex_mean) %>% select(id, condition, eyex_mean)

# rstatix::cor_mat()
ds %>% cor_mat(starts_with("eyex_")) %>% pull_lower_triangle()
ds %>% cor_mat(starts_with("eyex")) %>% cor_plot()


# Data Explorer: Histograms ----
ds %>% select(condition, gazex_std:posy_speed) %>% plot_histogram()

# Data Explorer: Density plots ----
ds %>% select(condition, gazex_std:posy_speed) %>% plot_density()

## Data Explorer: Boxplots by condition ----
ds %>% select(condition, gazex_std:posy_speed) %>% plot_boxplot(by = "condition")

## Data Explorer: Plot missing -----
ds %>% select(id:humidity) %>% plot_missing()

## Data Explorer: Correlations ----
ds %>% select(gazex_std:posy_speed) %>% plot_correlation()

## Data Explorer: Create Report ----
ds %>% create_report() # Saves an html report to your working directory

# ggplot examples ----

# Checking ranges with distribution plots

ds %>% mutate(age = ifelse(id == 103, 30, age)) %>%
ggplot(aes(x = posy_std)) + 
  geom_histogram() + 
  xlab("Average vertical head position")

ds %>% mutate(age = ifelse(id == 103, 30, age)) %>%
ggplot(aes(x = posy_std)) + 
  geom_density() + 
  xlab("Average vertical head position")

ds %>% mutate(age = ifelse(id == 103, 30, age)) %>%
ggplot(aes(x = posy_std)) + 
  geom_boxplot() + 
  xlab("Average vertical head position")

# Add a category

ggplot(ds) + geom_boxplot(aes(y = posy_std)) 
ggplot(ds) + geom_boxplot(aes(x = condition, y = posy_std))
ggplot(ds) + geom_boxplot(aes(x = condition, y = posy_std, fill = condition))
ggplot(ds) + geom_boxplot(aes(x = condition, y = posy_std, fill = target)) 

# Simple scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean)) + 
  geom_point() + 
  xlim(-20, 20) + 
  ylim(-20, 20) + 
  xlab("Mean horizontal head position") + 
  ylab("Mean vertical head position") +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0)

# Add category to scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean, color = condition)) + 
  geom_point()

# Facetting a scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean)) + 
  geom_point() + 
  facet_wrap("condition", ncol = 1)

# Saving plots

dir.create("eda") 
ggplot(ds, aes(x = posx_mean, y = posy_mean)) + geom_point()
#ggsave will save the last plot to file
ggsave("eda/head-position-scatter.jpg")

#You can also save plots to objects (they are lists)
p1 <- ggplot(ds, aes(x = posx_mean, y = posy_mean)) + geom_point()
ggsave("eda/head-position-scatter.jpg", plot = p1)

# Automation + plotting ----

# Simpler example
plot_dv_by_condition <- function(dv) {
  ggplot(ds, aes(fill = condition, y = .data[[dv]])) + 
    geom_boxplot()
}
plot_dv_by_condition("posx_std")
plot_dv_by_condition("posy_std")

ds %>% .[["posx_std"]] #same as next line
ds$posx_std #same as previous line

# Make the function more useful with labels/saving ----
plot_dv_by_condition <- function(dv) {
  p <- ggplot(ds, aes(fill = condition, y = .data[[dv]])) + 
    geom_boxplot() + 
    ylab(dv) + #we can also use dv to label the axis
    ggtitle(str_glue("{dv} by condition")) + #and title the plot
    theme(legend.position = "bottom")
  
  # and we can use it to write a sensible output filename
  ggsave(str_glue("eda/by_condition/{dv}_by_condition_boxplot.jpg"), plot = p,
         height = 6, width = 4, units = "cm", scale = 2)
  return(p)
}
plot_dv_by_condition("posx_std")

# Use colnames() to get each of the dv characters
dvs_to_plot <- ds %>% select(-(id:humidity)) %>% colnames
dvs_to_plot
walk(dvs_to_plot, plot_dv_by_condition)

# What if we want to specify the type of plot? ----

plot_dv_by_condition <- function(dv, geom = "boxplot") {
  # Start of the ggplot
  p <- ggplot(ds) + 
    ylab(dv) + 
    ggtitle(str_glue("{dv} by condition")) + 
    theme(legend.position = "bottom")
  
  # Add the geom based on the text argument
  if (geom == "boxplot") p <- p + geom_boxplot(aes(fill = condition, y = .data[[dv]]))
  if (geom == "density") p <- p + geom_density(aes(color = condition, x = .data[[dv]]))
  ggsave(str_glue("eda/by_condition/{dv}_by_condition_{geom}.jpg"), plot = p,
         height = 6, width = 4, units = "cm", scale = 2)
  return(p)
}

# Run for boxplots
plot_dv_by_condition(dv = "posx_std", geom = "boxplot")
walk(dvs_to_plot, ~ plot_dv_by_condition(dv = .x, geom = "boxplot"))
# Run for density plots
plot_dv_by_condition(dv = "posx_std", geom = "density")
walk(dvs_to_plot, ~ plot_dv_by_condition(dv = .x, geom = "density"))

# What if we want to plot subsets of data?
plot_filtered_histogram <- function(dv, cond = "All") {
  # Use an if/else to create a subset of data if condition is provided
  if (cond == "All") {
    temp_data <- ds
  } else {
    temp_data <- ds %>% filter(condition == cond)
  }
  # Plot based on the subset
  p <- ggplot(temp_data, aes(x = .data[[dv]])) + 
    geom_histogram() + 
    ggtitle(str_glue("Histogram of {dv} for condition: {cond}"))
  return(p)
}
plot_filtered_histogram(dv = "path_total", cond = "search")
plot_filtered_histogram(dv = "path_total", cond = "walk")
plot_filtered_histogram(dv = "path_total")

# Automation + plotting with two variable inputs ----

two_var_scatter <- function(df, var1, var2) {
  p <- ggplot(df, aes(x = .data[[var1]], y = .data[[var2]])) + 
    geom_point() + 
    xlab(var1) + 
    ylab(var2) + 
    ggtitle(str_glue("{var1} by {var2}"))
  ggsave(str_glue("eda/scatter/{var1}_{var2}.jpg"), plot = p,
         height = 7, width = 7, units = "cm", scale = 1.5)
  return(p)
}
two_var_scatter(ds, "posx_std", "posy_std")

# Get the column names
pred_x <- ds %>% select(matches("x_")) %>% colnames
pred_x
pred_y <- ds %>% select(matches("y_") & where(is.numeric)) %>% colnames
pred_y
pred_x_by_y <- map2(pred_x, pred_y, ~ two_var_scatter(ds, .x, .y))
pred_x_by_y

# Pivoting and faceting to automate plots ----
ds_long <- ds %>% select(matches("x_")) %>% 
  pivot_longer(cols = everything())

slice_head(ds_long, n = 7)

ds_long %>% ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~ name, scales = "free", nrow = 2)
