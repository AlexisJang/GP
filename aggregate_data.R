pacman::p_load(tidyverse)


# SAMPLE DATA -------------------------------------------------------------

# Load the dataset
sample_individuals <- read_csv("Data_Alexis/sample_individuals.csv")
sample_individuals_train_data <- read_csv("Data_Alexis/sample_individuals_train_data.csv")
sample_individuals_test_data <- read_csv("Data_Alexis/sample_individuals_test_data.csv")


# COMBINED ----------------------------------------------------------------

# Aggregate by state, race, gender, and year
sample_agg <- sample_individuals |>
  # sample_agg <- individual_data |> 
  mutate(
    race = case_when(
      race == 0 ~ "White", 
      race == 1 ~ "Black",
      race == 2 ~ "Other"),
    gender = case_when(
      gender == 0 ~ "Female",
      gender == 1 ~ "Male"
    )) |>  
  group_by(state, race, gender, year, group_id) |>  
  summarize(Y = sum(observed_support, na.rm = TRUE),
            n = n(),
            Y_bar = mean(observed_support, na.rm = TRUE))

sample_agg <- sample_agg |> 
  left_join(
    individual_data |> 
      select(group_id, latent_group_effect) |>
      distinct(),
    by = "group_id"
  )

# Fetch state center coordinates
df_states <- data.frame(
  state = state.name,
  lon = state.center$x,
  lat = state.center$y
)

# Combine datasets
sample_agg <- left_join(sample_agg, df_states, by = "state")

sample_agg <- sample_agg |> 
  rename(
    latent_group_pref = latent_group_effect
  )


# ADDITIONAL CHECKS -------------------------------------------------------

# Plot the latent group preference
ggplot(
  data = sample_agg,
  mapping = aes(x = latent_group_pref)
) +
  geom_histogram(
    binwidth = 0.2,
    color = "#72B4F3",
    fill = "#72B4F3",
    alpha = 0.7
  ) +
  geom_vline(xintercept = mean(sample_agg$latent_group_pref, na.rm=T),
             color = "#C6227F", lty = "dashed", lwd = 0.8) +
  annotate("text", y = 350, x = 2.1, label = "Mean = 0.02",
           color = "#C6227F", hjust = 1, size = 4.5) +
  labs(
    title = "Distribution of Latent Group Effect",
    subtitle = "Min = -6.02, Max = 5.76, Mean = 0.02",
    x = "Latent Group Effect",
    y = "Count"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 13)
  )

# Check the distribution of # individuals per group
sample_agg |> 
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 10, fill = "#72B4F3", 
                 color = "#72B4F3", alpha = 0.7) +
  geom_vline(xintercept = mean(sample_agg$n, na.rm = TRUE),
             linetype = "dashed", color = "#C6227F", size = 0.8) +
  annotate("text", y = 1300, x = 155, label = "Mean = 38 per group",
           color = "#C6227F", hjust = 1, size = 4.5) +
  labs(
    title = "Distribution of # Individuals Per Group",
    subtitle = "Min = 0, Max = 428, Mean = 38",
    x = "Number of Individuals Per Group",
    y = "Count"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 13)
  )

# Check the correlation between Y_bar and latent group preference
summary(lm(Y_bar ~ latent_group_pref, data = sample_agg))

# Consecutive year comparison by state, race, and gender
sample_agg |> 
  group_by(state, race, gender) |> 
  arrange(year) |> 
  mutate(
    Y_bar_next = lead(Y_bar, 1),
    year_next = lead(year, 1)
  ) |> 
  filter(!is.na(Y_bar_next)) |> 
  # Example state: Missouri
  filter(state == "Missouri") |> 
  ggplot(aes(x = Y_bar, y = Y_bar_next)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess",
              color = "#3564ED", fill = "#72B4F3") +
  facet_wrap(~ state + race + gender) +
  labs(
    x = "Mean Support: Year t",
    y = "Mean Support: Year t+1",
    title = "Consecutive-Year Comparison by State, Race, and Gender",
    subtitle = "Example State: Missouri"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 13),
    
  )

# Autocorrelation Function at lag 1
sample_agg |> 
  group_by(state, race, gender) |> 
  arrange(year) |> 
  mutate(Y_bar_lag1 = lag(Y_bar, 1)) |> 
  summarize(
    ACF1 = cor(Y_bar, Y_bar_lag1, use = "complete.obs"),
    .groups = "drop"
  )

# Correlation between neighboring states
sample_agg |> 
  filter(state %in% c("California", "Nevada", "Oregon")) |> 
  ggplot(aes(x = year, y = Y_bar, color = state, linetype = state)) +
  geom_line() +
  facet_wrap(~ race + gender) +
  labs(
    x = "Year",
    y = "Mean support",
    color = "State",
    linetype = "State",
    title = "Yearly Mean Support in Neighboring States",
    subtitle = "Example: California, Nevada, and Oregon"
  ) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 13),
    legend.position = "bottom"
  )



# TRAIN DATA --------------------------------------------------------------

# Aggregate by state, race, gender, and year
sample_agg_train <- sample_individuals_train_data |>
  # sample_agg <- individual_data |> 
  mutate(
    race = case_when(
      race == 0 ~ "White", 
      race == 1 ~ "Black",
      race == 2 ~ "Other"),
    gender = case_when(
      gender == 0 ~ "Female",
      gender == 1 ~ "Male"
    )) |>  
  group_by(state, race, gender, year, group_id) |>  
  summarize(Y = sum(observed_support, na.rm = TRUE),
            n = n(),
            Y_bar = mean(observed_support, na.rm = TRUE))

sample_agg_train <- sample_agg_train |> 
  left_join(
    individual_data |> 
      select(group_id, latent_group_effect) |>
      distinct(),
    by = "group_id"
  )

# Fetch state center coordinates
df_states <- data.frame(
  state = state.name,
  lon = state.center$x,
  lat = state.center$y
)

# Combine datasets
sample_agg_train <- left_join(sample_agg_train, df_states, by = "state")

sample_agg_train <- sample_agg_train |> 
  rename(
    latent_group_pref = latent_group_effect
  )


# TEST DATA ---------------------------------------------------------------

# Aggregate by state, race, gender, and year
sample_agg_test <- sample_individuals_test_data |>
  # sample_agg <- individual_data |> 
  mutate(
    race = case_when(
      race == 0 ~ "White", 
      race == 1 ~ "Black",
      race == 2 ~ "Other"),
    gender = case_when(
      gender == 0 ~ "Female",
      gender == 1 ~ "Male"
    )) |>  
  group_by(state, race, gender, year, group_id) |>  
  summarize(Y = sum(observed_support, na.rm = TRUE),
            n = n(),
            Y_bar = mean(observed_support, na.rm = TRUE))

sample_agg_test <- sample_agg_test |> 
  left_join(
    individual_data |> 
      select(group_id, latent_group_effect) |>
      distinct(),
    by = "group_id"
  )

# Fetch state center coordinates
df_states <- data.frame(
  state = state.name,
  lon = state.center$x,
  lat = state.center$y
)

# Combine datasets
sample_agg_test <- left_join(sample_agg_test, df_states, by = "state")

sample_agg_test <- sample_agg_test |> 
  rename(
    latent_group_pref = latent_group_effect
  )


# OUTPUT ------------------------------------------------------------------

write_csv(sample_agg_train, "sample_agg_train.csv")
write_csv(sample_agg_test, "sample_agg_test.csv")
