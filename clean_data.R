# Load packages
pacman::p_load(tidyverse, haven, dplyr)

# Generate train and test data
set.seed(9871)

training_size <- floor(0.7 * nrow(superSurvey_1))
training_indices <- sample(seq_len(nrow(superSurvey_1)), size = training_size)

superSurvey_train_data <- superSurvey_1[training_indices, ]
superSurvey_test_data <- superSurvey_1[-training_indices, ]


# Aggregate
aggregate_f <- function(data) {
  data |> 
    mutate(
      race = case_when(
        white == 1 ~ "White",
        black == 1 ~ "Black",
        other == 1 ~ "Other"
      ),
      gender = case_when(
        male == 1 ~ "Male",
        female == 1 ~ "Female"
      )
    ) |> 
    group_by(state_name, year, race, gender) |> 
    summarize(theta = mean(presapp, na.rm = TRUE),
              n = n(),
              Y = sum(presapp, na.rm = TRUE)) |> 
    mutate(theta = ifelse(is.nan(theta), NA_real_, theta))
}

superSurvey_train_data <- aggregate_f(superSurvey_train_data)
superSurvey_test_data <- aggregate_f(superSurvey_test_data)

# Export the data
write.csv(superSurvey_train_data, "superSurvey_train_data.csv", row.names = FALSE)
write.csv(superSurvey_test_data, "superSurvey_test_data.csv", row.names = FALSE)



# Split the data after aggregating ----------------------------------------


# Generate train and test data
set.seed(9871)

superSurvey_1_clean <- aggregate_f(superSurvey_1)

training_size <- floor(0.7 * nrow(superSurvey_1_clean))
training_indices <- sample(seq_len(nrow(superSurvey_1_clean)), size = training_size)

superSurvey_clean_train_data <- superSurvey_1_clean[training_indices, ]
superSurvey_clean_test_data <- superSurvey_1_clean[-training_indices, ]

# Export the data
write.csv(superSurvey_clean_train_data, "superSurvey_clean_train_data.csv", row.names = FALSE)
write.csv(superSurvey_clean_test_data, "superSurvey_clean_test_data.csv", row.names = FALSE)



