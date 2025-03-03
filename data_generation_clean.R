### SETUP
pacman::p_load(DeclareDesign, lme4, tidyverse, rdss, tibble, mvtnorm)

### SIMULATION
set.seed(123)

# Define years
years <- 2000:2024

# Define key coefficients
coef_gender <- 0.2
coef_race1 <- 0.2  # For race == 1
coef_race2 <- 0.1  # For race == 2

# Define Intra-class Correlation Coefficient (ICC)
desired_ICC <- 0.7
sigma_between_sq <- desired_ICC / (1 - desired_ICC)  # ≈ 2.333 for ICC = 0.7
sigma_between <- sqrt(sigma_between_sq)              # ≈ 1.5275

# Define length scales for the ARD kernel
length_scale_state <- 3.0
length_scale_gender <- 0.5
length_scale_race <- 0.5
length_scale_year <- 5.0  

# Create state-level data
states <-
  as_tibble(state.x77) |> 
  mutate(
    state = rownames(state.x77),
    prop_of_US = Population / sum(Population),
    
    # Scale to approximately 100,000 individuals
    state_n = round(prop_of_US * 99998.6),
    
    # Generate varying probabilities for demographics across states
    prob_men = runif(n(), min = 0.45, max = 0.55),
    prob_women = 1 - prob_men,
    prob_white = runif(n(), min = 0.5, max = 0.9),
    prob_black = runif(n(), min = 0.05, max = 0.4),
    prob_other = 1 - prob_white - prob_black,
    
    state_shock = rnorm(n = n(), mean = 0, sd = 0.5)
  )

# Ensure probabilities sum to 1 and are non-negative
states <- states |> 
  mutate(
    prob_other = pmax(prob_other, 0),
    total_prob = prob_white + prob_black + prob_other,
    prob_white = prob_white / total_prob,
    prob_black = prob_black / total_prob,
    prob_other = prob_other / total_prob
  )

# Replicate each state across the desired years
states_years <- states |> 
  crossing(year = years)  

# Generate the individual-level population data 
individuals <- states_years |> 
  rowwise() |> 
  do({
    data.frame(
      state = rep(.$state, times = .$state_n),
      year = rep(.$year, times = .$state_n),
      prob_men = .$prob_men,
      prob_white = .$prob_white,
      prob_black = .$prob_black,
      prob_other = .$prob_other,
      state_shock = .$state_shock
    )
  }) |> 
  ungroup()

individuals <- individuals |> 
  rowwise() |> 
  mutate(
    gender = rbinom(1, 1, prob_men),
    race = sample(x = 0:2, size = 1, replace = TRUE,
                  prob = c(prob_white, prob_black, prob_other))
  ) |> 
  ungroup()

# Declare the population using the generated data
population_decl <- declare_population(data = individuals)

# Add latent group effects using an ARD kernel
latent_effects_decl <- declare_step(
  handler = function(data, ...) {
    data <- data |> 
      mutate(group_id = paste(state, year, gender, race, sep = "_"))
    
    unique_groups <- unique(data$group_id)
    n_groups <- length(unique_groups)
    
    group_data <- data.frame(group_id = unique_groups) |> 
      separate(group_id, 
               into = c("state", "year", "gender", "race"), 
               sep = "_", 
               remove = FALSE)
    
    group_data$gender <- as.numeric(group_data$gender)
    group_data$race <- as.numeric(group_data$race)
    group_data$year <- as.numeric(group_data$year)
    
    state_centers <- data.frame(
      state = state.name,
      x = state.center$x,
      y = state.center$y
    )
    group_data <- group_data |> 
      left_join(state_centers, by = "state")
    
    if (any(is.na(group_data$x))) {
      stop("Some states could not be matched to state centers")
    }
    
    n_groups <- nrow(group_data)
    D <- matrix(0, n_groups, n_groups)
    
    for (i in 1:n_groups) {
      for (j in 1:n_groups) {
        dx <- (group_data$x[i] - group_data$x[j]) / length_scale_state
        dy <- (group_data$y[i] - group_data$y[j]) / length_scale_state
        d_state_sq <- dx^2 + dy^2
        
        d_gender_sq <- ((group_data$gender[i] - group_data$gender[j]) / length_scale_gender)^2
        
        d_race <- ifelse(group_data$race[i] == group_data$race[j], 0, 1) 
        d_race_sq <- (d_race / length_scale_race)^2
        
        # Add year proximity to the distance metric
        d_year_sq <- ((group_data$year[i] - group_data$year[j]) / length_scale_year)^2
        
        # Include year in the distance calculation
        D[i, j] <- d_state_sq + d_gender_sq + d_race_sq + d_year_sq
      }
    }
    
    # Compute covariance matrix K
    K <- sigma_between_sq * exp(-0.5 * D)
    
    # Sample latent group effects from the multivariate normal
    latent_group_effects <- rmvnorm(1, mean = rep(0, n_groups), sigma = K)
    latent_group_effects <- as.vector(latent_group_effects)
    
    group_data$latent_group_effect <- latent_group_effects
    
    data <- data |> 
      left_join(group_data[, c("group_id", "latent_group_effect")], 
                by = "group_id")
    
    return(data)
  }
)

# Define potential outcomes
potential_outcomes_decl <- declare_step(
  handler = function(data, ...) {
    data <- data |> 
      mutate(
        # Map continuous latent variable onto a probability so it can be 
        # interpreted as the probability of an individual supporting the policy
        policy_support = pnorm(
          latent_group_effect
        )
      )
    return(data)
  }
)


# Generate individual level observed support
reveal_support_decl <- declare_step(
  handler = function(data, ...) {
    data <- data |> 
      mutate(
        observed_support = rbinom(n(), size = 1, prob = policy_support) 
      )
    return(data)
  }
)

# Define the true state-level policy support mean
truth_decl <- declare_inquiry(
  ~mean(policy_support),
  label = "True mean policy support",
  handler = function(population, data) {
    population |> 
      group_by(state, year) |> 
      summarize(estimand = mean(policy_support)) |> 
      ungroup()
  }
)

# Build the design
design <- population_decl +
  latent_effects_decl +
  potential_outcomes_decl +
  reveal_support_decl +
  truth_decl +
  estimator_decl

# Number of Simulations
num_simulations <- 100

# Draw individual data
individual_data <- draw_data(design)

# Plot the latent group effect
ggplot(
  data = individual_data,
  mapping = aes(x = latent_group_effect)
) +
  geom_histogram(
    binwidth = 0.2,
    color = "lightblue",
    fill = "lightblue"
  ) +
  labs(
    title = "Distribution of Latent Group Effect",
    x = "Latent Group Effect",
    y = "Count"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

# Sample 10,000
sample_individuals <- individual_data |> 
  group_by(year) |> 
  slice_sample(n = 10000, replace = FALSE) |> 
  ungroup()

training_size <- floor(0.7 * nrow(sample_individuals))
training_indices <- sample(seq_len(nrow(sample_individuals)), size = training_size)

sample_individuals_train_data <- sample_individuals[training_indices, ]
sample_individuals_test_data <- sample_individuals[-training_indices, ]

# write_csv(sample_individuals, "sample_individuals.csv")
# write_csv(sample_individuals_train_data, "sample_individuals_train_data.csv")
# write_csv(sample_individuals_test_data, "sample_individuals_test_data.csv")

