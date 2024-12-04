

# Statistics Assumption ---------------------------------------------------

z_score <- 1.645  # Z-score for 90% confidence
relative_precision <- 0.30  # Desired relative precision (30%)


# Parametric Estimation ---------------------------------------------------

# Parameters
set.seed(123)  # For reproducibility
zero_inflation_rate <- 0.6  # Proportion of zeros in the population
mean_value <- 0.5  # Mean value of non-zero elements
sd <- 0.42

# Function to simulate a zero-inflated population
simulate_poisson_population <- function(size, zero_rate, mean_val) {
  non_zero_count <- round(size * (1 - zero_rate))
  zero_count <- size - non_zero_count
  c(rep(0, zero_count), rpois(non_zero_count, lambda = mean_val))
}

# Function to simulate a zero-inflated log-normal population
simulate_log_normal_population <- function(size, zero_rate, normal_mean, normal_sd) {
  # Calculate log scale parameters
  log_mean <- log(normal_mean^2 / sqrt(normal_mean^2 + normal_sd^2))
  log_sd <- sqrt(log(1 + (normal_sd^2 / normal_mean^2)))
  
  # Generate zero-inflated data
  zero_count <- round(size * zero_rate)
  non_zero_count <- size - zero_count
  zeros <- rep(0, zero_count)
  non_zeros <- rlnorm(non_zero_count, meanlog = log_mean, sdlog = log_sd)
  c(zeros, non_zeros)
}


# Function to calculate relative precision for a given sample size
calculate_relative_precision <- function(sample_size, z, data) {
  sample <- sample(data, sample_size, replace = TRUE)
  mean_sample <- mean(sample)
  sd_sample <- sd(sample)
  se <- sd_sample / sqrt(sample_size)
  z * se / mean_sample  # Relative precision
}

# Simulate population
population_size <- 1000000
population <- simulate_poisson_population(population_size, zero_inflation_rate, mean_value)
population <- simulate_log_normal_population(population_size, zero_inflation_rate, mean_value, sd)

hist(population, breaks = 50, main = "Histogram of Simulated Population", xlab = "Values")

# Calculate overall mean and standard deviation
overall_mean <- mean(population)
overall_sd <- sd(population)

cat("Overall Mean (All Values):", overall_mean, "\n")
cat("Overall SD (All Values):", overall_sd, "\n")

# Find minimum sample size
sample_sizes <- seq(50, 1000, by = 10)  # Test sample sizes from 50 to 1000
results <- data.frame(Sample_Size = sample_sizes)

results$Relative_Precision <- sapply(sample_sizes, function(n) {
  calculate_relative_precision(n, z_score, population)
})

# Filter results to find the smallest sample size meeting the precision goal
optimal_sample_size <- min(results$Sample_Size[results$Relative_Precision <= relative_precision], na.rm = TRUE)
optimal_sample_size
# Display results
print(results)
cat("Optimal Sample Size:", optimal_sample_size, "\n")

# Optional: Plot the results
library(ggplot2)
ggplot(results, aes(x = Sample_Size, y = Relative_Precision)) +
  geom_line() +
  geom_hline(yintercept = relative_precision, linetype = "dashed", color = "red") +
  labs(title = "Relative Precision vs. Sample Size",
       x = "Sample Size",
       y = "Relative Precision") +
  theme_minimal()



# Non-Paremetric Estimation -----------------------------------------------

# Sample data from the image
sample_data <- c(0, 0.1, 0, 0.516666667, 1.083333333, 0, 0.45, 0, 0, 0, 
                 0, 0, 0.1, 0.583333333, 0, 0, 1.1, 0, 0, 0, 
                 0.316666667, 0.116666667, 0.1, 0, 0, 0, 0, 
                 0.166666667, 0, 0, 0, 0.166666667, 0.833333333, 
                 0.45, 0, 0, 0.3, 0.9, 0, 0.15, 0.433333333, 0, 
                 0, 0, 0, 0, 0.083333333, 0.283333333, 0.573333333, 
                 1.216666667, 0, 0, 0, 0.966666667, 1.583333333, 
                 0, 0, 0, 0.133333333, 0)

# Estimate the empirical distribution
density_estimate <- density(sample_data, from = 0)  # KDE
plot(density_estimate, main = "Kernel Density Estimate of Sample Data",
     xlab = "Value", ylab = "Density")

# Sample from the estimated density
library(MASS)
sampled_data <- sample(sample_data, size = 1000, replace = TRUE)  # Resampling from empirical data


# Generate ECDF
ecdf_model <- ecdf(sample_data)
plot(ecdf_model, main = "Empirical Cumulative Distribution Function", xlab = "Value", ylab = "Cumulative Probability")


# Iterative sample size search
sample_sizes <- seq(10, 1000, by = 10)  # Test sample sizes
results <- data.frame(Sample_Size = sample_sizes)

# Calculate overall mean and standard deviation
overall_mean <- mean(population)
overall_sd <- sd(population)

cat("Overall Mean (All Values):", overall_mean, "\n")
cat("Overall SD (All Values):", overall_sd, "\n")


results$Relative_Precision <- sapply(sample_sizes, function(n) {
  calculate_relative_precision(n, z_score, sample_data)
})

# Find the minimum sample size meeting the 30% precision
optimal_sample_size <- min(results$Sample_Size[results$Relative_Precision <= relative_precision])

# Display results
cat("Optimal Sample Size:", optimal_sample_size, "\n")

# Plot the results
library(ggplot2)
ggplot(results, aes(x = Sample_Size, y = Relative_Precision)) +
  geom_line() +
  geom_hline(yintercept = relative_precision, linetype = "dashed", color = "red") +
  labs(title = "Relative Precision vs. Sample Size",
       x = "Sample Size",
       y = "Relative Precision") +
  theme_minimal()
