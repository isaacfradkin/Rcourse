library("dplyr")

original_income_wellbeing <- 
  read_csv("csv files/original_income_wellbeing.csv")

well_being = c()
income = c()
groups = c(1:15)
for(i in groups){
  temp_well_being = rnorm(original_income_wellbeing$experienced_wellbeing.personcount[i], 
                                   mean = original_income_wellbeing$experienced_wellbeing.Mean [i], 
                                   sd = original_income_wellbeing$experienced_wellbeing.std.error[i]*sqrt(original_income_wellbeing$experienced_wellbeing.personcount[i]))
  well_being = c(well_being, temp_well_being)
}

all_noise =  rnorm(length(well_being), mean = 0 , sd =original_income_wellbeing$household_income[1]/3)*12
income = well_being*mean(original_income_wellbeing$household_income) + all_noise
plot(well_being, income
     )

median(original_income_wellbeing$experienced_wellbeing.Mean)

mean(original_income_wellbeing$experienced_wellbeing.std.error)*sqrt(sum(original_income_wellbeing$experienced_wellbeing.personcount))/10
old_well_being = c()
old_income = c()
for(i in groups){
  temp_well_being = rnorm(original_income_wellbeing$experienced_wellbeing.personcount[i], 
                          mean = original_income_wellbeing$experienced_wellbeing.Mean [i], 
                          sd = original_income_wellbeing$experienced_wellbeing.std.error[i])
  old_well_being = c(old_well_being, temp_well_being)
}

all_noise =  rnorm(length(well_being), mean = 0 , sd =original_income_wellbeing$household_income[1]/3)*12
old_income_temp = old_well_being*mean(original_income_wellbeing$household_income) + all_noise



#### Cahts version
# Load necessary libraries
library(MASS)
library(dplyr)

# Load the data
data <- read.csv("path/to/your/income_wellbeing.csv")

# Compute standard deviation from standard error and person count
data <- data %>%
  mutate(
    experienced_wellbeing.sd = experienced_wellbeing.std.error * sqrt(experienced_wellbeing.personcount)
  )

# Function to simulate individual-level data for a single group
simulate_group_data <- function(group_row, high_corr = TRUE, threshold = 75000) {
  # Extract group-level statistics
  income_mean <- group_row$household_income
  income_sd <- 0.1 * income_mean  # Assume 10% variability in income within the group
  wellbeing_mean <- group_row$experienced_wellbeing.Mean
  wellbeing_sd <- group_row$experienced_wellbeing.sd
  n <- group_row$experienced_wellbeing.personcount
  
  # Simulate household income
  household_income <- rnorm(n, mean = income_mean, sd = income_sd)
  
  # Simulate experienced wellbeing
  if (high_corr) {
    # High correlation (0.75) for all income levels or below threshold
    correlation <- 0.75
    cov_matrix <- matrix(c(income_sd^2, correlation * income_sd * wellbeing_sd,
                           correlation * income_sd * wellbeing_sd, wellbeing_sd^2),
                         ncol = 2)
    simulated_data <- mvrnorm(n, mu = c(income_mean, wellbeing_mean), Sigma = cov_matrix)
    experienced_wellbeing <- simulated_data[, 2]
  } else {
    # Low or no correlation (random noise) for income above threshold
    experienced_wellbeing <- rnorm(n, mean = wellbeing_mean, sd = wellbeing_sd)
  }
  
  # Combine into a data frame
  return(data.frame(household_income = household_income, experienced_wellbeing = experienced_wellbeing))
}

# Simulate individual data for Stage 1 (high correlation across all groups)
stage1_data <- data %>%
  rowwise() %>%
  do(simulate_group_data(.)) %>%
  ungroup()

# Simulate individual data for Stage 2 (conditional correlation)
stage2_data <- data %>%
  rowwise() %>%
  do(simulate_group_data(., high_corr = .$household_income < 75000)) %>%
  ungroup()

# Save the simulated datasets as CSV
write.csv(stage1_data, "simulated_individual_data_stage1.csv", row.names = FALSE)
write.csv(stage2_data, "simulated_individual_data_stage2.csv", row.names = FALSE)
