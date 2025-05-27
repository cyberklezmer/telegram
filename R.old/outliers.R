# identifies top channles by reach, evaluates distribution of accepting/forwarding and makes a graph of proximity

# number of channels in graph and stats

library(gridExtra) # For arranging plots and tables in a PDF



source("defs.R")

con <- connect_db()


catalogue_data <- get_catalogue(con,islandcondition)

# Convert data into long format for ggplot2 compatibility



# Calculate the IQR and detect upper outliers for accepted

# Define a function to calculate upper bounds and detect outliers
detect_outliers <- function(data, variable) {
  q1 <- quantile(data[[variable]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[variable]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  upper_bound <- q3 + 1.5 * iqr
  data[[paste0("outlier_", variable)]] <- data[[variable]] > upper_bound
  return(list(data = data, upper_bound = upper_bound))
}




# Kernel Density Estimation (KDE)

detect_prob_of_outliers <- function(data, variable) {
  # Check if the variable exists in the data frame
  if (!variable %in% names(data)) {
    stop(paste("Variable", variable, "does not exist in the data frame"))
  }
  
  # Check if the variable is numeric
  if (!is.numeric(data[[variable]])) {
    stop(paste("Variable", variable, "is not numeric"))
  }
  
  # Perform kernel density estimation
  density_est <- density(data[[variable]], na.rm = TRUE)
  density_values <- approx(density_est$x, density_est$y, xout = data[[variable]])$y
  
  # Handle any NA density values (e.g., for extreme outliers)
  density_values[is.na(density_values)] <- min(density_values, na.rm = TRUE)
  
  # Assign probabilities (inverse of density for outliers)
  outlier_prob <- 1 / density_values
  outlier_prob <- outlier_prob / max(outlier_prob, na.rm = TRUE)  # Normalize to [0, 1]
  
  outlier_prob[is.na(data[[variable]])] <- NA
  
  # Add the probabilities as a new column in the original data frame
  data[[paste0(variable, "_outlier_prob")]] <- outlier_prob
  
  return(data)
}

# Apply the function to accepted
catalogue_data <- detect_prob_of_outliers(catalogue_data, "accepted")
result <- detect_outliers(catalogue_data, "accepted")
catalogue_data <- result$data
accepted_upper <- result$upper_bound


# Apply the function to reactions_per_msg_user
catalogue_data <- detect_prob_of_outliers(catalogue_data, "reactions_per_msg_user")
result <- detect_outliers(catalogue_data, "reactions_per_msg_user")
catalogue_data <- result$data
reactions_upper <- result$upper_bound
# Display the dataset with outlier flags
result_data <- catalogue_data %>%
    select(username,reactions_per_msg_user_outlier_prob,accepted_outlier_prob)
write.csv(result_data, paste0(output_folder, islandid, "_outliers_res.csv"))

# messages_data <- get_messages(con)  

# Load necessary libraries

# Calculate summary statistics and outlier counts

summary_stats <- function(data, variable) {
  mean_value <- mean(data[[variable]], na.rm = TRUE)
  variance_value <- var(data[[variable]], na.rm = TRUE)
  num_outliers <- sum(data[[paste0("outlier_", variable)]], na.rm = TRUE)
  return(data.frame(Variable = variable, Mean = mean_value, Variance = variance_value, Outliers = num_outliers))
}


# Summarize data for 'accepted' and 'reactions_per_msg_user'
stats_accepted <- summary_stats(catalogue_data, "accepted")
stats_reactions <- summary_stats(catalogue_data, "reactions_per_msg_user")
summary_table <- rbind(stats_accepted, stats_reactions)




# Write to PDF
pdf(paste0(output_folder, islandid, "_distributions_outliers_with_stats.pdf"))

# Display summary table in the PDF
print(grid.table(summary_table))

dev.off()


# Plot distribution for 'accepted'

p<- ggplot(catalogue_data, aes(x = accepted)) +
    geom_histogram(bins = 20, fill = "lightgreen", color = "black", alpha = 0.7) +
    geom_vline(xintercept = accepted_upper, color = "red", linetype = "dashed") +
    labs(title = "Distribution of accepted", x = "accepted", y = "Frequency") +
    theme_minimal()

# Plot distribution for 'reactions_per_msg_user'

q<-  ggplot(catalogue_data, aes(x = reactions_per_msg_user)) +
    geom_histogram(bins = 20, fill = "orange", color = "black", alpha = 0.7) +
    geom_vline(xintercept = reactions_upper, color = "red", linetype = "dashed") +
    labs(title = "Distribution of reactions_per_msg_user", x = "reactions_per_msg_user", y = "Frequency") +
    theme_minimal()

pdf(paste0(output_folder, islandid, "_distributions_outliers.pdf"), width = 10, height = 5)  # Adjust width and height as needed
grid.arrange(p, q, ncol = 2)
dev.off()

# Close the PDF


# Close the database connectionint
dbDisconnect(con)



