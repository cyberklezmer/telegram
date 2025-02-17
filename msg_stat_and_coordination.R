# identifies top channles by reach, evaluates distribution of accepting/forwarding and makes a graph of proximity

# number of channels in graph and stats

library(ppcor)
library(broom)
library(lubridate)
library(forecast)
source("defs.R")

start_date <- as.Date("2024-08-02")
end_date <- as.Date("2024-09-29")


con <- connect_db()


catalogue_data <- get_catalogue(con,islandcondition)


top_channels <- get_top_channels(con, catalogue_data,10)


# Save to CSV file






agg_messages <- dbGetQuery(con,"SELECT 
    channel_id,
    DATE(posted) AS posted_date,
    COUNT(*) AS message_count,
    COUNT(src_channel_id) AS forwarded_count
FROM 
    messages
WHERE 
    posted IS NOT NULL -- Ensures we only include messages with a valid posted datetime
GROUP BY 
    channel_id, 
    DATE(posted)
ORDER BY 
    channel_id, 
    posted_date;
" )

agg_messages <- agg_messages %>%
  filter(posted_date >= start_date & posted_date <= end_date)


agg_messages <- agg_messages %>%
  left_join(dplyr::select(top_channels, channel_id, username), by = "channel_id")

agg_messages <- agg_messages %>%
  filter(channel_id %in% top_channels$channel_id)


# Ensure `username` follows the order in `top_channels`
agg_messages <- agg_messages %>%
  mutate(username = factor(username, levels = top_channels$username))

# 1. DRAW TIME SERIES PLOTS 

# Splitting each row into two rows
agg_messages_split <- agg_messages %>%
  rowwise() %>%
  mutate(
    not_forwarded_count = message_count - forwarded_count
  ) %>%
  dplyr::select(-message_count) %>%
  pivot_longer(
    cols = c(forwarded_count, not_forwarded_count),
    names_to = "forwarded",
    values_to = "count"
  ) %>%
  mutate(
    forwarded = case_when(
      forwarded == "forwarded_count" ~ "Forwarded",
      forwarded == "not_forwarded_count" ~ "Not forwarded"
    )
  ) %>%
  ungroup()




# Create the grid plot using facet_wrap with ordered username

agg_messages_split <- agg_messages_split %>%
  mutate(posted_date = as.Date(posted_date))

p <- ggplot(agg_messages_split, aes(x = posted_date, y = count, fill = forwarded)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ username, ncol = 5, scales = "free_x") +
  labs(title = "Message Time Series for Top Channels",
       x = "Date",
       y = "Number of Messages",
       fill = "Message Type") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold") # Increase title size
  )



# Save the plot as a PDF
ggsave(paste0(output_folder, islandid, "_top_channels_messages.pdf"), plot = p, width = 15, height = 10)



# Example: Assuming agg_messages has columns 'username', 'posted_date', and 'message_count'

# Ensure `posted_date` is in Date format
# agg_messages <- agg_messages %>%
#  mutate(posted_date = as.Date(posted_date),
#         message_count = if_else(message_count == 0, NA_integer_, message_count))

# Remove linear trend and day-of-week seasonality

# Define the complete date range

# 2. DETREND AND DESEASON THE SERIES 


agg_messages_complete <- agg_messages %>%
  mutate(posted_date = as.Date(posted_date, format = "%Y-%m-%d"))

date_range <- seq.Date(min(agg_messages_complete$posted_date), 
                       max(agg_messages_complete$posted_date), by = "day")

# Expand the data to include all dates for each username
agg_messages_complete <- agg_messages_complete %>%
  group_by(username) %>%
  complete(posted_date = date_range, fill = list(message_count = 0)) %>%
  ungroup()

# Check if there are any remaining missing values
agg_messages_complete <- agg_messages_complete %>%
  mutate(message_count = ifelse(is.na(message_count), 0, message_count))

agg_messages_complete <- agg_messages_complete %>%
  mutate(weekday = factor(weekdays(posted_date)))

# Ensure message_count is numeric
agg_messages_complete <- agg_messages_complete %>%
  mutate(message_count = as.numeric(message_count))



# Initialize an empty data frame to store coefficients
coefficients_data <- data.frame(
  username = character(),
  trend_intercept = numeric(),
  trend_slope = numeric(),
  weekday_coefficients = list(),
  stringsAsFactors = FALSE
)


# Initialize an empty list to store coefficients for each user
coefficients_list <- list()

agg_residuals <- agg_messages_complete %>%
  group_by(username) %>%
  do({
    data <- .
    
    # Remove linear trend and day-of-week seasonality
    trend_model <- lm(message_count ~ as.numeric(posted_date) + weekday, data = data)
    trend_removed <- residuals(trend_model)
    data$residuals <- ifelse(data$message_count == 0, NA, trend_removed)
    data$residuals_nn <- ifelse(data$message_count == 0, 0, trend_removed)
    
    # Extract coefficients and their standard errors
    trend_summary <- summary(trend_model)
    trend_coeff <- coef(trend_model)
    trend_se <- coef(trend_summary)[, "Std. Error"] # Correctly extract std errors for all coefficients
    trend_zs <- abs(trend_coeff/trend_se)
    
    # Prepare coefficients and standard errors as named lists
    coeffs <- as.list(trend_coeff)
    ses <- as.list(trend_se)
    zs <- as.list(trend_zs)
    names(ses) <- paste0("se_", names(coeffs))
    names(zs) <- paste0("z_", names(coeffs))
    
    # Combine all coefficients and standard errors into a single named list
    cl <- c(
      list(username = unique(data$username)), # Ensure username is unique
      coeffs,
      ses,
      zs
    )
    
    # Append coefficients to the list
    coefficients_list[[length(coefficients_list) + 1]] <<- cl
    
    data
  }) %>%
  ungroup()

# Convert list to a data frame
coefficients_data <- bind_rows(coefficients_list)

# Check the stored coefficients
print(coefficients_data)

write.csv(agg_residuals,paste0(output_folder,islandid,"_app_residuals.csv"))


# 3. COMPUTE PARTIAL AND TREU CORRELATIONS 

# Attach residuals back to the original table








agg_wide_nn <- agg_residuals %>%
  dplyr::select(posted_date, username, residuals_nn) %>%
  pivot_wider(names_from = username, values_from = residuals_nn, values_fill = 0)


write.csv(agg_wide_nn, file.path(output_folder, paste0(islandid, "_partial_correlation_data.csv")), row.names = TRUE)

# Remove the date column for correlation calculations
message_matrix <- agg_wide_nn %>%
  dplyr::select(-posted_date) %>%
  as.matrix()

pcor_results <- pcor(message_matrix)
partial_correlations <- pcor_results$estimate
p_values <- pcor_results$p.value


write.csv(partial_correlations, file.path(output_folder, paste0(islandid, "_partial_correlations.csv")), row.names = TRUE)

# Display the partial correlation matrix

# Define significance level
alpha <- 0.05

# Mask insignificant correlations
partial_correlations_masked <- ifelse(p_values < alpha, partial_correlations, NA)

# Convert the matrix to a long format for ggplot2
heatmap_data <- as.data.frame(as.table(partial_correlations_masked)) %>%
  filter(!is.na(Freq)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq)

# Create the heatmap
p <- ggplot(heatmap_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Partial Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "Heatmap of Significant Partial Correlations",
       x = "Channel (Username)", y = "Channel (Username)")

ggsave(paste0(output_folder, islandid, "_partial_heatmap.pdf"), plot = p, width = 15, height = 15)


# Convert to matrix and remove unnecessary columns

agg_wide <- agg_residuals %>%
  dplyr::select(posted_date, username, residuals) %>%
  pivot_wider(names_from = username, values_from = residuals, values_fill = 0)


message_matrix <- agg_wide %>%
  dplyr::select(-posted_date) %>%
  as.matrix()

# Compute Pearson correlation matrix
cor_results <- cor(message_matrix, use = "pairwise.complete.obs")

# Compute p-values for correlations
p_values <- matrix(NA, nrow = ncol(message_matrix), ncol = ncol(message_matrix))
colnames(p_values) <- colnames(message_matrix)
rownames(p_values) <- colnames(message_matrix)

for (i in 1:ncol(message_matrix)) {
  for (j in i:ncol(message_matrix)) {
    test <- cor.test(message_matrix[, i], message_matrix[, j])
    p_values[i, j] <- test$p.value
    p_values[j, i] <- test$p.value
  }
}

# Save the correlation matrix
write.csv(cor_results, file.path(output_folder, paste0(islandid, "_correlations.csv")), row.names = TRUE)

# Define significance level
alpha <- 0.05

# Mask insignificant correlations
correlations_masked <- ifelse(p_values < alpha, cor_results, NA)

# Convert matrix to long format for ggplot2
heatmap_data <- as.data.frame(as.table(correlations_masked)) %>%
  filter(!is.na(Freq)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq)

# Create the heatmap
p <- ggplot(heatmap_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14)
  ) +
  labs(title = "Heatmap of Significant Correlations",
       x = "Channel (Username)", y = "Channel (Username)")

# Save the heatmap
ggsave(paste0(output_folder, islandid, "_correlation_heatmap.pdf"), plot = p, width = 15, height = 15)


library(dplyr)
library(tidyr)

# Create a matrix to store significant positive correlations
significant_cor <- (cor_results > 0) & (p_values < 0.05)

# Create a matrix to store significant positive partial correlations
significant_pcor <- (partial_correlations > 0) & (pcor_results$p.value < 0.05)

# Create a dataframe with counts for each row (variable)
cor_summary <- data.frame(
  Variable = colnames(message_matrix),
  Positive_Significant_Cor = rowSums(significant_cor, na.rm = TRUE)-1,
  Positive_Significant_Partial_Cor = rowSums(significant_pcor, na.rm = TRUE)-1
)

# Write the results to a CSV file
write.csv(cor_summary, file.path(output_folder, paste0(islandid, "_msg_stat_and_coordination_res.csv")), row.names = FALSE)


# Close the database connectionint
dbDisconnect(con)



