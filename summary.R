# identifies top channles by reach, evaluates distribution of accepting/forwarding and makes a graph of proximity

# number of channels in graph and stats


# treshold for displaying of a line in graph

source("defs.R")

con <- connect_db()


catalogue_data <- get_catalogue(con,islandcondition)

top_channels <- get_top_channels(con, catalogue_data, 10) %>%
  select(username)

cor_data <- read.csv(paste0(output_folder,islandid,"_msg_stat_and_coordination_res.csv"))


# Perform the left join
merged_data <- top_channels %>%
  left_join(cor_data, by = c("username" = "Variable"))

ent_data <- read.csv(paste0(output_folder,islandid,"_concentration_res.csv"))

merged_data <- merged_data %>%
  left_join(ent_data, by = "username")

fwd_data <- read.csv(paste0(output_folder,islandid,"_forwards_times_res.csv"))

merged_data <- merged_data %>%
  left_join(fwd_data, by = c("username" = "src_username"))

out_data <- read.csv(paste0(output_folder,islandid,"_outliers_res.csv"))
merged_data <- merged_data %>%
  left_join(out_data, by ="username") %>%
  select(username, Positive_Significant_Cor, Positive_Significant_Partial_Cor, 
         entropy,  black_count, red_count,  reactions_per_msg_user_outlier_prob ,
         accepted_outlier_prob)


# Function to apply min-max normalization
min_max_norm <- function(x) {
  if (all(is.na(x))) {
    return(x)  # Return as is if all values are NA
  }
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  
  if (min_x == max_x) {
    return(rep(0, length(x)))  # Avoid division by zero; return all zeros if min == max
  }
  
  (x - min_x) / (max_x - min_x)
}

# Apply normalization to all numeric columns except 'username'
merged_data <- merged_data %>%
  mutate(across(where(is.numeric), min_max_norm)) %>%
  mutate(entropy = 1- entropy)

# View the transformed data
merged_data <- merged_data %>%
  mutate(score = rowMeans(across(where(is.numeric)), na.rm = TRUE))

# Sort by row_mean in descending order
result_data <- merged_data %>%
  arrange(desc(score)) %>%
  select(username, score)

result_data <- result_data %>%
  mutate( username = gsub("_", "\\\\_", username), 
    score = paste0(round(score * 100)," \\%"))

# Convert to LaTeX table
latex_table <- xtable(result_data, caption = "Top Channels by Score", label = "tab:top_channels")

# Save LaTeX table to a file
output_file <- paste0(output_folder,islandid, "_summary_result.tex")
sink(output_file)
print(latex_table, include.rownames = FALSE, booktabs = TRUE, sanitize.text.function = identity)
sink()


# View the sorted data
head(merged_data)


# View the result


# Close the database connectionint
dbDisconnect(con)




