# identifies top channles by reach, evaluates distribution of accepting/forwarding and makes a graph of proximity

# number of channels in graph and stats

library(dplyr)
library(ggplot2)
library(entropy)
library(gridExtra)



# Function to calculate entropy
calculate_entropy <- function(counts, K) {
  
    n <- sum(counts)
    p <- counts / n
    
    # MLE (empirical) entropy in natural log
    H_mle <- -sum(p * log(p), na.rm = TRUE)
    
    # Miller–Madow correction term
 #   H_mm <- H_mle + (K - 1) / (2 * n)
    H_penalized <- H_mle + K / n
    return(H_penalized)
  }


# Data preprocessing


source("defs.R")

con <- connect_db()



messages_df <- dbGetQuery(con,"SELECT channel_id, src_channel_id, count(msg_id) as num_msgs FROM messages WHERE src_channel_id IS NOT NULL GROUP BY channel_id, src_channel_id")

K <- 10

# Join the data

catalogue_data <- get_catalogue(con,islandcondition)

top_channels <- get_top_channels(con, catalogue_data,10)

# Compute entropy for each channel_id
entropy_data <- messages_df %>%
  group_by(channel_id) %>%
  summarize(
    entropy = calculate_entropy(num_msgs,K),
    nsources = n_distinct(src_channel_id),
    forwarded_msgs = sum(num_msgs),
    .groups = "drop"
  )





entropy_data <- entropy_data %>%
  inner_join(top_channels, by = "channel_id")  %>%
  arrange(entropy)
             
result_data <- entropy_data %>% dplyr::select(username, entropy)

write.csv(result_data,paste0(output_folder,islandid,"_concentration_res.csv"))

# Assuming messages_df is your data frame with columns channel_id and msg_count
# If not already done, group the data and calculate the frequencies

# Calculate proportions for msg_count and preprocess data
plot_df <- messages_df %>%
  left_join(entropy_data, by = "channel_id") %>%
  dplyr::select(username, entropy, num_msgs) %>%
  group_by(username) %>%
  mutate(
    total_msgs = sum(num_msgs),
    proportion = num_msgs / total_msgs,
    msg_count_rank = dense_rank(-num_msgs)
  ) %>%
  arrange(entropy, msg_count_rank) %>%
  ungroup()


# Function to generate histogram for a specific username
plot_histogram <- function(username, data) {
  user_data <- data %>% filter(username == !!username)
  
  ggplot(user_data, aes(x = msg_count_rank, y = proportion)) +
    geom_bar(stat = "identity", fill = "blue", color = "black") +
    ggtitle(paste0(substr(username, 1, 11), ": ", 
                   round(user_data$entropy[1], 2), 
                   " (", user_data$num_msgs[1], ")")) +
    xlab("Message Count Rank") +
    ylab("Proportion of Total Messages")
}

# Get unique usernames
usernames <- unique(plot_df$username) %>% head(60)

# Generate a list of plots
plot_list <- lapply(usernames, function(user) {
  plot_histogram(user, plot_df)
})

# Save plots to PDF
pdf(paste0(output_folder,islandid,"_top_entropy_histograms.pdf"), width = 11, height = 8) # Adjust dimensions if needed
grid.arrange(grobs = plot_list, ncol = 4) # Adjust ncol for grid arrangement
dev.off()




dbDisconnect(con)



