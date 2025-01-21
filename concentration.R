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
    H_penalized <- H_mle + 5 / n
    return(H_penalized)
  }


# Data preprocessing


source("defs.R")

con <- connect_db()



messages_df <- dbGetQuery(con,"SELECT channel_id, src_channel_id, count(msg_id) as num_msgs FROM messages WHERE src_channel_id IS NOT NULL GROUP BY channel_id, src_channel_id")

K <- n_distinct(messages_df$src_channel_id)

# Join the data


# Compute entropy for each channel_id
entropy_data <- messages_df %>%
  group_by(channel_id) %>%
  summarize(
    entropy = calculate_entropy(num_msgs,K),
    nsources = n_distinct(src_channel_id),
    forwarded_msgs = sum(num_msgs),
    .groups = "drop"
  )




catalogue_data <- get_catalogue(con,islandcondition)

entropy_data <- entropy_data %>%
  left_join(catalogue_data, by = "channel_id")  %>%
  arrange(entropy)
             
write.csv(entropy_data,paste0(output_folder,islandid,"_entropy_data.csv"))

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
  ggplot(data %>% filter(username == !!username), aes(x = msg_count_rank, y = proportion)) +
    geom_bar(stat = "identity", fill = "blue", color = "black") +
    ggtitle(paste( username, " : ")) +
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
pdf(paste(output_folder,islandid,"_top_entropy_histograms.pdf"), width = 11, height = 50) # Adjust dimensions if needed
grid.arrange(grobs = plot_list, ncol = 4) # Adjust ncol for grid arrangement
dev.off()




dbDisconnect(con)



