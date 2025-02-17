
inisig_hist <- 20

test_treshold <- 2.5
orange_ratio <- 3

list_threshold <- 5
chnumber <- 5
 
min_src_size <- 50


source("defs.R")

con <- connect_db()

catalogue_data <- get_catalogue(con,islandcondition)


compute_mode <- function(x) {
  uniq_x <- unique(x)            # Get unique values
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Find the most frequent value
}

# Connect to MySQL database

# Query the database for relevant data
# catalogue_data <- dbGetQuery(con, "SELECT channel_id, username, CONVERT(name USING ASCII) as name, subscribers, catalogue_count,
#             msg_count, reaction_count, forwarded_from, forwarded_to, lang FROM channels_info WHERE (source = 'slerka')")


top_channels_to <- catalogue_data %>%
  arrange(desc(forwarded_to)) %>%
  head(chnumber)

top_channels_from <- catalogue_data %>%
  arrange(desc(forwarded_from)) %>%
  head(chnumber)

top_channels_reach <- get_top_channels(con, catalogue_data,chnumber)


top_channels <- bind_rows(top_channels_reach, top_channels_to, top_channels_from) %>%
  distinct()



# Loop through each row in top_channels

  forwards_data <- dbGetQuery(con, "SELECT * from forwards")
  
  forwards_data <- forwards_data %>%
    inner_join(top_channels, by = c("channel_id" = "channel_id")) %>%
    rename(dst_username = username )  %>%
    inner_join(top_channels, by = c("src_channel_id" = "channel_id")) %>%
    rename(src_username = username )  
  
  
  forwards_data <- forwards_data %>%
    mutate(log_forward_time = log10(as.numeric(difftime(posted, src_posted, units = "secs"))),
           cat_forward_time = ceiling(log_forward_time*2)/2) %>%
    select(src_username, dst_username, log_forward_time,cat_forward_time)

  output_csv <- paste0(output_folder, islandid, "_forward_data.csv")
  write.csv(forwards_data,output_csv)  
  
    
  summary_table <- forwards_data %>%
    group_by(src_username, dst_username) %>%            # Group by src_channel_id and channel_id
    filter(n() >= list_threshold) %>%                               # Keep groups with at least 10 records
    summarise(
      mean_forward_time = mean(log_forward_time, na.rm = TRUE), # Mean forward time
      mode_forward_time = compute_mode(cat_forward_time), # Mean forward time
      sd_forward_time = sd(log_forward_time, na.rm = TRUE),     # Standard deviation of forward time
      n_records = n(),    
      # Number of records
      ratio_below_t = sum(log_forward_time < test_treshold, na.rm = TRUE) / n(),
      # Number of records
      .groups = "drop"                                      # Ungroup after summarising
    )  %>%                               
 mutate( left_ratio = ratio_below_t / pnorm(test_treshold, mean = mean_forward_time, sd = sd_forward_time, lower.tail = TRUE, log.p = FALSE),
         bar_color = if_else(n_records < inisig_hist,"grey", 
                                    if_else(mode_forward_time <= 2, "black",
                                            if_else(left_ratio > orange_ratio,"red", "darkgreen")))
         )

    output_csv <- paste0(output_folder, islandid, "_forward_time_summary.csv")
  write.csv(summary_table,output_csv)  


  # Filter data for combinations with at least 50 rows
  filtered_data <- forwards_data %>%
    group_by(src_username, dst_username) %>%
    filter(n() >= list_threshold) %>%
    ungroup()

  
  output_pdf <- paste0(output_folder, islandid, "_forward_time_histograms_grid.pdf")


  # Add a flag for small datasets if needed

  merged_data <- filtered_data %>%
    left_join(summary_table %>%
                select(src_username, dst_username, mode_forward_time, n_records,left_ratio, bar_color),
              by = c("src_username", "dst_username")) 
  
  # Add a flag for small datasets if needed
  if (nrow(merged_data) == 0) {
    stop("No combinations with at least 50 rows found.")
  }
  
  
  # Create a faceted plot with color based on mode_forward_time
  # Load necessary libraries

  
  # Define output folder for PDF

    # Add missing '1' for cat_forward_time for each combination

  write.csv(merged_data,paste0(output_folder,"data_for_grid.csv"))
  

  # Create a new df grouped by src_username and dst_username with the first bar_color
  df_grouped <- merged_data %>%
    group_by(src_username, dst_username) %>%
    summarise(bar_color = first(bar_color), .groups = "drop")
  
  df_count <- df_grouped %>%
    group_by(src_username) %>%
    summarise(
      black_count = sum(bar_color == "black"),
      red_count = sum(bar_color == "red"),
      .groups = "drop"
    )
  
  output_file_name <- paste0(output_folder, islandid, "_forwards_times_res.csv")

  write.csv(df_count,output_file_name)
  
    
  output_pdf <- paste0(output_folder, islandid, "_forward_time_histograms_colored_start1.pdf")
  
    
  p <- ggplot(merged_data, aes(x = cat_forward_time, fill = bar_color)) +
    geom_histogram(stat = "count", binwidth = 1, color = "white", alpha = 0.7) +
    scale_fill_manual(
      values = c("red" = "red", "darkgreen" = "darkgreen", "grey" = "grey", "black" = "black"),
      labels = c("red" = paste0("<",orange_ratio, "x LP Norm"), "darkgreen" = "Normal", "grey" = "Insig", "black" = "Mode <=2")
    ) +
    scale_x_continuous(breaks = 0:8, limits = c(0, 8)) +
    labs(
      x = "t",
      y = "freq",
      fill = "Risk Level"  # Custom legend title
    ) +
    theme_minimal() +
    facet_grid(rows = vars(src_username), cols = vars(dst_username), scales = "free") +
    theme(
      strip.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 0)
    )
  
  # Save the plot to a PDF


  
  ggsave(output_pdf, plot = p, width = 17, height = 12)
  
    
  
  



dbDisconnect(con)
