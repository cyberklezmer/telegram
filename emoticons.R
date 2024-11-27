library(DBI)
library(psych)
library(factoextra)
library(gridExtra)
library(RMySQL)
library(dplyr)
library(stringr)
library(xtable)
library(knitr)
library(ggplot2)
library(tidyr)
library(stringr)
library(igraph)
library(ggraph)
library(tibble)
library(systemfonts)
library(png)
library(grid)
library(kableExtra)
library(knitr)

chnumber <- 2


list_messages <- function(username, column, desc) {
  
  col <- sym(column) 
  
  
  if(desc)
  {
    idstr <- paste0(column,"_desc")
    top_messages <- message_totals %>%
      arrange(desc(!!col)) %>% # Sort by total_reactions_count in descending order
      slice(1:10)                             # Select the top 10 messages
  }
  else
  {
    idstr <- paste0(column,"_asc")
    top_messages <- message_totals %>%
      arrange(desc(-!!col)) %>% # Sort by total_reactions_count in descending order
      slice(1:10)                             # Select the top 10 messages
  }
  # Assuming 'top_messages' is your data frame
  top_messages <- top_messages %>%
    mutate(
      message_url = paste0("https://t.me/", username, "/", message_id),
      embed_code = paste0(
        "<script async src=\"https://telegram.org/js/telegram-widget.js?22\" data-telegram-post=\"",
        username, "/", message_id, "\" data-width=\"100%\"></script> "
      ),
      message_link = sprintf("<a href='%s' target='_blank'>%s</a>", message_url, message_url),
      reaction_per_subscriber = round(total_reactions_count / nsubscribers, 2),
      ppos = round(total_positive / reactions_with_sentiment, 2),
      pneg = round(total_negative / reactions_with_sentiment, 2),
      info_code = paste0(
        "RPS ", reaction_per_subscriber,
        "<br>Pos ", round(positiveness, 2),
        "<br>Pol ", round(polarization, 2),
        "<br>Ppos ", ppos,
        "<br>Pneg ", pneg
      )
    ) %>%
    select(info_code, embed_code,all_emoinfo)
  
  # Generate the HTML table
  html_table <- top_messages %>%
    kable(
      format = "html",
      escape = FALSE,
      col.names = c("Info", "Message", "Emos")
    ) %>%
    kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "hover", "condensed")
    ) %>%
    column_spec(1, extra_css = "vertical-align: top;") %>% # Align Info column to the top
    column_spec(2, extra_css = "vertical-align: top;") %>%    # Align Message column to the top
    column_spec(3, extra_css = "vertical-align: top;")    
  
  # Step 2: Add the concatenated URL column
  # Assuming 'username' and 'message_id' are columns in the original dataset or grouped_data
  
  
  # Step 4: Write HTML to file
  writeLines(as.character(html_table), paste0(output_folder,username,"msgs_by_",idstr,".html") )  
}

hex_to_text <- function(hex_strings) {
  # Apply the conversion for each element in the vector
  sapply(hex_strings, function(hex_string) {
    # Split the hexadecimal string into pairs of two characters
    hex_pairs <- substring(hex_string, seq(1, nchar(hex_string), 2), seq(2, nchar(hex_string), 2))
    
    # Convert the hex pairs to raw values
    raw_values <- as.raw(strtoi(hex_pairs, base = 16))
    
    # Convert the raw values to a character string
    rawToChar(raw_values)
  })
}



# Define output folder for CSV and PDF
output_folder <- "out/"

# Connect to MySQL database
con <- dbConnect(
  MySQL(),
  host = "bethel.utia.cas.cz",
  user = "jak",
  password = "jaknajaka",
  dbname = "telegram_full"
)

# Query the database for relevant data
# catalogue_data <- dbGetQuery(con, "SELECT channel_id, username, CONVERT(name USING ASCII) as name, subscribers, catalogue_count,
#             msg_count, reaction_count, forwarded_from, forwarded_to, lang FROM channels_info WHERE (source = 'slerka')")


catalogue_data <- dbGetQuery(con, "SELECT channel_id, username, CONVERT(name USING ASCII) as name, subscribers, catalogue_count,
             msg_count, reaction_count, forwarded_from, forwarded_to, lang FROM channels_info WHERE (lang = 'CZECH' or lang = 'SLOVAK')")

catalogue_data <- catalogue_data %>%
  mutate(subscribers = if_else(
    str_detect(subscribers, "K"),
    as.numeric(str_replace(subscribers, "K", "")) * 1000,
    if_else(
      str_detect(subscribers, "M"),as.numeric(str_replace(subscribers, "M", "")) * 1000000, as.numeric(subscribers))
  ))

catalogue_data <- catalogue_data %>%
  mutate(reach_own = subscribers * msg_count)

top_channels <- catalogue_data %>%
  arrange(desc(reach_own)) %>%
  head(chnumber)

emoticons =  dbGetQuery(con, "SELECT *, HEX(emoticon) as hex_emo, HEX(unified_emo) as hex_u_emo FROM emoticons")

print(emoticons)

emo_sentiments <- read.csv("emo_sentiments.csv")

# Add columns based on counts or specific logic
emo_sentiments <- emo_sentiments %>%
  mutate(
    negative_norm = Negative/ Occurrences,
    neutral_norm = Neutral / Occurrences,
    positive_norm = Positive / Occurrences
  )


print(emo_sentiments)
      
top_emoticons <- read.csv(file.path(output_folder, "top_emoticons_99.csv"))

  
top_emoticons <- top_emoticons %>%
  left_join(emoticons, by = c("emo" = "hex_emo"))


top_emoticons <- top_emoticons %>%
  group_by(hex_u_emo,name) %>%
  summarize(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  select(count, hex_u_emo, name)

top_emoticons <- top_emoticons %>%
    left_join(emo_sentiments, by = c("hex_u_emo" = "hexcode"))

distinct_counts <- emoticons %>%
  summarise(across(everything(), ~ n_distinct(.)))

print(distinct_counts)



# Create a ranking based on descending "count" from top_emoticons
top_emoticons <- top_emoticons %>%
  arrange(desc(count)) %>%  # Sort descending by count
  mutate(rank = row_number())  # Add rank for sorting

write.csv(top_emoticons, "top_emotions.csv")


# Add required library for table rendering

global_catalogue_table <- data.frame()

# Loop through each row in top_channels
for (i in 2:nrow(top_channels)) {
  
  username <- top_channels$username[i]
  this_channel_id <- top_channels$channel_id[i]
  
  print(username)
  
  # Filter the row from catalogue_data matching the current channel_id
  channel_data <- catalogue_data %>% filter(channel_id == this_channel_id)
  
  nsubscribers <- channel_data$subscribers
  
  # Query reactions data
  sqltext <- paste0("SELECT *, HEX(reaction_emo) as hex_r_emo FROM message_reactions WHERE channel_id=", this_channel_id)
  reactions_data <- dbGetQuery(con, sqltext)

  # Joining reactions_data with emoticons
  merged_data <- reactions_data %>%
    left_join(emoticons, by = c("hex_r_emo" = "hex_emo"))
  

  # Group and summarize
  grouped_data <- merged_data %>%
    mutate(hex_u_emo = if_else(is.na(hex_u_emo), "other", hex_u_emo)) %>%
    group_by(message_id,hex_u_emo) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop')

  grouped_data <- grouped_data %>%
    left_join(emo_sentiments, by = c("hex_u_emo" = "hexcode"))
  
  grouped_data <- grouped_data %>%
    mutate(
      neutral = neutral_norm * count,
      positive = positive_norm * count,
      negative = negative_norm * count
    )
  

  grouped_data_summary <- grouped_data %>%
    summarize(
      messages_reacted_to = n_distinct(message_id),
      reaction_rows = n(),                                      # Count total rows
      reactions_with_sentiment = sum(if_else(!is.na(neutral_norm), count, 0), na.rm = TRUE), # Summing counts with condition
      total_reactions_count = sum(count, na.rm = TRUE),
      total_neutral = sum(neutral, na.rm = TRUE),
      total_negative = sum(negative, na.rm = TRUE),
      total_positive = sum(positive, na.rm = TRUE)
    )

  
  
  # Filter and prepare data for visualization
  filtered_grouped_data <- grouped_data %>%
    mutate(hex_u_emo = if_else(
      hex_u_emo %in% top_emoticons$hex_u_emo,
      hex_u_emo, 
      "other"
    )) %>%
    group_by(hex_u_emo, message_id) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
    left_join(top_emoticons %>% select(hex_u_emo, rank), by = "hex_u_emo") %>%
    mutate(rank = if_else(is.na(rank), Inf, rank)) %>%
    arrange(rank, desc(count)) %>%
    select(-rank)
  

  
  reaction_summary <- filtered_grouped_data %>%
    pivot_wider(
      names_from = hex_u_emo,
      values_from = count,
      values_fill = list(count = 0)
    ) %>%
    ungroup()
  
  # Save summarized data to CSV
  write.csv(reaction_summary, paste0(output_folder, username, "_reaction_summary.csv"), row.names = FALSE)
  
  # Generate histogram plot
  reaction_totals <- colSums(reaction_summary[-1])

  reaction_totals_df <- data.frame(
    Reaction = names(reaction_totals),
    Total = as.numeric(reaction_totals)
  )
  
  reaction_totals_df$Reaction_UTF8 <- hex_to_text(reaction_totals_df$Reaction)
  
  # Create plot
  png_file <- paste0(output_folder, username , "_hist.png")
  png(file = png_file)
  reaction_totals_df <- reaction_totals_df %>%
    mutate(Reaction_UTF8 = factor(Reaction_UTF8, levels = Reaction_UTF8))
  p <- ggplot(reaction_totals_df, aes(x = Reaction_UTF8, y = Total)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = "Total Reactions by Type",
      x = "Reaction Type",
      y = "Total Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(family = "AppleColorEmoji", hjust = 1))
  print(p)
  dev.off()
  
  # Read the generated PNG
  raster_image <- readPNG(png_file)
  
  # Create a table from the corresponding row in catalogue_data
  
  
  # Convert all columns to character to handle mixed types
  channel_data <- channel_data %>%
    mutate(
      reacts_per_msg = reaction_count / msg_count,
      rpms = reacts_per_msg / subscribers
    )
  
  channel_data <- channel_data %>%
    mutate(across(everything(), as.character))
  
  # Transform the row into a long format
  channel_data_long <- channel_data %>%
    pivot_longer(cols = everything(), 
                 names_to = "Label", 
                 values_to = username)

  grouped_data_summary <- grouped_data_summary %>%
    mutate(
      positiveness = (total_positive - total_negative) / reactions_with_sentiment,
      neutrality = total_neutral / reactions_with_sentiment,
      polarization = sqrt((total_positive + total_negative) / reactions_with_sentiment - positiveness)
    )
  
  grouped_data_summary <- grouped_data_summary %>%
    select(-c(total_positive,total_negative,total_neutral))
      
  grouped_data_long <- grouped_data_summary %>%
    pivot_longer(cols = everything(), names_to = "Label", values_to = username) %>%
    mutate(across(everything(), as.character))
  
  # Combine grouped_data_summary with channel_data_long
  extended_data <- bind_rows(
    channel_data_long,
    grouped_data_long
  )
  

  # Create a tableGrob from the extended data
  catalogue_table <- tableGrob(extended_data, rows = NULL, theme = ttheme_default())
  
  
# Generate PDF with the updated table
  pdf(file = paste0(output_folder, username, "_info.pdf"), width = 8, height = 10)
  grid.newpage()

  # Add plot
  grid.raster(raster_image)

  # Add the updated table
  grid.newpage()
  grid.draw(catalogue_table)

  dev.off()

  # Create a tableGrob from the extended data
  catalogue_table <- tableGrob(extended_data, rows = NULL, theme = ttheme_default())
  

  pdf(file = paste0(output_folder, username, "_info.pdf"), width = 8, height = 10)
  grid.newpage()
  
  # Add plot
  grid.raster(raster_image)
  
  # Add table
  grid.newpage()
  grid.draw(catalogue_table)
  
  grouped_data <-grouped_data %>%
    mutate(
      emoinfo = paste0("<br>",hex_to_text(hex_u_emo)," ",count)
    )

  message_totals <- grouped_data %>%
    group_by(message_id) %>%
    summarize(
      reaction_rows = n(),                                      # Count total rows
      reactions_with_sentiment = sum(if_else(!is.na(neutral_norm), count, 0), na.rm = TRUE), # Summing counts with condition
      # Count non-NA values in neutral
      total_reactions_count = sum(count, na.rm = TRUE),
      total_neutral = sum(neutral, na.rm = TRUE),
      total_negative = sum(negative, na.rm = TRUE),
      total_positive = sum(positive, na.rm = TRUE),
      all_emoinfo = toString(unique(emoinfo)) 
    )
  
  message_totals <- message_totals %>%
    mutate(
      positiveness = (total_positive - total_negative) / reactions_with_sentiment,
      neutrality = total_neutral / reactions_with_sentiment,
      polarization = sqrt((total_positive + total_negative) / reactions_with_sentiment - positiveness)
    )

  if(nrow(message_totals) >= 1)  {
    
    list_messages(username,"total_reactions_count", TRUE )    
    
    list_messages(username,"positiveness", TRUE )    

    list_messages(username,"positiveness", FALSE )    
    
        # Step 1: Filter top 10 messages by total_reaction_count
  }
  
  
  if (nrow(global_catalogue_table) == 0) {
    global_catalogue_table <- extended_data
  } else {
    global_catalogue_table <- bind_cols(global_catalogue_table, extended_data[,2])
  }
}

write.csv(global_catalogue_table, file.path(output_folder, "global_catalogue_table_wide.csv"), row.names = FALSE)


dbDisconnect(con)
