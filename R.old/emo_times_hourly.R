
source("defs.R")


library(lubridate)
library(purrr)

anal_start <- as.POSIXct("2025-01-30 00:00:00")
last_messages<- as.POSIXct("2025-01-30 20:00:00")
anal_end <- as.POSIXct("2025-02-01 10:00:00")

con <- connect_db()

top_emoticons <- init_emos(con)


reaction_data <- dbGetQuery(con, "SELECT RH.channel_id,  RH.message_id, HEX(RH.reaction_emo) as emo, count, RH.TS,
                              posted from reactions_history RH INNER JOIN messages M 
                            ON(RH.channel_id = M.channel_id AND RH.message_id = M.msg_id 
                            )")

reaction_data <- reaction_data %>%
  filter(posted > anal_start & posted < last_messages)

reaction_data <- reaction_data %>%
  arrange(channel_id, message_id, emo, TS) %>%
  group_by(channel_id, message_id) %>%
  mutate(
    time_end = difftime(TS, posted, units = "hours") 
    #, 
    #    time_diff = ifelse(is.na(lag(TS)) , 
    #                       difftime(TS, posted, units = "days"), 
    #                     difftime(TS, lag(TS), units = "days")),
    #    count_diff = count - lag(count, default = 0) 
  ) %>%
  ungroup()

reaction_data <- reaction_data %>%
  group_by(channel_id, message_id) %>%
  mutate(censor_point = difftime(anal_end,posted, units = "hours")) %>%
  ungroup()


time_grid <- reaction_data %>%
  distinct(channel_id, message_id, emo,censor_point) %>%
  mutate(hourly_times = map(censor_point, ~ seq(1, floor(.x), by = 1))) %>%
  unnest(hourly_times) %>%
  rename(hourly_end = hourly_times)


# Calculate time differences and count increments






# Calculate time differences and count increments, including hourly increases
result <- reaction_data %>%
  mutate(hourly_end = floor(as.numeric(time_end))) %>%
  group_by(channel_id, message_id, emo, hourly_end) %>%
  slice_max(order_by = TS, n = 1, with_ties = FALSE) %>%  # Keep the last row based on TS
  ungroup() %>%
  select(channel_id,message_id, emo, hourly_end,count)%>%
  right_join(time_grid, by = c("channel_id", "message_id", "emo", "hourly_end")) %>%
  arrange(channel_id, message_id, emo, hourly_end) %>%
  group_by(channel_id, message_id, emo) %>%
  fill(count, .direction = "down") %>% # Fill missing values in counts
  mutate(hourly_increase = count - lag(count, default = 0)) %>% # Calculate hourly increase
  ungroup()

# Write the result to a CSV
write.csv(result, paste0(output_folder, islandid,"_emo_times.csv"))

result$emo_UTF8 <- hex_to_text(result$emo)

# Iterate through each channel_id and message_id combination
result %>%
  group_by(channel_id, message_id) %>%
  group_walk(~ {
    data <- .x
    
    channel_id <- .y$channel_id
    message_id <- .y$message_id
    print(paste("Processing Channel:", channel_id, "Message:", message_id)) # Debug info
    
    
    # Create the bar graph
    p <- ggplot(data, aes(x = hourly_end, y = hourly_increase, fill = emo_UTF8)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = paste0("Channel: ", channel_id, 
                       " | Message: ", message_id),
        x = "Hour",
        y = "Hourly Increase",
        fill = "Emoticon"
      ) +
      theme_minimal()
    
    # Save the plot to a PNG file
    output_file <- paste0(
      graph_output_folder, 
      "channel_", channel_id, 
      "_message_", message_id, ".png"
    )
    ggsave(output_file, plot = p, width = 10, height = 6)
  })


channels <- dbGetQuery(con, "SELECT * FROM channels")

mychannels <- result  %>% distinct(channel_id) %>%
                left_join(channels,by = c("channel_id" = "id" ))

for (i in 1:nrow(mychannels)) {
  
  username <- mychannels$username[i]
  
    html_text <-result %>%
      filter(channel_id == mychannels$channel_id[i]) %>%
       group_by(channel_id, message_id) %>%
      mutate(
         message_url = paste0("https://t.me/", username, "/", message_id),
          embed_code = paste0(
        "<script async src=\"https://telegram.org/js/telegram-widget.js?22\" data-telegram-post=\"",
        username, "/", message_id, "\" data-width=\"100%\"></script> "
      ),
      message_link = sprintf("<a href='%s' target='_blank'>link</a>", message_url),
      censor_point = round(censor_point),
      info_code = paste0(
        "URL ", message_link,
        "<br>Len ", censor_point
      ),
      image_cell = paste0("<img src=\"",graph_subfolder,"channel_", unique(channel_id), 
                     "_message_", unique(message_id), ".png\" alt=\"histogram\" width=\"600\">")
    ) %>% 
      ungroup()  %>% 
      distinct(info_code, embed_code,image_cell)
  
  

    library(kableExtra)
    
    html_table <- html_text %>%
      kable(
        format = "html",
        escape = FALSE,
        col.names = c("Info", "Message", "Emos")
      ) %>%
      kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "hover", "condensed")
      ) %>%
      column_spec(1, extra_css = "vertical-align: top;") %>% 
      column_spec(2, extra_css = "vertical-align: top; width: 40%;") %>% # Set consistent width
      column_spec(3, extra_css = "vertical-align: top; width: 40%;")     # Set consistent width
    

  # Step 2: Add the concatenated URL column
  # Assuming 'username' and 'message_id' are columns in the original dataset or grouped_data
  html_content <- paste0(
    "<!DOCTYPE html><html><head><title>", username, "</title></head><body>",
    "<h1>", username, "</h1>",
    as.character(html_table),
    "</body></html>"
  )

    output_file <- file.path(output_folder, paste0(islandid,"_emo_time_hourly_", username, ".html"))
    writeLines(html_content, output_file)  
}



dbDisconnect(con)
