
source("defs.R")


library(lubridate)
library(purrr)

anal_start <- as.POSIXct("2024-12-09 20:00:00")
anal_end <- as.POSIXct("2024-12-12 20:00:00")

con <- connect_db()

top_emoticons <- init_emos(con)


reaction_data <- dbGetQuery(con, "SELECT RH.channel_id, RH.message_id, HEX(RH.reaction_emo) as emo, count, RH.TS,
                              posted from reactions_history RH INNER JOIN messages M 
                            ON(RH.channel_id = M.channel_id AND RH.message_id = M.msg_id 
AND RH.channel_id = 1535416724 and msg_id = 75599                            
                            )")

reaction_data <- reaction_data %>%
  filter(posted > anal_start)

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

result <- reaction_data %>%
  mutate(hourly_end = floor(as.numeric(time_end))) %>%
  full_join(time_grid, by = c("channel_id", "message_id", "emo", "hourly_end")) %>%
  arrange(channel_id, message_id, emo, hourly_end) %>%
  group_by(channel_id, message_id, emo) %>%
  fill(count, .direction = "down") %>%
  ungroup()


write.csv(result,paste0(output_folder,"times.csv"))



dbDisconnect(con)
