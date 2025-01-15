
source("defs.R")


forward_threshold <- 50


con <- connect_db()

top_emoticons <- init_emos(con)

top_channels <- init_channels(con, 50)

forwards_data <- init_forwards(con,top_channels)

forward_list <- forwards_data %>%
  group_by(src_username, dst_username,src_channel_id,channel_id) %>%            # Group by src_channel_id and channel_id
  filter(n() >= forward_threshold) %>%
  select(src_username, dst_username, src_channel_id, channel_id) %>%
  distinct()         

for (i in 1:nrow(forward_list)) {
  
  src_username <- forward_list$src_username[i]
  
  dst_username <- forward_list$dst_username[i]
  
  src_channel <- forward_list$src_channel_id[i]
  
  dst_channel <- forward_list$channel_id[i]
  
  forwarded_messages <- forwards_data %>%
    filter(src_channel_id == src_channel & channel_id == dst_channel)
  

  reactions_src <- get_reactions(con, src_channel, top_emoticons) 
#  sqltext <- paste0("SELECT *, HEX(reaction_emo) as hex_r_emo FROM message_reactions WHERE channel_id=", this_channel_id)
#  reactions_data <- dbGetQuery(con, sqltext)

  reactions_src_grouped  <- group_reactions(reactions_src)

  
  reactions_dst <- get_reactions(con, dst_channel, top_emoticons) 
  
  reactions_dst_grouped  <- group_reactions(reactions_dst)

  


  forwarded_messages  <- forwarded_messages %>% 
    left_join(reactions_src_grouped, by = c("src_msg_id" = "message_id")) %>% 
    rename(src_positivity = positivity, src_count = count)%>% 
    left_join(reactions_dst_grouped, by = c("msg_id" = "message_id")) %>% 
    rename(dst_positivity = positivity, dst_count = count)

  forwarded_messages_summary <- forwarded_messages %>% 
    summarize(total_src_count = sum(src_count, na.rm = TRUE),
              total_src_positivity = sum(src_positivity, na.rm = TRUE),
              total_dst_count = sum(dst_count, na.rm = TRUE),
              total_dst_positivity = sum(dst_positivity, na.rm = TRUE),
              .groups = 'drop')

  
    src_average = forwarded_messages_summary$total_src_positivity / forwarded_messages_summary$total_src_count

    dst_average = forwarded_messages_summary$total_dst_positivity / forwarded_messages_summary$total_dst_count

    forwarded_messages_summary <- forwarded_messages %>% 
      mutate(src_score = (src_positivity / src_count - src_average) * sqrt(src_count),
             dst_score = (dst_positivity / dst_count - dst_average) * sqrt(dst_count))
    
    corela <- cor(forwarded_messages_summary$src_score,forwarded_messages_summary$dst_score, 
         use = "pairwise.complete.obs", method = "pearson")

    print(paste0(src_username, " -> ", dst_username, " = ", nrow(forwarded_messages)," ", corela))
    
    
}
  
dbDisconnect(con)
