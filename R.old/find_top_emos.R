source("defs.R")



con <- connect_db()

# Query the database for relevant data
emoticons <- dbGetQuery(
  con,
  "SELECT unified_emo, emo_name, HEX(unified_emo) AS emo, SUM(count) AS count 
   FROM message_reactions_info  
   WHERE lang='CZECH' OR lang='SLOVAK'
   GROUP BY unified_emo 
   ORDER BY count DESC"
)

# Calculate cumulative percentage and filter for top 99%
emoticons_top <- emoticons %>%
  mutate(cumulative_sum = cumsum(count), # Cumulative sum
         total_sum = sum(count),         # Total sum
         cumulative_percent = cumulative_sum / total_sum * 100) %>%
  filter(cumulative_percent <= 99)      # Select rows within 99% cumulative total

# If you want to save the output as a CSV
write.csv(emoticons_top, file.path(output_folder, "top_emoticons_99.csv"), row.names = FALSE)

# Disconnect from the database
dbDisconnect(con)