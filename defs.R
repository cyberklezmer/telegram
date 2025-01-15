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

islandid <- 'CZ'
islandcondition <- 'lang = "CZECH"'

# Define output folder for CSV and PDF
output_folder <- "out/"
graph_subfolder <- "graphs/" # Change this to your desired folder
graph_output_folder <- paste0(output_folder,graph_subfolder)


connect_db <- function() {
  dbConnect(
    MySQL(),
    host = "bethel.utia.cas.cz",
    user = "jak",
    password = "jaknajaka",
    dbname = "telegram_full"
  )
}

gini_coefficient <- function(counts) {
  # Values are fixed as -1, 0, 1
  values <- c(-1, 0, 1)
  
  # Calculate probabilities (relative frequencies)
  prob <- counts / sum(counts)
  
  # Cumulative sums
  cum_pop <- cumsum(prob)  # Cumulative population proportion
  cum_val <- cumsum(prob * values) / sum(prob * values)  # Cumulative proportion of values
  
  # Gini coefficient calculation
  gini <- 1 - sum((cum_pop[-1] + cum_pop[-length(cum_pop)]) * diff(c(0, cum_val)))
  
  return(gini)
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


init_forwards <- function(con, top_channels) {
  
  forwards_data <- dbGetQuery(con, "SELECT * from forwards")
  
  forwards_data <- forwards_data %>%
    inner_join(top_channels, by = c("channel_id" = "channel_id")) %>%
    rename(dst_username = username )  %>%
    inner_join(top_channels, by = c("src_channel_id" = "channel_id")) %>%
    rename(src_username = username )  
  
  return(forwards_data)
}


get_reactions <- function(con, this_channel_id, top_emoticons) {
  
  sqltext <- paste0("SELECT *, HEX(reaction_emo) as hex_r_emo FROM message_reactions WHERE channel_id=", this_channel_id)
  reactions_data <- dbGetQuery(con, sqltext)
  
  emoticons =  dbGetQuery(con, "SELECT *, HEX(emoticon) as hex_emo, HEX(unified_emo) as hex_u_emo FROM emoticons")
  
  
  # Joining reactions_data with emoticons
  merged_data <- reactions_data %>%
    left_join(emoticons, by = c("hex_r_emo" = "hex_emo"))
  
  
  # Group and summarize
  grouped_data <- merged_data %>%
    mutate(hex_u_emo = if_else(is.na(hex_u_emo), "other", hex_u_emo)) %>%
    group_by(channel_id,message_id,hex_u_emo) %>%
    summarise(u_count = sum(count, na.rm = TRUE), .groups = 'drop')%>%
    select(channel_id, message_id, hex_u_emo,u_count)
  
  grouped_data <- grouped_data %>%
    left_join(top_emoticons, by = "hex_u_emo")
  
  
}

group_reactions <- function(reactions_src)  {
  result <- reactions_src %>%
    group_by(channel_id,message_id)  %>%
    summarise( count = sum(u_count, na.rm = TRUE), 
               positivity = sum(Positivity, na.rm = TRUE),
               positivity_sq = sum(Positivity^2, na.rm = TRUE),
               .groups = 'drop') %>%
    select(channel_id,message_id,count,positivity,positivity_sq)
  return(result)
}

init_channels <- function(con, chnumber) {
  
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
  return(top_channels)
}

init_emos <- function(con) {
  emoticons =  dbGetQuery(con, "SELECT *, HEX(emoticon) as hex_emo, HEX(unified_emo) as hex_u_emo FROM emoticons")
  
  emo_sentiments <- read.csv("emo_sentiments_novak_et_al.csv")
  
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
  
  # Create a ranking based on descending "count" from top_emoticons
  top_emoticons <- top_emoticons %>%
    arrange(desc(count)) %>%  # Sort descending by count
    mutate(rank = row_number())  # Add rank for sorting
  
  return(top_emoticons)
}  



# Connect to MySQL database

