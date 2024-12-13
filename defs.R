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




init_emos <- function(con) {
  emoticons =  dbGetQuery(con, "SELECT *, HEX(emoticon) as hex_emo, HEX(unified_emo) as hex_u_emo FROM emoticons")
  
  emo_sentiments <- read.csv("emo_sentiments_by_hand.csv")
  
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
  
  return(top_emoticons)
}  


# Connect to MySQL database

