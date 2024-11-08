# Load necessary packages
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(tibble)
library(xtable)

# Connect to MySQL database
con <- dbConnect(
  MySQL(),
  host = "bethel.utia.cas.cz",
  user = "jak",
  password = "jaknajaka",
  dbname = "telegram_czech"
)

print("Logged in")


# Retrieve data from the database
messages_with_lang <- dbGetQuery(con, "SELECT * FROM messages_with_lang")

# Replace NA values in src_channel_lang and channel_lang with "unknown"
messages_with_lang <- messages_with_lang %>%
  mutate(src_channel_lang = ifelse(is.na(src_channel_lang), "unknown", src_channel_lang),
         channel_lang = ifelse(is.na(channel_lang), "unknown", channel_lang))

# Create the contingency table
lang_matrix <- messages_with_lang %>%
  count(src_channel_lang, channel_lang) %>%
  complete(src_channel_lang = unique(c(messages_with_lang$src_channel_lang, messages_with_lang$channel_lang)),
           channel_lang = unique(c(messages_with_lang$src_channel_lang, messages_with_lang$channel_lang)),
           fill = list(n = 0)) %>%
  pivot_wider(names_from = channel_lang, values_from = n, values_fill = list(n = 0))

# Set row names and convert to matrix
row.names(lang_matrix) <- lang_matrix$src_channel_lang
lang_matrix <- as.matrix(lang_matrix[, -1])

# Create a LaTeX table from the matrix
latex_table <- xtable(lang_matrix)

# Specify the output file
output_file <- "lang_matrix_table.tex"

# Save the LaTeX table to the file
print(latex_table, file = output_file)

# Display the matrix
print(lang_matrix)

dbDisconnect(con)
