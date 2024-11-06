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
  dbname = "telegram"
)

catalogue_data <- dbGetQuery(con, "SELECT * FROM catalogue")

# Shrnutí dat podle 'lang_messeges' pro 'sum_members'
catalogue_summary_members <- catalogue_data %>%
  group_by(lang_messeges) %>%
  summarise(
    num_rows = n(),
    sum_members = sum(members, na.rm = TRUE)
  ) %>%
  arrange(desc(sum_members))

# Uspořádání na top 10 hodnot a zbytek jako "Other" pro 'sum_members'
top_10_members <- catalogue_summary_members %>% top_n(10, sum_members)
others_members <- catalogue_summary_members %>%
  filter(!lang_messeges %in% top_10_members$lang_messeges) %>%
  summarise(lang_messeges = "Other", num_rows = sum(num_rows), sum_members = sum(sum_members))

# Kombinace top 10 a "Other" pro 'sum_members'
catalogue_summary_members <- bind_rows(top_10_members, others_members)

# Reorder lang_messeges factor based on sum_members
catalogue_summary_members$lang_messeges <- factor(catalogue_summary_members$lang_messeges, levels = catalogue_summary_members$lang_messeges[order(-catalogue_summary_members$sum_members)])

# Abbreviate lang_messeges to three letters
catalogue_summary_members$lang_messeges_abbr <- substr(as.character(catalogue_summary_members$lang_messeges), 1, 3)

# Uložení summary do CSV pro 'sum_members'
write.csv(catalogue_summary_members, "catalogue_summary_members.csv", row.names = FALSE)

# Vykreslení koláčového grafu pro 'sum_members'
plot_sum_members <- ggplot(catalogue_summary_members, aes(x = "", y = sum_members, fill = lang_messeges_abbr)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Sum of Members by Lang_messeges (Catalogue Data)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  geom_text(aes(label = paste0(lang_messeges_abbr, ": ", scales::number(sum_members, accuracy = 1))),
            position = position_stack(vjust = 0.5), check_overlap = TRUE)

# Uložení grafu 'sum_members'
ggsave("catalogue_summary_members_pie_chart.png", plot = plot_sum_members, width = 10, height = 6)

# Shrnutí dat podle 'lang_messeges' pro 'num_rows'
catalogue_summary_rows <- catalogue_data %>%
  group_by(lang_messeges) %>%
  summarise(
    num_rows = n()
  ) %>%
  arrange(desc(num_rows))

# Uspořádání na top 10 hodnot a zbytek jako "Other" pro 'num_rows'
top_10_rows <- catalogue_summary_rows %>% top_n(10, num_rows)
others_rows <- catalogue_summary_rows %>%
  filter(!lang_messeges %in% top_10_rows$lang_messeges) %>%
  summarise(lang_messeges = "Other", num_rows = sum(num_rows))

# Kombinace top 10 a "Other" pro 'num_rows'
catalogue_summary_rows <- bind_rows(top_10_rows, others_rows)

# Reorder lang_messeges factor based on num_rows
catalogue_summary_rows$lang_messeges <- factor(catalogue_summary_rows$lang_messeges, levels = catalogue_summary_rows$lang_messeges[order(-catalogue_summary_rows$num_rows)])

# Abbreviate lang_messeges to three letters for num_rows
catalogue_summary_rows$lang_messeges_abbr <- substr(as.character(catalogue_summary_rows$lang_messeges), 1, 3)

# Uložení summary do CSV pro 'num_rows'
write.csv(catalogue_summary_rows, "catalogue_summary_rows.csv", row.names = FALSE)

# Vykreslení koláčového grafu pro 'num_rows'
plot_num_rows <- ggplot(catalogue_summary_rows, aes(x = "", y = num_rows, fill = lang_messeges_abbr)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Num Rows by Lang_messeges (Catalogue Data)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  geom_text(aes(label = paste0(lang_messeges_abbr, ": ", scales::number(num_rows, accuracy = 1))),
            position = position_stack(vjust = 0.5), check_overlap = TRUE)

# Uložení grafu 'num_rows'
ggsave("catalogue_summary_rows_pie_chart.png", plot = plot_num_rows, width = 10, height = 6)

# Analýza tabulky 'catalogue' s INNER JOIN
catalogue_joined_data <- dbGetQuery(con, "
  SELECT c.*, IFNULL(m.message_count, 0) AS message_count
FROM catalogue c
INNER JOIN channels ch ON c.account = ch.username
LEFT JOIN ( 
    SELECT channel_id, COUNT(*) AS message_count
    FROM messages
    GROUP BY channel_id
) m ON ch.id = m.channel_id;
")

# Shrnutí dat podle 'lang_messeges'
catalogue_joined_summary <- catalogue_joined_data %>%
  group_by(lang_messeges) %>%
  summarise(
    num_rows = n(),
    sum_members = sum(members, na.rm = TRUE)
  ) %>%
  arrange(desc(sum_members))

# Uspořádání na top 10 hodnot a zbytek jako "Other"
top_10_joined <- catalogue_joined_summary %>% top_n(10, sum_members)
others_joined <- catalogue_joined_summary %>%
  filter(!lang_messeges %in% top_10_joined$lang_messeges) %>%
  summarise(lang_messeges = "Other", num_rows = sum(num_rows), sum_members = sum(sum_members))

# Kombinace top 10 a "Other"
catalogue_joined_summary <- bind_rows(top_10_joined, others_joined)

# Reorder lang_messeges factor based on sum_members
catalogue_joined_summary$lang_messeges <- factor(catalogue_joined_summary$lang_messeges, levels = catalogue_joined_summary$lang_messeges[order(-catalogue_joined_summary$sum_members)])

# Abbreviate lang_messeges to three letters
catalogue_joined_summary$lang_messeges_abbr <- substr(as.character(catalogue_joined_summary$lang_messeges), 1, 3)

# Uložení summary do CSV
write.csv(catalogue_joined_summary, "catalogue_joined_summary.csv", row.names = FALSE)

# Vykreslení koláčového grafu pro sum_members
plot_sum_members_joined <- ggplot(catalogue_joined_summary, aes(x = "", y = sum_members, fill = lang_messeges_abbr)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Sum of Members by Lang_messeges (Joined Data)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  geom_text(aes(label = paste0(lang_messeges_abbr, ": ", scales::number(sum_members, accuracy = 1))),
            position = position_stack(vjust = 0.5), check_overlap = TRUE)

# Uložení grafu sum_members
ggsave("catalogue_joined_summary_sum_members_pie_chart.png", plot = plot_sum_members_joined, width = 10, height = 6)

# Vykreslení koláčového grafu pro num_rows
plot_num_rows_joined <- ggplot(catalogue_joined_summary, aes(x = "", y = num_rows, fill = lang_messeges_abbr)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Num Channels by Lang_messeges (Joined Data)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  geom_text(aes(label = paste0(lang_messeges_abbr, ": ", scales::number(num_rows, accuracy = 1))),
            position = position_stack(vjust = 0.5), check_overlap = TRUE)

# Uložení grafu num_rows
ggsave("catalogue_joined_summary_num_rows_pie_chart.png", plot = plot_num_rows_joined, width = 10, height = 6)

# Combine the summary tables
combined_summary <- catalogue_summary_rows %>%
  select(lang_messeges, num_rows) %>%
  left_join(catalogue_summary_members %>% 
              select(lang_messeges, sum_members),
            by = "lang_messeges") %>%
  left_join(catalogue_joined_summary %>% 
              select(lang_messeges, num_rows_joined = num_rows, sum_members_joined = sum_members),
            by = "lang_messeges")

# Replace NA values with 0
combined_summary[is.na(combined_summary)] <- 0

# Calculate percentages and format as strings
combined_summary <- combined_summary %>%
  mutate(
    perc_num_rows_joined = ifelse(num_rows == 0, "0%", paste0(round((num_rows_joined / num_rows) * 100, 2), "%")),
    perc_sum_members_joined = ifelse(sum_members == 0, "0%", paste0(round((sum_members_joined / sum_members) * 100, 2), "%"))
  )

# Abbreviate lang_messeges to three letters
combined_summary$lang_messeges_abbr <- substr(as.character(combined_summary$lang_messeges), 1, 3)

# Set row names
row.names(combined_summary) <- combined_summary$lang_messeges_abbr

# Select and arrange columns for the LaTeX table in the desired order
latex_table_data <- combined_summary %>%
  select(lang_messeges, num_rows, num_rows_joined, perc_num_rows_joined, sum_members, sum_members_joined, perc_sum_members_joined) %>%
  arrange(desc(num_rows))

# Create a LaTeX table from the combined summary table
latex_table <- xtable(latex_table_data, 
                      caption = "Comparison of Catalogue and Joined Catalogue Data by Language",
                      label = "tab:comparison")

# Specify the output file
output_file <- "combined_summary_table.tex"

# Save the LaTeX table to the file
print(latex_table, file = output_file, include.rownames = FALSE)

# Display the combined summary table
print(combined_summary)

# Disconnect from the database
dbDisconnect(con)
