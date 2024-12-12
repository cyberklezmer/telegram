fasdfasd

# Fetch the data (assuming you've already fetched it as `reactions_data`)
reactions_data <- dbGetQuery(con, "SELECT R.channel_id, message_id, (select name from emoticons E where E.emoticon=reaction_emo) as emo, count FROM message_reactions R JOIN channels_info I ON(R.channel_id=I.channel_id) WHERE lang='CZECH'")

reactions_data <- reactions_data %>%
  mutate( emo =  if_else(
    is.na(emo),  # Check if the string starts with E or F
    "other",
    emo)
  )

# print(reactions_data)


# reactions_data <- reactions_data %>%
#  mutate(reaction_emo = stringi::stri_trans_general(reaction_emo, "Any-Latin; Latin-ASCII"))


# grouped_data <- reactions_data %>%
# group_by(message_id, emo) %>%
#  summarize(count = sum(count, na.rm = TRUE), .groups = "drop")




# Transform the data


# Summarize the total counts for each reaction type
reaction_totals <- reaction_summary %>%
  select(-message_id) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "reaction_type", values_to = "total_count") %>%
  arrange(desc(total_count))

# Print reaction_totals for reference
print(reaction_totals)

# Generate histograms in descending order of total counts
pdf(file = paste0(output_folder, "histograms_sorted.pdf"), width = 8, height = 6)

reaction_summary %>%
  select(-message_id) %>%
  gather(key = "reaction_type", value = "count") %>%
  mutate(reaction_type = factor(reaction_type, levels = reaction_totals$reaction_type)) %>%  # Order by total_count
  group_split(reaction_type) %>%
  purrr::walk(~ {
    reaction_type <- unique(.x$reaction_type)
    p <- ggplot(.x, aes(x = count)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      labs(
        title = paste("Histogram for", reaction_type),
        x = "Count",
        y = "Frequency"
      ) +
      theme_minimal()
    
    # Save the plot
    print(p)
  })

dev.off()


# Compute mean and variance for each reaction_type
stats_summary <- reaction_summary %>%
  select(-message_id) %>%  # Exclude non-numeric column
  summarise(across(
    everything(),
    list(mean = ~ mean(.x, na.rm = TRUE), variance = ~ var(.x, na.rm = TRUE))
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("reaction_type", ".value"),
    names_sep = "_"
  )

# View the summary statistics
print(stats_summary)

# Optionally save the summary to a CSV file
write.csv(stats_summary, paste0(output_folder, "reaction_stats_summary.csv"), row.names = FALSE)


# Prepare data: exclude non-numeric columns like message_id
data_numeric <- reaction_summary %>%
  select(-message_id)

# PCA
pca_result <- prcomp(data_numeric, scale. = TRUE)


# Create a PDF for PCA visualizations
pdf(file = paste0(output_folder, "pca_visualizations.pdf"), width = 8, height = 6)

# Scree Plot
fviz_eig(pca_result) +
  labs(title = "Scree Plot of PCA") +
  theme_minimal()

# PCA Biplot (First Two Components)
fviz_pca_biplot(pca_result, axes = c(1, 2), repel = TRUE) +
  labs(title = "PCA Biplot (First Two Components)") +
  theme_minimal()

# Close the PDF device
dev.off()



# Save the two components to a file






dbDisconnect(con)




fasd

# Determine number of factors to extract
fa_parallel <- fa.parallel(data_numeric, fa = "fa", n.iter = 100)
num_factors <- fa_parallel$n_factors

# Perform Factor Analysis
factor_result <- fa(data_numeric, nfactors = num_factors, rotate = "varimax")

# Print loadings
print(factor_result$loadings)

# Visualize loadings
fviz_famd_var(factor_result, repel = TRUE) # Loadings plot

write.csv(factor_result$loadings, "factor_loadings.csv")
write.csv(pca_result$rotation, "pca_loadings.csv")


# Perform PCA and limit to two components
pca_result <- prcomp(data_numeric, scale. = TRUE)



library(psych)

# Perform Factor Analysis with two factors
factor_result <- fa(data_numeric, nfactors = 2, rotate = "varimax")

# View factor loadings
print(factor_result$loadings)

# Save the factor loadings to a file
write.csv(factor_result$loadings, "factor_loadings_two_factors.csv", row.names = TRUE)

# Visualize factor loadings
library(factoextra)
fviz_famd_var(factor_result, repel = TRUE)  # For loading plots

