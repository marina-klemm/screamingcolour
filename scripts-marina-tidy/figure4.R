require(tidytext)
require(stringr)
require(plotly)
require(dplyr)
require(sentimentr)
require(tidyverse)
require(corrr)
require(viridis)
require(scales)

### Trying to use a more nuanced package to qualify the words' sentiments

# Datasets to be used -----------------------------------------------------


taylor_album_songs
surpriseSongsDressColours  #Edited, all the
# mashups are underneath the main songs, and the names of the songs are changed
allSongsMetadata 



# Add colour grouping to dresses ------------------------------------------

surpriseSongsDressColours <- surpriseSongsDressColours %>%
  mutate(DressColourGroup = case_when(
    DressName %in% c("Pink", "Flamingo pink") ~ "Reds",
    DressName %in% c("Blue", "Ocean blue") ~ "Blues",
    DressName %in% c("Yellow", "Sunset orange") ~ "Yellows",
    DressName %in% c("Cotton candy", "Grapefruit", "Popsicle") ~ "Colourful",
    DressName == "Blurple" ~ "Purples",
    DressName == "Green" ~ "Greens"
  ))
  



# Getting the sentiment from the lyrics ----------------------------------

head(taylor_album_songs$lyrics) # this is where the information will be extracted from

lyrics_df <- taylor_album_songs %>%
  unnest(lyrics) %>%
  select(album_name, track_name, track_number, line, lyric, element)

head(lyrics_df)


# Using sentimentr now:

#sentiment_scores <- sentiment(lyrics_df$lyric)

#lyrics_sentiment <- cbind(lyrics_df, sentiment_scores)
#dim(lyrics_df)
#dim(sentiment_scores)

# This didn't work because some sentences within lyrics might'be been split into two
# because of commas, full stops, question marks. So, I'll add a marker per sentence instead:

lyrics_df$row_id <- 1:nrow(lyrics_df)

sentences_with_id <- get_sentences(lyrics_df$lyric, lyrics_df$row_id) # this processes
# each line as its own sentence as it's using the row_id. 
sentiment_with_id <- sentiment(sentences_with_id)
dim(sentiment_with_id)
dim(lyrics_df)
head(lyrics_df)

# Summarize the sentiment scores using the original row_id

sentiment_summary <- sentiment_with_id %>%
  group_by(element_id) %>%
  summarise(
    avg_sentiment = mean(sentiment, na.rm = TRUE),
    word_count = sum(word_count)
  )
dim(sentiment_summary)

# Try to join back again now:

lyrics_sentiment <- lyrics_df %>%
  left_join(sentiment_summary, by = c("row_id" = "element_id"))

dim(lyrics_df)
dim(lyrics_sentiment)

head(lyrics_sentiment)



# Now, plot it like I did before for tidytext -----------------------------

# Now aggregate by song to get net sentiment score
song_sentiment_scores_sentimentr <- lyrics_sentiment %>%
  group_by(track_number, track_name, album_name) %>%
  summarize(
    sum_sentiment = sum(avg_sentiment),
    total_sentiment_words = n(),
    avg_sentiment = sum(avg_sentiment) / n(), 
    .groups = "drop"
  )

# Then, order the songs by album and by track number:
albumOrder <- c("Taylor Swift", "Fearless (Taylor's Version)", 
                "Speak Now (Taylor's Version)", "Red (Taylor's Version)",
                "1989 (Taylor's Version)", "Reputation", "Lover",
                "folklore", "evermore", "Midnights",
                "THE TORTURED POETS DEPARTMENT")

lyrics_sentiment <- lyrics_sentiment %>%
  mutate(album_name = factor(album_name, levels = albumOrder))
lyrics_sentiment <- lyrics_sentiment %>%
  filter(!is.na(album_name)) 

# Then plot the net sentiment scores
ggplot(lyrics_sentiment, aes(x = track_number, y = avg_sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = colourPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "The emotional spectrum across Taylor Swift's discography",
    x = "Track Number",
    y = "Net Sentiment (Positive - Negative Words)"
  ) +
  theme(plot.title = element_text(hjust=0.5))


# Making it interactive:

p <- ggplot(lyrics_sentiment, aes(x = track_number, y = avg_sentiment, 
                                  fill = album_name,
                                  text = paste("Song:", track_name,
                                               "<br>Net sentiment:", avg_sentiment))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = colourPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "The emotional spectrum across Taylor Swift's discography",
    x = "Track Number",
    y = "Net Sentiment (Positive - Negative Words)"
  ) +
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none")

# Convert to an interactive plotly object
interactive_plot_sentimentr <- ggplotly(p, tooltip = "text")

# Display the interactive plot
interactive_plot_sentimentr



# Lastly, how do the song sentiment correlate to the colour sentiment? --------

# Similar to the average I used for the plot, but weighting it by word count
# Calculate weighted average sentiment for each song
song_sentiments <- lyrics_sentiment %>%
  group_by(album_name, track_name) %>%
  summarise(
    weighted_sentiment_lyric = sum(avg_sentiment * word_count, na.rm = TRUE) / sum(word_count, na.rm = TRUE),
    total_words = sum(word_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(weighted_sentiment_lyric))

# Create contingency table using Charlotte's findings
colour_contigency_table <- tribble(
  ~colour_category, ~negative, ~neutral, ~positive,
  "Black And White", 2, 2, 0,
  "Blacks", 3, 7, 3,
  "Blues", 18, 7, 10,
  "Colourful", 1, 3, 11,
  "Greens", 0, 3, 8,
  "Purples", 1, 1, 4,
  "Reds", 15, 11, 13,
  "Whites", 13, 12, 5,
  "Yellows", 1, 9, 17
)

# Calculate weighted average sentiment for each colour category
colour_sentiments <- colour_contigency_table %>%
  mutate(
    total_mentions = negative + neutral + positive,
    colour_weighted_sentiment = (positive * 1 + neutral * 0 + negative * -1) / total_mentions
  ) %>%
  # Map to dress colour groups
  mutate(
    DressColourGroup = case_when(
      colour_category == "Blues" ~ "Blues",
      colour_category == "Colourful" ~ "Colourful", 
      colour_category == "Greens" ~ "Greens",
      colour_category == "Purples" ~ "Purples",
      colour_category == "Reds" ~ "Reds",
      colour_category == "Yellows" ~ "Yellows",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(DressColourGroup))

summary(song_sentiments$weighted_sentiment_lyric)
dim(song_sentiments)

summary(colour_sentiments$colour_weighted_sentiment)
dim(colour_sentiments)

# Display top/bottom songs and the colours by sentiment
print(song_sentiments %>% 
        arrange(desc(weighted_sentiment_lyric)) %>% 
        head(5))

print(song_sentiments %>% 
        arrange(weighted_sentiment_lyric) %>% 
        head(5))


print(colour_sentiments %>% 
        arrange(desc(colour_weighted_sentiment)))



## If the weighted sentiment between dress colours and lyrics followed the same pattern,
### we would see:
  # Songs in green, colourful, yellow and purple dresses: more positive
  # Songs in red and blue dresses: more negative]

## Join the sentiment scores with the surprise songs dataset
dress_song_sentiments <- surpriseSongsDressColours %>%
  left_join(song_sentiments, by = c("Song title" = "track_name")) %>%
  filter(!is.na(weighted_sentiment_lyric))  # Remove songs without sentiment scores


## Calculate the song sentiment for each dress colour group:
dress_group_sentiments <- dress_song_sentiments %>%
  group_by(DressColourGroup) %>%
  summarise(
    performance_weighted_sentiment = mean(weighted_sentiment_lyric, na.rm = TRUE),
    n_performances = n(),
    .groups = 'drop'
  )

## At last, combine dress colour group sentiment with colour sentiment from lyrics:
combined_analysis <- dress_group_sentiments %>%
  left_join(colour_sentiments, by = "DressColourGroup") %>%
  select(DressColourGroup, performance_weighted_sentiment, colour_weighted_sentiment, n_performances, total_mentions)

print(combined_analysis)

## Correlation?

# Calculate correlation
correlation <- cor(combined_analysis$performance_weighted_sentiment, 
                   combined_analysis$colour_weighted_sentiment, 
                   use = "complete.obs")

correlation

cor.test(combined_analysis$performance_weighted_sentiment, combined_analysis$colour_weighted_sentiment)




# Plot :-) ----------------------------------------------------------------


ggplot(combined_analysis, aes(x = colour_weighted_sentiment, y = performance_weighted_sentiment)) +
  theme_minimal(base_size = 16, base_family = "Calibri") +
  
 
  
  geom_smooth(method = "lm", 
              se = TRUE, 
              #fill = "#BDC3C7",
              alpha = 0.3,
              size = 1.2) +
  
  geom_point(aes(size = n_performances, 
                 color = DressColourGroup), 
             alpha = 0.8,
             stroke = 0.5) +
  
  geom_text(aes(label = DressColourGroup), 
            vjust = -2, 
            hjust = 0.5,
            size = 5,
            fontface = "bold",
            show.legend = FALSE) +
  
  # Custom color scale
  scale_color_manual(values = colourGroupPalette,
                     name = "Dress Color\nGroup") +
  # Hide color guide specifically
  guides(color = "none") +

  scale_size_continuous(name = "Number of\nPerformances",
                        range = c(3, 12),
                        breaks = c(10, 20, 30, 40, 50),
                        guide = guide_legend(override.aes = list(alpha = 0.8))) +

  scale_x_continuous(labels = number_format(accuracy = 0.01),
                     breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01),
                     breaks = pretty_breaks(n = 6)) +

  labs(
    x = "Colour Sentiment in Lyrics",
    y = "Performance Sentiment by Dress Colour Group",
    ) +
  

  theme(
    plot.title = element_blank(),
    axis.title.x = element_text(size = 16, 
                                margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 14),

    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    legend.position = "right",
    legend.box = "vertical",
    legend.margin = margin(l = 20),
   
  

    plot.margin = margin(20, 20, 20, 20)
  )


