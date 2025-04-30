## Overview

*Could a Swiftie in the crowd reliably predict the mood of the surprise
songs based on the color of Swift’s dress?* Looking at the relationship
between Taylor Swift’s wardrobe choices and her surprise song selection
during the Eras Tour.

## Surprise Song Outfits

<table border="1">
<tr>
<th>Blue</th>
<th>Blurple</th>
<th>Cotton Candy</th>
<th>Flamingo Pink</th>
<th>Grapefruit</th>
<th>Green</th>
<th>Ocean Blue</th>
<th>Pink</th>
<th>Popsicle</th>
<th>Sunset Orange</th>
<th>Yellow</th>
</tr>
<tr>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/blue.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/blurple.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/cotton_candy.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/flamingo_pink.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/grapefruit.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/green.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/ocean_blue.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/pink.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/popsicle.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/sunset_orange.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
<td style="width:100px; height:150px; text-align:center; vertical-align:middle">
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/yellow.jpg?raw=true" width="100" height="100" style="object-fit:cover">
</td>
</tr>
</table>



## A Taylor Swift Dictionary of Colour
![](https://github.com/cmjt/studyinswift/blob/main/README_files/figure-markdown_strict/unnamed-chunk-1-4.png?raw=true)

<details>
<summary>
Plot code
</summary>

```r

```
</details>


# Some surprise songs were always performed within a dress color group

![](https://github.com/cmjt/studyinswift/blob/main/README_files/figure-markdown_strict/unnamed-chunk-1-5.png?raw=true)

<details>
<summary>
Plot code
</summary>

```r
# Step 1: Calculate which songs were performed with only one color group
songs_with_single_color_group <- multiple_performances %>%
  group_by(`Song title`) %>%
  summarize(
    total_performances = n(),
    unique_color_groups = n_distinct(groupName),
    color_group = first(groupName)  # The color group used (since there's only one)
  ) %>%
  filter(unique_color_groups == 1, total_performances > 1) %>%
  arrange(desc(total_performances))

# Step 2: Filter the dataset to include only these songs
single_color_performances <- multiple_performances %>%
  filter(`Song title` %in% songs_with_single_color_group$`Song title`)

# Step 3: Cool! Now, visualize them:

ggplot(single_color_performances, aes(x = reorder(`Song title`, table(single_color_performances$`Song title`)[`Song title`]), 
                                      fill = DressName)) +
  geom_bar() +
  coord_flip() +
  scale_fill_manual(values = colorPaletteDresses) +
  labs(
    title = "You Belong With Me: Songs That Found Their Perfect Color Match",
    subtitle = paste0(nrow(songs_with_single_color_group), " surprise songs that were always performed within the dress' color group"),
    x = "Song",
    y = "Number of Performances",
    fill = "Dress Color"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 13),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.position = "bottom"
  )
```
</details>


# Was there a difference in the sentiment of songs played on each instrument?
![](https://github.com/cmjt/studyinswift/blob/main/README_files/figure-markdown_strict/unnamed-chunk-1-6.png?raw=true)

<details>
<summary>
Plot code
</summary>

```r
# Let's examine both dataframes
str(surpriseSongsDressColours)
str(song_sentiment_scores)

# First, ensure both datasets have a normalized song name column
# For surpriseSongsDressColours
if(!"normalizedSongName" %in% colnames(surpriseSongsDressColours)) {
  surpriseSongsDressColours <- surpriseSongsDressColours %>%
    mutate(normalizedSongName = tolower(gsub("[^[:alnum:]]", "", `Song title`)))
}

# For song_sentiment_scores
song_sentiment_scores <- song_sentiment_scores %>%
  mutate(normalizedSongName = tolower(gsub("[^[:alnum:]]", "", track_name)))

# Now join using the normalized columns
instrument_sentiment_comparison <- surpriseSongsDressColours %>%
  left_join(song_sentiment_scores, by = "normalizedSongName") %>%
  filter(!is.na(Instrument), !is.na(net_sentiment))

# Create summary statistics by instrument
instrument_sentiment_summary <- instrument_sentiment_comparison %>%
  group_by(Instrument) %>%
  summarize(
    mean_sentiment = mean(net_sentiment, na.rm = TRUE),
    median_sentiment = median(net_sentiment, na.rm = TRUE),
    sd_sentiment = sd(net_sentiment, na.rm = TRUE),
    n_songs = n(),
    .groups = "drop"
  )

# Visualize the comparison
ggplot(instrument_sentiment_comparison, aes(x = net_sentiment, fill = Instrument)) +
  geom_density(alpha = 0.7) +
  geom_vline(data = instrument_sentiment_summary,
             aes(xintercept = mean_sentiment, color = Instrument),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Piano" = "#89CFF0", "Guitar" = "#F7CAC9")) +
  scale_color_manual(values = c("Piano" = "#0077BE", "Guitar" = "#E75480")) +
  labs(
    title = "Teardrops On My... Piano",
    subtitle = "Comparing the emotional tone of Taylor's surprise songs by instrument",
    x = "Net Sentiment Score",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Step 5: Perform a statistical test to see if the difference is significant
t_test_result <- t.test(net_sentiment ~ Instrument, data = instrument_sentiment_comparison)
print(t_test_result)
```

</details>
