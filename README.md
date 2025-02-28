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

### Distribution of Dresses
![](https://github.com/cmjt/studyinswift/blob/main/README_files/figure-markdown_strict/unnamed-chunk-1-2.png?raw=true)

<details>
<summary>
Plot code
</summary>

```r
    require(tidyverse)
    require(ggimage)
    surpriseSongsDressColours <- "raw_data/surprise_songs.xlsx"
    surpriseSongsDressColours <- readxl::read_excel(surpriseSongsDressColours, sheet = "List")

    pianoSongsData <- surpriseSongsDressColours[surpriseSongsDressColours$Instrument == "Piano",]
    dressColorMapping <- unique(pianoSongsData %>% select(DressName, ColourHex1))
    colorPaletteDresses <- setNames(dressColorMapping$ColourHex1, dressColorMapping$DressName)
    pianoSongsData$Date <- as.Date(pianoSongsData$Date)
    pathToDressColours <- "dress_images/images_high_res/cropped/"
    pianoSongsData %>%
      count(DressName) %>%
      mutate(
        percentage = n / sum(n) * 100,
        imagePath = case_when(
          DressName == "Pink" ~paste0(pathToDressColours, "pink.jpg"),
          DressName == "Green" ~paste0(pathToDressColours, "green.jpg"),
          DressName == "Yellow" ~paste0(pathToDressColours, "yellow.jpg"),
          DressName == "Blue" ~paste0(pathToDressColours, "blue.jpg"),
          DressName == "Flamingo pink" ~ paste0(pathToDressColours,"flamingo_pink.jpg"),
          DressName == "Ocean blue" ~ paste0(pathToDressColours,"ocean_blue.jpg"),
          DressName == "Sunset orange" ~ paste0(pathToDressColours,"sunset_orange.jpg"),
          DressName == "Cotton candy" ~paste0(pathToDressColours, "cotton_candy.jpg"),
          DressName == "Blurple" ~paste0(pathToDressColours, "blurple.jpg"),
          DressName == "Grapefruit" ~ paste0(pathToDressColours,"grapefruit.jpg"),
          DressName == "Popsicle" ~ paste0(pathToDressColours,"popsicle.jpg"),
          # Add more conditions for each DressName
          TRUE ~ NA_character_
        )
      ) -> pianoSongsDataWithImages

pianoSongsData %>%
  count(DressName) %>%
  mutate(
    percentage = n / sum(n) * 100,
    imagePath = case_when(
      DressName == "Pink" ~paste0(pathToDressColours, "Pink.jpg"),
      DressName == "Green" ~paste0(pathToDressColours, "Green.jpg"),
      DressName == "Yellow" ~paste0(pathToDressColours, "Yellow.jpg"),
      DressName == "Blue" ~paste0(pathToDressColours, "Blue.jpg"),
      DressName == "Flamingo pink" ~ paste0(pathToDressColours,"Flamingo pink.jpg"),
      DressName == "Ocean blue" ~ paste0(pathToDressColours,"Ocean blue.jpg"),
      DressName == "Sunset orange" ~ paste0(pathToDressColours,"Sunset orange.jpg"),
      DressName == "Cotton candy" ~paste0(pathToDressColours, "Cotton candy.jpg"),
      DressName == "Blurple" ~paste0(pathToDressColours, "Blurple.jpg"),
      DressName == "Grapefruit" ~ paste0(pathToDressColours,"Grapefruit.jpg"),
      DressName == "Popsicle" ~ paste0(pathToDressColours,"Popsicle.jpg"),
      TRUE ~ NA_character_
    )
  ) -> pianoSongsDataWithImages

dressVis <- ggplot(pianoSongsDataWithImages, aes(x = reorder(DressName, -n), y = n, fill = DressName)) +
  geom_bar(stat = "identity", width = 0.8) +  
  geom_image(
    aes(image = imagePath, y = n),  
    size = 0.15,                    
    by = "height"                    
  ) +
  geom_text(
    aes(y = n + 3.8, label = paste0(n, "\n(", round(percentage, 1), "%)")),  
    vjust = 0,  
    color = "black",
    size = 4
  ) +
  scale_fill_manual(values = colorPaletteDresses) +
  theme_minimal() +
  labs(
    title = "Surprise Song Dresses: The Most Worn Looks",
    x = "Dress Color",
    y = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  ylim(0, 35)  # Increase limit to prevent text from the pink dress from being cropped


ggsave("README_files/figure-markdown_strict/unnamed-chunk-1-2.png", width=10, height=8)

dressVis
```
</details>

### Dress transitions and special events across the Eras Tour
![](https://github.com/cmjt/studyinswift/blob/main/README_files/figure-markdown_strict/unnamed-chunk-1-3.png?raw=true)

<details>
<summary>
Plot code
</summary>

```r
# First, find the first date for each dress
dress_first_appearance <- pianoSongsData %>%
  group_by(DressName) %>%
  summarize(FirstAppearance = min(Date)) %>%
  arrange((FirstAppearance))

# Convert DressName to a factor with ordered levels
pianoSongsData$DressName <- factor(pianoSongsData$DressName, 
                                   levels = dress_first_appearance$DressName)

max_dress_level <- length(unique(pianoSongsData$DressName))

ggplot(pianoSongsData, aes(x = Date, y = DressName, color = ColourHex1)) +
  geom_point(size = 4, alpha = 0.5) +
  scale_color_identity() +
  theme_minimal() +
  labs(
    title = "Look What You Made Her Wear: Taylor's Eras Tour Color Story",
    x = "",
    y = ""
  ) +
  geom_rect(aes(xmin = as.Date("2023-08-28"), xmax = as.Date("2023-11-08"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +
  geom_rect(aes(xmin = as.Date("2023-11-27"), xmax = as.Date("2024-02-06"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +  
  geom_rect(aes(xmin = as.Date("2024-03-10"), xmax = as.Date("2024-05-08"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +
  geom_rect(aes(xmin = as.Date("2024-08-21"), xmax = as.Date("2024-10-17"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +
  # Vertical lines for the key events
  geom_vline(xintercept = as.Date("2024-05-09"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2024-10-18"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2023-08-24"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2024-02-07"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2024-04-19"), linetype = "solid", color = "white", size=2) +
  geom_vline(xintercept = as.Date("2023-07-07"), linetype = "solid", color = "purple", size =2) +
  geom_vline(xintercept = as.Date("2023-10-27"), linetype = "solid", color = "blue", size =2) +
  
  # Text annotations for the events above
  annotate("text", x = as.Date("2024-05-09"), y = max_dress_level, 
           label = "Europe", color = "black", angle = -90, vjust = -1) +
  annotate("text", x = as.Date("2024-10-18"), y = max_dress_level, 
           label = "North \nAmerica", color = "black", angle = -90, vjust = -0.2) +
  annotate("text", x = as.Date("2023-08-24"), y = max_dress_level, 
           label = "Latin \nAmerica", color = "black", angle = -90, vjust = -0.2) +
  annotate("text", x = as.Date("2024-02-07"), y = max_dress_level, 
           label = "Asia/\nOceania", color = "black", angle = -90, vjust = -0.2) +
  annotate("text", x = as.Date("2024-04-19"), y = max_dress_level, 
           label = "TTPD", color = "white", angle = -90, vjust = -1, 
           fontface = "bold") + ##this one is bold because white is hard to see, but TTPD IS white
   annotate("text", x = as.Date("2023-07-07"), y = max_dress_level, 
           label = "Speak\nNow TV", color = "purple", angle = -90, vjust = -0.2) +
  annotate("text", x = as.Date("2023-10-27"), y = max_dress_level, 
           label = "1989\nTV", color = "blue", angle = -90, vjust = -0.2) +
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust=0.5, size = 14, margin = margin(b = 20), face = "bold"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
        )
```
</details>

## A Taylor Swift Dictionary of Colour
![](https://github.com/cmjt/studyinswift/blob/main/README_files/figure-markdown_strict/unnamed-chunk-1-4.png?raw=true)

<details>
<summary>
Plot code
</summary>

```r
# First, create expanded dataset with separated colors and meanings
rawColorData <- data.frame(
  colour = trimws(unlist(strsplit(allSongsMetadata$colour_MK, ";"))),
  meaning = trimws(unlist(strsplit(allSongsMetadata$colour_meaningMK, ";")))
) %>%
  filter(!is.na(colour) & !is.na(meaning))

colorSentimentScores <- rawColorData %>%
  mutate(
    meaning = trimws(meaning),  
    score = case_when(
      tolower(meaning) == "positive" ~ 1,
      tolower(meaning) == "neutral" ~ 0,
      tolower(meaning) == "negative" ~ -1,
      TRUE ~ NA_real_
    )
  )

# Calculate average sentiment for each individual color
individualColorSentiments <- colorSentimentScores %>%
  group_by(colour) %>%
  summarise(
    avgSentiment = mean(score, na.rm = TRUE),
    mentionCount = n()
  ) %>%
  ungroup()

# Create color groups mapping dataset
colorGroupMappings <- data.frame(
  colour = names(colorGroups),
  groupName = unlist(colorGroups)
)

# Calculate group-level sentiments
colorGroupSentiments <- individualColorSentiments %>%
  left_join(colorGroupMappings, by = "colour") %>%
  group_by(groupName) %>%
  summarise(
    groupSentiment = mean(avgSentiment, na.rm = TRUE),
    totalMentions = sum(mentionCount),
    colorsInGroup = n()
  ) %>%
  arrange(desc(groupSentiment))

# Print results
print("Individual Color Sentiments:")
print(individualColorSentiments %>% arrange(desc(avgSentiment)))

print("\nColor Group Sentiments:")
print(colorGroupSentiments)


## Finally, look at it:
# Create representative colors for each group
groupColors <- c(
  "purples" = "#8A2BE2",      # Using ultraviolet
  "yellows" = "#FFD700",      # Using gold
  "reds" = "#FF0000",         # Using red
  "blues" = "#0000FF",        # Using blue
  "greens" = "#008000",       # Using green
  "colorful" = "#FF4500",     # Using rainbow
  "black and white" = "#C0C0C0", # Using silver
  "whites" = "#FFFFFF",       # Using white
  "blacks" = "#000000"        # Using black
)

# Create the enhanced plot
ggplot(colorGroupSentiments, 
       aes(x = reorder(groupName, groupSentiment), 
           y = groupSentiment,
           fill = groupName)) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "gray50",
             size = 0.3) +
  geom_bar(stat = "identity", 
           alpha = 0.8, 
           width = 0.7,
           color = "gray20",
           size = 0.5) +
  geom_text(aes(label = sprintf("%.2f", groupSentiment),
                vjust = ifelse(groupSentiment >= 0, -0.5, 1.5)),
            position = position_dodge(width = 0.7),
            size = 4) +
  scale_fill_manual(values = groupColors) +
  labs(
    title = "We Were Screaming Color",
    subtitle = "Average Emotional Ratings of Color Groups in Swift's Lyrics",
    x = NULL,
    y = "Sentiment Score (-1 = Negative, 0 = Neutral, 1 = Positive)",
    caption = paste("Analysis based on", nrow(rawColorData), "color mentions across 242 songs release to date")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, vjust =1),
    axis.text.y = element_text(size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  scale_y_continuous(
    limits = c(-1.1, 1.1),
    breaks = seq(-1, 1, 0.5)
  )

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
