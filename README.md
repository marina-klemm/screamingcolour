## Overview

*Could a Swiftie in the crowd reliably predict the mood of the surprise
songs based on the color of Swift’s dress?* Looking the relationship
between Taylor Swift’s wardrobe choices and her surprise song selection
during the Eras Tour.

## Surprise Song Outfits

<table border="1">
<tr>
<th>
Blue
</th>
<th>
Blurple
</th>
<th>
Cotton Candy
</th>
<th>
Flamingo Pink
</th>
<th>
Grapefruit
</th>
<th>
Green
</th>
<th>
Ocean Blue
</th>
<th>
Pink
</th>
<th>
Popsicle
</th>
<th>
Sunset Orange
</th>
<th>
Yellow
</th>
</tr>
<tr>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/blue.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/blurple.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/cotton_candy.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/flamingo_pink.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/grapefruit.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/green.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/ocean_blue.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/pink.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/popsicle.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/sunset_orange.jpg?raw=true" width="100">
</td>
<td>
<img src="https://github.com/cmjt/studyinswift/blob/main/dress_images/images_high_res/cropped/yellow.jpg?raw=true" width="100">
</td>
</tr>
</table>

### Distribution of Dresses

![](https://github.com/cmjt/studyinswift/blob/main/README_files/figure-markdown_strict/unnamed-chunk-1-1.png?raw=true)

<details>
<summary>
Plot code
</summary>

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

ggplot(pianoSongsDataWithImages, aes(x = reorder(DressName, -n), y = n, fill = DressName)) +
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


   

</details>
