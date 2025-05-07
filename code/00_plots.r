require(tidyverse)
require(ggimage)
require(cowplot)
require(extrafont) ## optional for GILL Sans MT font

## Data and wrangling etc
surpriseSongsDressColours <- "../raw_data/surprise_songs.xlsx"
surpriseSongsDressColours <- readxl::read_excel(surpriseSongsDressColours, sheet = "List")
surpriseSongsDressColours$Date <- as.Date(surpriseSongsDressColours$Date)
dressColorMapping <- unique(surpriseSongsDressColours %>% select(DressName, ColourHex1))
colorPaletteDresses <- setNames(dressColorMapping$ColourHex1, dressColorMapping$DressName)
pathToDressColours <- "../dress_images/images_high_res/cropped/"


oneRowPerConcert <- surpriseSongsDressColours %>%
    group_by(Date) %>%
    arrange(Date, Order) %>% 
    slice(1) %>%
    ungroup()

oneRowPerConcert %>%
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
            TRUE ~ NA_character_
        )) -> oneRowPerConcertWithImages

##############################
### The Most Worn Looks ######
##############################
ggplot(oneRowPerConcertWithImages, aes(x = reorder(DressName, -n), y = n, fill = DressName)) +
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
    ) + ylim(0, 35)
ggsave("../plots/the-most-worn-looks.png", width = 10, height = 8, bg = "white")

##########################################
### Eras' Outfits and Special Events #####
##########################################
dress_first_appearance <- surpriseSongsDressColours %>%
  group_by(DressName) %>%
  summarize(FirstAppearance = min(Date)) %>%
  arrange((FirstAppearance))
surpriseSongsDressColours$DressName <- factor(surpriseSongsDressColours$DressName,
                                              levels = dress_first_appearance$DressName)
max_dress_level <- length(unique(surpriseSongsDressColours$DressName))
dress_levels <- levels(factor(surpriseSongsDressColours$DressName))
oneRowPerConcertWithImages$DressName <- factor(oneRowPerConcertWithImages$DressName, levels = dress_levels)

main_plot <- ggplot(surpriseSongsDressColours, aes(x = as.Date(Date), y = DressName, color = ColourHex1)) +
    geom_point(size = 4, alpha = 1) +
    scale_color_identity() +
    theme_minimal() +
    labs(title = "", x = "", y = "" ) +
    geom_rect(aes(xmin = as.Date("2023-08-28"), xmax = as.Date("2023-11-08"),
                  ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +
    geom_rect(aes(xmin = as.Date("2023-11-27"), xmax = as.Date("2024-02-06"),
                  ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +  
    geom_rect(aes(xmin = as.Date("2024-03-10"), xmax = as.Date("2024-05-08"),
                  ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +
    geom_rect(aes(xmin = as.Date("2024-08-21"), xmax = as.Date("2024-10-17"),
                  ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +
    ## Vertical lines for the key events
    geom_vline(xintercept = as.Date("2024-05-09"), linetype = "dashed", color = "black") +
    geom_vline(xintercept = as.Date("2023-03-17"), linetype = "dashed", color = "black") +
    geom_vline(xintercept = as.Date("2024-10-18"), linetype = "dashed", color = "black") +
    geom_vline(xintercept = as.Date("2023-08-24"), linetype = "dashed", color = "black") +
    geom_vline(xintercept = as.Date("2024-02-07"), linetype = "dashed", color = "black") +
    geom_vline(xintercept = as.Date("2024-04-16"), linetype = "solid", color = "darkgray", linewidth = 2) +
    ## Changed to 16 (the right day is 19th) for vis requirements
    geom_vline(xintercept = as.Date("2023-07-07"), linetype = "solid", color = "purple", linewidth = 2) +
    geom_vline(xintercept = as.Date("2023-10-27"), linetype = "solid", color = "blue", linewidth = 2) +
    ## Text annotations for the events above
    annotate("text", x = as.Date("2024-05-09"), y = max_dress_level, 
             label = "Europe¹", color = "black", angle = -90, vjust = -0.5,
             family ="Gill Sans MT", size = 5) +
    annotate("text", x = as.Date("2023-03-17"), y = max_dress_level, 
             label = "United\nStates¹", color = "black", angle = -90, vjust = -0.2,
             family ="Gill Sans MT", size = 5) +
    annotate("text", x = as.Date("2024-10-18"), y = max_dress_level, 
             label = "North \nAmerica¹", color = "black", angle = -90, vjust = -0.2,
             family ="Gill Sans MT", size = 5) +
    annotate("text", x = as.Date("2023-08-24"), y = max_dress_level, 
             label = "Latin \nAmerica¹", color = "black", angle = -90, vjust = -0.2,
             family ="Gill Sans MT", size = 5) +
    annotate("text", x = as.Date("2024-02-07"), y = max_dress_level, 
             label = "Asia/\nOceania¹", color = "black", angle = -90, vjust = -0.2,
             family ="Gill Sans MT", size = 5) +
    annotate("text", x = as.Date("2024-04-16"), y = max_dress_level, 
             label = "TTPD²", color = "darkgray", angle = -90, vjust = -0.5,
             family ="Gill Sans MT", size = 5, 
             fontface = "bold") +
    annotate("text", x = as.Date("2023-07-07"), y = max_dress_level, 
             label = "Speak\nNow TV²", color = "purple", angle = -90, vjust = -0.2,
             family ="Gill Sans MT", size = 5) +
    annotate("text", x = as.Date("2023-10-27"), y = max_dress_level, 
             label = "1989\nTV²", color = "blue", angle = -90, vjust = -0.2,
             family ="Gill Sans MT", size = 5) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14, hjust = 0), 
          plot.title = element_text(hjust=0.5, size = 14, margin = margin(b = 20), face = "bold"),
          plot.margin = margin(t = -7, r = 0, b = 10, l = 0),
          text = element_text(color = "black", family = "Gill Sans MT", size = 14)) 

count_plot <- ggplot(oneRowPerConcertWithImages, aes(x = n, y = DressName, fill = DressName)) +
    geom_bar(stat = "identity", width = 0.8) +
    geom_image(
        aes(image = imagePath, x = n),  
        size = 0.09,                    
        nudge_x = 2,
        by = "height"                    
    ) +
    geom_text(
        aes(x = n + 3, label = paste0(n, " (", round(percentage, 1), "%)")),  
        hjust = 0,
        nudge_x = 3,
        color = "black",
        size = 5,
        family = "Gill Sans MT"
    ) +
    scale_fill_manual(values = colorPaletteDresses) +
    theme_minimal() +
    labs( title = "",x = "", y = "") +
    theme(
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "none",
        plot.margin = margin(t = -7, r = 0, b = 10, l = 0),
        text = element_text(color = "black", family = "Gill Sans MT", size = 14)
    ) + xlim(0, 50)

merged_plot <- plot_grid(
    count_plot, main_plot,
    ncol = 2,
    align = "h",
    axis = "tb",
    rel_widths = c(1.5, 3))

title_with_subtitle <- ggdraw() + 
    draw_label(
        "She Was Screaming Color",
        fontface = "bold",
        size = 20,
        y = 0.55,
        hjust = 0.5,
        fontfamily = "Gill Sans MT" 
    ) +
    draw_label(
        "Frequency and Timeline of Taylor Swift's Dress Colors Across Tour Legs¹ and Album Releases²",
        fontface = "plain",
        size = 16,
        y = 0.1,
        hjust = 0.5,
        fontfamily = "Gill Sans MT" )

plot_grid(
    title_with_subtitle, merged_plot,
    ncol = 1,
    rel_heights = c(0.2, 2))

ggsave("../plots/outfits-events.png", width = 19, height = 10, bg = "white")

##########################################
###### Taylor's Dictionary of Colour #####
##########################################
allSongsMetadata <- "../raw_data/album_info_metadata_neutral.xlsx"
allSongsMetadata <- readxl::read_excel(allSongsMetadata, sheet = "metadata")
source("colour_palletts.r")
rawColorData <- data.frame(
    colour = trimws(unlist(strsplit(allSongsMetadata$colour_MK, ";"))),
    meaning = trimws(unlist(strsplit(allSongsMetadata$colour_meaningMK, ";")))
) %>% filter(!is.na(colour) & !is.na(meaning))

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

## Calculate average sentiment for each individual color
individualColorSentiments <- colorSentimentScores %>%
    group_by(colour) %>%
    summarise(
        avgSentiment = mean(score, na.rm = TRUE),
        mentionCount = n()
    ) %>%
    ungroup()

## Create color groups mapping dataset
colorGroupMappings <- data.frame(
    colour = names(colorGroups),
    groupName = unlist(colorGroups)
)

## Calculate group-level sentiments
colorGroupSentiments <- individualColorSentiments %>%
    left_join(colorGroupMappings, by = "colour") %>%
    group_by(groupName) %>%
    summarise(
        groupSentiment = mean(avgSentiment, na.rm = TRUE),
        totalMentions = sum(mentionCount),
        colorsInGroup = n()
    ) %>%
    arrange(desc(groupSentiment))
## worcloud images/paterns
require(wordcloud)
join <- individualColorSentiments %>%
    left_join(colorGroupMappings, by = "colour") %>%
    left_join(data.frame(colour = names(colorPaletteColours),
                         colorPaletteColours = as.vector(colorPaletteColours)))

list <- split(join, join$groupName)
for(i in 1:length(list)){
    set.seed(1984)
    png(paste(names(list)[i], ".png", sep = ""), bg = 'transparent' )
    par(mar = c(0,0,0,0), mai = c(0,0,0,0))
    wordcloud(list[[i]]$colour, list[[i]]$mentionCount, min.freq = 0,
              colors =  list[[i]]$colorPaletteColours, ordered.colors = TRUE,
              scale = c(4,2))
    dev.off()
}
## cropping wordclouds
image_filenames <- unlist(list.files(pattern = ".png"))
require(GoodFibes)
crop.stack(image_filenames, save.images = TRUE)
library(packcircles)
## order by sentiment
colorGroupSentiments <- colorGroupSentiments[order(colorGroupSentiments$totalMentions),]
## layout only
coords <- circleProgressiveLayout(colorGroupSentiments$totalMentions,
                                 sizetype = 'area')
## overwrite y-vals with sentiment
coords$y <- 13.5*colorGroupSentiments$groupSentiment
coords$id <- colorGroupSentiments$groupName
## manual jigging
coords$x[2] <- -6.5
coords$x[3] <- 2
coords$x[4] <- 8.5
coords$x[6] <- 5
coords$x[6] <- 4.5
coords$x[8] <- -8
coords$x[9] <- -1
df.gg <- circleLayoutVertices(coords, npoints = 8, id = 4)

ggplot() + 
       geom_polygon(data = df.gg, aes(x, y, group = id), 
                    fill = "grey90", 
                    colour = "black") +
     geom_text(data = coords, aes(x, y, label = id))

require(ggpattern)
image_filenames <- unlist(list.files(pattern = "cropped"))
ggplot()  +
    geom_segment(data = data.frame(xend = -15, yend = 0, x = 6, y = 0),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 col = "grey", linetype = "dotted") + 
    geom_polygon_pattern(data = df.gg[df.gg$id == "black and white",], aes(x = x, y = y),
                         pattern = 'image', fill = "#C0C0C0", alpha = 0.1,
                         colour =  NA, pattern_filename = image_filenames[1]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == "blacks",], aes(x = x, y = y),
                         pattern = 'image', fill = "#000000", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[2]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == "blues",], aes(x = x, y = y),
                         pattern = 'image', fill = "#0000FF", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[3]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == "colorful",], aes(x = x, y = y),
                         pattern = 'image', fill = "#FF4500", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[4]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == "greens",], aes(x = x, y = y),
                         pattern = 'image', fill = "#008000", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[5]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == "purples",], aes(x = x, y = y),
                         pattern = 'image', fill = "#8A2BE2", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[6]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == "reds",], aes(x = x, y = y),
                         pattern = 'image', fill = "#FF0000", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[7]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == "whites",], aes(x = x, y = y),
                         pattern = 'image', fill = "#FFFFFF", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[8]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == "yellows",], aes(x = x, y = y),
                         pattern = 'image', fill = "#FFD700", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[9])  +
    scale_pattern_size(range = c(30,30)) +
    coord_equal() +  geom_segment(data = data.frame(x = -15, y = -6.75, xend = -15, yend = 13.5),
                                  aes(x = x, y = y, xend = xend, yend = yend),
                                  arrow = arrow(length = unit(0.2, "inches"))) +
    geom_segment(data = data.frame(xend = -15, yend = -6.75, x = -15, y = 13.5),
                                  aes(x = x, y = y, xend = xend, yend = yend),
                                  arrow = arrow(length = unit(0.2, "inches"))) +
    theme_void() + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    annotate("text", x = -16, y = -5.75, label = "Negative", angle = 90)  +
    annotate("text", x = -16, y = 12.5, label = "Positive", angle = 90)  +
    annotate("text", x = -16, y = 0, label = "Neutral", col = "grey", angle = 90) 

unlink("*.png")

ggsave("../plots/colour_dictionary.png", bg = "white")


##########################################
###### Surprise Song Colour Group ########
##########################################
surpriseSongsDressColours$groupName <- sapply(surpriseSongsDressColours$DressName, function(color) {
  if (color %in% c("Pink", "Flamingo pink")) return("reds")
  if (color %in% c("Green")) return("greens")
  if(color %in% c("Yellow", "Sunset orange")) return("yellows")
  if (color %in% c("Ocean blue", "Blue", "Blurple")) return ("blues")
  if (color %in% c("Popsicle", "Cotton candy", "Grapefruit")) return ("colorful")
  return("Neutral")
})

songs_with_single_color_group <- surpriseSongsDressColours %>%
  group_by(`Song title`) %>%
  summarize(
    total_performances = n(),
    unique_color_groups = n_distinct(groupName),
    color_group = first(groupName) 
  ) %>%
  filter(unique_color_groups == 1, total_performances > 1) %>%
  arrange(desc(total_performances))

single_color_performances <- surpriseSongsDressColours %>%
    filter(`Song title` %in% songs_with_single_color_group$`Song title`)

## pics
blues <- paste("../dress_images/images_high_res/cropped/", c("blue", "ocean_blue", "blurple"), ".jpg", sep = "")
reds <- paste("../dress_images/images_high_res/cropped/", c("pink", "flamingo_pink"), ".jpg", sep = "")
yellows <- paste("../dress_images/images_high_res/cropped/", c("yellow", "sunset_orange"), ".jpg", sep = "")

coords <- circleProgressiveLayout(table(single_color_performances$groupName),
                                  sizetype = 'area')
coords$id <- names(table(single_color_performances$groupName))
df.gg <- circleLayoutVertices(coords, npoints = 8, id = 4)
snames <- single_color_performances %>% select('Song title', groupName) %>%
    group_by(`Song title`) %>% mutate(count = n()) %>% ungroup() |> unique()
## annoingly adhoc
require(ggrepel)
set.seed(1984) ## for jitter repel
plot <- ggplot() + theme_void() +
    ## blues
    geom_polygon(data = df.gg[df.gg$id == "blues",], aes(x = x, y = y),
                 fill = "#0000FF", alpha = 0.05) +
    geom_text_repel(aes(x = coords$x[1], 
                        y = coords$y[1], 
                        label = snames$`Song title`[snames$groupName == "blues"]),
                    col = "#0000FF", nudge_y = -1.1, nudge_x = 0.1, segment.color = NA,
                    size = 1.5*snames$count[snames$groupName == "blues"], box.padding = 0.1,
                    family = "Gill Sans MT") +
    ## reds
    geom_polygon(data = df.gg[df.gg$id == "reds",], aes(x = x, y = y),
                 fill = "#FF0000", alpha = 0.05)  +
    geom_text_repel(aes(x = coords$x[2], 
                        y = coords$y[2], 
                        label = snames$`Song title`[snames$groupName == "reds"]),
                    col = "#FF0000", nudge_y = -0.9, nudge_x = 0.1, segment.color = NA,
                    size = 1.5*snames$count[snames$groupName == "reds"], box.padding = 0.1,
                    family = "Gill Sans MT") +
    ## yellows
    geom_polygon(data = df.gg[df.gg$id == "yellows",], aes(x = x, y = y),
                 fill = "#FFD700", alpha = 0.05)  +
    geom_text_repel(aes(x = coords$x[3], 
                        y = coords$y[3], 
                        label = snames$`Song title`[snames$groupName == "yellows"]),
                    col = "#FFD700", nudge_y = 1.4, nudge_x = 0, segment.color = NA,
                    size = 1.5*snames$count[snames$groupName == "yellows"], box.padding = 0.1,
                    family = "Gill Sans MT")

## image sizes relative to
## table(single_color_performances$DressName, single_color_performances$groupName)
set.seed(1984) ## for jitter repel
ggdraw() +
    draw_plot(plot) +
    draw_image(blues[1], -0.37, 0.23, scale = 0.5/3) +
    draw_image(blues[2],  -0.2, 0.32, scale = 0.8/3) +
    draw_image(blues[3],  -0.07, 0.26, scale = 0.4/3) +
    draw_image(reds[1], 0.1, 0.27, scale = 0.8/3) +
    draw_image(reds[2],  0.3, 0.33, scale = 0.7/3) +
    draw_image(yellows[1], -0.1, -0.25, scale = 0.7/3) +
    draw_image(yellows[2],  0.1, -0.3, scale = 1.1/3) 
   

ggsave("../plots/surprise_colour_groups.png", bg = "white")

##########################################
########### Album/Track Sentiment ########
##########################################
require(tidytext)
require(stopwords)
source("utility_functions.r")
source("useful_data.r")
tokenizedSongs <- createTokenizedLyricsF(allSongsMetadata) %>%
    anti_join(get_stopwords())
## manual removal of self-id stopwords words 
tokenizedSongs <- tokenizedSongs %>%
    filter(!word %in% wordsToRemove) %>%
    filter(!str_detect(word, "\\d+")) %>%
    filter(str_length(word) > 1)
## get sentiments
taylorLyrics <- calculateSentimentsF(tokenizedSongs) %>%
    mutate(sentiment_score = case_when(
               sentiment == "positive" ~ 1,
               sentiment == "negative" ~ -1,
               TRUE ~ 0  
           )) %>%
    mutate(album_name = factor(album_name, levels = albumOrder)) %>%
    filter(!is.na(album_name))
song_sentiment_scores_lyrics <- taylorLyrics %>%
    group_by(album_name, track_number, track_name) %>%
    summarize(
        net_sentiment = sum(sentiment_score),
        total_sentiment_words = n(),
        normalized_sentiment = sum(sentiment_score) / n(),
        .groups = "drop") %>%
    mutate(album_name = factor(album_name, levels = albumOrder),
           song_name =  normalize_song_name(track_name))
### album sentiment
ggplot(song_sentiment_scores_lyrics, aes(x = track_number,
                                         y = net_sentiment,
                                         fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = colorPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "The emotional spectrum across Taylor Swift's discography",
    x = "Track Number",
    y = "Net Sentiment (Positive - Negative Words)") +
    theme(plot.title = element_text(hjust=0.5))

##########################################
########### Instrument Sentiment #########
##########################################
## still unsure of what was origionaly done
instrument_sentiment_comparison <- surpriseSongsDressColours %>% select('Song title', "Instrument") %>%
    mutate(song_name = normalizeSongNameF(surpriseSongsDressColours$'Song title')) %>%
    left_join(., song_sentiment_scores_lyrics, relationship = "many-to-many")
instrument_sentiment_summary <- instrument_sentiment_comparison %>%
  group_by(Instrument) %>%
  summarize(
    mean_sentiment = mean(net_sentiment, na.rm = TRUE),
    median_sentiment = median(net_sentiment, na.rm = TRUE),
    sd_sentiment = sd(net_sentiment, na.rm = TRUE),
    n_songs = n(),
    .groups = "drop"
  )
### HMM 
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
    axis.title = element_text(linewidth = 12),
    axis.text = element_text(linewidth = 10),
    legend.position = "bottom",
    legend.title = element_text(linewidth = 12),
    legend.text = element_text(linewidth = 10)
  )
