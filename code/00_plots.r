require(tidyverse)
require(ggimage)
require(cowplot)
## require(extrafont) ## optional for GILL Sans MT font

## Data and wrangling etc
surpriseSongsDressColours <- "../raw_data/surprise_songs.xlsx"
surpriseSongsDressColours <- readxl::read_excel(surpriseSongsDressColours, sheet = "List")
pianoSongsData <- surpriseSongsDressColours[surpriseSongsDressColours$Instrument == "Piano",]
dressColorMapping <- unique(pianoSongsData %>% select(DressName, ColourHex1))
colorPaletteDresses <- setNames(dressColorMapping$ColourHex1, dressColorMapping$DressName)
pianoSongsData$Date <- as.Date(pianoSongsData$Date)
pathToDressColours <- "../dress_images/images_high_res/cropped/"

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
            TRUE ~ NA_character_
        )) -> pianoSongsDataWithImages

##############################
### The Most Worn Looks ######
##############################
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
    ) + ylim(0, 35)
ggsave("../plots/the-most-worn-looks.png", width = 10, height = 8, bg = "white")

##########################################
### Eras' Outfits and Special Events #####
##########################################
dress_first_appearance <- pianoSongsData %>%
  group_by(DressName) %>%
  summarize(FirstAppearance = min(Date)) %>%
    arrange((FirstAppearance))
pianoSongsData$DressName <- factor(pianoSongsData$DressName, 
                                   levels = dress_first_appearance$DressName)
max_dress_level <- length(unique(pianoSongsData$DressName))
dress_levels <- levels(factor(pianoSongsData$DressName))
pianoSongsDataWithImages$DressName <- factor(pianoSongsDataWithImages$DressName, levels = dress_levels)

main_plot <- ggplot(pianoSongsData, aes(x = Date, y = DressName, color = ColourHex1)) +
    geom_point(size = 4, alpha = 0.5) +
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

count_plot <- ggplot(pianoSongsDataWithImages, aes(x = n, y = DressName, fill = DressName)) +
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
allSongsMetadata <- "../raw_data/album_info_metadata.xlsx"
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
coords$y <- colorGroupSentiments$y <- 13.5*colorGroupSentiments$groupSentiment
coords$x <- colorGroupSentiments$x <- 2*coords$x
## manual jigging
coords$x[3] <- 2
coords$x[5] <- -2.5
coords$x[6] <- -1
coords$x[7] <- -3
coords$x[8] <- 5
coords$x[9] <- -10
df.gg <- circleLayoutVertices(coords, npoints = 8)

require(ggpattern)
image_filenames <- unlist(list.files(pattern = "cropped"))
ggplot()  +
    geom_segment(data = data.frame(xend = -15, yend = 0, x = 6, y = 0),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 col = "grey", linetype = "dotted") + 
    geom_polygon_pattern(data = df.gg[df.gg$id == 1,], aes(x = x, y = y),
                         pattern = 'image', fill = "#C0C0C0", alpha = 0.1,
                         colour =  NA, pattern_filename = image_filenames[1]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == 2,], aes(x = x, y = y),
                         pattern = 'image', fill = "#000000", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[2]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == 3,], aes(x = x, y = y),
                         pattern = 'image', fill = "#0000FF", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[3]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == 4,], aes(x = x, y = y),
                         pattern = 'image', fill = "#FF4500", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[4]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == 5,], aes(x = x, y = y),
                         pattern = 'image', fill = "#008000", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[5]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == 6,], aes(x = x, y = y),
                         pattern = 'image', fill = "#8A2BE2", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[6]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == 7,], aes(x = x, y = y),
                         pattern = 'image', fill = "#FF0000", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[7]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == 8,], aes(x = x, y = y),
                         pattern = 'image', fill = "#FFFFFF", alpha = 0.1,
                         colour = NA, pattern_filename = image_filenames[8]) +
    geom_polygon_pattern(data = df.gg[df.gg$id == 9,], aes(x = x, y = y),
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

table(single_color_performances$DressName,single_color_performances$groupName)
table(single_color_performances$groupName,single_color_performances$'Song title' )

## pics
blues <- paste("../dress_images/images_high_res/cropped/", c("blue", "ocean_blue"), ".jpg", sep = "")
reds <- paste("../dress_images/images_high_res/cropped/", c("pink", "flamingo_pink"), ".jpg", sep = "")
yellows <- paste("../dress_images/images_high_res/cropped/", c("yellow", "sunset_orange"), ".jpg", sep = "")

coords <- circleProgressiveLayout(table(single_color_performances$groupName),
                                  sizetype = 'area')
df.gg <- circleLayoutVertices(coords, npoints = 8)
snames <- single_color_performances %>% select('Song title', groupName) %>%
    group_by(`Song title`) %>% mutate(count = n()) %>% ungroup() |> unique()
## annoingly adhoc
require(ggrepel)
set.seed(1984) ## for jitter repel
plot <- ggplot() + theme_void() +
    ## blues
    geom_polygon(data = df.gg[df.gg$id == 1,], aes(x = x, y = y),
                 fill = "#0000FF", alpha = 0.05) +
    geom_text_repel(aes(x = coords$x[1], 
                        y = coords$y[1], 
                        label = snames$`Song title`[snames$groupName == "blues"]),
                    col = "#0000FF", nudge_y = -0.6, nudge_x = 0.3, segment.color = NA,
                    size = 1.5*snames$count[snames$groupName == "blues"], box.padding = 0.5,
                    family = "Gill Sans MT") +
    ## reds
    geom_polygon(data = df.gg[df.gg$id == 2,], aes(x = x, y = y),
                 fill = "#FF0000", alpha = 0.05)  +
    geom_text_repel(aes(x = coords$x[2], 
                        y = coords$y[2], 
                        label = snames$`Song title`[snames$groupName == "reds"]),
                    col = "#FF0000", nudge_y = -0.8, nudge_x = 0.5, segment.color = NA,
                    size = 1.5*snames$count[snames$groupName == "reds"], box.padding = 0.5,
                    family = "Gill Sans MT") +
    ## yellows
    geom_polygon(data = df.gg[df.gg$id == 3,], aes(x = x, y = y),
                 fill = "#FFD700", alpha = 0.05)  +
    geom_text_repel(aes(x = coords$x[3], 
                        y = coords$y[3], 
                        label = snames$`Song title`[snames$groupName == "yellows"]),
                    col = "#FFD700", nudge_y = 1, nudge_x = -0.6, segment.color = NA,
                    size = 1.5*snames$count[snames$groupName == "yellows"], box.padding = 0.5,
                    family = "Gill Sans MT")


set.seed(1984) ## for jitter repel
ggdraw() +
    draw_plot(plot) +
    draw_image(blues[1], -0.35, 0.2, scale = 0.3/2) +
    draw_image(blues[2],  -0.2, 0.27, scale = 0.3/2) +
    draw_image(reds[1], 0.08, 0.2, scale = 0.6/2) +
    draw_image(reds[2],  0.3, 0.33, scale = 0.4/2) +
    draw_image(yellows[1], -0.15, -0.31, scale = 0.5/2) +
    draw_image(yellows[2],  0.05, -0.2, scale = 0.6/2) 
   

ggsave("../plots/surprise_colour_groups.png", bg = "white")

##########################################
###### Instrument Sentiment ##############
##########################################
