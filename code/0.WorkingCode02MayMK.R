
# Load packages -----------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(taylor)
library(ggplot2)
library(lubridate)
library(vistime)
library(tidytext)
library(igraph)
library(ggimage)
library(ggrepel)
library(patchwork)
library(corrplot)
library(forcats)
library(cowplot)
library(plotly)
library(grid)
library(gridExtra)
library(extrafont)
#font_import() #only once
loadfonts() #every session


# Load datasets -----------------------------------------------------------


## Renaming the datasets from the taylor package because I don't like underscores haha!
taylorAlbums <- taylor_albums #includes album names, release dates, critics scores and user scores
taylorAlbumSongs <- taylor_album_songs #includes album names, album releases, track numbers, track names,

#write.xlsx(taylorAlbumSongs, "C:/Users/mklemm/OneDrive - AUCKLAND MUSEUM/Documents/Personal/FunAnalysis/taylorAlbumSongs.xlsx")
# Exported it so that I could edit the names of the songs properly


## featuring, all spotify metrics and lyrics
taylorAllSongs <- taylor_all_songs ##same as taylorAlbumSongs, except it also includes
## songs she wrote/sang and are not in her albums

# Load the Excel files
# include the edited taylorAlbumSongs (removed all the Taylor's version, from the vault, etc)
taylorAlbumSongs <- "taylorAlbumSongs.xlsx"
surpriseSongsDressColours <- "surpriseSongsDressColoursMashupsLong.xlsx" #Edited, all the
# mashups are underneath the main songs, and the names of the songs are changed
allSongsMetadata <- "albumInfoMetadataNeutralMK.xlsx"
relationshipsTimeline <- "surpriseSongsDressColours.xlsx"

surpriseSongsDressColours <- read_excel(surpriseSongsDressColours, sheet = "List")
allSongsMetadata <- read_excel(allSongsMetadata, sheet = "metadata")
relationshipsTimeline <- read_excel(relationshipsTimeline, sheet = "Relationships")

#Now, I need to select only one row per concert, which I can do by choosing only
# the first song played in each concert
oneRowPerConcert <- surpriseSongsDressColours %>%
  group_by(Date) %>%
  arrange(Date, Order) %>%  # Assuming 'Order' indicates song order in the concert
  slice(1) %>%
  ungroup()





# Colour palettes ---------------------------------------------------------

colorPaletteAlbums <- c(
  "Taylor Swift" = "#90EE90",    
  "Fearless (Taylor's Version)" = "#FFD700",        
  "Speak Now (Taylor's Version)" = "#8000ff",       
  "Red (Taylor's Version)" = "#D34C58",             
  "1989 (Taylor's Version)" = "#88C4D7",           
  "Reputation" = "#1A1A1A",      
  "Lover" = "#F1B3D1",           
  "folklore" = "#9E9E9E",        
  "evermore" = "#cc621b",        
  "Midnights" = "#2C2E52",
  "THE TORTURED POETS DEPARTMENT" = "gray"
)

colorPaletteRelationships <- c(
  "Travis Kelce" = "#E31837",    # Kansas chiefs colour
  "Matty Healy" = "#800000",     # Maroon
  "Joe Alwyn" = "#4B0082",       # Indigo blue
  "Karlie Kloss" = "#FFD700",   # Golden
  "Dianna Agron" = "#C8B575",   # Green eyes
  "Tom Hiddleston" = "#4f42b5",  # Ocean blue eyes
  "Calvin Harris" = "#91aac0",    # 1989 colour
  "Harry Styles" = "#FC8EAC",      # Flamingo pink
  "Jake Gyllenhaal" = "#D62728",  # burning red
  "Conor Kennedy" = "#00AEF3", # democratic party
  "John Mayer" = "#4a6583", # you paint me a blue sky then go back and turn it into rain
  "Taylor Lautner" = "darkgray", # I wore a dress, you wore a dark gray t shirt
  "Lucas Till" = "orange", #hannah montana colours 
  "Joe Jonas" = "#2CA02C" #his favorite colour
)


lineTypeRelationships <- c(
  "Dianna Agron" = "dotted", 
  "Karlie Kloss" = "dotted", 
  "Travis Kelce" = "solid",    
  "Matty Healy" = "solid",     
  "Joe Alwyn" = "solid",      
  "Karlie Kloss" = "solid",   
  "Dianna Agron" = "solid",   
  "Tom Hiddleston" = "solid",  
  "Calvin Harris" = "solid",   
  "Harry Styles" = "solid",      
  "Jake Gyllenhaal" = "solid",  
  "Conor Kennedy" = "solid", 
  "John Mayer" = "solid", 
  "Taylor Lautner" = "solid", 
  "Lucas Till" = "solid", 
  "Joe Jonas" = "solid")

colorPaletteColours <- c(
  "amber" = "#FFBF00",
  "aquamarine" = "#7FFFD4",
  "aurora borealis green" = "#78E08F",
  "black" = "#000000",
  "black and blue" = "#00008B",
  "black and white" = "#C0C0C0",
  "blackout" = "#1A1A1A",
  "bleached" = "#F5F5DC",
  "blood monlit" = "#8A0303",
  "blood-soaked" = "#8B0000",
  "blue" = "#0000FF",
  "blues" = "#1E90FF",
  "bluest" = "#4682B4",
  "bronze" = "#CD7F32",
  "burgundy" = "#800020",
  "burning red" = "#FF2400",
  "cherry" = "#DE3163",
  "colour" = "#808080",
  "colours" = "#A9A9A9",
  "crimson" = "#DC143C",
  "crimson clover" = "#B22222",
  "dark gray" = "#A9A9A9",
  "darkest gray" = "#696969",
  "deep blue" = "#00008B",
  "deep brown" = "#654321",
  "evergreen" = "#228B22",
  "flamingo pink" = "#FC8EAC",
  "gold" = "#FFD700",
  "golden" = "#FFDF00",
  "gray" = "#808080",
  "green" = "#008000",
  "greige" = "#B8B799",
  "hospital gray" = "#D3D3D3",
  "indigo" = "#4B0082",
  "key lime green" = "#BFFF00",
  "lavender" = "#E6E6FA",
  "light pink" = "#FFB6C1",
  "lilac" = "#C8A2C8",
  "maroon" = "#800000",
  "moonstone" = "#3AA8C1",
  "neon" = "#39FF14",
  "ocean blue" = "#1E90FF",
  "ocean wave blues" = "#4682B4",
  "opal" = "#A8C3BC",
  "orange" = "#FFA500",
  "pale blue" = "#AFEEEE",
  "pastel" = "#FFD1DC",
  "pink" = "#FFC0CB",
  "plaid" = "#BDB76B",
  "purple-pink" = "#DDA0DD",
  "rainbow" = "#FF4500", # Can adjust to gradient in plotting
  "red" = "#FF0000",
  "redhaired" = "#B22222",
  "redneck" = "#C71585",
  "rose" = "#FF007F",
  "rose golden" = "#E6BE8A",
  "rosy" = "#BC8F8F",
  "sapphire" = "#0F52BA",
  "scarlet" = "#FF2400",
  "scarlet maroon" = "#800000",
  "screaming colour" = "#FF4500",
  "silver" = "#C0C0C0",
  "snow white" = "#FFFFFF",
  "stained glass" = "#2E8B57",
  "tangerine" = "#F28500",
  "teal" = "#008080",
  "ultraviolet" = "#8A2BE2",
  "violets" = "#EE82EE",
  "white" = "#FFFFFF",
  "wine-stained" = "#722F37"
)


## Or, I can also group some colour:

colorGroups <- c(
  # Purples
  "violets" = "purples",
  "ultraviolet" = "purples",
  "purple-pink" = "purples",
  "lavender" = "purples",
  "lilac" = "purples",
  
  # Yellows
  "amber" = "yellows",
  "bronze" = "yellows",
  "deep brown" = "yellows",
  "gold" = "yellows",        
  "golden" = "yellows",      
  "greige" = "yellows",
  "rose golden" = "yellows",
  "orange" = "yellows",
  "tangerine" = "yellows",   
  
  # Reds
  "blood monlit" = "reds",
  "blood-soaked" = "reds",
  "burning red" = "reds",    
  "crimson" = "reds",
  "crimson clover" = "reds", 
  "flamingo pink" = "reds",
  "burgundy" = "reds",
  "cherry" = "reds",
  "maroon" = "reds",         
  "pink" = "reds",
  "light pink" = "reds",
  "red" = "reds",           
  "redhaired" = "reds",
  "redneck" = "reds",
  "rose" = "reds",          
  "rosy" = "reds",          
  "scarlet" = "reds",       
  "scarlet maroon" = "reds",
  "wine-stained" = "reds",
  
  # Blues
  "aquamarine" = "blues",   
  "blue" = "blues",         
  "blues" = "blues",        
  "bluest" = "blues",       
  "deep blue" = "blues",
  "indigo" = "blues",
  "ocean blue" = "blues",
  "ocean wave blues" = "blues",
  "opal" = "blues",
  "pale blue" = "blues",
  "sapphire" = "blues",
  
  # Greens
  "aurora borealis green" = "greens",
  "evergreen" = "greens",
  "green" = "greens",
  "key lime green" = "greens",
  "teal" = "greens",        
  
  # Colorful
  "colour" = "colorful",    
  "colours" = "colorful",   
  "moonstone" = "colorful", 
  "neon" = "colorful",      
  "pastel" = "colorful",    
  "plaid" = "colorful",     
  "rainbow" = "colorful",
  "screaming colour" = "colorful", 
  "stained glass" = "colorful",
  
  # Black and white
  "black and white" = "black and white",
  
  # Whites
  "bleached" = "whites",    
  "greige" = "whites",
  "hospital gray" = "whites", 
  "gray" = "whites",        
  "white" = "whites",       
  "snow white" = "whites",  
  
  # Blacks
  "black" = "blacks",       
  "blackout" = "blacks",    
  "black and blue" = "blacks",
  "dark gray" = "blacks",
  "darkest gray" = "blacks", 
  "silver" = "blacks"
)


## Organize the colour syntax
dressColorMapping <- unique(oneRowPerConcert %>% select(DressName, ColourHex1))
colorPaletteDresses <- setNames(dressColorMapping$ColourHex1, dressColorMapping$DressName)
rm(dressColorMapping)



# Normalize song names so the datasets match ------------------------------

# Function to normalize song names (remove punctuation, ignore case)
normalizeSongNameF <- function(songname) {
  songname <- tolower(songname)                
  songname <- gsub("[[:punct:]]", "", songname) 
  songname <- trimws(songname)                
  return(songname)
}

# Filter dataset for piano songs
pianoSongsF <- function(data) {
  return(data %>% filter(Instrument == "Piano"))
}

surpriseSongsDressColours <- surpriseSongsDressColours %>%
  mutate(normalizedSongName = normalizeSongNameF(`Song title`))

allSongsMetadata <- allSongsMetadata %>%
  mutate(normalizedSongName = normalizeSongNameF(`track_name`))


# Is the distribution of colours random? -------------------------------------------------

## First, there's the introduction of different colours in each leg of the tour,
# so I need to analyze them separately

## Also, because there are two sets of songs per concert, I want to use only
# half of them at this time so I'll filter for the songs played on the piano (could've been
# on the guitar) - this will be changed when I compare songs x dress

unique(oneRowPerConcert$Legs)

oneRowPerConcertFirstLegs <- oneRowPerConcert[oneRowPerConcert$Legs %in% c("First leg", "Latin America", "Asia-Oceania"),]
oneRowPerConcertEuropeanLeg <- oneRowPerConcert[oneRowPerConcert$Legs == "European leg",]
oneRowPerConcertFinalLeg <- oneRowPerConcert[oneRowPerConcert$Legs == "Final leg",]


str(oneRowPerConcertFirstLegs)


## FIRST LEGS: Americas and Asia-Oceania


# I'm adding a number for each colour name for the linear modelling
oneRowPerConcertFirstLegs <- oneRowPerConcertFirstLegs %>%
  mutate(DressName_numeric = as.numeric(factor(DressName)))
# this adds a column with the dress colour of the previous concert
oneRowPerConcertFirstLegs <- oneRowPerConcertFirstLegs %>%
  mutate(DressName_lag = lag(DressName_numeric))

oneRowPerConcertFirstLegs <- oneRowPerConcertFirstLegs %>%
  filter(!is.na(DressName_lag))

lm_resultFirstLegs <- lm(DressName_numeric ~ DressName_lag, data = oneRowPerConcertFirstLegs)

summary(lm_resultFirstLegs)


## EUROPEAN LEG: 

# I'm adding a number for each colour name for the linear modelling
oneRowPerConcertEuropeanLeg <- oneRowPerConcertEuropeanLeg %>%
  mutate(DressName_numeric = as.numeric(factor(DressName)))
# this adds a column with the dress colour of the previous concert
oneRowPerConcertEuropeanLeg <- oneRowPerConcertEuropeanLeg %>%
  mutate(DressName_lag = lag(DressName_numeric))

oneRowPerConcertEuropeanLeg <- oneRowPerConcertEuropeanLeg %>%
  filter(!is.na(DressName_lag))

lm_resultEuropeanLeg <- lm(DressName_numeric ~ DressName_lag, data = oneRowPerConcertEuropeanLeg)

summary(lm_resultEuropeanLeg)



## FINAL LEG: 

# I'm adding a number for each colour name for the linear modelling
oneRowPerConcertFinalLeg <- oneRowPerConcertFinalLeg %>%
  mutate(DressName_numeric = as.numeric(factor(DressName)))
# this adds a column with the dress colour of the previous concert
oneRowPerConcertFinalLeg <- oneRowPerConcertFinalLeg %>%
  mutate(DressName_lag = lag(DressName_numeric))

oneRowPerConcertFinalLeg <- oneRowPerConcertFinalLeg %>%
  filter(!is.na(DressName_lag))

lm_resultFinalLeg <- lm(DressName_numeric ~ DressName_lag, data = oneRowPerConcertFinalLeg)

summary(lm_resultFinalLeg)


# How to visualize it using a transition matrix: First Legs  -------------------------------

# Create transition matrix
colorTransitionsFirstLegs <- table(
  oneRowPerConcertFirstLegs$DressName[-1],  # Current colors (excluding first row)
  oneRowPerConcertFirstLegs$DressName[-nrow(oneRowPerConcertFirstLegs)]  # Previous colors (excluding last row)
)

# Calculate conditional probabilities
transitionProbabilitiesFirstLegs <- prop.table(colorTransitionsFirstLegs, margin = 2)

# Convert transition matrix to long format for plotting
transitionsDFFirstLegs <- as.data.frame.matrix(colorTransitionsFirstLegs)
transitionsDFFirstLegs$current_color <- rownames(transitionsDFFirstLegs)
transitionsLongFirstLegs <- transitionsDFFirstLegs %>%
  pivot_longer(cols = -current_color,
               names_to = "previous_color",
               values_to = "count")

# Create heatmap
ggplot(transitionsLongFirstLegs, aes(x = previous_color, y = current_color, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  theme_minimal() +
  labs(x = "Previous Concert's Dress Color",
       y = "Current Concert's Dress Color",
       title = "Dress Color Transition Heatmap - First Legs",
       fill = "Number of\nTransitions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Create a sequential visualization
concertColorsFirstLegs <- oneRowPerConcertFirstLegs %>%
  select(Date, DressName) %>%
  mutate(concert_number = row_number())

# Plot color sequence
ggplot(concertColorsFirstLegs, aes(x = concert_number, y = 1)) +
  geom_tile(aes(fill = DressName), width = 0.9) +
  scale_fill_manual(values = colorPaletteDresses) +
  theme_minimal() +
  labs(x = "Concert Number",
       y = NULL,
       title = "Sequential Dress Color Choices - First Legs",
       fill = "Dress Color") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# Print summary statistics

print(colorTransitionsFirstLegs)


print(round(transitionProbabilitiesFirstLegs, 3))



# Chi-square test for independence
chiSquareResultsFirstLegs <- chisq.test(colorTransitionsFirstLegs)

print(chiSquareResultsFirstLegs)


# Create graphFirstLegs from transition matrix
graphFirstLegs <- graph_from_adjacency_matrix(
  as.matrix(colorTransitionsFirstLegs),
  mode = "directed",
  weighted = TRUE
)

# Get vertex names to match with color palette
vertexNamesFirstLegs <- V(graphFirstLegs)$name

# Create color vector for vertices
vertex_colors <- colorPaletteDresses[vertexNamesFirstLegs]

# Plot the network with custom colors
plot(graphFirstLegs,
     edge.width = E(graphFirstLegs)$weight,
     vertex.color = vertex_colors,  # Use custom colors
     vertex.size = 30,
     vertex.label.color = "black",
     edge.curved = 0.3,
     main = "Dress Color Transition Network: First Legs")


# How to visualize it using a transition matrix: European Leg  -------------------------------

# Create transition matrix
colorTransitionsEuropeanLeg <- table(
  oneRowPerConcertEuropeanLeg$DressName[-1],  # Current colors (excluding first row)
  oneRowPerConcertEuropeanLeg$DressName[-nrow(oneRowPerConcertEuropeanLeg)]  # Previous colors (excluding last row)
)

# Calculate conditional probabilities
transitionProbabilitiesEuropeanLeg <- prop.table(colorTransitionsEuropeanLeg, margin = 2)

# Convert transition matrix to long format for plotting
transitionsDFEuropeanLeg <- as.data.frame.matrix(colorTransitionsEuropeanLeg)
transitionsDFEuropeanLeg$current_color <- rownames(transitionsDFEuropeanLeg)
transitionsLongEuropeanLeg <- transitionsDFEuropeanLeg %>%
  pivot_longer(cols = -current_color,
               names_to = "previous_color",
               values_to = "count")

# Create heatmap
ggplot(transitionsLongEuropeanLeg, aes(x = previous_color, y = current_color, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  theme_minimal() +
  labs(x = "Previous Concert's Dress Color",
       y = "Current Concert's Dress Color",
       title = "Dress Color Transition Heatmap - European Leg",
       fill = "Number of\nTransitions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Create a sequential visualization
concertColorsEuropeanLeg <- oneRowPerConcertEuropeanLeg %>%
  select(Date, DressName) %>%
  mutate(concert_number = row_number())

# Plot color sequence
ggplot(concertColorsEuropeanLeg, aes(x = concert_number, y = 1)) +
  geom_tile(aes(fill = DressName), width = 0.9) +
  scale_fill_manual(values = colorPaletteDresses) +
  theme_minimal() +
  labs(x = "Concert Number",
       y = NULL,
       title = "Sequential Dress Color Choices - European Leg",
       fill = "Dress Color") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# Print summary statistics

print(colorTransitionsEuropeanLeg)


print(round(transitionProbabilitiesEuropeanLeg, 3))




# Chi-square test for independence
chiSquareResultsEuropeanLeg <- chisq.test(colorTransitionsEuropeanLeg)

print(chiSquareResultsEuropeanLeg)


# Create graphEuropeanLeg from transition matrix
graphEuropeanLeg <- graph_from_adjacency_matrix(
  as.matrix(colorTransitionsEuropeanLeg),
  mode = "directed",
  weighted = TRUE
)

# Get vertex names to match with color palette
vertexNamesEuropeanLeg <- V(graphEuropeanLeg)$name

# Create color vector for vertices
vertex_colors <- colorPaletteDresses[vertexNamesEuropeanLeg]

# Plot the network with custom colors
plot(graphEuropeanLeg,
     edge.width = E(graphEuropeanLeg)$weight,
     vertex.color = vertex_colors,  # Use custom colors
     vertex.size = 30,
     vertex.label.color = "black",
     edge.curved = 0.3,
     main = "Dress Color Transition Network: European Leg")

# How to visualize it using a transition matrix: Final leg  -------------------------------

# Create transition matrix
colorTransitionsFinalLeg <- table(
  oneRowPerConcertFinalLeg$DressName[-1],  # Current colors (excluding first row)
  oneRowPerConcertFinalLeg$DressName[-nrow(oneRowPerConcertFinalLeg)]  # Previous colors (excluding last row)
)

# Calculate conditional probabilities
transitionProbabilitiesFinalLeg <- prop.table(colorTransitionsFinalLeg, margin = 2)

# Convert transition matrix to long format for plotting
transitionsDFFinalLeg <- as.data.frame.matrix(colorTransitionsFinalLeg)
transitionsDFFinalLeg$current_color <- rownames(transitionsDFFinalLeg)
transitionsLongFinalLeg <- transitionsDFFinalLeg %>%
  pivot_longer(cols = -current_color,
               names_to = "previous_color",
               values_to = "count")

# Create heatmap
ggplot(transitionsLongFinalLeg, aes(x = previous_color, y = current_color, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  theme_minimal() +
  labs(x = "Previous Concert's Dress Color",
       y = "Current Concert's Dress Color",
       title = "Dress Color Transition Heatmap - Final leg",
       fill = "Number of\nTransitions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Create a sequential visualization
concertColorsFinalLeg <- oneRowPerConcertFinalLeg %>%
  select(Date, DressName) %>%
  mutate(concert_number = row_number())

# Plot color sequence
ggplot(concertColorsFinalLeg, aes(x = concert_number, y = 1)) +
  geom_tile(aes(fill = DressName), width = 0.9) +
  scale_fill_manual(values = colorPaletteDresses) +
  theme_minimal() +
  labs(x = "Concert Number",
       y = NULL,
       title = "Sequential Dress Color Choices - Final leg",
       fill = "Dress Color") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# Print summary statistics

print(colorTransitionsFinalLeg)


print(round(transitionProbabilitiesFinalLeg, 3))


# Chi-square test for independence
chiSquareResultsFinalLeg <- chisq.test(colorTransitionsFinalLeg)

print(chiSquareResultsFinalLeg)


# Create graphFinalLeg from transition matrix
graphFinalLeg <- graph_from_adjacency_matrix(
  as.matrix(colorTransitionsFinalLeg),
  mode = "directed",
  weighted = TRUE
)

# Get vertex names to match with color palette
vertexNamesFinalLeg <- V(graphFinalLeg)$name

# Create color vector for vertices
vertex_colors <- colorPaletteDresses[vertexNamesFinalLeg]

# Plot the network with custom colors
plot(graphFinalLeg,
     edge.width = E(graphFinalLeg)$weight,
     vertex.color = vertex_colors,  # Use custom colors
     vertex.size = 30,
     vertex.label.color = "black",
     edge.curved = 0.3,
     main = "Dress Color Transition Network: Final leg")


# Plotting dress colours  ------------------------------------------------------------


# Now, let's plot the frequency of each dress

oneRowPerConcert %>%
  count(DressName) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(DressName, -n), y = n, fill = DressName)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = colorPaletteDresses) +
  theme_minimal() +
  labs(title = "Distribution of Dress Colors",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none") 

oneRowPerConcert$Date <- as.Date(oneRowPerConcert$Date)


# Adding images to the dress plot -----------------------------------------

unique(oneRowPerConcert$DressName)

pathToDressColours <- "C:/Users/mklemm/OneDrive - AUCKLAND MUSEUM/Documents/Personal/FunAnalysis/DressesColours/ImprovedQuality/Cropped/"


oneRowPerConcert %>%
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
  ) -> oneRowPerConcertWithImages




dressesVis <- ggplot(oneRowPerConcertWithImages, aes(x = reorder(DressName, -n), y = n, fill = DressName)) +
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

dressesVis



# Tour dress colours and special events -----------------------------------


ggplot(oneRowPerConcert, aes(x = Date, y = DressName, color = ColourHex1)) +
  geom_point(size = 4) +
  scale_color_identity() +
  theme_minimal() +
  labs(
    title = "Dress Colors and Special Events",
    x = "Date",
    y = "Dress Color"
  ) +
  # Adding transparent gray block for the break periods
  geom_rect(aes(xmin = as.Date("2023-08-28"), xmax = as.Date("2023-11-08"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +
  geom_rect(aes(xmin = as.Date("2023-11-27"), xmax = as.Date("2024-02-06"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +  
  geom_rect(aes(xmin = as.Date("2024-03-10"), xmax = as.Date("2024-05-08"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +
  geom_rect(aes(xmin = as.Date("2024-08-21"), xmax = as.Date("2024-10-17"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, color = NA) +
  # Adding vertical lines for the key events
  geom_vline(xintercept = as.Date("2024-05-09"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2024-10-18"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2023-08-24"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2024-02-07"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2024-04-19"), linetype = "dotted", color = "darkgray") +
  geom_vline(xintercept = as.Date("2023-07-07"), linetype = "dotted", color = "purple") +
  geom_vline(xintercept = as.Date("2023-10-27"), linetype = "dotted", color = "#88C4D7") +
  
  # Adding text annotations for the events
  annotate("text", x = as.Date("2024-05-09"), y = max(oneRowPerConcert$DressName), 
           label = "Europe", color = "black", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2024-10-18"), y = max(oneRowPerConcert$DressName), 
           label = "North \nAmerica", color = "black", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2023-08-24"), y = max(oneRowPerConcert$DressName), 
           label = "Latin \nAmerica", color = "black", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2024-02-07"), y = max(oneRowPerConcert$DressName), 
           label = "Asia/\nOceania", color = "black", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2024-04-19"), y = max(oneRowPerConcert$DressName), 
           label = "TTPD", color = "darkgray", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2023-07-07"), y = max(oneRowPerConcert$DressName), 
           label = "Speak\nNow TV", color = "purple", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2023-10-27"), y = max(oneRowPerConcert$DressName), 
           label = "1989\nTV", color = "blue", angle = -90, vjust = -0.5) +
  
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust=0.5))


# Trying to improve the code above: ---------------------------------------


# First, find the first date for each dress
dress_first_appearance <- oneRowPerConcert %>%
  group_by(DressName) %>%
  summarize(FirstAppearance = min(Date)) %>%
  arrange((FirstAppearance))

# Convert DressName to a factor with ordered levels
oneRowPerConcert$DressName <- factor(oneRowPerConcert$DressName, 
                                     levels = dress_first_appearance$DressName)

max_dress_level <- length(unique(oneRowPerConcert$DressName))

ggplot(oneRowPerConcert, aes(x = Date, y = DressName, color = ColourHex1)) +
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
        plot.margin = margin(t = 10, b = 10)
  )




# Merging the timeline plot with the dress distribution -------------------

# Create the main timeline plot
main_plot <- ggplot(oneRowPerConcert, aes(x = Date, y = DressName, color = ColourHex1)) +
  geom_point(size = 4, alpha = 0.5) +
  scale_color_identity() +
  theme_minimal() +
  labs(
    title = "",
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
  geom_vline(xintercept = as.Date("2023-03-17"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2024-10-18"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2023-08-24"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2024-02-07"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2024-04-16"), linetype = "solid", color = "darkgray", size=2) + ## Changed to 16 (the right day is 19th) for vis requirements
  geom_vline(xintercept = as.Date("2023-07-07"), linetype = "solid", color = "purple", size=2) +
  geom_vline(xintercept = as.Date("2023-10-27"), linetype = "solid", color = "blue", size=2) +
  
  # Text annotations for the events above
  annotate("text", x = as.Date("2024-05-09"), y = max_dress_level, 
           label = "Europe¹", color = "black", angle = -90, vjust = -0.5, family ="Gill Sans MT", size = 5) +
  annotate("text", x = as.Date("2023-03-17"), y = max_dress_level, 
           label = "United\nStates¹", color = "black", angle = -90, vjust = -0.2, family ="Gill Sans MT", size = 5) +
  annotate("text", x = as.Date("2024-10-18"), y = max_dress_level, 
           label = "North \nAmerica¹", color = "black", angle = -90, vjust = -0.2,  family ="Gill Sans MT", size = 5) +
  annotate("text", x = as.Date("2023-08-24"), y = max_dress_level, 
           label = "Latin \nAmerica¹", color = "black", angle = -90, vjust = -0.2,  family ="Gill Sans MT", size = 5) +
  annotate("text", x = as.Date("2024-02-07"), y = max_dress_level, 
           label = "Asia/\nOceania¹", color = "black", angle = -90, vjust = -0.2,  family ="Gill Sans MT", size = 5) +
  annotate("text", x = as.Date("2024-04-16"), y = max_dress_level, 
           label = "TTPD²", color = "darkgray", angle = -90, vjust = -0.5,  family ="Gill Sans MT", size = 5, 
           fontface = "bold") +
  annotate("text", x = as.Date("2023-07-07"), y = max_dress_level, 
           label = "Speak\nNow TV²", color = "purple", angle = -90, vjust = -0.2,  family ="Gill Sans MT", size = 5) +
  annotate("text", x = as.Date("2023-10-27"), y = max_dress_level, 
           label = "1989\nTV²", color = "blue", angle = -90, vjust = -0.2,  family ="Gill Sans MT", size = 5) +
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14, hjust = 0),  # Set hjust=0 for left alignment
        plot.title = element_text(hjust=0.5, size = 14, margin = margin(b = 20), face = "bold"),
        plot.margin = margin(t = -7, r = 0, b = 10, l = 0),
        text = element_text(color = "black", family = "Gill Sans MT", size = 14)) 

# Create dress count plot (to go on the right side)
# Ensure the dress order matches exactly with the main plot
dress_levels <- levels(factor(oneRowPerConcert$DressName))
oneRowPerConcertWithImages$DressName <- factor(oneRowPerConcertWithImages$DressName, levels = dress_levels)

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
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.position = "none",
    plot.margin = margin(t = -7, r = 0, b = 10, l = 0),
    text = element_text(color = "black", family = "Gill Sans MT", size = 14)
  ) +
  xlim(0, 50)   # Adjust limit to accommodate text and images

  
# Merge the plots using cowplot
merged_plot <- plot_grid(
  count_plot, main_plot,
  ncol = 2,
  align = "h",
  axis = "tb",
  rel_widths = c(1.5, 3)  # Adjust relative widths as needed
)

# Create title and subtitle with ggdraw
title_with_subtitle <- ggdraw() + 
  draw_label(
    "She Was Screaming Color",
    fontface = "bold",
    size = 20,
    #x = 0.5,
    y = 0.55,
    hjust = 0.5,
    fontfamily = "Gill Sans MT" 
  ) +
  draw_label(
    "Frequency and Timeline of Taylor Swift's Dress Colors Across Tour Legs¹ and Album Releases²",
    fontface = "plain",
    size = 16,
  #  x = 0.5,
    y = 0.1,
    hjust = 0.5,
    fontfamily = "Gill Sans MT" 
  )

# Then in the final plot assembly, replace the title component:
final_plot <- plot_grid(
  title_with_subtitle, merged_plot,
  ncol = 1,
  rel_heights = c(0.2, 2)  # Slightly increased height for title+subtitle
)

final_plot















# What is the sentiment of each song based on bing dictionary scoring of LYRICS  -----------------------------------------

colnames(allSongsMetadata)
unique(allSongsMetadata$sentiment_MK)
unique(allSongsMetadata$colour_MK)

## Tidytext vignette example

createTokenizedLyricsF <- function(songData) {
  # First create a unique identifier for each song
  songsWithIdF <- songData %>%
    mutate(songId = row_number()) %>%
    select(songId, everything())
  
  # Now tokenize the lyrics
  tokenizedLyricsF <- songsWithIdF %>%
    # Split lyrics into individual words
    unnest_tokens(
      output = word,      # Name of the new column containing individual words
      input = lyrics,     # Column containing the lyrics
      token = "words",    # Split by words (could also use other options like characters, sentences)
      drop = FALSE        # Keep the original lyrics column
    ) %>%
    # Remove empty strings and NA values
    filter(!is.na(word), word != "") %>%
    # Remove numbers
    filter(!str_detect(word, "^[0-9]+$"))
  
  return(tokenizedLyricsF)
}

# Apply the function to my data
tokenizedSongs <- createTokenizedLyricsF(allSongsMetadata)
# 126,416 words in total

# Preview the results
head(tokenizedSongs, 10)

# Get some basic statistics
tokenStats <- tokenizedSongs %>%
  summarise(
    totalWords = n(),
    uniqueWords = n_distinct(word)
  )

print(tokenStats)

colnames(tokenizedSongs)
head(tokenizedSongs$word)
unique(tokenizedSongs$word)
head(tokenizedSongs)

tokenizedSongs <- tokenizedSongs %>%
  anti_join(get_stopwords())
# 79,645 words now, which represents
79645/126416
# the removal of 37% of words! :-)

# what are the top words
tokenizedSongs %>%
  count(word, sort = TRUE)


#hummm there's an issue: the lyrics include the parts of the
# song and the singer's name as well.


wordsToRemove <- c(
  "taylor", "swift", "chorus", "verse", "bridge", "pre", "outro", "intro", 
  "instrumental", "break", "post", "repeat", "interlude", "hook", "refrain",
  "spoken", "voice", "vocals", "feat", "featuring", "produced", "written",
  "background", "harmonies", "recorded", "mixed", "engineered", "oh"
)

cleanTokensF <- tokenizedSongs %>%
  filter(!word %in% wordsToRemove) %>%
  filter(!str_detect(word, "\\d+")) %>%
  filter(str_length(word) > 1)


#check again
tokenizedSongs %>%
  count(word, sort = TRUE)

cleanTokensF %>%
  count(word, sort = TRUE)

tokenizedSongs <- cleanTokensF

## using bing dictionary to get a first look at the sentiments:

get_sentiments("bing")

# Create the sentiment analysis with correct join syntax
calculateSentimentsF <- function(tokenizedData) {
  sentimentResults <- tokenizedData %>%
    inner_join(get_sentiments("bing"), by = c("word" = "word"), 
               relationship = "many-to-many")
  
  return(sentimentResults)
}

# Apply the function
taylorLyrics <- calculateSentimentsF(tokenizedSongs)



# Convert sentiment categories to numeric values (+1/-1)
taylorLyrics <- taylorLyrics %>%
  mutate(sentiment_score = case_when(
    sentiment == "positive" ~ 1,
    sentiment == "negative" ~ -1,
    TRUE ~ 0  # Just in case there are any other categories
  ))

# Then, order the songs by album and by track number:
albumOrder <- c("Taylor Swift", "Fearless (Taylor's Version)", 
                "Speak Now (Taylor's Version)", "Red (Taylor's Version)",
                "1989 (Taylor's Version)", "Reputation", "Lover",
                "folklore", "evermore", "Midnights",
                "THE TORTURED POETS DEPARTMENT")

taylorLyrics <- taylorLyrics %>%
  mutate(album_name = factor(album_name, levels = albumOrder))
taylorLyrics <- taylorLyrics %>%
  filter(!is.na(album_name)) 

# Now aggregate by song to get net sentiment score
song_sentiment_scores_lyrics <- taylorLyrics %>%
  group_by(album_name, track_number, track_name) %>%
  summarize(
    net_sentiment = sum(sentiment_score),
    total_sentiment_words = n(),
    # You could also calculate normalized sentiment if you want
    normalized_sentiment = sum(sentiment_score) / n(),
    .groups = "drop"
  )


song_sentiment_scores_lyrics <- song_sentiment_scores_lyrics %>%
  mutate(album_name = factor(album_name, levels = albumOrder))

# Then plot the net sentiment scores
ggplot(song_sentiment_scores_lyrics, aes(x = track_number, y = net_sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = colorPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "The emotional spectrum across Taylor Swift's discography",
    x = "Track Number",
    y = "Net Sentiment (Positive - Negative Words)"
  ) +
  theme(plot.title = element_text(hjust=0.5))

# Making it interactive:
# Create the ggplot object first
p <- ggplot(song_sentiment_scores_lyrics, aes(x = track_number, y = net_sentiment, 
                                       fill = album_name,
                                       text = paste("Song:", track_name,
                                                    "<br>Net sentiment:", net_sentiment))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = colorPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "The emotional spectrum across Taylor Swift's discography",
    x = "Track Number",
    y = "Net Sentiment (Positive - Negative Words)"
  ) +
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none")

# Convert to an interactive plotly object
interactive_plot_lyrics <- ggplotly(p, tooltip = "text")

# Display the interactive plot
interactive_plot_lyrics

# Preview the results
head(taylorLyrics)

# Get a quick summary of sentiments
sentimentSummary <- taylorLyrics %>%
  count(sentiment) %>%
  spread(sentiment, n)

print(sentimentSummary)

taylorLyrics$track_number <- as.factor(taylorLyrics$track_number)


# Or, without the -1 and +1 scores
ggplot(taylorLyrics, aes(track_number, sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
    facet_wrap(~album_name, ncol = 3, scales = "free_x") +
    scale_fill_manual(values = colorPaletteAlbums) +
    theme_minimal() +
    labs(
      title = "How Positive is Each Song? A Lyrics Analysis",
      x = "Track Number",
      y = "Sentiment Score"
    ) +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y = element_blank())




# What is the sentiment of each song based on bing dictionary scoring of MY IMPRESSIONS ---------------

## Count the mininum amount of keywords since they might be biased
# Count keywords for each row
keyword_counts <- allSongsMetadata %>%
  mutate(keyword_count = str_count(keywords_MK, ",") + 1) %>%
  pull(keyword_count)

# Find the minimum number of keywords
min_keywords <- min(keyword_counts)

# Set seed for reproducibility
set.seed(123)

# Randomly sample the minimum number of keywords for each row
processMetadataColumnsF <- function(data) {
  data_with_sampled_keywords <- data %>%
    rowwise() %>%
    mutate(
      sampled_keywords = list(
        sample(
          str_split(keywords_MK, ",")[[1]], 
          min(length(str_split(keywords_MK, ",")[[1]]), min_keywords)
        )
      ),
      keywords_MK = paste(sampled_keywords, collapse = ", ")
    ) %>%
    ungroup()
  
  # Rest of my existing function remains the same
  combinedMetadata <- data_with_sampled_keywords %>%
    mutate(
      combined_metadata = paste(
        coalesce(sentiment_MK, ""),
        coalesce(message_MK, ""),
        coalesce(keywords_MK, ""),
        sep = ", "
      )
    ) %>%
    mutate(
      combined_metadata = str_replace_all(combined_metadata, ",\\s*,", ","),
      combined_metadata = str_trim(combined_metadata),
      combined_metadata = str_remove_all(combined_metadata, "^,\\s*|\\s*,$")
    )
  
  tokenizedMetadata <- combinedMetadata %>%
    unnest_tokens(
      output = metadata_word,
      input = combined_metadata,
      token = "regex",
      pattern = ",\\s*"  
    ) %>%
    mutate(
      metadata_word = str_trim(metadata_word),
      metadata_word = str_to_lower(metadata_word)
    ) %>%
    filter(metadata_word != "")
  
  return(tokenizedMetadata)
}


# Apply the function
processedMetadata <- processMetadataColumnsF(allSongsMetadata)
unique(processedMetadata$metadata_word)
colnames(processedMetadata)


# Preview the results
head(processedMetadata, 10)

# Get some summary statistics
processedMetadata %>%
  summarise(
    total_metadata_words = n(),
    unique_metadata_words = n_distinct(metadata_word)
  )

dim(processedMetadata) #1406 rows


# what are the top words
processedMetadata %>%
  count(metadata_word, sort = TRUE)



# Create the sentiment analysis with correct join syntax
calculateSentimentsMetadataF <- function(processedMetadata) {
  sentimentResults <- processedMetadata %>%
    inner_join(get_sentiments("bing"), by = c("metadata_word" = "word"), 
               relationship = "many-to-many")
  
  return(sentimentResults)
}

# Apply the function
taylorMetadata <- calculateSentimentsMetadataF(processedMetadata)
colnames(taylorMetadata)
unique(taylorMetadata$sentiment)


# Convert sentiment categories to numeric values (+1/-1)
taylorMetadata <- taylorMetadata %>%
  mutate(sentiment_score = case_when(
    sentiment == "positive" ~ 1,
    sentiment == "negative" ~ -1,
    TRUE ~ 0  # Just in case there are any other categories
  ))

# Then, order the songs by album and by track number:
albumOrder <- c("Taylor Swift", "Fearless (Taylor's Version)", 
                "Speak Now (Taylor's Version)", "Red (Taylor's Version)",
                "1989 (Taylor's Version)", "Reputation", "Lover",
                "folklore", "evermore", "Midnights",
                "THE TORTURED POETS DEPARTMENT")

taylorMetadata <- taylorMetadata %>%
  mutate(album_name = factor(album_name, levels = albumOrder))
taylorMetadata <- taylorMetadata %>%
  filter(!is.na(album_name)) 

# Now aggregate by song to get net sentiment score
song_sentiment_scores_metadata <- taylorMetadata %>%
  group_by(album_name, track_number, track_name) %>%
  summarize(
    net_sentiment = sum(sentiment_score),
    total_sentiment_words = n(),
    normalized_sentiment = sum(sentiment_score) / n(),
    .groups = "drop"
  )





# Preview the results
head(taylorMetadata)

# Get a quick summary of sentiments
sentimentSummary <- taylorMetadata %>%
  count(sentiment) %>%
  spread(sentiment, n)

print(sentimentSummary)

taylorMetadata$track_number <- as.factor(taylorMetadata$track_number)

## Also order the albums in chronological order

taylorMetadata <- taylorMetadata %>%
  mutate(album_name = factor(album_name, levels = albumOrder))

head(taylorMetadata)
taylorMetadata<- taylorMetadata %>%
  filter(!is.na(album_name)) 

# Then plot the net sentiment scores
ggplot(song_sentiment_scores_metadata, aes(x = track_number, y = net_sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = colorPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "The emotional spectrum across Taylor Swift's discography, fanbase lore",
    x = "Track Number",
    y = "Net Sentiment (Positive - Negative Words)"
  ) +
  theme(plot.title = element_text(hjust=0.5))

# Making it interactive:
# Create the ggplot object first
p <- ggplot(song_sentiment_scores_metadata, aes(x = track_number, y = net_sentiment, 
                                       fill = album_name,
                                       text = paste("Song:", track_name,
                                                    "<br>Net sentiment:", net_sentiment))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = colorPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "The emotional spectrum across Taylor Swift's discography, fanbase lore",
    x = "Track Number",
    y = "Net Sentiment (Positive - Negative Words)"
  ) +
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "none")

# Convert to an interactive plotly object
interactive_plot_metadata <- ggplotly(p, tooltip = "text")

interactive_plot_metadata
interactive_plot_lyrics








# Or, without the -1 and +1 scores
ggplot(taylorMetadata, aes(track_number, sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free_x") +
  scale_fill_manual(values = colorPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "How Positive is Each Song? A Fanbase Lore Metadata Analysis",
    x = "Track Number",
    y = "Sentiment Score"
  ) +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y = element_blank())

unique(taylorLyrics$sentiment)





## Trying to see the contribution of each word to the sentiment

wordContributionsLyrics <- taylorLyrics %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 20) %>%
  mutate(word = reorder_within(word, n, sentiment)) %>%
  ungroup()

wordContributionsLyrics %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 4) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_y_reordered() +  # Required for reorder_within to work
  labs(
    title = "Swiftly Feeling: The 40 Most Sentiment-Charged Words in Taylor’s Lyrics",
    x = "",
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("negative" = "#D65D5D", "positive" = "#7CAE7A")) +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size=12),
        strip.text = element_text(size = 12))

#todo
## whoa is a positive word?? lol maybe I'll remove it from the analysis


## Same, but for my metadata 

wordContributionsMetadata <- taylorMetadata %>%
  count(metadata_word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 20) %>%
  mutate(metadata_word = reorder_within(metadata_word, n, sentiment)) %>%
  ungroup()


wordContributionsMetadata %>%
  ggplot(aes(n, metadata_word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), hjust = -0.2, size = 4) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_y_reordered() +  # Required for reorder_within to work
  labs(
    title = "Swift’s Emotional Lexicon: 40 Words That Shape Her Fanbase’s Feelings About Her Songs",
    x = "",
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("negative" = "#D65D5D", "positive" = "#7CAE7A")) +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size=12),
        strip.text = element_text(size = 12))



#todo:
# things that might need correction: crush is shown as a negative word,
# obsession is positive, envious is positive.

wordContributionsMetadata %>%
  arrange(sentiment, desc(n)) %>%
  print()





# Correlation plot: Lyrics x Lore -----------------------------------------

# Aggregate sentiment scores for each song

taylorMetadata <- taylorMetadata %>%
  mutate(sentimentNumber = if_else(sentiment == "positive", 1, -1))

metadataSentimentScores <- taylorMetadata %>%
  group_by(track_number, album_name, track_name) %>%
  summarize(metadata_sentiment_score = mean(sentimentNumber, na.rm = TRUE))

taylorLyrics <- taylorLyrics %>%
  mutate(sentimentNumber = if_else(sentiment == "positive", 1, -1))

lyricsSentimentScores <- taylorLyrics %>%
  group_by(track_number, album_name, track_name) %>%
  summarize(lyrics_sentiment_score = mean(sentimentNumber, na.rm = TRUE))

# Combine the aggregated scores into one dataset
combinedSentiments <- metadataSentimentScores %>%
  inner_join(lyricsSentimentScores, by = c("track_number", "album_name", "track_name"))

# Calculate the absolute difference between metadata and lyrics sentiment scores
combinedSentiments <- combinedSentiments %>%
  mutate(difference = abs(metadata_sentiment_score - lyrics_sentiment_score))

# Correlation analysis
correlation <- cor(combinedSentiments$metadata_sentiment_score, combinedSentiments$lyrics_sentiment_score, use = "complete.obs")

# Identify outliers (e.g., songs with a difference > threshold, say 1.5)
outliers <- combinedSentiments %>%
  filter(difference > 0.5) 

# Print results
print(paste("Correlation between Metadata and Lyrics Sentiment Scores:", round(correlation, 2)))
print("Outliers:")
print(outliers)

# Create correlation plot
ggplot(combinedSentiments, aes(x = metadata_sentiment_score, y = lyrics_sentiment_score)) +
   geom_point(aes(color = album_name), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dotted") +
  geom_text_repel(
    data = outliers,
    aes(label = track_name),
    size = 3,
    max.overlaps = 15
  ) +
#  scale_x_continuous(limits = c(0, 1)) +
 # scale_y_continuous(limits = c(0, 1)) +
   labs(
    title = "Correlation between Lyrics and Lore Sentiments",
    subtitle = paste("Correlation coefficient:", round(correlation, 2)),
    x = "Lore Sentiment Score",
    y = "Lyrics Sentiment Score",
    color = "Album"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


# Create scatter plot
ggplot(combinedSentiments, aes(x = metadata_sentiment_score, y = lyrics_sentiment_score)) +
  geom_smooth(method = "lm", color = "steelblue", se = FALSE) +
  geom_point(color = "steelblue4", size = 2) +
  geom_text(
    data = combinedSentiments %>% 
      filter(difference > 0.5),  
    aes(label = track_name),
    hjust = -0.1,
    vjust = 0.1,
    size = 3
  ) +
  #scale_x_continuous(
  #  limits = c(0, 1),
  #  breaks = seq(0, 1, 0.2)
  #) +
  #scale_y_continuous(
  #  limits = c(0, 1),
  #  breaks = seq(0, 1, 0.2)
  #) +
  labs(
    x = "Lore",
    y = "Lyrics",
    title = "Sentiment Comparison: Lyrics vs Lore"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

## Or, better: 
ggplot(combinedSentiments, 
       aes(x = factor(track_name, levels = track_name), 
           y = factor(track_name, levels = rev(track_name)))) +
  geom_point(aes(size = difference, color = album_name), alpha = 0.7) +
  geom_abline(color = "gray80", linetype = "dashed") +
  facet_wrap(~album_name, scales = "free") +
  scale_color_manual(values = colorPaletteAlbums) +
  scale_size_continuous(range = c(2, 6)) +
  labs(
    x = "Lore Analysis",
    y = "Lyrics Analysis",
    title = "Song Sentiment Comparison by Album",
    subtitle = "Point size indicates magnitude of difference between analyses",
    size = "Sentiment\nDifference",
    color = "Album"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
    axis.text.y = element_text(size = 6),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    panel.grid = element_blank(),
    legend.position = "right",
    strip.text = element_text(size = 8, face = "bold"),
    strip.background = element_rect(fill = "white", color = "gray90"),
    panel.spacing = unit(1, "lines")
  )



# Separating them into different plots:

# Function to create plot for a single album with difference highlighting
create_album_plot_with_diff <- function(data, album_name, color) {
  album_data <- data %>%
    filter(album_name == !!album_name)
  
  ggplot(album_data, 
         aes(x = factor(track_name, levels = track_name), 
             y = factor(track_name, levels = rev(track_name)))) +
    geom_point(aes(size = difference), color = color, alpha = 0.7) +
    geom_abline(color = "gray80", linetype = "dashed") +
    scale_size_continuous(range = c(2, 6)) +
    labs(
      x = "Lore Analysis",
      y = "Lyrics Analysis",
      title = paste("Sentiment Comparison:", album_name),
      size = "Sentiment\nDifference"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      panel.grid = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      legend.position = "right"
    )
}

# Create a list of all plots with differences
album_plots_diff <- list()
for (album in unique(combinedSentiments$album_name)) {
  album_plots_diff[[album]] <- create_album_plot_with_diff(
    combinedSentiments, 
    album, 
    colorPaletteAlbums[album]
  )
}

# Print individual plots
for (album in names(album_plots_diff)) {
  print(album_plots_diff[[album]])
}


## Or, what about I print them as a list?

# Create the plot
ggplot(combinedSentiments, 
       aes(y = reorder(track_name, difference), 
           x = difference,
           fill = album_name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", difference)), 
            hjust = -0.2,
            size = 3.5) +
  scale_fill_manual(values = colorPaletteAlbums) +
  labs(
    title = "Songs with Largest Sentiment Differences",
    subtitle = "Between Lyrics and Lore Analysis",
    x = "Absolute Difference in Sentiment Score",
    y = NULL,
    fill = "Album"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

# Alternative version showing both scores
ggplot(combinedSentiments) +
  geom_segment(aes(x = lyrics_sentiment_score, 
                   xend = metadata_sentiment_score,
                   y = reorder(track_name, difference), 
                   yend = reorder(track_name, difference),
                   color = album_name),
               size = 1.5) +
  geom_point(aes(x = lyrics_sentiment_score, 
                 y = reorder(track_name, difference),
                 color = album_name),
             size = 3) +
  geom_point(aes(x = metadata_sentiment_score, 
                 y = reorder(track_name, difference),
                 color = album_name),
             size = 3) +
  scale_color_manual(values = colorPaletteAlbums) +
  labs(
    title = "Songs with Largest Sentiment Differences",
    subtitle = "Comparing Lyrics and Lore Analysis Scores",
    x = "Sentiment Score",
    y = NULL,
    color = "Album"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(x = lyrics_sentiment_score, 
                y = reorder(track_name, difference),
                label = sprintf("%.2f", lyrics_sentiment_score)),
            hjust = 1.5,
            size = 3) +
  geom_text(aes(x = metadata_sentiment_score, 
                y = reorder(track_name, difference),
                label = sprintf("%.2f", metadata_sentiment_score)),
            hjust = -0.5,
            size = 3)





##
# What are the top 10?
top_10_differences <- combinedSentiments %>%
  arrange(desc(difference)) %>%
  head(10)  

print(paste("Number of rows:", nrow(top_10_differences)))

# Create the dot-and-line plot with verified data
ggplot(top_10_differences, 
       aes(y = reorder(track_name, difference), 
           x = difference,
           fill = album_name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", difference)), 
            hjust = -0.2,
            size = 3.5) +
  scale_fill_manual(values = colorPaletteAlbums) +
   labs(
    title = "Top 10 Songs with Largest Sentiment Differences",
    subtitle = "Between Lyrics and Lore Analysis",
    x = "Absolute Difference in Sentiment Score",
    y = NULL,
    fill = "Album"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))




# Alternative version showing both scores
ggplot(top_10_differences) +
  geom_segment(aes(x = lyrics_sentiment_score, 
                   xend = metadata_sentiment_score,
                   y = reorder(track_name, difference), 
                   yend = reorder(track_name, difference),
                   color = album_name),
               size = 1.5) +
  geom_point(aes(x = lyrics_sentiment_score, 
                 y = reorder(track_name, difference),
                 color = album_name),
             size = 3) +
  geom_point(aes(x = metadata_sentiment_score, 
                 y = reorder(track_name, difference),
                 color = album_name),
             size = 3) +
  scale_color_manual(values = colorPaletteAlbums) +
  labs(
    title = "Top 10 Songs with Largest Sentiment Differences",
    subtitle = "Comparing Lyrics and Lore Analysis Scores",
    x = "Sentiment Score",
    y = NULL,
    color = "Album"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.major.y = element_blank()
  ) +
  geom_text(aes(x = lyrics_sentiment_score, 
                y = reorder(track_name, difference),
                label = sprintf("%.2f", lyrics_sentiment_score)),
            hjust = 1.5,
            size = 3) +
  geom_text(aes(x = metadata_sentiment_score, 
                y = reorder(track_name, difference),
                label = sprintf("%.2f", metadata_sentiment_score)),
            hjust = -0.5,
            size = 3)




# Differing sentiments, barplots ------------------------------------------

# First, get the top 15 songs
top_15_differences <- combinedSentiments %>%
  arrange(desc(difference)) %>%
  head(15) %>%
  mutate(
    clean_name = str_remove_all(track_name, "\\(.*?\\)") %>%  # Remove (text)
      str_remove_all("\\[.*?\\]") %>%               # Remove [text]
      str_trim()                                    # Remove extra spaces
  ) %>%
  mutate(album_name = factor(album_name, levels = albumOrder))

# Reshape the data for side-by-side bars
top_15_long <- top_15_differences %>%
  pivot_longer(
    cols = c(lyrics_sentiment_score, metadata_sentiment_score),
    names_to = "analysis_type",
    values_to = "score"
  ) %>%
  mutate(
    analysis_type = case_when(
      analysis_type == "lyrics_sentiment_score" ~ "Lyrics",
      analysis_type == "metadata_sentiment_score" ~ "Lore"
    )
  ) %>%
  mutate(song_group = as.numeric(factor(clean_name))) ## this is to make the spacing better!


ggplot(top_15_long, 
       aes(x = reorder(clean_name, as.numeric(album_name)), 
           y = score, 
           fill = album_name,
           group = analysis_type)) +
  geom_vline(xintercept = seq(1.5, 14.5, 1), 
             color = "gray", 
             size = 0.5) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.7),  
           width = 0.5) +  
  geom_text(aes(label = sprintf("%.2f", score)),
            position = position_dodge(width = 0.5),
            vjust = -0.5,
            size = 3) +
  geom_text(aes(label = analysis_type,
                y = -0.05),
            position = position_dodge(width = 0.9),
            size = 3) +
  scale_fill_manual(values = colorPaletteAlbums) +
  labs(
    title = "Folklore vs Facts: 15 Most Contrasting Taylor Swift Songs",
    subtitle = "Where Fan-interpreted Meanings Most Differ from Lyrical Content",
    x = NULL,
    y = "Sentiment Score",
    fill = "Album"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) 
  #scale_y_continuous(limits = c(-0.1, 1.1), 
   #                  expand = expansion(mult = c(0.02, 0.1)))


# What about the scores that are the most similar? ------------------------

# First, get the bottom 15 songs
bottom_15_differences <- combinedSentiments %>%
  arrange(desc(difference)) %>%
  tail(15) %>%
  mutate(
    clean_name = str_remove_all(track_name, "\\(.*?\\)") %>%  # Remove (text)
      str_remove_all("\\[.*?\\]") %>%               # Remove [text]
      str_trim()                                    # Remove extra spaces
  ) %>%
  mutate(album_name = factor(album_name, levels = albumOrder))

# Reshape the data for side-by-side bars
bottom_15_long <- bottom_15_differences %>%
  pivot_longer(
    cols = c(lyrics_sentiment_score, metadata_sentiment_score),
    names_to = "analysis_type",
    values_to = "score"
  ) %>%
  mutate(
    analysis_type = case_when(
      analysis_type == "lyrics_sentiment_score" ~ "Lyrics",
      analysis_type == "metadata_sentiment_score" ~ "Lore"
    )
  ) %>%
  mutate(song_group = as.numeric(factor(clean_name))) ## this is to make the spacing better!


ggplot(bottom_15_long, 
       aes(x = reorder(clean_name, as.numeric(album_name)), 
           y = score, 
           fill = album_name,
           group = analysis_type)) +
  geom_vline(xintercept = seq(1.5, 14.5, 1), 
             color = "gray", 
             size = 0.5) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.7), 
           width = 0.5) + 
  geom_text(aes(label = sprintf("%.2f", score)),
            position = position_dodge(width = 0.5),
            vjust = -0.5,
            size = 3) +
  geom_text(aes(label = analysis_type,
                y = -0.05),
            position = position_dodge(width = 0.9),
            size = 3) +
  scale_fill_manual(values = colorPaletteAlbums) +
  labs(
    title = "Folklore vs Facts: 15 Least Contrasting Taylor Swift Songs",
    subtitle = "Where Fan-interpreted Mostly Matched the Lyrical Content",
    x = NULL,
    y = "Sentiment Score",
    fill = "Album"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(limits = c(-0.1, 1.1), 
                     expand = expansion(mult = c(0.02, 0.1)))





# For the paper: basic numbers --------------------------------------------

# First, let's gather all song appearances across the three columns
all_songs <- bind_rows(
  surpriseSongsDressColours[,"Song title", drop = FALSE] %>%
    rename(song = "Song title"),
  
  surpriseSongsDressColours[,"Mashup", drop = FALSE] %>%
    filter(!is.na(Mashup)) %>%
    rename(song = Mashup),
  
  surpriseSongsDressColours[,"Mashup2", drop = FALSE] %>%
    filter(!is.na(Mashup2)) %>%
    rename(song = Mashup2)
)

# Get number of unique songs performed more than once
repeated_songs <- all_songs %>%
  count(song) %>%
  filter(n > 1)

# Get top songs including mashups
top_songs <- all_songs %>%
  count(song, sort = TRUE) %>%
  head(3)

# Count guests who performed multiple times
repeat_guests <- surpriseSongsDressColours %>%
  filter(!is.na(Guest)) %>%
  count(Guest) %>%
  filter(n > 1)

# Print results
cat("Total song performances:", nrow(all_songs), "\n")
cat("Number of songs performed more than once:", nrow(repeated_songs), "\n\n")

cat("Top 3 most performed songs:\n")
print(top_songs)

cat("\nNumber of repeat guests:", nrow(repeat_guests), "\n")
cat("Repeat guests and their appearances:\n")
print(repeat_guests)

# To get unique songs per concert
songs_per_concert <- surpriseSongsDressColours %>%
  group_by(Date, City) %>%
  summarise(
    songs = n_distinct(c(`Song title`, Mashup, Mashup2), na.rm = TRUE)
  ) %>%
  ungroup()

cat("\nAverage songs per concert:", mean(songs_per_concert$songs))



# What do colours mean for Taylor/the fans? -------------------------------

# I edited the dataset to include every mention of colour, both the lyrics that
# mention them and the colour itself that's mentioned.
# these are in:


allSongsMetadata$colour_lyric_MK
unique(allSongsMetadata$colour_MK)

## but I reckon the easiest way to decide if a colour is happy or sad is to manually
# look at the spreadsheet on excel and add the info, so I'll do just that.

allSongsMetadata$colour_meaningMK

#Now, I need to split the colours and their meanings into separate lines, since
# some songs have more than one colour in them:

# Expand the dataset to have one row per color-sentiment pair
expandedData <- allSongsMetadata %>%
  separate_rows(colour_MK, colour_meaningMK, sep = ";") %>%
  mutate(across(everything(), ~ trimws(.)))  # Remove extra spaces if any


#If there's an error, use this code to check and fix it on excel (easiest way)
#print(allSongsMetadata[35, c("colour_MK", "colour_meaningMK")])

# Summarize the sentiments associated with each color
colorSentimentSummary <- expandedData %>%
  group_by(colour_MK) %>%
  summarize(
    positiveCount = sum(colour_meaningMK == "positive"),
    negativeCount = sum(colour_meaningMK == "negative"),
    predominantSentiment = case_when(
      positiveCount > negativeCount ~ "positive",
      negativeCount > positiveCount ~ "negative",
      TRUE ~ "neutral"
    ),
    .groups = "drop"
  )

# View the result
print(colorSentimentSummary)


plotDataColors <- allSongsMetadata %>%
  separate_rows(colour_MK, sep = ";") %>%
  mutate(across(everything(), ~ trimws(.))) %>%  
  filter(!is.na(colour_MK)) %>% 
  group_by(colour_MK) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(
    across(everything(), ~ trimws(.)), 
    colour_group = colorGroups[colour_MK] 
  )

plotDataColors <- plotDataColors %>% mutate(count = as.numeric(count))


# Create the plot
ggplot(plotDataColors, aes(x = colour_MK, y = count, fill = colour_MK)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colorPaletteColours) + 
  theme_minimal() +
  labs(
    title = "Colours in Taylor Swift's Writing",
    x = "Colours",
    y = "Count",
    fill = "Colours"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) 

rainbowOrder <- c("reds", "yellows", "greens", "blues", "purples", "colorful", "whites", "black and white", "blacks")


# Reorder the colour_group based on the rainbow order
plotDataColors$colour_group <- factor(plotDataColors$colour_group, levels = rainbowOrder)


plotDataColors <- plotDataColors %>% 
  mutate(colour_MK = fct_reorder(colour_MK, count, .desc = TRUE))


# Create the plot
ggplot(plotDataColors, aes(x = colour_MK, y = count, fill = colour_MK)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colorPaletteColours) +  
  facet_wrap(~ colour_group, scales = "free_x", ncol = 3) +  
  theme_minimal() +
  labs(
    title = "Every Shade of Swift",
    subtitle = "Frequency of colors mentioned in Taylor Swift's discography",
    x = "",
    y = "Count",
    fill = "Colours"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size=12, hjust=0.5),
    legend.position = "none"
  ) +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  ylim(0,25)


# Color dictionary --------------------------------------------------------

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
    caption = paste("Analysis based on", nrow(rawColorData), "color mentions across 241 songs released to date")
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


# What are the stats about it? --------------------------------------------


# First, let's check for duplicates in our color groups
colorGroupsDf <- data.frame(
  colourName = names(colorGroups),
  groupName = colorGroups
)

# Check for duplicate color names
duplicateColors <- colorGroupsDf %>%
  count(colourName) %>%
  filter(n > 1)

print("Colors appearing in multiple groups:")
print(duplicateColors)

# Create scores for the sentiment analysis with all data intact
colorSentimentStats <- rawColorData %>%
  mutate(
    meaning = trimws(meaning),
    score = case_when(
      meaning == "positive" ~ 1,
      meaning == "neutral" ~ 0,
      meaning == "negative" ~ -1,
      TRUE ~ NA_real_
    )
  ) %>%
  # Then join with color groups
  left_join(colorGroupsDf, 
            by = c("colour" = "colourName"),
            relationship = "many-to-many") %>%
  filter(!is.na(groupName))

# Now we can perform the statistical tests
# ANOVA
sentimentAnova <- aov(score ~ groupName, data = colorSentimentStats)
sentimentAnovaSummary <- summary(sentimentAnova)

# Pairwise tests
sentimentPairwise <- pairwise.t.test(
  colorSentimentStats$score,
  colorSentimentStats$groupName,
  p.adjust.method = "bonferroni"
)

# Group statistics
sentimentGroupStats <- colorSentimentStats %>%
  group_by(groupName) %>%
  summarise(
    meanSentiment = mean(score, na.rm = TRUE),
    sdSentiment = sd(score, na.rm = TRUE),
    n = n(),
    se = sdSentiment / sqrt(n),
    ciLower = meanSentiment - qt(0.975, n-1) * se,
    ciUpper = meanSentiment + qt(0.975, n-1) * se
  ) %>%
  arrange(desc(meanSentiment))

# Print results
print("ANOVA Results:")
print(sentimentAnovaSummary)

print("Pairwise Comparisons:")
print(sentimentPairwise)

print("Descriptive Statistics by Group:")
print(sentimentGroupStats)


# Lastly: how do songs' sentiments relate to dress colors --------

# The datasets I want to work with:


colorGroupSentiments #the rating of each color group
taylorMetadata #the rating of each song based on the lore
surpriseSongsDressColours #all the dress colors and songs played in the Eras Tour

#What I need to do:
#Join all the songs into a single column, since there are songs in the Mashup and Mashup2 cols, keep the rest of the cols content;
#Classify the dress colors into a color group;
#Grab the songs played wearing each dress;
#Compare the rating of the dress x rating of the song and find a relationship (if any)

normalize_song_name <- function(name) {
  # Create a function that works on a single name
  normalize_one <- function(one_name) {
    if (is.na(one_name)) return(NA)
    
    # Convert to lowercase
    one_name <- tolower(one_name)
    
    # Remove common Taylor Swift song suffixes
    patterns_to_remove <- c(
      "\\(10 minute version\\)",
      "\\(taylor's version\\)",
      "\\[taylor's version\\]",
      "\\(from the vault\\)",
      "\\[from the vault\\]",
      "\\(acoustic version\\)",
      "\\(live\\)",
      "\\(remix\\)",
      "\\(feat\\. [^\\)]+\\)", # Remove featuring credits
      "featuring [^\\(\\)]+",  # Alternative format for features
      "ft\\. [^\\(\\)]+"       # Another common feature format
    )
    
    # Apply each pattern
    for (pattern in patterns_to_remove) {
      one_name <- gsub(pattern, "", one_name, ignore.case = TRUE)
    }
    
    # Remove punctuation
    one_name <- gsub("[[:punct:]]", "", one_name)
    
    # Remove extra spaces
    one_name <- gsub("\\s+", " ", one_name)
    
    # Trim leading/trailing whitespace
    one_name <- trimws(one_name)
    
    return(one_name)
  }
  
  # Apply the function to each element of the input vector
  return(sapply(name, normalize_one))
}
# Step 1: Create better normalized fields in both datasets
taylorMetadata <- taylorMetadata %>%
  mutate(
    # Simple normalization (keep for backward compatibility)
    normalizedSongName = tolower(gsub("[^[:alnum:]]", "", track_name)),
    # Better normalization
    better_normalized = normalize_song_name(track_name)
  )

# Check normalization in the taylorMetadata dataset
normalization_comparison <- taylorMetadata %>%
  select(track_name, normalizedSongName, better_normalized) %>%
  distinct() %>%
  arrange(track_name)

# View the first 20 rows
head(normalization_comparison, 20)

# Step 2: Apply the same normalization to the surprise songs
surprise_songs_long <- bind_rows(
  # Original songs
  surpriseSongsDressColours %>%
    mutate(
      normalizedSongName = tolower(gsub("[^[:alnum:]]", "", `Song title`)),
      better_normalized = normalize_song_name(`Song title`)
    ) %>%
    select(-Mashup, -Mashup2) %>%
    rename(song = `Song title`),
  
  # First mashup songs
  surpriseSongsDressColours %>%
    filter(!is.na(Mashup)) %>%
    mutate(
      normalizedSongName = tolower(gsub("[^[:alnum:]]", "", Mashup)),
      better_normalized = normalize_song_name(Mashup)
    ) %>%
    select(-`Song title`, -Mashup2) %>%
    rename(song = Mashup),
  
  # Second mashup songs
  surpriseSongsDressColours %>%
    filter(!is.na(Mashup2)) %>%
    mutate(
      normalizedSongName = tolower(gsub("[^[:alnum:]]", "", Mashup2)),
      better_normalized = normalize_song_name(Mashup2)
    ) %>%
    select(-`Song title`, -Mashup) %>%
    rename(song = Mashup2)
)

# Check a sample of the normalized names
surprise_songs_long %>%
  select(song, better_normalized) %>%
  distinct() %>%
  head(20)

# Check for any potential duplicates after normalization
duplicate_check <- surprise_songs_long %>%
  group_by(better_normalized) %>%
  summarize(
    count = n(),
    original_names = paste(unique(song), collapse = " | ")
  ) %>%
  filter(count > 1)

print(duplicate_check)

# Step 3: Try to match using the better normalization
matched_songs <- surprise_songs_long %>%
  left_join(
    taylorMetadata %>% 
      select(track_name, better_normalized, sentiment_score),
    by = "better_normalized",
    relationship = "many-to-many"
  )

# Step 4: For unmatched songs, apply manual mapping
manual_matches <- tibble(
  surprise_song = c(
    "mr perfectly fine", "youre on your own kid", "new years day", 
    "the last great american dinasty", 
    "tell me why", "is it over now", "you are in love",
    "chloe or sam or sophia or markus", "this is what you came for",
    "guilty as sin"
  ),
  metadata_song = c(
    "mr perfectly fine", "youre on your own kid", "new years day", 
    "the last great american dynasty", 
    "tell me why", "is it over now?", "you are in love",
    "Chloe Or Sam Or Sophia Or Marcus", NA, "guilty as sin?"
  )
)

# Apply manual matches
unmatched <- matched_songs %>% 
  filter(is.na(track_name)) %>%
  mutate(better_normalized_original = better_normalized)

for (i in 1:nrow(manual_matches)) {
  if (!is.na(manual_matches$metadata_song[i])) {
    unmatched <- unmatched %>%
      mutate(
        better_normalized = ifelse(
          better_normalized == manual_matches$surprise_song[i],
          normalize_song_name(manual_matches$metadata_song[i]),
          better_normalized
        )
      )
  }
}

# Try matching again with fixed names
manually_matched <- unmatched %>%
  left_join(
    taylorMetadata %>% 
      select(track_name, better_normalized, sentiment_score),
    by = "better_normalized"
  )



# Step 6: Combine all results
all_matched <- bind_rows(
  # Already matched in step 3
  matched_songs %>% filter(!is.na(song)),
  # Matched through manual mapping
  manually_matched %>% filter(!is.na(song)),
  # Matched through fuzzy matching
  fuzzy_matched
)

# Check how many songs were matched
cat("Total songs:", nrow(surprise_songs_long), "\n")
cat("Successfully matched:", nrow(all_matched), "\n")
cat("Unmatched:", nrow(surprise_songs_long) - nrow(all_matched), "\n")

# After all the join steps, count unique songs to check for duplicates
duplicate_matches <- all_matched %>%
  group_by(song, Date, DressName) %>%  # Use fields that should uniquely identify a performance
  summarize(
    match_count = n(),
    matched_to = paste(unique(track_name), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(match_count > 1)

print(duplicate_matches)
# That makes sense: she sang some songs more than once, with more than one
# dress color :-)


# Step 2: Calculate song sentiment scores
song_sentiment_scores <- taylorMetadata %>%
  group_by(normalizedSongName, track_name) %>%
  summarize(
    net_sentiment = sum(sentiment_score),
    total_sentiment_words = n(),
    normalized_sentiment = sum(sentiment_score) / n(),
    .groups = "drop"
  )



# Step 3: Create a long format dataset with all surprise songs and their dress colors
surprise_songs_long <- bind_rows(
  # Original songs
  surpriseSongsDressColours %>%
    mutate(normalizedSongName = tolower(gsub("[^[:alnum:]]", "", `Song title`))) %>%
    select(-Mashup, -Mashup2) %>%
    rename(song = `Song title`),
  
  # First mashup songs
  surpriseSongsDressColours %>%
    filter(!is.na(Mashup)) %>%
    mutate(normalizedSongName = tolower(gsub("[^[:alnum:]]", "", Mashup))) %>%
    select(-`Song title`, -Mashup2) %>%
    rename(song = Mashup),
  
  # Second mashup songs
  surpriseSongsDressColours %>%
    filter(!is.na(Mashup2)) %>%
    mutate(normalizedSongName = tolower(gsub("[^[:alnum:]]", "", Mashup2))) %>%
    select(-`Song title`, -Mashup) %>%
    rename(song = Mashup2)
)

# Step 4: Classify dress colors into color groups if not already done
surprise_songs_long <- surprise_songs_long %>%
  mutate(groupName = case_when(
    DressName %in% c("Pink", "Flamingo pink") ~ "reds",
    DressName %in% c("Green") ~ "greens",
    DressName %in% c("Yellow", "Sunset orange") ~ "yellows",
    DressName %in% c("Ocean blue", "Blue", "Blurple") ~ "blues",
    DressName %in% c("Popsicle", "Cotton candy", "Grapefruit") ~ "colorful",
    TRUE ~ "neutral"
  ))

# Step 5: Join the dress color group sentiments
surprise_songs_with_color_sentiment <- surprise_songs_long %>%
  left_join(colorGroupSentiments, by = "groupName")

# Step 6: Join with song sentiment scores
final_analysis_data <- surprise_songs_with_color_sentiment %>%
  left_join(song_sentiment_scores, by = "normalizedSongName")

# Step 7: Check how many songs were matched successfully
sum(!is.na(final_analysis_data$net_sentiment)) #only 163!
## Songs that didn't get matched:
final_analysis_data %>% filter(is.na(net_sentiment)) #280 didn't get matched!!

noMatch <- final_analysis_data %>% filter(is.na(net_sentiment)) 
unique(noMatch$song)


unique(final_analysis_data$groupSentiment)
unique(final_analysis_data$normalized_sentiment)
unique(final_analysis_data$net_sentiment)

# Step 8: Calculate correlation between dress color sentiment and song sentiment
correlation_result <- cor.test(
  final_analysis_data$groupSentiment,  # Dress color group sentiment
  final_analysis_data$net_sentiment,   # Song sentiment
  use = "complete.obs"                # Handle any missing values
)

print(correlation_result)

#Also, look at normalized sentiment
correlation_result_normalized <- cor.test(
  final_analysis_data$groupSentiment,
  final_analysis_data$normalized_sentiment,
  use = "complete.obs"
)

print(correlation_result_normalized)



# Attempt at making the code above more efficient -------------------------

# Step 1: Define normalize_song_name function
normalize_song_name <- function(name) {
  # Handle NA values
  if (all(is.na(name))) return(name)
  
  result <- name
  non_na <- !is.na(name)
  
  # Apply transformations only to non-NA values
  if (any(non_na)) {
    # Convert to lowercase
    result[non_na] <- tolower(name[non_na])
    
    # Remove common Taylor Swift song suffixes
    patterns_to_remove <- c(
      "\\(10 minute version\\)", "\\(taylor's version\\)", "\\[taylor's version\\]",
      "\\(from the vault\\)", "\\[from the vault\\]", "\\(acoustic version\\)",
      "\\(live\\)", "\\(remix\\)", "\\(feat\\. [^\\)]+\\)",
      "featuring [^\\(\\)]+", "ft\\. [^\\(\\)]+"
    )
    
    for (pattern in patterns_to_remove) {
      result[non_na] <- gsub(pattern, "", result[non_na], ignore.case = TRUE)
    }
    
    # Remove punctuation and clean up whitespace
    result[non_na] <- gsub("[[:punct:]]", "", result[non_na])
    result[non_na] <- gsub("\\s+", " ", result[non_na])
    result[non_na] <- trimws(result[non_na])
  }
  
  return(result)
}

# Step 2: Normalize both datasets in one go
taylorMetadata <- taylorMetadata %>%
  mutate(better_normalized = normalize_song_name(track_name))


# Step 3: Classify dress colors
surpriseSongsDressColours <- surpriseSongsDressColours %>%
  mutate(groupName = case_when(
    DressName %in% c("Pink", "Flamingo pink") ~ "reds",
    DressName %in% c("Green") ~ "greens",
    DressName %in% c("Yellow", "Sunset orange") ~ "yellows",
    DressName %in% c("Ocean blue", "Blue", "Blurple") ~ "blues",
    DressName %in% c("Popsicle", "Cotton candy", "Grapefruit") ~ "colorful",
    TRUE ~ "neutral"
  ))

# Step 5: Calculate song sentiment scores
song_sentiment_scores <- taylorMetadata %>%
  group_by(better_normalized, track_name) %>%
  summarize(
    net_sentiment = sum(sentiment_score, na.rm = TRUE),
    total_sentiment_words = n(),
    normalized_sentiment = mean(sentiment_score, na.rm = TRUE),
    .groups = "drop"
  )

# Step 6: Create manual matches lookup for problematic songs
manual_matches <- tibble(
  surprise_normalized = c(
    "mr perfectly fine", "youre on your own kid", "new years day", 
    "the last great american dinasty", "tell me why", "is it over now"
  ),
  metadata_normalized = c(
    "mr perfectly fine", "youre on your own kid", "new years day", 
    "the last great american dynasty", "tell me why", "is it over now"
  )
)

# Step 7: Create the final analysis dataset
final_analysis_data <- surprise_songs_long %>%
  # Join color sentiments
  left_join(colorGroupSentiments, by = "groupName") %>%
  # Try automatic matching first
  left_join(song_sentiment_scores, by = c("better_normalized")) %>%
  # Apply manual matches for unmatched songs
  rowwise() %>%
  mutate(
    # Find if there's a manual match
    manual_match = ifelse(
      is.na(net_sentiment) && better_normalized %in% manual_matches$surprise_normalized,
      manual_matches$metadata_normalized[match(better_normalized, manual_matches$surprise_normalized)],
      NA_character_
    )
  ) %>%
  # Join the manual matches
  left_join(
    song_sentiment_scores,
    by = c("manual_match" = "better_normalized"),
    suffix = c("", "_manual")
  ) %>%
  # Combine original and manual matches
  mutate(
    track_name = ifelse(is.na(track_name), track_name_manual, track_name),
    net_sentiment = ifelse(is.na(net_sentiment), net_sentiment_manual, net_sentiment),
    normalized_sentiment = ifelse(is.na(normalized_sentiment), normalized_sentiment_manual, normalized_sentiment)
  ) %>%
  select(-ends_with("_manual"), -manual_match) %>%
  ungroup()

# Step 8: Check match statistics
match_stats <- tibble(
  Total_Songs = nrow(surprise_songs_long),
  Matched_Songs = sum(!is.na(final_analysis_data$net_sentiment)),
  Unmatched_Songs = sum(is.na(final_analysis_data$net_sentiment)),
  Match_Percentage = round(sum(!is.na(final_analysis_data$net_sentiment)) / nrow(surprise_songs_long) * 100, 1)
)
print(match_stats)

# Step 9: Calculate correlations
dress_song_corr <- cor.test(
  final_analysis_data$groupSentiment,
  final_analysis_data$net_sentiment,
  use = "complete.obs"
)

dress_song_norm_corr <- cor.test(
  final_analysis_data$groupSentiment,
  final_analysis_data$normalized_sentiment,
  use = "complete.obs"
)

print(dress_song_corr)
print(dress_song_norm_corr)



# Which colours of dresses she wore for each song? ------------------------

# datasets to use:

surpriseSongsDressColours

# Step 1: Find songs performed more than once
song_counts <- surpriseSongsDressColours %>%
  count(`Song title`) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Step 2: Filter the dataset to include only these songs
multiple_performances <- surpriseSongsDressColours %>%
  filter(`Song title` %in% song_counts$`Song title`)

# Step 3: Create a visualization of dress colors for each song
ggplot(multiple_performances, aes(x = reorder(`Song title`, desc(`Song title`)), fill = DressName)) +
  geom_bar() +
  coord_flip() +  # Horizontal bars for better readability
  scale_fill_manual(values = colorPaletteDresses) +  # Assuming you have a color palette defined
  labs(
    title = "Dress Colors for Songs Performed Multiple Times",
    subtitle = paste0("Songs performed at least twice during the Eras Tour (", nrow(song_counts), " songs)"),
    x = "Song",
    y = "Number of Performances",
    fill = "Dress Color"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Calculate the percentage of each dress color for each song
dress_color_proportions <- multiple_performances %>%
  group_by(`Song title`) %>%
  mutate(total_performances = n()) %>%
  group_by(`Song title`, DressName, total_performances) %>%
  summarize(count = n(),
            percentage = count / total_performances * 100,
            .groups = "drop") %>%
  arrange(desc(total_performances), `Song title`)

# Create a visualization showing both count and percentage
ggplot(dress_color_proportions, 
       aes(x = reorder(`Song title`, desc(total_performances)), 
           y = count, 
           fill = DressName)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(percentage), "%")),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  coord_flip() +
  scale_fill_manual(values = colorPaletteDresses) +
  labs(
    title = "Dress Color Distribution for Frequently Performed Songs",
    x = "Song",
    y = "Number of Performances",
    fill = "Dress Color"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5)
  )
 

# Find the top 20 most performed songs
top_performed_songs <- song_counts %>%
  head(20)

# Filter for just these songs
top_performances <- surpriseSongsDressColours %>%
  filter(`Song title` %in% top_performed_songs$`Song title`)

# Create the visualization for just the top songs
ggplot(top_performances, aes(x = reorder(`Song title`, desc(`Song title`)), fill = DressName)) +
  geom_bar() +
  coord_flip() +
  scale_fill_manual(values = colorPaletteDresses) +
  labs(
    title = "Dress Colors for Taylor's Most Frequently Performed Surprise Songs",
    x = "Song",
    y = "Number of Performances",
    fill = "Dress Color"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )


## Show colour group of the dress, not the dress itself:

# Step 1: Calculate song frequencies
song_counts <- surpriseSongsDressColours %>%
  count(`Song title`) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Step 2: Filter for songs performed multiple times
multiple_performances <- surpriseSongsDressColours %>%
  filter(`Song title` %in% song_counts$`Song title`)

# Step 3: Join the frequency count directly
multiple_performances <- multiple_performances %>%
  left_join(song_counts, by = "Song title")

# Step 4: Now create the visualization with songs ordered by play count
ggplot(multiple_performances, aes(x = reorder(`Song title`, n), fill = groupName)) +
  geom_bar() +
  coord_flip() +
  scale_fill_manual(values = groupColors) +
  labs(
    title = "Color Groups for Songs Performed Multiple Times",
    subtitle = paste0("Songs performed at least twice during the Eras Tour (", nrow(song_counts), " songs)"),
    x = "Song",
    y = "Number of Performances",
    fill = "Color Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


# Are there songs that were sang with the same color group? ---------------

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

# Step 3: Create the visualization
ggplot(single_color_performances, aes(x = reorder(`Song title`, table(single_color_performances$`Song title`)[`Song title`]), 
                                      fill = groupName)) +
  geom_bar() +
  coord_flip() +
  scale_fill_manual(values = groupColors) +
  labs(
    title = "Songs Always Performed With the Same Color Group",
    subtitle = paste0(nrow(songs_with_single_color_group), " songs performed multiple times with consistent color groups"),
    x = "Song",
    y = "Number of Performances",
    fill = "Color Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


# Cool! Now, plot them by dress color:

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


# What about the sentiment across the instruments? ------------------------

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



# March 14th, 2025 --------------------------------------------------------


################################################################################
# Trying to use a more nuanced package to qualify the words' sentiments --------


# Load packages -----------------------------------------------------------

library(sentimentr)



# Datasets to be used -----------------------------------------------------


taylor_album_songs
surpriseSongsDressColours  #Edited, all the
# mashups are underneath the main songs, and the names of the songs are changed
allSongsMetadata 



# Getting the sentiment from the lyrics ----------------------------------

head(taylor_album_songs$lyrics) # this is where the information will be extracted from

lyrics_df <- taylor_album_songs %>%
  unnest(lyrics) %>%
  select(album_name, track_name, track_number, line, lyric, element)

head(lyrics_df)


# Using sentimentr now:

sentiment_scores <- sentiment(lyrics_df$lyric)

#lyrics_sentiment <- cbind(lyrics_df, sentiment_scores)
dim(lyrics_df)
dim(sentiment_scores)

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
colnames(lyrics_sentiment)


# Now, plot it like I did before for tidytext -----------------------------

# Now aggregate by song to get net sentiment score
song_sentiment_scores_sentimentr <- lyrics_sentiment %>%
  group_by(track_number, track_name, album_name) %>%
  summarize(
    sum_sentiment = sum(avg_sentiment),
    total_sentiment_words = n(),
    # You could also calculate normalized sentiment if you want
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
  scale_fill_manual(values = colorPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "The emotional spectrum across Taylor Swift's discography",
    x = "Track Number",
    y = "Net Sentiment (Positive - Negative Words)"
  ) +
  theme(plot.title = element_text(hjust=0.5))

# Making it interactive:
# Load the required package
library(plotly)

# Create the ggplot object first
p <- ggplot(lyrics_sentiment, aes(x = track_number, y = avg_sentiment, 
                                       fill = album_name,
                                       text = paste("Song:", track_name,
                                                    "<br>Net sentiment:", avg_sentiment))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free") +
  scale_fill_manual(values = colorPaletteAlbums) +
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



# Compare the results with the other results I had before -----------------

# Bing dictionary with lyrics:
lyrics_sentiment # lyrics, nuanced sentimentr package
song_sentiment_scores_lyrics # lyrics, tidytext, positive = +1, negative = -1
song_sentiment_scores_metadata # fan metadata, tidytext, positive = +1, negative = -1

table(lyrics_sentiment$avg_sentiment)
table(song_sentiment_scores_lyrics$net_sentiment)
table(song_sentiment_scores_metadata$net_sentiment)

lyrics_sentiment$track_number <- as.factor(lyrics_sentiment$track_number)
song_sentiment_scores_lyrics$track_number <- as.factor(song_sentiment_scores_lyrics$track_number)
song_sentiment_scores_metadata$track_number <- as.factor(song_sentiment_scores_metadata$track_number)

str(lyrics_sentiment)
str(song_sentiment_scores_lyrics)
str(song_sentiment_scores_metadata)

sentimentr_by_song <- lyrics_sentiment %>%
  group_by(album_name, track_number, track_name) %>%
  summarise(
    sentimentr_score = mean(avg_sentiment, na.rm = TRUE),
    .groups = "drop"
      )

sentiment_comparison <- song_sentiment_scores_lyrics %>%
  rename(
    tidytext_net_sentiment = net_sentiment,
    tidytext_normalized = normalized_sentiment
  ) %>%
  left_join(
    song_sentiment_scores_metadata %>%
      rename(
        metadata_net_sentiment = net_sentiment,
        metadata_normalized = normalized_sentiment
      ),
    by = c("album_name", "track_number", "track_name")
  ) %>%
  left_join(
    sentimentr_by_song,
    by = c("album_name", "track_number", "track_name")
  )

# Calculate differences between methods
sentiment_comparison <- sentiment_comparison %>%
  mutate(
    # Calculate absolute differences between each pair of methods
    diff_tidytext_metadata = abs(tidytext_net_sentiment - metadata_net_sentiment),
    diff_tidytext_sentimentr = abs(tidytext_net_sentiment - sentimentr_score),
    diff_metadata_sentimentr = abs(metadata_net_sentiment - sentimentr_score),
    
    # Calculate total difference (sum of all pairwise differences)
    total_difference = diff_tidytext_metadata + diff_tidytext_sentimentr + diff_metadata_sentimentr
  ) %>%
  arrange(desc(total_difference))

# Get top 10 songs with largest differences
top_15_diff_songs <- sentiment_comparison %>%
  head(15)


top_15_long <- top_15_diff_songs %>%
  select(album_name, track_name, tidytext_net_sentiment, metadata_net_sentiment, sentimentr_score) %>%
  pivot_longer(
    cols = c(tidytext_net_sentiment, metadata_net_sentiment, sentimentr_score),
    names_to = "method",
    values_to = "sentiment"
  ) %>%
  mutate(
    method = case_when(
      method == "tidytext_net_sentiment" ~ "tidytext + Lyrics",
      method == "metadata_net_sentiment" ~ "tidytext + Lore",
      method == "sentimentr_score" ~ "sentimentR + Lyrics",
      TRUE ~ method
    ),
    # Create song label with album
    song_label = paste0(track_name, " (", album_name, ")")
  )

# Reorder songs by total difference to show largest differences at the top
top_15_long$song_label <- fct_reorder(top_15_long$song_label, 
                                      rep(top_15_diff_songs$total_difference, each=3))

top_15_long <- top_15_long %>%
  mutate(
    clean_track_name = str_remove_all(track_name, "\\(Taylor's Version\\)") %>%
      str_remove_all("\\[From The Vault\\]") %>%
      str_remove_all("\\(Remix\\)") %>%
      str_remove_all("\\[Taylor's Version\\]") %>%
      str_trim()
  )



# Create the visualization
ggplot(top_15_long, aes(x = sentiment, y = clean_track_name, color = method, shape = method)) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0, linetype = 'dashed', alpha = 0.5) +
  scale_color_brewer(palette = 'Set1') +
  labs(
    title = 'Top 10 Songs with Most Divergent Sentiment Analysis Results',
    subtitle = 'Comparing tidytext Lexicon, Fan Metadata, and SentimentR approaches',
    x = 'Sentiment Score (net_sentiment)',
    y = NULL,
    color = 'Analysis Method',
    shape = 'Analysis Method'
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))


## Interesting!! The "lore" and the sentimentr scores are very very similar in the
# songs where tidytext differs the most. 
# What are the songs where the "lore" and sentimentr differ the most?


fan_vs_sentimentr <- song_sentiment_scores_metadata %>%
  select(album_name, track_number, track_name, net_sentiment) %>%
  left_join(
    sentimentr_by_song,
    by = c("album_name", "track_number", "track_name")
  ) %>%
  mutate(
    # Calculate a scaled difference since the scales are different
    # (net_sentiment is -3 to +3, sentimentr is roughly -0.5 to +0.5)
    scaled_diff = abs(net_sentiment - (sentimentr_score * 6)),
    
    # Flag if directions differ (sign mismatch)
    direction_mismatch = sign(net_sentiment) != sign(sentimentr_score),
    
    # Flag neutral cases separately
    is_neutral = net_sentiment == 0 | abs(sentimentr_score) < 0.05
  ) %>%
  arrange(desc(scaled_diff))

# Get top 15 songs with largest differences
top_15_fan_sentimentr_diff <- fan_vs_sentimentr %>%
  head(15)

# First, create a clean version of track names for plotting
top_15_fan_sentimentr_diff <- top_15_fan_sentimentr_diff %>%
  mutate(
    clean_track_name = str_remove_all(track_name, "\\(Taylor's Version\\)") %>%
      str_remove_all("\\[From The Vault\\]") %>%
      str_remove_all("\\[Taylor's Version\\]") %>%
      str_remove_all("\\(10 Minute Version\\)") %>%
      str_remove_all("\\(Piano Remix\\)") %>%
      str_trim()
  )


# Visualize these differences
ggplot(top_15_fan_sentimentr_diff, 
       aes(x = reorder(clean_track_name, scaled_diff))) +
  geom_point(aes(y = net_sentiment, color = "tidytext + Lore", shape = "tidytext + Lore"), size = 4) +
  geom_point(aes(y = sentimentr_score * 6, color = "sentimentR + Lyrics", shape = "sentimentR + Lyrics"), size = 4) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Songs Where Fan Perception Differs Most from SentimentR Analysis",
    subtitle = "Using raw net_sentiment values (scaled SentimentR for comparison)",
    x = "Song",
    y = "Sentiment Score",
    color = "Method",
    shape = "Method"
  ) +
  scale_color_manual(values = c("tidytext + Lore" = "blue", "sentimentR + Lyrics" = "red")) +
  scale_shape_manual(values = c("tidytext + Lore" = 17, "sentimentR + Lyrics" = 16)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))




#### Ok, I prefer sentimentr! So, use that for the rest of the analysis 


