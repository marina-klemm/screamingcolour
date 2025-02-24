
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

# Load datasets -----------------------------------------------------------


## Renaming the datasets from the taylor package because I don't like underscores haha!
taylorAlbums <- taylor_albums #includes album names, release dates, critics scores and user scores
taylorAlbumSongs <- taylor_album_songs #includes album names, album releases, track numbers, track names,
## featuring, all spotify metrics and lyrics
taylorAllSongs <- taylor_all_songs ##same as taylorAlbumSongs, except it also includes
## songs she wrote/sang and are not in her albums

# Load the Excel files
surpriseSongsDressColours <- "surpriseSongsDressColours.xlsx"
allSongsMetadata <- "albumInfoMetadataNeutralMK.xlsx"
relationshipsTimeline <- "surpriseSongsDressColours.xlsx"

surpriseSongsDressColours <- read_excel(surpriseSongsDressColours, sheet = "List")
allSongsMetadata <- read_excel(allSongsMetadata, sheet = "metadata")
relationshipsTimeline <- read_excel(relationshipsTimeline, sheet = "Relationships")

pianoSongsData <- surpriseSongsDressColours[surpriseSongsDressColours$Instrument == "Piano",]




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
  "gold" = "yellows",        # Added
  "golden" = "yellows",      # Added
  "greige" = "yellows",
  "rose golden" = "yellows",
  "orange" = "yellows",
  "tangerine" = "yellows",   # Added
  
  # Reds
  "blood monlit" = "reds",
  "blood-soaked" = "reds",
  "burning red" = "reds",    # Added
  "crimson" = "reds",
  "crimson clover" = "reds", # Added
  "flamingo pink" = "reds",
  "burgundy" = "reds",
  "cherry" = "reds",
  "maroon" = "reds",         # Added
  "pink" = "reds",
  "light pink" = "reds",
  "red" = "reds",           # Added
  "redhaired" = "reds",
  "redneck" = "reds",
  "rose" = "reds",          # Added
  "rosy" = "reds",          # Added
  "scarlet" = "reds",       # Added
  "scarlet maroon" = "reds",
  "wine-stained" = "reds",
  
  # Blues
  "aquamarine" = "blues",   # Added
  "blue" = "blues",         # Added
  "blues" = "blues",        # Added
  "bluest" = "blues",       # Added
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
  "teal" = "greens",        # Added
  
  # Colorful
  "colour" = "colorful",    # Added
  "colours" = "colorful",   # Added
  "moonstone" = "colorful", # Added
  "neon" = "colorful",      # Added
  "pastel" = "colorful",    # Added
  "plaid" = "colorful",     # Added
  "rainbow" = "colorful",
  "screaming colour" = "colorful", # Added
  "stained glass" = "colorful",
  
  # Black and white
  "black and white" = "black and white",
  
  # Whites
  "bleached" = "whites",    # Added
  "greige" = "whites",
  "hospital gray" = "whites", # Added
  "gray" = "whites",        # Added
  "white" = "whites",       # Added
  "snow white" = "whites",  # Added
  
  # Blacks
  "black" = "blacks",       # Added
  "blackout" = "blacks",    # Added
  "black and blue" = "blacks",
  "dark gray" = "blacks",
  "darkest gray" = "blacks", # Added
  "silver" = "blacks"
)


## Organize the colour syntax
dressColorMapping <- unique(pianoSongsData %>% select(DressName, ColourHex1))
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

unique(pianoSongsData$Legs)

pianoSongsDataFirstLegs <- pianoSongsData[pianoSongsData$Legs %in% c("First leg", "Latin America", "Asia-Oceania"),]
pianoSongsDataEuropeanLeg <- pianoSongsData[pianoSongsData$Legs == "European leg",]
pianoSongsDataFinalLeg <- pianoSongsData[pianoSongsData$Legs == "Final leg",]


str(pianoSongsDataFirstLegs)


## FIRST LEGS: Americas and Asia-Oceania


# I'm adding a number for each colour name for the linear modelling
pianoSongsDataFirstLegs <- pianoSongsDataFirstLegs %>%
  mutate(DressName_numeric = as.numeric(factor(DressName)))
# this adds a column with the dress colour of the previous concert
pianoSongsDataFirstLegs <- pianoSongsDataFirstLegs %>%
  mutate(DressName_lag = lag(DressName_numeric))

pianoSongsDataFirstLegs <- pianoSongsDataFirstLegs %>%
  filter(!is.na(DressName_lag))

lm_resultFirstLegs <- lm(DressName_numeric ~ DressName_lag, data = pianoSongsDataFirstLegs)

summary(lm_resultFirstLegs)


## EUROPEAN LEG: 

# I'm adding a number for each colour name for the linear modelling
pianoSongsDataEuropeanLeg <- pianoSongsDataEuropeanLeg %>%
  mutate(DressName_numeric = as.numeric(factor(DressName)))
# this adds a column with the dress colour of the previous concert
pianoSongsDataEuropeanLeg <- pianoSongsDataEuropeanLeg %>%
  mutate(DressName_lag = lag(DressName_numeric))

pianoSongsDataEuropeanLeg <- pianoSongsDataEuropeanLeg %>%
  filter(!is.na(DressName_lag))

lm_resultEuropeanLeg <- lm(DressName_numeric ~ DressName_lag, data = pianoSongsDataEuropeanLeg)

summary(lm_resultEuropeanLeg)



## FINAL LEG: 

# I'm adding a number for each colour name for the linear modelling
pianoSongsDataFinalLeg <- pianoSongsDataFinalLeg %>%
  mutate(DressName_numeric = as.numeric(factor(DressName)))
# this adds a column with the dress colour of the previous concert
pianoSongsDataFinalLeg <- pianoSongsDataFinalLeg %>%
  mutate(DressName_lag = lag(DressName_numeric))

pianoSongsDataFinalLeg <- pianoSongsDataFinalLeg %>%
  filter(!is.na(DressName_lag))

lm_resultFinalLeg <- lm(DressName_numeric ~ DressName_lag, data = pianoSongsDataFinalLeg)

summary(lm_resultFinalLeg)


# How to visualize it using a transition matrix: First Legs  -------------------------------

# Create transition matrix
colorTransitionsFirstLegs <- table(
  pianoSongsDataFirstLegs$DressName[-1],  # Current colors (excluding first row)
  pianoSongsDataFirstLegs$DressName[-nrow(pianoSongsDataFirstLegs)]  # Previous colors (excluding last row)
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
concertColorsFirstLegs <- pianoSongsDataFirstLegs %>%
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
cat("\nTransition Matrix (raw counts):\n")
print(colorTransitionsFirstLegs)

cat("\nTransition Probabilities (probability of current color given previous color):\n")
print(round(transitionProbabilitiesFirstLegs, 3))



# Chi-square test for independence
chiSquareResultsFirstLegs <- chisq.test(colorTransitionsFirstLegs)
cat("\nChi-square test for independence:\n")
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
  pianoSongsDataEuropeanLeg$DressName[-1],  # Current colors (excluding first row)
  pianoSongsDataEuropeanLeg$DressName[-nrow(pianoSongsDataEuropeanLeg)]  # Previous colors (excluding last row)
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
concertColorsEuropeanLeg <- pianoSongsDataEuropeanLeg %>%
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
cat("\nTransition Matrix (raw counts):\n")
print(colorTransitionsEuropeanLeg)

cat("\nTransition Probabilities (probability of current color given previous color):\n")
print(round(transitionProbabilitiesEuropeanLeg, 3))




# Chi-square test for independence
chiSquareResultsEuropeanLeg <- chisq.test(colorTransitionsEuropeanLeg)
cat("\nChi-square test for independence:\n")
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
  pianoSongsDataFinalLeg$DressName[-1],  # Current colors (excluding first row)
  pianoSongsDataFinalLeg$DressName[-nrow(pianoSongsDataFinalLeg)]  # Previous colors (excluding last row)
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
concertColorsFinalLeg <- pianoSongsDataFinalLeg %>%
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
cat("\nTransition Matrix (raw counts):\n")
print(colorTransitionsFinalLeg)

cat("\nTransition Probabilities (probability of current color given previous color):\n")
print(round(transitionProbabilitiesFinalLeg, 3))


# Chi-square test for independence
chiSquareResultsFinalLeg <- chisq.test(colorTransitionsFinalLeg)
cat("\nChi-square test for independence:\n")
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

pianoSongsData %>%
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

pianoSongsData$Date <- as.Date(pianoSongsData$Date)


# Adding images to the dress plot -----------------------------------------

unique(pianoSongsData$DressName)

pathToDressColours <- "C:/Users/mklemm/OneDrive - AUCKLAND MUSEUM/Documents/Personal/FunAnalysis/DressesColours/ImprovedQuality/Cropped/"


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




dressesVis <- ggplot(pianoSongsDataWithImages, aes(x = reorder(DressName, -n), y = n, fill = DressName)) +
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


ggplot(pianoSongsData, aes(x = Date, y = DressName, color = ColourHex1)) +
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
  annotate("text", x = as.Date("2024-05-09"), y = max(pianoSongsData$DressName), 
           label = "Europe", color = "black", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2024-10-18"), y = max(pianoSongsData$DressName), 
           label = "North \nAmerica", color = "black", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2023-08-24"), y = max(pianoSongsData$DressName), 
           label = "Latin \nAmerica", color = "black", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2024-02-07"), y = max(pianoSongsData$DressName), 
           label = "Asia/\nOceania", color = "black", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2024-04-19"), y = max(pianoSongsData$DressName), 
           label = "TTPD", color = "darkgray", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2023-07-07"), y = max(pianoSongsData$DressName), 
           label = "Speak\nNow TV", color = "purple", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2023-10-27"), y = max(pianoSongsData$DressName), 
           label = "1989\nTV", color = "blue", angle = -90, vjust = -0.5) +
  
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust=0.5))




# Plotting the relationships timeline -------------------------------------

# Order Person by DateStart
relationshipsTimeline$Person <- factor(relationshipsTimeline$Person, 
                                       levels = relationshipsTimeline$Person[order(relationshipsTimeline$DateStart)])


## DISCLAIMER: I included all the confirmed men + two women the fandom believes she might've
## dated, which was never confirmed. I personally think that it's only bad to especulate
# over someone's bissexuality if you think bissexuality is a bad thing.BUT, we might
# prefer to not include them into the analysis to avoid backlash. At this point, the women
# are shown with dashed lines


ggplot(relationshipsTimeline, aes(x = DateStart, xend = DateEnd, 
                                  y = Person, yend = Person, 
                                  color = Person, linetype = Person)) +
  geom_segment(size = 2) +
  geom_point(size = 4) +
  geom_point(aes(x = DateEnd), size = 4) +
  scale_color_manual(values = colorPaletteRelationships) +
  scale_linetype_manual(values = lineTypeRelationships) +
  scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  labs(title = "Relationships Timeline")




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

# Preview the results
head(taylorLyrics)

# Get a quick summary of sentiments
sentimentSummary <- taylorLyrics %>%
  count(sentiment) %>%
  spread(sentiment, n)

print(sentimentSummary)

taylorLyrics$track_number <- as.factor(taylorLyrics$track_number)

# I also wanna see the plots in chronological order

albumOrder <- c("Taylor Swift", "Fearless (Taylor's Version)", 
                 "Speak Now (Taylor's Version)", "Red (Taylor's Version)",
                 "1989 (Taylor's Version)", "Reputation", "Lover",
                 "folklore", "evermore", "Midnights",
                 "THE TORTURED POETS DEPARTMENT")

taylorLyrics <- taylorLyrics %>%
  mutate(album_name = factor(album_name, levels = albumOrder))
taylorLyrics <- taylorLyrics %>%
  filter(!is.na(album_name)) 

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


# Correlation between my meanings x lyrics meanings -----------------------


# Relating taylorLyrics and taylorMetadata

head(taylorLyrics$sentiment)
head(taylorMetadata$sentiment)



# Create plot for Taylor's Lyrics
lyrics_plot <- comparisonSentimentsTwoAnalysis %>%
  filter(dataset == "taylorLyrics") %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(n, " (", round(percent, 1), "%)")), 
            vjust = -0.5, size = 4) +
  labs(
    title = "Swift's Lyrics",
    y = "",
    x = NULL
  ) +
  scale_fill_manual(values = c("positive" = "#2E8B57", 
                               "negative" = "#D9534F")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "none",
    axis.text.x = element_text(hjust = 0.5, size = 12)
  )

# Create plot for Fanbase Lore
lore_plot <- comparisonSentimentsTwoAnalysis %>%
  filter(dataset == "taylorMetadata") %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(n, " (", round(percent, 1), "%)")), 
            vjust = -0.5, size = 4) +
  labs(
    title = "Swiftie Fanbase Lore",
    y = "",
    x = NULL
  ) +
  scale_fill_manual(values = c("positive" = "#2E8B57", 
                               "negative" = "#D9534F")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "none",
    axis.text.x = element_text(hjust = 0.5, size = 12)
  )

# Combine plots with a shared title
combined_plot <- lyrics_plot + lore_plot +
  plot_annotation(
    title = "Swiftly Feeling: Comparing Song Lyrics & Fanbase Lore Sentiments",
    #subtitle = "Independent scales for better proportion comparison",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

# Display the combined plot
combined_plot



# Correlation plot: Lyrics x Lore -----------------------------------------

# Aggregate sentiment scores for each song

taylorMetadata <- taylorMetadata %>%
  mutate(sentimentNumber = if_else(sentiment == "positive", 1, 0))

metadataSentimentScores <- taylorMetadata %>%
  group_by(track_number, album_name, track_name) %>%
  summarize(metadata_sentiment_score = mean(sentimentNumber, na.rm = TRUE))

taylorLyrics <- taylorLyrics %>%
  mutate(sentimentNumber = if_else(sentiment == "positive", 1, 0))

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
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
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
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
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
  ) +
  scale_y_continuous(limits = c(-0.1, 1.1), 
                     expand = expansion(mult = c(0.02, 0.1)))


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
    caption = paste("Analysis based on", nrow(rawColorData), "color mentions")
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

print("\nPairwise Comparisons:")
print(sentimentPairwise)

print("\nDescriptive Statistics by Group:")
print(sentimentGroupStats)


# Lastly: how do songs' sentiments relate to dress colors --------

# The datasets I want to work with:
## FIRST, the lore:

colorGroupSentiments #the rating of each color group
taylorMetadata #the rating of each song based on the lore
surpriseSongsDressColours #all the dress colors and songs played in the Eras Tour

#What I need to do:
#Join all the songs into a single column, since there are songs in the Mashup and Mashup2 cols
#Classify the dress colors into a color group;
#Grab the songs played wearing each dress;
#Compare the rating of the dress x rating of the song and find a relationship (if any)

colnames(colorGroupSentiments)
colnames(taylorMetadata)
colnames(surpriseSongsDressColours)

# Step 0: Join all songs into one column


allSongsExpanded <- bind_rows(
  surpriseSongsDressColours %>%
    select(`Song title`, ColorGroup, groupName, groupSentiment) %>%
    rename(song = `Song title`),
  
  surpriseSongsDressColours %>%
    select(Mashup, ColorGroup, groupName, groupSentiment) %>%
    filter(!is.na(Mashup)) %>%
    rename(song = Mashup),
  
  surpriseSongsDressColours %>%
    select(Mashup2, ColorGroup, groupName, groupSentiment) %>%
    filter(!is.na(Mashup2)) %>%
    rename(song = Mashup2)
)


# Step 1: Classify dress colors into color groups

#look at the dresses:
dressesVis

unique(surpriseSongsDressColours$DressName)
unique(colorGroupSentiments$groupName)

surpriseSongsDressColours$groupName <- sapply(surpriseSongsDressColours$DressName, function(color) {
  if (color %in% c("Pink", "Flamingo pink")) return("reds") ## the gradient is quite similar, it fits reds
  if (color %in% c("Green")) return("greens")
  if(color %in% c("Yellow", "Sunset orange")) return("yellows")
  if (color %in% c("Ocean blue", "Blue", "Blurple")) return ("blues")
  if (color %in% c("Popsicle", "Cotton candy", "Grapefruit")) return ("colorful")
  return("Neutral")
})
#Check
unique(surpriseSongsDressColours$groupName)


# Step 2: Merge datasets to get ratings
mergedDressColorGroups <- merge(surpriseSongsDressColours, colorGroupSentiments, by = "groupName")
mergedDressColorsMetadata <- merge(mergedDressColorGroups, taylorMetadata, by = "normalizedSongName")

# Step 3: Compare ratings of the dress and the song
correlationResult <- cor(mergedDressColorsMetadata$groupSentiment, mergedDressColorsMetadata$sentimentNumber)
print(correlationResult)

library(ggplot2)

ggplot(mergedDressColorsMetadata, aes(x = groupSentiment, y = sentimentNumber)) +
  geom_point(color = "blue", alpha = 0.6) + # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Trend line
  labs(title = "Correlation Between Dress Color and Song Sentiment",
       x = "Dress Color Sentiment",
       y = "Song Sentiment") +
  theme_minimal()


