
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

# Load datasets -----------------------------------------------------------


## Renaming the datasets from the taylor package because I don't like underscores haha!
taylorAlbums <- taylor_albums #includes album names, release dates, critics scores and user scores
taylorAlbumSongs <- taylor_album_songs #includes album names, album releases, track numbers, track names,
                                ## featuring, all spotify metrics and lyrics
taylorAllSongs <- taylor_all_songs ##same as taylorAlbumSongs, except it also includes
                                ## songs she wrote/sang and are not in her albums

# Load the Excel files
surpriseSongsDressColours <- "surpriseSongsDressColours.xlsx"
allSongsMetadata <- "albumInfoMetadataMK.xlsx"
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
  "THE TORTURED POETS DEPARTMENT" = "#edebe7"
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
  "snow white" = 'whites',
  
  # Blacks
  "black" = "blacks",
  "blackout" = "blacks",
  "black and blue" = "blacks",
  "dark gray" = "blacks",
  "darkest gray" = "blacks",
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
# No statistically significant relationship between the current dress colour with the 
# previous one (p=0.18). Low R-squared (0.02322), not much variability is explained
# by the model


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
# Significant relationship (p=0.01494) between current and previous dress colours.
# Low R-squared (0.1273), but higher than the first legs. 



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
# No statistically significant relationship between the current dress colour with the 
# previous one (p=0.8062). Lowest of the R-squareds (0.004444), not much variability is explained
# by the model


# How to visualize it using a transition matrix  -------------------------------

# Create transition matrix
color_transitions <- table(
  pianoSongsDataFirstLegs$DressName[-1],  # Current colors (excluding first row)
  pianoSongsDataFirstLegs$DressName[-nrow(pianoSongsDataFirstLegs)]  # Previous colors (excluding last row)
)

# Calculate conditional probabilities
transition_probabilities <- prop.table(color_transitions, margin = 2)

# Convert transition matrix to long format for plotting
transitions_df <- as.data.frame.matrix(color_transitions)
transitions_df$current_color <- rownames(transitions_df)
transitions_long <- transitions_df %>%
  pivot_longer(cols = -current_color,
               names_to = "previous_color",
               values_to = "count")

# Create heatmap
ggplot(transitions_long, aes(x = previous_color, y = current_color, fill = count)) +
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
concert_colors <- pianoSongsDataFirstLegs %>%
  select(Date, DressName) %>%
  mutate(concert_number = row_number())

# Plot color sequence
ggplot(concert_colors, aes(x = concert_number, y = 1)) +
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
print(color_transitions)

cat("\nTransition Probabilities (probability of current color given previous color):\n")
print(round(transition_probabilities, 3))

#Maybe some relationship between Yellow > Pink? But not a lot with the other ones.


# Chi-square test for independence
chisq_result <- chisq.test(color_transitions)
cat("\nChi-square test for independence:\n")
print(chisq_result)


# Create graph from transition matrix
graph <- graph_from_adjacency_matrix(
  as.matrix(color_transitions),
  mode = "directed",
  weighted = TRUE
)

# Get vertex names to match with color palette
vertex_names <- V(graph)$name

# Create color vector for vertices
vertex_colors <- colorPaletteDresses[vertex_names]

# Plot the network with custom colors
plot(graph,
     edge.width = E(graph)$weight,
     vertex.color = vertex_colors,  # Use custom colors
     vertex.size = 30,
     vertex.label.color = "black",
     edge.curved = 0.3,
     main = "Dress Color Transition Network: First Legs")



## EUROPEAN LEG
# Create transition matrix
color_transitions <- table(
  pianoSongsDataEuropeanLeg$DressName[-1],  # Current colors (excluding first row)
  pianoSongsDataEuropeanLeg$DressName[-nrow(pianoSongsDataEuropeanLeg)]  # Previous colors (excluding last row)
)

# Calculate conditional probabilities
transition_probabilities <- prop.table(color_transitions, margin = 2)

# Convert transition matrix to long format for plotting
transitions_df <- as.data.frame.matrix(color_transitions)
transitions_df$current_color <- rownames(transitions_df)
transitions_long <- transitions_df %>%
  pivot_longer(cols = -current_color,
               names_to = "previous_color",
               values_to = "count")

# Create heatmap
ggplot(transitions_long, aes(x = previous_color, y = current_color, fill = count)) +
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
concert_colors <- pianoSongsDataEuropeanLeg %>%
  select(Date, DressName) %>%
  mutate(concert_number = row_number())

# Plot color sequence
ggplot(concert_colors, aes(x = concert_number, y = 1)) +
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
print(color_transitions)

cat("\nTransition Probabilities (probability of current color given previous color):\n")
print(round(transition_probabilities, 3))

# So, it looks like the order was mostly sunset orange > ocean blue > flamingo pink > sunset orange

# Chi-square test for independence
chisq_result <- chisq.test(color_transitions)
cat("\nChi-square test for independence:\n")
print(chisq_result)


# Create graph from transition matrix
graph <- graph_from_adjacency_matrix(
  as.matrix(color_transitions),
  mode = "directed",
  weighted = TRUE
)

# Get vertex names to match with color palette
vertex_names <- V(graph)$name

# Create color vector for vertices
vertex_colors <- colorPaletteDresses[vertex_names]

# Plot the network with custom colors
plot(graph,
     edge.width = E(graph)$weight,
     vertex.color = vertex_colors,  # Use custom colors
     vertex.size = 30,
     vertex.label.color = "black",
     edge.curved = 0.3,
     main = "Dress Color Transition Network: European Leg")



## FINAL LEG

# Create transition matrix
color_transitions <- table(
  pianoSongsDataFinalLeg$DressName[-1],  # Current colors (excluding first row)
  pianoSongsDataFinalLeg$DressName[-nrow(pianoSongsDataFinalLeg)]  # Previous colors (excluding last row)
)

# Calculate conditional probabilities
transition_probabilities <- prop.table(color_transitions, margin = 2)

# Convert transition matrix to long format for plotting
transitions_df <- as.data.frame.matrix(color_transitions)
transitions_df$current_color <- rownames(transitions_df)
transitions_long <- transitions_df %>%
  pivot_longer(cols = -current_color,
               names_to = "previous_color",
               values_to = "count")

# Create heatmap
ggplot(transitions_long, aes(x = previous_color, y = current_color, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  theme_minimal() +
  labs(x = "Previous Concert's Dress Color",
       y = "Current Concert's Dress Color",
       title = "Dress Color Transition Heatmap - Final Leg",
       fill = "Number of\nTransitions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Create a sequential visualization
concert_colors <- pianoSongsDataFinalLeg %>%
  select(Date, DressName) %>%
  mutate(concert_number = row_number())

# Plot color sequence
ggplot(concert_colors, aes(x = concert_number, y = 1)) +
  geom_tile(aes(fill = DressName), width = 0.9) +
  scale_fill_manual(values = colorPaletteDresses) +
  theme_minimal() +
  labs(x = "Concert Number",
       y = NULL,
       title = "Sequential Dress Color Choices - Final Leg",
       fill = "Dress Color") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# Print summary statistics
cat("\nTransition Matrix (raw counts):\n")
print(color_transitions)

cat("\nTransition Probabilities (probability of current color given previous color):\n")
print(round(transition_probabilities, 3))

# No strong relationship, but also worth saying that the last concerts were all being
# recorded (maybe another documentary?) and so she was weating Blurple in all of
# the surprise songs

# Chi-square test for independence
chisq_result <- chisq.test(color_transitions)
cat("\nChi-square test for independence:\n")
print(chisq_result)


# Create graph from transition matrix
graph <- graph_from_adjacency_matrix(
  as.matrix(color_transitions),
  mode = "directed",
  weighted = TRUE
)

# Get vertex names to match with color palette
vertex_names <- V(graph)$name

# Create color vector for vertices
vertex_colors <- colorPaletteDresses[vertex_names]

# Plot the network with custom colors
plot(graph,
     edge.width = E(graph)$weight,
     vertex.color = vertex_colors,  # Use custom colors
     vertex.size = 30,
     vertex.label.color = "black",
     edge.curved = 0.3,
     main = "Dress Color Transition Network: Final Leg")




# Plotting dress colours  ------------------------------------------------------------


# Now, let's plot the frequency of each dress

pianoSongsData %>%
  count(DressName) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(DressName, n), y = n, fill = DressName)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = colorPaletteDresses) +
  theme_minimal() +
  labs(title = "Distribution of Dress Colors",
       x = "Dress Color",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

pianoSongsData$Date <- as.Date(pianoSongsData$Date)

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
  geom_vline(xintercept = as.Date("2024-04-19"), linetype = "dotted", color = "gray") +
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
           label = "TTPD", color = "gray", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2023-07-07"), y = max(pianoSongsData$DressName), 
           label = "Speak\nNow TV", color = "purple", angle = -90, vjust = -0.5) +
  annotate("text", x = as.Date("2023-10-27"), y = max(pianoSongsData$DressName), 
           label = "1989\nTV", color = "blue", angle = -90, vjust = -0.5) +
  
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust=0.5))


## Trying to separate the album releases into a separate timeline
# I used vistime example from their vignette, which means I need to rename some columns 
# to make it work

colnames(surpriseSongsDressColours)

timelineSurpriseSongsDressColours <- surpriseSongsDressColours %>%
  select(Legs, Relationship, Start, End, ColourHex1) %>%
  rename(
    event = "Legs",
    group = "Relationship",
    start = "Start",
    end = "End",
    color = "ColourHex1"
  )

# dress colours x relationship
gg_vistime(timelineSurpriseSongsDressColours)

timelineSurpriseSongsDressColours <- surpriseSongsDressColours %>%
  select(Legs, Relationship, Start, End, ColourHex1) %>%
  rename(
    event = "Relationship",
    group = "Legs",
    start = "Start",
    end = "End",
    color = "ColourHex1"
  )

# dress colour x tour leg
gg_vistime(timelineSurpriseSongsDressColours)


## The plots I had before are better, possibly remove gg_vistime from this code.


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

# Apply the function to your data
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
taylorSentimentsBing <- calculateSentimentsF(tokenizedSongs)

# Preview the results
head(taylorSentimentsBing)

# Get a quick summary of sentiments
sentimentSummary <- taylorSentimentsBing %>%
  count(sentiment) %>%
  spread(sentiment, n)

print(sentimentSummary)

taylorSentimentsBing$track_number <- as.factor(taylorSentimentsBing$track_number)

# I also wanna see the plots in chronological order

albumOrder <- c("Taylor Swift", "Fearless (Taylor's Version)", 
                 "Speak Now (Taylor's Version)", "Red (Taylor's Version)",
                 "1989 (Taylor's Version)", "Reputation", "Lover",
                 "folklore", "evermore", "Midnights",
                 "THE TORTURED POETS DEPARTMENT")

taylorSentimentsBing <- taylorSentimentsBing %>%
  mutate(album_name = factor(album_name, levels = albumOrder))

ggplot(taylorSentimentsBing, aes(track_number, sentiment, fill = album_name)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~album_name, ncol = 3, scales = "free_x") +
    scale_fill_manual(values = colorPaletteAlbums) +
    theme_minimal() +
    labs(
      title = "Sentiment Distribution Across Albums - Lyrics + Bing Dictionary",
      x = "Track Number",
      y = "Sentiment Score"
    ) +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y = element_blank())




# What is the sentiment of each song based on bing dictionary scoring of MY IMPRESSIONS ---------------



# I first need to combine my metadata columns into one so I can sort them later

processMetadataColumnsF <- function(data) {
  
  combinedMetadata <- data %>%
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
taylorSentimentsMetadataBing <- calculateSentimentsMetadataF(processedMetadata)
colnames(taylorSentimentsMetadataBing)
unique(taylorSentimentsMetadataBing$sentiment)


# Preview the results
head(taylorSentimentsMetadataBing)

# Get a quick summary of sentiments
sentimentSummary <- taylorSentimentsMetadataBing %>%
  count(sentiment) %>%
  spread(sentiment, n)

print(sentimentSummary)

taylorSentimentsMetadataBing$track_number <- as.factor(taylorSentimentsMetadataBing$track_number)

## Also order the albums in chronological order

taylorSentimentsMetadataBing <- taylorSentimentsMetadataBing %>%
  mutate(album_name = factor(album_name, levels = albumOrder))

ggplot(taylorSentimentsMetadataBing, aes(track_number, sentiment, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~album_name, ncol = 3, scales = "free_x") +
  scale_fill_manual(values = colorPaletteAlbums) +
  theme_minimal() +
  labs(
    title = "Sentiment Distribution Across Albums - Marina's Metadata + Bing Dictionary",
    x = "Track Number",
    y = "Sentiment Score"
  ) +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.y = element_blank())



## Trying to see the contribution of each word to the sentiment

wordContributions <- taylorSentimentsBing %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 50) %>%
  mutate(word = reorder_within(word, n, sentiment)) %>%
  ungroup()

wordContributions %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_y_reordered() +  # Required for reorder_within to work
  labs(
    title = "Top 50 Words Contributing to Sentiment - Lyrics + Bing dictionary",
    x = "Frequency",
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("negative" = "#D65D5D", "positive" = "#7CAE7A")) +
  theme(plot.title = element_text(hjust=0.5))

#todo
## whoa is a positive word?? lol maybe I'll remove it from the analysis

wordContributions <- taylorSentimentsBing %>%
    count(word, sentiment) %>%
    group_by(sentiment) %>%
   slice_max(n, n = 10) %>%
  mutate(word = reorder_within(word, n, sentiment)) %>%
  ungroup()

wordContributions %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_y_reordered() +  # Required for reorder_within to work
  labs(
    title = "Top 10 Words Contributing to Sentiment - Lyrics + Bing dictionary",
    x = "Frequency",
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("negative" = "#D65D5D", "positive" = "#7CAE7A")) +
  theme(plot.title = element_text(hjust=0.5))


wordContributions %>%
  arrange(sentiment, desc(n)) %>%
  print(n = 20)


## Same, but for my metadata 

wordContributionsMetadata <- taylorSentimentsMetadataBing %>%
  count(metadata_word, sentiment) %>%
  group_by(sentiment) %>%
  #slice_max(n, n = 10) %>%
  mutate(metadata_word = reorder_within(metadata_word, n, sentiment)) %>%
  ungroup()


wordContributionsMetadata %>%
  ggplot(aes(n, metadata_word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_y_reordered() +  # Required for reorder_within to work
  labs(
    title = "All Words Contributing to Sentiment - Marina's metadata + Bing dictionary",
    x = "Frequency",
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("negative" = "#D65D5D", "positive" = "#7CAE7A")) +
  theme(plot.title = element_text(hjust=0.5))

wordContributionsMetadata <- taylorSentimentsMetadataBing %>%
  count(metadata_word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  mutate(metadata_word = reorder_within(metadata_word, n, sentiment)) %>%
  ungroup()

wordContributionsMetadata %>%
  ggplot(aes(n, metadata_word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  scale_y_reordered() +  # Required for reorder_within to work
  labs(
    title = "Top 10 Words Contributing to Sentiment - Marina's metadata + Bing dictionary",
    x = "Frequency",
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("negative" = "#D65D5D", "positive" = "#7CAE7A")) +
  theme(plot.title = element_text(hjust=0.5))

## todo:
# things that might need correction: crush is shown as a negative word,
# obsession is positive, envious is positive.

wordContributionsMetadata %>%
  arrange(sentiment, desc(n)) %>%
  print()



# What do colours mean for Taylor/the fans? -------------------------------

# I edited the dataset to include every mention of colour, both the lyrics that
# mention them and the colour itself that's mentioned.
# these are in:


allSongsMetadata$colour_lyric_MK
unique(allSongsMetadata$colour_MK)

## but I reckon the easiest way to decide if a colour is happy or sad is to manually
# look at the spreadsheet on excel and add the info, so I'll do just that.
# I'll only use positive/negative this time to make it easier:

allSongsMetadata$colour_meaningMK

#Now, I need to split the colours and their meanings into separate lines, since
# some songs have more than one colour in them:

# Expand the dataset to have one row per color-sentiment pair
expandedData <- allSongsMetadata %>%
  separate_rows(colour_MK, colour_meaningMK, sep = ";") %>%
  mutate(across(everything(), ~ trimws(.)))  # Remove extra spaces if any


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


plotData <- allSongsMetadata %>%
  separate_rows(colour_MK, sep = ";") %>%
  mutate(across(everything(), ~ trimws(.))) %>%  # Clean whitespace
  filter(!is.na(colour_MK)) %>% 
  group_by(colour_MK) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(
    across(everything(), ~ trimws(.)), # Remove extra spaces
    colour_group = colorGroups[colour_MK] # Map colors to broader groups
  )



# Create the plot
ggplot(plotData, aes(x = colour_MK, y = count, fill = colour_MK)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colorPaletteColours) +  # Match colors
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
plotData$colour_group <- factor(plotData$colour_group, levels = rainbowOrder)


#plotData <- allSongsMetadata %>%
#  filter(!is.na(colour_group)) %>% # Exclude NA values
#  group_by(colour_MK, colour_group, colour_meaningMK) %>%
#  summarize(count = n(), .groups = "drop")



# Create the plot
ggplot(plotData, aes(x = colour_MK, y = count, fill = colour_MK)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colorPaletteColours) +  # Apply custom colors for each colour_MK
  facet_wrap(~ colour_group, scales = "free_x") +   # Match colors
  theme_minimal() +
  labs(
    title = "Colours in Taylor Swift's Writing",
    x = "",
    y = "Count",
    fill = "Colours"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )



## or, group the colours and plot them in groups
plotData <- allSongsMetadata %>%
  separate_rows(colour_MK, colour_meaningMK, sep = ";") %>%
  mutate(across(everything(), ~ trimws(.))) %>%  # Clean whitespace
  filter(!is.na(colour_MK)) %>% 
  group_by(colour_MK, colour_meaningMK) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(
    across(everything(), ~ trimws(.)), # Remove extra spaces
    colour_group = colorGroups[colour_MK] # Map colors to broader groups
  )



ggplot(plotData, aes(x = colour_group, y = count, fill = colour_meaningMK)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("positive" = "#4CAF50", "negative" = "#F44336")) +  # Apply color for sentiments
  coord_flip() +  # Flip the coordinates to have positive on the left and negative on the right
  theme_minimal() +
  labs(
    title = "Distribution of Colours and Sentiments in Taylor Swift's Writing",
    x = "Colour Groups",
    y = "Count",
    fill = "Sentiment"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
   # legend.position = "none"
  )


colorPaletteColourGroups <- c("reds" = "red",
                              "yellows" = "yellow",
                              "blues" = "blue", 
                              "purples" = "purple",
                              "colorful" = "#FF4500", #screaming colour shade,
                              "whites" = "white",
                              "black and white" = "gray",
                              "blacks" = "black",
                              "positive" = "darkgreen",
                              "negative" = "brown"
                              )



ggplot(plotData, aes(x = colour_group, y = count, 
                     fill = colour_meaningMK, color = colour_meaningMK)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE, size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Colours and Sentiments in Taylor Swift's Writing",
    x = "Colour Groups",
    y = "Count",
    fill = "Sentiment"
  ) +
  scale_fill_manual(values = colorPaletteColourGroups) +
  scale_color_manual(values = colorPaletteColourGroups) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

#todo
## UGH - something broke it and now there's more than one positive/negative column
# for each group colour - to be revised



# REMOVED CODE BELOW ------------------------------------------------------

# PCA for each group ------------------------------------------------------

columnsForPCA <- c("DressName_numeric", "DressName_lag")
firstLegsScaled <- scale(pianoSongsDataFirstLegs[,columnsForPCA])
europeanLegScaled <- scale(pianoSongsDataEuropeanLeg[,columnsForPCA])
finalLegScaled <- scale(pianoSongsDataFinalLeg[,columnsForPCA])

pcaFirstLeg <- prcomp(firstLegsScaled, center = TRUE, scale. = TRUE)
pcaEuropeanLeg <- prcomp(europeanLegScaled, center = TRUE, scale. = TRUE)
pcaFinalLeg <- prcomp(finalLegScaled, center = TRUE, scale. = TRUE)


plotPCA <- function(pcaResult, groupName) {
  pcaData <- as.data.frame(pcaResult$x)
  pcaData$PC1 <- pcaResult$x[, 1]
  pcaData$PC2 <- pcaResult$x[, 2]
  
  ggplot(pcaData, aes(x = PC1, y = PC2)) +
    geom_point(color = "blue", size = 3, alpha = 0.6) +
    labs(title = paste("PCA Plot -", groupName),
         x = "Principal Component 1",
         y = "Principal Component 2") +
    theme_minimal()
}

plotPCA(pcaFirstLeg, "First Leg")
plotPCA(pcaEuropeanLeg, "European Leg")
plotPCA(pcaFinalLeg, "Final Leg")

summary(pcaFirstLeg)
summary(pcaEuropeanLeg)
summary(pcaFinalLeg)


screePlot <- function(pcaResult, groupName) {
  explainedVar <- pcaResult$sdev^2 / sum(pcaResult$sdev^2)
  df <- data.frame(PC = 1:length(explainedVar), Variance = explainedVar)
  
  ggplot(df, aes(x = PC, y = Variance)) +
    geom_line() +
    geom_point(size = 3) +
    labs(title = paste("Scree Plot -", groupName),
         x = "Principal Components",
         y = "Proportion of Variance Explained") +
    theme_minimal()
}


screePlot(pcaFirstLeg, "First Leg")
screePlot(pcaEuropeanLeg, "European Leg")
screePlot(pcaFinalLeg, "Final Leg")



