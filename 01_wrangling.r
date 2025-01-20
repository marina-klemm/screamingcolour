## reading in data
require(readxl)
require(tidyverse)
require(GGally)
require(corrplot)
require(factoextra)
require(vegan)
require(ape)
require(RColorBrewer)
require(plotly)
require(MASS)
require(klaR)
album <- read_xlsx("raw_data/albumInfoMetadataMK.xlsx")
surprise <-  read_xlsx("raw_data/surpriseSongsDressColours.xlsx")

## multivariate EDA for album songs
data <- album %>% 
    drop_na("danceability") %>%
    mutate(short_track_name = gsub("^((\\w+\\W+){4}\\w+).*$","\\1",track_name)) %>%
    column_to_rownames(var = "short_track_name")
   
M <- data %>%
    select(where(is.numeric)) %>%
    select(!c("track_number", "mode", "time_signature")) %>%
    cor()
corrplot(M, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PiYG'))
## PCA
pca <- data %>%
    select(where(is.numeric)) %>%
    select(!c("track_number", "mode", "time_signature")) %>%
    scale() %>% 
    prcomp()

fviz_pca_biplot(pca, geom = "text", labelsize = 3) +
    geom_point(alpha = 0.2)

## clustering
dist <-  data %>%
    select(where(is.numeric)) %>%
    select(!c("track_number", "mode", "time_signature")) %>%
    vegdist(method = "euclidean")
fviz_dist(dist, lab_size = 5)
ward <- dist %>% hclust(method = "ward.D")

phylo <- as.phylo(ward)
phylo$tip.label <- rownames(data)
colours  <- RColorBrewer::brewer.pal(n = 6, name = "Dark2")
plot(phylo, cex = 0.6, tip.color = colours[cutree(ward, k = 6)], label.offset = 0.05,
     type = "radial")
split(names(cutree(ward, k = 6)), cutree(ward, k = 6))

## surprise songs
## matching song names
## clunky way of removing qualifiers and fixing typos
name <- gsub(" (Taylor's Version)", "", album$track_name, fixed = TRUE)
name <- gsub(" [Taylor's Version]", "", name, fixed = TRUE)
name <- gsub("  [From The Vault]", "", name, fixed = TRUE)
name <- gsub(" [From The Vault]", "", name, fixed = TRUE)
name <- gsub(" (From The Vault)", "", name, fixed = TRUE)
## typos
surprise$`Song title` <- gsub("Mr. Perfecly Fine", "Mr. Perfectly Fine", surprise$`Song title`)
surprise$`Song title` <- gsub("Mr Perfectly Fine", "Mr. Perfectly Fine", surprise$`Song title`)
surprise$`Song title` <- gsub("Markus", "Marcus", surprise$`Song title`)
## remove special characters bc they can cause issues
surprise$`Song title` <- gsub("Slut!", "Slut", surprise$`Song title`)
name <- gsub("\"Slut!\"", "Slut", name, fixed = TRUE)
name <- gsub("Is It Over Now?", "Is It Over Now", name, fixed = TRUE)
name <- gsub("'tis the damn season", "tis the damn season", name, fixed = TRUE)
album$name <- name
## songs in "surprise" not in "album$name" sould be non-swift songs
## check with Marina
surprise[which(!tolower(surprise$`Song title`) %in% tolower(album$name)),]
## uncapitalise
album$name <- tolower(album$name)
surprise$`Song title` <- tolower(surprise$`Song title`)
## left join
songs <- left_join(surprise, album, by = c(`Song title` = "name")) %>%  drop_na("danceability") 

df <- songs %>%
    select(where(is.numeric)) %>%
    select(!c("track_number", "mode", "time_signature", `Night #`, "Order"))

cluster <-  kmeans(df, centers = 8, nstart = 50)

f1 <- fviz_cluster(cluster, data = df, geom = "point") +
    geom_text(aes(label = songs$Colour))

ggplotly(f1)
## LDA
da <- songs %>%
    select(c("Colour", where(is.numeric))) %>%
    select(!c("track_number", "mode", "time_signature", `Night #`, "Order"))
da$Colour <- as.factor(da$Colour)

prop.table(table(songs$Colour))

lda  <-  lda(Colour ~ danceability + acousticness + energy + key +
                 loudness + speechiness + instrumentalness +
           liveness + valence + tempo + duration_ms, data = da)
lda
ghat <- predict(lda)$class
table(prediced = ghat, observed = songs$Colour)
mean(ghat != songs$Colour) ## !

partimat(Colour ~ danceability + acousticness + energy + key +
             loudness + speechiness + instrumentalness +
             liveness + valence + tempo + duration_ms, method = "lda", data = da)

## plot

ggplot(surprise, aes(x = Date, y = Colour)) +
    geom_point(size = 5, col = "grey") +
    geom_vline(xintercept = surprise$Date[!is.na(surprise$"Special Annoucement")]) +
    annotate("text", x = unique(surprise$Date[!is.na(surprise$`Special Annoucement`)]),
             y = 6, label = unique(surprise$`Special Annoucement`[!is.na(surprise$`Special Annoucement`)]),
             angle = 90, vjust = 1.5)+
    annotate("text", x = surprise$Date[!is.na(surprise$Notes)],
             y = 6, label = surprise$Notes[!is.na(surprise$Notes)],
             angle = 90, vjust = 1.5) +
    geom_vline(xintercept = surprise$Date[!is.na(surprise$Notes)])

