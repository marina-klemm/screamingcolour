#' Function to normalize song names
#' (i.e., remove punctuation, ignore case)
#' @param name character string of song name
normalize_song_name <- function(name) {
    if (all(is.na(name))) return(name)
    result <- name
    non_na <- !is.na(name)
    if (any(non_na)) {
        result[non_na] <- tolower(name[non_na])
        patterns_to_remove <- c(
            "\\(10 minute version\\)", "\\(taylor's version\\)",
            "\\[taylor's version\\]",
            "\\(from the vault\\)", "\\[from the vault\\]",
            "\\(acoustic version\\)",
            "\\(live\\)", "\\(remix\\)", "\\(feat\\. [^\\)]+\\)",
            "featuring [^\\(\\)]+", "ft\\. [^\\(\\)]+"
        )
        for (pattern in patterns_to_remove) {
            result[non_na] <- gsub(pattern, "", result[non_na],
                                   ignore.case = TRUE)
        }
        result[non_na] <- gsub("[[:punct:]]", "", result[non_na])
        result[non_na] <- gsub("\\s+", " ", result[non_na])
        result[non_na] <- trimws(result[non_na])
    }
    return(result)
}

#' Function to create tokenized lyrics
#' @param songData all lyrics of a song
createTokenizedLyricsF <- function(songData) {
    songsWithIdF <- songData %>%
        mutate(songId = row_number()) %>%
        select(songId, everything())
    tokenizedLyricsF <- songsWithIdF %>%
        unnest_tokens(
            output = word,      
            input = lyrics,    
            token = "words",   
            drop = FALSE       
        ) %>%
        filter(!is.na(word), word != "") %>%
        filter(!str_detect(word, "^[0-9]+$"))
    return(tokenizedLyricsF)
}
#' Function to joins bing sentiments
#' to tokenized data
#' @param tokenizedData output from \code{createTokenizedLyricsF}
calculateSentimentsF <- function(tokenizedData) {
    sentimentResults <- tokenizedData %>%
        inner_join(get_sentiments("bing"), by = c("word" = "word"), 
                   relationship = "many-to-many")
    
    return(sentimentResults)
}
#' Function to randomly sample the minimum number of keywords (per row)
#' @param data data frame with named column \code{keywords_MK} to sample
#' from in each row
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
