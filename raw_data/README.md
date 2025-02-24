## Data overview

    .
    ├── album_info_metadata_neutral.xlsx
    ├── album_info_metadata.xlsx
    ├── README.md
    └── surprise_songs.xlsx

    0 directories, 4 files

    library(readxl)
    files <- list.files(pattern = ".xlsx")
    ## key info on first sheet of all files
    lapply(files, read_xlsx)

    ## [[1]]
    ## # A tibble: 241 × 39
    ##    album_name     ep    album_release       track_number track_name sentiment_MK
    ##    <chr>          <lgl> <dttm>                     <dbl> <chr>      <chr>       
    ##  1 Taylor Swift   FALSE 2006-10-24 00:00:00            2 Picture T… angry       
    ##  2 Red (Taylor's… FALSE 2021-11-12 00:00:00           15 Starlight… joyful      
    ##  3 Taylor Swift   FALSE 2006-10-24 00:00:00            5 Cold As Y… desperate   
    ##  4 Lover          FALSE 2019-08-23 00:00:00            2 Cruel Sum… passionate  
    ##  5 Red (Taylor's… FALSE 2021-11-12 00:00:00           23 Nothing N… vulnerable  
    ##  6 THE TORTURED … FALSE 2024-04-19 00:00:00           17 The Black… sad         
    ##  7 Midnights      FALSE 2022-10-21 00:00:00           14 Hits Diff… heartbroken 
    ##  8 folklore       FALSE 2020-07-24 00:00:00           16 hoax       desperate   
    ##  9 Midnights      FALSE 2022-10-21 00:00:00           23 Karma (Re… playful     
    ## 10 evermore       FALSE 2020-12-11 00:00:00           15 evermore   optimistic  
    ## # ℹ 231 more rows
    ## # ℹ 33 more variables: message_MK <chr>, keywords_MK <chr>, muse_MK <chr>,
    ## #   colour_lyric_MK <chr>, colour_meaningMK <chr>, colour_MK <chr>,
    ## #   notes_MK <chr>, secret_message_MK <chr>, artist <chr>, featuring <chr>,
    ## #   bonus_track <lgl>, promotional_release <dttm>, single_release <dttm>,
    ## #   track_release <dttm>, danceability <dbl>, energy <dbl>, key <dbl>,
    ## #   loudness <dbl>, mode <dbl>, speechiness <dbl>, acousticness <dbl>, …
    ## 
    ## [[2]]
    ## # A tibble: 241 × 39
    ##    album_name     ep    album_release       track_number track_name sentiment_MK
    ##    <chr>          <lgl> <dttm>                     <dbl> <chr>      <chr>       
    ##  1 Lover          FALSE 2019-08-23 00:00:00            2 Cruel Sum… passionate  
    ##  2 Red (Taylor's… FALSE 2021-11-12 00:00:00           23 Nothing N… vulnerable  
    ##  3 THE TORTURED … FALSE 2024-04-19 00:00:00           17 The Black… sad         
    ##  4 Midnights      FALSE 2022-10-21 00:00:00           14 Hits Diff… heartbroken 
    ##  5 folklore       FALSE 2020-07-24 00:00:00           16 hoax       desperate   
    ##  6 Midnights      FALSE 2022-10-21 00:00:00           23 Karma (Re… playful     
    ##  7 evermore       FALSE 2020-12-11 00:00:00           15 evermore   optimistic  
    ##  8 Midnights      FALSE 2022-10-21 00:00:00            5 You're On… melancholic 
    ##  9 Midnights      FALSE 2022-10-21 00:00:00           15 You're On… melancholic 
    ## 10 THE TORTURED … FALSE 2024-04-19 00:00:00           26 The Proph… vulnerable  
    ## # ℹ 231 more rows
    ## # ℹ 33 more variables: message_MK <chr>, keywords_MK <chr>, muse_MK <chr>,
    ## #   colour_lyric_MK <chr>, colour_meaningMK <chr>, colour_MK <chr>,
    ## #   notes_MK <chr>, secret_message_MK <chr>, artist <chr>, featuring <chr>,
    ## #   bonus_track <lgl>, promotional_release <dttm>, single_release <dttm>,
    ## #   track_release <dttm>, danceability <dbl>, energy <dbl>, key <dbl>,
    ## #   loudness <dbl>, mode <dbl>, speechiness <dbl>, acousticness <dbl>, …
    ## 
    ## [[3]]
    ## # A tibble: 294 × 25
    ##    `Song title`         Mashup Mashup2 Guest         City  State Country Stadium
    ##    <chr>                <chr>  <chr>   <chr>         <chr> <chr> <chr>   <chr>  
    ##  1 Mirrorball           <NA>   <NA>    <NA>          Glen… Ariz… US      State …
    ##  2 Tim McGraw           <NA>   <NA>    <NA>          Glen… Ariz… US      State …
    ##  3 This is Me Trying    <NA>   <NA>    <NA>          Glen… Ariz… US      State …
    ##  4 State of Grace       <NA>   <NA>    <NA>          Glen… Ariz… US      State …
    ##  5 Our Song             <NA>   <NA>    <NA>          Las … Neva… US      Allegi…
    ##  6 Snow on the Beach    <NA>   <NA>    <NA>          Las … Neva… US      Allegi…
    ##  7 Cowboy Like Me       <NA>   <NA>    Marcus Mumfo… Las … Neva… US      Allegi…
    ##  8 White Horse          <NA>   <NA>    <NA>          Las … Neva… US      Allegi…
    ##  9 Sad Beautiful Tragic <NA>   <NA>    <NA>          Arli… Texas US      AT&T   
    ## 10 Ours                 <NA>   <NA>    <NA>          Arli… Texas US      AT&T   
    ## # ℹ 284 more rows
    ## # ℹ 17 more variables: Date <dttm>, DressName <chr>, Legs <chr>,
    ## #   Relationship <chr>, Start <dttm>, End <dttm>, Colour1 <chr>,
    ## #   ColourHex1 <chr>, ColourRGB1 <chr>, Colour2 <chr>, ColourHex2 <chr>,
    ## #   ColourRGB2 <chr>, `Night #` <dbl>, Order <dbl>, Instrument <chr>,
    ## #   `Special Annoucement` <chr>, Notes <chr>
