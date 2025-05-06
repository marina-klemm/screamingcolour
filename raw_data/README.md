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
    ## # A tibble: 241 × 40
    ##    album_name     ep    album_release       track_number track_name sentiment_MK
    ##    <chr>          <lgl> <dttm>                     <dbl> <chr>      <chr>       
    ##  1 evermore       FALSE 2020-12-11 00:00:00           13 marjorie   longing     
    ##  2 Midnights      FALSE 2022-10-21 00:00:00            4 Snow On T… peaceful    
    ##  3 Midnights      FALSE 2022-10-21 00:00:00           22 Snow On T… peaceful    
    ##  4 THE TORTURED … FALSE 2024-04-19 00:00:00           17 The Black… sad         
    ##  5 folklore       FALSE 2020-07-24 00:00:00            2 cardigan   melancholic 
    ##  6 THE TORTURED … FALSE 2024-04-19 00:00:00           12 loml       sad         
    ##  7 Lover          FALSE 2019-08-23 00:00:00           18 Daylight   hopeful     
    ##  8 Speak Now (Ta… FALSE 2023-07-07 00:00:00           22 Timeless … nostalgic   
    ##  9 1989 (Taylor'… FALSE 2023-10-27 00:00:00            4 Out Of Th… anxious     
    ## 10 reputation     FALSE 2017-11-10 00:00:00            5 Delicate   vulnerable  
    ## # ℹ 231 more rows
    ## # ℹ 34 more variables: message_MK <chr>, keywords_MK <chr>, Column1 <dbl>,
    ## #   muse_MK <chr>, colour_lyric_MK <chr>, colour_meaningMK <chr>,
    ## #   colour_MK <chr>, notes_MK <chr>, secret_message_MK <chr>, artist <chr>,
    ## #   featuring <chr>, bonus_track <lgl>, promotional_release <dttm>,
    ## #   single_release <dttm>, track_release <dttm>, danceability <dbl>,
    ## #   energy <dbl>, key <dbl>, loudness <dbl>, mode <dbl>, speechiness <dbl>, …
    ## 
    ## [[2]]
    ## # A tibble: 240 × 105
    ##    album_name ep    album_release       track_number track_name artist featuring
    ##    <chr>      <lgl> <dttm>                     <dbl> <chr>      <chr>  <chr>    
    ##  1 Red        FALSE 2021-11-12 00:00:00            6 "22"       Taylo… <NA>     
    ##  2 1989       FALSE 2023-10-27 00:00:00           17 "\"Slut!\… Taylo… <NA>     
    ##  3 reputation FALSE 2017-11-10 00:00:00            1 "...Ready… Taylo… <NA>     
    ##  4 Taylor Sw… FALSE 2006-10-24 00:00:00           14 "A Perfec… Taylo… <NA>     
    ##  5 Taylor Sw… FALSE 2006-10-24 00:00:00            4 "A Place … Taylo… <NA>     
    ##  6 Lover      FALSE 2019-08-23 00:00:00           15 "Afterglo… Taylo… <NA>     
    ##  7 Red        FALSE 2021-11-12 00:00:00            5 "All Too … Taylo… <NA>     
    ##  8 Red        FALSE 2021-11-12 00:00:00           30 "All Too … Taylo… <NA>     
    ##  9 1989       FALSE 2023-10-27 00:00:00            5 "All You … Taylo… <NA>     
    ## 10 Midnights  FALSE 2022-10-21 00:00:00            3 "Anti-Her… Taylo… <NA>     
    ## # ℹ 230 more rows
    ## # ℹ 98 more variables: bonus_track <lgl>, promotional_release <dttm>,
    ## #   single_release <dttm>, track_release <dttm>, danceability <dbl>,
    ## #   energy <dbl>, key <dbl>, loudness <dbl>, mode <dbl>, speechiness <dbl>,
    ## #   acousticness <dbl>, instrumentalness <dbl>, liveness <dbl>, valence <dbl>,
    ## #   tempo <dbl>, time_signature <dbl>, duration_ms <dbl>, explicit <lgl>,
    ## #   key_name <chr>, mode_name <chr>, key_mode <chr>, lyrics <chr>, …
    ## 
    ## [[3]]
    ## # A tibble: 443 × 26
    ##    `Song title`         Mashups Mashup Mashup2 Guest City  State Country Stadium
    ##    <chr>                <chr>   <chr>  <chr>   <chr> <chr> <chr> <chr>   <chr>  
    ##  1 mirrorball           None    <NA>   <NA>    <NA>  Glen… Ariz… US      State …
    ##  2 Tim McGraw           None    <NA>   <NA>    <NA>  Glen… Ariz… US      State …
    ##  3 State Of Grace       None    <NA>   <NA>    <NA>  Glen… Ariz… US      State …
    ##  4 this is me trying    None    <NA>   <NA>    <NA>  Glen… Ariz… US      State …
    ##  5 Our Song             None    <NA>   <NA>    <NA>  Las … Neva… US      Allegi…
    ##  6 Snow On The Beach    None    <NA>   <NA>    <NA>  Las … Neva… US      Allegi…
    ##  7 cowboy like me       None    <NA>   <NA>    Marc… Las … Neva… US      Allegi…
    ##  8 White Horse          None    <NA>   <NA>    <NA>  Las … Neva… US      Allegi…
    ##  9 Ours                 None    <NA>   <NA>    <NA>  Arli… Texas US      AT&T   
    ## 10 Sad Beautiful Tragic None    <NA>   <NA>    <NA>  Arli… Texas US      AT&T   
    ## # ℹ 433 more rows
    ## # ℹ 17 more variables: Date <dttm>, DressName <chr>, Legs <chr>,
    ## #   Relationship <chr>, Start <dttm>, End <dttm>, Colour1 <chr>,
    ## #   ColourHex1 <chr>, ColourRGB1 <chr>, Colour2 <chr>, ColourHex2 <chr>,
    ## #   ColourRGB2 <chr>, `Night #` <dbl>, Order <dbl>, Instrument <chr>,
    ## #   `Special Annoucement` <chr>, Notes <chr>
