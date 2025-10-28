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

colorPaletteGroups <- c(
  "yellows" = "#FFD700",
  "blues" = "#0000FF",
  "greens" = "#008000",
  "blacks" = "#000000",
  "black and white" = "#C0C0C0",
  "whites" = "#FFFFFF",
  "reds" = "#FF0000",
  "colorful" = "#FF4500",
  "purples" = "#8A2BE2"
)


colourGroupPalette <- c(
  "blues" = "#5A8BC8",      
  "reds" = "#D17A8A",       
  "purples" = "#A675C8",  
  "greens" = "#7AB87A",     
  "yellows" = "#E6C470",    
  "colourful" = "#D19A70"
)
  