require(cowplot)
require(ggplot2)
require(ggimage)
require(gridExtra)
require(extrafont)


# Screaming colour Plot ----------------------------------------------------



# First, find the first date for each dress
dress_first_appearance <- oneRowPerConcert %>%
  group_by(DressName) %>%
  summarize(FirstAppearance = min(Date)) %>%
  arrange((FirstAppearance))

# Convert DressName to a factor with ordered levels
oneRowPerConcert$DressName <- factor(oneRowPerConcert$DressName, 
                                     levels = dress_first_appearance$DressName)

max_dress_level <- length(unique(oneRowPerConcert$DressName))



# Create the main timeline plot
main_plot <- ggplot(oneRowPerConcert, aes(x = Date, y = DressName, colour = ColourHex1)) +
  geom_point(size = 4, alpha = 1) +
  scale_colour_identity() +
  theme_minimal() +
  labs(x = "", y = "") +
  geom_rect(aes(xmin = as.Date("2023-08-28"), xmax = as.Date("2023-11-08"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, colour = NA) +
  geom_rect(aes(xmin = as.Date("2023-11-27"), xmax = as.Date("2024-02-06"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, colour = NA) +  
  geom_rect(aes(xmin = as.Date("2024-03-10"), xmax = as.Date("2024-05-08"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, colour = NA) +
  geom_rect(aes(xmin = as.Date("2024-08-21"), xmax = as.Date("2024-10-17"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01, colour = NA) +
  # Vertical lines for the key events
  geom_vline(xintercept = as.Date("2024-05-09"), linetype = "dashed", colour = "black") +
  geom_vline(xintercept = as.Date("2023-03-17"), linetype = "dashed", colour = "black") +
  geom_vline(xintercept = as.Date("2024-10-18"), linetype = "dashed", colour = "black") +
  geom_vline(xintercept = as.Date("2023-08-24"), linetype = "dashed", colour = "black") +
  geom_vline(xintercept = as.Date("2024-02-07"), linetype = "dashed", colour = "black") +
  geom_vline(xintercept = as.Date("2024-04-16"), linetype = "solid", colour = "darkgray", size=2) +
  geom_vline(xintercept = as.Date("2023-07-07"), linetype = "solid", colour = "purple", size=2) +
  geom_vline(xintercept = as.Date("2023-10-27"), linetype = "solid", colour = "blue", size=2) +

  annotate("text", x = as.Date("2024-05-09"), y = max_dress_level, 
           label = "Europeᵃ", colour = "black", angle = -90, vjust = -0.5, family = "Calibri", size = 6, lineheight = 0.6) +
  annotate("text", x = as.Date("2023-03-17"), y = max_dress_level, 
           label = "United\nStatesᵃ", colour = "black", angle = -90, vjust = -0.2, family = "Calibri", size = 6, lineheight = 0.6) +
  annotate("text", x = as.Date("2024-10-18"), y = max_dress_level, 
           label = "North\nAmericaᵃ", colour = "black", angle = -90, vjust = -0.2, family = "Calibri", size = 6, lineheight = 0.6) +
  annotate("text", x = as.Date("2023-08-24"), y = max_dress_level, 
           label = "Latin\nAmericaᵃ", colour = "black", angle = -90, vjust = -0.2, family = "Calibri", size = 6, lineheight = 0.6) +
  annotate("text", x = as.Date("2024-02-07"), y = max_dress_level, 
           label = "Asia/\nOceaniaᵃ", colour = "black", angle = -90, vjust = -0.2, family = "Calibri", size = 6, lineheight = 0.6) +
  annotate("text", x = as.Date("2024-04-16"), y = max_dress_level, 
           label = "TTPDᵇ", colour = "darkgray", angle = -90, vjust = -0.5, family = "Calibri", size = 6, lineheight = 0.6,
           fontface = "bold") +
  annotate("text", x = as.Date("2023-07-07"), y = max_dress_level, 
           label = "Speak Now\nTVᵇ", colour = "purple", angle = -90, vjust = -0.2, family = "Calibri", size = 6, lineheight = 0.6) +
  annotate("text", x = as.Date("2023-10-27"), y = max_dress_level, 
           label = "1989\nTVᵇ", colour = "blue", angle = -90, vjust = -0.2, family = "Calibri", size = 6, lineheight = 0.6) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust=0.5, size = 14, margin = margin(b = 20), face = "bold"),
    plot.margin = margin(t = -7, r = 0, b = 10, l = 0),
    text = element_text(colour = "black", family = "Calibri", size = 16)
  )


main_plot

# Create dress count plot (to go on the right side)
# Ensure the dress order matches exactly with the main plot
dress_levels <- levels(factor(oneRowPerConcert$DressName))
oneRowPerConcertWithImages$DressName <- factor(oneRowPerConcertWithImages$DressName, levels = dress_levels)

count_plot <- ggplot(oneRowPerConcertWithImages, aes(x = n, y = DressName, fill = DressName)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_image(
    aes(image = imagePath, x = n),  
    size = 0.09,                    
    nudge_x = 0,
    by = "height"                    
  ) +
  geom_text(
    aes(x = n + 3, label = paste0(n, " (", round(percentage, 1), "%) - ", DressName)),  # Added dress name
    hjust = 0,
    nudge_x = 3,
    colour = "black",
    size = 6,
    family = "Calibri"
  ) +
  scale_fill_manual(values = colourPaletteDresses) +
  theme_minimal() +
  labs(title = "", x = "", y = "") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),  # Remove any remaining ticks
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.position = "none",
    plot.margin = margin(t = -7, r = 0, b = 10, l = 0),
    text = element_text(colour = "black", family = "Calibri", size = 16)
  ) +
  xlim(0, 65)   # Increased limit to accommodate longer text with dress names

# Merge the plots using cowplot with adjusted widths
merged_plot <- plot_grid(
  count_plot, main_plot,
  ncol = 2,
  align = "h",
  axis = "tb",
  rel_widths = c(2, 3)  # Increased count plot width to accommodate longer text
)


merged_plot



