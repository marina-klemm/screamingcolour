require(tidyverse)
require(markovchain)
require(msm)
surpriseSongsDressColours <- "../raw_data/surprise_songs.xlsx"
surpriseSongsDressColours <- readxl::read_excel(surpriseSongsDressColours, sheet = "List")
surpriseSongsDressColours$Date <- as.Date(surpriseSongsDressColours$Date)

oneRowPerConcert <- surpriseSongsDressColours %>%
    group_by(Date) %>%
    arrange(Date, Order) %>% 
    slice(1) %>%
    ungroup()

data <- data.frame(chain = oneRowPerConcert$DressName,
                   leg = ifelse(oneRowPerConcert$Legs %in% c("First leg", "Latin America", "Asia-Oceania"),
                                "First",
                         ifelse(oneRowPerConcert$Legs == "European leg", "Europe", "Final")))


## Note that The Markov property is that the future state of a system depends only on its current state and is independent of its past history
verifyMarkovProperty(data$chain) ## reject Markov property p-value 0.00912 overall?
verifyMarkovProperty(data$chain[data$leg == "First"]) ## not really evidence against the Markov property p-value 0.0955
verifyMarkovProperty(data$chain[data$leg == "Europe"]) ## no evidence against the Markov property p-value 0.834 (~likely a Markov chain)
verifyMarkovProperty(data$chain[data$leg == "Final"]) ## no evidence against the Markov property p-value 0.393  (~likely a Markov chain)

first <- createSequenceMatrix(data$chain[data$leg == "First"], toRowProbs = TRUE) |> as("markovchain")
first |> plot(edge.arrow.size = 0.2)
steadyStates(first) ## estimated long term proportion in states
mid <- createSequenceMatrix(data$chain[data$leg == "Europe"], toRowProbs = TRUE) |> as("markovchain")
mid |> plot(edge.arrow.size = 0.2)
steadyStates(mid)
end <- createSequenceMatrix(data$chain[data$leg == "Final"], toRowProbs = TRUE) |> as("markovchain")
end |> plot(edge.arrow.size = 0.2)
steadyStates(end)
mc_fit = markovchainFit(data = data$chain[data$leg == "First"], method = "mle")
mc_fit
states <- mc_fit$estimate@states
transition_data <- expand.grid(From = states, To = states)
transition_data$lower <- as.vector(mc_fit$lowerEndpointMatrix)
transition_data$upper <- as.vector(mc_fit$upperEndpointMatrix)
transition_data$mean  <- as.vector(mc_fit$estimate@transitionMatrix)
transition_data$Label <- paste(transition_data$From, "→", transition_data$To)

# Add a position index for plotting on x-axis
transition_data <- transition_data %>% 
  arrange(factor(From, levels = states), factor(To, levels = states)) %>%
  mutate(pos = row_number())

# Plot
ggplot(transition_data, aes(x = pos)) +
    geom_segment(aes(y = lower, yend = upper, xend = pos), size = 1.2, color = "blue") +
    geom_point(aes(y = mean), color = "red", size = 2.5) +
  scale_x_continuous(breaks = transition_data$pos, labels = transition_data$Label) +
  labs(x = "Transition", y = "95% Confidence Interval", 
       title = "Transition Probabilities with 95% Confidence Intervals") +
  theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../plots/europe_leg.png")
## following https://bookdown.org/kevin_davisross/applied-stochastic-processes/notes-mc-inference.html
## first order stuff
x_current <- data$chain[data$leg == "Europe"]
x = tibble(x_current,
           x_next = lead(x_current, 1),
           x_previous = lag(x_current, 1))

x |> head()
x_summary_mc = x |>
  group_by(x_current, x_previous) |>
  count(x_next, name = "count") |>
filter(!is.na(x_next) & !is.na(x_previous))
x_summary_mc
x_summary_mc |>
  ggplot(aes(x = x_previous,
             y = count,
             fill = x_next)) +
  geom_bar(position = "fill",
           stat = "identity") +
  scale_fill_viridis_d() +
  labs(y = "Conditional probability given previous state") +
      facet_wrap(~x_current, labeller = label_both)

verifyMarkovProperty(x_current) ## no reason to reject Markov (first order)
ggsave("../plots/europe_firstorder.png")
## second order stuff
x_and_y = x |>
  filter(!is.na(x_next) & !is.na(x_previous)) |>
  unite("y_current", c("x_previous", "x_current"), remove = FALSE) |>
  mutate(y_next = lead(y_current, 1),
         y_previous = lag(y_current, 1))

x_and_y |> select(x_previous, x_current, y_current, y_previous, y_next) |> head()

y_summary_mc = x_and_y |>
  group_by(y_current, y_previous) |>
  count(y_next, name = "count") |>
  filter(!is.na(y_next) & !is.na(y_previous))

y_summary_mc

y_summary_mc |>
  ggplot(aes(x = y_previous,
             y = count,
             fill = y_next)) +
  geom_bar(position = "fill",
           stat = "identity") +
  scale_fill_viridis_d() +
  labs(y = "Conditional probability given previous state") +
      facet_wrap(~y_current, labeller = label_both)

verifyMarkovProperty(x_and_y$y_current) ## no evidence to reject Markov
