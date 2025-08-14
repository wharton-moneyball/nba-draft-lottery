# Load libraries
library(tidyverse)

# Read in the data
odds <- read_csv("./moneyball/nba_lottery_odds.csv")

# Define market groups
big_market_teams <- c(
  "New York Knicks", "Brooklyn Nets",
  "Los Angeles Lakers", "LA Clippers",
  "Chicago Bulls", "Philadelphia 76ers",
  "Dallas Mavericks", "Toronto Raptors",
  "Golden State Warriors", "Atlanta Hawks",
  "Houston Rockets", "Washington Wizards",
  "Boston Celtics", "Phoenix Suns"
)

medium_market_teams <- c(
  "Miami Heat", "Cleveland Cavaliers", "Denver Nuggets", "Detroit Pistons",
  "Minnesota Timberwolves", "Portland Trail Blazers", "Indiana Pacers",
  "Charlotte Hornets", "Orlando Magic", "San Antonio Spurs"
)

# Infer small market teams as teams not in big or medium market groups
small_market_teams <- odds %>%
  filter(!(Team %in% c(big_market_teams, medium_market_teams))) %>%
  distinct(Team) %>%
  pull(Team)

# Function for permutation test and plot
plot_perm_test <- function(market_group, group_label) {
  odds_labeled <- odds %>%
    mutate(MarketGroup = if_else(Team %in% market_group, group_label, paste0("Not", group_label)))
  
  actual_winners <- odds_labeled %>% filter(Pick == 1)
  actual_count <- actual_winners %>% filter(MarketGroup == group_label) %>% nrow()
  
  set.seed(123)
  n_sim <- 10000
  simulated_counts <- replicate(n_sim, {
    shuffled_labels <- sample(odds_labeled$MarketGroup)
    odds_shuffled <- odds_labeled %>% mutate(ShuffledGroup = shuffled_labels)
    winners_shuffled <- odds_shuffled %>% filter(Pick == 1)
    sum(winners_shuffled$ShuffledGroup == group_label)
  })
  
  print(simulated_counts)
  
  p_val <- mean(simulated_counts >= actual_count)
  
  # Plot
  tibble(simulated = simulated_counts) %>%
    ggplot(aes(x = simulated)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "white") +
    geom_vline(xintercept = actual_count, color = "red", linetype = "dashed", size = 1) +
    annotate("text", x = actual_count + 0.5, y = Inf,
             label = paste("Actual", group_label, "Picks =", actual_count),
             hjust = 0, vjust = 2, color = "red", size = 4) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = paste("Permutation Test:", group_label, "vs Others"),
      subtitle = sprintf("P-value = %.4f", p_val),
      x = paste("# of #1 Picks by", group_label, "Teams"),
      y = "Percent of Simulations"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
}

# Plot 1: Medium vs Not Medium
plot_perm_test(medium_market_teams, "Medium Market")


