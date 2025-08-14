# set seed
set.seed(42)

# load required libraries
library(tidyverse)

# read in data
odds = read_csv("nba_lottery_odds.csv")

# normalize within each year so they sum to 1
odds = odds %>%
  group_by(Year) %>%
  mutate(Prob = Chances / sum(Chances)) %>%
  ungroup()

# filter to only the team that got pick 1 each year
first_picks = odds %>%
  filter(Pick == 1) %>%
  mutate(Surprise = -log(Prob))

# define a function for entropy
entropy = function(prob_list) {
  -sum(prob_list * log(prob_list))
}

# compute entropy per year
entropy_by_year = odds %>%
  group_by(Year) %>%
  summarise(Entropy = entropy(Prob)) %>%
  ungroup()

# merge entropy and surprise dataframes
merged_data = inner_join(entropy_by_year, 
                         first_picks %>% select(Year, Surprise), 
                         by = "Year") %>%
  select(Year, Entropy, Surprise)

# calculate totals
total_entropy = sum(merged_data$Entropy)
total_surprise = sum(merged_data$Surprise)

cat(sprintf("Total Entropy: %f\n", total_entropy))
cat(sprintf("Total Surprise: %f\n", total_surprise))

# plot entropy and surprise over time
ggplot(merged_data, aes(x = Year)) +
  geom_line(aes(y = Entropy, color = "Entropy")) +
  geom_line(aes(y = Surprise, color = "Surprise")) +
  labs(title = "Entropy and Surprise Over Time",
       x = "Year", y = "Value", color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

# simulation using tidyverse approach
n_samples = 1000000

# use purrr to simulate across all years
surprise_simulations = odds %>%
  group_by(Year) %>%
  group_map(~ {
    probs = .x$Prob
    sampled_indices = sample(length(probs), size = n_samples, replace = TRUE, prob = probs)
    sampled_probs = probs[sampled_indices]
    -log(sampled_probs)
  }) %>%
  set_names(unique(odds$Year))

# convert to tibble using tidyverse
surprise_df = map_dfr(surprise_simulations, ~ tibble(Surprise = .x), .id = "Year") %>%
  mutate(Year = as.numeric(Year))

# create matrix and calculate row sums using tidyverse
sim_total_surprises_array = surprise_simulations %>%
  bind_cols() %>%
  as.matrix()

sim_total_surprises_sum = sim_total_surprises_array %>%
  rowSums()

cat(sprintf("Average simulated surprise: %f\n", mean(sim_total_surprises_sum)))
cat(sprintf("Total Entropy: %f\n", total_entropy))
cat(sprintf("Total Surprise: %f\n", total_surprise))

# calculate proportion of simulations with surprise >= actual surprise
right_tail_prob = mean(sim_total_surprises_sum >= total_surprise)
right_tail_pct = right_tail_prob * 100

# create histogram with shading using tidyverse
tibble(surprise = sim_total_surprises_sum, greater = (surprise > total_surprise)) %>%
  ggplot(aes(x = surprise)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "lightcoral", alpha = 0.7) +
  geom_vline(xintercept = total_surprise, color = "navy", linetype = "dashed", size = 1) +
  annotate("text", x = total_surprise, y = Inf, 
           label = sprintf("Right-tail Prob: %.2f%%", right_tail_pct),
           vjust = 2, hjust = -0.05, color = "navy", size = 4) +
  labs(title = "Distribution of Total Surprise Across All Simulated Lotteries",
       x = "Total Surprise (Sum of -ln(prob))", y = "Density") +
  theme_minimal()

cat(sprintf("P-value for actual outcome: %.6g\n", right_tail_prob))