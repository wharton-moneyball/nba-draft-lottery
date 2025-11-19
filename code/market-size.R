# Market size analysis of No. 1 overall picks by tier

# load libraries
library(tidyverse)

# 0) Data: ensure `odds` exists and has Prob normalized within year
if (!exists("odds")) {
  odds <- readr::read_csv("data/nba_lottery_odds.csv", show_col_types = FALSE)
}

if (!("Prob" %in% names(odds))) {
  odds <- odds %>%
    group_by(Year) %>%
    mutate(Prob = Chances / sum(Chances)) %>%
    ungroup()
}

# 0) Add a market-size mapping (tiers fixed across years)
large_market_teams <- c(
  "New York Knicks", "Brooklyn Nets", "Los Angeles Lakers", "LA Clippers",
  "Chicago Bulls", "Philadelphia 76ers", "Dallas Mavericks", "Toronto Raptors",
  "Golden State Warriors", "Atlanta Hawks", "Houston Rockets", "Washington Wizards",
  "Boston Celtics", "Phoenix Suns"
)

medium_market_teams <- c(
  "Miami Heat", "Cleveland Cavaliers", "Denver Nuggets", "Detroit Pistons",
  "Minnesota Timberwolves", "Portland Trail Blazers", "Indiana Pacers",
  "Charlotte Hornets", "Orlando Magic", "San Antonio Spurs"
)

# canonical map covering every franchise present in `odds$Team`
market_map <- odds %>%
  distinct(Team) %>%
  mutate(
    Tier = case_when(
      Team %in% large_market_teams ~ "Large",
      Team %in% medium_market_teams ~ "Medium",
      TRUE ~ "Small"
    )
  )

odds_tier <- odds %>%
  left_join(market_map, by = "Team") %>%
  mutate(Tier = factor(Tier, levels = c("Large", "Medium", "Small")))

# 1) q_{g,y}: total tier odds by year
q_by_year_tier <- odds_tier %>%
  group_by(Year, Tier) %>%
  summarise(q = sum(Prob), .groups = "drop")

# (optional) Expected counts E_g for reporting
E_by_tier <- q_by_year_tier %>%
  group_by(Tier) %>%
  summarise(Expected = sum(q), .groups = "drop")

# 2) Observed tier wins O_g^{obs}
winners_obs <- odds_tier %>% filter(Pick == 1)
O_obs <- winners_obs %>%
  count(Tier, name = "Observed") %>%
  tidyr::complete(Tier = levels(odds_tier$Tier), fill = list(Observed = 0)) %>%
  arrange(Tier)

# 3) Monte Carlo: redraw winners year-by-year using published team odds
set.seed(42)
nsim <- 100000

year_slices <- split(odds_tier, odds_tier$Year)

sim_counts <- replicate(nsim, {
  winners_tiers <- vapply(year_slices, function(df) {
    as.character(df$Tier[sample.int(nrow(df), size = 1, prob = df$Prob)])
  }, character(1))
  as.integer(table(factor(winners_tiers, levels = levels(odds_tier$Tier))))
})
rownames(sim_counts) <- levels(odds_tier$Tier)

# 4) p-values (over- and under-performance), plus simulation CIs
O_vec <- O_obs$Observed
p_over  <- rowMeans(sweep(sim_counts, 1, O_vec, `>=`))
p_under <- rowMeans(sweep(sim_counts, 1, O_vec, `<=`))
sim_ci  <- apply(sim_counts, 1, stats::quantile, probs = c(.025, .5, .975))

results_tier <- dplyr::tibble(
  Tier      = levels(odds_tier$Tier),
  Observed  = O_vec,
  Expected  = E_by_tier$Expected[match(levels(odds_tier$Tier), E_by_tier$Tier)],
  `Sim p_over`  = p_over,
  `Sim p_under` = p_under,
  `Sim 2.5%`    = sim_ci[1,],
  `Sim 50%`     = sim_ci[2,],
  `Sim 97.5%`   = sim_ci[3,]
)

print(results_tier)