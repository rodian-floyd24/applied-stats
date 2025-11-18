# Filters nflfastR player-week stats to quarterbacks and exports weekly and season-level summaries.

library(data.table)
library(nflfastR)

season <- 2023
raw_csv <- sprintf("nflfastR_player_stats_%s.csv", season)

# Load player-week stats from disk if present; otherwise fetch from nflfastR
if (file.exists(raw_csv)) {
  ps <- fread(raw_csv)
} else {
  ps <- as.data.table(load_player_stats(season))
  fwrite(ps, raw_csv)
}

setDT(ps)

# Keep quarterbacks only
qb_weekly <- ps[position == "QB"]

# Select commonly used QB columns
keep_cols <- c(
  "season", "week", "player_id", "player_display_name", "team", "opponent_team",
  "completions", "attempts", "passing_yards", "passing_tds", "passing_interceptions",
  "sacks_suffered", "sack_yards_lost", "passing_air_yards", "passing_yards_after_catch",
  "passing_first_downs", "passing_epa", "passing_cpoe",
  "carries", "rushing_yards", "rushing_tds", "rushing_epa",
  "fantasy_points", "fantasy_points_ppr"
)

qb_weekly <- qb_weekly[, ..keep_cols]

# Season-level aggregates per QB
qb_season <- qb_weekly[
  order(week),
  .(
    team                 = tail(na.omit(team), 1L), # team from last week played
    games                = uniqueN(week),
    attempts             = sum(attempts, na.rm = TRUE),
    completions          = sum(completions, na.rm = TRUE),
    passing_yards        = sum(passing_yards, na.rm = TRUE),
    passing_tds          = sum(passing_tds, na.rm = TRUE),
    passing_interceptions= sum(passing_interceptions, na.rm = TRUE),
    sacks_suffered       = sum(sacks_suffered, na.rm = TRUE),
    sack_yards_lost      = sum(sack_yards_lost, na.rm = TRUE),
    passing_air_yards    = sum(passing_air_yards, na.rm = TRUE),
    passing_yac          = sum(passing_yards_after_catch, na.rm = TRUE),
    passing_first_downs  = sum(passing_first_downs, na.rm = TRUE),
    passing_epa_total    = sum(passing_epa, na.rm = TRUE),
    passing_epa_per_drop = mean(passing_epa, na.rm = TRUE),
    passing_cpoe_avg     = mean(passing_cpoe, na.rm = TRUE),
    carries              = sum(carries, na.rm = TRUE),
    rushing_yards        = sum(rushing_yards, na.rm = TRUE),
    rushing_tds          = sum(rushing_tds, na.rm = TRUE),
    rushing_epa_total    = sum(rushing_epa, na.rm = TRUE),
    fantasy_points       = sum(fantasy_points, na.rm = TRUE),
    fantasy_points_ppr   = sum(fantasy_points_ppr, na.rm = TRUE)
  ),
  by = .(season, player_id, player_display_name)
]

# Save outputs
qb_weekly_csv <- sprintf("qb_weekly_%s.csv", season)
qb_season_csv <- sprintf("qb_season_%s.csv", season)

fwrite(qb_weekly, qb_weekly_csv)
fwrite(qb_season, qb_season_csv)

cat(sprintf("QB weekly rows: %s  Cols: %s  -> %s\n",
            nrow(qb_weekly), ncol(qb_weekly), qb_weekly_csv))
cat(sprintf("QB season rows: %s  Cols: %s  -> %s\n",
            nrow(qb_season), ncol(qb_season), qb_season_csv))

# Preview a few rows
cat("\nQB weekly preview:\n")
print(head(qb_weekly, 5))

cat("\nQB season preview:\n")
print(head(qb_season[order(-passing_yards)], 5))
