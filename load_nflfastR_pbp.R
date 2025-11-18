# Quick helper to load and summarize nflfastR play-by-play CSV data.

library(data.table)

# Change this if your CSV is elsewhere.
csv_path <- "nflfastR_pbp_2023.csv"

# Columns to pull; trim/add as needed for your analysis.
cols <- c(
  "game_id", "play_id", "week", "qtr", "down", "ydstogo", "yardline_100",
  "posteam", "defteam", "pass", "rush", "air_yards", "yards_gained",
  "epa", "cpoe", "wp", "wpa", "score_differential"
)

# Load data
pbp <- fread(csv_path, select = cols)

# Basic overall snapshot
cat(sprintf("Rows: %s  Cols: %s\n", nrow(pbp), ncol(pbp)))
cat(sprintf("Unique games: %s  Weeks: %s-%s\n",
            uniqueN(pbp$game_id),
            min(pbp$week, na.rm = TRUE),
            max(pbp$week, na.rm = TRUE)))

# Play-type counts
cat("\nPlay-type counts (pass/rush indicators):\n")
print(pbp[, .N, .(pass, rush)][order(-N)])

# Quick numeric summaries
cat("\nNumeric means/sd:\n")
print(pbp[, .(
  yards_gained_mean = mean(yards_gained, na.rm = TRUE),
  yards_gained_sd   = sd(yards_gained, na.rm = TRUE),
  air_yards_mean    = mean(air_yards, na.rm = TRUE),
  epa_mean          = mean(epa, na.rm = TRUE),
  epa_sd            = sd(epa, na.rm = TRUE),
  cpoe_mean         = mean(cpoe, na.rm = TRUE),
  wp_mean           = mean(wp, na.rm = TRUE),
  wpa_mean          = mean(wpa, na.rm = TRUE),
  score_diff_mean   = mean(score_differential, na.rm = TRUE)
)])

# Example: team-game EPA per play
team_game <- pbp[, .(
  plays = .N,
  pass_rate = mean(pass == 1, na.rm = TRUE),
  epa_per_play = mean(epa, na.rm = TRUE)
), by = .(game_id, posteam, defteam, week)]

cat("\nTeam-game sample (first 10 rows):\n")
print(head(team_game, 10))

# Write out a smaller derived table if desired
# fwrite(team_game, "team_game_epa_2023.csv")
