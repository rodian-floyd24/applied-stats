# Fetches nflfastR player-week stats for 2023, writes a CSV, and prints a quick preview.

library(nflfastR)
library(data.table)

season <- 2023
out_csv <- sprintf("nflfastR_player_stats_%s.csv", season)

# Load and coerce to data.table so `.` works
ps <- as.data.table(load_player_stats(season))

fwrite(ps, out_csv)

cat(sprintf("Player-week rows: %s  Cols: %s\nOutput: %s\n",
            nrow(ps), ncol(ps), out_csv))

# Preview key columns (first 10 rows)
print(head(ps[, .(
  season, week, player_id, player_display_name,
  position, team, opponent_team, fantasy_points_ppr,
  passing_yards, passing_tds, rushing_yards, rushing_tds,
  receiving_yards, receiving_tds
)], 10))
