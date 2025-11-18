# Cleans nflfastR play-by-play CSV to keep only offensive plays (pass/rush),
# remove penalties/kneels/spikes/no-plays, and write a trimmed clean file.

library(data.table)

csv_path_in  <- "nflfastR_pbp_2023.csv"
csv_path_out <- "nflfastR_pbp_2023_clean.csv"

# Columns to read (add more if you need them for modeling)
cols <- c(
  "game_id", "play_id", "week", "qtr", "down", "ydstogo", "yardline_100",
  "posteam", "defteam", "pass", "rush",
  "air_yards", "yards_gained", "epa", "cpoe", "wp", "wpa", "score_differential",
  "play_type", "penalty", "qb_kneel", "qb_spike"
)

pbp <- fread(csv_path_in, select = cols, showProgress = TRUE)

# If any filter columns are missing, create them as 0 to avoid errors.
for (col in c("penalty", "qb_kneel", "qb_spike")) {
  if (!col %in% names(pbp)) pbp[, (col) := 0L]
}

baseline_rows <- nrow(pbp)

clean <- pbp[
  !is.na(posteam) & !is.na(defteam) &                # must have both teams
    (pass == 1 | rush == 1) &                       # keep only pass/run plays
    (penalty == 0 | is.na(penalty)) &               # drop penalized plays
    qb_kneel == 0 & qb_spike == 0 &                 # drop kneels/spikes
    play_type != "no_play" & !is.na(epa) &          # drop no-plays, missing EPA
    !is.na(yards_gained)                            # drop missing yardage
]

kept <- nrow(clean)

fwrite(clean, csv_path_out)

cat(sprintf("Baseline rows: %s\nKept rows: %s (%.1f%%)\nOutput: %s\n",
            baseline_rows, kept, kept / baseline_rows * 100, csv_path_out))

# Quick check of play-type counts in cleaned data
cat("\nPlay-type counts in cleaned data:\n")
print(clean[, .N, .(pass, rush)][order(-N)])
