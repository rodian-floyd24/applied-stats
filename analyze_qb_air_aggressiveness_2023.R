# Analyze QB air-yards aggressiveness and drive outcomes for 2023.

library(data.table)
library(ggplot2)

# Try to find files in working dir, then parent (in case data weren’t copied into repo)
find_file <- function(name) {
  if (file.exists(name)) return(name)
  parent <- file.path("..", name)
  if (file.exists(parent)) return(parent)
  stop(sprintf("File not found: %s (checked . and ..)", name))
}

# Prefer full PBP (has interceptions/drive info); fall back to clean only if present
pbp_candidates <- c("nflfastR_pbp_2023.csv", "nflfastR_pbp_2023_clean.csv")
pbp_file <- NULL
for (f in pbp_candidates) {
  if (file.exists(f)) { pbp_file <- f; break }
  if (file.exists(file.path("..", f))) { pbp_file <- file.path("..", f); break }
}
if (is.null(pbp_file)) stop("No play-by-play file found (looked for nflfastR_pbp_2023.csv or nflfastR_pbp_2023_clean.csv in . and ..)")

qb_weekly_file <- find_file("qb_weekly_2023.csv")
qb_season_file <- find_file("qb_season_2023.csv")

required_cols <- c(
  "pass", "air_yards", "ydstogo", "interception", "epa", "cpoe", "down",
  "score_differential", "drive", "game_id", "posteam_score", "touchdown", "posteam",
  "passer_player_id", "passer_player_name"
)

if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
}

# Load data
pbp <- fread(pbp_file)
qb_weekly <- fread(qb_weekly_file)
qb_season <- fread(qb_season_file)

missing_cols <- setdiff(required_cols, names(pbp))
if (length(missing_cols) > 0) {
  stop(sprintf(
    "Missing required columns in play-by-play data: %s. Use the full nflfastR_pbp_2023.csv (not the cleaned subset).",
    paste(missing_cols, collapse = ", ")
  ))
}

# Focus on dropbacks with air-yards info
pass_plays <- pbp[pass == 1 & !is.na(air_yards)]

# QB-level aggressiveness/air-yards metrics
qb_air <- pass_plays[, .(
  dropbacks = .N,
  deep_attempts = sum(air_yards >= 20, na.rm = TRUE),
  deep_rate = mean(air_yards >= 20, na.rm = TRUE),
  avg_air_yards = mean(air_yards, na.rm = TRUE),
  aggressiveness_share = mean(air_yards >= ydstogo, na.rm = TRUE),
  int_rate = mean(interception == 1, na.rm = TRUE),
  epa_per_dropback = mean(epa, na.rm = TRUE),
  cpoe_avg = mean(cpoe, na.rm = TRUE)
), by = .(passer_player_id, passer_player_name, posteam)]

# Blend in season-level passing volume for context
qb_air <- merge(
  qb_air,
  qb_season[, .(player_id, player_display_name, attempts, completions, passing_interceptions,
                passing_epa_per_drop = passing_epa_per_drop, passing_cpoe_avg = passing_cpoe_avg)],
  by.x = "passer_player_id",
  by.y = "player_id",
  all.x = TRUE,
  suffixes = c("", "_season")
)

setorder(qb_air, -deep_rate)
fwrite(qb_air, file.path("outputs", "qb_airyards_aggressiveness_2023.csv"))

# Drive-level outcomes
pbp[, drive_id := paste(game_id, drive, sep = "-")]

compute_drive_points <- function(score_vector) {
  if (all(is.na(score_vector))) return(NA_real_)
  max(score_vector, na.rm = TRUE) - min(score_vector, na.rm = TRUE)
}

drive_summary <- pbp[, .(
  plays = .N,
  drive_points = as.numeric(compute_drive_points(posteam_score)), # force consistent numeric type
  drive_td = any(touchdown == 1, na.rm = TRUE),
  drive_int = any(interception == 1, na.rm = TRUE)
), by = .(game_id, drive, posteam, drive_id)]

drive_outcomes <- drive_summary[, .(
  drives = .N,
  points_per_drive = mean(drive_points, na.rm = TRUE),
  td_rate = mean(drive_td, na.rm = TRUE),
  int_rate = mean(drive_int, na.rm = TRUE)
), by = posteam]

setorder(drive_outcomes, -points_per_drive)
fwrite(drive_summary, file.path("outputs", "drive_level_2023.csv"))
fwrite(drive_outcomes, file.path("outputs", "drive_outcomes_2023.csv"))

# Air-yards quartiles vs INT rate / EPA (robust to ties)
pass_plays[, air_rank := frank(air_yards, ties.method = "average")]
pass_plays[, pct := air_rank / max(air_rank, na.rm = TRUE)]
pass_plays[, air_yds_quartile := cut(pct,
                                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                                     include.lowest = TRUE,
                                     labels = paste0("Q", 1:4))]

quartile_summary <- pass_plays[, .(
  attempts = .N,
  int_rate = mean(interception == 1, na.rm = TRUE),
  epa_avg = mean(epa, na.rm = TRUE)
), by = air_yds_quartile]

quartile_summary[, quartile_num := as.integer(gsub("Q", "", air_yds_quartile))]
cor_results <- data.table(
  metric = c("int_rate", "epa_avg"),
  correlation = c(cor(quartile_summary$quartile_num, quartile_summary$int_rate, use = "complete.obs"),
                  cor(quartile_summary$quartile_num, quartile_summary$epa_avg, use = "complete.obs"))
)

fwrite(quartile_summary, file.path("outputs", "airyards_quartile_summary.csv"))
fwrite(cor_results, file.path("outputs", "airyards_quartile_correlations.csv"))

# Plots
int_plot <- ggplot(quartile_summary, aes(x = air_yds_quartile, y = int_rate)) +
  geom_col(fill = "#2b8cbe") +
  labs(title = "INT rate by air-yards quartile (2023)", x = "Air-yards quartile", y = "INT rate") +
  theme_minimal()

epa_plot <- ggplot(quartile_summary, aes(x = air_yds_quartile, y = epa_avg)) +
  geom_col(fill = "#a6bddb") +
  labs(title = "EPA per dropback by air-yards quartile (2023)", x = "Air-yards quartile", y = "EPA per dropback") +
  theme_minimal()

ggsave(filename = file.path("outputs", "int_rate_by_airyards_quartile.png"), plot = int_plot, width = 6, height = 4)
ggsave(filename = file.path("outputs", "epa_by_airyards_quartile.png"), plot = epa_plot, width = 6, height = 4)

# Regressions
reg_data <- pass_plays[complete.cases(air_yards, cpoe, down, ydstogo, score_differential)]

int_model <- glm(interception ~ air_yards + cpoe + down + ydstogo + score_differential,
                 data = reg_data, family = binomial(link = "logit"))
int_coefs <- as.data.table(coef(summary(int_model)), keep.rownames = "term")
fwrite(int_coefs, file.path("outputs", "regression_int_rate_coefs.csv"))

epa_model <- lm(epa ~ air_yards + cpoe + down + ydstogo + score_differential,
                data = reg_data)
epa_coefs <- as.data.table(coef(summary(epa_model)), keep.rownames = "term")
fwrite(epa_coefs, file.path("outputs", "regression_epa_coefs.csv"))

cat("Analysis complete. Outputs written to the outputs/ directory.\n")
