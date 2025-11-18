# Analyze QB air-yards aggressiveness and drive outcomes for 2023.

library(data.table)
library(ggplot2)

# Parameters
min_dropbacks <- 100           # QB-level minimum to be included in summaries
min_drive_plays <- 3           # drop very short/invalid drives
regular_season_only <- TRUE    # restrict to REG if column exists
depth_bins <- c(-99, -1, 9, 19, Inf) # screen/short/intermediate/deep
depth_labels <- c("Behind LOS", "Short (0-9)", "Intermediate (10-19)", "Deep (20+)")
min_deep_att <- 20             # min deep attempts for leaderboards

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

# Optional season filter
if (regular_season_only && "season_type" %in% names(pbp)) {
  pbp <- pbp[season_type == "REG"]
}

# Focus on dropbacks with air-yards info; drop spikes/kneels/penalties/throwaways/batted if available
pass_plays <- pbp[pass == 1 & !is.na(air_yards)]
for (col in c("qb_spike", "qb_kneel", "penalty", "throw_out", "batted_pass")) {
  if (col %in% names(pass_plays)) {
    pass_plays <- pass_plays[(get(col) == 0) | is.na(get(col))]
  }
}
pass_plays <- pass_plays[!is.na(epa)]

# Add depth buckets
pass_plays[, depth_bucket := cut(air_yards, breaks = depth_bins, labels = depth_labels, include.lowest = TRUE, right = TRUE)]

# Winsorize EPA to dampen outliers for regression
epa_bounds <- quantile(pass_plays$epa, probs = c(0.01, 0.99), na.rm = TRUE)
pass_plays[, epa_w := pmin(pmax(epa, epa_bounds[1]), epa_bounds[2])]
pass_plays[, air_yards_sq := air_yards^2]

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

# Minimum volume filter
qb_air <- qb_air[dropbacks >= min_dropbacks]

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

# Drop extremely short/invalid drives
drive_summary <- drive_summary[plays >= min_drive_plays & !is.na(drive_points)]

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

# Depth bucket EPA/INT by QB
qb_depth <- pass_plays[, .(
  attempts = .N,
  int_rate = mean(interception == 1, na.rm = TRUE),
  epa_avg = mean(epa, na.rm = TRUE),
  cpoe_avg = mean(cpoe, na.rm = TRUE)
), by = .(passer_player_id, passer_player_name, posteam, depth_bucket)]

# Situational splits
pass_plays[, early_down := down %in% c(1, 2)]
pass_plays[, third_fourth := down %in% c(3, 4)]
pass_plays[, red_zone := yardline_100 <= 20]
pass_plays[, in_play_action := if ("play_action" %in% names(pass_plays)) play_action == 1 else NA]
pass_plays[, sacked := if ("sack" %in% names(pass_plays)) sack == 1 else FALSE]
pass_plays[, pressured := if ("qb_hit" %in% names(pass_plays)) qb_hit == 1 | sacked else sacked]

qb_situational <- pass_plays[, .(
  dropbacks = .N,
  epa_per_db = mean(epa, na.rm = TRUE),
  success = mean(epa > 0, na.rm = TRUE),
  int_rate = mean(interception == 1, na.rm = TRUE),
  td_rate = mean(touchdown == 1, na.rm = TRUE),
  cpoe_avg = mean(cpoe, na.rm = TRUE)
), by = .(passer_player_id, passer_player_name, posteam,
          early_down, third_fourth, red_zone, in_play_action, pressured)]

# Third/fourth down conversion rate for passes
if ("first_down" %in% names(pass_plays)) {
  qb_conv <- pass_plays[third_fourth == TRUE, .(
    attempts = .N,
    conv_rate = mean(first_down == 1, na.rm = TRUE),
    epa_per_db = mean(epa, na.rm = TRUE)
  ), by = .(passer_player_id, passer_player_name, posteam)]
} else {
  qb_conv <- data.table()
}

quartile_summary[, quartile_num := as.integer(gsub("Q", "", air_yds_quartile))]
cor_results <- data.table(
  metric = c("int_rate", "epa_avg"),
  correlation = c(cor(quartile_summary$quartile_num, quartile_summary$int_rate, use = "complete.obs"),
                  cor(quartile_summary$quartile_num, quartile_summary$epa_avg, use = "complete.obs"))
)

fwrite(quartile_summary, file.path("outputs", "airyards_quartile_summary.csv"))
fwrite(cor_results, file.path("outputs", "airyards_quartile_correlations.csv"))
fwrite(qb_depth, file.path("outputs", "qb_depth_splits.csv"))
fwrite(qb_situational, file.path("outputs", "qb_situational_splits.csv"))
if (nrow(qb_conv)) fwrite(qb_conv, file.path("outputs", "qb_third_fourth_conv.csv"))

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
reg_data <- copy(pass_plays)
reg_data[, pressure_flag := if ("pressured" %in% names(reg_data)) as.integer(pressured) else 0L]
reg_data[, play_action_flag := if ("in_play_action" %in% names(reg_data)) fifelse(is.na(in_play_action), 0L, as.integer(in_play_action)) else 0L]
reg_data <- reg_data[complete.cases(air_yards, air_yards_sq, cpoe, down, ydstogo, score_differential, pressure_flag, play_action_flag)]

int_model <- glm(interception ~ air_yards + air_yards_sq + cpoe + down + ydstogo + score_differential + pressure_flag + play_action_flag,
                 data = reg_data, family = binomial(link = "logit"))
int_coefs <- as.data.table(coef(summary(int_model)), keep.rownames = "term")
int_coefs[, odds_ratio := exp(Estimate)]
fwrite(int_coefs, file.path("outputs", "regression_int_rate_coefs.csv"))

# Robust EPA model: winsorized EPA with quadratic depth and pressure
epa_model <- tryCatch(
  {
    suppressWarnings(MASS::rlm(epa_w ~ air_yards + air_yards_sq + cpoe + down + ydstogo + score_differential + pressure_flag + play_action_flag,
                               data = reg_data, psi = MASS::psi.huber))
  },
  error = function(e) lm(epa_w ~ air_yards + air_yards_sq + cpoe + down + ydstogo + score_differential + pressure_flag + play_action_flag,
                         data = reg_data)
)
epa_coefs <- as.data.table(coef(summary(epa_model)), keep.rownames = "term")
fwrite(epa_coefs, file.path("outputs", "regression_epa_coefs.csv"))

# Human-readable summaries -----------------------------------------------------

round3 <- function(x) round(x, 3)
percent <- function(x) round(x * 100, 1)

# QB summary: readable (top 15 by dropbacks)
qb_air_pretty <- qb_air[, .(
  passer_player_name,
  team = posteam,
  dropbacks,
  deep_rate_pct = percent(deep_rate),
  avg_air_yards = round3(avg_air_yards),
  aggressiveness_share_pct = percent(aggressiveness_share),
  int_rate_pct = percent(int_rate),
  epa_per_dropback = round3(epa_per_dropback),
  cpoe_avg = round3(cpoe_avg)
)]
setorder(qb_air_pretty, -dropbacks)
fwrite(qb_air_pretty, file.path("outputs", "qb_airyards_aggressiveness_2023_readable.csv"))

# Depth splits readable
qb_depth_pretty <- qb_depth[, .(
  passer_player_name,
  team = posteam,
  depth_bucket,
  attempts,
  int_rate_pct = percent(int_rate),
  epa_avg = round3(epa_avg),
  cpoe_avg = round3(cpoe_avg)
)]
qb_depth_pretty <- qb_depth_pretty[attempts >= min_deep_att | depth_bucket != "Deep (20+)"]
fwrite(qb_depth_pretty, file.path("outputs", "qb_depth_splits_readable.csv"))

# Situational readable
qb_situational_pretty <- qb_situational[, .(
  passer_player_name, team = posteam,
  early_down, third_fourth, red_zone, in_play_action, pressured,
  dropbacks, epa_per_db = round3(epa_per_db), success_pct = percent(success),
  td_rate_pct = percent(td_rate), int_rate_pct = percent(int_rate), cpoe_avg = round3(cpoe_avg)
)]
fwrite(qb_situational_pretty, file.path("outputs", "qb_situational_splits_readable.csv"))

if (nrow(qb_conv)) {
  qb_conv_pretty <- qb_conv[, .(
    passer_player_name, team = posteam, attempts,
    conv_rate_pct = percent(conv_rate),
    epa_per_db = round3(epa_per_db)
  )]
  fwrite(qb_conv_pretty, file.path("outputs", "qb_third_fourth_conv_readable.csv"))
}

# Drive outcomes readable
drive_outcomes_pretty <- drive_outcomes[, .(
  posteam,
  drives,
  points_per_drive = round3(points_per_drive),
  td_rate_pct = percent(td_rate),
  int_rate_pct = percent(int_rate)
)]
setorder(drive_outcomes_pretty, -points_per_drive)
fwrite(drive_outcomes_pretty, file.path("outputs", "drive_outcomes_2023_readable.csv"))

# Quartile summary readable
quartile_summary_pretty <- quartile_summary[, .(
  air_yds_quartile,
  attempts,
  int_rate_pct = percent(int_rate),
  epa_avg = round3(epa_avg)
)]
fwrite(quartile_summary_pretty, file.path("outputs", "airyards_quartile_summary_readable.csv"))

# Regression readable: add odds ratios for INT model
int_coefs_pretty <- copy(int_coefs)
int_coefs_pretty[, odds_ratio := round3(exp(Estimate))]
fwrite(int_coefs_pretty, file.path("outputs", "regression_int_rate_coefs_readable.csv"))

# Quick markdown summary for eyeballing
# Helper to safely slice top/bottom
top_n <- function(dt, n = 5) dt[seq_len(min(n, nrow(dt)))]
bottom_n <- function(dt, n = 5) {
  if (nrow(dt) == 0) return(dt)
  start <- max(1, nrow(dt) - n + 1)
  dt[seq(start, nrow(dt))]
}

summary_lines <- c(
  "# QB Air-Yards Aggressiveness (2023)",
  "",
  "Scout-style readout focusing on depth, situational results, and risk/ reward.",
  "",
  "## Files",
  "- `qb_airyards_aggressiveness_2023_readable.csv`: QB-level metrics (top by dropbacks).",
  "- `drive_outcomes_2023_readable.csv`: team drive outcomes.",
  "- `airyards_quartile_summary_readable.csv`: quartiles vs INT rate and EPA.",
  "- `regression_int_rate_coefs_readable.csv`: logistic regression with odds ratios.",
  "- `qb_depth_splits_readable.csv`: EPA/CPOE/INT by throw depth.",
  "- `qb_situational_splits_readable.csv`: early/late downs, red zone, play-action, pressure splits.",
  "- `qb_third_fourth_conv_readable.csv`: 3rd/4th down conversion (if available).",
  "- `regression_epa_coefs.csv`: linear regression on EPA.",
  "",
  "## Highlights",
  sprintf("- QBs with at least %d dropbacks included.", min_dropbacks),
  sprintf("- Drives under %d plays dropped; regular season only: %s.", min_drive_plays, ifelse(regular_season_only, "yes", "no")),
  "",
  "## Correlations",
  sprintf("- Air-yards quartile vs INT rate: %.3f", cor_results[metric == "int_rate", correlation]),
  sprintf("- Air-yards quartile vs EPA: %.3f", cor_results[metric == "epa_avg", correlation]),
  "",
  "## Top-5 Deep Rate (by dropbacks)",
  paste0(
    top_n(qb_air_pretty[order(-deep_rate_pct)], 5)[, sprintf(
      "- %s (%s): deep rate %.1f%%, avg air yards %.2f, INT rate %.1f%%, EPA/drop %.3f",
      passer_player_name, team, deep_rate_pct, avg_air_yards, int_rate_pct, epa_per_dropback
    )]
  ),
  "",
  "## Team Points per Drive (top/bottom 5)",
  paste0(
    top_n(drive_outcomes_pretty, 5)[, sprintf(
      "- %s: points/drive %.2f, TD rate %.1f%%, INT rate %.1f%%",
      posteam, points_per_drive, td_rate_pct, int_rate_pct
    )]
  ),
  paste0(
    bottom_n(drive_outcomes_pretty, 5)[, sprintf(
      "- %s: points/drive %.2f, TD rate %.1f%%, INT rate %.1f%%",
      posteam, points_per_drive, td_rate_pct, int_rate_pct
    )]
  )
)
writeLines(summary_lines, file.path("outputs", "summary.md"))

cat("Analysis complete. Outputs written to the outputs/ directory.\n")
