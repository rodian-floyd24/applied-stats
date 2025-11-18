# Applied Stats (NFLfastR)

Scripts and project materials for exploring NFL data with `nflfastR`.

## Contents
- `load_nflfastR_pbp.R`: Load/summarize play-by-play CSV.
- `clean_nflfastR_pbp.R`: Filter plays to offensive snaps and write a cleaned CSV.
- `load_player_stats_2023.R`: Fetch player-week stats for 2023 and write CSV.
- `build_qb_data_2023.R`: Produce QB weekly and season aggregates.
- `analyze_qb_air_aggressiveness_2023.R`: Analyze QB air-yards aggressiveness vs. drive outcomes, write summaries/plots to `outputs/`.
- `research_question.tex` / `research_question.pdf`: Project research question and evidence plan.

## Usage
Run scripts from the repo root, e.g.:
```bash
R -q -f load_nflfastR_pbp.R        # expects nflfastR_pbp_2023.csv in working dir
R -q -f clean_nflfastR_pbp.R       # writes nflfastR_pbp_2023_clean.csv
R -q -f load_player_stats_2023.R   # fetches player-week stats and writes csv
R -q -f build_qb_data_2023.R       # writes qb_weekly_2023.csv and qb_season_2023.csv
```

By default, CSV outputs are `.gitignore`'d to keep the repo small; place input CSVs in the repo root before running the scripts. The analysis script will also look in the parent directory for the CSVs if they are not in the repo.

Plots and analysis outputs are written to `outputs/` (ignored by git).
