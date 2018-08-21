source("data-raw/generation-helpers.R")

set.seed(42L)

# generate indices for subsignals
ind_tbl <- gen_sim_ind(nrow = 100L, ncol = 42L, max_subsig = 3L)

# generate signals - needed mainly for testing purposes
# (e.g. for mbte_extract_subsignals())
sig_tbl <- gen_sim_sig_default(ind_tbl)

# save internal signal-dataset (meant for testing only)
devtools::use_data(sig_tbl, internal = TRUE, overwrite = TRUE)

# add 0-padding, convert dataset to long form and rename time column
raw_signals <- gen_sim_raw(sig_tbl) %>%
  rename(t = time)

# dataset for examples (to avoid having to redo nesting and subsignal extraction
# over and over again)
filtered_signals <- raw_signals %>%
  group_by(mv) %>%
  new_tbl_mbte(t, value) %>%
  mbte_nest_signals() %>%
  mbte_extract_subsignals()
filtered_signals <- filtered_signals %>%
  filter(map_int(signal, nrow) > 20) %>%
  mbte_reconstruct(filtered_signals)

# save datasets for users
devtools::use_data(raw_signals, filtered_signals, overwrite = TRUE)
