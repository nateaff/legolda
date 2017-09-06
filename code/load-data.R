# Connect to database and load all lego sets

load_data <- function(sample_data = TRUE, n_samples = 10000) {
  library(DBI)
  library(dplyr)

  con <- connect()
  # Check themes
  cat("Assigning themes to theme_df \n")
  theme_df <<- tbl(con, "themes") %>%
    collect()
  cat("Assigning sets to sets_df \n")
  sets_df <<- tbl(con, "sets") %>%
    collect()
  tbl(con, "themes") %>%
    collect() %>%
    select(name) %>%
    unique()

  # Counts by theme
  counts <- tbl(con, "themes") %>%
    group_by(parent_id) %>%
    count() %>%
    collect()
  sum(counts$n)

  # All valid parents
  parents <- counts %>%
    filter(!is.na(parent_id))
  parent_set <- parents$parent_id
  # TODO: Redo or not us
  parent_set
  # --------------------------------------------------

  # Get tables
  themes <- tbl(con, "themes")
  inventory_sets <- tbl(con, "inventory_sets")
  inventories <- tbl(con, "inventories")
  inventory_parts <- tbl(con, "inventory_parts")
  colors <- tbl(con, "colors")
  sets <- tbl(con, "sets")

  set_raw <- themes %>%
    # Set root_id parent or to id if it is a top level theme
    mutate(root_id = ifelse(!is.na(parent_id), parent_id, id)) %>%
    select(name, id, root_id) %>%
    right_join(sets, by = c("id" = "theme_id")) %>%
    select(set_num, name.y, year, root_id, id, name.x) %>%
    right_join(inventories, by = "set_num", copy = TRUE) %>%
    right_join(inventory_parts, by = c("id.y" = "inventory_id")) %>%
    left_join(colors, by = c("color_id" = "id")) %>%
    select(set_num, name.y, root_id, name.x, id.x, rgb, year, is_trans, quantity) %>%
    collect()

  if (sum(is.na(set_raw$root_id)) > 0) stop("NA values in root_id")
  n_sets <- set_raw$set_num %>%
    unique() %>%
    length()

  cat("Disconnecting from database \n")
  DBI::dbDisconnect(con)

  # Clean up data and and transparency
  names(set_raw) <- c("set_num", "name", "root_id", "theme", "theme_id", "rgb", "year", "is_trans", "quantity")
  set_colors <- set_raw %>%
    dplyr::mutate(name = stringr::str_sub(name, 1, 20)) %>%
    # Add alpha, transparent = 0.5 = '80', opaque = 'FF
    dplyr::mutate(rgba = ifelse(is_trans == "t",
      paste0("#", rgb, "80"),
      paste0("#", rgb, "FF")
    )) %>%
    dplyr::select(set_num, name, theme, theme_id, root_id, year, rgba, quantity)

  sampled_status <- "full set"
  if (sample_data) {
    cat("Taking ", n_samples, " rows sample of set inventories")
    set_colors <- set_colors %>%
      sample_n(n_samples)
    sampled <- "sampled"
  }
  cat(paste0("Assigning ", sampled_status, " set inventories to 'set_colors' \n"))
  # Expand rows by quantity : Note
  set_colors <<- set_colors[rep(seq(nrow(set_colors)), set_colors$quantity), 1:7]
}
