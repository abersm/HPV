# Functions
upload_raw_data <- function(type = "rct", ignore_cols = NULL) {
  if (toupper(type) == "RCT") {
    raw_data_file <- "rct_study_results_tables_19apr2026.xls"
    est_column <- "RR_comb_val"
  } else {
    raw_data_file <- "observational_study_results_tables_13apr2026.xls"
    est_column <- "VE_comb_val"
  }

  # Upload raw data for each study
  raw <- read_excel(raw_data_file)
  if (anyNA(raw$tablenumstrat)) {
    stop("Column 'tablenumstrat' in raw data contains missing values")
  }
  if (anyDuplicated(drop_na(distinct(raw, tablenumstrat, table_id), table_id)$table_id)) {
    stop("Mapping between 'table_id' and 'tablenumstrat' columns in raw data is not correct")
  }
  rr <- strsplit(.subset2(raw, est_column), " |\\(|\\)|,")
  rr <- lapply(rr, function(x) trimws(x[x != ""]))
  if (!all(lengths(rr) == 3L)) {
    stop("'rr' should return a list of length 3 character vectors containing estimate and 95% CI")
  }
  rr <- lapply(rr, as.numeric)
  raw$rr <- vapply(rr, `[`, numeric(1), 1L, USE.NAMES = FALSE)
  raw$rr_lower <- vapply(rr, `[`, numeric(1), 2L, USE.NAMES = FALSE)
  raw$rr_upper <- vapply(rr, `[`, numeric(1), 3L, USE.NAMES = FALSE)
  if (any(raw$rr < raw$rr_lower) || any(raw$rr > raw$rr_upper) || any(raw$rr_lower > raw$rr_upper)) {
    stop("1 or more rows contains an error in 1 or more of the following columns: 'rr', 'rr_lower', and 'rr_upper'")
  }
  raw$rr_label <- gsub(", ", "-", .subset2(raw, est_column), fixed = TRUE)
  raw <- raw[!is.na(raw$table_id), , drop = FALSE]
  if (!is.null(ignore_cols)) {
    raw <- raw[setdiff(names(raw), ignore_cols)]
  }
  raw
}

upload_meta <- function(type = "RCT", ignore_cols = NULL) {
  # Upload meta-analysis results
  meta <- read_excel("hpv_meta", sheet = if (toupper(type) == "RCT") "RCT" else "Observational")
  meta$rr_label <- format_num_range(estimate = meta$rr, lower = meta$rr_lower, upper = meta$rr_upper)
  meta$study <- "Meta-analysis"
  if (anyNA(meta$tablenumstrat)) {
    stop("Column 'tablenumstrat' in meta analysis data contains missing values")
  }
  if (anyNA(meta$n_studies)) {
    stop("Column 'n_studies' contains 1 or more missing values")
  }
  if (!is.null(ignore_cols)) {
    meta <- meta[setdiff(names(meta), ignore_cols)]
  }
  meta
}

upload_all <- function(type = "RCT", n_min_meta = 3, by = "tablenumstrat", use_meta_as_lookup = TRUE, lookup_cols = c("outcome", "outcome_label", "comparison", "follow_up", "age_at_vax")) {
  raw <- upload_raw_data(type)
  meta <- upload_meta(type)

  if (length(setdiff(meta[[by]], raw[[by]])) != 0L) {
    stop("1 or more entries in 'by' column of 'meta' are not present in 'by' column in 'raw'")
    #setdiff(meta[[by]], raw[[by]])
  }
  meta$table_id <- NULL
  meta <- left_join(meta, distinct(raw[c(by, "table_id")]), by = by)
  lookup_cols <- setdiff(lookup_cols, c(by, "table_id"))
  if (use_meta_as_lookup) {
    lookup <- distinct(meta[c(by, intersect(names(meta), lookup_cols))])
    #raw <- raw[setdiff(names(raw), lookup_cols)]
  } else {
    lookup <- distinct(raw[c(by, intersect(names(raw), lookup_cols))])
    #meta <- meta[setdiff(names(meta), lookup_cols)]
  }
  if (anyDuplicated(lookup[[by]])) {
    stop(sprintf("Lookup data frame that includes 'lookup_cols' identified duplicates for column '%s'", by))
  }
  raw <- raw[c(by, setdiff(names(raw), names(lookup)))]
  meta <- meta[c(by, setdiff(names(meta), names(lookup)))]
  all <- bind_rows(raw, meta)
  all <- left_join(all, lookup, by = by)
  all <- all[order(all[[by]]), , drop = FALSE]
  all <- all[unique(c(by, "study", intersect(names(all), lookup_cols), names(all)))]
  if (!is.null(n_min_meta) && n_min_meta > 1L) {
    idx <- all[[by]] %in% meta[[by]][meta$n_studies >= n_min_meta]
    all <- all[idx,  , drop = FALSE]
  }
  all$is_meta <- ifelse(all$study == "Meta-analysis", 1L, 0L)
  list(raw = raw, meta = meta, all = all)
}

# Upload data
obs <- upload_all("Observational")
rct <- upload_all("RCT")
# Total counts for RCT meta-analysis
rct$all <- bind_rows(lapply(split(rct$all, rct$all$tablenumstrat), function(x) {
  idx <- x$study == "Meta-analysis"
  if (!any(idx)) return(x)
  other <- x[!idx, , drop = FALSE]
  for (i in c("n_vax_outcome", "total_vax_n", "n_unvax_outcome", "total_unvax_n")) {
    vals <- .subset2(x, i)
    if (is.numeric(vals)) {
      x[[i]][idx] <- sum(vals, na.rm = TRUE)
    }
  }
  x
}))

# Keep only combined data frame
obs <- obs$all
rct <- rct$all

# Clean RCT data
rct <- rct %>%
  #dplyr::filter(problem == 0 | is.na(problem)) %>%
  dplyr::mutate(
    study_design = "RCT",
    is_rct = 1L,
    weight = Weight
  )
cols <- c(
  "study_design", "is_rct", "id",
  "table_id", "tablenumstrat", "outcome_label", "comparison",
  "study", "is_meta", "vip_update",
  "n_studies",
  "outcome", "follow_up", "hpv_type",
  "vaccine_product",
  "sex", "age_at_vax",
  "n_vax_outcome", "total_vax_n", "n_unvax_outcome", "total_unvax_n",
  "rr", "rr_lower", "rr_upper", "rr_label",
  "tau_sq", "chi_sq", "i_sq", "df", "p", "weight"
)

setdiff(names(rct), cols)
rct <- rct[intersect(cols, names(rct))]

# Clean observational data
obs <- obs %>%
  dplyr::mutate(
    outcome_label = gsub("≤16", "≤ 16", outcome_label, fixed = TRUE),
    is_rct = 0L
  )
cols <- c(
  "study_design", "is_rct", "id",
  "table_id", "tablenumstrat", "outcome_label", "comparison",
  "study", "is_meta", "vip_update", "footnote", "footnote_details",
  "n_studies",
  "outcome", "follow_up", "hpv_type",
  "vaccine_product",
  "sex", "age_at_vax",
  "n_vax_outcome", "total_vax_n", "n_unvax_outcome", "total_unvax_n",
  "rr", "rr_lower", "rr_upper", "rr_label",
  "tau_sq", "chi_sq", "i_sq", "df", "p", "weight"
)
setdiff(names(obs), cols)
obs <- obs[intersect(cols, names(obs))]

# Combine RCT and observational
Setdiff_any(names(obs), names(rct))

# Export
hpv <- list(RCT = rct, Observational = obs)
xlsx(hpv)

# Clean up work space
remove(upload_raw_data, upload_meta, upload_all, cols)
