# Upload observational data
obs <- read_excel("hpv_final", sheet = "Observational") %>%
  dplyr::filter(problem == 0 | is.na(problem)) %>%
  dplyr::mutate(
    #outcome_label = gsub("Chronic fatigue syndrome/myalgic encephalomyelitis", "CFS/ME", outcome_label, fixed = TRUE),
    study_category = "Observational",
    is_rct = 0L,
  ) %>%
  dplyr::rename(
    id = tablenumstrat
  ) %>%
  dplyr::select(-c(n_vax_outcome, n_vax_no_outcome, n_unvax_outcome, n_unvax_no_outcome))

# Upload RCT
rct <- read_excel("hpv_final", sheet = "RCT") %>%
  dplyr::filter(problem == 0 | is.na(problem)) %>%
  dplyr::mutate(
    study_category = "RCT",
    study_design = "RCT",
    is_rct = 1L
  ) %>%
  dplyr::rename(
    id = tablenumstrat
  )

# Restrict to relevant columns only
cols <- c(
  "study_design", "study_category", "is_rct",
  "table_id", "id", "outcome_label",
  "study", "study_unique", "is_meta",
  #"vip_update",
  "outcome", "follow_up",
  #"follow_up_actual",
  "hpv_type",
  "comparison", "vax_product_analysis",
  #"vax_product_actual",
  "sex", "age_at_vax",
  #"age_actual",
  "n_vax_outcome", "n_vax_no_outcome", "total_vax_n", "n_unvax_outcome", "n_unvax_no_outcome", "total_unvax_n",
  "rr", "rr_lower", "rr_upper", "rr_label",
  "tau_sq", "chi_sq", "i_sq", "p",
  "n_studies", "df",
  #"weight",
  "footnote", "footnote_details"
)
obs <- obs[intersect(names(obs), cols)]
rct <- rct[intersect(names(rct), cols)]

# Combine data
data <- dplyr::bind_rows(rct, obs)

# Add columns to order by another variable
order_studies <- function(x, ..., group_vars = c("study_category", "id"), rev = FALSE, footnote_vector = letters) {
  by_cols <- dots_as_quoted(...)
  by_cols_prefix <- paste(by_cols, collapse = "_")
  x$IDX <- seq_len(nrow(x))
  x$HAS_FOOTNOTE <- !is.na(x$footnote_details)
  x <- dplyr::arrange(x, ...)
  x <- dplyr::group_by(x, is_meta, !!!rlang::syms(group_vars))
  if (rev) {
    x <- dplyr::mutate(x, ORDER = dplyr::n():1L)
  } else {
    x <- dplyr::mutate(x, ORDER = 1L:dplyr::n())
  }
  x <- dplyr::ungroup(x)
  x <- dplyr::group_by(x, is_meta, HAS_FOOTNOTE, !!!rlang::syms(group_vars))
  x <- dplyr::arrange(x, ORDER)
  x <- dplyr::mutate(x, FOOTNOTE_NUMBER = dplyr::n():1L)
  x <- dplyr::ungroup(x)
  x$ORDER[x$is_meta] <- 0L
  x$FOOTNOTE_NUMBER[x$is_meta | !x$HAS_FOOTNOTE] <- NA_integer_
  x$FOOTNOTE_NUMBER <- footnote_vector[x$FOOTNOTE_NUMBER]
  names(x)[names(x) == "ORDER"] <- paste0(by_cols_prefix, "_order")
  names(x)[names(x) == "FOOTNOTE_NUMBER"] <- paste0(by_cols_prefix, "_footnote_superscript")
  x <- dplyr::arrange(x, IDX)
  x$IDX <- x$HAS_FOOTNOTE <- NULL
  x
}

data$row_order <- seq_len(nrow(data))
data <- data %>%
  arrange(rr, rr_lower, rr_upper) %>%
  order_studies(rr)
data <- data %>%
  arrange(rr_lower, rr, rr_upper) %>%
  order_studies(rr_lower)
data <- data %>%
  arrange(rr_upper, rr, rr_lower) %>%
  order_studies(rr_upper)
data <- arrange(data, row_order)
data$row_order <- NULL

data <- data %>%
  mutate(
    study_original = study,
    study_label = study_unique,
    study = study_label,
    outcome_long = case_when(
      outcome == "CFS/ME" ~ "Chronic fatigue syndrome/myalgic encephalomyelitis",
      outcome == "GBS" ~ "Guillain-Barré syndrome",
      .default = outcome
    ),
    efficacy = ifelse(outcome %in% c("CFS/ME", "Paralysis", "GBS"), 0L, 1L)
  ) %>%
  filter(
    vax_product_analysis == "HPV vaccine"
  )

# Export as csv file
write_csv(data, "df_shiny")
