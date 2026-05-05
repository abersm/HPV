# Upload observational data
obs <- read_excel("hpv_final", sheet = "Observational") %>%
  dplyr::filter(problem == 0 | is.na(problem)) %>%
  dplyr::mutate(
    #outcome_label = gsub("Chronic fatigue syndrome/myalgic encephalomyelitis", "CFS/ME", outcome_label, fixed = TRUE),
    study_design_category = "Observational",
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
    study_design_category = "RCT",
    study_design = "RCT",
    is_rct = 1L
  ) %>%
  dplyr::rename(
    id = tablenumstrat
  )

# Restrict to relevant columns only
cols <- c(
  "study_design", "study_design_category", "is_rct",
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
meta <- dplyr::bind_rows(rct, obs)

# Add columns to order by another variable
order_studies <- function(x, ..., group_vars = c("study_design_category", "id"), rev = FALSE, footnote_vector = letters) {
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

meta$row_order <- seq_len(nrow(meta))
meta <- meta %>%
  arrange(rr, rr_lower, rr_upper) %>%
  order_studies(rr)
meta <- meta %>%
  arrange(rr_lower, rr, rr_upper) %>%
  order_studies(rr_lower)
meta <- meta %>%
  arrange(rr_upper, rr, rr_lower) %>%
  order_studies(rr_upper)
meta <- arrange(meta, row_order)
meta$row_order <- NULL

meta$id_meta <- match(meta$outcome_label, unique(meta$outcome_label))

meta <- meta %>%
  mutate(
    study_original = study,
    #study_label = study_unique,
    study = study_unique,
    outcome_long = case_when(
      outcome == "CFS/ME" ~ "Chronic fatigue syndrome/myalgic encephalomyelitis",
      outcome == "GBS" ~ "Guillain-Barré syndrome",
      .default = outcome
    ),
    age_at_vax = gsub("yrs", "years", age_at_vax, fixed = TRUE),
    domain = ifelse(outcome %in% c("CFS/ME", "Paralysis", "GBS"), "Safety", "Efficacy")
  ) %>%
  filter(
    vax_product_analysis == "HPV vaccine"
  ) |>
  select(
    # Domain
    domain,
    # Analysis identifiers
    id, table_id,
    analysis_label = outcome_label,
    # Study
    study_original, study, is_meta, is_rct, study_design_category, study_design,
    footnote_details,
    # Analysis details
    comparison, #vax_product_analysis,
    outcome, outcome_long,
    age_at_vax, sex, hpv_type, follow_up,
    n_studies,
    # Data
    rr, rr_lower, rr_upper, rr_label, tau_sq, chi_sq, df, p, i_sq,
    n_vax_outcome, n_vax_no_outcome, total_vax_n, n_unvax_outcome, n_unvax_no_outcome, total_unvax_n,
    # Order for forest plot
    rr_order,
    rr_lower_order,
    rr_upper_order,
    # Others
    tidyselect::everything(),
    -c(study_unique, rr_footnote_superscript, rr_lower_footnote_superscript, rr_upper_footnote_superscript, footnote, vax_product_analysis)
  ) %>%
  rename(
    footnote = footnote_details
  )

# Add link column
pmid_doi <- read_excel("hpv_doi_pmid")
pmid_doi$doi <- paste0("https://doi.org/", pmid_doi$doi)
pmid_doi$link <- paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid_doi$pmid)
meta <- left_join(meta, pmid_doi, by = "study_original")

# Export data
usethis::use_data(meta, overwrite = TRUE)

# Create csv file for v1 folder
unlink("/Users/michaelabers/Desktop/R packages/HPV/inst/v1/df_shiny.csv")
write_csv(meta, "df_shiny", directory = "/Users/michaelabers/Desktop/R packages/HPV/inst/v1")
