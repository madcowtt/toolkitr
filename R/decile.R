#' Calculate percentile by volume
#'
#' @description Create percentiles for a single column. This appends the decile column to the dataset.
#' Will also keep the same volume in the same category
#'
#' @param data data frame with value column
#' @param value_var variable to denote the numerical variable
#' @param tie_breaker_var variable to denote what to use for tiebreakers
#' @param num_groups number of groups, e.g. 10 means decile plus a 0 group
#' @param group_label string to name the deciles
#' @param verbose verbose option of readout
#' @param calc_type 2 options "equal sum" or "group by value". Former uses continuous breaks and the latter groups observations based on their value as well. i.e. if 2 customers have the value 5, they will not be split across groups and will be forced together. The ones in a lower group will be promoted up
#'
#' @return Returns the same dataframe with an additional decile column
#' @export
#'
#' @importFrom dplyr groups
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom rlang sym
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @importFrom dplyr if_else
#' @importFrom dplyr summarize
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_at
#' @importFrom dplyr funs
#'
#' @details NAs and negatives are considered 0.
#' @examples
#' decile_df <- toolkitr::profile_example %>% dplyr::group_by(market_trx_post_decile) %>%
#' decile(value_var = "market_decile_nbrx_pre",
#' tie_breaker_var = "source_id", num_groups = 5, new_col = "quintile")
#'
#' decile_df <- toolkitr::profile_example %>% tile(value_var = "market_decile_nbrx_pre",
#' tie_breaker_var = "source_id", num_groups = 5,
#' new_col = "quintile", calc_type = "group by value")
decile <- function(data,
                   unique_id,
                   value_var,
                   num_groups,
                   new_col,
                   tie_breaker_var = {{ unique_id }},
                   verbose = TRUE,
                   calc_type = "group by value") {

  # Capture symbols as quosures (for later use where needed)
  unique_id_quo <- rlang::enquo(unique_id)
  value_var_quo <- rlang::enquo(value_var)
  new_col_quo <- rlang::enquo(new_col)
  tie_breaker_quo <- rlang::enquo(tie_breaker_var)

  # Initial group_bys
  initial_group_bys <- dplyr::groups(data)

  # Check for existence of variables in the dataset
  if (!rlang::as_name(value_var_quo) %in% colnames(data)) {
    stop(paste0("Variable: ", rlang::as_name(value_var_quo), " not found in the dataset"))
  }

  if (rlang::as_name(new_col_quo) %in% colnames(data)) {
    stop(paste0("New variable: ", rlang::as_name(new_col_quo), " already in dataset, please use a different name"))
  }


  # Check uniqueness at unique_id level
  if (!(nrow(data) == data %>% dplyr::select(!!unique_id_quo) %>% dplyr::n_distinct())) {
    stop(paste0("data not unique at unique_id level"))
  }

  # Sort
  if (!is.null(initial_group_bys)) {
    data <- data %>%
      dplyr::arrange(
        dplyr::desc(!!value_var_quo),
        dplyr::desc(!!tie_breaker_quo),
        .by_group = TRUE
      )
  } else {
    data <- data %>%
      dplyr::arrange(
        dplyr::desc(!!value_var_quo),
        dplyr::desc(!!tie_breaker_quo)
      )
  }

  # Create normalized index
  data <- data %>% dplyr::mutate(.index = !!value_var_quo)
  data <- data %>% tidyr::replace_na(list(.index = 0))

  data <- data %>%
    dplyr::mutate(
      .index = dplyr::if_else(.data$.index < 0, 0, .data$.index),
      .index_sum = sum(.data$.index),
      .index = .data$.index / .data$.index_sum,
      .cum_index = cumsum(.data$.index)
    )

  # Determine break points
  data <- data %>%
    dplyr::mutate(
      .grps = cut(
        .data$.cum_index,
        breaks = seq(0, 1 + 1 / num_groups, by = 1 / num_groups),
        labels = paste(seq(num_groups, 0), sep = "_"),
        include.lowest = FALSE,
        right = TRUE
      )
    )

  # Convert the levels to number
  data$.grps <- as.numeric(as.character(data$.grps))
  data <- data %>%
    dplyr::mutate(.grps = dplyr::if_else(.data$.index == 0, 0, .data$.grps))

  # Group by value thresholds (optional)
  if (calc_type == "group by value") {
    group_by_value_threshold <- data %>%
      dplyr::group_by(.data$.index, .add = TRUE) %>%
      dplyr::summarize(.grps_max = max(.data$.grps), .groups = "drop")

    data <- data %>%
      dplyr::left_join(group_by_value_threshold, by = ".index") %>%
      dplyr::mutate(.grps = .data$.grps_max) %>%
      dplyr::select(-.data$.grps_max)
  }

  # Rename to final new_col
  data <- data %>% dplyr::rename(!!new_col_quo := .data$.grps)

  # Verbose summary
  if (verbose == TRUE) {
    summary <- data %>%
      mutate(!!new_col_quo := as.character(!!new_col_quo)) %>%
      bind_rows(
        data %>% mutate(!!new_col_quo := "total")
      ) %>%
      dplyr::group_by(!!new_col_quo, .add = TRUE) %>%
      dplyr::summarize(
        n = dplyr::n(),
        dplyr::across(
          {{ value_var }},
          list(
            sum = ~sum(.x, na.rm = TRUE),
            min = ~min(.x, na.rm = TRUE),
            max = ~max(.x, na.rm = TRUE)
          ),
          .names = "{.fn}_{.col}"
        ),
        .groups = "drop"
      )
    print(summary)
  }

  data <- data %>% dplyr::select(-.data$.index, -.data$.index_sum, -.data$.cum_index)

  return(data)
}

