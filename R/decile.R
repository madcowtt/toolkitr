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
#' tie_breaker_var = "source_id", num_groups = 5, group_label = "quintile")
#'
#' decile_df <- toolkitr::profile_example %>% tile(value_var = "market_decile_nbrx_pre",
#' tie_breaker_var = "source_id", num_groups = 5,
#' group_label = "quintile", calc_type = "group by value")
decile <- function(data,
                   unique_id,
                   value_var = "value",
                   num_groups,
                   group_label,
                   tie_breaker_var = unique_id,
                   verbose = TRUE,
                   calc_type = "group by value") {


  # Initial group_bys
  initial_group_bys <- dplyr::groups(data)

  if (!(value_var %in% colnames(data))){
    stop(paste0("Variable: ", value_var, " not found in the dataset"))
  }

  if (group_label %in% colnames(data)){
    stop(paste0("New variable: ", group_label, " already in dataset please use a different name"))
  }


  if (!(nrow(data) == data %>% select(!!rlang::sym(unique_id)) %>% n_distinct())){
    stop(paste0("data not unique at unique_id level"))
  }

  # Sort
  if (!is.null(initial_group_bys))
  {
    data <- data %>%
      dplyr::arrange(dplyr::desc(!!rlang::sym(value_var)), dplyr::desc(!!rlang::sym(tie_breaker_var)), .by_group = TRUE)

  }else
  {
    data <- data %>%
      dplyr::arrange(dplyr::desc(!!rlang::sym(value_var)), dplyr::desc(!!rlang::sym(tie_breaker_var)))

  }

  # Create normalized index
  data <- data %>% dplyr::mutate(.index = !!rlang::sym(value_var))
  data <- data %>% tidyr::replace_na(list(.index=0))

  data <- data %>%
    dplyr::mutate(.index = dplyr::if_else(.data$.index < 0, 0, .data$.index),
                  .index_sum = sum(.data$.index),
                  .index = .data$.index / .data$.index_sum,
                  .cum_index = cumsum(.data$.index))

  # determine break points (these are continuous and discrete, so obs with the same value are not in the same decile)
  data <- data %>% dplyr::mutate(.grps = cut(.data$.cum_index, breaks = seq(0, 1+1/num_groups, by = 1/num_groups),
                                             labels = paste(seq(num_groups, 0), sep = "_"), include.lowest = F, right = T))



  # Convert the levels to number
  data$.grps <- as.numeric(as.character(data$.grps))

  data <- data %>% dplyr::mutate(.grps = dplyr::if_else(.data$.index == 0, 0, .data$.grps))

  # Get group by value thresholds (the index for each group)
  if(calc_type == "group by value")
  {
    group_by_value_threshold <- data %>% dplyr::group_by(.data$.index, .add = TRUE) %>% dplyr::summarize(.grps_max = max(.data$.grps))
    data <- data %>% dplyr::left_join(group_by_value_threshold)

    data <- data %>% dplyr::mutate(.grps = .data$.grps_max)
    data <- data %>% dplyr::select(-.data$.grps_max)
  }


  data <- data %>% dplyr::rename(!!rlang::sym(group_label) := .data$.grps)

  # Verbose readout
  if(verbose==TRUE)
  {
    summary <- data %>% dplyr::group_by_(group_label, add = TRUE) %>%
      dplyr::summarize(
        n = dplyr::n(),
        dplyr::across(
          value_var,
          list(
            sum = ~ sum(.x, na.rm = TRUE),
            min = ~ min(.x, na.rm = TRUE),
            max = ~ max(.x, na.rm = TRUE)
          ),
          .names = "{.fn}_{.col}"
        )
      )
    print(summary)
  }

  data <- data %>% dplyr::select(-.data$.index, -.data$.index_sum, -.data$.cum_index)

  return(data)
}
