
#' Title
#'
#' @param data data frame with value column
#' @param value_var variable to denote the numerical variable
#' @param value_breaks sequence of value %s to flag for graphing
#' @param cust_breaks set of customer %s to flag for graphing
#' @param keep_first_n_points how many of the first n-points to keep for graphing the steep slope
#'
#' @returns Returns a new dataframe of all the customers x groups and concentration calculations
#' @export
#'
#' @examples
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_modify
#' @importFrom dplyr ungroup
draw_conc_curves <- function(data,
                                value_var,
                                value_breaks = seq(0, 1, by = 0.05),
                                cust_breaks = c(0.2, 0.5),
                                keep_first_n_points = 14) {

  value_var <- enquo(value_var)

  # Calculate the number that is closest to the target based on absolute distance
  flag_closest <- function(dat, x_col, targets) {
    v <- dat[[x_col]]
    idx <- vapply(
      targets,
      function(t) which.min(abs(v - t)),
      integer(1)
    )
    unique(idx)
  }


  data %>%
    dplyr::arrange(desc(!!value_var), .by_group = TRUE) %>%
    dplyr::mutate(
      cumu_val      = cumsum(coalesce(!!value_var, 0)),
      total_sum     = sum(!!value_var, na.rm = TRUE),
      cumu_perc_val = if_else(total_sum > 0, cumu_val / total_sum, NA_real_),
      row_num       = row_number(),
      max_row_num   = n(),
      cumu_perc_n   = row_num / max_row_num
    ) %>%
    dplyr::group_modify(~{
      dat <- .x

      flag <- dat$row_num <= min(keep_first_n_points, nrow(dat))

      if (!all(is.na(dat$cumu_perc_val))) {
        idx_rev <- flag_closest(dat, "cumu_perc_val", value_breaks)
        flag[idx_rev] <- TRUE
      }

      # Flag specific customer % break points
      idx_cust <- flag_closest(dat, "cumu_perc_n", cust_breaks)
      flag[idx_cust] <- TRUE

      dat$flag_for_graphing <- as.integer(flag)
      dat
    }) %>%
    dplyr::ungroup()
}
