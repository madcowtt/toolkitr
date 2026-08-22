#' Clean column names by converting case and replacing non-standard characters to _
#'
#' @param data dataframe to clean up column names
#' @param case string for lower or upper case
#'
#' @returns the dataframe with updated column names
#' @export
#'
#' @examples
#' df <- toolkitr::profile_example %>% clean_col_names()
clean_col_names <- function(data, case = c("lower", "upper")) {
  case <- match.arg(case)

  new_names <- names(data)

  # Replace any run of non-alphanumeric characters with a single underscore
  new_names <- gsub("[^A-Za-z0-9]+", "_", new_names)

  # Trim leading/trailing underscores left over from edge punctuation
  new_names <- gsub("^_+|_+$", "", new_names)

  # Apply case
  new_names <- if (case == "lower") tolower(new_names) else toupper(new_names)

  # Append _1, _2, etc. to every name that has duplicates (including the first)
  dup_names <- unique(new_names[duplicated(new_names)])
  for (nm in dup_names) {
    idx <- which(new_names == nm)
    new_names[idx] <- paste0(nm, "_", seq_along(idx))
  }

  names(data) <- new_names
  data
}
