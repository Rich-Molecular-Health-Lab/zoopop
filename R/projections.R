#' Create a Leslie Matrix using demographic summary table
#'
#' @param F name of column containing fecundities
#' @param S name of column containing survival probabilities
#' @return Values for matrix `A`
#' @noRd
#'
#'
make_leslie <- function(F, S) {
  n <- length(F)
  A <- matrix(0, nrow = n, ncol = n, byrow = TRUE, dimnames = as.list(rep("Age_yr", n)))
  A[1, ] <- F
  A[cbind(2:n, 1:(n-1))] <- S[1:(n-1)]
  A
}

#' Add projection matrices to demographic summary table
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param iterations Number of year-steps to project out through iterations (default = 20 years)
#' @return Summary table with projection values
#' @export
#'
#' @importFrom dplyr filter group_by summarize ungroup left_join join_by
#' @importFrom popbio pop.projection
#' @importFrom purrr map2 map_dbl
#'
proj_matrix <- function(studbook, cohort_params = NULL, iterations = 20) {
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  demog <- demog_summary(studbook, params)
  proj <- demog %>%
    group_by(Cohort_years, Sex) %>%
    summarize(
      Ages = list(Age[[1]]),   # a numeric vector of ages
      S    = list(Px[[1]]),    # a numeric vector of survivals
      F    = list(Fx[[1]]),    # a numeric vector of fecundities
      N0   = list(Nx[[1]]),    # a numeric vector of starting abundances
      .groups = "drop"
    ) %>%
    mutate(
      Leslie = map2(F, S, make_leslie),
      Projection = map2(
        Leslie, N0,
        function(A, n0) {
          pr  <- popbio::pop.projection(A, n0, iterations = iterations)
          mat <- t(pr$stage.vector)
          full <- rbind(`0` = n0, mat)
          colnames(full) <- as.character(0:(ncol(full)-1))
          rownames(full) <- 0:iterations
          full
        }
      )
    ) %>%
    left_join(demog, by = join_by(Cohort_years, Sex))

  return(proj)
}

#' Add projection matrices to demographic summary table with all birth-year cohorts merged
#'
#' @param studbook Studbook tibble
#' @param iterations Number of year-steps to project out through iterations (default = 20 years)
#' @return Summary table with projection values factoring in all individuals across all birth years
#' @export
#'
#' @importFrom dplyr filter group_by summarize ungroup left_join join_by
#' @importFrom popbio pop.projection
#' @importFrom purrr map2 map_dbl
#'
proj_matrix_overall <- function(studbook, iterations = 20) {
  demog <- demog_summary_total(studbook)
  proj <- demog %>%
    group_by(Sex) %>%
    summarize(
      Ages = list(Age[[1]]),   # a numeric vector of ages
      S    = list(Px[[1]]),    # a numeric vector of survivals
      F    = list(Fx[[1]]),    # a numeric vector of fecundities
      N0   = list(Nx[[1]]),    # a numeric vector of starting abundances
      .groups = "drop"
    ) %>%
    mutate(
      Leslie = map2(F, S, make_leslie),
      Projection = map2(
        Leslie, N0,
        function(A, n0) {
          pr  <- popbio::pop.projection(A, n0, iterations = iterations)
          mat <- t(pr$stage.vector)
          full <- rbind(`0` = n0, mat)
          colnames(full) <- as.character(0:(ncol(full)-1))
          rownames(full) <- 0:iterations
          full
        }
      )
    ) %>%
    left_join(demog, by = join_by(Sex))

  return(proj)
}


#' Compute a full time×age grid for a given variable at each t (*_grid) for a projection summary table
#'
#' @param proj_df Summary table with projection values (produced by `proj_matrix`)
#' @param variable Name of the variable for which the time series should be projected
#'  -  one of: `Fx` (reproductive output), `Mx` (fertility rate), `Qx` (mortality rate), `Px` (survival probability), `Tx` (total future lifetime), `ex` (life expectancy)
#' @return Summary table with projection time series list-columns for values across all ages projected for a given variable
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map2
#'
proj_var_grid <- function(proj_df, var) {
  grid_col <- paste0(var, "_grid")
  result <- proj_df %>% mutate(!!grid_col := map2(Projection,  .data[[var]], \(x, y) sweep(x, 2,  y, `*`)))
  return(result)
}

#' Compute a total time‐series summing over age at each t (*_ts) for a projection summary table
#'
#' @param proj_df Summary table with projection values (produced by `proj_matrix`)
#' @param variable Name of the variable for which the time series should be projected
#'  -  one of: `Fx` (reproductive output), `Mx` (fertility rate), `Qx` (mortality rate), `Px` (survival probability), `Tx` (total future lifetime), `ex` (life expectancy)
#' @return Summary table with projection time series list-columns for values across all ages projected for a given variable
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map2
#'
proj_var_ts <- function(proj_df, var) {
  ts_col  <- paste0(var, "_ts")
  result <- proj_df %>% mutate(!!ts_col := map2(Projection, .data[[var]], \(x, y) as.numeric(x %*% y)))
  return(result)
}


#' Wrapper function to compute grid and ts columns for all age-specific variables
#'
#' @param proj_df Summary table with projection values (produced by `proj_matrix`)
#' @param variables Names of the variables for which the values should be computed (default is c(`Fx`,`Mx`,`Qx`,`Px`,`Tx`,`ex`))
#' @return Summary table with projection time series list-columns for values across all ages projected for a given variable
#' @export
#'
#' @importFrom purrr reduce
#'
proj_ts <- function(proj_df, variables = c("Fx","Mx","Qx","Px","Tx","ex")) {
  reduce(
    variables,
    function(df, var) {
      df %>% proj_var_grid(var) %>% proj_var_ts(var)
    },
    .init = proj_df
  )
}

#' Create a detailed projection summary table from a studbook tibble
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param iterations Number of year-steps to project out through iterations (default = 20 years)
#' @return Summary table with projection values, including full `time x age` grids and total time-series list-cols for each age-specific variable
#' @export
#'
#'
projections_studbook <- function(studbook, cohort_params = NULL, iterations = 20) {
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  proj_overall <- projections_overall_studbook(studbook, iterations = iterations)
  proj   <- proj_matrix(studbook, params, iterations) %>% proj_ts() %>%
    bind_rows(proj_overall)
  return(proj)
}

#' Create a detailed projection summary table from a studbook tibble with all birth-year cohorts merged
#'
#' @param studbook Studbook tibble
#' @param iterations Number of year-steps to project out through iterations (default = 20 years)
#' @return Summary table with projection values, including full `time x age` grids and total time-series list-cols for each age-specific variable
#' @export
#'
#'
projections_overall_studbook <- function(studbook, iterations = 20) {
  proj   <- proj_matrix_overall(studbook, iterations) %>% proj_ts()
  return(proj)
}
