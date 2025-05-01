#' Create a Leslie Matrix using demographic summary table
#'
#' @param F name of column containing fecundities
#' @param S name of column containing survival probabilities
#' @return Values for matrix `A`
#' @export
#'
#'
make_leslie <- function(F, S) {
  n <- length(F)
  A <- matrix(0, nrow = n, ncol = n)
  A[1, ] <- F
  A[cbind(2:n, 1:(n-1))] <- S[1:(n-1)]
  A
}

#' Add projection matrices to demographic summary table
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @return Summary table with projection values
#' @export
#'
#' @importFrom dplyr filter group_by summarize ungroup left_join join_by
#' @importFrom popbio pop.projection
#' @importFrom purrr map2 map_dbl
#'
proj_matrix <- function(studbook, cohort_params = NULL) {
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
          pr  <- popbio::pop.projection(A, n0, iterations = 10)
          mat <- t(pr$stage.vector)
          full <- rbind(`0` = n0, mat)
          colnames(full) <- as.character(0:(ncol(full)-1))
          rownames(full) <- 0:10
          full
        }
      )
    ) %>%
    left_join(demog, by = join_by(Cohort_years, Sex))

  return(proj)
}

#' Add projection time series columns to demographic summary table
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @return Summary table with a new projection time series columns
#' @export
#'
#' @importFrom dplyr filter group_by summarize ungroup left_join join_by
#' @importFrom popbio pop.projection
#' @importFrom purrr map2 map_dbl
#'
proj_ts <- function(studbook, cohort_params = NULL, variable) {

}







