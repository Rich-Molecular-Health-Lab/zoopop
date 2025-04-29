#' Add projected values to demographic summary table
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param sex Which group to include for summary under the `Sex` column - one of `Males`, `Females`, or `Overall` (optional, default is `Overall`)
#' @return Summary table with projection values to be used in leslie matrix construction
#' @export
#'
#' @importFrom dplyr filter group_by summarize ungroup
#'
proj_input <- function(studbook, cohort_params = NULL, sex = NULL) {
  if (is.null(sex)) { sex <- "Overall" }
  demog_summary(studbook, cohort_params) %>%
    filter(Sex == sex) %>%
    group_by(Cohort_years) %>%
    summarize(
      Ages = Age[[1]],                  # numeric vector of ages
      S    = Px[[1]],                   # survival probabilities
      F    = Fx[[1]],                   # fecundities
      N1   = Nx[[1]]
    ) %>%
    ungroup()
}

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
