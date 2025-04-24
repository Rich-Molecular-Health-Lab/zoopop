# rct_demog.R
#'
#'#'
#'
#'
#' @importFrom htmlwidgets JS
#'
#' @export
#'
rows_bySex <- function() {
  JS("
    function(rowInfo, state) {
      if (!rowInfo) return {};
      if (rowInfo.values['Sex'] === 'Females') {
      return { borderLeft: '4px solid #C44D76FF', boxShadow: 'inset 0 -2px 0 #C44D7633'  }
    }  else if (rowInfo.values['Sex'] === 'Males') {
      return { borderLeft: '4px solid #4457A5FF', boxShadow: 'inset 0 -2px 0 #4457A533' }
      } else if (rowInfo.values['Sex'] === 'Overall') {
      return { borderLeft: '4px solid transparent' , boxShadow: 'inset 0 -1.5px 0 #444444FF' }
      }
    }
  ")
}


#'
#' @importFrom reactable reactable colDef
#' @importFrom reactablefmtr save_reactable_test
#' @importFrom dplyr mutate arrange select group_by distinct
#' @importFrom forcats fct_recode  fct_relevel
#'
#' @export
#'
rct_lifetime <- function(studbook, cohort_params = NULL) {
  params   <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  lifetime <- lifetime_tab(studbook, cohort_params = params) %>%
    distinct() %>%
    mutate(Sex = fct_recode(Sex, Overall = "Total", Males = "M", Females = "F")) %>%
    mutate(Sex = fct_relevel(Sex, "Males", "Females", "Overall")) %>%
    arrange(Cohort_period, Sex) %>%
    select(
      Cohort_years,
      Sex,
      r,
      N1,
      T,
      MLE,
      repro_first,
      repro_last,
      age_max,
      R0
    ) %>%
    group_by(Cohort_years)

  table <- reactable(
    lifetime,
    borderless = TRUE,
    rowStyle   = rows_bySex(),
    columns    = list(
      Cohort_years = Cohort_years(),
      r            = intrins_rate(),
      N1           = n1(lifetime),
      T            = gen_T(lifetime),
      MLE          = MLE(lifetime),
      repro_first  = age_ranges(lifetime),
      repro_last   = colDef(show = FALSE),
      age_max      = colDef(show = FALSE),
      R0           = r0(lifetime)
    )
  )

  return(table)
}
