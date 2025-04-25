# rct_demog.R
#'
#'
#' @importFrom dplyr distinct mutate filter select
#'
#' @export
footer_lifetime <- function(studbook) {
  lifetime_total(studbook = studbook) %>%
    distinct() %>%
    mutate(Sex          = fct_recode(Sex, Overall = "Total", Males = "M", Females = "F"),
           Cohort_years = "All years",
           sex_col      = "black") %>%
    filter(Sex == "Overall") %>%
    mutate(Sex = "Summary") %>%
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
      R0,
      sex_col
    )
}
#'
#'
#' @importFrom htmlwidgets JS
#'
#' @export
#'
rows_bySex <- function() {
  JS("
    function(rowInfo, state) {
      if (rowInfo.values['Sex'] === 'Females') {
      return { borderLeft: '4px solid #C44D76FF', boxShadow: 'inset 0 -2px 0 #C44D7633'  }
    }  else if (rowInfo.values['Sex'] === 'Males') {
      return { borderLeft: '4px solid #4457A5FF', boxShadow: 'inset 0 -2px 0 #4457A533' }
      } else if (rowInfo.values['Sex'] === 'Overall') {
      return { borderLeft: '4px solid transparent' , boxShadow: 'inset 0 -1.5px 0 #444444FF' }
      } else if (rowInfo.values['Sex'] === 'Summary')  {
      return { borderLeft: '4px solid #999999', boxShadow: 'outset 0 2px 0 #eeeeee' , background: '#eeeeee', fontWeight: 'bold' }
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
  colors   <- set_colors()
  params   <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  total    <- footer_lifetime(studbook = studbook)
  data <- lifetime_tab(studbook, cohort_params = params) %>%
    distinct() %>%
    mutate(Sex = fct_recode(Sex, Overall = "Total", Males = "M", Females = "F")) %>%
    mutate(Sex = fct_relevel(Sex, "Males", "Females", "Overall")) %>%
    arrange(Cohort_period, Sex) %>%
    mutate(sex_col = case_match(Sex,
                                "Overall" ~colors[["t"]],
                                "Males"   ~colors[["m"]],
                                "Females" ~colors[["f"]]
    )) %>%
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
      R0,
      sex_col
    ) %>%
    bind_rows(total) %>%
    group_by(Cohort_years) %>%
    mutate(Sex = as_factor(Sex))
  table <- reactable(
    data,
    height     = 750,
    pagination = FALSE,
    sortable   = FALSE,
    borderless = TRUE,
    rowStyle      = rows_bySex(),
    columns    = list(
      Sex          = Sex(),
      Cohort_years = Cohort_years(),
      r            = intrins_rate(),
      N1           = n1(data),
      T            = gen_T(data),
      MLE          = MLE(data),
      repro_first  = age_ranges(data),
      repro_last   = colDef(show = FALSE),
      age_max      = colDef(show = FALSE),
      R0           = r0(data),
      sex_col      = colDef(show = FALSE)
    )
  )

  return(table)
}
