# rct_demog.R
#'
#'
#' Style each row of a reactable demographic table based on groups by Sex
#'
#' @importFrom htmlwidgets JS
#'
#' @noRd
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
      return { position: 'sticky', bottom: '0' , border: '3px solid #999999', fontWeight: 'bold' }
      }
    }
  ")
}
#'
#' Create a list of values to use as a footer row in a reactable summary of demographic variables
#'
#' @param studbook Studbook tibble
#'
#' @importFrom dplyr ungroup mutate select
#'
#' @noRd
#'
foot_demog <- function(studbook) {
  footer <- demog_summary_total(studbook = studbook) %>%
    ungroup() %>%
    mutate(r = round(r, digits = 2), N1 = NA) %>%
    select(
      Cohort_years,
      Sex         ,
      sex_col     ,
      r           ,
      N1          ,
      Lx1         ,
      Qx          ,
      MLE         ,
      ex          ,
      repro_first ,
      repro_last  ,
      age_max     ,
      T           ,
      Fx          ,
      R0
    )
}

#' Create a list of column definitions for projection reactable
#'
#' @importFrom reactable colDef
#'
#' @noRd
#'
rct_cols_proj <- function(data) {
  list(
    Cohort_years = Cohort_years(),
    Sex          = Sex(),
    r            = intrins_rate(),
    N1           = n1(data),
    Lx1          = lx1(data),
    Qx           = qx(data),
    MLE          = MLE(data),
    ex           = ex(data),
    ex_ts           = ex(data),
    repro_first  = age_ranges(data),
    T            = gen_T(data),
    Fx           = fx(data),
    R0           = r0(data),
    sex_col      = colDef(show = FALSE),
    repro_last   = colDef(show = FALSE),
    age_max      = colDef(show = FALSE)
  )
}



#' Create a list of column definitions for demography reactable
#'
#' @importFrom reactable colDef
#'
#' @noRd
#'
rct_cols_demog <- function(data) {
  list(
    Cohort_years = Cohort_years(),
    Sex          = Sex(),
    r            = intrins_rate(),
    N1           = n1(data),
    Lx1          = lx1(data),
    Qx           = qx(data),
    MLE          = MLE(data),
    ex           = ex(data),
    repro_first  = age_ranges(data),
    T            = gen_T(data),
    Fx           = fx(data),
    R0           = r0(data),
    sex_col      = colDef(show = FALSE),
    repro_last   = colDef(show = FALSE),
    age_max      = colDef(show = FALSE)
  )
}

#' Prepare a demographic tibble for rendering in reactable
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#'
#' @importFrom dplyr ungroup mutate select bind_rows
#'
#' @noRd
#'
rct_prep_demog <- function(studbook, cohort_params = NULL) {
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  footer <- foot_demog(studbook = studbook)
  data <- demog_summary(studbook, cohort_params = params) %>%
    ungroup() %>%
    mutate(r = round(r, digits = 2)) %>%
    select(
      Cohort_years,
      Sex         ,
      sex_col     ,
      r           ,
      N1          ,
      Lx1         ,
      Qx          ,
      MLE         ,
      ex          ,
      repro_first ,
      repro_last  ,
      age_max     ,
      T           ,
      Fx          ,
      R0
    ) %>%
    bind_rows(footer)
  return(data)

}


#' Create a reactable summary table of demographic stats using a formatted studbook
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param data An optional tibble to add directly to the reactable for rendering (default is to construct this from studbook)
#' @param colDefs An optional list of `colDef` functions to render (default is created using internal function `rct_cols_demog`)
#'
#' @importFrom reactable reactable colDef
#' @importFrom dplyr mutate ungroup select bind_rows
#' @importFrom htmlwidgets prependContent JS
#' @importFrom htmltools HTML tags
#'
#' @export
#'
rct_demog <- function(studbook, cohort_params = NULL, data = NULL, colDefs = NULL) {
  colors   <- set_colors()
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  if (is.null(data)) {
    data  <- rct_prep_demog(studbook, params)
  }
  if (is.null(colDefs)) {
    colDefs <- rct_cols_demog(data)
  }
  table <- reactable(
    data,
    height        = 750,
    pagination    = FALSE,
    sortable      = FALSE,
    highlight     = TRUE,
    compact       = TRUE,
    borderless    = TRUE,
    style         = list(fontFamily = "Veranda, sans-serif", fontVariant = "small-caps"),
    rowStyle      = rows_bySex(),
    defaultColDef = colDef(
         vAlign       = "center",
         headerVAlign = "bottom",
         html         = TRUE,
         footerStyle = list(
           fontFamily    = "Veranda, sans-serif",
           fontVariant   = "small-caps",
           fontWeight    = "bold"
           ),
         headerStyle = list(
           fontFamily    = "Veranda, sans-serif",
           fontVariant   = "small-caps",
           background    = "#99999926",
           fontWeight    = "bold"
           )
         ),
  rowClass = JS("
    function(rowInfo) {
      return rowInfo.values.Sex === 'Summary'
        ? 'sticky-footer'
        : null;
    }
  "),
    columns = colDefs
  )

  css <- "
.reactable .sticky-footer {
  position: sticky;
  bottom: 0;
  background: white;
  z-index: 1;
}
"
  result <- prependContent(table, tags$style(HTML(css)))

  return(result)
}
