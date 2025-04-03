
# reactables.R
# Reactable column definitions and summary table generators

#' Reactable Column Defs
#'
#' These helper functions generate formatted column definitions for `reactable`
#' tables to summarize studbook, pedigree, and demographic data.
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr color_tiles
#' @importFrom tippy tippy
ID <- function(df) {
  colDef(
    header   = tippy("ID", tooltip = "Studbook ID color-coded by sex (maroon = F, blue = M, green = Undetermined)"),
    maxWidth = 70,
    cell     = color_tiles(
      data       = df,
      color_ref  = "color",
      box_shadow = TRUE,
      bold_text  = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom htmlwidgets JS
#' @importFrom reactable colDef
#' @importFrom tippy tippy
Status <- function() {
  colDef(
    header   = tippy("Status", tooltip = "Alive or Deceased"),
    maxWidth = 100,
    style    = JS("
      function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        if (!firstSorted || firstSorted.id === 'Status') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Status'] === prevRow['Status']) {
            return { visibility: 'hidden' }
          }
        }
      }")
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr color_tiles
#' @importFrom scales label_date
#' @importFrom tippy tippy
Date_birth <- function(df, colors = set_colors()) {
  colDef(
    header   = tippy("Birthdate", tooltip = "Date of birth (captive-born) or capture (wild-born)"),
    maxWidth = 200,
    cell     = color_tiles(
      data                = df,
      colors              = colors[["seq"]],
      opacity             = 0.4,
      color_by            = "Year_birth",
      brighten_text_color = "black",
      box_shadow          = TRUE,
      number_fmt          = label_date()
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr color_tiles
#' @importFrom scales label_date
#' @importFrom tippy tippy
Date_last <- function(df, colors = set_colors()) {
  colDef(
    header   = tippy("Death Date", tooltip = "Date of death (NA for living individuals)"),
    maxWidth = 200,
    cell     = color_tiles(
      data                = df,
      colors              = colors[["seq"]],
      opacity             = 0.4,
      color_by            = "Year_last",
      brighten_text_color = "black",
      box_shadow          = TRUE,
      number_fmt          = label_date()
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom htmltools div htmltools span
#' @importFrom reactable colDef
#' @importFrom tippy tippy
Loc_birth <- function(df) {
  colDef(
    header   = tippy("Born", tooltip = "Location of birth (captive-born) or capture (wild-born)"),
    maxWidth = 100,
    cell     = function(value, index) {
      flag <- df$iconLoc_birth[index]
      name <- span(style = "text-decoration: underline; text-decoration-style: dotted",
                   tippy(value, df$Institution_birth[index]))
      date <- div(style = "display:inline",
                  span(style = "font-weight: 600; font-size:12pt", df$Year_birth[index]),
                  span(style = "font-size:10pt", paste0(" (", df$Month_birth[index], ")")))
      div(style = "display:grid; row-gap:2px", div(style = "display:inline-block", flag, name), date)
    }
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom htmltools div span
#' @importFrom reactable colDef
#' @importFrom tippy tippy
Loc_last <- function(df) {
  colDef(
    header   = tippy("Last", tooltip = "Current institution (Alive) or institution and date of death (Deceased)"),
    maxWidth = 100,
    cell     = function(value, index) {
      flag <- df$iconLoc_last[index]
      name <- span(style = "text-decoration: underline; text-decoration-style: dotted",
                   tippy(value, df$Institution_last[index]))
      date <- div(style = "font-weight: 600; font-size:12pt", df$Date_last[index])
      div(style = "display:grid; row-gap:2px", div(style = "display:inline-block", flag, name), date)
    }
  )
}

#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom htmltools div span
#' @importFrom reactable colDef
#' @importFrom tippy tippy
Current_Location <- function(df) {
  colDef(
    header   = tippy("Current Location", tooltip = "Current institution"),
    maxWidth = 70,
    cell     = function(value, index) {
      flag <- df$iconLoc_last[index]
      name <- span(style = "text-decoration: underline; text-decoration-style: dotted",
                   tippy(value, df$Institution_last[index]))
      div(style = "display:inline-block", flag, name)
    }
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr pill_buttons
#' @importFrom scales label_date
#' @importFrom tippy tippy
age_last <- function(df, colors = set_colors()) {
  colDef(
    header   = tippy("Age", tooltip = "Now (Alive) or at time of death (Deceased)"),
    maxWidth = 50,
    align    = "center",
    cell     = pill_buttons(
      data                = df,
      colors              = colors[["seq"]],
      opacity             = 0.4,
      brighten_text_color = "black",
      box_shadow          = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#' @param name The name of the column to be shown in the table
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr bubble_grid
#' @importFrom tippy tippy
bubble_count <- function(df, name, colors = set_colors()) {
  colDef(
    name     = name,
    maxWidth = 250,
    align    = "center",
    cell     = bubble_grid(
      data          = df,
      text_color    = "#ffffff",
      bold_text     = TRUE,
      brighten_text = TRUE,
      colors        = colors[["seq"]]
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr pill_buttons
#' @importFrom tippy tippy
Sire <- function(df, colors = set_colors()) {
  colDef(
    header   = tippy("Father", tooltip = "Studbook ID of Sire (0 if wildborn or unknown)"),
    maxWidth = 70,
    cell     = pill_buttons(
      data                = df,
      colors              = colors[["sire"]],
      opacity             = 0.6,
      brighten_text_color = "black",
      box_shadow          = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr pill_buttons
#' @importFrom tippy tippy
Dam <- function(df, colors = set_colors()) {
  colDef(
    header   = tippy("Mother", tooltip = "Studbook ID of Dam (0 if wildborn or unknown)"),
    maxWidth = 70,
    cell     = pill_buttons(
      data                = df,
      colors              = colors[["dam"]],
      opacity             = 0.6,
      brighten_text_color = "black",
      box_shadow          = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr data_bars
#' @importFrom scales label_percent
#' @importFrom tippy tippy
Rel_Contribution <- function(df, colors = set_colors()) {
  colDef(
    header   = tippy("Relative Contribution", tooltip = "Individual's contribution to living population relative to total founder representation"),
    maxWidth = 200,
    cell     = data_bars(
      data          = df,
      text_position = "outside-base",
      fill_color    = colors[["seq"]],
      number_fmt    = label_percent(),
      background    = "white",
      box_shadow    = TRUE
    )
  )
}
#'
#' @param df A data frame passed to the reactable column definition
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr data_bars
#' @importFrom scales label_number
#' @importFrom tippy tippy
inbred <- function(df, colors = set_colors()) {
  colDef(
    header   = tippy("F", tooltip = "Inbreeding coefficient: probability two alleles are identical by descent"),
    align    = "center",
    maxWidth = 300,
    cell     = data_bars(
      data          = df,
      text_position = "outside-end",
      box_shadow    = TRUE,
      fill_color    = colors[["div"]],
      background    = "#ffffff00",
      text_color    = "#ffffff",
      bold_text     = TRUE,
      number_fmt    = label_number(accuracy = 0.001),
      bar_height    = 8
    )
  )
}
# reactable_col_lists.R
# Column list generators for use in reactable tables

#' Column definitions for full studbook summary tables
#'
#' @param df A filtered founder summary dataframe
#' @return A named list of column definitions
#' @export
#'
#' @importFrom reactable colDef
studbook.cols <- function(df) {
  list(
    Status            = Status(),
    ID                = ID(df),
    Loc_birth         = Loc_birth(df),
    age_last          = age_last(df),
    Loc_last          = Loc_last(df),
    Sire              = Sire(df),
    Dam               = Dam(df),
    Date_last         = colDef(show = FALSE),
    Date_birth        = colDef(show = FALSE),
    Sex               = colDef(show = FALSE),
    color             = colDef(show = FALSE),
    Year_birth        = colDef(show = FALSE),
    Month_birth       = colDef(show = FALSE),
    Year_last         = colDef(show = FALSE),
    Date_last         = colDef(show = FALSE),
    iconLoc_birth     = colDef(show = FALSE),
    colorLoc_birth    = colDef(show = FALSE),
    iconLoc_last      = colDef(show = FALSE),
    colorLoc_last     = colDef(show = FALSE),
    Institution_birth = colDef(show = FALSE),
    Institution_last  = colDef(show = FALSE),
    sex_ped           = colDef(show = FALSE),
    sex_kinship       = colDef(show = FALSE)
  )
}

#' Column definitions for founder summary tables
#'
#' @param df A filtered founder summary dataframe
#' @return A named list of column definitions
#' @export
#'
#' @importFrom reactable colDef
founder.cols <- function(df) {
  list(
    Status            = Status(),
    ID                = ID(df),
    Loc_birth         = Loc_birth(df),
    age_last          = age_last(df),
    Loc_last          = Loc_last(df),
    Rel_Contribution  = Rel_Contribution(df),
    Sire              = colDef(show = FALSE),
    Dam               = colDef(show = FALSE),
    Date_last         = colDef(show = FALSE),
    Date_birth        = colDef(show = FALSE),
    Sex               = colDef(show = FALSE),
    color             = colDef(show = FALSE),
    Year_birth        = colDef(show = FALSE),
    Month_birth       = colDef(show = FALSE),
    Year_last         = colDef(show = FALSE),
    Date_last         = colDef(show = FALSE),
    iconLoc_birth     = colDef(show = FALSE),
    colorLoc_birth    = colDef(show = FALSE),
    iconLoc_last      = colDef(show = FALSE),
    colorLoc_last     = colDef(show = FALSE),
    Institution_birth = colDef(show = FALSE),
    Institution_last  = colDef(show = FALSE),
    sex_ped           = colDef(show = FALSE),
    sex_kinship       = colDef(show = FALSE)
  )
}

#' Column definitions for living individual summaries
#'
#' @param df A living population summary dataframe
#' @return A named list of column definitions
#' @export
#'
#' @importFrom reactable colDef
living.cols <- function(df) {
  list(
    LocCurrent        = Current_Location(df),
    ID                = ID(df),
    Loc_birth         = Loc_birth(df),
    Age               = age_last(df),
    Sire              = Sire(df),
    Dam               = Dam(df),
    inbred            = inbred(df),
    N_Ancestors       = bubble_count(df, "Ancestors"),
    N_Descendants     = bubble_count(df, "Descendants"),
    N_Children        = bubble_count(df, "Offspring"),
    N_Siblings        = bubble_count(df, "Siblings"),
    Date_birth        = colDef(show = FALSE),
    Sex               = colDef(show = FALSE),
    color             = colDef(show = FALSE),
    Year_birth        = colDef(show = FALSE),
    Month_birth       = colDef(show = FALSE),
    Year_last         = colDef(show = FALSE),
    iconLoc_birth     = colDef(show = FALSE),
    colorLoc_birth    = colDef(show = FALSE),
    iconLoc_last      = colDef(show = FALSE),
    colorLoc_last     = colDef(show = FALSE),
    Institution_birth = colDef(show = FALSE),
    Institution_last  = colDef(show = FALSE),
    sex_ped           = colDef(show = FALSE),
    sex_kinship       = colDef(show = FALSE)
  )
}

#' Column groupings for living summary table
#'
#' @return A named list of column groups
#' @export
#'
#' @importFrom reactable colGroup
kin.group <- function() {
  colGroup(name = "Cumulative Counts", columns = c("N_Ancestors", "N_Children", "N_Siblings"))
}

#' Column definitions for kinship summary tables
#'
#' @return A named list of column definitions
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactable colFormat
#' @importFrom tippy tippy
kin.cols <- function() {
  list(
    N              = colDef(header = tippy("N"          , tooltip = "Number of currently living individuals" )),
    Ne             = colDef(header = tippy("Ne"         , tooltip = "Effective population size"              ), format = colFormat(digits = 0)),
    Ne_over_N      = colDef(header = tippy("Ne/N"       , tooltip = "Effective population size relative to N"), format = colFormat(digits = 1)),
    n_founder_reps = colDef(header = tippy("Founders"   , tooltip = "Number of represented founders"         )),
    FGE            = colDef(header = tippy("FGE"        , tooltip = "Founder genome equivalents"             ), format = colFormat(digits = 1)),
    F_mean         = colDef(header = tippy("F"          , tooltip = "Mean inbreeding coefficient"            )),
    mean_gen       = colDef(header = tippy("Generations", tooltip = "Mean generation of living population"   ), format = colFormat(digits = 1)),
    delta_F        = colDef(header = tippy("\u0394F"    , tooltip = "Per-generation inbreeding increase"     )),
    GD             = colDef(header = tippy("GD"         , tooltip = "Gene diversity retained"                ), format = colFormat(percent = TRUE)),
    MK             = colDef(header = tippy("MK"         , tooltip = "Population mean kinship"                ))
  )
}

# reactable_helpers.R
# High-level table generators and column sets for zoo biology summaries

#' Create a reactable summary table from any dataset
#'
#' @param df The data frame to summarize
#' @param cols A named list of column definitions
#' @param ... Additional arguments passed to `reactable()`
#' @return A reactable widget
#' @export
#' @importFrom reactable reactable
#' @importFrom reactablefmtr flatly
studbook_react <- function(df, cols, ...) {
  reactable(
    df,
    fullWidth           = TRUE,
    theme               = flatly(centered = TRUE),
    height              = 700,
    sortable            = TRUE,
    resizable           = TRUE,
    filterable          = TRUE,
    defaultExpanded     = TRUE,
    defaultPageSize     = 20,
    showPageSizeOptions = TRUE,
    highlight           = TRUE,
    columns             = cols,
    ...
  )
}

