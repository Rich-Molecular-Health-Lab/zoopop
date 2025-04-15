# new_studbook.R

#' Estimate the Midpoint Date Between Two Dates
#'
#' Computes an estimated date that is approximately halfway between \code{date1} and \code{date2}.
#'
#' @param date1 A date object representing the start date.
#' @param date2 A date object representing the end date.
#' @return A date object corresponding to the midpoint between \code{date1} and \code{date2}.
#' @importFrom lubridate days interval as.period
#' @export
#'
est_date_btn <- function(date1, date2) {
  date1 + days(floor(as.numeric(as.period(lubridate::interval(date1, date2), unit = "days"), "days")/2))
}

#' Calculate Age
#'
#' Computes the age in whole years based on a given birth date and a reference date.
#'
#' @param birth A date object representing the birth date.
#' @param date A date object representing the reference date.
#' @return An integer representing the age in whole years.
#' @importFrom lubridate floor_date interval as.period
#' @export
calculate_age <- function(birth, date) {
  floor(as.numeric(as.period(lubridate::interval(floor_date(birth, "month"), date), unit = "years"), "years"))
}

#' Read and Process Location Data
#'
#' Reads a CSV file containing location data, processes the data using a specified color palette, writes the processed data to an output CSV file, and returns the resulting data frame.
#'
#' @param file_in A file path to the input CSV file. This file must include the columns: \code{Institution Name}, \code{Mnemonic}, \code{Country}, and \code{State_Province}. Use a tool such as Adobe Acrobat to extract institution lists from the published studbook report, then edit the file so that every location in your studbook has a matching \code{Mnemonic}.
#' @param file_out A file path to the output CSV file.
#' @param palette A list of colors to assign to each location value (optional)
#' @return A data frame with processed location data.
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate distinct arrange if_else left_join pull right_join lead
#' @importFrom stringr str_squish str_remove_all str_sub str_replace str_to_upper
#' @importFrom tidyr replace_na
#' @importFrom countrycode countrycode
#' @importFrom purrr set_names map
#' @importFrom tibble enframe
#' @importFrom magrittr %>%
#' @export
#'
read_locations <- function(file_in, file_out, palette = NULL) {
  if (is.null(palette)) {
    cols    <- set_colors()
    palette <- cols[["rnd"]]
  }
  locs <- read_csv(file_in) %>%
    mutate(across(where(is.character), ~str_squish(.))) %>%
    mutate(Location = str_remove_all(Mnemonic, "[^\\w+]"), .keep = "unused") %>%
    distinct() %>%
    arrange(Location) %>%
    mutate(code1 = as.character(str_sub(Location, 1L, 3L)),
           code2 = as.character(str_sub(Location, -2L, -1L))) %>%
    mutate(code = if_else(!is.na(lead(code1)) & code1 == lead(code1),
                          str_replace(code1, "(?<=^\\w)\\w{2}$", code2), code1), .keep = "unused") %>%
    mutate(code_country = countrycode(Country, origin = "country.name", destination = "iso3c"),
           iconLoc = countrycode(code_country, origin = "iso3c", destination = "unicode.symbol")) %>%
    mutate(iconLoc = replace_na(iconLoc, "\u3f"),
           code_country = replace_na(code_country, "UND"),
           code = str_to_upper(code))

  loc_vals <- pull(locs, code) %>% unique() %>% as.list()

  df <- as.list(
    sample(palette, size = length(loc_vals), replace = FALSE)
    ) %>%
    set_names(., map(loc_vals, \(x) paste0(x))) %>%
    enframe(name = "code", value = "colorLoc") %>%
    mutate(colorLoc = as.character(colorLoc),
           code = as.character(code)) %>%
    right_join(locs, by = join_by(code)) %>%
    select(
      code,
      Location,
      Institution = "Institution Name",
      State_Province,
      Country,
      code_country,
      iconLoc,
      colorLoc
    )
  write_csv(df, file_out)
  return(df)
}

#' Read and Process BTP Data
#'
#' Reads a CSV file containing Breeding and Transfer Plan (BTP) data, processes the data, writes an updated BTP summary to an output CSV file, and returns the resulting data frame.
#'
#' @param file_in A file path to the input CSV file created by converting a published BTP to a CSV table. The file must include the columns: \code{BTP}, \code{Institution}, \code{SB ID}, \code{Sex}, \code{Age}, \code{Disposition}, \code{Location}, \code{Breeding}, \code{With}, \code{Notes}, and \code{Facility_Note}. (You should add the \code{BTP} column manually to indicate the 4-digit publication year.)
#' @param file_out A file path to the output CSV file that will contain the processed BTP summary.
#' @param loc_key A data frame containing location key mappings produced by \code{read_locations}.
#' @return A data frame with processed BTP data.
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr rename mutate case_when if_else group_by arrange select distinct left_join join_by case_match
#' @importFrom stringr str_extract_all str_remove_all str_glue str_detect
#' @importFrom tidyr fill pivot_wider
#' @importFrom tidyselect starts_with
#' @importFrom lubridate make_date
#' @export
#'
read_btp <- function(file_in, file_out, loc_key) {
  df <- read_csv(file_in) %>%
    rename(ID = "SB ID") %>%
    mutate(facility = str_extract_all(Institution, "^.+(?=\\s[:upper:][:lower:])"),
           note = str_remove_all(Facility_Note, "Facility Note: "),
           age = as.numeric(Age),
           .keep = "unused") %>%
    mutate(Year_birth = BTP - age,
           exclude = case_when(
             str_detect(Notes, "Excluded") & str_detect(Notes, "Age") ~ "age",
             str_detect(Notes, "Excluded") & str_detect(Notes, "Behavior") ~ "behavior",
             str_detect(Notes, "Died") ~ "deceased",
             .default = "n"),
           disposition = case_match(
             Disposition,
             "HOLD" ~ "current",
             "RECEIVE FROM" ~ "current",
             "SEND TO" ~ "transfer",
             .default = Disposition
           )
    ) %>%
    mutate(include = if_else(exclude == "n", "y", "n"),
           mate = if_else(is.na(With) & str_detect(Notes, "Breed"),
                          str_extract(Notes, "\\d+"), as.character(With))
    ) %>%
    mutate(mate = as.integer(mate)) %>%
    group_by(ID, BTP) %>%
    fill(note, .direction = "downup") %>%
    ungroup() %>%
    arrange(ID, BTP, facility) %>%
    pivot_wider(names_from = "disposition", values_from = c("Location", "note")) %>%
    select(-facility) %>%
    group_by(ID, BTP) %>%
    fill(starts_with("Location_"), starts_with("note_"), .direction = "downup") %>%
    ungroup() %>%
    distinct() %>%
    mutate(note = case_when(
      note_current == note_transfer ~ note_current,
      is.na(note_current) ~ note_transfer,
      is.na(note_transfer) ~ note_current,
      !is.na(note_transfer) & !is.na(note_current) & note_current != note_transfer ~ str_glue("{note_current}", "{note_transfer}", sep = "; ")), .keep = "unused") %>%
    mutate(note = as.character(note),
           Location = str_remove_all(Location_current, "[^\\w+]"),
           transfer = str_remove_all(Location_transfer, "[^\\w+]"),
           Event = "btp",
           Date = make_date(year = BTP, month = 2, day = 1)) %>%
    select(ID,
           include,
           exclude,
           Sex,
           year = BTP,
           Date,
           Event,
           age,
           Year_birth,
           Location,
           transfer,
           breeding = Breeding,
           mate,
           note
    ) %>%
    distinct() %>%
    left_join(loc_key, by = join_by(Location)) %>%
    left_join(select(loc_key, transfer = Location, transfer_code = code),
              by = join_by(transfer))
  write_csv(df, file_out)
  return(df)
}

#' Label Names
#'
#' Adds names or other string values to a column based on unique, numerical IDs
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param names Factor labeling vector written as `c({String} = "{ID}")` for assigning each name to an ID number.
#' @return A data frame of studbook data now containing a column `label_special` with a string only for those IDs specified in the `names` parameter.
#' @importFrom dplyr mutate
#' @importFrom forcats fct_recode fct_other
#' @export
#'
label_names <- function(studbook, names = NULL) {

  if (!is.null(names)) {
    studbook %>%
      mutate(ID        = as.character(ID)) %>%
      mutate(name_spec = fct_recode(ID, !!!names)) %>%
      mutate(ID        = as.integer(ID)) %>%
      mutate(name_spec = fct_other(name_spec, keep = c(names(names)), other_level = ""))
  } else {return(studbook)}

}

#' Read and Process Studbook Data
#'
#' Reads a CSV file containing studbook data, processes it together with BTP data and location key mappings, and returns a cleaned studbook data frame. This function merges multiple exported files, removes redundant entries, and performs necessary corrections for downstream analyses.
#'
#' @param file_in A file path to the input CSV file. Export the studbook tables from the published PDF using Adobe Acrobat (or a similar tool), and ensure that the file contains the columns: \code{ID}, \code{Sex}, \code{Sire}, \code{Dam}, \code{Birth_Type}, \code{Event}, \code{Date}, and \code{Location}. Combine rows from multiple files as needed.
#' @param loc_key A data frame containing location key mappings produced by \code{read_locations}.
#' @param btp A data frame containing formatted BTP data produced by \code{read_btp}.
#' @param names Factor labeling vector written as `c({String} = "{ID}")` for assigning special name labels to selected ID numbers (optional - will return empty cells for all rows if not included).
#' @param add_births List provided to account for new births implied but not documented in most recent studbook. The top level list names should be an estimated date of birth. The next level should be the names `ID`, `Sire`, and `Dam` with a numeric ID value provided for each. (if `add_births` is not used, then this option is ignored)
#' @return A data frame with processed studbook data that can be used to resolve missing parental assignments.
#' @importFrom readr read_csv
#' @importFrom dplyr filter left_join rename mutate select distinct arrange group_by if_else bind_rows row_number lead join_by last case_when setdiff across consecutive_id pull first desc
#' @importFrom tidyr fill pivot_wider pivot_longer
#' @importFrom stringr str_squish str_extract str_remove_all str_to_lower str_replace_all str_glue str_sub
#' @importFrom lubridate dmy floor_date today years interval as.period
#' @importFrom forcats fct_recode
#' @importFrom tidyselect where
#' @export
#'
read_studbook <- function(file_in, loc_key, btp, names = NULL, add_births = NULL) {
  events <- c(
    birth    = "birth/hatch",
    capture  = "wild capture",
    transfer = "transfer",
    ltf      = "go ltf",
    death    = "death"
  )
  if (!is.null(add_births)) {
    add_births <- enframe(add_births, name = "Date") %>%
      unnest_wider(value) %>%
      mutate(Date = ymd(Date))
  }
  std <- read_csv(file_in) %>%
    mutate(across(where(is.character), ~str_squish(.))) %>%
    filter(Location != "Location") %>%
    fill(ID, Sex, Sire, Dam, Birth_Type) %>%
    mutate(Sex      = str_sub(Sex, 1L, 1L),
           Date     = dmy(str_extract(Date, "\\d{1,2}[-/]\\w{3}[-/]\\d{2,4}")),
           Location = str_remove_all(Location, "[^\\w+]"),
           across(c(Sire, Dam), ~as.integer(str_replace_all(., "WILD", "0"))),
           Type_birth = str_extract(Birth_Type, "^\\w+(?=\\s)"),
           Event      = str_to_lower(Event)
    ) %>%
    mutate(Event    = fct_recode(Event, !!!events),
           Location = if_else(Location == "Undetermined", "UNDETERMI", Location),
           Date     = if_else(Date > today(), Date - years(100), Date),
           ID       = as.integer(ID)) %>%
    select(
      ID,
      Sex,
      Sire,
      Dam,
      Type_birth,
      Event,
      Date,
      Location
    ) %>%
    left_join(loc_key, by = join_by(Location)) %>%
    mutate(loc_order = consecutive_id(code),
           Sex       = last(Sex), .by = ID)

  new_births <- setdiff(pull(btp, ID), pull(std, ID))

  living <- filter(btp, year == max(year) & exclude != "deceased") %>%
    pull(ID) %>% unique()

  loc_orders <- std %>%
    select(ID, code, loc_order) %>%
    distinct()

  btp_births <- btp %>%
    filter(ID %in% new_births) %>%
    select(ID, Sex, Date, Event, code) %>%
    arrange(ID, Date) %>%
    distinct() %>%
    bind_rows(add_births) %>%
    group_by(ID) %>%
    fill(Date, Sire, Event, code, Dam, .direction = "downup") %>%
    ungroup() %>%
    mutate(Date  = if_else(is.na(Date), floor_date(Date, "years"), Date),
           Event = "birth")

  btp_deaths <- btp %>%
    filter(exclude == "deceased") %>%
    select(ID, Sex, Date, Event, code) %>%
    arrange(ID, Date) %>%
    distinct() %>%
    mutate(Date = floor_date(Date, "years"))

  deaths <- std %>%
    filter(Event %in% c("ltf", "death")) %>%
    select(ID, Sex, Date, Event, code) %>%
    bind_rows(btp_deaths) %>%
    distinct() %>%
    pivot_wider(names_from = "Event", values_from = "Date") %>%
    mutate(Date = case_when(is.na(death) & is.na(btp) ~ ltf,
                            is.na(death) & is.na(ltf) ~ btp,
                            .default = death),
           Event = "death", .keep = "unused") %>%
    filter(!is.na(Date)) %>%
    arrange(ID, Date)

  birth_dates <- std %>%
    filter(Event %in% c("birth", "capture")) %>%
    select(ID, Sire, Dam, Sex, Date, Event, code) %>%
    distinct() %>%
    pivot_wider(names_from = "Event", values_from = "Date") %>%
    mutate(Date  = if_else(is.na(birth), capture, birth),
           Event = "birth", .keep = "unused") %>%
    bind_rows(btp_births) %>%
    arrange(ID, Date)

  births <- birth_dates %>%
    select(Sire, Dam, Date, Event, code, offspring = ID) %>%
    pivot_longer(cols = c("Sire", "Dam"), names_to = "Sex", values_to = "ID") %>%
    mutate(Sex = case_match(Sex, "Sire" ~ "M", "Dam" ~ "F"),
           Event = "breed") %>%
    filter(ID != 0 & !is.na(ID)) %>%
    select(ID, Sex, Date, Event, code) %>%
    arrange(ID, Date) %>%
    distinct()

  transfers <- std %>%
    filter(Event == "transfer") %>%
    select(ID, Sex, Date, Event, code) %>%
    arrange(ID, Date) %>%
    distinct()

  btps <- btp %>%
    select(ID, Sex, Date, Event, code) %>%
    arrange(ID, Date) %>%
    distinct()

  event_fact <- c("birth", "capture", "transfer", "breed", "btp", "death")
  studbook   <- bind_rows(birth_dates, births, transfers, btps, deaths) %>%
    mutate(Event = factor(Event, levels = event_fact)) %>%
    left_join(loc_orders, by = join_by(ID, code)) %>%
    arrange(ID, loc_order, Event, Date) %>%
    group_by(ID) %>%
    fill(Sire, Dam, Sex, .direction = "downup") %>%
    mutate(loc_order = consecutive_id(code),
           Status = if_else(ID %in% living, "A", "D")) %>%
    mutate(Date_last = case_when(Status == "D" & Event == "death" ~ Date,
                                 Status == "A" ~ today())) %>%
    fill(Date_last, .direction = "up") %>%
    mutate(Date_last = if_else(is.na(Date_last), last(Date) + years(1), Date_last),
           Loc_last  = last(code)) %>%
    filter(Event != "death") %>%
    mutate(Date_birth = if_else(Event == "birth", Date, NA),
           Loc_birth  = if_else(Event == "birth", code, NA)) %>%
    fill(Date_birth, Loc_birth) %>%
    filter(Event != "birth") %>%
    distinct() %>%
    select(ID,
           Date_birth,
           Date,
           Event,
           Date_last,
           Status,
           Loc_event = code,
           Loc_birth,
           Loc_last,
           Sex,
           Sire,
           Dam) %>%
    mutate(Date = case_when(
      is.na(Date) & row_number() == 1 & !is.na(Date_birth) ~ est_date_btn(lead(Date), Date_birth),
      is.na(Date) & row_number() == 1 &  is.na(Date_birth) ~ lead(Date) - years(1),
      .default = Date
    )) %>%
    mutate(Date_birth = if_else(is.na(Date_birth), first(Date) - years(2), Date_birth)) %>%
    fill(Date_birth) %>%
    mutate(age_event = calculate_age(Date_birth, Date),
           age_last  = calculate_age(Date_birth, Date_last)) %>%
    left_join(select(std, ID, Type_birth), by = join_by(ID)) %>%
    left_join(select(btp, ID, Date, Event, exclude), by = join_by(ID, Date, Event)) %>%
    fill(exclude) %>%
    mutate(exclude = case_when(Status == "D" ~ "deceased",
                               Status == "A" & Event == "breed" ~ "n",
                               .default = exclude),
           Type_birth = replace_na(Type_birth, "Undetermined"),
           Year_birth = year(floor_date(Date_birth, "years")),
           across(c(Loc_event, Loc_birth, Loc_last), ~replace_na(., "UND"))) %>%
    fill(exclude, Type_birth, .direction = "downup") %>%
    mutate(across(c(Sire, Dam), ~if_else(is.na(.) & Type_birth == "Undetermined", 0, .))) %>%
    mutate(Sire = if_else(!is.na(Dam) & Dam != 0  & Sire == 0, NA, Sire),
           Dam  = if_else(!is.na(Sire) & Sire != 0 & Dam  == 0, NA, Dam)) %>%
    mutate(Sex = last(Sex)) %>%
    ungroup() %>%
    left_join(loc_key, by = join_by(Loc_birth == code)) %>%
    rename(Institution_birth = Institution,
           State_Province_birth = State_Province,
           Country_birth = Country,
           iconLoc_birth = iconLoc,
           colorLoc_birth = colorLoc) %>%
    select(-c(code_country, Location)) %>%
    left_join(loc_key, by = join_by(Loc_last == code)) %>%
    rename(Institution_last = Institution,
           State_Province_last = State_Province,
           Country_last = Country,
           iconLoc_last = iconLoc,
           colorLoc_last = colorLoc) %>%
    select(-c(code_country, Location)) %>%
    label_names(c(Culi = "2652", Warble = "2677")) %>%
    relocate(name_spec, .after = ID) %>%
    ungroup() %>%
    arrange(ID, Date) %>%
    distinct()

  return(studbook)
}

#' Create a Short Version of the Studbook
#'
#' Generates a condensed version of the studbook by reducing multiple records per individual to a single row.
#'
#' @param studbook A data frame containing the full studbook data (with multiple rows per individual) produced by \code{read_studbook}.
#' @return A data frame in which each unique individual ID is represented by exactly one row.
#' @importFrom dplyr arrange select distinct
#' @importFrom tidyselect ends_with
#' @export
#'
studbook_short <- function(studbook) {
  studbook %>%
    arrange(Date) %>%
    select(
      ID,
      name_spec,
      Sex,
      Status,
      Sire,
      Dam,
      ends_with("_birth"),
      ends_with("_last"),
      exclude
    ) %>%
    arrange(ID) %>%
    mutate(exclude = last(exclude), .by = ID) %>%
    distinct()
}

#' Generate Location Census Data
#'
#' Creates an annual census of locations based on studbook data and location key mappings. The result is a nested list organized by the floor date (January 1) for each year represented in the data. Each year contains a list of locations, with each location represented by a tibble listing the individuals recorded there along with their sex and age.
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param loc_key A data frame containing location key mappings produced by \code{read_locations}.
#' @return A nested list of location census data organized by year.
#' @importFrom dplyr filter select distinct arrange group_by mutate left_join summarize if_else row_number
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom purrr pmap map
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
#' @export
#'
location_census <- function(studbook, loc_key) {
  locations <- studbook %>%
    filter(Event == "transfer") %>%
    select(
      ID,
      Loc_birth,
      Date_birth,
      Loc_last,
      Date_last,
      Date_event = Date,
      Loc_event,
      Sex
    ) %>%
    distinct() %>%
    arrange(ID, Date_event) %>%
    group_by(ID) %>%
    mutate(Loc_start = if_else(row_number() == 1, Date_birth, NA),
           Loc_end   = if_else(row_number() == max(row_number()), Date_last, lead(Date_event))) %>%
    group_by(ID, Loc_event) %>%
    mutate(Loc_start = if_else(is.na(Loc_start), min(Date_event), Loc_start)) %>%
    ungroup() %>%
    mutate(Start = floor_date(Loc_start, "years"),
           End   = ceiling_date(Loc_end, "years")) %>%
    select(
      ID,
      Sex,
      Date_birth,
      code = Loc_event,
      Start,
      End
    ) %>%
    left_join(loc_key, by = join_by(code)) %>%
    mutate(Dates = pmap(list(Start, End), \(x, y) seq(x, y, by = "years"))) %>%
    unnest(Dates) %>%
    mutate(Date     = Dates,
           Location = code,
           Age      = calculate_age(Date_birth, Date)) %>%
    group_by(Date, Location) %>%
    summarize(Individuals = list(tibble(ID, Sex, Age))) %>%
    group_by(Date) %>%
    summarize(Locations   = split(Individuals, Location)) %>%
    split(.$Date) %>%
    map(~ .x$Locations)
  return(locations)
}

#' Match Birth Records by Parent Sex
#'
#' Filters and orders candidate birth records to identify potential parent matches based on the specified sex.
#'
#' @param x A data frame or tibble of candidate birth records.
#' @param sex_parent A character string indicating the parent's sex ("M" or "F").
#' @return A tibble of matching birth records sorted in descending order by age, or a tibble with default values if \code{x} is \code{NULL}.
#' @importFrom dplyr filter arrange distinct
#' @importFrom tibble tibble
#' @export
#'
match_births <- function(x, sex_parent) {
  if (is.null(x)) {
    return(tibble(ID = NA, Sex = "None", Age = 0))
  } else {
    filter(x, Sex == sex_parent) %>%
      ungroup() %>%
      arrange(desc(Age)) %>%
      distinct()
  }
}

#' Retrieve Detailed Parent Information
#'
#' Joins candidate parent records with a condensed version of the studbook to provide detailed information for potential parental assignments. This function is intended for internal use by \code{find_parent}.
#'
#' @param x A data frame containing candidate parent records.
#' @param studbook A data frame containing the full studbook data.
#' @return A tibble with detailed parent information.
#' @importFrom dplyr left_join distinct join_by
#' @importFrom tibble tibble
#' @export
parent_details <- function(x, studbook) {
  studbook_short <- studbook_short(studbook)
  if (is.null(x) | nrow(x) < 1) {
    return(tibble())
  } else {
    left_join(x, studbook_short, by = join_by(ID, Sex)) %>%
      distinct()
  }
}

#' Find Parent Information from the Studbook
#'
#' Searches the studbook for potential parent candidates (sire or dam) based on the birth institution and birth year, and returns matching details. This function assists in identifying likely paternity or maternity assignments.
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param loc_key A data frame containing location key mappings produced by \code{read_locations}.
#' @param parent A character string indicating the parent type (e.g., "Sire", "sire", "dad", "father" for males; analogous values for females).
#' @return A list of tibbles containing matched parent details. Each tibble corresponds to an individual and lists potential parent matches.
#' @importFrom dplyr filter mutate relocate distinct pull select
#' @importFrom tibble deframe
#' @importFrom lubridate floor_date
#' @importFrom purrr imap pluck set_names map_depth
#' @export
#'
find_parent <- function(studbook, loc_key, parent) {
  if (parent %in% c("Sire", "sire", "dad", "father")) {
    sex_parent <- "M"
    subjects <- studbook_short(studbook) %>%
      filter(Type_birth != "Wild" & is.na(Sire)) %>%
      mutate(Date = floor_date(Date_birth, unit = "years")) %>%
      relocate(Sire, Dam, .after = Loc_birth) %>%
      distinct()
  } else {
    sex_parent <- "F"
    subjects <- studbook_short(studbook) %>%
      filter(Type_birth != "Wild" & is.na(Dam)) %>%
      mutate(Date = floor_date(Date_birth, unit = "years")) %>%
      relocate(Sire, Dam, .after = Loc_birth) %>%
      distinct()
  }
  names      <- pull(subjects, ID) %>% unique()
  loc_census <- location_census(studbook, loc_key)
  search     <- as.list(deframe(select(subjects, Date, Loc_birth)))
  imap(search, \(x, idx) pluck(loc_census, idx, x, 1)) %>%
    set_names(., as.list(deframe(select(subjects, ID)))) %>%
    map_depth(., 1, \(x) match_births(x, sex_parent)) %>%
    map_depth(., 1, \(x) parent_details(x, studbook))
}

#' Add Hypothetical Parent Entries to the Studbook
#'
#' Creates a hypothetical parent entry (sire or dam) for the specified offspring IDs and returns a single-row data frame representing the new entry. This function is intended for internal use by \code{add_all_hypotheticals}.
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param parent A character string indicating the parent type (e.g., "Sire", "sire", "dad", "father" for sires; analogous values for dams).
#' @param ids A vector of one or more IDs for which a hypothetical parent should be created.
#' @param loc_key A data frame containing location key mappings produced by \code{read_locations}.
#' @return A data frame with one row representing the new hypothetical parent entry.
#' @importFrom dplyr filter arrange select bind_cols mutate left_join bind_rows distinct slice_head
#' @importFrom tibble tibble
#' @importFrom lubridate year
#' @export
#'
add_hypotheticals <- function(studbook, parent, ids, loc_key) {
  if (parent %in% c("Sire", "sire", "dad", "father")) {
    sex_parent <- "M"
    add <- 10000
    offspring_dads <- filter(studbook, ID %in% ids) %>%
      mutate(Sire = min(ids) + 10000)
    offspring_moms <- filter(studbook, ID %in% ids)
  } else {
    sex_parent <- "F"
    add <- 20000
    offspring_dads <- filter(studbook, ID %in% ids)
    offspring_moms <- filter(studbook, ID %in% ids) %>%
      mutate(Dam = min(ids) + 20000)
  }
  hypSire <- min(ids) + 10000
  hypDam  <- min(ids) + 20000

  hyp_cols <- tibble(
    Event       = "breed",
    Status      = "H",
    Loc_birth   = "UND",
    Sex         = sex_parent,
    Sire        = 0,
    Dam         = 0,
    age_event   = 2,
    Type_birth  = "Undetermined",
    exclude     = "hypothetical",
    name_spec   = ""
  )

  offspring <- bind_rows(offspring_dads, offspring_moms) %>%
    group_by(ID) %>%
    fill(Sire, Dam, .direction = "downup") %>%
    ungroup() %>%
    distinct()

  hypotheticals <- studbook %>%
    filter(ID %in% ids) %>%
    arrange(Date_birth) %>%
    select(Date                = Date_birth,
           Loc_event           = Loc_birth,
           Institution_last    = Institution_birth,
           State_Province_last = State_Province_birth,
           Country_last        = Country_birth,
           iconLoc_last        = iconLoc_birth,
           colorLoc_last       = colorLoc_birth) %>%
    bind_cols(hyp_cols) %>%
    mutate(ID         = min(ids) + add,
           Date_birth = first(Date) - years(2),
           Loc_last   = Loc_event,
           Date_last  = last(Date) + years(2)) %>%
    mutate(age_last   = calculate_age(Date_birth, Date_last),
           Year_birth = year(Date_birth)) %>%
    left_join(select(loc_key,
                     Loc_birth            = code,
                     Institution_birth    = Institution,
                     State_Province_birth = State_Province,
                     Country_birth        = Country,
                     iconLoc_birth        = iconLoc,
                     colorLoc_birth       = colorLoc), by = join_by(Loc_birth)) %>%
    bind_rows(slice_head(., n = 1)) %>%
    bind_rows(offspring) %>%
    group_by(ID) %>%
    fill(Sire, Dam, .direction = "downup") %>%
    ungroup() %>%
    distinct()

  return(hypotheticals)
}

#' Add All Hypothetical Parent Entries to the Studbook
#'
#' Combines hypothetical parent entries for both sires and dams and appends them to the studbook. This function processes a list of missing parentage definitions and returns an updated studbook with new hypothetical entries.
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param loc_key A data frame containing location key mappings produced by \code{read_locations}.
#' @param hyp_defs A list containing definitions for individuals that remain missing a parental assignment after manual matching using \code{find_parent}.
#' @return A data frame with all hypothetical parent entries added as new rows and assigned to the relevant offspring.
#' @importFrom purrr map_depth list_rbind
#' @importFrom dplyr bind_rows distinct arrange
#' @export
#'
add_all_hypotheticals <- function(studbook, hyp_defs, loc_key) {
  hypothetical_sires <- map_depth(hyp_defs$sires, 1, \(x) add_hypotheticals(studbook, "sire", x, loc_key)) %>%
    list_rbind(.)
  hypothetical_dams <- map_depth(hyp_defs$dams, 1, \(x) add_hypotheticals(studbook, "dam", x, loc_key)) %>%
    list_rbind(.)

  offspring <- list_flatten(hyp_defs, name_spec = "") %>%
    list_c() %>% unique()

  hypotheticals <- bind_rows(hypothetical_sires, hypothetical_dams) %>%
    distinct()

  studbook <- filter(studbook, !(ID %in% offspring)) %>%
    bind_rows(hypotheticals) %>%
    group_by(ID) %>%
    fill(Sire, Dam, .direction = "downup") %>%
    ungroup() %>%
    distinct() %>%
    arrange(ID, Date)

  return(studbook)
}
