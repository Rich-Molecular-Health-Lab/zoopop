est_date_btn   <- function(date1, date2) {
  date1 + days(floor(as.numeric(as.period((lubridate::interval(date1, date2)), unit = "days"), "days")/2))
}

calculate_age <- function(birth, date) {
  floor(as.numeric(as.period(lubridate::interval(floor_date(birth, "month"), date), unit = "years"), "years"))
}

read_btp <- function(file_in, file_out, loc_key) {
  df <- read_csv(file_in) %>%
    rename(ID = "SB ID") %>%
    mutate(facility      = str_extract_all(Institution, "^.+(?=\\s[:upper:][:lower:])"),
           note          = str_remove_all(Facility_Note, "Facility Note: "),
           age           = as.numeric(Age),
           .keep = "unused") %>%
    mutate(Year_birth    = BTP - age,
           exclude       = case_when(
             str_detect(Notes, "Excluded") & str_detect(Notes, "Age")      ~ "age",
             str_detect(Notes, "Excluded") & str_detect(Notes, "Behavior") ~ "behavior",
             str_detect(Notes, "Died")     ~ "deceased",
             .default = "n"),
           disposition = case_match(
             Disposition,
             "HOLD"         ~ "current",
             "RECEIVE FROM" ~ "current",
             "SEND TO"      ~ "transfer",
             .default = Disposition
           )
    ) %>%
    mutate(include = if_else(exclude == "n", "y", "n"),
           mate    = if_else(is.na(With) & str_detect(Notes, "Breed"),
                             str_extract(Notes, "\\d+"), as.character(With))
    ) %>%
    mutate(mate = as.integer(mate)) %>%
    group_by(ID, BTP) %>%
    fill(note, .direction = "downup") %>%
    ungroup() %>%
    arrange(ID, BTP, facility) %>%
    pivot_wider(names_from  = "disposition", values_from = c("Location", "note")) %>%
    select(-facility) %>%
    group_by(ID, BTP) %>%
    fill(starts_with("Location_"), starts_with("note_"), .direction = "downup") %>%
    ungroup() %>%
    distinct() %>%
    mutate(note = case_when(
      note_current == note_transfer ~ note_current,
      is.na(note_current)  ~ note_transfer,
      is.na(note_transfer)       ~ note_current,
      !is.na(note_transfer) & !is.na(note_current) & note_current != note_transfer ~ str_glue("{note_current}", "{note_transfer}", sep = "; ")), .keep = "unused") %>%
    mutate(note      = as.character(note),
           Location = str_remove_all(Location_current, "[^\\w+]"),
           transfer = str_remove_all(Location_transfer, "[^\\w+]"),
           Event    = "btp",
           Date     = make_date(year = BTP, month = 2, day = 1)) %>%
    select(ID ,
           include,
           exclude,
           Sex,
           year          = BTP,
           Date,
           Event,
           age,
           Year_birth,
           Location,
           transfer,
           breeding      = Breeding,
           mate,
           note
    ) %>%
    distinct() %>%
    left_join(loc_key, by = join_by(Location)) %>%
    left_join(select(loc_key,
                     transfer      = Location,
                     transfer_code = code),
              by = join_by(transfer))
  write_csv(df, file_out)
  return(df)
}

read_locations <- function(file_in, file_out, khroma_pal) {
  locs  <- read_csv(file_in) %>%
    mutate(across(where(is.character), ~str_squish(.))) %>%
    mutate(Location = str_remove_all(Mnemonic, "[^\\w+]"), .keep = "unused") %>%
    distinct() %>%
    arrange(Location) %>%
    mutate(code1   = as.character(str_sub(Location,  1L,  3L)),
           code2   = as.character(str_sub(Location, -2L, -1L))) %>%
    mutate(code = if_else(!is.na(lead(code1)) & code1 == lead(code1),
                          str_replace(code1, "(?<=^\\w)\\w{2}$",
                                      code2), code1), .keep = "unused")  %>%
    mutate(code_country = countrycode(Country     , origin = "country.name", destination = "iso3c"),
           iconLoc      = countrycode(code_country, origin = "iso3c"       , destination = "unicode.symbol")) %>%
    mutate(iconLoc      = replace_na(iconLoc, "\u3f"),
           code_country = replace_na(code_country, "UND"),
           code         = str_to_upper(code))

  loc_vals <- pull(locs, code) %>% unique() %>% as.list()

  df <-   as.list(sample(palettes_d$khroma[[paste0(khroma_pal)]],
                         size = length(loc_vals),
                         replace = FALSE)) %>%
    set_names(., map(loc_vals, \(x) paste0(x))) %>%
    enframe(name = "code", value = "colorLoc")  %>%
    mutate(colorLoc = as.character(colorLoc),
           code     = as.character(code)) %>%
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

read_studbook <- function(file_in, loc_key, btp) {
  events <- c(
    birth    = "birth/hatch",
    capture  = "wild capture",
    transfer = "transfer",
    ltf      = "go ltf",
    death    = "death"
  )
  std <- read_csv(file_in) %>%
    mutate(across(where(is.character), ~str_squish(.))) %>%
    filter(Location != "Location") %>%
    fill(ID, Location_Current, Sex, Sire, Dam, Birth_Type) %>%
    mutate(Sex        = str_sub(Sex, 1L, 1L),
           Date       = dmy(str_extract(Date, "\\d{1,2}[-/]\\w{3}[-/]\\d{2,4}")),
           Location   = str_remove_all(Location, "[^\\w+]"),
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
  living     <- filter(btp, year == max(year) & exclude != "deceased") %>%
    pull(ID) %>% unique()
  loc_orders <- std %>%
    select(ID, code, loc_order) %>%
    distinct()

  btp_births <- btp %>%
    filter(ID %in% new_births) %>%
    select(ID ,
           Sex,
           Date,
           Event,
           code) %>%
    arrange(ID, Date) %>%
    distinct() %>%
    mutate(Date  = floor_date(Date, "years"),
           Event = "birth")

  btp_deaths <- btp %>%
    filter(exclude == "deceased") %>%
    select(ID ,
           Sex,
           Date,
           Event,
           code) %>%
    arrange(ID, Date) %>%
    distinct() %>%
    mutate(Date = floor_date(Date, "years"))

  deaths      <- std %>%
    filter(Event %in% c("ltf", "death")) %>%
    select(ID ,
           Sex,
           Date,
           Event,
           code) %>%
    bind_rows(btp_deaths) %>%
    distinct() %>%
    pivot_wider(names_from  = "Event",
                values_from = "Date") %>%
    mutate(Date  = case_when(is.na(death) & is.na(btp) ~ltf,
                             is.na(death) & is.na(ltf) ~btp,
                             .default = death),
           Event = "death", .keep = "unused") %>%
    filter(!is.na(Date)) %>%
    arrange(ID, Date)

  birth_dates <- std %>%
    filter(Event %in% c("birth", "capture")) %>%
    select(ID ,
           Sire,
           Dam,
           Sex,
           Date,
           Event,
           code) %>%
    distinct() %>%
    pivot_wider(names_from  = "Event",
                values_from = "Date") %>%
    mutate(Date  = if_else(is.na(birth), capture, birth),
           Event = "birth", .keep = "unused") %>%
    bind_rows(btp_births) %>%
    arrange(ID, Date)

  births <- birth_dates %>%
    select(Sire,
           Dam,
           Date,
           Event,
           code,
           offspring = ID) %>%
    pivot_longer(cols      = c("Sire", "Dam"),
                 names_to  = "Sex",
                 values_to = "ID") %>%
    mutate(Sex   = case_match(Sex, "Sire" ~"M", "Dam" ~"F"),
           Event = "breed") %>%
    filter(ID != 0 & !is.na(ID)) %>%
    select(ID,
           Sex,
           Date,
           Event,
           code) %>%
    arrange(ID, Date) %>%
    distinct()

  transfers <- std %>%
    filter(Event == "transfer") %>%
    select(ID ,
           Sex,
           Date,
           Event,
           code) %>%
    arrange(ID, Date) %>%
    distinct()

  btps <- btp %>%
    select(ID ,
           Sex,
           Date,
           Event,
           code) %>%
    arrange(ID, Date) %>%
    distinct()

  event_fact <- c(
    "birth"    ,
    "capture"  ,
    "transfer" ,
    "breed"    ,
    "btp"      ,
    "death"
  )
  studbook <- bind_rows(
    birth_dates,
    births,
    transfers,
    btps,
    deaths
  ) %>%
    mutate(Event = factor(Event, levels = event_fact)) %>%
    left_join(loc_orders, by = join_by(ID, code)) %>%
    arrange(ID, loc_order, Event, Date) %>%
    group_by(ID) %>%
    fill(Sire, Dam, Sex, .direction = "downup") %>%
    mutate(loc_order = consecutive_id(code),
           Status    = if_else(ID %in% living, "A", "D")) %>%
    mutate(Date_last  = case_when(Status == "D" & Event == "death" ~Date,
                                  Status == "A" ~today())) %>%
    fill(Date_last, .direction = "up") %>%
    mutate(Date_last = if_else(is.na(Date_last), last(Date) + years(1), Date_last),
           Loc_last  = last(code)) %>%
    filter(Event != "death") %>%
    mutate(Date_birth = if_else(Event == "birth", Date, NA),
           Loc_birth  = if_else(Event == "birth", code, NA)) %>%
    fill(Date_birth, Loc_birth) %>%
    filter(Event != "birth") %>%
    distinct() %>%
    mutate(event_order = row_number()) %>%
    select(
      ID,
      Date_birth,
      Date,
      event_order,
      Event,
      Date_last,
      Status,
      Loc_event = code,
      Loc_birth,
      Loc_last,
      Sex,
      Sire,
      Dam
    ) %>%
    mutate(Date = case_when(
      is.na(Date) & event_order == 1 & !is.na(Date_birth) ~est_date_btn(lead(Date), Date_birth),
      is.na(Date) & event_order == 1 & is.na(Date_birth) ~lead(Date) - years(1),
      .default = Date
    )) %>%
    mutate(Date_birth = if_else(is.na(Date_birth), first(Date) - years(2), Date_birth)) %>%
    fill(Date_birth) %>%
    mutate(age_event = calculate_age(Date_birth, Date),
           age_last  = calculate_age(Date_birth, Date_last)) %>%
    left_join(select(std, ID, Type_birth), by = join_by(ID)) %>%
    left_join(select(btp, ID, Date, Event, exclude), by = join_by(ID, Date, Event)) %>%
    fill(exclude) %>%
    mutate(exclude = case_when(Status == "D" ~"deceased",
                               Status == "A" & Event == "breed" ~"n",
                               .default = exclude),
           Type_birth = replace_na(Type_birth, "Undetermined"),
           Year_birth  = year(floor_date(Date_birth, "years")),
           across(c(Loc_event, Loc_birth, Loc_last),  ~replace_na(., "UND"))) %>%
    fill(exclude, Type_birth, .direction = "downup") %>%
    mutate(across(c(Sire, Dam), ~if_else(is.na(.) & Type_birth == "Undetermined", 0, .))) %>%
    ungroup() %>%
    left_join(loc_key, by = join_by(Loc_birth == code)) %>%
    rename(Institution_birth    = Institution,
           State_Province_birth = State_Province,
           Country_birth        = Country,
           iconLoc_birth        = iconLoc,
           colorLoc_birth       = colorLoc) %>%
    select(-c(code_country, Location)) %>%
    left_join(loc_key, by = join_by(Loc_last == code)) %>%
    rename(Institution_last     = Institution,
           State_Province_last  = State_Province,
           Country_last         = Country,
           iconLoc_last         = iconLoc,
           colorLoc_last        = colorLoc) %>%
    select(-c(code_country, Location)) %>%
    distinct()

  return(studbook)
}

studbook_short <- function(studbook) {
  studbook %>%
    arrange(Date) %>%
    select(
      ID,
      Sex,
      Status,
      Sire,
      Dam,
      ends_with("_birth"),
      ends_with("_last"),
      exclude
    ) %>%
    arrange(ID) %>%
    distinct()
}

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
           Loc_end   = if_else(row_number() == max(row_number()),
                               Date_last, lead(Date_event))) %>%
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
    mutate(Date      = Dates,
           Location  = code,
           Age       = calculate_age(Date_birth, Date)) %>%
    group_by(Date, Location) %>%
    summarize(Individuals = list(tibble(ID, Sex, Age)))  %>%
    group_by(Date) %>%
    summarize(Locations = split(Individuals, Location)) %>%
    split(.$Date) %>%
    map(~ .x$Locations)
  return(locations)
}

match_births <- function(x, sex_parent) {
  if (is.null(x)) {
    return(tibble(ID = NA, Sex = "None", Age = 0))
  } else {
   filter(x, Sex == sex_parent & !(ID %in% c(names))) %>%
      arrange(desc(Age)) %>%
      distinct()
  }
}

parent_details <- function(x, studbook) {
  studbook_short <- studbook_short(studbook)
  if (is.null(x) | nrow(x) < 1) {
    return(tibble())
  } else {
    left_join(x, studbook_short, by = join_by(ID, Sex)) %>%
      distinct()
  }
}

find_parent <- function(studbook, loc_key, parent) {
  if (parent %in% c("Sire", "sire", "dad", "father")) {
    sex_parent <- "M"
    subjects       <- studbook_short(studbook) %>%
      filter(Type_birth != "Wild" & is.na(Sire)) %>%
      mutate(Date = floor_date(Date_birth, unit = "years")) %>%
      relocate(Sire, Dam, .after = Loc_birth) %>%
      distinct()
  } else {
    sex_parent <- "F"
    subjects       <- studbook_short(studbook) %>%
      filter(Type_birth != "Wild" & is.na(Dam)) %>%
      mutate(Date = floor_date(Date_birth, unit = "years")) %>%
      relocate(Sire, Dam, .after = Loc_birth) %>%
      distinct()
  }
  loc_census     <- location_census(studbook, loc_key)
  names          <- pull(subjects, ID) %>% unique()
  search         <- as.list(deframe(select(subjects, Date, Loc_birth)))
  imap(search, \(x, idx) pluck(loc_census, idx, x, 1)) %>%
    set_names(., as.list(deframe(select(subjects, ID)))) %>%
    map_depth(., 1, \(x) match_births(x, sex_parent)) %>%
    map_depth(., 1, \(x) parent_details(x, studbook))
}


add_hypotheticals <- function(studbook, parent, ids, loc_key) {
  if (parent %in% c("Sire", "sire", "dad", "father")) {
    sex_parent <- "M"
    add  <- 10000
  } else {
    sex_parent <- "F"
    add  <- 20000
  }
  hypSire <- min(ids) + 10000
  hypDam  <- min(ids) + 20000

  hyp_cols <- tibble(
    event_order         = 1,
    Event               = "breed",
    Status              = "H",
    Loc_birth           = "UND",
    Sex                 = sex_parent,
    Sire                = 0,
    Dam                 = 0,
    age_event           = 2,
    Type_birth          = "Undetermined",
    exclude             = "hypothetical"
  )

  hypotheticals <-  studbook %>%
    filter(ID %in% ids) %>%
    arrange(Date_birth) %>%
    select(Date                = Date_birth,
           Loc_event           = Loc_birth,
           Institution_last    = Institution_birth,
           State_Province_last = State_Province_birth,
           Country_last        = Country_birth,
           iconLoc_last        = iconLoc_birth,
           colorLoc_last       = colorLoc_birth
           ) %>%
    bind_cols(hyp_cols) %>%
    mutate(ID                  = min(ids) + add,
           Date_birth          = first(Date) - years(2),
           Loc_last            = Loc_event,
           Date_last           = last(Date) + years(2)
           ) %>%
    mutate(age_last            = calculate_age(Date_birth, Date_last),
           Year_birth          = year(Date_birth)) %>%
    left_join(select(
      loc_key,
      Loc_birth            = code,
      Institution_birth    = Institution,
      State_Province_birth = State_Province,
      Country_birth        = Country,
      iconLoc_birth        = iconLoc,
      colorLoc_birth       = colorLoc
    ), by = join_by(Loc_birth)) %>%
    bind_rows(slice_head(., n = 1)) %>%
    distinct()

  return(hypotheticals)
}

add_all_hypotheticals <- function(studbook, hyp_defs, loc_key) {
  hypothetical_sires <- map_depth(hyp_defs$sires, 1, \(x) add_hypotheticals(studbook, "sire", x, loc_key)) %>%
    list_rbind(.)
  hypothetical_dams <- map_depth(hyp_defs$dams, 1, \(x) add_hypotheticals(studbook, "dam", x, loc_key)) %>%
    list_rbind(.)

  hypotheticals <- bind_rows(hypothetical_sires, hypothetical_dams) %>%
    distinct()

  studbook <- bind_rows(studbook, hypotheticals) %>%
    distinct() %>%
    arrange(ID, Date)

  return(studbook)
}


