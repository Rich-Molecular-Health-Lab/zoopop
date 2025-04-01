prepare_btp <- function(file, dir) {
  df <- read_csv(file) %>%
    rename(ID = "SB ID") %>%
    mutate(facility      = str_extract_all(Institution, "^.+(?=\\s[:upper:][:lower:])"),
           note           = str_remove_all(Facility_Note, "Facility Note: "),
           age           = as.numeric(Age),
           .keep = "unused") %>%
    mutate(year_birth    = BTP - age,
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
    mutate(note = as.character(note)) %>%
    select(ID ,
           include,
           exclude,
           sex           = Sex,
           year          = BTP,
           age,
           year_birth,
           location      = Location_current,
           transfer      = Location_transfer,
           breeding      = Breeding,
           mate,
           note
    ) %>%
    distinct()
  write_csv(df, dir)
  return(df)
}


name_locations <- function(df) {
  df %>%
    select(Name  = Institution.Name,
           Label = Mnemonic,
           Country) %>%
    distinct() %>%
    mutate(across(where(is.character), ~ na_if(., "")),
           across(where(is.character), ~str_trim(str_replace_all(., "\\n|\\t", " ")))) %>%
    mutate(Label = if_else(is.na(Label), str_remove_all(Name, "(?<=.)(\\s\\w+){1,}"), Label)) %>%
    mutate(Label = str_to_upper(str_remove_all(Label, "[^\\w+]"))) %>%
    mutate(Label = str_to_upper(Label))  %>%
    mutate(
      Label   = if_else(Name  == "SWITZRLND: Invalid use of GeoRef", "SWITZRLNDINVALIDUSEOFGEOREF", Label),
      Country = if_else(Label == "PUBLIC", "Undetermined", Country)
    ) %>%
    distinct() %>%
    arrange(Label) %>%
    mutate(Abbrev    = as.character(str_sub(Label,  1L,  3L)),
           Abbrev2   = as.character(str_sub(Label, -2L, -1L))) %>%
    mutate(LocAbbrev = if_else(!is.na(lead(Abbrev)) & Abbrev == lead(Abbrev),
                               str_replace(Abbrev, "(?<=^\\w)\\w{2}$",
                                           Abbrev2), Abbrev),
           NameLoc = Name, .keep = "unused") %>%
    arrange(Country, NameLoc) %>% filter(!is.na(NameLoc))
}

label_locations <- function(df, palette) {
  sample(palette,
         size    = length(unique(pull(df, LocAbbrev))),
         replace = FALSE) %>%
    as.list() %>%
    set_names(., map(as.list(unique(pull(df, LocAbbrev))), \(x) paste0(x))) %>%
    enframe(name = "LocAbbrev", value = "colorLoc")  %>%
    mutate(colorLoc = as.character(colorLoc)) %>%
    right_join(df, by = join_by(LocAbbrev)) %>%
    mutate(iconLoc = case_match(
      Country,
      "United States"       ~ "\U1F1FA\U1F1F8",
      "United Kingdom"      ~ "\U1F1EC\U1F1E7",
      "Vietnam"             ~ "\U1F1FB\U1F1F3",
      "Canada"              ~ "\U1F1E8\U1F1E6",
      "Denmark"             ~ "\U1F1E9\U1F1F0",
      "Germany"             ~ "\U1F1E9\U1F1EA",
      "Poland"              ~ "\U1F1F5\U1F1F1",
      "Russian Federation"  ~ "\U1F1F7\U1F1FA",
      "Sweden"              ~ "\U1F1F8\U1F1EA",
      "Switzerland"         ~ "\U1F1E8\U1F1ED",
      "Taiwan"              ~ "\U1F1F9\U1F1FC",
      "Thailand"            ~ "\U1F1F9\U1F1ED",
      "Laos"                ~ "\U1F1F1\U1F1E6",
      "Undetermined"        ~ "\U1F6A9",
      NA                    ~ "\U1F6A9"))
}

location_key <- function(file, palette) {
  locations <- read.csv(file, header = TRUE) %>%
    name_locations() %>%
    label_locations(palette)
}

clean_studbook <- function(df, alive, locations) {
  df %>%
    select(ID        = Studbook.ID,
           Sex       = Sex.Type,
           Sire,
           Dam,
           Date,
           TypeEvent = Event.Type,
           Location) %>%
    fill(ID) %>%
    mutate(across(c(Sire, Dam), as.character),
           across(where(is.character), ~ na_if(., "")),
           Date      =  dmy(str_extract(Date, "\\d{1,2}.\\w{3}.\\d{2,4}")),
           Sex       = str_sub(Sex, 1, 1),
           TypeEvent = fct_recode(str_to_lower(TypeEvent),
                                  Birth    = "birth/hatch",
                                  Capture  = "wild capture",
                                  Transfer = "transfer",
                                  LTF      = "go ltf",
                                  Death    = "death")) %>%
    mutate(Date            = if_else(Date > today(), Date - years(100), Date),
           Confirmed_Death = if_else(TypeEvent == "Death", "y", NA)) %>%
    mutate(across(c(Sire, Dam), ~as.integer(str_replace_all(., "WILD", "0")))) %>%
    mutate(across(c(Sire, Dam), ~na_if(., 0))) %>%
    group_by(ID) %>%
    fill(Confirmed_Death, .direction = "up") %>%
    mutate(Confirmed_Death = replace_na(Confirmed_Death, "n")) %>%
    fill(Sire, Dam, Sex) %>%
    mutate(Status   = if_else(ID %in% alive, "A", "D"),
           Location = na_if(Location, ""),
           Location = str_to_upper(str_trim(str_remove_all(Location, "[^\\w+]")))) %>%
    left_join(select(locations,
                     LocAbbrev,
                     Location = Label,
                     NameLoc,
                     Country),
              by = join_by(Location)) %>%
    mutate(Location = LocAbbrev, .keep = "unused") %>%
    mutate(Date = case_when(
      is.na(Date) & TypeEvent == "Capture" ~ lag(Date),
      is.na(Date) & TypeEvent == "Birth" & lead(TypeEvent) == "Capture" ~ lead(Date),
      .default = Date
    ),
    Location = case_when(
      (is.na(Location) | Location == "UND") & TypeEvent == "Capture" ~ lag(Location),
      (is.na(Location) | Location == "UND") & TypeEvent == "Birth" & lead(TypeEvent) == "Capture" ~ lead(Location),
      .default = Location
    )) %>%
    mutate(TypeEvent = fct_collapse(TypeEvent,
                                    Birth = c("Birth", "Capture"),
                                    End   = c("Death", "LTF"))) %>%
    distinct() %>%
    group_by(ID) %>%
    mutate(
      Date          = if_else(TypeEvent == "Birth" & TypeEvent == lag(TypeEvent) & !is.na(lag(Date)), lag(Date), Date),
      remove        = if_else(max(row_number()) > 1 & TypeEvent == "Birth" & TypeEvent == lead(TypeEvent) & ID == lead(ID), "y", "n"),
      Confirmed_Age = if_else(TypeEvent == "Birth" & !is.na(Date), "y", NA),
      Sex           = if_else(ID == 2504 | ID == 2717, "F", Sex)) %>%
    fill(Confirmed_Age, remove, .direction = "downup") %>%
    filter(remove == "n") %>%
    mutate(OrderLoc      = consecutive_id(Location),
           Confirmed_Age = replace_na(Confirmed_Age, "n")) %>%
    ungroup() %>%
    distinct() %>%
    mutate(TypeEvent = as.character(TypeEvent)) %>%
    select(ID,
           Sex,
           Status,
           Sire,
           Dam,
           OrderLoc,
           Location,
           Date,
           TypeEvent,
           NameLoc,
           Country,
           Confirmed_Death,
           Confirmed_Age
    )
}

calculate_age <- function(birth, date) {
  floor(as.numeric(as.period(interval(floor_date(birth, "month"), date), unit = "years"), "years"))

}

check_loc_dates <- function(df) {
  df %>%
    mutate(check = if_else(
      between(Date, StartLoc, EndLoc) |
        is.na(StartLoc) & Date < EndLoc |
        is.na(EndLoc) & Date > StartLoc |
        (is.na(StartLoc) & is.na(EndLoc)) | is.na(Date),
      "keep",
      "discard"
    )) %>%
    filter(check == "keep") %>%
    select(-check)

  return(df)
}

est_date_between   <- function(date1, date2) {
  date1 + days(floor(as.numeric(as.period((interval(date1, date2)), unit = "days"), "days")/2))
}

assemble_timeline <- function(studbook) {

  end.records <- studbook %>%
    select(ID,
           Status,
           OrderLoc,
           Location,
           TypeEvent,
           StartLoc,
           Date,
           EndLoc
    ) %>%
    arrange(ID,
            OrderLoc,
            TypeEvent,
            Date) %>%
    slice_tail(n = 1, by = ID) %>%
    mutate(Date      = case_when(Status == "A" ~ today(),
                                 is.na(Date) & ID %in% deaths.21_24 ~ ymd("2023-1-1"),
                                 is.na(Date) & ID %in% deaths.24    ~ ymd("2025-1-1"),
                                 .default = Date),
           TypeEvent = "End") %>%
    mutate(EndLoc    = Date) %>%
    distinct()

  sires <- studbook %>%
    filter(TypeEvent == "Birth" & !is.na(Sire)) %>%
    select(ID      = Sire,
           OffspID = ID,
           Location,
           Date)

  parents <- studbook %>%
    filter(TypeEvent == "Birth" & !is.na(Dam)) %>%
    select(ID      = Dam,
           OffspID = ID,
           Location,
           Date) %>%
    bind_rows(sires)

  births <- studbook %>%
    select(ID,
           OrderLoc,
           Location,
           StartLoc,
           EndLoc) %>%
    right_join(parents, by = join_by(ID, Location)) %>%
    check_loc_dates() %>%
    mutate(TypeEvent = "Breed") %>%
    select(-OffspID) %>%
    distinct() %>%
    arrange(ID,
            OrderLoc,
            Date)

  studbook %>%
    select(
      ID,
      OrderLoc,
      Location,
      TypeEvent,
      StartLoc,
      Date,
      EndLoc) %>%
    filter(TypeEvent != "End") %>%
    bind_rows(births) %>%
    bind_rows(select(end.records, -Status)) %>%
    mutate(TypeEvent = fct(TypeEvent,
                           levels = c(
                             "Birth",
                             "Transfer",
                             "Breed",
                             "End"
                           ))) %>%
    arrange(ID,
            OrderLoc,
            TypeEvent,
            Date) %>%
    distinct()
}

fill_dates_timeline <- function(df) {
  df %>%
    arrange(ID,
            OrderLoc,
            TypeEvent,
            Date) %>%
    select(ID,
           OrderLoc,
           Location,
           StartLoc,
           TypeEvent,
           Date,
           EndLoc)  %>%
    group_by(ID) %>%
    mutate(Date = case_when(
      is.na(Date) & TypeEvent != "End"  & TypeEvent != "Breed" ~ StartLoc,
      is.na(Date) & TypeEvent == "End"   ~ EndLoc,
      is.na(Date) & TypeEvent == "Birth" ~ lead(Date) - years(1),
      is.na(Date) & TypeEvent == "Transfer" & lead(TypeEvent) != "End" ~
        est_date_between(lag(Date), lead(Date)),
      .default = Date
    )) %>%
    mutate(Date = if_else(is.na(Date) & TypeEvent == "Transfer" & lead(TypeEvent) != "End",
                          est_date_between(lag(Date), lead(Date)), Date)) %>%
    mutate(Date = if_else(
      is.na(Date) & lag(TypeEvent) == "Birth" & is.na(lag(Date)),
      lead(Date) - years(1), Date
    )) %>%
    mutate(Date = case_when(
      is.na(Date) & TypeEvent == "Birth"                  ~ lead(Date) - years(1),
      is.na(Date) & TypeEvent == "End" & !(ID %in% alive) ~ lag(Date) + years(1),
      .default = Date)) %>%
    mutate(
      StartLoc = case_when(
        TypeEvent == "Breed" ~ lag(StartLoc),
        TypeEvent != "End" &      TypeEvent != "Breed" ~ Date,
        TypeEvent == "End" & lag(TypeEvent) != "Breed" ~ lag(Date),
        .default = StartLoc
      ),
      EndLoc = case_when(
        TypeEvent       == "Breed" & Location == lead(Location)  ~ lead(EndLoc),
        lead(TypeEvent) == "Transfer" | lead(TypeEvent) == "End" ~ lead(Date),
        TypeEvent       == "End"                                 ~ Date,
        .default = EndLoc
      )
    ) %>% group_by(ID, Location) %>%
    fill(StartLoc, EndLoc, .direction = "updown") %>%
    ungroup() %>% distinct()

}

revise_studbook <- function(studbook, timeline) {
  deceased <- filter(studbook, Status == "D") %>% pull(ID) %>% unique()

  birth.dates <- filter(timeline, TypeEvent == "Birth") %>%
    select(ID,
           LocBirth  = Location,
           DateBirth = Date) %>%
    mutate(BirthYear = year(DateBirth))

  deaths   <- filter(timeline, TypeEvent == "End" & ID %in% deceased) %>%
    select(ID,
           LocDeath  = Location,
           DateDeath = Date)

  studbook.revised <- studbook %>%
    select(
      ID,
      Status,
      Sex,
      Sire,
      Dam
    ) %>%
    distinct() %>%
    left_join(deaths, by = join_by(ID)) %>%
    left_join(birth.dates, by = join_by(ID)) %>%
    mutate(AgeDeath = calculate_age(DateBirth, DateDeath))

  return(studbook.revised)
}

expand_timeline <- function(timeline, period, studbook) {
  timeline <- timeline %>%
    select(ID,
           Location,
           StartLoc,
           EndLoc) %>%
    distinct()
  if (period == "months") {
    timeline.long <- timeline %>%
      mutate(StartLoc = floor_date(StartLoc, "months"),
             EndLoc   = ceiling_date(EndLoc, "months")) %>%
      mutate(Months = pmap(list(StartLoc, EndLoc), \(x, y) seq(x, y, by = "months"))) %>%
      unnest(Months) %>%
      select(ID,
             Date = Months,
             Location) %>%
      left_join(select(
        studbook,
        ID,
        Sex,
        DateBirth
      ), by = join_by(ID)) %>%
      mutate(BirthYear = year(DateBirth),
             Age    = calculate_age(DateBirth, Date)) %>%
      select(ID,
             Sex,
             BirthYear,
             Date,
             Age,
             Location)

  } else if (period == "years") {
    timeline.new <- timeline %>%
      mutate(StartLoc = floor_date(StartLoc, "years"),
             EndLoc   = ceiling_date(EndLoc, "years")) %>%
      mutate(Years = pmap(list(StartLoc, EndLoc), \(x, y) seq(x, y, by = "years"))) %>%
      unnest(Years) %>%
      select(ID,
             Date = Years,
             Location) %>%
      left_join(select(
        studbook,
        ID,
        Sex,
        DateBirth
      ), by = join_by(ID)) %>%
      mutate(BirthYear = year(DateBirth),
             Age    = calculate_age(floor_date(DateBirth, "years"), Date)) %>%
      select(ID,
             Sex,
             BirthYear,
             Date,
             Age,
             Location)
  }

  return(timeline.long)

}
