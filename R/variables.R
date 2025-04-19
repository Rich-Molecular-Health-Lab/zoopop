if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # Variables used in studbook formatting
    "Birth", "Birth_Type", "Breeding", "Country", "Date", "Date_event", "Dates",
    "Death", "Disposition", "Individuals", "Institution", "Loc_end", "Loc_event",
    "Loc_star", "Location_current", "Location_transfer", "M", "MLE", "Mx_risk",
    "Qx_1", "Qx_ris", "Special_label", "State_Province", "Total_Cohort_label",
    "U", "With", "Yea", "Year_birth", "age", "age_event", "age_last", "birth",
    "capture", "children", "dad", "deat", "from_node", "id", "id_node", "include",
    "kid", "lx1", "mate", "mo", "note", "subnuc_id", "tip_connector", "to_node",

    # Variables used in pedigree functions
    "Sex", "sex_ped", "sex_kin", "ID", "Sire", "Dam", "name_spec", "exclude",
    "Date_birth", "Date_last", "Institution_birth", "State_Province_birth",
    "Country_birth", "iconLoc_birth", "colorLoc_birth", "Institution_last",
    "State_Province_last", "Country_last", "iconLoc_last", "colorLoc_last",
    "pedigree", "consecutive_id", "nonfounders", "parents", "series", "name", "depth",

    # Variables used in network and visualization functions
    "colors", "cols.light", "nodes", "edges", "links", "hubs", "moms", "dads", "kids",
    "internal_ids", "from", "to", "length", "width", "curved", "smooth", "dashes",
    "shadow", "lty", "arrow.size", "arrows", "famid", "link", "id_count", "id_ped",
    "id_stud", "label", "level", "generation", "group", "type", "value", "color",
    "shape", "frame.color", "size", "label.cex", "title", "label_spec", "fam_year",
    "color_connector", "label_connector",

    # Variables used in location processing
    "Location", "Mnemonic", "code1", "code2", "code", "country", "code_country",
    "iconLoc", "colorLoc", "locs", "loc_vals", "df", "palette",

    # Variables used in BTP and studbook functions
    "BTP", "Facility_Note", "Notes", "facility", "transfer", "year", "Event",
    "Type_birth", "Status", "loc_order",

    # Variables used in census, life table, and cohort functions
    "Age", "lx", "Births", "Nx", "N0", "N1", "Px", "Lx", "Lx1", "Qx", "Mx",
    "Fx", "Tnum", "lambda", "repro_first", "repro_last", "age_max", "R0", "T",
    "numT", "delta_F", "GD", "MK", "Deaths", "Loc_start", "Qx_risk", "Year", "death", "mom",

    # Variables used in reactable and plotly helpers
    "icon", "img_src", "index", "hover_lambda",

    # Variables used in cohort functions
    "Cohort", "Cohort_birth", "Cohort_label", "Cohort_min", "Cohort_max",
    "Start", "End", "Years",

    # Temporary names and dot placeholder
    "."
  ))
}
