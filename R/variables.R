# variables.R
# Declares all non-standard evaluation variables used across the package

utils::globalVariables(c(
    # Variables used in pedigree functions
    "Sex", "sex_ped", "sex_kin", "ID", "Sire", "Dam", "name_spec", "exclude",
    "Date_birth", "Date_last", "Institution_birth", "State_Province_birth",
    "Country_birth", "iconLoc_birth", "colorLoc_birth", "Institution_last",
    "State_Province_last", "Country_last", "iconLoc_last", "colorLoc_last",
    "pedigree", "consecutive_id", "nonfounders", "parents", "series", "name", "depth",

    # Variables used in network visualization functions
    "colors", "cols.light", "nodes", "edges", "links",
    "hubs", "moms", "dads", "kids", "internal_ids",
    "from", "to", "length", "width", "curved", "smooth", "dashes",
    "shadow", "lty", "arrow.size", "arrows", "famid", "link",
    "id_count", "id_ped", "id_stud", "label", "level", "generation", "group",
    "type", "value", "color", "shape", "frame.color", "size", "label.cex",
    "title", "exclude", "label_spec", "fam_year", "color_connector", "label_connector",

    # Variables used in location processing
    "Location", "Mnemonic", "code1", "code2", "code", "country", "code_country",
    "iconLoc", "colorLoc", "locs", "loc_vals", "df", "palette",

    # Variables used in BTP and studbook functions
    "BTP", "Facility_Note", "Notes", "facility", "transfer", "year", "Event",
    "Type_birth", "Status", "loc_order",

    # Variables used in census, life tables, and cohort functions
    "Age", "lx", "Births", "Nx", "N0", "N1", "Px", "Lx", "Lx1", "Qx", "Mx",
    "Fx", "Tnum", "lambda", "repro_first", "repro_last", "age_max", "R0", "T",
    "numT", "delta_F", "GD", "MK",

    # Variables used in reactable column definitions and tooltips
    "icon", "img_src", "index", "hover_lambda",

    # Variables used in cohort functions
    "Cohort", "Cohort_birth", "Cohort_label", "Cohort_min", "Cohort_max",
    "Date", "Start", "End", "Age", "Sex", "Years",

    # Other temporary column names that appear in the pipelines
    "loc_order", "facility", "note", "mate", "transfer", "Sire", "Dam", "."
  ))
