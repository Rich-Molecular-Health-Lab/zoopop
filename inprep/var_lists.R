col.pal <- c("#B24422FF", "#C44D76FF", "#4457A5FF", "#13315FFF", "#B1A1CCFF", "#59386CFF", "#447861FF", "#7CAF5CFF")

lifevars <- list(
  N0          = "Total number of individuals at age 0 (i.e. total births in the cohort).",
  N1          = "Total number of individuals at age 1 (i.e. total surviving first year).",
  Qx_1        = "Infant mortality rate, computed as the ratio of observed deaths in the first age class to the initial cohort size.",
  R0          = "Net Reproductive Rate, the sum of reproductive outputs (`Fx`) across all ages for the cohort.",
  MLE         = "Maximum Likelihood Estimate of the age at 50% survivorship; the interpolated age where survival drops to 50%.",
  T           = "Mean generation time (average age of reproduction), computed as `T = Tnum / R0` where `Tnum` is the age‐weighted reproductive output.",
  repro_first = "The minimum age at which reproduction is observed, indicating the onset of reproductive activity.",
  repro_last  = "The maximum age at which reproduction is observed in the cohort.",
  age_max     = "The highest age at which mortality is recorded, representing the maximum observed lifespan in the cohort.",
  lambda      = "Finite rate of increase calculated as \u03BB `= R0^(1/T)`, representing the population growth rate.",
  r           = "Intrinsic rate of increase: the continuous growth rate of the population, computed as `r = log(\u03BB)`, where \u03BB is the finite rate of increase."
)

agevars <- list(
  Age       = "The age or age class (in years) for which all other demographic values are computed.",
  Births    = "The number of births recorded for that specific age class.",
  Deaths    = "The number of deaths occurring within that age interval (or age class).",
  Nx        = "The number of individuals alive at the beginning of the age class x.",
  Qx_risk   = "The total number of individuals alive at the start of age x (`Nx`)",
  Qx        = "The age-specific mortality rate, calculated as the number of deaths in the age interval divided by the risk population (often `Nx`).",
  Lx        = "The proportion of the original cohort (`N0`) surviving to age x. It is a cumulative survival function.",
  Lx1       = "The proportion of individuals surviving past the initial age interval (relative to `N1`), used to assess survival beyond infancy.",
  Px        = "The probability of surviving from age x to the next age class, computed as the ratio of survivors in the next age class to `Nx.`",
  ex        = "Life expectancy at age x: the average number of additional years an individual is expected to live if they survive to age x. Typically calculated as `ex = Tx/Lx`.",
  Tx        = "Total future lifetime: the sum of survivors (`Lx`) from age x onward, representing the total person-years lived by the cohort beyond age x.",
  Mx_risk   = "The total number of individuals capable of reproducing at age x (`Nx`)",
  Mx        = "The age-specific fertility rate (or production rate), representing the per-individual contribution of offspring at each age (often adjusted by 1/2 for biparental reproduction).",
  Fx        = "The age-specific reproductive output, computed as `Mx` multiplied by `Lx.` It represents the expected number of offspring produced by an individual at age x.",
  numT      = "The product of `Age` and `Fx`, which weights the reproductive output by age and is used to compute the mean generation time (`T`)."
)
