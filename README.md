
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zoopop <img src="man/figures/logo_richlab.png" align="right" height="138"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Rich-Molecular-Health-Lab/zoopop/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Rich-Molecular-Health-Lab/zoopop/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`zoopop` is a custom package for the **Rich Lab for Molecular Health at
the University of Nebraska Omaha**, designed and maintained by
Dr. Alicia Rich.

This package includes many of the **functions, datasets, color palettes,
interactive tables, and plotting tools** used most often in our lab
workflows involving published AZA studbook data. Its sister-package,
`zoolabs` contains many of the same tools designed more specifically for
use in teaching exercises from Dr. Rich’s Zoo Biology course at UNO. The
biggest difference between the two packages is that this one also
contains the necessary functions for importing, cleaning, and
standardizing studbook data, starting from the published pdf files that
we convert into messy csv tables using Adobe Acrobat’s pdf-excel
conversion tool.

## Features

- **Pedigree visualization** with `pedtools` and `visNetwork`
- **Population viability metrics** including lambda, life tables, and
  kinship
- **Interactive demographic tables** using `reactable`
- **Standardized color palettes** for sex, status, and location
- 📦 Supports `learnr` modules, R Markdown labs, and Shiny apps

## 🚀 Installation

### If you have not used this package yet

Install **`zoopop`** on your local `R Studio` by running the following
from the console:

``` r
install.packages("pak")
```

Then run this from the same console:

``` r
pak::pak("Rich-Molecular-Health-Lab/zoopop")
```

### After installing for first use

Load the package each time you want to use a lab tutorial in your
session by copying and pasting the following into your console:

``` r
library(zoopop)
```

## Usage

Once installed, you’ll be able to:

- Use all core functions like
  `read_studbook`()`,`read_btp()`, and`find_parents()\`
- Load demo data like `studbook.csv` via `system.file()`
- Launch `learnr` tutorials.

## Resources

- Lab Site:
  [rich-molecular-health-lab.github.io](https://github.com/Rich-Molecular-Health-Lab)
- Issue Tracker: [GitHub
  Issues](https://github.com/Rich-Molecular-Health-Lab/zoopop/issues)
- Maintainer: [Alicia Rich](mailto:aliciarich@unomaha.edu)

## License

This project is licensed under the **GPL-3** license.

------------------------------------------------------------------------

*This package is intended for members of the Rich Lab but open to anyone
who finds it useful.*
