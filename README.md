
<!-- README.md is generated from README.Rmd. Please edit that file -->

# journalR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/journalR)](https://CRAN.R-project.org/package=journalR)
[![R-CMD-check](https://github.com/epi-sam/journalR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epi-sam/journalR/actions/workflows/R-CMD-check.yaml)
![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/journlaR)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

``` r
install.packages("journalR")
```

## Overview

**journalR** eases numeric formatting for real-world scientific
publication writing.

The core utility formats triplet sets of central/lower/upper (CLU) raw
numeric values into a drop-in string that’s immediately pasteable into
journal text.

``` r
journalR::format_journal_clu(2e6, 1e6, 3e6, metric = 'count')
#> [1] "2.00 million (1.00–3.00)"
```

This core utility is vectorized, with greatest power coming from
data.frame wrappers.

``` r
journalR::format_journal_df(
   data.frame(
      id    = 1:3,
      mean  = c(.558, .234, .789),
      lower = c(.507, .201, .756),
      upper = c(.607, .267, .821)
      
   )
   , metric     = "prop"
   , style_name = "lancet"
)
#>   id           clu_fmt
#> 1  1 55·8% (50·7–60·7)
#> 2  2 23·4% (20·1–26·7)
#> 3  3 78·9% (75·6–82·1)
```

The user may also define ‘styles’ for rounding strategy, significant
digits, numbers of decimals, and more (see ‘Styles’ below).

## Why journalR?

In the Platonic ideal world, an article is written in Rmarkdown or pure
LaTeX.

In the real world, journals are written collaboratively in Word, with
multiple messy copies.

Sometimes, you just need to copy and paste some numbers.

``` r
library(journalR)
library(data.table) # journalR works on both data.frames and data.tables
```

Once you’ve done a statistical analysis, you’ll want to format numbers
for presentation.

``` r
DT <- data.table::as.data.table(mtcars)

# calculate mean/lower/upper 95% UI for mpg by cyl
DT_hp <- DT[
   , .(
      mean  = mean(hp)
      , lower = quantile(hp, 0.025)
      , upper = quantile(hp, 0.975)
   )
   , by = cyl
]
data.table::setorderv(DT_hp, 'cyl')

print(DT_hp)
#>      cyl      mean  lower   upper
#>    <num>     <num>  <num>   <num>
#> 1:     4  82.63636  54.50 112.000
#> 2:     6 122.28571 105.75 167.200
#> 3:     8 209.21429 150.00 311.925
```

Enter `format_journal_df()`:

- Format a central/lower/upper set of three columns into an
  e.g. $mean (lower-upper)$ string.
  - Ready for direct copy/paste into Word or similar.
- The user must provide a data type (`metric`), which tells the function
  how to format the numbers.
- central/lower/upper relationships are asserted (lower \< central \<
  upper) before formatting.

``` r
DT_hp |>
   journalR::format_journal_df(
      metric = "count"
   )
#>      cyl         clu_fmt
#>    <num>          <char>
#> 1:     4 82.6 (54.5–112)
#> 2:     6   122 (106–167)
#> 3:     8   209 (150–312)
```

## Options

The user’s data.frame may be formatted differently than the simple case
above, with different data types.

- Things get messy in the real world!

``` r
# build a mean/lower/upper summary table with some percentage from DT
DT_summary <- DT[
   , .(
      n            = .N
      , mean_mpg     = mean(mpg)
      , lower_mpg    = quantile(mpg, 0.025)
      , upper_mpg    = quantile(mpg, 0.975)
      , pct_vshape   = mean(vs) 
      , lower_vshape = quantile(vs, 0.025) 
      , upper_vshape = quantile(vs, 0.975) 
   )
   , by = cyl
]
```

As long as your table is tidy, it can be formatted.

``` r
DT_summary |>
   journalR::format_journal_df(
      metric               = c("prop")
      , new_var            = "pct_vshape_fmt" # specify output column name
      , central_var        = "pct_vshape" 
      , lower_var          = "lower_vshape"
      , upper_var          = "upper_vshape"
      , remove_clu_columns = FALSE            # retain the original columns if desired
   )
#>      cyl     n mean_mpg lower_mpg upper_mpg pct_vshape lower_vshape
#>    <num> <int>    <num>     <num>     <num>      <num>        <num>
#> 1:     6     7 19.74286    17.845   21.3400  0.5714286         0.00
#> 2:     4    11 26.66364    21.425   33.5250  0.9090909         0.25
#> 3:     8    14 15.10000    10.400   19.0375  0.0000000         0.00
#>    upper_vshape     pct_vshape_fmt
#>           <num>             <char>
#> 1:            1  57.1% (0.0–100.0)
#> 2:            1 90.9% (25.0–100.0)
#> 3:            0     0.0% (0.0–0.0)
```

## Assumptions

### General:

1.  Central/lower/upper triplets are formatted at the same ‘scale’.
    1.  Based on the central value.
    2.  User may override (see ‘Rates’ below).
2.  All triplets present with the same number of significant digits.

### Proportions:

1.  Proportions (prop) and Percentage Points (pp) are multiplied by 100
2.  All-negative triplets are presented with negative mean only, with
    upper and lower bounds reversed.

``` r
DT_prop <- data.table::data.table(
   data_space    = c("all_positive", "mixed_negative", "all_negative", "lower_negative", "central_lower_neg")
   , mean        = c(.558,           -0.1,             -0.1,            0.05,            -0.05)
   , lower       = c(.507,           -0.25,            -0.2,           -0.02,            -0.1)
   , upper       = c(.607,            1.3,             -0.05,           0.12,             0.1)
)
DT_prop|>
   journalR::format_journal_df(
      metric = "prop"
      , remove_clu_columns = FALSE
   )
#>           data_space   mean  lower  upper                 clu_fmt
#>               <char>  <num>  <num>  <num>                  <char>
#> 1:      all_positive  0.558  0.507  0.607       55.8% (50.7–60.7)
#> 2:    mixed_negative -0.100 -0.250  1.300 -10.0% (-25.0 to 130.0)
#> 3:      all_negative -0.100 -0.200 -0.050       -10.0% (5.0–20.0)
#> 4:    lower_negative  0.050 -0.020  0.120     5.0% (-2.0 to 12.0)
#> 5: central_lower_neg -0.050 -0.100  0.100   -5.0% (-10.0 to 10.0)
```

### Counts:

1.  Formatting is applied *after* scaling.
2.  e.g. 55,831,000 first scaled to 55.831000 …
3.  … then truncated to the correct sigfigs (55.8).
4.  Negative counts are not supported.
5.  Magnitudes from ones to billions are supported.

``` r
DF_count <- data.frame(
   data_space = c("thousands", "thousands_edge", "millions", "billions"),
   mean       = c(999000,      999999,           55831000,   5.4717e+12),
   lower      = c(888888,      888888,           50724000,   4.8266e+12),
   upper      = c(2222222,     2222222,          60797000,   5.9786e+12)
)
DF_count|>
   journalR::format_journal_df(
      metric = "count"
   )
#>       data_space                     clu_fmt
#> 1      thousands 999,000 (889,000–2,220,000)
#> 2 thousands_edge   1.00 million (0.889–2.22)
#> 3       millions    55.8 million (50.7–60.8)
#> 4       billions 5,470 billion (4,830–5,980)
```

- Count-space magnitude-edge-cases are handled.
- By default, thousands do not receive a label.
- Users may set their own style with `count_label_thousands = TRUE`.
  - See ‘Styles’ section below for more details.

``` r
journalR::new_style(
   style_name = "thousands_labeled"
   , count_label_thousands = TRUE
)
```

``` r
DF_count|>
   journalR::format_journal_df(
      metric               = "count"
      , style_name         = "thousands_labeled"
   )
#>       data_space                     clu_fmt
#> 1      thousands    999 thousand (889–2,220)
#> 2 thousands_edge   1.00 million (0.889–2.22)
#> 3       millions    55.8 million (50.7–60.8)
#> 4       billions 5,470 billion (4,830–5,980)
```

### Rates:

1.  Rates are like counts, but with a `per X magnitude` suffix.
    1.  The user may override the magnitude label easily.

``` r
DT_rates <- data.table::data.table(
   id    = 1:3,
   mean  = c(0.000123, 0.0000456, 0.00000789),
   lower = c(0.000100, 0.0000400, 0.00000700),
   upper = c(0.000150, 0.0000500, 0.00000900)
)

# Default magnitude (depends on style settings - see 'Styles' heading below)
journalR::format_journal_df(DT_rates, metric = "rate", rate_unit = "deaths")
#>       id                                clu_fmt
#>    <int>                                 <char>
#> 1:     1    12.3 deaths (10.0–15.0) per 100,000
#> 2:     2  45.6 deaths (40.0–50.0) per 1 million
#> 3:     3 78.9 deaths (70.0–90.0) per 10 million

# User Override - one magnitude applies to the entire data.frame
journalR::format_journal_df(DT_rates, metric = "rate", rate_unit = "deaths", mag = "per1m")
#>       id                               clu_fmt
#>    <int>                                <char>
#> 1:     1    123 deaths (100–150) per 1 million
#> 2:     2 45.6 deaths (40.0–50.0) per 1 million
#> 3:     3 7.89 deaths (7.00–9.00) per 1 million
```

If you’re not sure of the available ‘mag’ options, the error code will
tell you.

``` r
try(
   journalR::format_journal_df(DT_rates, metric = "rate", rate_unit = "deaths", mag = "I_dunno")
)
#> Error : 
#> Invalid option: i_dunno
#> Valid options:  per10, per100, per1k, per10k, per100k, per1m, per10m, per100m, per1b, per10b
```

This magnitude override is probably most useful for rates, but is
available for ALL metrics.

Proportions:

- In case you’ve already multiplied by 100

``` r
df_prop <- data.frame(
   id    = 1:3,
   mean  = c(55.8, 23.4, 78.9),
   lower = c(50.7, 20.1, 75.6),
   upper = c(60.7, 26.7, 82.1)
)

# The package tries to keep you safe...
try(
   journalR::format_journal_df(df_prop, metric = "prop")
)
#> Error : Proportion values must be between -1 and +1. Found values outside range, e.g.: 55.8

#... and this is how to use what you have
journalR::format_journal_df(df_prop, metric = "prop", mag = 'as-is')
#>   id           clu_fmt
#> 1  1 55.8% (50.7–60.7)
#> 2  2 23.4% (20.1–26.7)
#> 3  3 78.9% (75.6–82.1)
```

Counts:

``` r
df_count <- data.frame(
   id    = 1:3,
   mean  = c(55.8e7, 123.4e7, 5.67e10),
   lower = c(50.7e7, 110.2e7, 5.12e10),
   upper = c(60.7e7, 135.6e7, 6.23e10)
)

# Default
journalR::format_journal_df(df_count, metric = "count")
#>   id                  clu_fmt
#> 1  1    558 million (507–607)
#> 2  2 1.23 billion (1.10–1.36)
#> 3  3 56.7 billion (51.2–62.3)

# User Override
journalR::format_journal_df(df_count, metric = "count", mag = "b")
#>   id                     clu_fmt
#> 1  1 0.558 billion (0.507–0.607)
#> 2  2    1.23 billion (1.10–1.36)
#> 3  3    56.7 billion (51.2–62.3)
```

## Styles

Two formatting styles are built into journalR, and the user may extend
this.

- Nature (default)
- Lancet (particular)

Styles are a key/value list of metadata controlling rounding and
formatting.

Here is an example of a very particular style, and how these values
affect presentation tables.

``` r
print(style_lancet) 
#>                       key          value
#>                    <fctr>         <char>
#>  1:            style_name         lancet
#>  2:     prop_digits_round              1
#>  3:           prop_nsmall              1
#>  4:          count_method         sigfig
#>  5:     count_pad_sigfigs           TRUE
#>  6:   count_digits_sigfig              3
#>  7:          count_nsmall              1
#>  8:          decimal.mark              ·
#>  9:        count_big.mark               
#> 10: count_label_thousands          FALSE
#> 11:           rate_method         sigfig
#> 12:    rate_digits_sigfig              3
#> 13:      rate_pad_sigfigs           TRUE
#> 14:           rate_nsmall              1
#> 15:         neg_mark_mean a decrease of 
#> 16:           neg_mark_UI              –
#> 17:               UI_text               
#> 18:               UI_only          FALSE
#> 19:      assert_clu_order           TRUE
#> 20:             is_lancet           TRUE
#> 21:            round_5_up           TRUE
#>                       key          value
```

Lancet receives non-standard formatting

- Lancet decimals are `mid_dot()`.
- Thousands counts from 9,999 to 999,999 receive `thin_space()`
  delimeter.

``` r
journalR::format_journal_df(
   data.frame(
      mean    = c(55.8e3, 54.7e6)
      , lower = c(50.7e3, 48.6e6)
      , upper = c(60.7e3, 59.6e6)
   )
   , metric = "count"
   , style = "lancet"
)
#>                    clu_fmt
#> 1   55 800 (50 700–60 700)
#> 2 54·7 million (48·6–59·6)
```

Negatives are handled gracefully.

- Lancet negatives are `en_dash()`.

``` r
journalR::format_journal_df(
   data.table::data.table(
      data_space = c("all_positive", "mixed_negative", "all_negative")
      , mean     = c(.5584654,       -0.15665,         -0.1321684)
      , lower    = c(.5076231,       -0.25321,         -0.235321)
      , upper    = c(.6076589,       1.365432,         -0.056549)
   )
   , metric = "prop"
   , style = "lancet"
)
#>        data_space                              clu_fmt
#>            <char>                               <char>
#> 1:   all_positive                    55·8% (50·8–60·8)
#> 2: mixed_negative a decrease of 15·7% (–25·3 to 136·5)
#> 3:   all_negative       a decrease of 13·2% (5·7–23·5)
```

## Extending styles

Users may define their own styles by creating a named list with the same
keys as the built-in styles.

- All arguments except the name have sensible defaults - only change
  what you need.
- Read the `new_style()` documentation carefully to understand how each
  element affects your final output.
  - Using the “sigfig” `count_method` option involves the most
    assumptions
  - Read `fround_count_rate()` source in ‘R/format_vectors.R’ for
    implementation details.

``` r

journalR::new_style(
   style_name            = 'wacky_style'
   , prop_digits_round   = 3
   , prop_nsmall         = 3
   , count_digits_sigfig = 5
   , count_method        = "sigfig"
   , count_pad_sigfigs   = TRUE
   , count_nsmall        = 3
   , neg_mark_mean       = "Negative "
   , neg_mark_UI         = en_dash()
   , UI_text             = "95%UI "
)
```

<!-- Perhaps the user prefers standard thousands separators for counts. -->

``` r
# Counts
journalR::format_journal_df(
   data.frame(
      mean          = c(55.8e3, 54.7e6)
      , lower         = c(50.7e3, 48.6e6)
      , upper         = c(60.7e3, 59.6e6)
   )
   , metric = "count"
   , style  = "wacky_style"
)
#>                                clu_fmt
#> 1         55,800 (95%UI 50,700–60,700)
#> 2 54.700 million (95%UI 48.600–59.600)

# Proportions
journalR::format_journal_df(
   data.table::data.table(
      data_space    = c("all_positive", "mixed_negative", "all_negative")
      , mean          = c(.5584654, -0.15665, -0.1321684)
      , lower         = c(.5076231, -0.25321, -0.235321)
      , upper         = c(.6076589, 1.365432, -0.056549)
   )
   , metric = "prop"
   , style = "wacky_style"
)
#>        data_space                                     clu_fmt
#>            <char>                                      <char>
#> 1:   all_positive               55.847% (95%UI 50.762–60.766)
#> 2: mixed_negative Negative 15.665% (95%UI –25.321 to 136.543)
#> 3:   all_negative       Negative 13.217% (95%UI 5.655–23.532)
```

## Contributing

[Contribution Style
Guide](https://github.com/epi-sam/journalR/blob/main/.github/CONTRIBUTING.md)

## Code of Conduct

Please note that the journalR project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
