
<!-- README.md is generated from README.Rmd. Please edit that file -->

# journalR

<!-- badges: start -->

<!-- badges: end -->

The goal of journalR is to ease real-world scientific publication
writing.

## Installation

You can install the development version of journalR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("epi-sam/journalR")
```

## Basics

In the Platonic ideal world, a journal is written in pure LaTeX,
Rmarkdown, or the like.

In the real world, journals are written collaboratively in Word, with
messy versioning, multiple copies of the same file, broken citations,
and enjoy constant updates and hand offs between team members.

Sometimes, you just need to copy and paste some numbers.

Enter journalR.

``` r
library(journalR)
# data.table is not required, but the author finds it helpful.
library(data.table)
```

Let’s say you have done some statistical analysis.

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

Once they’re ready for presentation, you’ll want to format them.

Enter `format_journal_df()`:

- Format a central/lower/upper set of three columns into an
  e.g. `mean (lower -- upper)` string.
  - Ready for easy copy/paste into Word or similar.
- The user must provide a data type (`d_type`), which tells the function
  how to format the numbers.
  - central/lower/upper relationships are asserted (lower \< central \<
    upper) before formatting.

``` r
DT_hp |>
   journalR::format_journal_df(
      d_type = "count"
   )
#>      cyl         clu_fmt
#>    <num>          <char>
#> 1:     4 82.6 (54.5–112)
#> 2:     6   122 (106–167)
#> 3:     8   209 (150–312)
```

## Assumptions

### General:

1.  Central/lower/upper triplets are formatted at the same ‘scale’.
2.  All triplets present with the same number of significant digits.

### Counts:

1.  The central value controls the scaling
    1.  Formatting is applied *after* scaling
        1.  e.g. 55,831,000 first scaled to 55.831000 …
        2.  … then truncated to the correct sigfigs (55.8)
2.  Negative counts are not supported
3.  Magnitudes over 1 billion are not supported

``` r
DF_count <- data.frame(
      data_space = c("thousands", "thousands_edge", "millions", "billions"),
      mean       = c(999000,      999999,           55831000,   5.4717e+12),
      lower      = c(888888,      888888,           50724000,   4.8266e+12),
      upper      = c(2222222,     2222222,          60797000,   5.9786e+12)
)
DF_count|>
   journalR::format_journal_df(
      d_type = "count"
   )
#>       data_space                     clu_fmt
#> 1      thousands 999,000 (889,000–2,220,000)
#> 2 thousands_edge   1.00 million (0.889–2.22)
#> 3       millions    55.8 million (50.7–60.8)
#> 4       billions 5,470 billion (4,830–5,980)
```

- Counts are rounded, but not truncated below 1 million
  - e.g. 999,999 presents as 999,000 if rounded to 3 digits
  - Users may set their own style with `label_thousands = TRUE` to
    override this behavior

``` r
new_style(
   style_name = "thousands_labeled"
   , label_thousands = TRUE
)
```

``` r
DF_count|>
   journalR::format_journal_df(
      d_type               = "count"
      , style_name         = "thousands_labeled"
   )
#>       data_space                     clu_fmt
#> 1      thousands    999 thousand (889–2,220)
#> 2 thousands_edge   1.00 million (0.889–2.22)
#> 3       millions    55.8 million (50.7–60.8)
#> 4       billions 5,470 billion (4,830–5,980)
```

### Proportions:

1.  Proportions (prop) and Percentage Points (pp) are all multiplied by
    100
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
      d_type = "prop"
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
      d_type               = c("prop")
      , new_var            = "pct_vshape_fmt" # specify output colname (must be a new one)
      , central_var        = "pct_vshape" 
      , lower_var          = "lower_vshape"
      , upper_var          = "upper_vshape"
      , remove_clu_columns = FALSE # retain the original columns if desired
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
# print(style_lancet()) # as a list
print(lancet) # formatted a little nicer
#>                     key          value
#>                  <fctr>         <char>
#>  1:   prop_digits_round              1
#>  2:         prop_nsmall              1
#>  3:        count_method         sigfig
#>  4:   count_pad_sigfigs           TRUE
#>  5: count_digits_sigfig              3
#>  6:        count_nsmall              1
#>  7:        decimal.mark              ·
#>  8:      count_big.mark               
#>  9:       neg_mark_mean a decrease of 
#> 10:         neg_mark_UI              –
#> 11:             UI_text               
#> 12:             UI_only          FALSE
#> 13:    assert_clu_order           TRUE
#> 14:     label_thousands          FALSE
#> 15:           is_lancet           TRUE
#> 16:          round_5_up           TRUE
```

Lancet receives non-standard formatting

- Lancet decimals are `mid_dot()`.
- Thousands counts from 9,999 to 999,999 recieve `thin_space()`
  delimeter.

``` r
journalR::format_journal_df(
   data.frame(
        mean          = c(55.8e3, 54.7e6)
      , lower         = c(50.7e3, 48.6e6)
      , upper         = c(60.7e3, 59.6e6)
   )
   , d_type = "count"
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
                  data_space    = c("all_positive", "mixed_negative", "all_negative")
                , mean          = c(.5584654,       -0.15665,         -0.1321684)
                , lower         = c(.5076231,       -0.25321,         -0.235321)
                , upper         = c(.6076589,       1.365432,         -0.056549)
             )
   , d_type = "prop"
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
  - Read the `new_style()` documentation carefully to understand how
    each element affects your final output.
  - Using the “sigfig” `count_method` option involves the most
    assumptions
    - Read `fround_count()` source in ‘R/format_vectors.R’ for
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
   , d_type = "count"
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
   , d_type = "prop"
   , style = "wacky_style"
)
#>        data_space                                     clu_fmt
#>            <char>                                      <char>
#> 1:   all_positive               55.847% (95%UI 50.762–60.766)
#> 2: mixed_negative Negative 15.665% (95%UI –25.321 to 136.543)
#> 3:   all_negative       Negative 13.217% (95%UI 5.655–23.532)
```

Done.
