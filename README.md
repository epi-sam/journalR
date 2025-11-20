
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
and enjoy constant updates and handoffs between team members.

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
DT_mpg <- DT[
   , .(
        mean  = mean(mpg)
      , lower = quantile(mpg, 0.025)
      , upper = quantile(mpg, 0.975)
   )
   , by = cyl
]

print(DT_mpg)
#>      cyl     mean  lower   upper
#>    <num>    <num>  <num>   <num>
#> 1:     6 19.74286 17.845 21.3400
#> 2:     4 26.66364 21.425 33.5250
#> 3:     8 15.10000 10.400 19.0375
```

Once they’re ready for presenation, you’ll want to format them.

Enter `format_journal_df()`:

- Format a mean/lower/upper set of three columns into a
  `mean (lower -- upper)` string.
  - Ready for easy copy/paste into Word or similar.
- The user must provide a data type (`d_type`), which tells the function
  how to format the numbers.
  - mean/lower/upper relationships are asserted (lower \< mean \< upper)
    before formatting.

``` r
DT_mpg |>
   journalR::format_journal_df(
      d_type = "count"
   )
#>      cyl          clu_fmt
#>    <num>           <char>
#> 1:     6 19.7 (17.8–21.3)
#> 2:     4 26.7 (21.4–33.5)
#> 3:     8 15.1 (10.4–19.0)
```

## Options

The user’s data.frame may be formatted differently than the simple case
above, with different data types.

- Things get messy in the real world!

``` r
# build a mean/lower/uppper summary table with some percentage from DT
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

- Nature (defautl)
- Lancet (particular)

Styles are a key/value list of metadata controlling rounding and
formatting.

Here is an example of a very particular style, and how these values
affect presentation tables.

``` r
# print(style_lancet()) # as a list
print(lancet) # formatted a little nicer
#>                          key          value
#>                       <fctr>         <char>
#>  1:        digits_round_prop              1
#>  2:                   nsmall              1
#>  3:      digits_sigfig_count              3
#>  4:             decimal.mark              ·
#>  5:            negative_sign              –
#>  6:           big.mark_count               
#>  7:            mean_neg_text a decrease of 
#>  8:                  UI_only          FALSE
#>  9:                  UI_text               
#> 10: assert_clu_relationships           TRUE
#> 11:                is_lancet           TRUE
#> 12:          allow_thousands          FALSE
```

Counts receive a non-standard thousands separator.

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

Netagives are handled gracefully.

- Lancet decimals are `mid_dot()` characters

``` r
journalR::format_journal_df(
   data.table::data.table(
                  data_space    = c("all_positive", "mixed_negative", "all_negative")
                , mean          = c(.5584654, -0.15665, -0.1321684)
                , lower         = c(.5076231, -0.25321, -0.235321)
                , upper         = c(.6076589, 1.365432, -0.056549)
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

``` r

journalR::new_style(
   style_name                 = 'my_style'
   , digits_round_prop        = 3
   , digits_sigfig_count      = 5
   , nsmall                   = 3
   , decimal.mark             = "."
   , negative_sign            = "-"
   , big.mark_count           = ","
   , mean_neg_text            = "Negative "
   , UI_only                  = FALSE
   , UI_text                  = "95%UI "
   , assert_clu_relationships = TRUE
   , is_lancet                = FALSE
   , allow_thousands          = FALSE
)
```

<!-- Perhaps the user prefers standard thousands separators for counts. -->

``` r
journalR::format_journal_df(
   data.frame(
        mean          = c(55.8e3, 54.7e6)
      , lower         = c(50.7e3, 48.6e6)
      , upper         = c(60.7e3, 59.6e6)
   )
   , d_type = "count"
   , style  = "my_style"
)
#>                                clu_fmt
#> 1         55,800 (95%UI 50,700–60,700)
#> 2 54.700 million (95%UI 48.600–59.600)
```

<!-- Or perhaps thousands should be treated like millions with high precision. -->

Done.
