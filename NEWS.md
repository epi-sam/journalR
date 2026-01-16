# journalR 0.6.1

BUGFIX 

- CLU order assumption was over-checked at an inappropriate spot. 
- Converted count-space negative stop to warning.

# journalR 0.6.0

## Improvements

`format_means_df()` 

- now dispatches `fround_x()` helpers for more consistent formatting, especially for Lancet

## Breaking

- renamed `format_means_df()` to `format_metric_cols()`, with arg name change from `central_var` to `var_prefix`

# journalR 0.5.1

cran initial submit harmonization

- description update
- remove dontrun examples
- remove globalenv references

# journalR 0.5.0

mag override can now be passed down from user API

# journalR 0.4.0

 added rate-space metric/metric

# journalR 0.3.0

Internal state management improvements.  No changes to user interface.

# journalR 0.2.0

Ready for CRAN submission.

# journalR 0.1.0

All tests passing.
