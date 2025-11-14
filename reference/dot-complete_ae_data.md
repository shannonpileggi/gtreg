# Create a complete and expanded data frame for tabulating adverse events

Returns a data frame that has an observation for each patient in the
study, with combinations for each ID, SOC, and AE. The returned data
frame includes new logical columns `"..ae.."` and `"..soc.."` indicating
whether that row should be included when tabulating the AE table. When
multiple AEs of the same type are observed, the AE with the largest
`by=` value is the observation to be used in the tabulation.

## Usage

``` r
.complete_ae_data(
  data,
  id,
  ae,
  soc = NULL,
  by = NULL,
  strata = NULL,
  id_df = NULL,
  by_values = NULL,
  missing_text = "Unknown",
  missing_location = "first"
)
```

## Arguments

- data:

  Data frame

- id:

  String variable name of the patient ID

- ae:

  String variable name of the adverse event column

- soc:

  Optional string variable name of the system organ class column

- by:

  Optional string variable to split results by, e.g. report AEs by grade
  or attribution

- strata:

  Optional string variable to stratify results by, e.g. report AEs
  summaries by treatment group

- id_df:

  Optional data frame of complete id values and strata to achieve
  correct base n for the situation in which not all subjects experience
  adverse events

- by_values:

  Optional vector of complete by values, listed in desired order, to
  achieve correct table structure for the situation in which an adverse
  event of a certain grade is not observed for a given soc

- missing_text:

  String that will be shown for missing levels of `by=`, Default is
  `"Unknown"`

- missing_location:

  location where the column summarizing values with missing levels `by=`
  will be located in the final table. Must be one of
  `c("first", "last", "hide)`. Default is `"first"`

## Value

a tibble

## Examples

``` r
df_adverse_events %>%
  .complete_ae_data(
    id = "patient_id",
    ae = "adverse_event",
    soc = "system_organ_class",
    by = "grade",
    strata = "trt"
  )
#> # A tibble: 126 × 7
#>    id    strata soc                                  ae     by    ..soc.. ..ae..
#>    <chr> <chr>  <fct>                                <chr>  <fct> <lgl>   <lgl> 
#>  1 ID 1  Drug B Blood and lymphatic system disorders Anaem… 4     FALSE   TRUE  
#>  2 ID 1  Drug B Blood and lymphatic system disorders Incre… 4     FALSE   FALSE 
#>  3 ID 1  Drug B Blood and lymphatic system disorders Incre… 5     TRUE    TRUE  
#>  4 ID 1  Drug B Blood and lymphatic system disorders Iron … NOT … FALSE   TRUE  
#>  5 ID 1  Drug B Blood and lymphatic system disorders Throm… 1     FALSE   FALSE 
#>  6 ID 1  Drug B Blood and lymphatic system disorders Throm… 3     FALSE   TRUE  
#>  7 ID 1  Drug B Gastrointestinal disorders           Diffi… NOT … FALSE   TRUE  
#>  8 ID 1  Drug B Gastrointestinal disorders           Intes… 1     FALSE   FALSE 
#>  9 ID 1  Drug B Gastrointestinal disorders           Intes… 2     FALSE   TRUE  
#> 10 ID 1  Drug B Gastrointestinal disorders           Myoch… 3     FALSE   FALSE 
#> # ℹ 116 more rows
```
