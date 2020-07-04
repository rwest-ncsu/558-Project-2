Project 2
================
Robert West
6/30/2020

## Set up the call to Analysis.RMD

To automate R Markdown, we need to pass in a list of day parameters into
the `Analysis.Rmd` file with the proper output names. This is done below
by putting all of this information into a tibble and then applying the
`rmarkdown::render()` function to produce the
files.

``` r
days = c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
out_file = paste0(days, ".md")

params = lapply(days, FUN=function(x){
  return(list(day=x))
})

reports = tibble(days, out_file, params)
reports
```

    ## # A tibble: 7 x 3
    ##   days      out_file     params          
    ##   <chr>     <chr>        <list>          
    ## 1 monday    monday.md    <named list [1]>
    ## 2 tuesday   tuesday.md   <named list [1]>
    ## 3 wednesday wednesday.md <named list [1]>
    ## 4 thursday  thursday.md  <named list [1]>
    ## 5 friday    friday.md    <named list [1]>
    ## 6 saturday  saturday.md  <named list [1]>
    ## 7 sunday    sunday.md    <named list [1]>

## Generating the reports

``` r
library(rmarkdown)

apply(reports, MARGIN = 1, FUN=function(x){
  render(input = "Analysis.Rmd", output_file = x[[2]], params=x[[3]])
})
```

Here are the the reports that are generated for each day:

  - The analysis for [Monday is available here](Analysis.md).  
  - The analysis for [Tuesday is available here](tuesday.md).  
  - The analysis for [Wednesday is available here](wednesday.md).  
  - The analysis for [Thursday is available here](thursday.md).  
  - The analysis for [Friday is available here](friday.md).  
  - The analysis for [Saturday is available here](saturday.md).  
  - The analysis for [Sunday is available here](sunday.md).
