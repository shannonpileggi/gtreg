# basic as_table_shell works 

    Code
      as_tibble(tbl1)
    Output
      # A tibble: 11 x 11
         `**Adverse Event**`   `**1**` `**2**` `**3**` `**4**` `**5**` `**1**` `**2**`
         <chr>                 <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
       1 Blood and lymphatic ~ <NA>    xx (xx) <NA>    xx (xx) xx (xx) <NA>    <NA>   
       2 Anaemia               <NA>    <NA>    xx (xx) xx (xx) <NA>    <NA>    <NA>   
       3 Increased tendency t~ <NA>    <NA>    <NA>    xx (xx) <NA>    <NA>    <NA>   
       4 Iron deficiency anae~ <NA>    <NA>    <NA>    xx (xx) xx (xx) xx (xx) xx (xx)
       5 Thrombocytopenia      <NA>    xx (xx) <NA>    xx (xx) <NA>    <NA>    <NA>   
       6 Gastrointestinal dis~ <NA>    <NA>    <NA>    xx (xx) xx (xx) <NA>    <NA>   
       7 Difficult digestion   <NA>    <NA>    <NA>    xx (xx) <NA>    xx (xx) <NA>   
       8 Intestinal dilatation xx (xx) <NA>    <NA>    <NA>    <NA>    xx (xx) xx (xx)
       9 Myochosis             <NA>    xx (xx) xx (xx) <NA>    <NA>    <NA>    xx (xx)
      10 Non-erosive reflux d~ xx (xx) <NA>    <NA>    <NA>    <NA>    xx (xx) <NA>   
      11 Pancreatic enzyme ab~ <NA>    <NA>    xx (xx) xx (xx) xx (xx) xx (xx) xx (xx)
      # ... with 3 more variables: `**3**` <chr>, `**4**` <chr>, `**5**` <chr>

---

    Code
      tbl1$table_styling$header$spanning_header
    Output
       [1] NA                   NA                   NA                  
       [4] NA                   "Drug A"             "**Drug A**, N = xx"
       [7] "**Drug A**, N = xx" "**Drug A**, N = xx" "**Drug A**, N = xx"
      [10] "**Drug A**, N = xx" "**Drug A**, N = xx" "Drug B"            
      [13] "**Drug B**, N = xx" "**Drug B**, N = xx" "**Drug B**, N = xx"
      [16] "**Drug B**, N = xx" "**Drug B**, N = xx" "**Drug B**, N = xx"

# modify spanning header as_table_shell works 

    Code
      as_tibble(tbl2_shell)
    Output
      # A tibble: 11 x 11
         `**Adverse Event**`   `**1**` `**2**` `**3**` `**4**` `**5**` `**1**` `**2**`
         <chr>                 <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
       1 Blood and lymphatic ~ <NA>    xx (xx~ <NA>    xx (xx~ xx (xx~ <NA>    <NA>   
       2 Anaemia               <NA>    <NA>    xx (xx~ xx (xx~ <NA>    <NA>    <NA>   
       3 Increased tendency t~ <NA>    <NA>    <NA>    xx (xx~ <NA>    <NA>    <NA>   
       4 Iron deficiency anae~ <NA>    <NA>    <NA>    xx (xx~ xx (xx~ xx (xx~ xx (xx~
       5 Thrombocytopenia      <NA>    xx (xx~ <NA>    xx (xx~ <NA>    <NA>    <NA>   
       6 Gastrointestinal dis~ <NA>    <NA>    <NA>    xx (xx~ xx (xx~ <NA>    <NA>   
       7 Difficult digestion   <NA>    <NA>    <NA>    xx (xx~ <NA>    xx (xx~ <NA>   
       8 Intestinal dilatation xx (xx~ <NA>    <NA>    <NA>    <NA>    xx (xx~ xx (xx~
       9 Myochosis             <NA>    xx (xx~ xx (xx~ <NA>    <NA>    <NA>    xx (xx~
      10 Non-erosive reflux d~ xx (xx~ <NA>    <NA>    <NA>    <NA>    xx (xx~ <NA>   
      11 Pancreatic enzyme ab~ <NA>    <NA>    xx (xx~ xx (xx~ xx (xx~ xx (xx~ xx (xx~
      # ... with 3 more variables: `**3**` <chr>, `**4**` <chr>, `**5**` <chr>

---

    Code
      tbl2_shell$table_styling$header$spanning_header
    Output
       [1] NA                                       
       [2] NA                                       
       [3] NA                                       
       [4] NA                                       
       [5] "Drug A"                                 
       [6] "**Control Group**, N = xx/xx (xx%)"     
       [7] "**Control Group**, N = xx/xx (xx%)"     
       [8] "**Control Group**, N = xx/xx (xx%)"     
       [9] "**Control Group**, N = xx/xx (xx%)"     
      [10] "**Control Group**, N = xx/xx (xx%)"     
      [11] "**Drug A**, N = xx"                     
      [12] "Drug B"                                 
      [13] "**Experimental Group**, N = xx/xx (xx%)"
      [14] "**Experimental Group**, N = xx/xx (xx%)"
      [15] "**Experimental Group**, N = xx/xx (xx%)"
      [16] "**Experimental Group**, N = xx/xx (xx%)"
      [17] "**Experimental Group**, N = xx/xx (xx%)"
      [18] "**Drug B**, N = xx"                     

