# add_overall() works

    Code
      tbl1 %>% as.data.frame()
    Output
                    **Adverse Event** **Grade 1** **Grade 2** **Grade 3** **Grade 4**
      1                       Anaemia        <NA>        <NA>           2           2
      2           Difficult digestion           1        <NA>        <NA>           3
      3  Increased tendency to bruise        <NA>        <NA>        <NA>           4
      4         Intestinal dilatation           2           1        <NA>        <NA>
      5       Iron deficiency anaemia           1           2        <NA>           2
      6                     Myochosis        <NA>           3           1           1
      7    Non-erosive reflux disease           4        <NA>        <NA>           3
      8 Pancreatic enzyme abnormality           2           1           2           2
      9              Thrombocytopenia        <NA>           1           3           1
        **Grade 5** **Overall**
      1           3           7
      2           1           5
      3           2           6
      4           1           4
      5           2           7
      6           3           8
      7           3          10
      8           1           8
      9           4           9

---

    Code
      tbl1 %>% as.data.frame()
    Output
                            **Adverse Event** **Grade 1** **Grade 2** **Grade 3**
      1  Blood and lymphatic system disorders        <NA>      1 (33)        <NA>
      2                               Anaemia        <NA>        <NA>      1 (33)
      3          Increased tendency to bruise        <NA>        <NA>        <NA>
      4               Iron deficiency anaemia        <NA>        <NA>        <NA>
      5                      Thrombocytopenia        <NA>      1 (33)        <NA>
      6            Gastrointestinal disorders        <NA>        <NA>        <NA>
      7                   Difficult digestion        <NA>        <NA>        <NA>
      8                 Intestinal dilatation      1 (33)        <NA>        <NA>
      9                             Myochosis        <NA>      2 (67)      1 (33)
      10           Non-erosive reflux disease     3 (100)        <NA>        <NA>
      11        Pancreatic enzyme abnormality        <NA>        <NA>      1 (33)
         **Grade 4** **Grade 5** **Overall** **Grade 1** **Grade 2** **Grade 3**
      1       1 (33)      1 (33)     3 (100)        <NA>        <NA>        <NA>
      2       1 (33)        <NA>      2 (67)        <NA>        <NA>      1 (14)
      3       1 (33)        <NA>      1 (33)        <NA>        <NA>        <NA>
      4       1 (33)      1 (33)      2 (67)      1 (14)      2 (29)        <NA>
      5       1 (33)        <NA>      2 (67)        <NA>        <NA>      3 (43)
      6       2 (67)      1 (33)     3 (100)        <NA>        <NA>        <NA>
      7      3 (100)        <NA>     3 (100)      1 (14)        <NA>        <NA>
      8         <NA>        <NA>      1 (33)      1 (14)      1 (14)        <NA>
      9         <NA>        <NA>     3 (100)        <NA>      1 (14)        <NA>
      10        <NA>        <NA>     3 (100)      1 (14)        <NA>        <NA>
      11      1 (33)      1 (33)     3 (100)      2 (29)      1 (14)      1 (14)
         **Grade 4** **Grade 5** **Overall** **Grade 1** **Grade 2** **Grade 3**
      1       1 (14)      6 (86)     7 (100)        <NA>      1 (10)        <NA>
      2       1 (14)      3 (43)      5 (71)        <NA>        <NA>      2 (20)
      3       3 (43)      2 (29)      5 (71)        <NA>        <NA>        <NA>
      4       1 (14)      1 (14)      5 (71)      1 (10)      2 (20)        <NA>
      5         <NA>      4 (57)     7 (100)        <NA>      1 (10)      3 (30)
      6       2 (29)      5 (71)     7 (100)        <NA>        <NA>        <NA>
      7         <NA>      1 (14)      2 (29)      1 (10)        <NA>        <NA>
      8         <NA>      1 (14)      3 (43)      2 (20)      1 (10)        <NA>
      9       1 (14)      3 (43)      5 (71)        <NA>      3 (30)      1 (10)
      10      3 (43)      3 (43)     7 (100)      4 (40)        <NA>        <NA>
      11      1 (14)        <NA>      5 (71)      2 (20)      1 (10)      2 (20)
         **Grade 4** **Grade 5** **Overall**
      1       2 (20)      7 (70)    10 (100)
      2       2 (20)      3 (30)      7 (70)
      3       4 (40)      2 (20)      6 (60)
      4       2 (20)      2 (20)      7 (70)
      5       1 (10)      4 (40)      9 (90)
      6       4 (40)      6 (60)    10 (100)
      7       3 (30)      1 (10)      5 (50)
      8         <NA>      1 (10)      4 (40)
      9       1 (10)      3 (30)      8 (80)
      10      3 (30)      3 (30)    10 (100)
      11      2 (20)      1 (10)      8 (80)

