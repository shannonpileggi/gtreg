# style_xxx works with tbl_ae family

    Code
      as.data.frame(t1)
    Output
                            **Adverse Event** **Grade 1** **Grade 2** **Grade 3**
      1  Blood and lymphatic system disorders   xx (xx.x)   xx (xx.x)   xx (xx.x)
      2                               Anaemia   xx (xx.x)   xx (xx.x)   xx (xx.x)
      3          Increased tendency to bruise   xx (xx.x)   xx (xx.x)   xx (xx.x)
      4               Iron deficiency anaemia   xx (xx.x)   xx (xx.x)   xx (xx.x)
      5                      Thrombocytopenia   xx (xx.x)   xx (xx.x)   xx (xx.x)
      6            Gastrointestinal disorders   xx (xx.x)   xx (xx.x)   xx (xx.x)
      7                   Difficult digestion   xx (xx.x)   xx (xx.x)   xx (xx.x)
      8                 Intestinal dilatation   xx (xx.x)   xx (xx.x)   xx (xx.x)
      9                             Myochosis   xx (xx.x)   xx (xx.x)   xx (xx.x)
      10           Non-erosive reflux disease   xx (xx.x)   xx (xx.x)   xx (xx.x)
      11        Pancreatic enzyme abnormality   xx (xx.x)   xx (xx.x)   xx (xx.x)
         **Grade 4** **Grade 5**
      1    xx (xx.x)   xx (xx.x)
      2    xx (xx.x)   xx (xx.x)
      3    xx (xx.x)   xx (xx.x)
      4    xx (xx.x)   xx (xx.x)
      5    xx (xx.x)   xx (xx.x)
      6    xx (xx.x)   xx (xx.x)
      7    xx (xx.x)   xx (xx.x)
      8    xx (xx.x)   xx (xx.x)
      9    xx (xx.x)   xx (xx.x)
      10   xx (xx.x)   xx (xx.x)
      11   xx (xx.x)   xx (xx.x)

---

    Code
      as.data.frame(t2)
    Output
                            **Adverse Event** **Grade 1** **Grade 2** **Grade 3**
      1  Blood and lymphatic system disorders          xx          xx          xx
      2                               Anaemia          xx          xx          xx
      3          Increased tendency to bruise          xx          xx          xx
      4               Iron deficiency anaemia          xx          xx          xx
      5                      Thrombocytopenia          xx          xx          xx
      6            Gastrointestinal disorders          xx          xx          xx
      7                   Difficult digestion          xx          xx          xx
      8                 Intestinal dilatation          xx          xx          xx
      9                             Myochosis          xx          xx          xx
      10           Non-erosive reflux disease          xx          xx          xx
      11        Pancreatic enzyme abnormality          xx          xx          xx
         **Grade 4** **Grade 5**
      1           xx          xx
      2           xx          xx
      3           xx          xx
      4           xx          xx
      5           xx          xx
      6           xx          xx
      7           xx          xx
      8           xx          xx
      9           xx          xx
      10          xx          xx
      11          xx          xx

---

    Code
      as.data.frame(t3)
    Output
                            **Adverse Event** **Any Grade Complication**
      1  Blood and lymphatic system disorders                  xx (xx.x)
      2                               Anaemia                  xx (xx.x)
      3          Increased tendency to bruise                  xx (xx.x)
      4               Iron deficiency anaemia                  xx (xx.x)
      5                      Thrombocytopenia                  xx (xx.x)
      6            Gastrointestinal disorders                  xx (xx.x)
      7                   Difficult digestion                  xx (xx.x)
      8                 Intestinal dilatation                  xx (xx.x)
      9                             Myochosis                  xx (xx.x)
      10           Non-erosive reflux disease                  xx (xx.x)
      11        Pancreatic enzyme abnormality                  xx (xx.x)
         **Grade 3+ Complication**
      1                  xx (xx.x)
      2                  xx (xx.x)
      3                  xx (xx.x)
      4                  xx (xx.x)
      5                  xx (xx.x)
      6                  xx (xx.x)
      7                  xx (xx.x)
      8                  xx (xx.x)
      9                  xx (xx.x)
      10                 xx (xx.x)
      11                 xx (xx.x)

# style_xxx works with tbl_reg_summary

    Code
      as.data.frame(t4)
    Output
        **Characteristic**  **N = xx**
      1  Biological Marker        <NA>
      2      N Non-missing          xx
      3          Mean (SD)     xx (xx)
      4    Median (Q1, Q3) xx (xx, xx)
      5           Min, Max      xx, xx
      6          N Missing          xx
      7    Treatment Group        <NA>
      8             Drug A  xx (xx.x%)
      9             Drug B  xx (xx.x%)

---

    Code
      as.data.frame(t5)
    Output
        **Characteristic**  **N = xx**
      1  Biological Marker        <NA>
      2      N Non-missing          xx
      3          Mean (SD) xx.x (xx.x)
      4    Median (Q1, Q3) xx (xx, xx)
      5           Min, Max      xx, xx
      6          N Missing        xx.x
      7    Treatment Group        <NA>
      8             Drug A  xx (xx.x%)
      9             Drug B  xx (xx.x%)

