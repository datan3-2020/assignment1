Statistical assignment 1
================
\[add your name and candidate number here\]
\[add date here\]

## Open data (20 points)

In this assignment you will work with individual-level data from wave 8
of the Understanding Society. First, you need to open the data set.
Please complete the code below.

``` r
library(tidyverse)

Data <- read_tsv("/Users/bessudnov/Documents/datan3_2020/data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

Now you have got your data frame stored as Data.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society were collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the result

``` r
Data <- Data %>%
        select(pidp, h_eumem, h_sex_dv, h_age_dv, h_memorig)
```

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of the analysis (see the User Guide, pp. 65-71).
We will not do this in this assignment. However, what we want to do is
to keep data from the original Understanding Society sample (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain.

``` r
Data <- Data %>%
        filter(h_memorig == 1)
```

## Recode the data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/wave/8/datafile/h_indresp>

We want to do the following:

1)  recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know” as NA).
2)  recode sex into a character variable with the values “male” or
    “female”.
3)  recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables with these data.

``` r
Data <- Data %>%
        mutate(EU = case_when(
                h_eumem < 1 ~ NA_real_,
# Note that we use NA_real_ here since the variable is numeric. If it was a character vector, it would have been NA_character_
                h_eumem == 1 ~ 1,
                h_eumem == 2 ~ 0
               )
        ) %>%
        mutate(sex = recode(h_sex_dv, `1` = "male", `2` = "female", .default = NA_character_)) %>%
        mutate(agegr = case_when(
                between(h_age_dv, 16, 25) ~ "16-25",
# check the use of between by typing "? between" in the console
                between(h_age_dv, 26, 40) ~ "26-40",
                between(h_age_dv, 41, 55) ~ "41-55",
                between(h_age_dv, 56, 70) ~ "56-70",
                h_age_dv > 70 ~ "70+"
        ))
```

## Summarise data (10 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
Data %>%
        filter(!is.na(EU)) %>%
        # This filters out missing values
        count(EU) %>%
        mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 2 x 3
    ##      EU     n  perc
    ##   <dbl> <int> <dbl>
    ## 1     0  9338  45.6
    ## 2     1 11118  54.4

Write a couple of sentences with the interpretation of this result. How
does this compare with the result of the 2016 referendum? Why?

In our sample, 54% said that they supported Remain, and 46% supported
Leave. Evidently, this is different from the result of the 2016
referendum. A number of factors could affect this: 1) the timing of data
collection for the Understanding Society, 2) survey representation
errors (coverage, sampling, non-response), 3) measurement error (people
unwilling to reveal their true preferences in surveys), 4) differential
probability of turnout (saying that you support Remain in a survey is
not the same as actually voting in the referendum), 5) our estimates
from the Understanding Society have not been weighted properly, etc.

## Summarise data by sex and age (30 points)

Now let us look at support by Leave and Remain by sex and age. Use your
newly created variables.

By sex:

``` r
Data %>%
        group_by(sex) %>%
        summarise(
                meanEU = mean(EU, na.rm = TRUE)
        )
```

    ## # A tibble: 3 x 2
    ##   sex    meanEU
    ##   <chr>   <dbl>
    ## 1 female  0.567
    ## 2 male    0.514
    ## 3 <NA>    1

By age groups:

``` r
Data %>%
        group_by(agegr) %>%
        summarise(
                meanEU = mean(EU, na.rm = TRUE)
        )
```

    ## # A tibble: 5 x 2
    ##   agegr meanEU
    ##   <chr>  <dbl>
    ## 1 16-25  0.696
    ## 2 26-40  0.618
    ## 3 41-55  0.546
    ## 4 56-70  0.492
    ## 5 70+    0.411

By sex and age group:

``` r
Data %>%
        filter(!is.na(sex)) %>%
        group_by(sex, agegr) %>%
        summarise(
                meanEU = mean(EU, na.rm = TRUE)
        )
```

    ## # A tibble: 10 x 3
    ## # Groups:   sex [2]
    ##    sex    agegr meanEU
    ##    <chr>  <chr>  <dbl>
    ##  1 female 16-25  0.726
    ##  2 female 26-40  0.640
    ##  3 female 41-55  0.569
    ##  4 female 56-70  0.505
    ##  5 female 70+    0.442
    ##  6 male   16-25  0.661
    ##  7 male   26-40  0.588
    ##  8 male   41-55  0.517
    ##  9 male   56-70  0.476
    ## 10 male   70+    0.376

Write a couple of sentences interpreting your results.

Younger people are considerably more likely to support Remain than older
people. Women are somewhat more likely to support Remain than men in all
age groups.
