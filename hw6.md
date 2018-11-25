P8105 Homework 6
================
2018-11-25

**Context**: This assignment reinforces ideas in Linear Models.

Problem 1
---------

Create a `city_state` variable (e.g. “Baltimore, MD”), and a binary variable indicating whether the homicide is solved. Omit cities Dallas, TX; Phoenix, AZ; and Kansas City, MO – these don’t report victim race. Also omit Tulsa, AL – this is a data entry mistake. Modifiy victim\_race to have categories white and non-white, with white as the reference category. Be sure that victim\_age is numeric

``` r
homicide_df = read_csv("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv", na = c("", "NA", "Unknown")) %>%
  mutate(
    city_state = str_c(city, state, sep = ", "),
    resolution = case_when(
      disposition == "Closed without arrest" ~ "unsolved",
      disposition == "Open/No arrest"        ~ "unsolved",
      disposition == "Closed by arrest"      ~ "solved"
    )
  ) %>% 
  filter(city_state != "Tulsa, AL", city_state != "Dallas, TX", city_state != "Phoenix, AZ", city_state != "Kansas City, MO") 
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_integer(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

``` r
# Modified `victim_race` to have categories
homicide_df$victim_race[which(homicide_df$victim_race != "White")] = "Non-White"

homi_df = homicide_df %>%
  mutate(victim_race = factor(victim_race, levels = c("Non-White", "White")))
  
# Checked to see if `victim_age` is numeric
is.numeric(homi_df$victim_age)
```

    ## [1] TRUE

``` r
# Confirmed that victim_age is numeric 

View(homi_df) 
table(homi_df$city_state)
```

    ## 
    ##    Albuquerque, NM        Atlanta, GA      Baltimore, MD 
    ##                378                973               2827 
    ##    Baton Rouge, LA     Birmingham, AL         Boston, MA 
    ##                424                800                614 
    ##        Buffalo, NY      Charlotte, NC        Chicago, IL 
    ##                521                687               5535 
    ##     Cincinnati, OH       Columbus, OH         Denver, CO 
    ##                694               1084                312 
    ##        Detroit, MI         Durham, NC     Fort Worth, TX 
    ##               2519                276                549 
    ##         Fresno, CA        Houston, TX   Indianapolis, IN 
    ##                487               2942               1322 
    ##   Jacksonville, FL      Las Vegas, NV     Long Beach, CA 
    ##               1168               1381                378 
    ##    Los Angeles, CA     Louisville, KY        Memphis, TN 
    ##               2257                576               1514 
    ##          Miami, FL      Milwaukee, wI    Minneapolis, MN 
    ##                744               1115                366 
    ##      Nashville, TN    New Orleans, LA       New York, NY 
    ##                767               1434                627 
    ##        Oakland, CA  Oklahoma City, OK          Omaha, NE 
    ##                947                672                409 
    ##   Philadelphia, PA     Pittsburgh, PA       Richmond, VA 
    ##               3037                631                429 
    ##     Sacramento, CA    San Antonio, TX San Bernardino, CA 
    ##                376                833                275 
    ##      San Diego, CA  San Francisco, CA       Savannah, GA 
    ##                461                663                246 
    ##      St. Louis, MO       Stockton, CA          Tampa, FL 
    ##               1677                444                208 
    ##          Tulsa, OK     Washington, DC 
    ##                583               1345

``` r
is.numeric(homi_df$resolution)
```

    ## [1] FALSE

For the city of Baltimore, MD, use the glm function to fit a logistic regression with resolved vs unresolved as the outcome and victim age, sex and race (as just defined) as predictors. Save the output of glm as an R object; apply the broom::tidy to this object; and obtain the estimate and confidence interval of the adjusted odds ratio for solving homicides comparing non-white victims to white victims keeping all other variables fixed.

``` r
baltimore = homicide_df %>%
  filter(city_state == "Baltimore, MD") %>%
  mutate(solved = as.numeric(resolution == "solved")) %>%
  select(solved, victim_age, victim_race, victim_sex) %>%
  glm(solved ~ victim_age + victim_race + victim_sex, data = ., family = binomial())
  
baltimore %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```

| term              |  log\_OR|     OR|  p.value|
|:------------------|--------:|------:|--------:|
| (Intercept)       |    0.366|  1.443|    0.029|
| victim\_age       |   -0.007|  0.993|    0.032|
| victim\_raceWhite |    0.820|  2.270|    0.000|
| victim\_sexMale   |   -0.888|  0.412|    0.000|

``` r
confint(baltimore)
```

    ## Waiting for profiling to be done...

    ##                        2.5 %        97.5 %
    ## (Intercept)       0.03847396  0.6977091435
    ## victim_age       -0.01342427 -0.0006274281
    ## victim_raceWhite  0.47856926  1.1642313291
    ## victim_sexMale   -1.15576002 -0.6218668703

Now run glm for each of the cities in your dataset, and extract the adjusted odds ratio (and CI) for solving homicides comparing non-white victims to white victims. Do this within a “tidy” pipeline, making use of purrr::map, list columns, and unnest as necessary to create a dataframe with estimated ORs and CIs for each city.
