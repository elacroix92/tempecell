Oxygen Analysis & Figures - Biogeochem. 2020
================
Emily Lacroix
4/15/2020

## Set-Up

### Load libraries

``` r
library(MASS)
library(MuMIn)
library(car)
library(readxl)
library(modelr)
library(tidyverse)

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
```

### Designate filepaths

``` r
data_file <- "~/Desktop/TempeCell_MasterSpreadsheet.xlsx"
```

### Figure labels

``` r
#Field key
fields <- 
  tribble(
    ~crop_type, ~fields,
    "Native Grassland", c("7-1", "8-6", "3-6"),
    "Riparian Grassland", c("RG-1", "RG-2", "RG-3"),
    "Wheat/Cover", c("1-6", "3-8", "7-3"),
    "Wheat/Fallow", c("1-7", "4-9", "8-3"),
  ) %>% 
  unnest()
```

``` r
moisture_label <- c("low" = "< 80 % WFPS", "high" = "> 80 % WFPS")
```

## Import & Clean Data

### Import data

Filter out any Fe cores as they were not utilized in our incubations

``` r
grav_data <- 
  read_xlsx(
    data_file, 
    sheet = "GravimetricData",
    na = "NA"
  ) %>%   
  filter(!str_detect(sample, "Fe"))
```

``` r
raw_do <- 
  read_xlsx(
    data_file, 
    sheet = "DissolvedOxygen",
    na = "NA"
  )
```

``` r
gas_filled <- 
  read_xlsx(data_file, sheet = "GasFilledBags", na = "NA")
```

### Clean-up data

This code:

  - sets all negative DO values to zero
  - sets all DO values greater than 283 to 283
  - omits any data points with a dilution factor greater than 1
  - removes all data points where the bags filled with nitrogen gas
  - classifies core moistures into two classes: above and below 80% WFPS
  - calculates the oxygen consumption in each core
  - classifies our data into different disturbance classes

<!-- end list -->

``` r
do <- 
  raw_do %>% 
  mutate_at(vars(adj_do), ~ if_else(. < 0, 0, .)) %>% 
  mutate_at(vars(adj_do), ~ if_else(. > 283, 283, .)) %>% 
  mutate_at(vars(incubated_days, pressure), as.integer) %>% 
  left_join(fields, by = c("field" = "fields")) %>% 
  filter(df <= 1) %>% 
  anti_join(
    gas_filled, 
    by = 
      c(
        "field", 
        "incubated_days" = "day", 
        "pressure", 
        "moisture", 
        "disturbed"
      )
  ) %>% 
  left_join(
    grav_data %>% select(-sample), 
    by = 
      c("field", "incubated_days" = "day", "disturbed", "moisture")
  ) %>% 
  mutate(
    AMC = case_when(
      wfps < 0.80 ~ "low",
      wfps > 0.80 ~ "high")
  ) %>% 
  mutate_at(
    vars(AMC),
    ~ factor(., levels = c("low", "high"))
  ) %>% 
  mutate(
    total_do = adj_do * flush_vol / 1000,
    theoretical_max = 283 * flush_vol / 1000,
    umol_consumed = theoretical_max - total_do
  ) %>% 
  mutate(
    tillage = 
      case_when(
        crop_type %in% c("Wheat/Fallow", "Wheat/Cover") ~ "Tilled Cropland",
        crop_type %in% c("Native Grassland", "Riparian Grassland") & 
          disturbed == FALSE ~ "Undisturbed Grassland",
        crop_type %in% c("Native Grassland", "Riparian Grassland") & 
          disturbed == TRUE ~ "Sieved & Re-packed Grassland",
      )
  ) %>% 
  drop_na(pressure)
```

## Figures & Tables

### Table 3: Oxygen Consumed

``` r
do %>% 
  drop_na(umol_consumed) %>% 
  group_by(pressure, tillage) %>% 
  summarise(
    mean_umol_consumed = mean(umol_consumed, na.rm = TRUE),
    se_umol_consumed = sd(umol_consumed, na.rm = TRUE)/sqrt(n())
  ) %>% 
  arrange(tillage, pressure) %>%
  mutate_at(
    vars(mean_umol_consumed, se_umol_consumed), 
    ~ round(., digits = 3)
  ) %>% 
  knitr::kable()
```

| pressure | tillage                      | mean\_umol\_consumed | se\_umol\_consumed |
| -------: | :--------------------------- | -------------------: | -----------------: |
|      100 | Sieved & Re-packed Grassland |                1.770 |              0.191 |
|      300 | Sieved & Re-packed Grassland |                0.160 |              0.039 |
|      500 | Sieved & Re-packed Grassland |                0.060 |              0.011 |
|      100 | Tilled Cropland              |                0.618 |              0.111 |
|      300 | Tilled Cropland              |                0.090 |              0.021 |
|      500 | Tilled Cropland              |                0.073 |              0.015 |
|      100 | Undisturbed Grassland        |                0.517 |              0.086 |
|      300 | Undisturbed Grassland        |                0.079 |              0.015 |
|      500 | Undisturbed Grassland        |                0.068 |              0.011 |

### Table 3: Proportion of Oxygen Consumed

``` r
do %>% 
  drop_na(umol_consumed) %>% 
  pivot_wider(
    id_cols = 
      c(
        "field", 
        "incubated_days", 
        "crop_type", 
        "wfps", 
        "AMC", 
        "tillage",
        "disturbed"
      ),
    names_from = "pressure",
    values_from = "umol_consumed"
  ) %>% 
  rename_at(
    vars(`100`,`300`,`500`),
    ~ paste0("umol_consumed_",.)
  ) %>% 
  mutate(
    total_umol_consumed = 
      umol_consumed_100 + umol_consumed_300 + umol_consumed_500,
    prop_consumed_100 = umol_consumed_100 / total_umol_consumed,
    prop_consumed_300 = umol_consumed_300 / total_umol_consumed,  
    prop_consumed_500 = umol_consumed_500 / total_umol_consumed,
    
  ) %>% 
  drop_na(total_umol_consumed) %>% 
  group_by(tillage) %>% 
  summarize(
    mean_prop_100 = mean(prop_consumed_100, na.rm = TRUE),
    se_prop_100 = sd(prop_consumed_100, na.rm = TRUE) / sqrt(n()),
    mean_prop_300 = mean(prop_consumed_300, na.rm = TRUE),
    se_prop_300 = sd(prop_consumed_300, na.rm = TRUE) / sqrt(n()),
    mean_prop_500 = mean(prop_consumed_500, na.rm = TRUE),
    se_prop_500 = sd(prop_consumed_500, na.rm = TRUE) / sqrt(n())
  ) %>% 
  pivot_longer(
    cols = c(
      mean_prop_100, 
      mean_prop_300, 
      mean_prop_500, 
      se_prop_100, 
      se_prop_300,
      se_prop_500
    )
  ) %>% 
  rename(prop_consumed = value) %>% 
  mutate(
    pressure = str_extract(name, "\\d{3}"),
    value_type = str_extract(name, "\\w{2,4}(?=_)")
  ) %>% 
  pivot_wider(
    id_cols = c("tillage", "pressure"),
    names_from = c("value_type"),
    values_from = "prop_consumed"
  ) %>% 
  mutate_at(vars(mean, se), round, digits = 4) %>% 
  knitr::kable()
```

| tillage                      | pressure |   mean |     se |
| :--------------------------- | :------- | -----: | -----: |
| Sieved & Re-packed Grassland | 100      | 0.9090 | 0.0209 |
| Sieved & Re-packed Grassland | 300      | 0.0640 | 0.0182 |
| Sieved & Re-packed Grassland | 500      | 0.0270 | 0.0060 |
| Tilled Cropland              | 100      | 0.6801 | 0.0649 |
| Tilled Cropland              | 300      | 0.1568 | 0.0402 |
| Tilled Cropland              | 500      | 0.1631 | 0.0469 |
| Undisturbed Grassland        | 100      | 0.7063 | 0.0695 |
| Undisturbed Grassland        | 300      | 0.1116 | 0.0260 |
| Undisturbed Grassland        | 500      | 0.1821 | 0.0616 |

### Figure S4: Boxplot of DO by disturbance and moisture

``` r
do %>% 
  mutate(
    tillage = 
      case_when(
        crop_type %in% c("Wheat/Fallow", "Wheat/Cover") ~ "Tilled Cropland",
        crop_type %in% c("Native Grassland", "Riparian Grassland") & 
          disturbed == FALSE ~ "Undisturbed Grassland",
        crop_type %in% c("Native Grassland", "Riparian Grassland") & 
          disturbed == TRUE ~ "Sieved & Re-packed Grassland",
      )
  ) %>% 
  mutate_at(
    vars(tillage),
    ~ factor(
      ., 
      levels = 
        c(
          "Undisturbed Grassland", 
          "Tilled Cropland", 
          "Sieved & Re-packed Grassland" 
        ),
      labels = 
        c(
          "Undisturbed \nGrassland", 
          "Tilled \nCropland",
          "Sieved \nGrassland"
        )
      )
  ) %>% 
  drop_na(pressure) %>% 
  mutate_at(
    vars(pressure),
    ~ factor(
      .,
      levels = c(100, 300, 500), 
      ordered = TRUE 
    )
  ) %>% 
  ggplot(aes(x = pressure, y = adj_do)) + 
  geom_hline(aes(yintercept = 283), linetype = 2, color = "darkred") +
  geom_hline(aes(yintercept = 15), linetype = 2, color = "darkred") +
  geom_boxplot(
    aes(fill = tillage), 
    position = position_dodge(0.8),
    color = "black"
  ) +
  scale_fill_brewer(palette = "Greys") + 
  scale_x_discrete(
    labels = 
      c(
        expression(paste("> 3", mu, "m")),
        expression(paste("1 - 3", mu, "m")),
        expression(paste("0.6 - 1", mu, "m"))
      )
  ) +
  facet_grid(
    rows = vars(AMC),
    labeller = 
      labeller(
        AMC = moisture_label,
        pressure = label_parsed
      )
  ) +
  theme_bw() + 
  theme(
    legend.position = "right",
    legend.text = element_text(margin = margin(b = 2, t = 2, unit = "pt")),
    axis.ticks.x = element_line(color = "grey92")
  ) + 
  labs(
    fill = "Degree of \nDisturbance",
    y = expression(paste("Dissolved Oxygen (",mu,"M)")),
    x = "Effective Pore Diameter"
  ) 
```

![](DO_publication_scriptv3_files/figure-gfm/Fig%20S4-1.png)<!-- -->

### Figure 3: Boxplot of DO by disturbance, averaged across moisture classes

**Calculate the means for each pore size**

``` r
average_do_by_pore <- 
  do %>% 
  mutate(
    tillage = 
      case_when(
        crop_type %in% c("Wheat/Fallow", "Wheat/Cover") ~ "Tilled Cropland",
        crop_type %in% c("Native Grassland", "Riparian Grassland") & 
          disturbed == FALSE ~ "Undisturbed Grassland",
        crop_type %in% c("Native Grassland", "Riparian Grassland") & 
          disturbed == TRUE ~ "Sieved & Re-packed Grassland",
      )
  ) %>% 
  mutate_at(
    vars(tillage),
    ~ factor(
      ., 
      levels = 
        c(
          "Undisturbed Grassland", 
          "Tilled Cropland", 
          "Sieved & Re-packed Grassland" 
        ),
      labels = 
        c(
          "Undisturbed \nGrassland", 
          "Tilled \nCropland",
          "Sieved \nGrassland"
        )
      )
  ) %>% 
  drop_na(pressure) %>% 
  mutate_at(
    vars(pressure),
    ~ factor(
      .,
      levels = c(100, 300, 500),
      ordered = TRUE
    )
  ) %>%
  group_by(pressure) %>% 
  summarise(
    mean_do = mean(adj_do, na.rm = TRUE),
    se_do = sd(adj_do, na.rm = TRUE)/sqrt(n())
  ) 
```

``` r
average_do_by_pore %>% knitr::kable()
```

| pressure | mean\_do |    se\_do |
| :------- | -------: | --------: |
| 100      | 154.9337 | 10.134812 |
| 300      | 214.3501 |  8.144492 |
| 500      | 160.2981 | 10.615931 |

**Plot**

``` r
do %>% 
  mutate(
    tillage = 
      case_when(
        crop_type %in% c("Wheat/Fallow", "Wheat/Cover") ~ "Tilled Cropland",
        crop_type %in% c("Native Grassland", "Riparian Grassland") & 
          disturbed == FALSE ~ "Undisturbed Grassland",
        crop_type %in% c("Native Grassland", "Riparian Grassland") & 
          disturbed == TRUE ~ "Sieved & Re-packed Grassland",
      )
  ) %>% 
  mutate_at(
    vars(tillage),
    ~ factor(
      ., 
      levels = 
        c(
          "Undisturbed Grassland", 
          "Tilled Cropland", 
          "Sieved & Re-packed Grassland" 
        ),
      labels = 
        c(
          "Undisturbed Grassland", 
          "Tilled Cropland",
          "Disturbed Grassland"
        )
      )
  ) %>% 
  drop_na(pressure) %>% 
  mutate_at(
    vars(pressure),
    ~ factor(
      .,
      levels = c(100, 300, 500), 
      ordered = TRUE 
    )
  ) %>% 
  ggplot() + 
  geom_hline(aes(yintercept = 283), linetype = 2, color = "black") +
  geom_hline(aes(yintercept = 15), linetype = 2, color = "black") +
  geom_boxplot(
    aes(x = pressure, y = adj_do, fill = tillage), 
    position = position_dodge(0.8),
  ) + 
  geom_pointrange(
    data = average_do_by_pore,
    aes(
      x = pressure,
      y = mean_do,
      ymin = mean_do - se_do,
      ymax = mean_do + se_do
    ),
    color = "darkred",
    alpha = 0.8
  ) +
  scale_fill_brewer(palette = "Greys") + 
  scale_x_discrete(
    labels =
      c(
        expression(paste("> 3", mu, "m")),
        expression(paste("1 - 3", mu, "m")),
        expression(paste("0.6 - 1", mu, "m"))
      )
  ) +
  theme_bw() + 
  theme(
    rect = element_rect(fill = "transparent"),
    legend.position = "right",
    legend.text = element_text(margin = margin(b = 2, t = 2, unit = "pt")),
    text = element_text(family = "Arial", color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA)
  ) + 
  labs(
    fill = NULL,
    y = expression(paste("Dissolved Oxygen (",mu,"M)")),
    x = "Effective Pore Diameter"
  ) 
```

![](DO_publication_scriptv3_files/figure-gfm/Fig%203-1.png)<!-- -->

### Figure 4: DO concentration over time

``` r
do %>% 
  filter(disturbed == FALSE) %>% 
  drop_na(pressure) %>% 
  mutate_at(
    vars(pressure),
    ~ factor(
      .,
      levels = c(100, 300, 500), 
      ordered = TRUE, 
      labels = 
        c(
          expression(paste("> 3", mu, "m")),
          expression(paste("1 - 3", mu, "m")),
          expression(paste("0.6 - 1", mu, "m"))
        )
    )
  ) %>% 
  group_by(incubated_days, pressure, AMC) %>% 
  summarise(
    adj_do_mean = mean(adj_do, na.rm = TRUE),
    se_adj_do = sd(adj_do, na.rm = TRUE) / sqrt(n())
  ) %>% 
  ggplot(
    aes(
      x = incubated_days, 
      color = AMC, 
      fill = AMC, 
      y = adj_do_mean
    )
  ) + 
  geom_point(size = 2, shape = 21) +
  geom_errorbar(
    aes(
      ymin = adj_do_mean - se_adj_do, 
      ymax = adj_do_mean + se_adj_do
    ),
    width = 1
  ) +
  geom_line(aes(linetype = AMC)) + 
  geom_hline(yintercept = 283, linetype = 2) + 
  scale_x_continuous(breaks = c(7, 14, 28)) + 
  scale_color_manual(values = c("gray60", "grey12")) + 
  scale_fill_manual(
    values = c("gray60", "grey12"), 
    labels = c("< 80", "> 80")
  ) + 
  scale_linetype_manual(
    values = c(2,3)
  ) + 
  facet_grid(
    cols = vars(pressure),
    labeller = label_parsed
  ) + 
  labs(
    y = expression(paste("Dissolved Oxygen (",mu,"M)")),
    x = "Days Incubated",
    fill = " Moisture \n(% WFPS)"
  ) + 
  guides(
    color = "none",
    linetype = "none"
  ) +
  theme_bw() + 
  theme(
    legend.title = element_text(size = 8, face = "bold", color = "black"),
  ) 
```

    ## Warning: Removed 1 rows containing missing values (geom_errorbar).

![](DO_publication_scriptv3_files/figure-gfm/Figure%204-1.png)<!-- -->

## Statistics

### Sample Sizes (N)

``` r
do %>%
  drop_na(umol_consumed) %>%
  group_by(pressure, tillage, incubated_days) %>% 
  summarise(
    n = n()
  ) %>% 
  knitr::kable()
```

| pressure | tillage                      | incubated\_days |  n |
| -------: | :--------------------------- | --------------: | -: |
|      100 | Sieved & Re-packed Grassland |               7 |  9 |
|      100 | Sieved & Re-packed Grassland |              14 |  9 |
|      100 | Sieved & Re-packed Grassland |              28 |  8 |
|      100 | Tilled Cropland              |               7 | 12 |
|      100 | Tilled Cropland              |              14 | 12 |
|      100 | Tilled Cropland              |              28 | 11 |
|      100 | Undisturbed Grassland        |               7 | 11 |
|      100 | Undisturbed Grassland        |              14 | 11 |
|      100 | Undisturbed Grassland        |              28 | 11 |
|      300 | Sieved & Re-packed Grassland |               7 |  8 |
|      300 | Sieved & Re-packed Grassland |              14 | 10 |
|      300 | Sieved & Re-packed Grassland |              28 |  8 |
|      300 | Tilled Cropland              |               7 | 12 |
|      300 | Tilled Cropland              |              14 |  8 |
|      300 | Tilled Cropland              |              28 | 11 |
|      300 | Undisturbed Grassland        |               7 | 12 |
|      300 | Undisturbed Grassland        |              14 |  8 |
|      300 | Undisturbed Grassland        |              28 | 12 |
|      500 | Sieved & Re-packed Grassland |               7 |  4 |
|      500 | Sieved & Re-packed Grassland |              14 |  6 |
|      500 | Sieved & Re-packed Grassland |              28 |  4 |
|      500 | Tilled Cropland              |               7 |  7 |
|      500 | Tilled Cropland              |              14 |  4 |
|      500 | Tilled Cropland              |              28 | 11 |
|      500 | Undisturbed Grassland        |               7 |  7 |
|      500 | Undisturbed Grassland        |              14 |  5 |
|      500 | Undisturbed Grassland        |              28 |  8 |

### Checking Normality for adj\_do values

``` r
do %>% 
  drop_na(adj_do) %>% 
  group_by(tillage, pressure) %>% 
  summarise(
    normality_stat = 
      if_else(
        sd(adj_do, na.rm = TRUE) != 0, 
        shapiro.test(adj_do)$statistic, 
        NA_real_
      ),
    normality_p_value =  
      if_else(
        sd(adj_do, na.rm = TRUE) != 0, 
        shapiro.test(adj_do)$p.value, 
        NA_real_
      )
  ) %>% 
  knitr::kable()
```

| tillage                      | pressure | normality\_stat | normality\_p\_value |
| :--------------------------- | -------: | --------------: | ------------------: |
| Sieved & Re-packed Grassland |      100 |       0.8193964 |           0.0003834 |
| Sieved & Re-packed Grassland |      300 |       0.8743850 |           0.0036419 |
| Sieved & Re-packed Grassland |      500 |       0.9330076 |           0.3360729 |
| Tilled Cropland              |      100 |       0.9159908 |           0.0109459 |
| Tilled Cropland              |      300 |       0.7968337 |           0.0000451 |
| Tilled Cropland              |      500 |       0.8717639 |           0.0084440 |
| Undisturbed Grassland        |      100 |       0.8672518 |           0.0008360 |
| Undisturbed Grassland        |      300 |       0.8136532 |           0.0000744 |
| Undisturbed Grassland        |      500 |       0.8853948 |           0.0221509 |

The data are not normal.

### Check for equal variances

``` r
leveneTest(
  adj_do ~ tillage*pressure, 
  data = do %>% 
  mutate_at(vars(pressure), as.factor)
)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##        Df F value   Pr(>F)   
    ## group   8  3.3234 0.001285 **
    ##       231                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The variances amongst the tillage x pressure groups are unequal.

### GLM fits - adj\_do

Create a dataframe with unique observation identifiers to use in GLMMs

``` r
do_core_id <-
  do %>% 
  group_by(field, incubated_days, disturbed, moisture) %>% 
  pivot_wider(
    .,
    id_cols = c(
      field, 
      incubated_days, 
      disturbed, 
      moisture, 
      AMC,
      crop_type,
      bulk_dens,
      wfps,
      dry_mass,
      tillage
    ),
    names_from = pressure, 
    values_from = adj_do
  ) %>% 
  rowid_to_column() %>% 
  pivot_longer(
    cols = c(`100`, `300`, `500`), 
    names_to = "pressure", 
    values_to = "adj_do"
  ) %>% 
  rename(
    core_id = rowid
  ) %>% 
  rowid_to_column(var = "obsv_id")
```

#### GLMM for overall

``` r
glmmPQL_overall <-
  glmmPQL(
    adj_do ~ tillage + bulk_dens * wfps + incubated_days + pressure,
    random = list(field = ~1, core_id = ~1),
    family = quasipoisson(link = "log"),
    data = 
      do_core_id %>% 
      ungroup() %>% 
      mutate_at(vars(adj_do), as.integer) %>% 
      mutate_at(vars(pressure, field), as.factor)
  )
```

``` r
glmmPQL_overall %>% 
  summary()
```

    ## Linear mixed-effects model fit by maximum likelihood
    ##  Data: do_core_id %>% ungroup() %>% mutate_at(vars(adj_do), as.integer) %>%      mutate_at(vars(pressure, field), as.factor) 
    ##   AIC BIC logLik
    ##    NA  NA     NA
    ## 
    ## Random effects:
    ##  Formula: ~1 | field
    ##          (Intercept)
    ## StdDev: 0.0001785561
    ## 
    ##  Formula: ~1 | core_id %in% field
    ##          (Intercept) Residual
    ## StdDev: 1.531082e-15 6.207426
    ## 
    ## Variance function:
    ##  Structure: fixed weights
    ##  Formula: ~invwt 
    ## Fixed effects: adj_do ~ tillage + bulk_dens * wfps + incubated_days + pressure 
    ##                                  Value Std.Error  DF   t-value p-value
    ## (Intercept)                   4.711709 2.0361627 138  2.314014  0.0221
    ## tillageTilled Cropland        0.593177 0.1093232  10  5.425902  0.0003
    ## tillageUndisturbed Grassland  0.438898 0.0871701  83  5.034963  0.0000
    ## bulk_dens                     0.222279 1.6320316  83  0.136198  0.8920
    ## wfps                         -0.253505 2.3922505  83 -0.105969  0.9159
    ## incubated_days               -0.012992 0.0035671  83 -3.642292  0.0005
    ## pressure300                   0.337072 0.0696201 138  4.841598  0.0000
    ## pressure500                   0.039314 0.0851762 138  0.461559  0.6451
    ## bulk_dens:wfps                0.068380 1.9057522  83  0.035881  0.9715
    ##  Correlation: 
    ##                              (Intr) tllgTC tllgUG blk_dn wfps   incbt_
    ## tillageTilled Cropland       -0.139                                   
    ## tillageUndisturbed Grassland -0.172  0.588                            
    ## bulk_dens                    -0.993  0.115  0.152                     
    ## wfps                         -0.972 -0.026  0.115  0.963              
    ## incubated_days               -0.067 -0.139 -0.037  0.045  0.083       
    ## pressure300                  -0.035  0.015  0.009  0.010  0.019 -0.005
    ## pressure500                  -0.022 -0.015 -0.013  0.007  0.010 -0.056
    ## bulk_dens:wfps                0.974  0.021 -0.123 -0.976 -0.994 -0.084
    ##                              prs300 prs500
    ## tillageTilled Cropland                    
    ## tillageUndisturbed Grassland              
    ## bulk_dens                                 
    ## wfps                                      
    ## incubated_days                            
    ## pressure300                               
    ## pressure500                   0.465       
    ## bulk_dens:wfps               -0.015 -0.009
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.5253429 -0.5761056  0.1284165  0.7174297  1.9646499 
    ## 
    ## Number of Observations: 240
    ## Number of Groups: 
    ##              field core_id %in% field 
    ##                 12                100

#### GLMM for \> 3.0 um

``` r
glmmPQL_do100 <-
  glmmPQL(
    adj_do ~ tillage + bulk_dens * wfps + incubated_days,
    random = list(field = ~1),
    family = quasipoisson(link = "log"),
    data = 
      do_core_id %>% 
      ungroup() %>% 
      filter(pressure == 100) %>% 
      mutate_at(vars(adj_do), as.integer) %>% 
      mutate_at(vars(pressure, field), as.factor)
  )
```

``` r
glmmPQL_do100 %>% summary() 
```

    ## Linear mixed-effects model fit by maximum likelihood
    ##  Data: do_core_id %>% ungroup() %>% filter(pressure == 100) %>% mutate_at(vars(adj_do),      as.integer) %>% mutate_at(vars(pressure, field), as.factor) 
    ##   AIC BIC logLik
    ##    NA  NA     NA
    ## 
    ## Random effects:
    ##  Formula: ~1 | field
    ##         (Intercept) Residual
    ## StdDev:  0.03400426 6.444551
    ## 
    ## Variance function:
    ##  Structure: fixed weights
    ##  Formula: ~invwt 
    ## Fixed effects: adj_do ~ tillage + bulk_dens * wfps + incubated_days 
    ##                                  Value Std.Error DF   t-value p-value
    ## (Intercept)                   0.849542  3.647389 77  0.232918  0.8164
    ## tillageTilled Cropland        1.269303  0.226968 10  5.592425  0.0002
    ## tillageUndisturbed Grassland  1.239524  0.197890 77  6.263693  0.0000
    ## bulk_dens                     3.029374  2.980979 77  1.016235  0.3127
    ## wfps                          3.786330  4.228632 77  0.895403  0.3734
    ## incubated_days               -0.017798  0.006805 77 -2.615480  0.0107
    ## bulk_dens:wfps               -3.331945  3.442136 77 -0.967988  0.3361
    ##  Correlation: 
    ##                              (Intr) tllgTC tllgUG blk_dn wfps   incbt_
    ## tillageTilled Cropland       -0.136                                   
    ## tillageUndisturbed Grassland -0.163  0.752                            
    ## bulk_dens                    -0.993  0.096  0.128                     
    ## wfps                         -0.975 -0.013  0.090  0.968              
    ## incubated_days               -0.052 -0.096 -0.032  0.029  0.063       
    ## bulk_dens:wfps                0.975  0.008 -0.100 -0.979 -0.994 -0.064
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.0796254 -0.8723966  0.1071104  0.6544227  2.6076041 
    ## 
    ## Number of Observations: 94
    ## Number of Groups: 12

#### GLMM for 1.0 - 3.0 um

``` r
glmmPQL_do300 <-
  glmmPQL(
    adj_do ~ tillage + bulk_dens * wfps + incubated_days,
    random = list(field = ~1),
    family = quasipoisson(link = "log"),
    data = 
      do_core_id %>% 
      ungroup() %>% 
      filter(pressure == 300) %>% 
      mutate_at(vars(adj_do), as.integer) %>% 
      mutate_at(vars(pressure, field), as.factor)
  )
```

``` r
glmmPQL_do300 %>% summary()
```

    ## Linear mixed-effects model fit by maximum likelihood
    ##  Data: do_core_id %>% ungroup() %>% filter(pressure == 300) %>% mutate_at(vars(adj_do),      as.integer) %>% mutate_at(vars(pressure, field), as.factor) 
    ##   AIC BIC logLik
    ##    NA  NA     NA
    ## 
    ## Random effects:
    ##  Formula: ~1 | field
    ##         (Intercept) Residual
    ## StdDev:  0.06994757 4.910189
    ## 
    ## Variance function:
    ##  Structure: fixed weights
    ##  Formula: ~invwt 
    ## Fixed effects: adj_do ~ tillage + bulk_dens * wfps + incubated_days 
    ##                                  Value Std.Error DF    t-value p-value
    ## (Intercept)                   3.984414 2.4239441 73  1.6437730  0.1045
    ## tillageTilled Cropland        0.348075 0.1372383 10  2.5362856  0.0296
    ## tillageUndisturbed Grassland  0.310391 0.0990173 73  3.1347126  0.0025
    ## bulk_dens                     1.209940 1.9454593 73  0.6219303  0.5359
    ## wfps                          1.538255 2.8901127 73  0.5322475  0.5962
    ## incubated_days               -0.011848 0.0042774 73 -2.7700064  0.0071
    ## bulk_dens:wfps               -1.393745 2.3029641 73 -0.6051962  0.5469
    ##  Correlation: 
    ##                              (Intr) tllgTC tllgUG blk_dn wfps   incbt_
    ## tillageTilled Cropland       -0.114                                   
    ## tillageUndisturbed Grassland -0.169  0.515                            
    ## bulk_dens                    -0.994  0.090  0.152                     
    ## wfps                         -0.971 -0.055  0.111  0.963              
    ## incubated_days               -0.053 -0.128 -0.046  0.031  0.068       
    ## bulk_dens:wfps                0.972  0.051 -0.119 -0.975 -0.995 -0.070
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -3.0518385 -0.3881736  0.1261416  0.5835724  1.9499325 
    ## 
    ## Number of Observations: 90
    ## Number of Groups: 12

#### GLMM for 0.6 - 1.0 um

``` r
glmmPQL_do500 <-
  glmmPQL(
    adj_do ~ tillage + bulk_dens * wfps + incubated_days,
    random = list(field = ~1),
    family = quasipoisson(link = "log"),
    data = 
      do_core_id %>% 
      ungroup() %>% 
      filter(pressure == 500) %>% 
      mutate_at(vars(adj_do), as.integer) %>% 
      mutate_at(vars(pressure, field), as.factor)
  )
```

``` r
glmmPQL_do500 %>% summary()
```

    ## Linear mixed-effects model fit by maximum likelihood
    ##  Data: do_core_id %>% ungroup() %>% filter(pressure == 500) %>% mutate_at(vars(adj_do),      as.integer) %>% mutate_at(vars(pressure, field), as.factor) 
    ##   AIC BIC logLik
    ##    NA  NA     NA
    ## 
    ## Random effects:
    ##  Formula: ~1 | field
    ##         (Intercept) Residual
    ## StdDev: 1.19041e-05 5.968618
    ## 
    ## Variance function:
    ##  Structure: fixed weights
    ##  Formula: ~invwt 
    ## Fixed effects: adj_do ~ tillage + bulk_dens * wfps + incubated_days 
    ##                                   Value Std.Error DF   t-value p-value
    ## (Intercept)                   13.889205  5.078996 39  2.734636  0.0093
    ## tillageTilled Cropland         0.453048  0.248622 10  1.822240  0.0984
    ## tillageUndisturbed Grassland  -0.329103  0.189996 39 -1.732157  0.0911
    ## bulk_dens                     -6.695662  3.964809 39 -1.688773  0.0992
    ## wfps                         -13.041742  5.892545 39 -2.213262  0.0328
    ## incubated_days                -0.010164  0.007598 39 -1.337729  0.1887
    ## bulk_dens:wfps                10.015433  4.559564 39  2.196577  0.0341
    ##  Correlation: 
    ##                              (Intr) tllgTC tllgUG blk_dn wfps   incbt_
    ## tillageTilled Cropland       -0.259                                   
    ## tillageUndisturbed Grassland -0.233  0.380                            
    ## bulk_dens                    -0.994  0.252  0.223                     
    ## wfps                         -0.967  0.068  0.220  0.953              
    ## incubated_days               -0.085 -0.191 -0.021  0.061  0.107       
    ## bulk_dens:wfps                0.972 -0.080 -0.230 -0.969 -0.994 -0.106
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.1045717 -0.5679045  0.1559384  0.5625136  2.4590971 
    ## 
    ## Number of Observations: 56
    ## Number of Groups: 12
