#' ---
#' title: "02 - Data Quality and Cleaning"
#' output: github_document
#' ---
#'
#' # 2. Data Quality and Cleaning
#'
#' Input: `data/processed/base_join2.rds` (output of notebook 01, built on the
#' synthetic dataset).

#+ libraries, message=FALSE, warning=FALSE
library(tidyverse)
library(skimr)
library(naniar)

base_join2 <- readRDS("data/processed/base_join2.rds")
n_initial <- nrow(base_join2)

#' ## Summary statistics
#'
#' First, a preliminary review of summary statistics, missing values and
#' histograms by variable type.

skim(base_join2)

#' ## Business-rule check: machines sold in 2021
#'
#' The extraction was defined to contain alerts from **2020**, so it is
#' unusual that machines *sold in 2021* appear with alerts in 2020.

base_join2 %>%
  filter(year_sold == "2021") %>%
  summarise(n = n())

#' * **Affected records:** `r sum(base_join2$year_sold == "2021", na.rm = TRUE)`
#' * **Decision:** remove them; they are not significant (`r scales::percent(sum(base_join2$year_sold == "2021", na.rm = TRUE) / n_initial, accuracy = 0.1)` of the data) and violate the extraction rule.

base_join2 <- base_join2 %>%
  filter(is.na(year_sold) | year_sold != "2021")

#' ## Missing values
#'
#' The table below shows the number (`n_miss`) and percentage (`pct_miss`) of
#' missing values per variable, relative to the **`r format(nrow(base_join2), big.mark = ",")`
#' records** that remain.

miss_var_summary(base_join2) %>%
  filter(n_miss != 0)

#+ missing-map, fig.height=5
vis_miss(base_join2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#+ miss-share, include=FALSE
miss_desc <- sum(is.na(base_join2$machine_type_l2)) / nrow(base_join2)

#' The variables coming from the *equipment master* (`machine_type_l2`,
#' `year_sold`) are missing in **`r scales::percent(miss_desc, accuracy = 0.01)`** of the
#' records: machines that raised alerts but are not in the equipment master. After consulting the telemetry team, the explanation is
#' that they are machines purchased before 2011 that have already exceeded their
#' useful life and are **not candidates for after-sales service**. We therefore
#' decide to discard them.
#'
#' ## Bias analysis
#'
#' We now check the missing values of `sum_ocr_cnt` (number of fault
#' occurrences) and `tla` (fault category) to see whether dropping them would
#' bias the analysis.
#'
#' **Definition of bias:** differences in the percentage of missing values
#' **greater than 5 percentage points** between the categories of `year_sold`,
#' which is a relevant variable for detecting failures.
#'
#' **Missing `sum_ocr_cnt` by `year_sold`:**

miss_ocr <- base_join2 %>%
  group_by(year_sold) %>%
  summarise(n_miss = sum(is.na(sum_ocr_cnt)), n = n(),
            pct = n_miss / n * 100)
miss_ocr

#' The gap between the year with the highest share of missing values
#' (**`r miss_ocr %>% filter(!is.na(year_sold)) %>% slice_max(pct, n = 1) %>% pull(year_sold)`**
#' - `r round(max(miss_ocr$pct[!is.na(miss_ocr$year_sold)]), 1)`%) and the lowest
#' (**`r miss_ocr %>% filter(!is.na(year_sold)) %>% slice_min(pct, n = 1) %>% pull(year_sold)`**
#' - `r round(min(miss_ocr$pct[!is.na(miss_ocr$year_sold)]), 1)`%) is below 5
#' points, so dropping these records **does not create bias**.
#'
#' **Missing `tla` by `year_sold`:**

miss_tla <- base_join2 %>%
  group_by(year_sold) %>%
  summarise(n_miss = sum(is.na(tla)), n = n(),
            pct = n_miss / n * 100)
miss_tla

#' Year **2012** has `r miss_tla$n_miss[miss_tla$year_sold == "2012" & !is.na(miss_tla$year_sold)]`
#' missing records (`r round(miss_tla$pct[miss_tla$year_sold == "2012" & !is.na(miss_tla$year_sold)], 2)`%).
#' With our definition this looks like bias, so we inspect 2012 in more detail.

base_join2 %>%
  filter(year_sold == "2012", alert_level == "UNKNOWN") %>%
  group_by(alert_level, alert_defn_dsc) %>%
  summarise(n_miss = sum(is.na(tla)), n = n(), pct = n_miss / n * 100,
            .groups = "drop") %>%
  filter(pct != 0)

#' The missing `tla` values of 2012 are all alerts of level `UNKNOWN`, grouped
#' under the description **`Invalid DTC Name`** (plus a handful of `test`
#' entries). They are not real faults, so they carry no information for
#' failure analysis. We can omit them without distorting the analysis.
#'
#' ## Cleaning
#'
#' The missing `sum_ocr_cnt` values are within the 5-point tolerance, and the
#' 2012 spike in `tla` is fully explained by non-informative `Invalid DTC Name`
#' records, so no relevant bias remains. All rows with missing values are
#' dropped.

base_limpia <- drop_na(base_join2)

#' The clean dataset (`base_limpia`) has **`r format(nrow(base_limpia), big.mark = ",")`
#' observations and `r ncol(base_limpia)` variables**
#' (`r format(n_initial - nrow(base_limpia), big.mark = ",")` rows removed,
#' `r scales::percent((n_initial - nrow(base_limpia)) / n_initial, accuracy = 0.1)` of the initial data).

#+ save
saveRDS(base_limpia, "data/processed/base_limpia_pre_outliers.rds")
