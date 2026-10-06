#' ---
#' title: "04 - Descriptive Statistics"
#' output: github_document
#' ---
#'
#' # 4. Descriptive Statistics
#'
#' With a clean dataset (no missing values or outliers), we review descriptive
#' statistics by variable. Input: `data/processed/base_limpia.rds`
#' (output of notebook 03).

#+ libraries, message=FALSE, warning=FALSE
library(tidyverse)
library(skimr)

base_limpia <- readRDS("data/processed/base_limpia.rds")

#' The dataset has **`r format(nrow(base_limpia), big.mark = ",")` observations**
#' and **`r ncol(base_limpia)` variables**. The console output is long, but it
#' gives a clear picture of the values of each variable.

Hmisc::describe(base_limpia)

#' ## `first_dtc_engn_hours`: engine hours at failure
#'
#' Throughout the project we investigate which variables may help predict
#' failures. Here we analyse in detail the engine hours at the time of the
#' failure, starting with the skewness of the distribution.
#'
#' * **Skewness coefficient:**

psych::skew(base_limpia$first_dtc_engn_hours)

#' * **Kurtosis:**

psych::kurtosi(base_limpia$first_dtc_engn_hours)

#' * **Quartiles:**

quantile(base_limpia$first_dtc_engn_hours, prob = seq(0, 1, 0.25))

#' Summary by year of sale:

hours_by_year <- base_limpia %>%
  group_by(year_sold) %>%
  summarise(
    Min = min(first_dtc_engn_hours),
    Q1 = quantile(first_dtc_engn_hours, probs = 0.25),
    Median = quantile(first_dtc_engn_hours, probs = 0.50),
    Q3 = quantile(first_dtc_engn_hours, probs = 0.75),
    Max = max(first_dtc_engn_hours),
    Mean = mean(first_dtc_engn_hours),
    `Std. Dev.` = sd(first_dtc_engn_hours),
    Skewness = psych::skew(first_dtc_engn_hours),
    Kurtosis = psych::kurtosi(first_dtc_engn_hours)
  )
hours_by_year %>% mutate(across(where(is.numeric), ~ round(.x, 1)))

#+ hours-histograms, fig.width=9, fig.height=6
ggplot(base_limpia, aes(x = first_dtc_engn_hours, fill = year_sold)) +
  geom_histogram(col = "darkgrey", show.legend = FALSE, bins = 30) +
  labs(title = "Engine hours at failure, by year of sale",
       x = "Hours", y = "Frequency") +
  facet_wrap(~year_sold, scales = "free") +
  theme_minimal()

#+ example-year, include=FALSE
ex <- hours_by_year %>% filter(year_sold == "2019")

#' The histograms and the table together give a clear reading of each year.
#' For example, for machines sold in **2019** the engine hours at failure range
#' from **`r format(round(ex$Min, 1), big.mark = ",")` to
#' `r format(round(ex$Max, 1), big.mark = ",")` hours**, with a right-skewed
#' distribution (skewness = `r round(ex$Skewness, 2)`) around a mean of
#' `r format(round(ex$Mean, 1), big.mark = ",")` hours.
#'
#' ## `prod_line_nm`: product line
#'
#' `prod_line_nm` categorises the types of machinery. It is important to see
#' which product lines concentrate the alerts.

count_prod <- base_limpia %>%
  group_by(prod_line_nm) %>%
  count() %>%
  mutate(prop = n / nrow(base_limpia))
count_prod

#+ prod-line-plot, fig.width=9, fig.height=5
count_prod %>%
  ggplot(aes(x = reorder(prod_line_nm, -n), y = n)) +
  geom_bar(stat = "identity", fill = "royalblue2") +
  labs(title = "Number of alerts, by product line",
       x = "Product line", y = "Alerts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(face = "bold", size = 14)) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            vjust = -0.3, size = 3)

#+ top-lines, include=FALSE
top3 <- count_prod %>% ungroup() %>% slice_max(prop, n = 3)

#' The most represented product lines are
#' **`r top3$prod_line_nm[1]`** (`r scales::percent(top3$prop[1], accuracy = 0.1)`),
#' **`r top3$prod_line_nm[2]`** (`r scales::percent(top3$prop[2], accuracy = 0.1)`) and
#' **`r top3$prod_line_nm[3]`** (`r scales::percent(top3$prop[3], accuracy = 0.1)`).
#' Each product line belongs to a machine family (construction or forestry),
#' stored in `machine_type_l2`.
#'
#' ## `machine_type_l2` vs `prior_service`
#'
#' Do construction and forestry machines carry out their services at the
#' dealer's service network?

gmodels::CrossTable(base_limpia$machine_type_l2, base_limpia$prior_service,
                    prop.t = FALSE, prop.chisq = FALSE,
                    dnn = c("Machine family", "Prior service?"))

#+ service-shares, include=FALSE
tab <- prop.table(table(base_limpia$machine_type_l2, base_limpia$prior_service), 1)
share_no  <- mean(base_limpia$prior_service == "No")
cons_yes  <- tab["CONSTRUCTION", "Yes"]
fore_yes  <- tab["FORESTRY", "Yes"]

#' Overall, **`r scales::percent(share_no, accuracy = 0.1)`** of the machines that
#' reported an alert during 2020 had **no prior service** at the dealer's
#' service network. Construction machines are the most represented family and
#' also the one with the lowest share of prior service
#' (**`r scales::percent(cons_yes, accuracy = 0.1)`** vs
#' **`r scales::percent(fore_yes, accuracy = 0.1)`** for forestry machines).
#'
#' ## `sum_ocr_cnt`: number of fault occurrences
#'
#' `sum_ocr_cnt` is a **cumulative frequency per machine** (it adds up the
#' faults recorded for a machine ID), so summing it directly would distort the
#' real picture. Instead, we build a data frame that counts how many times a
#' fault is registered for each alert level.

frecuencia_fallas <- base_limpia %>%
  group_by(alert_level, tla, prod_line_nm, year_sold) %>%
  summarise(n = n(), .groups = "drop")

#+ failure-frequency, fig.width=10, fig.height=6
frecuencia_fallas %>%
  mutate(alert_level = factor(alert_level, levels = c("INFO", "YELLOW", "RED", "UNKNOWN"))) %>%
  ggplot(aes(x = year_sold, y = n)) +
  geom_col(fill = "royalblue2") +
  labs(title = "Fault frequency by year of sale and alert level",
       x = "Year of sale", y = "Number of fault groups") +
  facet_wrap(~alert_level) +
  theme_minimal()

#' Machines sold between **2017 and 2020** show a higher number of alerts.
#' Talking to the technical teams, this makes sense: newer machines carry more
#' sensors and technology that can issue informative or preventive notices
#' frequently. Most of the volume concentrates in **YELLOW** and **INFO**
#' alerts.
#'
#' ## Answers to the initial questions
#'
#' * **Can commercial management be driven by the data?**
#'   Yes. A large group of machines does not get serviced at the dealer's
#'   network, which is an opportunity for the sales team to focus on
#'   **construction machinery**.
#' * **Can telemetry be used for after-sales management?**
#'   Yes. Telemetry provides the exact location of each machine, so
#'   commercial proposals can be assigned to the **nearest service centre**.
#'
#' > Next step (not covered in this repository): a supervised classification
#' > model to predict the alert level (e.g. `RED`/`YELLOW` vs `INFO`), using the
#' > clean dataset produced here.

#+ save
saveRDS(base_limpia, "data/processed/base_final.rds")
