#' ---
#' title: "03 - Outlier Management"
#' output: github_document
#' ---
#'
#' # 3. Outlier Management
#'
#' Input: `data/processed/base_limpia_pre_outliers.rds` (output of notebook 02).

#+ libraries, message=FALSE, warning=FALSE
library(tidyverse)

base_limpia <- readRDS("data/processed/base_limpia_pre_outliers.rds")
n_before <- nrow(base_limpia)

#' ## Visualising outliers
#'
#' A boxplot of `year_sold` (year of sale) against `first_dtc_engn_hours`
#' (engine hours at the time of the failure), both relevant to the problem of
#' identifying what helps predict failures. Points beyond the whiskers are
#' candidate outliers.

#+ boxplot-before
plot_hours_by_year <- function(df, title) {
  df %>%
    ggplot(aes(x = year_sold, y = first_dtc_engn_hours, fill = year_sold)) +
    geom_boxplot(outlier.size = 0.8) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = title, x = "Year of sale", y = "Hours")
}
plot_hours_by_year(base_limpia, "Engine hours at failure by year of sale (before outlier treatment)")

#' ## Detection rules
#'
#' **High outliers.** For each sales year, a record is considered a high
#' outlier if it exceeds
#'
#' *High threshold = Q1 + 3 x IQR*
#'
#' where Q1 is the first quartile (25% of the data) and IQR the interquartile
#' range (Q3 - Q1).
#'
#' **Low outliers.** By business rule, any record with fewer than **5 engine
#' hours** at the time of the failure:
#'
#' *Low outlier = `first_dtc_engn_hours` < 5*
#'
#' First we store the unique values of `year_sold`:

years <- unique(base_limpia$year_sold)

#' Then, for each year we compute the threshold (`thresholds`) and the
#' row positions that exceed it (`pos_high`):

thresholds <- list()
pos_high   <- list()

for (y in years) {
  hours_y <- base_limpia %>% filter(year_sold == y) %>% pull(first_dtc_engn_hours)
  thr <- quantile(hours_y, prob = 0.25) + IQR(hours_y) * 3
  thresholds[[as.character(y)]] <- thr
  pos_high[[as.character(y)]] <-
    which(base_limpia$year_sold == y & base_limpia$first_dtc_engn_hours > thr)
}

#' High outliers per year:

count_high <- vapply(pos_high, length, numeric(1))
count_high

total_high <- sum(count_high)
total_high

#' There are **`r total_high` high outliers** in the `r format(n_before, big.mark = ",")`
#' observations of the clean dataset. Now the low outliers:

pos_low   <- which(base_limpia$first_dtc_engn_hours < 5)
total_low <- length(pos_low)
total_low

total_high + total_low

#' In total **`r total_high + total_low` outlier records** were identified:
#' **`r total_high` high** and **`r total_low` low**. The decision is to remove
#' them so they do not distort the analysis.

pos_all <- unique(c(unlist(pos_high), pos_low))
base_limpia <- base_limpia %>% slice(-pos_all)

#' After handling missing values and outliers the dataset has
#' **`r format(nrow(base_limpia), big.mark = ",")` rows and `r ncol(base_limpia)` columns**.
#'
#' The boxplot after the treatment:

#+ boxplot-after
plot_hours_by_year(base_limpia, "Engine hours at failure by year of sale (after outlier treatment)")

#' Overall, the dispersion decreases for machines sold from 2017 onwards, which
#' makes sense: older machines have accumulated more working hours before their
#' first recorded fault, and newer machines have had less time in operation.

#+ save
saveRDS(base_limpia, "data/processed/base_limpia.rds")
