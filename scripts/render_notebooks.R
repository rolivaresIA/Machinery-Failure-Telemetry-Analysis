# Renders the analysis scripts (01-04) into Markdown notebooks.
# Usage (from the repository root):
#   Rscript scripts/render_notebooks.R
# Requires: tidyverse, janitor, skimr, lubridate, naniar, Hmisc, psych, gmodels, knitr

notebooks <- c(
  "scripts/01_data_understanding.R",
  "scripts/02_data_quality.R",
  "scripts/03_outlier_management.R",
  "scripts/04_descriptive_statistics.R",
  "scripts/05_modeling.R"
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) notebooks <- args   # optional: render only the given scripts

if (!file.exists("data/synthetic/telemetry_alerts.csv")) {
  source("scripts/00_generate_synthetic_data.R")
}

knitr::opts_knit$set(root.dir = getwd())  # run chunks from the repository root
options(cli.unicode = FALSE, width = 110)

for (f in notebooks) {
  out <- paste0(sub("^scripts/", "", tools::file_path_sans_ext(f)), ".md")
  knitr::opts_chunk$set(
    fig.path = paste0("figures/", tools::file_path_sans_ext(out), "/"),
    dev = "png", dpi = 110, comment = "##", error = FALSE, fig.width = 8, fig.height = 4.5
  )
  rmd <- knitr::spin(f, knit = FALSE)
  knitr::knit(rmd, output = out, quiet = TRUE)
  file.remove(rmd)
  message("Rendered ", out)
}
