#' ---
#' title: "00 - Synthetic data generator"
#' ---
#'
#' # Synthetic telemetry data generator
#'
#' The original project used proprietary telemetry and sales records from a
#' heavy-equipment dealer. Those files contain real equipment serial numbers
#' (VIN/PIN) and customer-level information, so **they are not published**.
#'
#' This script builds a fully **synthetic** replacement that keeps the original
#' *structure and data-quality problems* (same columns, same kinds of missing
#' values, the same sales-year pattern, skewed engine hours, heavy-tailed fault
#' counts, etc.). Nothing here is a real record: serial numbers, fault codes,
#' dates and counters are all randomly generated with `set.seed(2026)`.
#'
#' Two tables are produced in `data/synthetic/`:
#'
#' * `telemetry_alerts.csv`: one row per alert emitted by a machine in 2020.
#' * `equipment_master.csv`: descriptive data of the machines (sales date,
#'   machine type, last service, ...).

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(readr)
})

set.seed(2026)

#' ## 1. Design parameters

# Target number of alert rows per sales year (NA = machine not found in the
# equipment master, "2021" = machines sold after the data window).
rows_by_year <- c(
  "2011" = 587,  "2012" = 1050, "2013" = 569,  "2014" = 692,  "2015" = 3178,
  "2016" = 1983, "2017" = 5830, "2018" = 7094, "2019" = 6338, "2020" = 2987,
  "2021" = 122,  "NA" = 2164
)
stopifnot(sum(rows_by_year) == 32594)

# Engine hours at the time of the first fault, by sales year (mean, sd).
# Older machines have accumulated more hours.
hours_params <- tribble(
  ~year,  ~mean,  ~sd,
  "2011", 10100, 2700, "2012", 9700, 2600, "2013", 10600, 2900,
  "2014", 9800,  2550, "2015", 11500, 3500, "2016", 9400, 3150,
  "2017", 5750,  2600, "2018", 3850, 2000, "2019", 2050, 1380,
  "2020", 540,   640,  "2021", 400,  300,  "NA",   9000, 3500
)

# Missing values injected by sales year (quality problems to be found later).
na_ocr_by_year <- c(
  "2011" = 18, "2012" = 16, "2013" = 19, "2014" = 3, "2015" = 134,
  "2016" = 33, "2017" = 81, "2018" = 65, "2019" = 60, "2020" = 40,
  "2021" = 0,  "NA" = 89
)
na_tla_by_year <- c(
  "2011" = 13, "2012" = 74, "2013" = 9, "2014" = 10, "2015" = 33,
  "2016" = 7,  "2017" = 13, "2018" = 30, "2019" = 37, "2020" = 0,
  "2021" = 0,  "NA" = 19
)
n_na_mfg <- 234

# Product lines: share of alerts, machine family and machine type.
product_lines <- tribble(
  ~prod_line_nm,             ~share, ~family,        ~machine_type,        ~pin_prefix, ~model_code,
  "MOTOR GRADERS",           0.226,  "CONSTRUCTION", "MOTOR GRADER",       "1DX",       "MG",
  "WHEEL LOADERS",           0.200,  "CONSTRUCTION", "WHEEL LOADER",       "1DX",       "WL",
  "BACKHOE LOADERS",         0.175,  "CONSTRUCTION", "BACKHOE LOADER",     "1AX",       "BL",
  "EXCAVATORS (AMERICAS)",   0.067,  "CONSTRUCTION", "EXCAVATOR",          "1BX",       "EX",
  "CRAWLER DOZERS",          0.027,  "CONSTRUCTION", "BULLDOZER",          "1CX",       "CD",
  "ADT",                     0.003,  "CONSTRUCTION", "ARTICULATED TRUCK",  "1CX",       "AD",
  "WHEELED HARVESTERS",      0.069,  "FORESTRY",     "HARVESTER",          "1EX",       "WH",
  "SWING MACHINES",          0.058,  "FORESTRY",     "SWING MACHINE",      "1FX",       "SM",
  "FORWARDERS",              0.055,  "FORESTRY",     "FORWARDER",          "1EX",       "FW",
  "SKIDDERS",                0.049,  "FORESTRY",     "SKIDDER",            "1FX",       "SK",
  "TRACKED HARVESTERS",      0.039,  "FORESTRY",     "HARVESTER",          "1EX",       "TH",
  "TRACKED FELLER BUNCHERS", 0.023,  "FORESTRY",     "FELLER BUNCHER",     "1FX",       "FB",
  "KNUCKLEBOOM LOADERS",     0.011,  "FORESTRY",     "KNUCKLEBOOM",        "HCM",       "KB"
) %>% mutate(share = share / sum(share))

#' ## 2. Fault-code catalogue (synthetic)
#'
#' A pool of ~2,800 fictitious diagnostic trouble codes. Each code has a
#' three-letter component group (`tla`), a numeric code, a failure mode and
#' a severity level.

tla_pool <- c(
  "ECU","PDU","ICZ","ACL","ACR","ADU","ALC","ALS","BCU","BRK","CAB","CAN",
  "CLT","CMP","DEF","DPF","EGR","EHC","ENG","FAN","FLT","FUE","GEN","HYD",
  "IMP","INJ","JCU","LGT","LUB","MCU","MTG","OIL","PTO","PMP","RAD","SCR",
  "SEN","SSV","STR","SWG","TCU","TRN","TRB","TUR","VCU","VPC","VSS","WAB",
  "WAC","WHL","XHA","XJC","YAW","ZCM","CNT","DSP"
)
stopifnot(length(tla_pool) == 56)

phrases <- c(
  "Sensor voltage above normal", "Sensor voltage below normal",
  "Data erratic, intermittent or incorrect", "Circuit open or shorted",
  "Pressure below normal operating range", "Temperature above normal",
  "Component not responding", "Calibration required",
  "Communication lost with module", "Value out of range",
  "Actuator stuck or not moving", "Restart engine and contact service",
  "Preventive service due", "Filter restriction detected",
  "Low fluid level", "Fault active - monitor operation"
)

# Some component groups are more associated with critical faults. This weight
# is the "tla" signal used later by the model (synthetic assumption).
high_risk_tla <- c("ENG", "HYD", "BRK", "TRN", "PMP", "TCU", "ECU", "PDU", "CAN", "SCR")
tla_weights <- function(level) {
  w <- ifelse(tla_pool %in% high_risk_tla,
              switch(level, RED = 5, YELLOW = 1.5, INFO = 0.5, 1), 1)
  w / sum(w)
}

make_dtc_pool <- function(n, level, severity_phrase = NULL) {
  tibble(
    level = level,
    tla   = sample(tla_pool, n, replace = TRUE, prob = tla_weights(level)),
    spn   = sample(100:9999, n, replace = TRUE),
    fmi   = sample(0:31, n, replace = TRUE),
    phrase = sample(phrases, n, replace = TRUE)
  ) %>%
    distinct(level, tla, spn, fmi, .keep_all = TRUE) %>%
    mutate(
      tla_spn_fmi    = sprintf("%s-%d.%d", tla, spn, fmi),
      alert_defn_dsc = sprintf("%s   %s %06d.%02d    %s",
                               level, tla, spn, fmi, phrase)
    )
}

dtc_pool <- bind_rows(
  make_dtc_pool(1100, "YELLOW"),
  make_dtc_pool(1000, "INFO"),
  make_dtc_pool(450,  "RED"),
  make_dtc_pool(250,  "UNKNOWN")
)

# "Invalid DTC Name" entries are the origin of the missing `tla` values.
invalid_dtc <- tibble(
  level = "UNKNOWN", tla = NA_character_, spn = NA_integer_, fmi = NA_integer_,
  phrase = NA_character_, tla_spn_fmi = "Invalid-DTC-Name",
  alert_defn_dsc = "Invalid DTC Name"
)
test_dtc <- invalid_dtc %>% mutate(alert_defn_dsc = "test")

#' ## 3. Machines (units)
#'
#' Rows per machine follow a heavy-tailed distribution (a few machines raise
#' many alerts).

n_units_total <- 1385
units_per_year <- pmax(round(rows_by_year / sum(rows_by_year) * n_units_total), 8)
units_per_year["NA"] <- 96

units <- bind_rows(lapply(names(rows_by_year), function(y) {
  tibble(sales_year = y, n_units = units_per_year[[y]])
})) %>%
  tidyr::uncount(n_units) %>%
  mutate(unit_id = row_number())

# product line per unit
units <- units %>%
  mutate(prod_idx = sample(nrow(product_lines), n(), replace = TRUE,
                           prob = product_lines$share),
         prod_line_nm = product_lines$prod_line_nm[prod_idx],
         family       = product_lines$family[prod_idx],
         machine_type = product_lines$machine_type[prod_idx],
         pin_prefix   = product_lines$pin_prefix[prod_idx],
         model_code   = product_lines$model_code[prod_idx])

# model name (synthetic) and VIN-like identifiers
rand_chars <- function(n, pool, len) {
  vapply(seq_len(n), function(i) paste(sample(pool, len, TRUE), collapse = ""),
         character(1))
}
units <- units %>%
  mutate(
    decal_model_nm = paste0(model_code, sample(c(210, 250, 310, 330, 410, 520, 650, 700), n(), TRUE),
                            sample(c("", "L", "K", "G"), n(), TRUE)),
    serial   = sprintf("%06d", sample(1:999999, n(), replace = FALSE)),
    native_pin = paste0(pin_prefix, rand_chars(n(), 0:9, 3),
                        rand_chars(n(), LETTERS, 2), rand_chars(n(), LETTERS, 3),
                        serial),
    pin = paste0(substr(native_pin, 2, 8), substr(native_pin, 12, 17))
  )
stopifnot(all(nchar(units$native_pin) == 17), !anyDuplicated(units$native_pin))

# distribute alert rows among units (every unit has >= 1 alert)
alloc_rows <- function(n_rows, n_units) {
  w <- rlnorm(n_units, 0, 1.2)
  extra <- n_rows - n_units
  1 + as.vector(rmultinom(1, extra, w / sum(w)))
}
units <- units %>%
  group_by(sales_year) %>%
  mutate(n_alerts = alloc_rows(rows_by_year[[first(sales_year)]], n())) %>%
  ungroup()
stopifnot(sum(units$n_alerts) == 32594)

# unit-level attributes that will drive alert severity (see section 4):
# * prior service at the dealer network (2019-2020) is much more frequent in
#   forestry machines
# * a latent "unit risk" captures everything the data does not observe
units <- units %>%
  mutate(
    had_service = runif(n()) < ifelse(family == "FORESTRY", 0.55, 0.16),
    unit_risk   = rnorm(n(), 0, 0.45)
  )

#' ## 4. Alert rows

alerts <- units[rep(seq_len(nrow(units)), units$n_alerts), ] %>%
  mutate(row_id = row_number())

# engine hours at first fault: gamma by sales year (matched mean/sd) with a
# few extreme high values and some near-zero values (data-quality problems)
gamma_hours <- function(n, m, s) rgamma(n, shape = (m / s)^2, rate = m / s^2)
alerts <- alerts %>%
  left_join(hours_params, by = c("sales_year" = "year")) %>%
  mutate(first_dtc_engn_hours = round(gamma_hours(n(), mean, sd), 2))

idx_extreme <- sample(seq_len(nrow(alerts)), 330)
alerts$first_dtc_engn_hours[idx_extreme] <-
  round(alerts$first_dtc_engn_hours[idx_extreme] * runif(length(idx_extreme), 2.5, 6), 2)
idx_low <- sample(which(alerts$sales_year %in% c("2018", "2019", "2020")), 190)
alerts$first_dtc_engn_hours[idx_low] <- round(runif(length(idx_low), 0, 4.9), 2)

#' ### Alert level (the modelling target)
#'
#' **Important:** to make the later modelling exercise meaningful, the probability
#' that an alert is `RED` (critical) is generated from a logistic model on
#' observable features. This is a deliberate, documented assumption, **not a
#' finding about real machines**:
#'
#' * more engine hours at failure -> higher risk (log scale),
#' * product-line effect (e.g. harvesters and graders above backhoe loaders),
#' * machines with prior service at the dealer network -> lower risk,
#' * the component group (`tla`) of the fault (see the fault catalogue weights),
#' * plus unit-level random noise that no variable observes.
#'
#' Overall ~11% of the alerts are `RED`, as in the original data. Non-critical
#' alerts are split between `INFO`, `YELLOW` and `UNKNOWN`.

prod_effect <- c(
  "MOTOR GRADERS" = 0.25, "WHEEL LOADERS" = 0.15, "BACKHOE LOADERS" = -0.20,
  "EXCAVATORS (AMERICAS)" = 0.20, "CRAWLER DOZERS" = 0.10, "ADT" = 0.30,
  "WHEELED HARVESTERS" = 0.50, "SWING MACHINES" = 0.00, "FORWARDERS" = 0.30,
  "SKIDDERS" = 0.20, "TRACKED HARVESTERS" = 0.55, "TRACKED FELLER BUNCHERS" = 0.35,
  "KNUCKLEBOOM LOADERS" = 0.10
)
z_hours <- as.numeric(scale(log1p(alerts$first_dtc_engn_hours)))
lin_red <- 0.65 * z_hours +
  unname(prod_effect[alerts$prod_line_nm]) +
  ifelse(alerts$had_service, -0.55, 0) +
  alerts$unit_risk
b0 <- uniroot(function(b) mean(plogis(b + lin_red)) - 0.111, c(-8, 2))$root
alerts$is_red <- runif(nrow(alerts)) < plogis(b0 + lin_red)

non_red_probs <- c(INFO = 0.381, YELLOW = 0.475, UNKNOWN = 0.033)
non_red_probs <- non_red_probs / sum(non_red_probs)
alerts$`Alert Level` <- ifelse(
  alerts$is_red, "RED",
  sample(names(non_red_probs), nrow(alerts), TRUE, non_red_probs)
)

# alert definition (fault code) according to level
pick_dtc <- function(level) {
  pool <- dtc_pool %>% filter(.data$level == !!level)
  pool[sample(nrow(pool), 1), ]
}
dtc_idx <- vapply(alerts$`Alert Level`, function(l) {
  which(dtc_pool$level == l)[sample(sum(dtc_pool$level == l), 1)]
}, integer(1))
alerts <- bind_cols(alerts, dtc_pool[dtc_idx, c("tla", "tla_spn_fmi", "alert_defn_dsc")])

# missing `tla`: they come from "Invalid DTC Name" / "test" UNKNOWN alerts
make_tla_na <- function(df) {
  for (y in names(na_tla_by_year)) {
    k <- na_tla_by_year[[y]]
    if (k == 0) next
    cand <- which(df$sales_year == y)
    pick <- sample(cand, k)
    n_test <- if (y == "2012") 7 else 0
    df$`Alert Level`[pick]   <- "UNKNOWN"
    df$tla[pick]             <- NA_character_
    df$tla_spn_fmi[pick]     <- "Invalid-DTC-Name"
    df$alert_defn_dsc[pick]  <- "Invalid DTC Name"
    if (n_test > 0) df$alert_defn_dsc[pick[seq_len(n_test)]] <- "test"
  }
  df
}
alerts <- make_tla_na(alerts)

# timestamps (all alerts happen in 2020)
alerts <- alerts %>%
  mutate(
    first_cptr_tm  = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") +
      runif(n(), 0, 366 * 24 * 3600 - 1),
    start_of_month = floor_date(as.Date(first_cptr_tm), "month")
  )

# failure-count (heavy tail) and missing values
alerts <- alerts %>%
  mutate(sum_ocr_cnt = pmin(pmax(round(exp(rnorm(n(), 1.15, 2.0))), 1), 17690))
for (y in names(na_ocr_by_year)) {
  k <- na_ocr_by_year[[y]]
  if (k > 0) alerts$sum_ocr_cnt[sample(which(alerts$sales_year == y), k)] <- NA
}

#' ## 5. Equipment master (descriptive table)

# invoice (sales) date, per unit
unit_dates <- units %>%
  mutate(
    invoice_date = case_when(
      sales_year == "NA"   ~ as.Date(NA),
      TRUE ~ as.Date(paste0(sales_year, "-01-01")) + sample(0:364, n(), TRUE)
    )
  )

# previous service in 2019-2020 is much more frequent in forestry machines
unit_dates <- unit_dates %>%
  mutate(
    last_service_date = case_when(
      had_service ~ as.Date("2019-01-01") + sample(0:729, n(), TRUE),
      runif(n()) < 0.55 ~ as.Date("2015-01-01") + sample(0:1460, n(), TRUE),
      TRUE ~ as.Date(NA)
    ),
    hour_meter = round(runif(n(), 5, 9000)),
    odometer   = ifelse(family == "CONSTRUCTION" & runif(n()) < 0.3,
                        round(runif(n(), 100, 80000)), NA)
  )

alerts <- alerts %>%
  left_join(select(unit_dates, unit_id, invoice_date, last_service_date,
                   hour_meter, odometer, machine_type, family),
            by = "unit_id")

# telemetry activation date (`mfg_dt`)
alerts <- alerts %>%
  mutate(mfg_dt = case_when(
    is.na(invoice_date) ~ as.Date("2011-06-14") + sample(0:700, n(), TRUE),
    TRUE ~ pmin(invoice_date + sample(0:60, n(), TRUE), as.Date("2020-10-29"))
  ))
alerts$mfg_dt <- as.POSIXct(alerts$mfg_dt, tz = "UTC")
alerts$mfg_dt[sample(nrow(alerts), n_na_mfg)] <- NA

# remaining attributes
alerts <- alerts %>%
  mutate(
    dealer_name = "SALINAS Y FABRES S.A.",
    dealer_acct_id = 100001,
    tier_level = sample(c("Non-Certified", "Tier 3", "Tier 4 Final", NA), n(), TRUE,
                        c(0.35, 0.25, 0.25, 0.15)),
    last_known_engn_hours = round(first_dtc_engn_hours + runif(n(), 0, 600), 2),
    last_known_lat  = round(runif(n(), -45, -20), 4),
    last_known_long = round(runif(n(), -74, -68), 4)
  )

telemetry_alerts <- alerts %>%
  transmute(
    `Decal Model Nm` = decal_model_nm, `Alert Level`, tla_spn_fmi, alert_defn_dsc,
    dealer_name, `PIN Prefix` = pin_prefix, prod_line_nm, tier_level, tla,
    dealer_acct_id, first_cptr_tm, mfg_dt, native_pin, pin, start_of_month,
    first_dtc_engn_hours, last_known_engn_hours, last_known_lat,
    last_known_long, sum_ocr_cnt
  ) %>%
  slice_sample(prop = 1)   # shuffle rows

equipment_master <- unit_dates %>%
  filter(sales_year != "NA") %>%
  transmute(
    vin = native_pin, invoice_date = invoice_date,
    brand = "OEM A", model = paste("OEM A", decal_model_nm),
    machine_type_l2 = family, machine_type_l3 = machine_type,
    last_service_date = last_service_date, hour_meter = hour_meter,
    odometer = odometer
  )

# machines that never raised an alert (completes the equipment master)
n_extra <- 4131 - nrow(equipment_master)
extra <- tibble(
  prod_idx = sample(nrow(product_lines), n_extra, TRUE, product_lines$share)
) %>%
  mutate(
    family = product_lines$family[prod_idx],
    machine_type = product_lines$machine_type[prod_idx],
    pin_prefix = product_lines$pin_prefix[prod_idx],
    model_code = product_lines$model_code[prod_idx],
    vin = paste0(pin_prefix, rand_chars(n(), 0:9, 3),
                 rand_chars(n(), LETTERS, 2), rand_chars(n(), LETTERS, 3),
                 sprintf("%06d", sample(1:999999, n(), TRUE))),
    invoice_date = as.Date(paste0(sample(2011:2020, n(), TRUE), "-01-01")) + sample(0:364, n(), TRUE),
    brand = "OEM A",
    model = paste0("OEM A ", model_code, sample(c(210, 310, 410, 650), n(), TRUE)),
    machine_type_l2 = family, machine_type_l3 = machine_type,
    last_service_date = as.Date(ifelse(runif(n()) < 0.4, as.Date("2016-01-01") + sample(0:1700, n(), TRUE), NA)),
    hour_meter = round(runif(n(), 5, 9000)), odometer = NA_real_
  ) %>% select(names(equipment_master))
equipment_master <- bind_rows(equipment_master, extra) %>%
  mutate(n = 1) %>%
  slice_sample(prop = 1)

#' ## 6. Save

dir.create("data/synthetic", recursive = TRUE, showWarnings = FALSE)
write_csv(telemetry_alerts, "data/synthetic/telemetry_alerts.csv", na = "")
write_csv(equipment_master, "data/synthetic/equipment_master.csv", na = "")

cat("telemetry_alerts:", nrow(telemetry_alerts), "rows x", ncol(telemetry_alerts), "cols\n")
cat("equipment_master:", nrow(equipment_master), "rows x", ncol(equipment_master), "cols\n")
