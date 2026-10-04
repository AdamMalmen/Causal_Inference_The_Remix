
# --- Dependencies setup: ----

dependencies <- function() {
  libs <- c("tidyverse", "fixest", "haven", "cli")
  pacman_check <- FALSE
  libs_check <- FALSE

  pb_dp <- cli::cli_progress_bar("Setup", total = 2, clear = FALSE)

  cli::cli_h1("Dependencies:")
  cli::cli_h3(
    "dependencies() installs and loads packages that the analysis is dependent upon."
  )

  # Check for pacman:
  proceed <- FALSE
  if (!require("pacman", quietly = TRUE)) {
    ans_1 <- utils::menu(
      title = "pacman not installed. Install pacman",
      choices = c("Yes", "No")
    )
    if (ans_1 == 1) {
      proceed <- TRUE
    }
    if (proceed) {
      install.packages("pacman", quiet = TRUE)
      cli::cli_alert_success("{.pkg pacman} installed.")
      pacman_check <- TRUE
    } else {
      cli::cli_abort("Aborted. {.pkg pacman} must be installed.")
    }
  } else {
    cli::cli_alert_info("{.pkg pacman} already installed.")
    pacman_check <- TRUE
    proceed <- TRUE
  }

  if (pacman_check) {
    cli::cli_alert_success(
      "{.pkg pacman} installed and ready.
                                           Installation proces complete."
    )
  }
  Sys.sleep(1)
  cli::cli_progress_update(id = pb_dp, set = 1, status = "pacman installed")

  # Libs:
  proceed_libs <- FALSE
  if (proceed && pacman_check) {
    ans_2 <- utils::menu(
      title = "Pacman is ready. Load libraries?",
      choices = c("Yes", "No")
    )
    if (ans_2 == 1) {
      proceed_libs <- TRUE
    }
    if (proceed_libs) {
      cli::cli_alert_info("Libraries loading: {length(libs)} packages")
      pacman::p_load(char = libs, character.only = TRUE)
      libs_check <- TRUE
    } else {
      cli::cli_abort("Aborted. {.pkg libs} must be loaded to proceed")
    }
  }
  Sys.sleep(1)
  cli::cli_progress_update(id = pb_dp, set = 2, status = "libraries loaded")

  # End:
  cli::cli_rule()
  cli::cli_alert_success("Packages loaded:")
  cli::cli_ul(libs)
  cli::cli_h1("Ready for analysis")
  cli::cli_progress_done(id = pb_id)
}
dependencies()

# --- Dependencies setup: ----

# ---- good_doctor.do replication ---- #
# Author: Adam Malmén
# Description: simulation of a perfect doctor assigning the treatment
# Updated: 2026-10-03
# ------------------------------------ #


# --- 0.1 Data frame ---

set.seed(20200403)

df <- tibble(
  person = seq_len(100000),
  y_0 = rnorm(100000, mean = 9.4, sd = 4),
  y_1 = rnorm(100000, mean = 10, sd = 4)
) |> 
  mutate(
    y_0 = if_else(y_0 < 0, 0, y_0),
    y_1 = if_else(y_1 < 0, 0, y_1),
    delta = y_1 - y_0,                                      # beräknas EFTER trunkering
    vents = if_else(delta > 0, 1, 0),
    ate = mean(delta),
    att = mean(if_else(vents == 1, delta, NA_real_), na.rm = TRUE),   # filtrerat medelvärde
    atu = mean(if_else(vents == 0, delta, NA_real_), na.rm = TRUE),   # filtrerat medelvärde
    y = vents * y_1 + (1 - vents) * y_0,
    ey_01 = mean(if_else(vents == 1, y_0, NA_real_), na.rm = TRUE),
    ey_00 = mean(if_else(vents == 0, y_0, NA_real_), na.rm = TRUE),
    selection_bias = ey_01 - ey_00,
    pi = mean(vents),
    sdo = ate + selection_bias + (1 - pi) * (att - atu)
  )

# --- 0.2 Regression and check SDO ---
reg_bd <- feols(y ~ vents, data = df, vcov = "HC1")
etable(reg_bd)
df |> distinct(sdo) |> pull() |> round(digits = 4)

# Count the biases:
ate_val <- df |> distinct(ate) |> pull() |> round(digits = 4)
att_val <- df |> distinct(att) |> pull() |> round(digits = 4)
atu_val <- df |> distinct(atu) |> pull() |> round(digits = 4)
sb_val <- df |> distinct(selection_bias) |> pull() |> round(digits = 4)
pi_val <- df |> distinct(pi) |> pull() |> round(digits = 4)
vents <- reg_bd$coefficients["vents"] |> round(digits = 4)

sdo_val <- (ate_val + sb_val + (1 - pi_val) * (att_val - atu_val)) |> round(digits = 4)

# Comments: 
# In the perfect doctor example we have that the ate = 0.5853, att = 4.7225, atu = -4.282 and pi = 0.54.
# We note that selection_bias = -4.4968, i.e., negative selection appears, as is casual in this setup due to
# non-randomization. 

# The Simple difference in mean outcomes = 0.2261. Note that this is very close to the coef on vents.
# However, this is not a causal parameter in this case - since the SDO is drenched in bias, particularly
# a selection bias that stems from what Cunningham (2026) refers to as "sorting on treatment gains".


# ----------------------------------------------------------------


# ---- bad_doctor.do replication ---- #
# Author: Adam Malmén
# Description: simulation of a bad doctor assigning the treatment
# Updated: 2026-10-03
# ------------------------------------ #

# --- 1.0 Data frame ---

set.seed(20200403)

df <- tibble(
 person = seq(1:100000),
 y_0 = rnorm(seq_along(person), 9.4, 4),
 y_1 = rnorm(seq_along(person), 10, 4)
) |> 
  mutate(
    y_0 = if_else(y_0 < 0, 0, y_0),
    y_1 = if_else(y_1 < 0, 0, y_1),
    delta = y_1 - y_0,                                      # beräknas EFTER trunkering
    vents = if_else(person <= 50000, 1, 0),
    ate = mean(delta),
    att = mean(if_else(vents == 1, delta, NA_real_), na.rm = TRUE),   # filtrerat medelvärde
    atu = mean(if_else(vents == 0, delta, NA_real_), na.rm = TRUE),   # filtrerat medelvärde
    y = vents * y_1 + (1 - vents) * y_0,
    ey_01 = mean(if_else(vents == 1, y_0, NA_real_), na.rm = TRUE),
    ey_00 = mean(if_else(vents == 0, y_0, NA_real_), na.rm = TRUE),
    selection_bias = ey_01 - ey_00,
    pi = mean(vents),
    sdo = ate + selection_bias + (1 - pi) * (att - atu)
  )

# --- 1.1 Regression and check SDO ---
reg_gd <- feols(y ~ vents, data = df, vcov = "HC1")
etable(reg_gd)

# Count the biases:
ate_val <- df |> distinct(ate) |> pull() |> round(digits = 4)
att_val <- df |> distinct(att) |> pull() |> round(digits = 4)
atu_val <- df |> distinct(atu) |> pull() |> round(digits = 4)
sb_val <- df |> distinct(selection_bias) |> pull() |> round(digits = 4)
pi_val <- df |> distinct(pi) |> pull() |> round(digits = 4)
vents <- reg_gd$coefficients["vents"] |> round(digits = 4)

sdo_val <- (ate_val + sb_val + (1 - pi_val) * (att_val - atu_val)) |>
  round(digits = 4)

# Comments:
# In the bad doctor example we have a completly differenct situation! Note that ate ≈ att ≈ atu ≈ sdo, while
# the selection_bias = 0.0337 (small!). But perticularly, note that sdo = vents! In this case, we have that - due to
# not perfect, but indicative randomization - sdo is a causal parameter which is also unbiased. Since treatment
# status is basically de-coupled from treatment gains, we can interpret treatment status as randomized, even tough
# there is some sorting into treatment based on the time of the visit. But the important thing is that treatment
# status is independent of potential outcomes!

# One important thing to note though is that in the real world we never observe potential outcomes! These
# are always unobservable, excepts for these kinds of fictional cases where we simulate and create the data.
# In the real world we only have realized, or, actual outcomes and treatment status. We never observe potential
# outcomes.

# In the bad doctor example above we basically reach a state of the world where treatment status is independent
# of potential outcomes, which in fact is the CIA - Conditional Independence Assumption - which states that
# treatment status, or assignment to treatment, is independent of potential outcomes, which then basically gives that
# treatment status is "as good as random".

# ----------------------------------------------------------- #

# ---- giffen.do Replication ---- #
# Author: Adam Malmén
# Description: replicating tables and regressions from their aer on giffen behavior
# Update: 2026-10-04
# ------------------------------- #

data <- list(
  giffen_dat = "https://github.com/scunning1975/mixtape/raw/master/Giffen.dta"
)
dat <- map(data, read_dta)
list2env(dat, envir = .GlobalEnv)

# Jensen and Miller 2008, portions of Table 3, Hunan regression, rice.

# Filter to get a subset of the data:
dat_hunan <- giffen_dat |>
  filter(province == "Hunan", person_id == min_p)

# Use fixest to replicate statas areg command:

# Formula:
fml <- pct_ch_hh_rice ~ pct_ch_sub_rice_arc + pct_ch_hh_pay +
  pct_ch_hh_nonwage + pct_ch_hh_people | county_time

# Subsets: Check conditions
samples <- list(
  "(1): Full sample"           = expr(TRUE),
  "(2): Calorie share <= 0.8"  = expr(hh_staple_calorie_share_1 <= 0.8),
  "(3): Calorie share > 0.8"   = expr(hh_staple_calorie_share_1 > 0.8),
  "(4): Calorie share 0.6-0.8" = expr(hh_staple_calorie_share_1 >= 0.6 &
                                      hh_staple_calorie_share_1 <= 0.8)
)

# Iterate:
models <- map(samples, \(cond) {
  feols(
    fml,
    data = dat_hunan |> filter(!!cond),
    vcov = ~hhid,
    ssc = ssc(adj = TRUE, fixef.K = "full", cluster.adj = TRUE)
  )
}, .progress = TRUE)

etable(models, se.below = TRUE) 
