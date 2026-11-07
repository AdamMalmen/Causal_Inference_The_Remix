# =========================================================== #
# File:   Chapter_4_DAG.R
# Author: Adam Malmén
# Date:   2026-10-31
#
# Info:
# This script is a replication of the .py scritp in the folder
# It produces the code to replicate (to some degree) the results
# in Cunningham (2026).
#
# =========================================================== #

# ---- 0.0 Setup: ----
dependencies <- function() {
  # Libs:
  libs <- c("tidyverse", "fixest", "cli", "rlang", "haven")

  # Checks and balances:
  pac_check <- FALSE
  pac_install <- FALSE
  libs_load <- FALSE

  # Progress bar:
  pb_id <- cli::cli_progress_bar(
    name = "Dependencies",
    total = 2,
    clear = FALSE
  )

  # Messages:
  cli::cli_h1("Dependencies:")
  cli::cli_alert_info(
    "{.pkg dependencies()} installs and loads packages that this program is dependent upon"
  )

  # Check for pacman and install if not installed:
  if (!pac_check) {
    if (!require("pacman", quietly = TRUE)) {
      ans_1 <- utils::menu(
        title = "Pacman not installed. Install?",
        choices = c("Yes", "No")
      )
      if (ans_1 == 1) {
        install.packages("pacman", quiet = TRUE)
        pac_check <- TRUE
        pac_install <- TRUE
        cli::cli_alert_success("{.pkg pacman} installed.")
      } else {
        cli::cli_abort("Aborted. --- {.pkg pacman} must be installed.")
      }
    } else {
      pac_check <- TRUE
      pac_install <- TRUE
      cli::cli_alert_success("{.pkg pacman} already installed.")
    }
  }

  if (pac_check && pac_install && !libs_load) {
    Sys.sleep(1)
    cli::cli_progress_update(id = pb_id, set = 1, status = "pacman installed")
  }

  # Load packages:
  if (pac_check && pac_install && !libs_load) {
    cli::cli_alert_info("pacman is installed and ready. Load packages?")
    cli::cli_ul(libs)
    ans_2 <- utils::menu(choices = c("Yes", "No"))
    if (ans_2 == 1) {
      pacman::p_load(char = libs, character.only = TRUE)
      libs_load <- TRUE
      cli::cli_alert_success("{length(libs)} packages loaded:")
      cli::cli_ul(libs)
    } else {
      cli::cli_abort("Aborted. --- {length(libs)} packages must be loaded.")
    }
  }

  if (pac_check && pac_install && libs_load) {
    Sys.sleep(1)
    cli::cli_progress_update(id = pb_id, set = 2, status = "packages loaded")
  }

  # End:
  cli::cli_rule()
  cli::cli_h1("Ready for analysis")
  cli::cli_progress_done(id = pb_id)
}
dependencies()

# --------------------------------------- #

# ---- 1.1 Colider bias: ----
set.seed(3444)
star <- tibble(
  talent = rnorm(2500),
  beauty = rnorm(2500),
  score = talent + beauty,
  c85 = quantile(score, 0.85),
  star = if_else(score >= c85, 1, 0)
)

# Iterate:
datasets <- list(
  d1 = star,
  d2 = star |> filter(star == 1),
  d3 = star |> filter(star == 0)
)
rhs <- c(m1 = "talent", m2 = "talent + score")
models <- map(rhs, \(x) reformulate(x, response = "beauty"))

res <- imap(datasets, \(df, df_name) {
  cli::cli_h1("{df_name}")

  fits <- imap(models, \(f, model) {
    cli::cli_h2("{model}")
    fit <- fixest::feols(f, data = df)
    print(fixest::etable(fit, se.below = TRUE, ci = 0.95))
    fit
    cli::cli_text("\n")
  })

  p <- ggplot(df, aes(x = talent, y = beauty)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    labs(title = df_name) +
    theme_classic()
  cli::cli_h3("\n Plot --- Data: {df_name} \n")
  print(p)
  ggsave(
    plot = p,
    filename = glue::glue("Plots/Chapter_4_{df_name}.pdf"),
    device = pdf,
    create.dir = TRUE
  )

  list(fits = fits, plot = p)
})

# --------------------------------------- #

# ---- 1.2: Discrimination & Colider Bias ----

# DGP
set.seed(123)
n <- 10000
df <- tibble(
  female = if_else(rnorm(n) >= 0.5, 1, 0),
  ability = rnorm(n),
  discrimination = female,
  occupation = 1 + 2 * ability + 0 * female + (-2) * discrimination + rnorm(n),
  wage = 1 - discrimination + occupation + 2 * ability + rnorm(n)
)

# Run regressions:
rhs <- list(
  Unconditional_Biased = "female",
  Conditional_Biased = "female + occupation",
  Conditional_Unbiased = "female + occupation + ability"
)
models <- map(rhs, \(f) reformulate(f, response = "wage"))
results <- imap(models, \(m, model) {
  r <- fixest::feols(m, data = df)
  cli::cli_h1("{model}")
  fixest::etable(r, se.below = TRUE) |> print()
  return(r)
})
results
