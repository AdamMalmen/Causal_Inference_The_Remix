# =========================================================== #
# File:   Chapter_5_Unconfoundness.R
# Author: Adam Malmén
# Date:   2026-11-01
#
# Info:
# This script is a replication of the .py scritp in the folder
# It produces the code to replicate (to some degree) the results
# in Cunningham (2026).
#
# =========================================================== #

# ---- 0.0 Setup: -----
dependencies <- function() {
  # Packages:
  libs <- c("tidyverse", "haven", "cli", "rlang", "labelled", "fixest")

  # Checks & Balances:
  pac_check <- FALSE
  pac_install <- FALSE
  load_libs <- FALSE
  theme_check <- FALSE

  # Progress-bar:
  pb_id <- cli::cli_progress_bar(
    name = "dependencies",
    total = 3,
    clear = FALSE
  )

  # Initial message:
  cli::cli_h1("Dependencies")
  cli::cli_text(
    "{.pkg dependencies} checks for pacman and set up the environment for analysis."
  )

  # Pacman:
  if (!pac_check && !pac_install) {
    if (!require("pacman", quietly = TRUE)) {
      ans_1 <- utils::menu(
        title = "pacman not installed. Install?",
        choices = c("Yes", "No")
      )
      if (ans_1 == 1) {
        install.packages("pacman", quiet = TRUE)
        pac_check <- TRUE
        pac_install <- TRUE
      } else {
        cli::cli_abort("Aborted. ---- {.pkg pacman} must be installed.")
      }
    } else {
      cli::cli_alert_info("{.pkg pacman} already installed.")
      pac_check <- TRUE
      pac_install <- TRUE
    }
  }

  # Update progress bar
  if (pac_check && pac_install) {
    Sys.sleep(1)
    cli::cli_progress_update(id = pb_id, set = 1, status = "pacman checked.")
  }

  # Packages:
  if (pac_check && pac_install && !load_libs) {
    cli::cli_text("pacman is checked and ready. Load packages?")
    cli::cli_ul(libs)
    ans_2 <- utils::menu(choices = c("Yes", "No"))
    if (ans_2 == 1) {
      pacman::p_load(char = libs)
      load_libs <- TRUE
      cli::cli_alert_success("{length(libs)} packages loaded:")
      cli::cli_ul(libs)
    } else {
      cli::cli_abort("Aborted. ---- Packages in libs must be loaded.")
    }
  }

  # Update progress bar:
  if (pac_check && pac_install && load_libs) {
    Sys.sleep(1)
    cli::cli_progress_update(id = pb_id, set = 2, status = "Packages loaded")
  }

  # Set theme:
  if (!theme_check) {
    cli::cli_alert_info("Theme will be set to {.pkg theme_classic}")
    theme_set(theme_classic())
    theme_check <- TRUE
  }

  # Ending:
  if (pac_check && pac_install && load_libs && theme_check) {
    cli::cli_rule()
    cli::cli_h1("Setup complete. Ready for analysis")
    cli::cli_progress_done(id = pb_id)
  }
}
dependencies()

# ----------------------
