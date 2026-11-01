# ========================================================================= #
# File:   Data cleaning template
# Author: Adam Malmén
# Date:   2026-11-01
#
# Info:
# Simple script for referencing data cleaning using the usual suspects
# of packages: tidyverse (dplyr, stringr, etc.), janitor and so fourth.
# I take inspiration from Albert Rapp's YT videos. The script is for my sole
# usage.
# ========================================================================= #

# ---- Dependencies: ----
dependencies <- function() {
  libs <- c("tidyverse", "janitor", "readxl", "haven", "writexl")

  check_pac <- FALSE
  install_pac <- FALSE
  load_libs <- FALSE

  # Run program:
  cli::cli_h1("Dependencies:")
  cli::cli_text("{.pkg dependencies()} sets the environment for data cleaning")

  if (!check_pac && !install_pac) {
    if (!require("pacman", quietly = TRUE)) {
      ans_1 <- utils::menu(
        title = "pacman not installed. Install?",
        choices = c("Yes", "No")
      )
      if (ans_1 == 1) {
        install.packages("pacman", quiet = TRUE)
        check_pac <- TRUE
        install_pac <- TRUE
        cli::cli_alert_success("{.pkg pacman} installed.")
      } else {
        cli::cli_abort("Aborted. ---- {.pkg pacman} must be installed")
      }
    } else {
      cli::cli_alert_success("{.pkg pacman} already installed")
      check_pac <- TRUE
      install_pac <- TRUE
    }
  }

  if (check_pac && install_pac && !load_libs) {
    cli::cli_alert_info("{.pkg pacman} ready. Load packages:")
    cli::cli_ul(libs)
    ans_2 <- utils::menu(c("Yes", "No"))
    if (ans_2 == 1) {
      pacman::p_load(char = libs)
      load_libs <- TRUE
      cli::cli_alert_success("{length(libs)} packages loaded")
    } else {
      cli::cli_abort("Aborted. ---- packages in libs must be loaded")
    }
  }

  if (check_pac && install_pac && load_libs) {
    cli::cli_rule()
    cli::cli_h1("Setup completed")
  }
}
dependencies()

# ---- Data: ----
