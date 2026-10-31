
# ================================ #
# Dependencies - a leaner approach
# Author: Claude Code
# Date:   2026-10-31
#
# ================================ #


dependencies <- function(
  libs = c("tidyverse", "fixest", "cli", "rlang", "haven"),
  interactive_prompt = interactive()
) {
  cli::cli_h1("Dependencies")

  # Install pacman if needed
  if (!requireNamespace("pacman", quietly = TRUE)) {
    if (interactive_prompt) {
      ok <- utils::menu(
        c("Yes", "No"),
        title = "pacman not installed. Install?"
      ) ==
        1
      if (!ok) cli::cli_abort("{.pkg pacman} must be installed.")
    }
    install.packages("pacman", quiet = TRUE)
    cli::cli_alert_success("{.pkg pacman} installed.")
  } else {
    cli::cli_alert_success("{.pkg pacman} already installed.")
  }

  # Install any missing packages and attach everything
  pacman::p_load(char = libs)
  cli::cli_alert_success("{length(libs)} package{?s} loaded: {.pkg {libs}}")

  cli::cli_rule()
  cli::cli_h1("Ready for analysis")
  invisible(libs)
}
dependencies()