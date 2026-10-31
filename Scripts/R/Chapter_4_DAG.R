library(tidyverse)

set.seed(3444)
star <- tibble(
  talent = rnorm(2500),
  beauty = rnorm(2500),
  score  = talent + beauty,
  c85    = quantile(score, 0.85),
  star   = if_else(score >= c85, 1, 0)
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
    print(fit)
    fit
    cli::cli_h3("\n Plot --- Data: {df_name} \n")
  })
  
  p <- ggplot(df, aes(x = talent, y = beauty)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    labs(title = df_name) +
    theme_classic()
  print(p)
  
  list(fits = fits, plot = p)
})

  
  

