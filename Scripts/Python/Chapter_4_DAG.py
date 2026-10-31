import polars as pl
import pandas as pd
import numpy as np
import pyfixest as pf
import plotnine as pn

# =========================================================== #
# File:   Chapter_4_DAG.py 
# Author: Adam Malmén
# Date:   2026-10-31
#
# Info:
# This script is a replication of the .R scritp in the folder
# It produces the code to replicate (to some degree) the results
# in Cunningham (2026).
#
# =========================================================== #


# ---- 1.1 Collider bias ----
rng = np.random.default_rng(3444)
n = 2500

star = pl.DataFrame({
    "talent": rng.normal(size=n),
    "beauty": rng.normal(size=n),
}).with_columns(
    score=pl.col("talent") + pl.col("beauty")
)

c85 = star["score"].quantile(0.85, interpolation="linear")
star = star.with_columns(
    star=(pl.col("score") >= c85).cast(pl.Int8)
)

datasets = {
    "d1": star,
    "d2": star.filter(pl.col("star") == 1),
    "d3": star.filter(pl.col("star") == 0),
}
models = {
    "m1": "beauty ~ talent",
    "m2": "beauty ~ talent + score",
}

res = {}
for df_name, df_pl in datasets.items():
    df = pd.DataFrame(df_pl.to_dict(as_series=False))   # polars -> pandas utan pyarrow
    print(f"\n{'=' * 10} {df_name} {'=' * 10}")

    fits = {}
    for model_name, formula in models.items():
        print(f"\n--- {model_name}: {formula} ---")
        fit = pf.feols(formula, data=df)
        fit.summary()
        fits[model_name] = fit

    p = (
        pn.ggplot(df, pn.aes(x="talent", y="beauty"))
        + pn.geom_point(size=0.5)
        + pn.geom_smooth(method="lm", se=False, color="grey")
        + pn.labs(title=df_name)
        + pn.theme_classic()
    )
    p.show()

    res[df_name] = {"fits": fits, "plot": p}