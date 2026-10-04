
import pandas as pd
import polars as pl
import numpy as np
import statsmodels.formula.api as smf
import pyfixest as pf


# ---- good_doctor.do replication ---- #
# Author: Adam Malmén
# Description: simulation of a perfect doctor assigning the treatment
# Updated: 2026-10-03
# ------------------------------------ #

# 1. Polars version: -------------------------------

rng = np.random.default_rng(20200403)
n = 100_000

df = (
    pl.DataFrame({
        "person": np.arange(1, n + 1),
        "y_0": rng.normal(9.4, 4, n),
        "y_1": rng.normal(10, 4, n),
    })
    # trunkera vid 0
    .with_columns(
        pl.col("y_0").clip(lower_bound=0),
        pl.col("y_1").clip(lower_bound=0),
    )
    # delta beräknas EFTER trunkering
    .with_columns(delta=pl.col("y_1") - pl.col("y_0"))
    .with_columns(vents=(pl.col("delta") > 0).cast(pl.Int8))
    # aggregat (skalärer som broadcastas till alla rader)
    .with_columns(
        ate=pl.col("delta").mean(),
        att=pl.col("delta").filter(pl.col("vents") == 1).mean(),
        atu=pl.col("delta").filter(pl.col("vents") == 0).mean(),
        y=pl.when(pl.col("vents") == 1).then(pl.col("y_1")).otherwise(pl.col("y_0")),
        ey_01=pl.col("y_0").filter(pl.col("vents") == 1).mean(),
        ey_00=pl.col("y_0").filter(pl.col("vents") == 0).mean(),
        pi=pl.col("vents").mean(),
    )
    # nya kolumner som bygger på de ovan måste ligga i en separat with_columns
    .with_columns(selection_bias=pl.col("ey_01") - pl.col("ey_00"))
    .with_columns(
        sdo=pl.col("ate")
        + pl.col("selection_bias")
        + (1 - pl.col("pi")) * (pl.col("att") - pl.col("atu"))
    )
)

# --- 0.2 Regression och kontroll av SDO ---
reg_df = pd.DataFrame({
    "y": df["y"].to_numpy(),
    "vents": df["vents"].to_numpy(),
})

reg_bd = smf.ols("y ~ vents", data=df.select("y", "vents").to_pandas()).fit(cov_type="HC1")
print(reg_bd.summary())

print(round(df["sdo"][0], 4))   # motsvarar distinct(sdo) |> pull()

# Räkna ut biaserna:
ate_val = round(df["ate"][0], 4)
att_val = round(df["att"][0], 4)
atu_val = round(df["atu"][0], 4)
sb_val  = round(df["selection_bias"][0], 4)
pi_val  = round(df["pi"][0], 4)
vents   = round(reg_bd.params["vents"], 4)

sdo_val = round(ate_val + sb_val + (1 - pi_val) * (att_val - atu_val), 4)

