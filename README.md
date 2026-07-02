# irtempirical

An R package for **empirical (nonparametric) Item Response Theory** analysis.

Instead of fitting a parametric IRT model (1PL/2PL/3PL, GRM, etc.) and
assuming a specific mathematical form for the item response function,
`irtempirical` estimates each item's characteristic curve **directly from
the data**: respondents are grouped by their observed total (sum) score,
and the probability of endorsing each response option is simply the
proportion of respondents in that score group who did. This is the same
idea behind classic nonparametric IRT tools (e.g. Ramsay's TestGraf /
kernel-smoothed ICCs), traded here for a very lightweight,
dependency-free implementation based on binning + optional logistic
smoothing.

It works for both dichotomous (0/1) and polytomous (ordinal, e.g. Likert)
items.

## Installation

```r
install.packages("devtools")
library(devtools)
install_github("maisk/irtempirical")
```

## Quick start

```r
library(irtempirical)

sim_dichotomous <- data.frame(psych::sim.irt(
  nvar = 10, n = 1000, low = -4, high = 4,
  a = NULL, c = 0, z = 1, d = NULL, mu = 0, sd = 1,
  mod = "logistic"
)$items)

sim_polytomous <- data.frame(psych::sim.poly(
  nvar = 10, n = 1000, low = -4, high = 4,
  a = NULL, c = 0, z = 1, d = NULL, mu = 0, sd = 1,
  cat = 5, mod = "logistic"
)$items)

m1 <- irt_empirical_model(data = sim_dichotomous, items = NULL)
m2 <- irt_empirical_model(data = sim_polytomous, items = NULL)

plot(m1, type = "ICC", xlim = c(-6, 6), item = "V2",
     draw.logit_method = 0, add_score_axis = TRUE)

plot(m2, type = "ICC", xlim = c(-6, 6), item = "V2",
     draw.logit_method = 0, add_score_axis = TRUE)
```

## How it works

Given a `data.frame` of item responses (one column per item, one row per
respondent):

1. **Total score & z-score axis.** The row sum score is computed for
   every respondent (`sum_scores`). Its mean/SD are used to convert every
   *possible* integer score in the observed range into a z-score — this
   z-score axis (`scores_axis` / `z_scores_axis`) stands in for the
   latent trait (theta) on the x-axis of every plot.

2. **Per-item, per-option response vectors.** For each item and each
   response option `i` (1..max), three indicator vectors are built:
   - `responses1`: 1 for respondents who chose exactly option `i`
   - `responses2`: 1 for respondents who chose *more than* `i`
   - `responses3`: 1 for respondents who chose *less than* `i`

   Each option gets an `EmpiricalIRT` object and also records its
   `position` on the option scale (`1` = first/left, `2` = middle,
   `3` = last/right).

3. **Empirical probabilities.** Respondents are grouped by their exact
   total score. Within each score group, the proportion who satisfy each
   of the three response vectors above becomes one point of the
   empirical curve (`y1`, `y2`, `y3` — one probability per possible total
   score). `y1` is the empirical item/option characteristic curve;
   `y2`/`y3` describe how much probability mass sits above/below that
   option (used for polytomous items and for smoothing thresholds).
   Point-wise Bernoulli "information" (`p * (1 - p)`) is also stored per
   score group.

4. **Optional logistic smoothing (`addlogit = TRUE`).** A quasibinomial
   GLM (`y ~ x`) is fit through each of `y1`, `y2`, `y3` against the
   z-score axis, giving smoothed curves `logit1`, `logit2`, `logit3` that
   can be overlaid on the raw empirical points.

5. **Monotonicity.** A single monotonicity index per option is computed
   as the average absolute Spearman correlation between the z-score axis
   and `y2`/`y3` (falling back to the correlation with `y1` when one of
   those is degenerate, e.g. all zero). Well-behaved items should show
   monotonically increasing/decreasing option curves along the trait
   axis; this index is a quick numeric check of that.

The result is an `EmpiricalIRTModel` object: it holds the original data,
the score/z-score axes, and a named list (`irts`) mapping each item name
to the list of its per-option `EmpiricalIRT` results.

## API

### `irt_empirical_model(data, items = NULL, addlogit = TRUE)`

Computes the empirical IRT model described above.

| Argument | Meaning |
| --- | --- |
| `data` | `data.frame` of item responses, one column per item |
| `items` | character vector of column names to include; `NULL` = all columns |
| `addlogit` | whether to fit the quasibinomial logistic smoothing curves |

Returns an `EmpiricalIRTModel` (S4).

### `plot(x, type = "ICC", item, option = NULL, ...)`

S4 `plot` method for `EmpiricalIRTModel`. Draws the empirical
item/option characteristic curve(s) for one item.

| Argument | Meaning |
| --- | --- |
| `x` | an `EmpiricalIRTModel` |
| `item` | item (column) name to plot — required |
| `option` | restrict the plot to a single response option; `NULL` plots all options for the item, each in its own color |
| `xlim`, `ylim` | axis limits; default `ylim = c(0, 1.1)` |
| `draw.points` | plot the raw empirical probability points |
| `draw.lines` | connect the raw empirical points |
| `draw.logit` | overlay the smoothed logistic curve |
| `draw.logit_method` | which smoothed curve to draw: `0` = the "exactly this option" curve derived from `1 - logit2 - logit3`, `1`/`2`/`3` = `logit1`/`logit2`/`logit3` directly |
| `add_score_axis` | add a secondary axis showing raw total scores under the z-score axis |
| `main` | optional title prefix |

When `option` is given, the monotonicity index for that option is
printed in the plot's top-right margin, along with the sample size.

### S4 classes

- **`EmpiricalIRT`** — one response option of one item: its empirical
  curve points (`x`, `y1`, `y2`, `y3`), fitted logit models (`logit1/2/3`),
  `information`, and `monotonicity`.
- **`EmpiricalIRTModel`** — the whole fitted model: original `data`,
  score/z-score axes, `items`, and `irts` (item name → list of
  `EmpiricalIRT`).

## Package layout

```text
DESCRIPTION           # package metadata (title/description currently blank)
NAMESPACE             # exports: irt_empirical_model, EmpiricalIRT, EmpiricalIRTModel, plot method
R/irtempirical.R      # all source: S4 classes, plot method, irt_empirical_model()
man/                  # Rd help files generated by roxygen2 from R/irtempirical.R
docs/                 # pkgdown-generated static site (do not hand-edit; regenerate instead)
generate_package.R    # developer script: document(), install(), pkgdown::build_site(), R CMD build/check
```

## Development

`generate_package.R` is the maintainer workflow script (run from RStudio,
not part of the package itself):

```r
library(devtools)
library(roxygen2)
document()                          # regenerate NAMESPACE/man from roxygen comments
install()
pkgdown::build_site(getwd())        # regenerate docs/
system("R CMD Rd2pdf irtempirical --force")
system("R CMD build irtempirical --resave-data")
```

## Status / known gaps

- `DESCRIPTION`'s `Title` and `Description` fields are empty.
- The `plot()` method has a dead/commented-out `type == "INF"` branch for
  plotting item information curves — not currently reachable.
- No automated tests.
