This package is complimentary to a report which analyses data from
simulated guppy experiments, collected via the software Simbio.

Install and load data
=====================

    > devtools::install_github("rozeykex/guppies")
    > library(guppies)
    > data(guppies)

Model simplification
====================

The analysis files are stored in `/analysis` (these are ignored from the
package build). There are various model simplifications carried out in
`/analysis/predator_density.R` and `/analysis/substrate.R` in
particular.

Package functions
=================

Man pages included for everything except `smooth_predation` which wasn't
used in the final analysis.

-   predict\_density()
-   predict\_substrate()
-   rgba()
-   roses\_unicode()
-   roses\_set\_ggtheme()
-   smooth\_predation()
