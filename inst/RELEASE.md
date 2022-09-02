## Steps to release to CRAN

```
library(devtools)
devtools::document()

devtools::check()

devtools::check_win_release()
devtools::check_win_devel()

install.packages("rhub")

check_rhub(platforms = c("ubuntu-gcc-release", "macos-m1-bigsur-release", "macos-highsierra-release"))

install.packages("spelling")
spell_check()

devtools::install_github('r-lib/revdepcheck')
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers = 4)

devtools::release()
```