## Steps to release to CRAN

```
library(devtools)
devtools::document()

devtools::check()

devtools::check_win_release()
devtools::check_win_devel()

install.packages("rhub")
library(rhub)
check_rhub(platforms = c("ubuntu-gcc-release", "macos-highsierra-release-cran", "macos-highsierra-release"))

install.packages("spelling")
library(spelling)
spell_check()

devtools::install_github('r-lib/revdepcheck')
library(revdepcheck)
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers = 4)

use_cran_comments()

devtools::release()
```
