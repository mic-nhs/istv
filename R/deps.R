#
# @import hunspell
# @import phonics
# @import sf
# @import tidytext
# @import dplyr
# @import stringr
# @import fs
# @import purrr
# @import readr
# @import tibble
# @import tidyr
# @import usethis
# @import word2vec
# @import workflows
# @import brulee
# @import parsnip
# @import runner

usethis::use_package("hunspell", type = "Imports")
usethis::use_package("phonics", type = "Imports")
usethis::use_package("sf", type = "Imports")
usethis::use_package("tidytext", type = "Imports")
usethis::use_package("dplyr", type = "Imports")
usethis::use_package("fs", type = "Imports")
usethis::use_package("tidyr", type = "Imports")
usethis::use_package("readr", type = "Imports")
usethis::use_package("usethis", type = "Imports")
usethis::use_package("word2vec", type = "Imports")
usethis::use_package("utils", type = "Imports")
usethis::use_package("hardhat", type = "Imports")
usethis::use_package("brulee", type = "Imports")
usethis::use_package("runner", type = "Imports")

usethis::use_gpl_license()
usethis::use_pipe()
usethis::use_build_ignore(c("dev/.*", "data-raw/.*"))

#usethis::use_author(given = "Michael", family = "Cheetham", email = "michael.cheetham@nhs.net")
