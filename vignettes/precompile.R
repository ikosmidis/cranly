library("knitr")

knitr::knit("vignettes/extractors.Rmd.orig",
            output = "vignettes/extractors.Rmd")


files <- dir(".", pattern = "precomp-*")
file.copy(files, "vignettes/", overwrite = TRUE)
file.remove(files)
