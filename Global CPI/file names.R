library(tidyverse)

company <- "Lamons"
computer <- "~/"
#computer <- "C:/Users/Brent/"
side <- "Clients/"
#side <- "DCI/Clients/"

file <- paste0(computer, "Dropbox/", side, company, "/CPI/")
file.o <- paste0(file, "OUTPUT/")
file.i <- paste0(file, "input/")
file.i.raw <- paste0(file.i, "raw/")
file.i.prep <- paste0(file.i, "prep/")
file.i.final <- paste0(file.i, "final/")

file.o.plots <- paste0(file.o, "plots/")
file.o.plots.all <- paste0(file.o.plots, "all/")
file.o.plots.demo <- paste0(file.o.plots, "demo/")
file.o.analyses <- paste0(file.o, "analyses - nps calculations/")
file.o.nps <- paste0(file.o.analyses, "nps scores/")
file.o.ph.plots <- paste0(file.o.analyses, "plots/")
file.o.stats <- paste0(file.o.analyses, "stats/")
file.o.means <- paste0(file.o.analyses, "means/")

file.scripts <- paste0(file, "scripts-materials/scripts/")
file.scripts.prep <- paste0(file.scripts, "data prep/")
file.scripts.plots <- paste0(file.scripts, "plots/")
file.scripts.analyses <- paste0(file.scripts, "analyses/")