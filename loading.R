
packages <- c("shiny",
              "shinyFiles",
              "shinythemes", 
              "shinyWidgets",
              "tidyverse", "magrittr","DT", 
              "RColorBrewer", "circlize", "ComplexHeatmap", "scales",
              "fgsea",
              "xmcutil",
              "here")

lapply(packages, require, character.only = TRUE)