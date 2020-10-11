# options(BioC_mirror="https://bioconductor.org/")
# source("https://bioconductor.org/biocLite.R")


packages.in.CRAN <- c(
  "readr",
  "readxl",
  "magrittr",
  "plyr",
  "dplyr",
  "tibble"
  )

# packages.in.Bioc <- c(
#   "HMP16SData",
#   "curatedMetagenomicData"
# )


for(i in 1:length(packages.in.CRAN)){
  if (!requireNamespace(packages.in.CRAN[i], quietly = TRUE)) install.packages(packages.in.CRAN[i])
  eval(
    parse(text=sprintf("require(%s)", packages.in.CRAN[i]))
  )
}

# for(i in 1:length(packages.in.Bioc)){
#   if (!requireNamespace(packages.in.Bioc[i], quietly = TRUE)) BiocManager::install(packages.in.Bioc[i])
#   eval(
#     parse(text=sprintf("require(%s)", packages.in.Bioc[i]))
#   )
# }


if(!require(ExploratoryDataAnalysis)){
  devtools::install_github("mrmtshmp/ExploratoryDataAnalysis")
}


# if(Bibtex){
#   write(toBibtex(citation()),file="CRAN")
#   for(i in 1:length(packages)){
#     write(toBibtex(citation(packages[i])),file=sprintf("../Biblio/%s%s.bib",packages[i],"_CRAN"))
#   }
# }
# 
