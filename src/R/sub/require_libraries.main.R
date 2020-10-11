# options(BioC_mirror="https://bioconductor.org/")
# source("https://bioconductor.org/biocLite.R")

Bibtex <- TRUE

packages.in.CRAN <- c(
  'readxl',
  'magrittr',
  'plyr',
  'dplyr',
  'tidyr',
  'tibble',
  'ggplot2',
  'ggbeeswarm',
  'robustbase',
  'vegan',
  'ggsci'
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


if(Bibtex){
  write(toBibtex(citation()),file="CRAN")
  for(i in 1:length(packages.in.CRAN)){
    write(toBibtex(citation(packages.in.CRAN[i])),file=sprintf("./src/biblio/%s%s.bib",packages.in.CRAN[i],"_CRAN"))
  }
}

