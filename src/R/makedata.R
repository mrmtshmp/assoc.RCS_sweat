#' Make analysis data from Summer/Winter 2018 data.
#' PI: Dr Miwa Ashida
#' 2020/10/11

source('./src/R/sub/require_libraries.makedata.R')

dir.data <- '../data'

dir.origData.QSART      <- "../../mail/2007/data/QSART"
dir.origData.other_clin <- "../../mail/2007/data/other_clin"
dir.origData.QSART_HC <- "../../mail/2002/data/QSART"

fn.patient_data.QSART <- list.files(path = c(dir.origData.QSART))
fn.patient_data.other_clin <- list.files(path = c(dir.origData.other_clin))
fn.control_data.QSART <- list.files(path = c(dir.origData.QSART_HC))

# fn.data.res_QSART  <- "res_2004_all.xlsx"
# fn.data.demo       <- "demo_2004_all.xlsx"



# Data loading ------------------------------------------------------------

# Patient's data

for(i in 1:length(fn.patient_data.QSART)){
  col_info.patient_data.QSART <-
    readxl::read_excel(
      sprintf(
        "%s/%s",
        dir.origData.QSART, fn.patient_data.QSART[i]
      ),
      sheet = 'col_info',
      skip = 0,
      col_types = NULL
      )
  
  for(exl.sheet in 1:2){
    raw.df.res_2004_tmp <- read_excel(
      sprintf(
        "%s/%s",
        dir.origData.QSART, fn.patient_data.QSART[i]
      ),
      sheet = exl.sheet,
      skip = 2,
      col_names = col_info.patient_data.QSART$col_names,
      col_types = col_info.patient_data.QSART$col_types
    ) %>%
      mutate(
        exl.sheet = exl.sheet 
        )
    if(exl.sheet==1) raw.df.res_2004 <- raw.df.res_2004_tmp
    if(exl.sheet==2) raw.df.res_2004 <- dplyr::bind_rows(raw.df.res_2004,raw.df.res_2004_tmp)
    }
  }
  
for(i in 1:length(fn.patient_data.other_clin)){
  col_info.patient_data.other_clin <-
    readxl::read_excel(
      sprintf(
        "%s/%s",
        dir.origData.other_clin, fn.patient_data.other_clin[i]
      ),
      sheet = 'col_info',
      skip = 0,
      col_types = NULL
    )
  
  raw.df.demo <- read_excel(
      sprintf(
        "%s/%s",
        dir.origData.other_clin, fn.patient_data.other_clin[i]
        ),
      sheet = 1,
      skip = 2,
      col_names = col_info.patient_data.other_clin$col_names,
      col_types = col_info.patient_data.other_clin$col_types
    ) %>%
      mutate(
        exl.sheet = exl.sheet 
      )
  }

#' Healthy control

for(i in 1:length(fn.control_data.QSART)){
  col_info.control_data.QSART <-
    readxl::read_excel(
      sprintf(
        "%s/%s",
        dir.origData.QSART_HC, fn.control_data.QSART[i]
      ),
      sheet = 'col_info',
      skip = 0,
      col_types = NULL
    )
  
    df.HC <- read_excel(
      sprintf(
        "%s/%s",
        dir.origData.QSART_HC, fn.control_data.QSART[i]
      ),
      sheet = 1,
      skip = 0,
      col_names = col_info.control_data.QSART$col_names,
      col_types = col_info.control_data.QSART$col_types
      )
    }

# Data  -------------------------------------------------------------------

df.res_2004 <- 
  raw.df.res_2004 %>%
    mutate(
      sweat.time = ifelse(sweat.time=="∞",300, as.numeric(sweat.time,na.rm=TRUE)),
      sweat.amount =  ifelse(sweat.time=="∞",0, as.numeric(sweat.amount,na.rm=TRUE)),
      sweat.event = ifelse(sweat.time=="∞",0, 1)
      ) %>%
  data.frame() %>%
  mutate(
    season = ifelse(exl.sheet==1, "Summer", "Winter")
    )

df.demo <- 
  raw.df.demo %>%
  dplyr::filter(!is.na(Sex)) %>%
  sapply(
    function(vec){
      vec[ is.na(vec)]    <- 0
      vec[ vec == "N.E."] <- NA
      return(vec)
    }
  ) %>%
  data.frame() %>%
  mutate(
    id = as.character(id),
    Sex= factor(Sex, levels = c(1,0), labels = c('F','M')))



# Output ------------------------------------------------------------------

save.image(file = sprintf("%s/%s", dir.data, "ADS_2004.v01.RData"))



