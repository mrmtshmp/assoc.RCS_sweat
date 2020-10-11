#' Latency time of sweating analysis.
#' 

Bibtex <- TRUE
source('./src/R/sub/require_libraries.main.R')

sink('session_info.txt')
sessionInfo()
sink()

dir.data   <- "../data"
dir.output <- "./src/output/"

fn.RData  <- "ADS_2004.v01.RData"

# Data loading ------------------------------------------------------------

#' Note:
#' 
#'  In the data.frame object 'tidy_...ADS', subjects with
#'  multiple disease or positivities in sABs have duplicated
#'  rows. These duplications are indicated by the variable 'comm'.
#'

load(
  sprintf("%s/%s", dir.data, fn.RData)
)

ADS <- df.demo %>%
  left_join(
    df.res_2004,
    by="id"
  ) %>%
  dplyr::filter(season %in% c("Summer", "Winter")) %>%
  mutate(
    season = factor(season,levels = c("Summer","Winter")),
    sweat.amount_log = log(sweat.amount, exp(1)),
    sweat.time_log = log(sweat.time, exp(1)),
    Disease.all = 1,
    fct.RS.total = factor(RS.total),
    fct.RS.attack = factor(RS.attack),
    fct.RS.pain = factor(RS.pain),
    fct.RS.color = factor(RS.color),
    fct.RS.duration = factor(RS.duration),
    bin.RS.total = factor(ifelse(RS.total<=7, "0-7", "> 7"), levels = c("0-7","> 7"),labels = c("low","high")),
    bin.RS.attack = factor(ifelse(RS.attack==0, "0", "> 0"), levels = c("0","> 0"),labels = c("low","high")),
    bin.RS.pain = factor(ifelse(RS.pain==0, "0", "> 0"), levels = c("0","> 0"),labels = c("low","high")),
    bin.RS.color = factor(ifelse(RS.color==0, "0", "> 0"), levels = c("0","> 0"),labels = c("low","high")),
    bin.RS.duration = factor(ifelse(RS.duration==0, "0", "> 0"), levels = c("0","> 0"),labels = c("low","high"))
  ) %>%
  filter(!is.na(fct.RS.total)) %>%
  filter(!is.na(season)) %>%
  dplyr::arrange(id, exl.sheet)

# ADS.RS <- ADS[ ,  grep( "[RS\\.(.+)|^id]", colnames(ADS))]
# 
# ADS.Dis <- ADS[ ,  grep( "Disease\\.(.+)", colnames(ADS))]

colnames.Diseases <- colnames(ADS[grep( "Disease\\.(.+)", colnames(ADS))])
colnames.sAB  <- colnames(ADS[grep( "sAB\\.(.+)", colnames(ADS))])
colnames.Comp <- colnames(ADS[grep( "Comp\\.(.+)", colnames(ADS))])


for(i in 1:length(colnames.Diseases)){
  tmp.tidy_Disease.ADS <- ADS %>%
    filter(
      eval(parse(
        text=sprintf('%s==1',colnames.Diseases[i]))
        )
      ) %>%
    mutate(ind.Disease=colnames.Diseases[i])
  if(i==1){tidy_Disease.ADS <- tmp.tidy_Disease.ADS}else{
    tidy_Disease.ADS <- 
      rbind(
        tidy_Disease.ADS, tmp.tidy_Disease.ADS
        )
  }
}

for(i in 1:length(colnames.sAB)){
  if(
    nrow(
      ADS %>%
      filter(
        eval(parse(
          text=sprintf('%s==1',colnames.sAB[i]))
          )
        )
      )
  ){
    tmp.tidy_sAB.ADS <- ADS %>%
      filter(
        eval(parse(
          text=sprintf('%s==1',colnames.sAB[i]))
        )
      ) %>%
      mutate(ind.sAB=colnames.sAB[i])
    if(!("tidy_sAB.ADS" %in% ls())){tidy_sAB.ADS <- tmp.tidy_sAB.ADS}else{
      tidy_sAB.ADS <- 
        rbind(
          tidy_sAB.ADS, tmp.tidy_sAB.ADS
        )
      }    
    }
  }

for(i in 1:length(colnames.Comp)){
  if(
    nrow(
      ADS %>%
      filter(
        eval(parse(
          text=sprintf('%s==1',colnames.Comp[i]))
        )
      )
    )
  ){
    tmp.tidy_Comp.ADS <- ADS %>%
      filter(
        eval(parse(
          text=sprintf('%s==1',colnames.Comp[i]))
        )
      ) %>%
      mutate(ind.Comp=colnames.Comp[i])
    if(!("tidy_Comp.ADS" %in% ls())){tidy_Comp.ADS <- tmp.tidy_Comp.ADS}else{
      tidy_Comp.ADS <- 
        rbind(
          tidy_Comp.ADS, tmp.tidy_Comp.ADS
        )
    }
    }
  }

tidy_Disease.ADS <- 
  tidy_Disease.ADS %>% 
  mutate(
    comm = as.numeric((duplicated(id)+duplicated(id,fromLast = TRUE)) >1) # All subjects duplicated because of the 'Disease.all' column. 
    ) 

tidy_sAB.ADS <- 
  tidy_sAB.ADS %>% 
  mutate(
    comm = as.numeric((duplicated(id)+duplicated(id,fromLast = TRUE)) >0)
  ) 

tidy_Comp.ADS <- 
  tidy_Comp.ADS %>% 
  mutate(
    comm = as.numeric((duplicated(id)+duplicated(id,fromLast = TRUE)) >0)
  ) 

# Kaplan-Meiyer plot for latent time ------------------------------------------------------

ADS.surv.SpAB <- 
  ADS %>%
  dplyr::select(
    sweat.time, season,
    starts_with('sAB.'),
    starts_with('bin.RS.')
    ) %>%
  dplyr::filter(!is.na(sweat.time))

df.ADS.surv.SpAB <- ADS.surv.SpAB %>%
  gather(
    factor, val, -sweat.time, -season
    ) %>%
  mutate(
    event = ifelse(sweat.time==300,.0,1)
  ) %>%
  dplyr::filter(!(factor%in%c('sAB.other'))) %>%
  dplyr::filter(!is.na(val))

ggsurv.obj.strat <-
  df.ADS.surv.SpAB %>%
  dlply(
    .(season, factor),
    function(D){

      if(length(unique(D$val))>1){

        if(length(grep('^sAB',unique(D$factor)))>0){
          D$val=factor(D$val, levels = c(1,0), labels = c('Positive','Negative'))
          name.factor <- 
            gsub(
              'sAB\\.(.+)',
              '\\1',unique(D$factor)
            )
          .legend.labs = c('Positive','Negative')
          .linetype = c('solid', 'dashed')
          
        }else{
          if(unique(D$factor)=='bin.RS.total'){
            D$val=factor(D$val, levels = c('low','high'), labels = c('0-7','> 7'))
          }else{
            D$val=factor(D$val, levels = c('low','high'), labels = c('0','> 0'))
            }
          name.factor <- 
            gsub(
              'bin.RS.\\.(.+)',
              '\\1',unique(D$factor)
            )
          .legend.labs = unique(D$val)
          .linetype = c('solid', 'solid')
          }

        surv.obj <- 
          surv_fit(
            Surv(sweat.time, event) ~ val,
            D
          )
        
        ggsurv <- 
          ggsurvplot(
            surv.obj,
            risk.table = TRUE,
            risk.table.title="",
            legend.title= "",
            legend.labs = .legend.labs,
            legend = c(.7,.2),
            linetype = .linetype,
            palette = 'npg',
            fun = 'event'
          )

        gg.plot <- ggsurv$plot +
          theme_bw() + 
          theme(
            axis.text = element_text(colour = 'black', family = 'Arial',size = 16),
            text = element_text(colour = 'black', family = 'Arial',size = 16)
            ) +
          labs(title=unique(D$season), subtitle = name.factor)
        gg.table <- ggsurv$table
        
        result.plot <-
          ggarrange(
            gg.plot,
            gg.table,
            nrow=2,
            heights = c(2,1/2)
          )
        
        list.result.plot <- list(result.plot)
        
        
        # Facet
        return(list(list.result.plot, surv.obj))
      }
    }
  )


quartz(file = sprintf('%s/%s',dir.output,'cumev.sweat.time_sAB.pdf'),type = 'pdf',family = 'Arial')
llply(ggsurv.obj.strat,function(L)L[[1]])
dev.off()

write.csv(
  x = ldply(ggsurv.obj.strat,function(L)unlist(summary(L[[2]])$table)),
  file = sprintf('%s/%s',dir.output,'cumev.sweat.time_sAB.csv')
  )

