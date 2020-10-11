#' Protocol Planned Analyses.
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
  filter(!is.na(fct.RS.total))

# ADS.RS <- ADS[ ,  grep( "[RS\\.(.+)|^id]", colnames(ADS))]
# 
# ADS.Dis <- ADS[ ,  grep( "Disease\\.(.+)", colnames(ADS))]

colnames.fct.RS <- colnames(ADS[grep( "fct.RS\\.(.+)", colnames(ADS))])
colnames.sweat  <- colnames(ADS[grep( "sweat\\.(.+)", colnames(ADS))])
colnames.Diseases <-
  colnames(
    ADS[
      grep( "Disease\\.(.+)", colnames(ADS))])
colnames.Diseases <- colnames.Diseases[-which(colnames.Diseases=="Disease.all")]
colnames.sAB  <- colnames(ADS[grep( "sAB\\.(.+)", colnames(ADS))])
colnames.Comp <- colnames(ADS[grep( "Comp\\.(.+)", colnames(ADS))])
colnames.med <- colnames(ADS[grep( "med\\.(.+)", colnames(ADS))])

ADS_comp <- ADS[
  complete.cases(
    ADS[,c(colnames.fct.RS,colnames.sweat,colnames.Diseases,colnames.sAB)]),
  ]

colnames.fct.RS  <- colnames(ADS[grep( "fct.RS\\.(.+)", colnames(ADS))])
colnames.bin.RS  <- colnames(ADS[grep( "bin.RS\\.(.+)", colnames(ADS))])


df.lm_fct.RS.ADS <- 
  gather(
    ADS_comp[,c("id", "season", colnames.fct.RS)], 
    colnames.RS, val, -id, -season
    ) %>%
  mutate(
    val = as.numeric(val)
  ) %>%
  left_join(
    ADS_comp, by=c("id", "season")
    )

df.lm_bin.RS.ADS <- 
  gather(
    ADS_comp[,c("id", "season", colnames.bin.RS)], 
    colnames.RS, val, -id, -season
  ) %>%
  mutate(
    val = factor(val, levels = c("low","high"))
    ) %>%
  left_join(
    ADS, by=c("id", "season")
  )


# Analysis 9.3.1.1 <robust linear regression>---------------------------
#' 
#' Linear regression estimate and output them as csv file.
#' 


list.res.linReg <-
  dlply(
    df.lm_fct.RS.ADS %>%
      mutate(
        type.RS="fct"
        ) %>%
      bind_rows(
        df.lm_bin.RS.ADS %>%
          mutate(
            type.RS="bin",
            val = as.numeric(val)
            )
        ),
    .(type.RS,colnames.RS), # Define "val" as factor object with levels of unique(D$val)[order(unique(D$val))]---------------- 
    function(D){
      D <- D %>% 
        mutate(
          val=factor(
            val,
            levels = unique(D$val)[
              order(unique(D$val))
              ]
            )
          )
      dlply(
        D,
        .(season),  # Robust linear regression via do.call(lmrob)--------------------------------
        #' 
        #' and then extract regression  coefficient and the confidence interbal and the residuals of individual subjects.
        #' 
        function(d){
          if(
            length(
              unique(d$val[!is.na(d$val)])
              ) > 1
            ){
            d <- d %>% column_to_rownames("id")
            for(y.var in c("sweat.amount_log","sweat.time_log")){
              d$val <- as.numeric(as.character(d$val))
              fml <- sprintf("%s ~ val", y.var)
              res.lm <- do.call(
                robustbase::lmrob, 
                list(
                  formula= as.formula(fml), data=d, 
                  control = lmrob.control(fast.s.large.n = Inf,maxit.scale = 500)
                  )
                )
              res.lm.coef.confint_tmp <- try(confint(res.lm) %>% data.frame() %>% rownames_to_column("terms") %>% mutate(y.var=y.var))
              
              if(class(res.lm.coef.confint_tmp)!='try-error'){
                res.lm.coef_tmp    <- 
                  summary(res.lm)$coefficients %>% data.frame() %>% rownames_to_column("terms") %>% mutate(y.var=y.var) %>%
                  left_join(
                    res.lm.coef.confint_tmp
                  )
              }else{
                res.lm.coef_tmp    <- 
                  summary(res.lm)$coefficients %>% data.frame() %>% rownames_to_column("terms") %>% mutate(y.var=y.var)
                }
              
              
              res.lm.fitted  <- res.lm$fitted.values %>% unlist()
              res.lm.resid   <- res.lm$residuals %>%  unlist()
              res.lm.fitted.resid_tmp <- data.frame(
                id=names(res.lm.fitted), y.var = y.var,
                fitted=data.frame(res.lm.fitted),resid=data.frame(res.lm.resid)
                )
              
              res.lm.coef_tmp$var.y <- y.var
              res.lm.fitted.resid_tmp$var.y <- y.var
              
              res.lm.coef_tmp$N <- nrow(res.lm.fitted.resid_tmp)
              
              if(y.var=="sweat.amount_log") res.lm.coef <- res.lm.coef_tmp
              if(y.var=="sweat.time_log")   res.lm.coef <- bind_rows(res.lm.coef, res.lm.coef_tmp)
              if(y.var=="sweat.amount_log") res.lm.fitted.resid <- res.lm.fitted.resid_tmp
              if(y.var=="sweat.time_log")   res.lm.fitted.resid <- bind_rows( res.lm.fitted.resid, res.lm.fitted.resid_tmp)
              }
            return(
              list(res.lm.coef, res.lm.fitted.resid)
              )
            }
          }
        )
      }
    )


require(scales)

df.res.lm.coefficients <- ldply(   #' output them as csv file.
  list.res.linReg,
  function(list){
    ldply(
      list,
      function(l){
        return(data.frame(l[[1]]))
      }
    )
  }
) %>%
  filter(
    terms=="val"
  ) %>%
  dplyr::select(colnames.RS, season, N, var.y, Estimate, X2.5.., X97.5..,Pr...t..) %>%
  mutate(
    Estimate=round(Estimate, 4),
    N=N,
    X2.5..=round(X2.5.., 4),
    X97.5..=round(X97.5.., 4),
    Pr...t.. = scales::pvalue(Pr...t..)
  )

write.csv(df.res.lm.coefficients, sprintf("%s/lmrob_coefficients.csv",dir.output))


df.res.lm.fitted.resid <- llply(   #' output them as x-y plot with colorization by sAB positivity.
  list.res.linReg,
  function(list){
    ldply(
      list,
      function(l){
        return(data.frame(l[[2]]))
      }
    )
  }
) %>%
  ldply() %>%
  dplyr::rename("name.RS"=".id") %>%
  mutate(
    name.RS2 = gsub("(^bin\\.|^fct\\.)(.?)","\\2",name.RS)
  ) %>%
  left_join(
    ADS[,c("id","season",colnames(ADS)[grep("^(fct.RS\\.|bin.RS\\.)", colnames(ADS))])] %>%
      gather(name.RS2, val, -id, -season)
  )


colnames.colors <- c(
  "Smoke", "DU", "Perni", colnames.sAB#,
  # colnames.Comp, colnames.med, "Asp.DLCO_pct", "PlmArtPress.dias", "PlmArtPress.syst", "HbA1c", "samp.plasm"
)



sink(sprintf("%s/test.txt", dir.output))

for(var.str in 1:length(colnames.Diseases)){
  for(var.col in 1:length(colnames.colors)){
    
    df.plot <- 
      df.res.lm.fitted.resid %>%
      left_join(
        df.demo,
        by = "id"
      ) %>%
      rename(
        "var.str" = colnames.Diseases[var.str],
        "var.col" = colnames.colors[var.col]
        )
    
    if(colnames.colors[var.col] %in% colnames.sAB) df.plot$var.col=factor(df.plot$var.col, levels = c(1,0), labels = c('Positive','Negative'))
    if(!(colnames.colors[var.col] %in% colnames.sAB)) df.plot$var.col=factor(df.plot$var.col, levels = c(1,0), labels = c('Yes','No'))
    
    print(str(df.plot$var.col))
    
    quartz(
      type = "pdf",
      file = 
        sprintf(
          "%s/xyplot_residuals_and_RSscore.%s_%s.pdf",
          dir.output,
          colnames.Diseases[var.str],
          colnames.colors[var.col]
        )
    )
    
    dlply(
      .progress = "text",
      df.plot,
      .(name.RS),
      function(d){
        d <- d %>% arrange(var.col)
        if(
          length(
            grep("^bin\\.",d[1,"name.RS"])) &
          unique(d$name.RS) != "bin.bin.RS.total"
          ) d <- d %>% mutate(val=factor(val, levels = c("low","high"), labels = c("0","> 0")))
        if(
          length(
            grep("^bin\\.bin\\.RS\\.total",d[1,"name.RS"])
            )
          ) d <- d %>% mutate(val=factor(val, levels = c("low","high"), labels = c("0-7","> 7")))
        if(
          length(
            grep("^fct\\.",d[1,"name.RS"])
            )
          ) d <- d %>% mutate(val=factor(as.numeric(val)))
        
        
        gg.df.plot <-
          ggplot(
            d,
            aes(
              x = val, y = res.lm.resid, group=var.col
            )
          )
        
        print(unique(d$var.col))
        
        gg.df.plot_2 <-
          ggplot(
            d %>% mutate(var.col=as.factor(as.numeric(var.col))),
            aes(
              x = val, y = res.lm.resid, group=var.col
            )
          )
        
        geom <-
          geom_point(
            aes(color=var.col),
            size=0.3,position = position_quasirandom()
          )
        
        geom_2 <-
          geom_point(
            aes(color=as.factor(as.numeric(var.col))),
            size=0.3,position = position_quasirandom()
          )
        
        geom_median <-
          geom_boxplot(
            aes(x = as.factor(val), y = res.lm.resid, group=as.factor(val)),
            size=0.1,
            outlier.alpha = 0
            )
        
        smooth <-
          stat_smooth(
            method = robustbase::lmrob, 
            formula = y ~ x,
            aes(x = val, y = res.lm.resid, group=var.col, color=var.col),data = d,
            alpha=0.3, size=0.3,
            se=FALSE
          )
        
        # smooth <- geom_line(aes(y = Pred))
        # ribbon <- geom_ribbon(aes(ymin = lower, ymax = upper, fill=var.col),
        #               alpha = .2
        #               )
        
        facet <-
          facet_wrap(
            y.var ~ var.str ~ season, nrow = 2,
            scales = "free_x"#, labeller = label.Disease
          )
        
        print(d$var.col)
        
        plot(
          gg.df.plot + 
            geom_median +
            smooth +
            geom + 
            # ribbon +
            facet + theme_bw() + 
            scale_color_manual(
              values=c("#DC0000B2", "#4DBBD5B2")
              ) +
            
            ggplot2::labs(
              title = gsub("^bin\\.bin\\.|^fct\\.fct\\.(.+)","\\1",unique(d$name.RS)), 
              subtitle=colnames.colors[var.col]
              ) +
            xlab("Raynaud's condition score") +
            ylab("Residuals from predicted values of the sweat volume.") +
            
            theme(
              plot.title = element_text(size=5, family = "Arial"),
              axis.text.x = element_text(angle=45, hjust = 1, colour = "black"),
              strip.background = element_blank(),
              strip.text = element_text(family = "Arial",size = 10, margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
            )
        )
        plot(  #  
          gg.df.plot_2 + 
            geom_median +
            smooth +
            geom_2 + 
            # ribbon +
            facet + theme_bw() + 
            scale_color_manual(values=c("#DC0000B2", "#4DBBD5B2", "#4DBBD5B2", "#DC0000B2")) +
            ggplot2::labs(
              title = unique(d$name.RS), subtitle=colnames.colors[var.col]
              ) +
            xlab("Raynaud's condition score") +
            ylab("Residuals from predicted values of the sweat volume.") +
            theme(
              plot.title = element_text(size=5, family = "Arial"),
              axis.text.x = element_text(angle=45, hjust = 1, colour = "black"),
              strip.background = element_blank(),
              strip.text = element_text(family = "Arial",size = 10, margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
            )
        )
      }
    )
    dev.off()
  }
}
sink()

