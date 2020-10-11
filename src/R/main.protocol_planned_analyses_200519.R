#' Proj.name : QSART
#' Object    : Protocol Planned Analyses.
#' 
#' created  : 20/05/20
#' 

Bibtex <- TRUE
source('./src/R/sub/require_libraries.main.R')

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

# 
# 9.3.1. 主要評価項目の解析
# 主要評価項目間の関係性について解析を行う（主要な解析「１」）。両者の関係性に対する、副次評価項目の影響を調べることも本研究の主な目的であるため、その解析も主要な解析に含める（主要な解析「２」「３」）。
# 副次的な解析については、特異抗体のプロファイルの類似度と主要な解析「１」の線形回帰式からの残差の関連性を解析する。「軸索反射性発汗による発汗量」、「発汗反応時間」を1変数にまとめた尺度を評価項目として、主要な解析「２」「３」を行う。
# いずれの解析においても、時期（「温暖期」、「寒冷期」）については、別々に解析を行う。
# 本研究の目的は、仮説検定をベースとした解析方法に馴染まないため、研究計画書中に有意水準は設定しない。検定の多重性への対処は、解析ごとにその目的に応じて行う。同様に、信頼区間の幅も研究計画書内には設定しない。
# 
# 9.3.1.1. 主要な解析
# １．	「軸索反射性発汗による発汗量」、「発汗反応時間」それぞれについて、「レイノースコア」への最小二乗法による線形回帰を行い、パラメータの推定値（及びその信頼区間）を確認する。
# ２．	「軸索反射性発汗による発汗量」、「発汗反応時間」それぞれについて、主要な解析「１」の線形回帰式からの残差を縦軸、線形回帰式の説明変数もしくは「レイノースコア」のいずれかを横軸とし、プロットをいずれかの評価項目（患者背景、特異抗体の測定値、等）の値で色分けした散布図（残差プロット）を作成する。この散布図を用いて、それぞれの評価項目の値と、残差および横軸に設定した評価項目の値の関連を、視覚的に確認する。残差プロット上での評価項目の値による特徴を検証する際は、プロット上での距離を尺度としたクラスタリング（Ward法）を行い、興味のあるクラスターの純度をシャノン・エントロピーを用いて表す。
# ３．	膠原病の疾患名（全身性強皮症、混合性結合織病、全身性エリテマトーデス、シェーグレン症候群、皮膚筋炎）で患者を層別したうえで主要な解析「１」「２」を行い、疾患間における「軸索反射性発汗による発汗量」及び「発汗反応時間」の、「レイノースコア」との関連性の違いを確認する。
# 
# 9.3.1.2. 副次的な解析
# 「特異抗体」のプロファイルの類似度と、主要な解析「１」から得られる線形回帰式からの残差「２」の関連性を解析する。プロファイルの類似度については、ジャカード距離等の複数の尺度で評価し、探索的に決定する。
# 「軸索反射性発汗による発汗量」、「発汗反応時間」の2変数について主成分分析を行った際の第１主成分軸上の値をこれら2変数の代わりに用いて、主要な解析「１」「２」「３」を行う。
# 
# 9.3.3. 安全性解析
# 有害事象を発現した研究対象者の一覧を作成する。一覧には、以下の情報を含む。
# 性別、年齢、疾患名（膠原病のみ）、事象名、重篤性、重症度、転帰
# 
# 9.4. 中間解析
# 本研究では中間解析は実施しない。
# 
# 9.5. 欠落、不採用及び異常データの取扱いの手順
# 欠測値の解析上の取り扱いについて，原則として補完を行わない方針とし，必要に応じて試験統計家の意見を参考に統計解析計画書に記載する。未記入値や誤記が疑われる異常データ等の詳細な取扱いは別途「症例取扱い規準」に規定する。
# 

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

