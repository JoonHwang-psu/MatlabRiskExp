library(rstatix)
library(readr)
library(ggplot2)
library(ggpubr)
library(ggeffects)
library(effects)
library(interplot)
library(stargazer)
library(gridExtra)
library(jtools)
library(sjPlot)
library(dplyr)
library(modelsummary)

##### 1. Data preparation

Data <- read.csv("https://raw.githubusercontent.com/JoonHwang-psu/MatlabRiskExp/refs/heads/main/MatlabRiskExp_250218.csv", header=TRUE, fileEncoding="UTF-8-BOM")

Data$new_wealth1000 <- Data$new_wealth2022/1000

Data$agesq <- Data$age^2
Data$gender <- as.factor(Data$gender)
Data$risk_prem_inv <- -(Data$risk_prem)
Data$higher_edu <- with(Data, ifelse(educ_ord>4, 1, 0))

summary(Data$new_wealth1000)
hist(Data$new_wealth1000)
summary(Data$higher_edu)

Data$money_lg_btw_pct <- 10*Data$money_lg_btw # structural social capital measures converted into 0.1 so that the coefficients are comparable with other variables in the regression models
Data$money_lg_cluster_pct <- 10*Data$money_lg_cluster
Data$item_btw_pct <- 10*Data$item_btw
Data$item_cluster_pct <- 10*Data$item_cluster

##### 2. Descriptive statistics
descript <- Data[,c("age", "higher_edu", "gender", "new_wealth1000", 
                    "money_lg_in", "money_lg_mutual", "item_in", "item_mutual", 
                    "money_lg_cluster", "money_lg_btw",
                    "item_cluster", "item_btw")]
descript$gender <- with(descript, ifelse(gender=="Female",1,0))
descript <- na.omit(descript)
stargazer(as.data.frame(descript), summary=TRUE, digits=2,
                    out="RiskExp_summary.htm")

##### 3. regression models predicting risk preference

# 3.1. main effect model (without interaction)
rp_model_main<-lm(risk_prem_inv ~ age+agesq+higher_edu+gender
                  +new_wealth1000
                  +money_lg_in+money_lg_mutual
                  +item_in+item_mutual
                  +money_lg_cluster_pct+money_lg_btw_pct
                  +item_cluster_pct+item_btw_pct
                  , data=Data)
summary(rp_model_main)

# 3.2. interaction effect model (social capital * household wealth)
rp_model_inter<-lm(risk_prem_inv ~ age+agesq+higher_edu+gender
                     +new_wealth1000*(
                     +money_lg_in+money_lg_mutual
                     +item_in+item_mutual
                     +money_lg_cluster_pct+money_lg_btw_pct
                     +item_cluster_pct+item_btw_pct 
                     ), data=Data)
summary(rp_model_inter)

# 3.3. summary of regression results
model.list <- list(rp_model_main, rp_model_inter)
stargazer(model.list, single.row=TRUE, star.char = c(".", "*", "**"), notes = c(". p<0.1; * p<0.05; ** p<0.01"), notes.append=FALSE, out="rpmodels.htm")

# 3.4. visualization of main effect model results
labels <- c('item_btw_pct' = 'betweenness (material) (0.1)',
            'item_cluster_pct' = 'clustering (material) (0.1)',
            'money_lg_btw_pct' = 'betweenness (financial) (0.1)',
            'money_lg_cluster_pct' = 'clustering (financial) (0.1) ',
            'item_mutual' = 'reciprocal ties (material)',
            'item_in' = 'support-receiving ties (material)',
            'money_lg_mutual' = 'reciprocal ties (financial)',
            'money_lg_in' = 'support-receiving ties (financial)',
            'new_wealth1000' = 'Wealth (1,000 BDT)',
            'genderMale' = 'Male',
            'higher_edu' = 'Higher education',
            'agesq' = 'Age^2',
            'age' = 'Age')

modelplot(rp_model_main, size=0.7, coef_omit="Intercept", coef_map=labels) + geom_vline(xintercept = 0, linetype = 2) +
  aes(color = ifelse(p.value > 0.05, "Not significant", " Significant (p<0.05)")) + 
  scale_color_manual(values = c("black","grey")) +
  scale_fill_discrete(labels=c("Significant (p<0.05)", "Marginally significant (p<0.1)", "Not significant")) +
  annotate("segment", x=30, xend=30, y=5, yend=8, size=1) +
  annotate("segment", x=27, xend=30, y=5, yend=5, size=1) +
  annotate("segment", x=27, xend=30, y=8, yend=8, size=1) +
  annotate("text", x=38, y=6.7, label = "Dyadic", fontface="bold") +
  annotate("text", x=38, y=6.3, label = "social capital", fontface="bold") +
  annotate("segment", x=30, xend=30, y=1, yend=4, size=1) +
  annotate("segment", x=27, xend=30, y=1, yend=1, size=1) +
  annotate("segment", x=27, xend=30, y=4, yend=4, size=1) +
  annotate("text", x=38, y=2.7, label = "Structural", fontface="bold") +
  annotate("text", x=38, y=2.3, label = "social capital", fontface="bold") +
  theme(axis.text = element_text(size = 12), axis.title.x = element_blank(), legend.position = "bottom", legend.text=element_text(size=12))

# 3.5. marginal effects of dyadic social capital    
(inter1 <- interplot(m=rp_model_inter, var1="money_lg_in", var2="new_wealth1000")
  +theme_bw()+geom_hline(yintercept=0, linetype="dashed")
  +xlab("material wealth")+ylab("marginal effect of in-degree (financial support)")
  +theme(strip.background = element_blank(), strip.text.x = element_blank())
)
(inter2 <- interplot(m=rp_model_inter, var1="money_lg_mutual", var2="new_wealth1000")
  +theme_bw()+geom_hline(yintercept=0, linetype="dashed")
  +xlab("material wealth")+ylab("marginal effect of reciprocity (financial support)")
  +theme(strip.background = element_blank(), strip.text.x = element_blank())
)
(inter3 <- interplot(m=rp_model_inter, var1="item_in", var2="new_wealth1000")
  +theme_bw()+geom_hline(yintercept=0, linetype="dashed")
  +xlab("material wealth")+ylab("marginal effect of in-degree (material support)")
  +theme(strip.background = element_blank(), strip.text.x = element_blank())
)
(inter4 <- interplot(m=rp_model_inter, var1="item_mutual", var2="new_wealth1000")
  +theme_bw()+geom_hline(yintercept=0, linetype="dashed")
  +xlab("material wealth")+ylab("marginal effect of reciprocity (material support)")
  +theme(strip.background = element_blank(), strip.text.x = element_blank())
)

# 3.6. marginal effects of structural social capital
(inter5 <- interplot(m=rp_model_inter, var1="money_lg_cluster_pct", var2="new_wealth1000")
  +theme_bw()+geom_hline(yintercept=0, linetype="dashed")
  +xlab("material wealth")+ylab("marginal effect of clustering (financial support)")
  +theme(strip.background = element_blank(), strip.text.x = element_blank())
)
(inter6 <- interplot(m=rp_model_inter, var1="money_lg_btw_pct", var2="new_wealth1000")
  +theme_bw()+geom_hline(yintercept=0, linetype="dashed")
  +xlab("material wealth")+ylab("marginal effect of betweenness (financial support)")
  +theme(strip.background = element_blank(), strip.text.x = element_blank())
)
(inter7 <- interplot(m=rp_model_inter, var1="item_cluster_pct", var2="new_wealth1000")
  +theme_bw()+geom_hline(yintercept=0, linetype="dashed")
  +xlab("material wealth")+ylab("marginal effect of clustering (material support)")
  +theme(strip.background = element_blank(), strip.text.x = element_blank())
)
(inter8 <- interplot(m=rp_model_inter, var1="item_btw_pct", var2="new_wealth1000")
  +theme_bw()+geom_hline(yintercept=0, linetype="dashed")
  +xlab("material wealth")+ylab("marginal effect of betweenness (material support)")
  +theme(strip.background = element_blank(), strip.text.x = element_blank())
)

