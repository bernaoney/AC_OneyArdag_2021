## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
library(haven)
library(tidyverse)

TRcitz <- read_stata ("ZA6670_v2-0-0.dta") %>% 
  as_tibble() %>% 
  select(C_ALPHAN,
         V5, V6, V7, V8, V9, V10, V11, V12, V13,
         V32, V33, V34, V35, V36, V37, V38, V39, V40, V62,
         SEX, AGE, EDUCYRS, TR_DEGR,
         TR_RINC, TR_INC,
         TR_RELIG, TR_ETHN1, 
         URBRURAL,
         TR_PRTY, V48) %>%
  filter(C_ALPHAN %in% c("TR")) %>%
  rename(AlwVot=V5, NevaTax=V6, ObLaw=V7, WaAcGov=V8, AcSoPoAs=V9, UndReaOP=V10,
         EnvFri=V11, HlpLpriC=V12, HlpLpriW=V13,
         SaStdLiv=V32, GovProMinRig=V33, PubParDM=V34, CivDis=V35, GovResDemRig=V36, PeLoCitRrev=V37,
         LTRrtVot=V38, CitnoVot=V39, HealCar=V40, DemT=V62,
         gender=SEX, age=AGE, edu=EDUCYRS, CS_eduCat=TR_DEGR,
         per_income=TR_RINC, hh_income=TR_INC,
         Religion=TR_RELIG, Ethnicity=TR_ETHN1,
         UrbanRural=URBRURAL,
         GroupVote=TR_PRTY, PO_LR=V48) %>%
  mutate(GroupVoteBIN = GroupVote)

colnames(TRcitz)

library(psych)
describe(TRcitz[,-1])


## -------------------------------------------------------------------------------------------------------------------------------------------------------
library(naniar)
TRcitz <- TRcitz %>% 
  replace_with_na_at(.vars = c("AlwVot","NevaTax","ObLaw",
                               "WaAcGov","AcSoPoAs","UndReaOP","EnvFri","HlpLpriC","HlpLpriW",
                               "SaStdLiv","GovProMinRig","PubParDM",
                               "CivDis","GovResDemRig","PeLoCitRrev","LTRrtVot","CitnoVot","HealCar"),
                     condition = ~.x == 8)
TRcitz <- TRcitz %>% 
  replace_with_na_at(.vars = c("AlwVot","NevaTax","ObLaw",
                               "WaAcGov","AcSoPoAs","UndReaOP","EnvFri","HlpLpriC","HlpLpriW",
                               "SaStdLiv","GovProMinRig","PubParDM",
                               "CivDis","GovResDemRig","PeLoCitRrev","LTRrtVot","CitnoVot","HealCar",
                               "gender"),
                     condition = ~.x == 9)
TRcitz <- TRcitz %>% 
  replace_with_na_at(.vars = c("edu", "PO_LR"),
                     condition = ~.x == 98)
TRcitz <- TRcitz %>% 
  replace_with_na_at(.vars = c("DemT", "GroupVote", "GroupVoteBIN","CS_eduCat", "PO_LR"),
                     condition = ~.x == 99)
TRcitz <- TRcitz %>% 
  replace_with_na_at(.vars = c("GroupVote", "GroupVoteBIN"),
                     condition = ~.x == 0)
TRcitz <- TRcitz %>% 
  replace_with_na_at(.vars = c("age"),
                     condition = ~.x == 999)
TRcitz <- TRcitz %>% 
  replace_with_na_at(.vars = c("per_income"),
                     condition = ~.x == 999999)
TRcitz <- TRcitz %>% 
  replace_with_na_at(.vars = c("hh_income"),
                     condition = ~.x == 999999)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRcitz$GroupVoteBIN  <- car::recode(TRcitz$GroupVoteBIN,"1=1; 2=0; 3=0; 4=0; 5=0; 6=0; 7=0; 94=0; 96=0")
library(janitor)
tabyl(TRcitz$GroupVoteBIN) # non-AKP 0.43 AKP 0.57
# 1=AKP 3=BDP 4=CHP 6=MHP 100=other
TRcitz$GroupVote  <- car::recode(TRcitz$GroupVote,"1=1; 3=3; 4=4; 6=6; 2=100; 5=100; 7=100; 94=100; 96=100")
tabyl(TRcitz$GroupVote) # AKP 0.57 BDP 0.10 CHP 0.21 MHP 0.09 other 0.03

TRcitz$GroupVoteBIN <- factor(TRcitz$GroupVoteBIN, levels=c(0,1), labels = c("nonAKPv", "AKPv"))
TRcitz$GroupVote <- factor(TRcitz$GroupVote, levels=c(1,3,4,6,100), labels = c("AKP","BDP","CHP","MHP","Other"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRcitz$ageI <- cut(TRcitz$age, breaks=c(20,30,40,50,60, 70, 80, Inf),
                   labels=c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">80"))

TRcitz$gender <- factor(TRcitz$gender, levels = c(1,2), labels = c("Male", "Female"))
TRcitz$CS_eduCat <- factor(TRcitz$CS_eduCat, levels = c(1,2,3,4,5,6,7),
                           labels = c("NoFormal", "Primary", "jHigh", "High", "IncompleteUni", "Uni", "postGrad"))
TRcitz$CS_eduCat <- fct_collapse(TRcitz$CS_eduCat,
                                 NoFormal = "NoFormal",
                                 somePrimarySchool = "Primary",
                                 someHighSchool = c("jHigh", "High", "IncompleteUni"),
                                 UniHigher = c("Uni", "postGrad"))
tabyl(TRcitz$CS_eduCat)
TRcitz$per_incomeCat <- cut(TRcitz$per_income,
                            breaks=c(-Inf, 1000, 3000, Inf),
                            labels=c("<1000TL","1000-3000TL",">3000TL")) # define personal income categories
TRcitz$hh_incomeCat <- cut(TRcitz$hh_income,
                           breaks=c(-Inf, 1500, 3000, Inf),
                           labels=c("<1500TL","1500-3000TL",">3000TL")) # define household income categories
TRcitz$Religion <- factor(TRcitz$Religion, levels = c(690, 999, 0, 950, 300, 500),
                          labels = c("Muslim", "Other", "Other",  "Other", "Other","Other"))
TRcitz$Ethnicity <- factor(TRcitz$Ethnicity, levels = c(2, 6, 4, 99, 1, 3, 95),
                           labels = c("Bulgarian", "Turk", "Kurd",  "No answer", "Arab","Circassian", "Other"))
TRcitz$UrbanRural <- factor(TRcitz$UrbanRural, levels = c(1,2,3,4,5),
                            labels= c("A big city", "The suburbs or outskirts of a",
                                      "A town or a small city",
                                      "A country village", "A farm or home in the country"))
describe(TRcitz[,-c(1)])


## -------------------------------------------------------------------------------------------------------------------------------------------------------
EDU_bp <- TRcitz %>% dplyr::select(CS_eduCat) %>% drop_na() %>%
  ggplot(., aes(x=CS_eduCat)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Education") +
  coord_flip()

INC_bp <- TRcitz %>% dplyr::select(hh_incomeCat) %>% drop_na() %>%
  ggplot(., aes(x=hh_incomeCat)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Household Income") +
  coord_flip()

UR_bp <- TRcitz %>% dplyr::select(UrbanRural) %>% ggplot(., aes(x=UrbanRural)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Urban vs. Rural") +
  coord_flip()

ETH_bp <- TRcitz %>% dplyr::select(Ethnicity) %>% drop_na() %>%
  ggplot(., aes(x=Ethnicity)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Ethnicity") +
  coord_flip()

REL_bp <- TRcitz %>% dplyr::select(Religion) %>% drop_na() %>%
  ggplot(., aes(x=Religion)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Religion") +
  coord_flip()

VR_bp <- TRcitz %>% dplyr::select(GroupVote) %>% drop_na() %>%
  ggplot(., aes(x=GroupVote)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..), label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Vote Recall") +
  coord_flip()

LR_bp <- TRcitz %>% dplyr::select(PO_LR) %>% drop_na() %>%
  ggplot(., aes(x=as_factor(PO_LR))) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) +
  scale_x_discrete(labels = c("Left","1","2","3","4","5","6","7","8","9","Right")) +
  geom_text(aes(y = (..count..) ,label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", angle=45) + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Left-Right")

GE_bp <- TRcitz %>% dplyr::select(gender) %>% drop_na() %>%
  ggplot(., aes(x=gender)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Gender") +
  coord_flip()

AG_bp <- TRcitz %>% dplyr::select(ageI) %>% drop_na() %>%
  ggplot(., aes(x=ageI)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Age") +
  coord_flip()

library(grid)
library(gridExtra)
grid.arrange(GE_bp, AG_bp, ETH_bp,
             REL_bp, EDU_bp, INC_bp,
             UR_bp, LR_bp, VR_bp,
             nrow = 3, ncol = 3,
             top = "Sample characteristics in Study 1",
             bottom = textGrob("Source: ISSP Citizenship Module 3 
                               \nhttps://www.gesis.org/issp/modules/issp-modules-by-topic/citizenship/2014/
                               \nData Collection: 2015",
                               gp = gpar(fontface = 3, fontsize = 10), just = "center"))

rm(list=setdiff(ls(), "TRcitz"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRcitz_ds <- describe(TRcitz[,-c(1)])
TRcitz_ds <- round(TRcitz_ds,2)
library(knitr)
TRcitz_ds %>% kable()
rm(TRcitz_ds)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
paCIT <- fa.parallel (TRcitz [2:10], fm="ml", fa="fa", main = " PA Conceptions of Good Citizenship") # parallel analysis
paCIT # suggests that the number of factors =  3

vssCIT <- vss (TRcitz [2:10], n = 9, fm="mle", rotate = "oblimin", title = "VSS Conceptions of Good Citizenship") # Very simple structure
vssCIT

cormatCIT <- corFiml(TRcitz [,2:10], covar = F, show = F) # correlation matrix

CIT2d  <- fa(r=cormatCIT, nfactors=2, fm="ml", rotate="oblimin") # efa 2 dimensions
print (CIT2d, cut=0.3, sort = T)
fa.diagram (CIT2d)

CIT3d  <- fa(r=cormatCIT, nfactors=3, fm="ml", rotate="oblimin") # efa 3 dimensions
print (CIT3d, cut=0.3, sort = T)
fa.diagram (CIT3d)

CIT4d  <- fa(r=cormatCIT, nfactors=4, fm="ml", rotate="oblimin") # efa 4 dimensions
print (CIT4d, cut=0.3, sort = T)
fa.diagram (CIT4d)

# confirmatory analysis
library(lavaan)
modelCIT <- '
hlpCit =~ HlpLpriC + HlpLpriW
dtyCit =~ NevaTax + ObLaw + AlwVot
actCit =~ AcSoPoAs + UndReaOP + WaAcGov + EnvFri
'
fitCIT <- cfa (modelCIT, data=TRcitz, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(fitCIT, fit.measures=T, standardized=T, rsquare=T)
library(semPlot)
semPaths(fitCIT, "mod", "std", intercepts = F, edge.label.cex=1.1, layout="tree3", rotation = 4, curve = 2)
library(semTools)
round(reliability(fitCIT),2)

CIT_fs <- lavPredict(fitCIT, type = "lv", method = "EBM", label = T, fsm = F)
CIT_fs <- as.data.frame(CIT_fs)
describe(CIT_fs)
hist(CIT_fs)
TRcitz$hlpCit <- scales::rescale(CIT_fs$hlpCit, to = c(0, 1), from = range(CIT_fs$hlpCit, na.rm = F, finite = T))
TRcitz$dtyCit <- scales::rescale(CIT_fs$dtyCit, to = c(0, 1), from = range(CIT_fs$dtyCit, na.rm = F, finite = T))
TRcitz$actCit <- scales::rescale(CIT_fs$actCit, to = c(0, 1), from = range(CIT_fs$actCit, na.rm = F, finite = T))

colnames(TRcitz)

library(ggridges)
ggr_AC <- TRcitz %>% select(actCit, GroupVoteBIN) %>%
  ggplot(aes(x=actCit, y=GroupVoteBIN, fill = GroupVoteBIN)) + geom_density_ridges(alpha=.1) + theme_bw() +
  xlab("Normalized Factor Scores") +
  ylab("") + 
  ggtitle("Active Citizen") + theme(legend.position = "none")
ggr_DC <- TRcitz %>% select(dtyCit, GroupVoteBIN) %>%
  ggplot(aes(x=dtyCit, y=GroupVoteBIN, fill = GroupVoteBIN)) + geom_density_ridges(alpha=.1) + theme_bw() +
  xlab("Normalized Factor Scores") +
  ylab("") + 
  ggtitle("Dutiful Citizen") + theme(legend.position = "none")
ggr_HC <- TRcitz %>% select(hlpCit, GroupVoteBIN) %>%
  ggplot(aes(x=hlpCit, y=GroupVoteBIN, fill = GroupVoteBIN)) + geom_density_ridges(alpha=.1) + theme_bw() +
  xlab("Normalized Factor Scores") +
  ylab("") + 
  ggtitle("Helpful Citizen")

grid.arrange(ggr_AC,ggr_DC,ggr_HC,
             nrow = 2, ncol = 2,
             top = "Ridge Plots",
             left= "VoteRecall")


## -------------------------------------------------------------------------------------------------------------------------------------------------------
paDEM <- fa.parallel (TRcitz [,11:19], fm="ml", fa="fa", main = " PA Conceptions of Democracy") # parallel analysis
paDEM # suggests that the number of factors =  3

vssDEM <- vss (TRcitz [,11:19], n = 9, fm="mle", rotate = "oblimin", title = "VSS Conceptions of Democracy") # Very simple structure
vssDEM

cormatDEM <- corFiml(TRcitz [,11:19], covar = F, show = F) # correlation matrix

DEM2d  <- fa(r=cormatDEM, nfactors=2, fm="ml", rotate="oblimin") # efa 2 dimensions
print (DEM2d, cut=0.3, sort = T)
fa.diagram (DEM2d)

DEM3d  <- fa(r=cormatDEM, nfactors=3, fm="ml", rotate="oblimin") # efa 3 dimensions
print (DEM3d, cut=0.3, sort = T)
fa.diagram (DEM3d)

DEM4d  <- fa(r=cormatDEM, nfactors=4, fm="ml", rotate="oblimin") # efa 4 dimensions
print (DEM4d, cut=0.3, sort = T)
fa.diagram (DEM4d)

# confirmatory analysis
modelDEM2d <- '
DEMsd =~ GovProMinRig + PubParDM + SaStdLiv + GovResDemRig + HealCar
DEMpr =~ LTRrtVot + PeLoCitRrev + CivDis + CitnoVot
'
fitDEM2d <- cfa (modelDEM2d, data=TRcitz, estimator="mlr", std.ov=T, std.lv=T, mimic="mplus", missing="fiml")
summary(fitDEM2d, fit.measures=T, standardized=T, rsquare=T)
semPaths(fitDEM2d, "mod", "std", intercepts = F, edge.label.cex=1.1, layout="tree3", rotation = 4)
round(reliability(fitDEM2d),2)
# goodness of fit statistics are aaceptable, reliabilities are not bad

DEM_fs <- lavPredict(fitDEM2d, type = "lv", method = "EBM", label = T, fsm = F)
DEM_fs <- as.data.frame(DEM_fs)
describe(DEM_fs)
hist(DEM_fs)
TRcitz$DEMsd <- scales::rescale(DEM_fs$DEMsd, to = c(0, 1), from = range(DEM_fs$DEMsd, na.rm = F, finite = T))
TRcitz$DEMpr <- scales::rescale(DEM_fs$DEMpr, to = c(0, 1), from = range(DEM_fs$DEMpr, na.rm = F, finite = T))

ggr_DEp <- TRcitz %>% select(DEMpr, GroupVoteBIN) %>%
  ggplot(aes(x=DEMpr, y=GroupVoteBIN, fill = GroupVoteBIN)) + geom_density_ridges(alpha=.1) + theme_bw() +
  xlab("Normalized Factor Scores") +
  xlim(0:1) +
  ylab("") + 
  ggtitle("Civic Engagement") + theme(legend.position = "none")
ggr_DEs <- TRcitz %>% select(DEMsd, GroupVoteBIN) %>%
  ggplot(aes(x=DEMsd, y=GroupVoteBIN, fill = GroupVoteBIN)) + geom_density_ridges(alpha=.1) + theme_bw() +
  xlab("Normalized Factor Scores") +
  xlim(0:1) +
  ylab("") + 
  ggtitle("State's Duties")

grid.arrange(ggr_DEp,ggr_DEs,
             nrow = 1, ncol = 2,
             top = "Ridge Plots",
             left= "VoteRecall")

colnames(TRcitz)
LC_ds <- describe(TRcitz[,35:39])
LC_ds %>% kable(digits = 2)

rm(list=setdiff(ls(), "TRcitz"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
logit_sociodem <- glm(GroupVoteBIN ~ gender + age + hh_incomeCat + CS_eduCat,
                      data = TRcitz, family = binomial(link="logit"))
summary(logit_sociodem)
fmsb::NagelkerkeR2(logit_sociodem)

logit_citizen <- glm(GroupVoteBIN ~ hlpCit + dtyCit + actCit + gender + age + hh_incomeCat + CS_eduCat,
                     data = TRcitz, family = binomial(link="logit"))
summary(logit_citizen)
fmsb::NagelkerkeR2(logit_citizen)

# marginals
library(margins)
mar_cit <- margins(logit_citizen)
sum_mar_cit <-summary(mar_cit)
sum_mar_cit %>% kable(digits = 3)

logit_democracy <- glm(GroupVoteBIN ~ DEMsd + DEMpr + gender + age + hh_incomeCat + CS_eduCat,
                       data = TRcitz, family = binomial(link="logit"))
summary(logit_democracy)
fmsb::NagelkerkeR2(logit_democracy)

mar_dem <- margins(logit_democracy)
sum_mar_dem <- summary(mar_dem)
sum_mar_dem %>% kable(digits = 3)

logit_all <- glm(GroupVoteBIN ~ hlpCit + dtyCit + actCit + DEMsd + DEMpr + DemT + gender + age + hh_incomeCat + CS_eduCat,
                       data = TRcitz, family = binomial(link="logit"))
summary(logit_all)
fmsb::NagelkerkeR2(logit_all)
mar_all <- margins(logit_all)
sum_mar_all <-summary(mar_all)
sum_mar_all %>% kable(digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
library(strengejacke)
tab_model(logit_sociodem,logit_citizen,logit_democracy,
          show.se = T,
          transform = NULL,
          pred.labels = c("Intercept",
                          "Gender",
                          "Age Group",
                          "Mid-Level Income", "High-Level Income",
                          "Some primary school completed",
                          "Some high school completed",
                          "University degree & higher",
                          "Helpful Citizen", "Dutiful Citizen", "Active Citizen",
                          "State's Duties", "Political Civic Engagement"),
          dv.labels = c("Socio-demographics model","Citizenship model", "Democracy model"),
          string.pred = "Coefficient",
          string.ci = "CI (95%)",
          string.p = "P-Value")


## -------------------------------------------------------------------------------------------------------------------------------------------------------
theme_set(theme_sjplot())
p_citz <- plot_model(logit_citizen, type = "eff", terms = "actCit [all]") +
    labs(title = "Probability of voting for the AKP by being an active citizen",
       tag = "A",
       x = "Active Citizen (Normalized Factor Scores)", y = "")
p_dem <- plot_model(logit_democracy, type = "eff", terms = "DEMpr [all]") +
  labs(title = "Probability of voting for the AKP by civic engagement",
       tag = "B",
       x = "Civic Engagement (Normalized Factor Scores)", y = "")

grid.arrange(p_citz, p_dem, ncol = 2, nrow=1,
             left="Predicted % (AKP Vote)",
             bottom="Note: All other variables held at their observed values
             Source: ISSP Citizenship Module II") # plot side by side


## -------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls(all.names = T)) # clear memory


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew <- read_stata ("PewResearchGlobalAttitudesSpring2015DatasetforWebFINAL.dta") %>% 
  as_tibble() %>% 
  select(COUNTRY,
         Q9, Q11,
         Q182TUR,
         Q78TUR, 
         Q145, Q146, Q163B, Q163TUR, Q165TUR, Q165TURB,
         Q219TUR, Q220TUR) %>%
  filter(COUNTRY %in% c("34")) %>% 
  rename(DemFG_SL = Q9, SatWLvODem = Q11,
         gender=Q145, age=Q146, edu=Q163B, CS_eduCat=Q163TUR,
         hh_income=Q165TUR, hh_incomeBcat = Q165TURB,
         Religion=Q78TUR,
         UR=Q219TUR, UrbanRural=Q220TUR,
         GroupVote=Q182TUR) %>%
  mutate(GroupVoteBIN = GroupVote)

colnames(TRpew)
describe(TRpew)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("DemFG_SL","SatWLvODem"),
                     condition = ~.x == 8)
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("DemFG_SL","SatWLvODem"),
                     condition = ~.x == 9)
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("GroupVote", "GroupVoteBIN"),
                     condition = ~.x == 96)
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("GroupVote", "GroupVoteBIN"),
                     condition = ~.x == 97)
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("GroupVote", "GroupVoteBIN", "edu", "hh_income", "hh_incomeBcat"),
                     condition = ~.x == 98)
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("GroupVote", "GroupVoteBIN", "age", "edu", "CS_eduCat", "hh_income", "hh_incomeBcat", "Religion"),
                     condition = ~.x == 99)

describe(TRpew [,-1])


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew$GroupVoteBIN  <- car::recode(TRpew$GroupVoteBIN,"1=1; 2=0; 3=0; 4=0; 5=0; 6=0; 7=0; 8=0; 9=0; 10=0;
                                   11=0; 12=0; 13=0; 14=0; 15=0; 16=0; 17=0; 18=0; 19=0; 20=0; 21=0; 22=0;
                                   23=0; 24=0; 25=0; 26=0; 27=0")
TRpew$GroupVoteBIN <- factor(TRpew$GroupVoteBIN, levels=c(0,1), labels = c("nonAKPv", "AKPv"))
tabyl(TRpew$GroupVoteBIN)

# 1=AKP 15=HDP 5=CHP 2=MHP 100=other
TRpew$GroupVote  <- car::recode(TRpew$GroupVote,"1=1; 19=2; 5=3; 15=4;
                                2=100; 3=100; 4=100; 6=100; 7=100; 8=100; 9=100; 10=100; 11=100; 12=100; 13=100; 14=100;
                                16=100; 17=100; 18=100; 20=100; 21=100; 22=100; 23=100; 24=100; 2100=100; 26=100; 27=100")

TRpew$GroupVote <- factor(TRpew$GroupVote, levels = c(1,2,3,4,100), labels = c("AKP","MHP","CHP","HDP","Other"))
tabyl(TRpew$GroupVote) # AKP 0.48 MHP 0.11 CHP 0.25 HDP 0.13 other 0.04


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew$ageI <- cut(TRpew$age, breaks=c(20,30,40,50,60, 70, 80, Inf),
                  labels=c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">80"))
round(prop.table(table(TRpew$ageI)),2)

TRpew$gender <- factor(TRpew$gender, labels = c("Male", "Female"))
round(prop.table(table(TRpew$gender)),2) # M 0.53   F 0.47

round(prop.table(table(TRpew$CS_eduCat)),2)
TRpew$CS_eduCat <- factor(TRpew$CS_eduCat, levels = c(1:11), labels = c("NoFormal","IncompletePri","Primary5", "Primary8",
                                                                        "Complete junior high school","Complete vocational school at junior high",
                                                                        "Complete high school","Complete vocational school at high school",
                                                                        "Uni degree", "Graduate degree", "PhD"))
TRpew$CS_eduCat <- fct_collapse(TRpew$CS_eduCat,
                                NoFormal = c("NoFormal","IncompletePri"),
                                somePrimarySchool = c("Primary5", "Primary8"),
                                someHighSchool = c("Complete junior high school","Complete vocational school at junior high",
                                                   "Complete high school","Complete vocational school at high school"),
                                UniHigher = c("Uni degree", "Graduate degree", "PhD"))
round(prop.table(table(TRpew$CS_eduCat)),2)
tabyl(TRpew$CS_eduCat)

TRpew$hh_incomeCat <- cut(TRpew$hh_income,
                          breaks=c(-Inf, 5, 9, Inf),
                          labels=c("low","medium","high")) # define household income categories
round(prop.table(table(TRpew$hh_incomeCat)),2) # 0.18 low 0.76 medium 0.06 high //roughly correspond to 1000 3000 divide
tabyl(TRpew$hh_incomeCat)

TRpew$hh_incomeBcat <- factor(TRpew$hh_incomeBcat, labels = c("<1000", ">1000")) # define household income categories alternative
round(prop.table(table(TRpew$hh_incomeBcat)),2) # <1000 0.13  >1000 0.87

TRpew$Religion <- factor(TRpew$Religion, levels = c(1,3,4,5,6),
                         labels = c("Muslim","Atheist","Agnostic","Nothing in particular","Something else"))
TRpew$Religion <- fct_collapse(TRpew$Religion,
                               Muslim = "Muslim",
                               Other = c("Atheist","Agnostic","Nothing in particular","Something else"))
tabyl(TRpew$Religion)

TRpew$UrbanRural <- factor(TRpew$UrbanRural, levels = c(1,2,3), labels = c("Urban center","Urban periphery","Rural"))
tabyl(TRpew$UrbanRural)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
GE_bp <- TRpew %>% select(gender) %>% drop_na() %>%
  ggplot(., aes(x=gender)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Gender") +
  coord_flip()

AG_bp <- TRpew %>% select(ageI) %>% drop_na() %>%
  ggplot(., aes(x=ageI)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Age") +
  coord_flip()

REL_bp <- TRpew %>% select(Religion) %>% drop_na() %>%
  ggplot(., aes(x=Religion)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Religion") +
  coord_flip()

EDU_bp <- TRpew %>% dplyr::select(CS_eduCat) %>% drop_na() %>%
  ggplot(., aes(x=CS_eduCat)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Education") +
  coord_flip()

INC_bp <- TRpew %>% dplyr::select(hh_incomeCat) %>% drop_na() %>%
  ggplot(., aes(x=hh_incomeCat)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Household Income") +
  coord_flip()

UR_bp <- TRpew %>% dplyr::select(UrbanRural) %>% ggplot(., aes(x=UrbanRural)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Urban vs. Rural") +
  coord_flip()

VR_bp <- TRpew %>% dplyr::select(GroupVote) %>% drop_na() %>%
  ggplot(., aes(x=GroupVote)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Feeling close to") +
  coord_flip()

grid.arrange(GE_bp, AG_bp, REL_bp, 
             EDU_bp, INC_bp, UR_bp,
             VR_bp,
             nrow = 3, ncol = 3,
             top = "Sample characteristics in Study 2a",
             bottom = textGrob("Source: Pew Global Attitudes 2015
                               \nhttps://www.pewresearch.org/global/datasets/2015/
                               \nData Collection: 2015",
                               gp = gpar(fontface = 3, fontsize = 10), just = "center"))

rm(list=setdiff(ls(), "TRpew"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(TRpew$SatWLvODem)),2)
TRpew$SatWLvODem <- car::recode(TRpew$SatWLvODem, "1=4; 2=3; 3=2; 4=1")
TRpew$SatWLvODem <- factor(TRpew$SatWLvODem, labels = c("Not at all satisfied","Not too satisfied", "Somewhat satisfied", "Very satisfied"))
round(prop.table(table(TRpew$SatWLvODem)),2)
tabyl(TRpew$SatWLvODem)

round(prop.table(table(TRpew$DemFG_SL)),2)
TRpew$DemFG_SL <- factor(TRpew$DemFG_SL, labels = c("Democratic form of government","Strong leader"))
round(prop.table(table(TRpew$DemFG_SL)),2)
tabyl(TRpew$DemFG_SL)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew %>%
  tabyl(DemFG_SL, GroupVoteBIN) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 2) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  kable()

TRpew %>%
  tabyl(SatWLvODem, GroupVoteBIN) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 2) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  kable()

describe(TRpew [,-1])


## -------------------------------------------------------------------------------------------------------------------------------------------------------
logit_vote_socdem <- glm(GroupVoteBIN ~ gender + age + hh_incomeCat + CS_eduCat,
                        data = TRpew, family = binomial(link="logit"))
summary(logit_vote_socdem)
fmsb::NagelkerkeR2(logit_vote_socdem)

logit_vote <- glm(GroupVoteBIN ~ DemFG_SL + SatWLvODem + gender + age + hh_incomeCat + CS_eduCat,
                  data = TRpew, family = binomial(link="logit"))
summary(logit_vote)
fmsb::NagelkerkeR2(logit_vote)

mar_vote <- margins(logit_vote)
mar_vote_sum <- summary(mar_vote)
mar_vote_sum %>% kable(digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
tab_model(logit_vote_socdem, logit_vote,
          show.se = T,
          transform = NULL,
          pred.labels = c("Intercept",
                          "Gender","Age",
                          "Medium-level Income", "High-level Income",
                          "Some primary school completed",
                          "Some high school completed",
                          "University degree & higher",
                          "Preferring a strong leader (Authoritarianism)",
                          "Being not too satisfied with the way democracy is working in TR",
                          "Being somewhat satisfied with the way democracy is working in TR",
                          "Being very satisfied with the way democracy is working in TR"),
          dv.labels = c("Socio-demographics model","... [full] model"),
          string.pred = "Coefficient",
          string.ci = "CI (95%)",
          string.p = "P-Value")


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_pred_dem <- ggeffect(logit_vote, terms = "SatWLvODem")
p_demS <- plot(fit_pred_dem) + labs(title = "Probability of feeling closer to AKP by Satisfaction with Democracy",
                                    tag = "A",
                                    x = "Level of Satisfaction with Democracy", y = "")
fit_pred_auth <- ggeffect(logit_vote, terms = "DemFG_SL")
p_auth <- plot(fit_pred_auth) + labs(title = "Probability of feeling closer to AKP by Authoritarianism",
                                     tag = "B",
                                     x = "Democracy vs. Strong Leader", y = "")
grid.arrange(p_demS, p_auth, ncol = 2, nrow=1,
             left="Predicted % (Feeling closer to AKP)",
             bottom="Note: All other variables held at their observed values
             Source: Pew Global Attitudes 2015") # plot side by side


## -------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls(all.names = T)) # clear memory


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRces <- read_stata ("cses4.dta") %>% 
  as_tibble() %>% 
  select(D1006_NAM,
         D3006_LH_PL,
         D3017,
         D2002, D2001_Y, D2003, D2020,
         D2026,
         D2031,
         D3014) %>%
  filter(D1006_NAM %in% c("Turkey")) %>% 
  rename(SatWLvODem = D3017,
         gender=D2002, age=D2001_Y, edu=D2003,
         hh_income=D2020,
         Religion=D2026,
         UrbanRural=D2031,
         PO_LR=D3014,
         GroupVote=D3006_LH_PL) %>%
  mutate(GroupVoteBIN = GroupVote)

colnames(TRces)
describe(TRces[,-1])


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRces <- TRces %>%
  replace_with_na(replace = list(GroupVote = c(92,97,98,99),
                                 GroupVoteBIN = c(92,97,98,99),
                                 PO_LR = c(95,97,98,99),
                                 SatWLvODem = c(9),
                                 age = c(9997,9998),
                                 edu = c(97),
                                 hh_income = c(9),
                                 Religion = c(9997)))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRces$GroupVoteBIN  <- car::recode(TRces$GroupVoteBIN,"1=1; 2=0; 3=0; 4=0; 5=0; 90=0")
TRces$GroupVoteBIN <- factor(TRces$GroupVoteBIN, levels=c(0,1), labels = c("nonAKPv", "AKPv"))
tabyl(TRces$GroupVoteBIN)

# 1=AKP 2=HDP 3=CHP 4=MHP 5=SP 90=other
TRces$GroupVote  <- car::recode(TRces$GroupVote,"1=1; 4=2; 3=3; 2=4;
                                5=100; 90=100")
TRces$GroupVote <- factor(TRces$GroupVote, levels = c(1,2,3,4,100), labels = c("AKP","MHP","CHP","HDP","Other"))
tabyl(TRces$GroupVote) # AKP 0.48 MHP 0.11 CHP 0.25 HDP 0.13 other 0.04


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRces$age <- 2015-TRces$age
TRces$ageI <- cut(TRces$age, breaks=c(20,30,40,50,60, 70, 80, Inf),
                  labels=c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">80"))
round(prop.table(table(TRces$ageI)),2)

TRces$gender <- factor(TRces$gender, labels = c("Male", "Female"))
round(prop.table(table(TRces$gender)),2) # M 0.51   F 0.49

round(prop.table(table(TRces$edu)),2)
TRces$edu <- factor(TRces$edu, levels = c(2,3,4,7,8,9,96),
                          labels = c("PRIMARY","LOWER SECONDARY","UPPER SECONDARY",
                                     "BACHELOR OR EQUIVAL","MASTER OR EQUIVALEN","DOCTORAL OR EQUIVAL",
                                     "NO EDUCATION"))
TRces$edu <- fct_collapse(TRces$edu,
                          NoFormal = c("NO EDUCATION"),
                          somePrimarySchool = c("PRIMARY"),
                          someHighSchool = c("LOWER SECONDARY","UPPER SECONDARY"),
                          UniHigher = c("BACHELOR OR EQUIVAL","MASTER OR EQUIVALEN","DOCTORAL OR EQUIVAL"))
round(prop.table(table(TRces$edu)),2)
tabyl(TRces$edu)
TRces$edu <- relevel(TRces$edu, "NoFormal")

TRces$hh_income <- cut(TRces$hh_income,
                       breaks=c(-Inf, 2, 4, Inf),
                       labels=c("low","medium","high")) # define household income categories
round(prop.table(table(TRces$hh_income)),2) # 0.18 low 0.76 medium 0.06 high //roughly correspond to 1000 3000 divide
tabyl(TRces$hh_income)

TRces$Religion <- factor(TRces$Religion, levels = c(3000,8300),
                         labels = c("Muslim","None"))
tabyl(TRces$Religion)

TRces$UrbanRural <- factor(TRces$UrbanRural, levels = c(1,2,3,4), labels = c("Rural area or village","Small or middle sized town",
                                                                             "Suburbs of large town or city", "Large town or city"))
tabyl(TRces$UrbanRural)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
GE_bp <- TRces %>% select(gender) %>% drop_na() %>%
  ggplot(., aes(x=gender)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Gender") +
  coord_flip()

AG_bp <- TRces %>% select(ageI) %>% drop_na() %>%
  ggplot(., aes(x=ageI)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Age") +
  coord_flip()

REL_bp <- TRces %>% select(Religion) %>% drop_na() %>%
  ggplot(., aes(x=Religion)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Religion") +
  coord_flip()

EDU_bp <- TRces %>% dplyr::select(edu) %>% drop_na() %>%
  ggplot(., aes(x=edu)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Education") +
  coord_flip()

INC_bp <- TRces %>% dplyr::select(hh_income) %>% drop_na() %>%
  ggplot(., aes(x=hh_income)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Household Income") +
  coord_flip()

UR_bp <- TRces %>% dplyr::select(UrbanRural) %>% ggplot(., aes(x=UrbanRural)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Urban vs. Rural") +
  coord_flip()

LR_bp <- TRces %>% dplyr::select(PO_LR) %>% drop_na() %>%
  ggplot(., aes(x=as_factor(PO_LR))) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) +
  scale_x_discrete(labels = c("Left","1","2","3","4","5","6","7","8","9","Right")) +
  geom_text(aes(y = (..count..) ,label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", angle=45) + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Left-Right")

VR_bp <- TRces %>% dplyr::select(GroupVote) %>% drop_na() %>%
  ggplot(., aes(x=GroupVote)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Vote Recall") +
  coord_flip()

grid.arrange(GE_bp, AG_bp, REL_bp, 
             EDU_bp, INC_bp, UR_bp,
             LR_bp,VR_bp,
             nrow = 3, ncol = 3,
             top = "Sample characteristics in Study 2b",
             bottom = textGrob("Source: Comparative Election Studies 4 2015
                               \nhttps://cses.org/data-download/module-4-2011-2016/
                               \nData Collection: 2015",
                               gp = gpar(fontface = 3, fontsize = 10), just = "center"))

rm(list=setdiff(ls(), "TRces"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(TRces$SatWLvODem)),2)
TRces$SatWLvODem <- car::recode(TRces$SatWLvODem, "1=4; 2=3; 4=2; 5=1")
TRces$SatWLvODem <- factor(TRces$SatWLvODem, labels = c("Not at all satisfied","Not too satisfied", "Somewhat satisfied", "Very satisfied"))
round(prop.table(table(TRces$SatWLvODem)),2)
tabyl(TRces$SatWLvODem)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRces %>%
  tabyl(SatWLvODem, GroupVoteBIN) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 2) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  kable()

describe(TRces [,-1])


## -------------------------------------------------------------------------------------------------------------------------------------------------------
logit_vote_socdem <- glm(GroupVoteBIN ~ gender + age + hh_income + edu,
                         data = TRces, family = binomial(link="logit"))
summary(logit_vote_socdem)
fmsb::NagelkerkeR2(logit_vote_socdem)

logit_vote <- glm(GroupVoteBIN ~ SatWLvODem + gender + age + hh_income + edu,
                  data = TRces, family = binomial(link="logit"))
summary(logit_vote)
fmsb::NagelkerkeR2(logit_vote)

mar_vote <- margins(logit_vote)
mar_vote_sum <- summary(mar_vote)
mar_vote_sum %>% kable(digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
tab_model(logit_vote_socdem, logit_vote,
          show.se = T,
          transform = NULL,
          pred.labels = c("Intercept",
                          "Gender","Age",
                          "Medium-level Income", "High-level Income",
                          "Some primary school completed",
                          "Some high school completed",
                          "University degree & higher",
                          "Being not too satisfied with the way democracy is working in TR",
                          "Being somewhat satisfied with the way democracy is working in TR",
                          "Being very satisfied with the way democracy is working in TR"),
          dv.labels = c("Socio-demographics model","... [full] model"),
          string.pred = "Coefficient",
          string.ci = "CI (95%)",
          string.p = "P-Value")


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_pred_dem <- ggeffect(logit_vote, terms = "SatWLvODem")
plot(fit_pred_dem) + labs(title = "Probability of voting for AKP by Satisfaction with Democracy",
                           subtitle = "Note: All other variables held at their mean values",
                           x = "Level of Satisfaction with Democracy",
                           y = "Predicted % (Vote Recall)",
                           caption = "Source: Comparative Election Studies 4 2015")


## -------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls(all.names = T)) # clear memory


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew <- read_stata ("PewResearchGlobalAttitudesSpring2017DatasetWEBFINAL.dta") %>% 
  as_tibble() %>% 
  select(Country,
         dem_stable,
         polsys_autocracy,
         d_ptyid_proximity_turkey,
         d_relig_turkey, 
         sex, age, d_educ_turkey, d_income_turkey, d_income2_turkey,
         QS6TUR) %>%
  filter(Country %in% c("34")) %>% 
  rename(gender=sex, CS_eduCat=d_educ_turkey,
         hh_income=d_income_turkey, hh_incomeBcat = d_income2_turkey,
         Religion=d_relig_turkey,
         UrbanRural=QS6TUR,
         GroupVote=d_ptyid_proximity_turkey) %>%
  mutate(GroupVoteBIN = GroupVote)

colnames(TRpew)
describe(TRpew[,-1])


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("dem_stable","polsys_autocracy",
                               "hh_incomeBcat"),
                     condition = ~.x == 8)
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("dem_stable","polsys_autocracy",
                               "hh_incomeBcat"),
                     condition = ~.x == 9)
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("GroupVote", "GroupVoteBIN", "age", "CS_eduCat", "hh_income"),
                     condition = ~.x == 98)
TRpew <- TRpew %>% 
  replace_with_na_at(.vars = c("GroupVote", "GroupVoteBIN", "age", "CS_eduCat", "hh_income", "Religion"),
                     condition = ~.x == 99)

describe(TRpew [,-1])


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew$GroupVoteBIN  <- car::recode(TRpew$GroupVoteBIN,"1=1; 3=0; 5=0; 10=0;
                                   11=0; 13=0; 14=0; 15=0; 17=0; 18=0; 21=0; 22=0")
TRpew$GroupVoteBIN <- factor(TRpew$GroupVoteBIN, levels=c(0,1), labels = c("nonAKPv", "AKPv"))
tabyl(TRpew$GroupVoteBIN)

# 1=AKP 10=HDP 3=CHP 14=MHP 100=other
TRpew$GroupVote  <- car::recode(TRpew$GroupVote,"1=1; 14=2; 3=3; 10=4;
                                22=100; 17=100; 5=100; 15=100; 21=100; 13=100; 11=100; 18=100")
TRpew$GroupVote <- factor(TRpew$GroupVote, levels = c(1,2,3,4,100), labels = c("AKP","MHP","CHP","HDP","Other"))
tabyl(TRpew$GroupVote) # AKP 0.54 MHP 0.13 CHP 0.19 HDP 0.02 other 0.12


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew$ageI <- cut(TRpew$age, breaks=c(20,30,40,50,60, 70, 80, Inf),
                  labels=c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">80"))
round(prop.table(table(TRpew$ageI)),2)

TRpew$gender <- factor(TRpew$gender, labels = c("Male", "Female"))
round(prop.table(table(TRpew$gender)),2) # M 0.53   F 0.47

round(prop.table(table(TRpew$CS_eduCat)),2)
TRpew$CS_eduCat <- factor(TRpew$CS_eduCat, levels = c(1:11), labels = c("NoFormal","IncompletePri","Primary5", "Primary8",
                                                                        "Complete junior high school","Complete vocational school at junior high",
                                                                        "Complete high school","Complete vocational school at high school",
                                                                        "Uni degree", "Graduate degree", "PhD"))
TRpew$CS_eduCat <- fct_collapse(TRpew$CS_eduCat,
                                NoFormal = c("NoFormal","IncompletePri"),
                                somePrimarySchool = c("Primary5", "Primary8"),
                                someHighSchool = c("Complete junior high school","Complete vocational school at junior high",
                                                   "Complete high school","Complete vocational school at high school"),
                                UniHigher = c("Uni degree", "Graduate degree", "PhD"))
round(prop.table(table(TRpew$CS_eduCat)),2)
tabyl(TRpew$CS_eduCat)

TRpew$hh_incomeCat <- cut(TRpew$hh_income,
                          breaks=c(-Inf, 6, 10, Inf),
                          labels=c("low","medium","high")) # define household income categories
round(prop.table(table(TRpew$hh_incomeCat)),2) # 0.18 low 0.76 medium 0.06 high //roughly correspond to 1000 3000 divide
tabyl(TRpew$hh_incomeCat)

TRpew$hh_incomeBcat <- factor(TRpew$hh_incomeBcat, labels = c("<1750-2000", ">1750-2000")) # define household income categories alternative
round(prop.table(table(TRpew$hh_incomeBcat)),2) # <1750-2000 0.6 >1750-2000 0.4

TRpew$Religion <- factor(TRpew$Religion, levels = c(1,2,3,4,5,6),
                         labels = c("Muslim","Orthodox Christian","Atheist","Agnostic","Something else","Nothing in particular"))
TRpew$Religion <- fct_collapse(TRpew$Religion,
                               Muslim = "Muslim",
                               Other = c("Orthodox Christian","Atheist","Agnostic","Something else","Nothing in particular"))
tabyl(TRpew$Religion)

TRpew$UrbanRural <- factor(TRpew$UrbanRural, levels = c(1,2), labels = c("Urban","Rural"))
tabyl(TRpew$UrbanRural)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
GE_bp <- TRpew %>% select(gender) %>% drop_na() %>%
  ggplot(., aes(x=gender)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Gender") +
  coord_flip()

AG_bp <- TRpew %>% select(ageI) %>% drop_na() %>%
  ggplot(., aes(x=ageI)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Age") +
  coord_flip()

REL_bp <- TRpew %>% select(Religion) %>% drop_na() %>%
  ggplot(., aes(x=Religion)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Religion") +
  coord_flip()

EDU_bp <- TRpew %>% dplyr::select(CS_eduCat) %>% drop_na() %>%
  ggplot(., aes(x=CS_eduCat)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Education") +
  coord_flip()

INC_bp <- TRpew %>% dplyr::select(hh_incomeCat) %>% drop_na() %>%
  ggplot(., aes(x=hh_incomeCat)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Household Income") +
  coord_flip()

UR_bp <- TRpew %>% dplyr::select(UrbanRural) %>% ggplot(., aes(x=UrbanRural)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Urban vs. Rural") +
  coord_flip()

VR_bp <- TRpew %>% dplyr::select(GroupVote) %>% drop_na() %>%
  ggplot(., aes(x=GroupVote)) + 
  geom_bar(aes(y = (..count..)),fill="grey", alpha=0.6) + 
  geom_text(aes(y = (..count..) ,label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black") + 
  theme_bw() +
  xlab("") +
  ylab("Frequency") + 
  labs(title="Feeling close to") +
  coord_flip()

grid.arrange(GE_bp, AG_bp, REL_bp, 
             EDU_bp, INC_bp, UR_bp,
             VR_bp,
             nrow = 3, ncol = 3,
             top = "Sample characteristics in Study 3",
             bottom = textGrob("Source: Pew Global Attitudes 2017
                               \nhttps://www.pewresearch.org/global/dataset/spring-2017-survey-data/
                               \nData Collection: 2017",
                               gp = gpar(fontface = 3, fontsize = 10), just = "center"))

rm(list=setdiff(ls(), "TRpew"))


## -------------------------------------------------------------------------------------------------------------------------------------------------------
round(prop.table(table(TRpew$dem_stable)),2)
TRpew$dem_stable <- factor(TRpew$dem_stable, labels = c("DemGovRPinS","StbGovRnoDem"))
round(prop.table(table(TRpew$dem_stable)),2)
tabyl(TRpew$dem_stable)

# reverse code the following items
round(prop.table(table(TRpew$polsys_autocracy)),2)
TRpew$polsys_autocracy <- car::recode(TRpew$polsys_autocracy, "1=4; 2=3; 3=2; 4=1")
TRpew$polsys_autocracy <- factor(TRpew$polsys_autocracy, labels = c("Very bad","Somewhat bad", "Somewhat good", "Very good"))
round(prop.table(table(TRpew$polsys_autocracy)),2)
tabyl(TRpew$polsys_autocracy)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
TRpew %>%
  tabyl(dem_stable, GroupVoteBIN) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 2) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  kable()

TRpew %>%
  tabyl(polsys_autocracy, GroupVoteBIN) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 2) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  kable()

describe(TRpew [,-1])


## -------------------------------------------------------------------------------------------------------------------------------------------------------
logit_vote_socdem <- glm(GroupVoteBIN ~ gender + age + hh_incomeCat + CS_eduCat,
                         data = TRpew, family = binomial(link="logit"))
summary(logit_vote_socdem)
fmsb::NagelkerkeR2(logit_vote_socdem)

logit_vote <- glm(GroupVoteBIN ~ dem_stable + polsys_autocracy + gender + age + hh_incomeCat + CS_eduCat,
                  data = TRpew, family = binomial(link="logit"))
summary(logit_vote)
fmsb::NagelkerkeR2(logit_vote)

mar_vote <- margins(logit_vote)
mar_vote_sum <- summary(mar_vote)
mar_vote_sum %>% kable(digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
tab_model(logit_vote_socdem, logit_vote,
          show.se = T,
          transform = NULL,
          pred.labels = c("Intercept",
                          "Gender","Age",
                          "Medium-level Income", "High-level Income",
                          "Some primary school completed",
                          "Some high school completed",
                          "University degree & higher",
                          "Preferring a Stable Government with Risk of not being Fully Democratic",
                          "Autocracy Somewhat Bad",
                          "Autocracy Somewhat Good",
                          "Autocracy Very Good"),
          dv.labels = c("Socio-demographics model","... [full] model"),
          string.pred = "Coefficient",
          string.ci = "CI (95%)",
          string.p = "P-Value")


## -------------------------------------------------------------------------------------------------------------------------------------------------------
fit_pred_auth <- ggeffect(logit_vote, terms = "polsys_autocracy")
fit_pred_auth$x <- 1:4
plot(fit_pred_auth) + labs(title = "Probability of feeling closer to AKP by Supporting Autocracy",
                           subtitle = "Note: All other variables held at their mean values",
                           x = "Evaluation of Autocracy in a Political System",
                           y = "Predicted % (Feeling closer to AKP)",
                           caption = "Source: Pew Global Attitudes 2017")


## -------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls(all.names = T)) # clear memory

