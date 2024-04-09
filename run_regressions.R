library(readxl)
library(data.table)
library(irr)
library(ggplot2)
library(cowplot)

## LOAD DATA ##
dat_raw <- as.data.table(read_xlsx("rawdata.xlsx",skip = 1))

# Change column names
setnames(dat_raw,
         c("Code...12","rK39...13","DAT...14","Sandfly saliva","PCR T0","Code...18","rK39...19","DAT...20","Sandfly salaiva","PCR T1"),
         c("Code_T1","rK39_T1","DAT_T1","SS_T1","PCR_T1","Code_T2","rK39_T2","DAT_T2","SS_T2","PCR_T2"))
setnames(dat_raw, "1st time", "FC")
setnames(dat_raw, "multiple time in years", "multiple_time_in_years")
setnames(dat_raw, "come to korhumer via delelo(d) or abdurafi (ab)", "route_to_korhumer")

# Number of participants
nrow(dat_raw)

# Check ID codes
dat_raw[,sum(code == `...2`)]
View(dat_raw[code != `...2`])
# Drop first column as it seems to have an error in the codes
dat_raw[,code := NULL]
setnames(dat_raw,"...2","code")

dat_raw[,sum(code == Code_T1)]
dat_raw[,sum(!is.na(Code_T2))]
dat_raw[,sum(code == Code_T2, na.rm = T)]

# All T1 and T2 codes match, so drop Code_T1 and Code_T2
dat_raw[,c("Code_T1","Code_T2") := NULL]

# Drop ...17 as it's a blank column
dat_raw[,sum(is.na(`...17`))]
dat_raw[,`...17` := NULL]

## CLEAN DATA ##
dat_raw[,age := as.integer(age)]
dat_raw[sex == "F",sex := "f"]
dat_raw[sex == "no", sex := ""]
dat_raw[,sex := fifelse(sex == "m", 1, 0)]
dat_raw[,table(sex, useNA = "always")]

dat_raw[occupation %in% c("constraction",
                          "contraction worker",
                          "Contruction worker"),
        occupation := "construction"]
dat_raw[occupation %in% c("daily laborao",
                          "daily laboror"),
        occupation := "daily labourer"]
dat_raw[occupation == "Farmer",
        occupation := "farmer"]
dat_raw[occupation == "trade",
        occupation := "trader"]
dat_raw[,table(occupation)]

dat_raw[,occupation1 := occupation]
dat_raw[!(occupation1 %in% c("farmer","student")),
        occupation1 := "other"]

dat_raw[,table(district)]
dat_raw[,endemic := 
            fifelse(district %in% 
                        c("aderemet","belessa","ebnat",
                          "fogera","gondar city","libokemkem",
                          "metema","sanja","sekota"
                          ),1,0)]
dat_raw[is.na(district),endemic := NA]
dat_raw[,table(endemic,useNA = "always")]

dat_raw[,FC := fifelse(FC == "yes", 1, 0)]
dat_raw[FC == 1, multiple_time_in_years := 0]
dat_raw[,table(route_to_korhumer, useNA = "always")]
dat_raw[,route_to_korhumer := factor(route_to_korhumer,levels = c("d","ab"))]
dat_raw[,height := as.numeric(height)]
dat_raw[,bmi := as.numeric(bmi)]

dat_raw[muac %in% c("no","not done"),muac := NA]
dat_raw[,muac := as.numeric(muac)]
dat_raw[muac == 557, muac := NA]

# # Leave cleaning of knowledge and behaviour variables till later
dat_raw[,table(Symptoms, useNA = "always")]
dat_raw[,symptoms := fifelse(Symptoms %in% c("no","yes, caused by  genital sore"),0,1)]
dat_raw[is.na(Symptoms), symptoms := NA]
dat_raw[,table(symptoms, useNA = "always")]
dat_raw[,Symptoms := NULL]
dat_raw[,table(`ka transmission`, useNA = "always")]
dat_raw[,ka_transmission := 
            fifelse(`ka transmission` %in% 
                        c("no","No",
                          "drinking dirty water or eating  contaminated food with TB",
                          "yes, caused by contaminated food and eating cold food",
                          "yes, caused by contaminated food and sleeping on dirty floor",
                          "yes, caused by eating dirty sugar cane","yes, cuased by  dirty water",
                          "yes, cuased by sexual intercourse"),0,1)]
dat_raw[is.na(`ka transmission`), ka_transmission := NA]
dat_raw[,table(ka_transmission, useNA = "always")]
dat_raw[,`ka transmission` := NULL]
dat_raw[,prevention := fifelse(!is.na(prevention),fifelse(prevention %in% c("no","yes, no sex"), 0, 1), NA_integer_)]
dat_raw[,table(prevention,useNA = "always")]
dat_raw[,treatment := fifelse(`treatment if yes how` == "no", 0, 1)]
dat_raw[,table(treatment, useNA = "always")]
dat_raw[,`treatment if yes how` := NULL]
# # BLANKS FOR VARIABLES BELOW ARE FOR FIRST COMERS AS THEY WERE NOT ASKED THESE QS
# # SO THESE VARIABLES ARE UNUSABLE
# dat_raw[,table(`animals at your sleep`, useNA = "always")]
# dat_raw[,table(trees, useNA = "always")]
# dat_raw[,table(`sleep outside`, useNA = "always")]
# dat_raw[,table(`bed net utilization`, useNA = "always")]
# dat_raw[,table(`repellents utilization`, useNA = "always")]

dat_raw[DAT_T2 == "1:1600", DAT_T2 := "(1:1600)"]
dat_raw[DAT_T2 %in% c("( 1:3200)","1:3200"), DAT_T2 := "(1:3200)"]

# Binary variables for T1 tests
dat_raw[,DAT_pos_T1 := 
            fifelse(!is.na(DAT_T1),
                    fifelse(DAT_T1 %in% c("neg","(1:400)","(1:800)"),0,1),
                    NA_integer_)]
dat_raw[,rK39_pos_T1 := fifelse(rK39_T1 == "pos",1,0)]
dat_raw[,PCR_pos_T1 := 
            fifelse(!is.na(PCR_T1),
                    fifelse(PCR_T1 == "N/A",0,1),
                    NA_integer_)]
dat_raw[,SS_pos_T1 := fifelse(SS_T1 > 0.105, 1, 0)]

# Binary variables for T2 tests
dat_raw[,DAT_pos_T2 := 
            fifelse(!is.na(DAT_T2),
                    fifelse(DAT_T2 %in% c("neg","(1:400)","(1:800)"),0,1),
                    NA_integer_)]
dat_raw[,rK39_pos_T2 := fifelse(rK39_T2 == "pos",1,0)]
dat_raw[,PCR_pos_T2 := 
            fifelse(!is.na(PCR_T2),
                    fifelse(PCR_T2 == "N/A",0,1),
                    NA_integer_)]
dat_raw[,SS_pos_T2 := fifelse(SS_T2 > 0.105, 1, 0)]

## EXAMINE TEST DATA
# Numbers tested with different tests at different timepoints
cols <- c("rK39_T1","rK39_T2","SS_T1","PCR_T1","DAT_T1","DAT_T2","SS_T2","PCR_T2")
dat_raw[,lapply(.SD,function(x) sum(x != "", na.rm = T)), 
        .SDcols = cols]
dat_raw[,lapply(.SD,function(x) table(x, useNA = "always")),.SDcols = c("rK39_T1","rK39_T2")]
dat_raw[,table(rK39_pos_T1, useNA = "always")]
dat_raw[,table(FC,rK39_pos_T1, useNA = "always")]
fisher.test(dat_raw[,table(FC,rK39_pos_T1)])
dat_raw[,table(rK39_pos_T2, useNA = "always")]
dat_raw[,table(FC,rK39_pos_T2, useNA = "always")]
fisher.test(dat_raw[,table(FC,rK39_pos_T2)])
dat_raw[!rK39_pos_T1 & !is.na(rK39_pos_T2),table(FC,rK39_pos_T2)]
dat_raw[,table(DAT_T1, useNA = "always")]
dat_raw[,table(DAT_pos_T1, useNA = "always")]
dat_raw[,table(FC,DAT_pos_T1, useNA = "always")]
fisher.test(dat_raw[,table(FC,DAT_pos_T1)])
dat_raw[,table(DAT_T2, useNA = "always")]
dat_raw[,table(DAT_pos_T2, useNA = "always")]
dat_raw[,table(FC,DAT_pos_T2, useNA = "always")]
fisher.test(dat_raw[,table(FC,DAT_pos_T2)])
dat_raw[!DAT_pos_T1 & !is.na(DAT_pos_T2),table(FC,DAT_pos_T2)]
fisher.test(dat_raw[!DAT_pos_T1 & !is.na(DAT_pos_T2),table(FC,DAT_pos_T2)])
dat_raw[,summary(SS_T1)]
dat_raw[,table(SS_pos_T1, useNA = "always")]
dat_raw[,summary(SS_T2)]
dat_raw[,table(SS_pos_T2, useNA = "always")]
dat_raw[,table(PCR_T1, useNA = "always")]
dat_raw[,table(PCR_pos_T1, useNA = "always")]
dat_raw[,table(FC,PCR_pos_T1, useNA = "always")]
fisher.test(dat_raw[,table(FC,PCR_pos_T1)])
dat_raw[,table(PCR_T2, useNA = "always")]
dat_raw[,table(PCR_pos_T2, useNA = "always")]
dat_raw[,table(FC,PCR_pos_T2, useNA = "always")]
fisher.test(dat_raw[,table(FC,PCR_pos_T2)])
dat_raw[!PCR_pos_T1 & !is.na(PCR_pos_T2),table(FC,PCR_pos_T2)]
fisher.test(dat_raw[!PCR_pos_T1 & !is.na(PCR_pos_T2),table(FC,PCR_pos_T2)])

p1 <- ggplot(dat_raw[!is.na(FC) & !is.na(DAT_T1),.(number = .N), by = .(FC,DAT_T1)][
    ,`:=`(FC = factor(FC, levels = c("1", "0")), 
          DAT_T1 = factor(DAT_T1, levels = c("neg","(1:400)","(1:800)","(1:1600)","(1:3200)","(1:6400)","(1:12800)","(1:25600)","(1:51200)")))], 
             aes(x = DAT_T1, y = number, group = factor(FC), fill = factor(FC))) + 
    geom_col(position = position_dodge()) + 
    scale_fill_discrete(name = "", labels = c("FC","RC")) + 
    labs(y = "Frequency") +
    theme_cowplot(font_size = 10)
ggsave("DAT_T1_distn.pdf", p1, width = 5, height = 3)

# Try different codings of asympomatic infection at T1
# # rK39 positive only
# dat_raw[,AS := fifelse(rK39_T1 == "pos", 1, 0)]
# dat_raw[,table(AS, useNA = "always")]

# # DAT positive only
# dat_raw[,AS := fifelse(DAT_T1 %in% c("neg","(1:400)","(1:800)"), 0, 1)]
# dat_raw[is.na(DAT_T1),AS := NA]
# dat_raw[,table(AS, useNA = "always")]

# rK39 or DAT positive
dat_raw[,AS := fifelse(!is.na(DAT_T1) & !is.na(rK39_T1), 
                       fifelse(DAT_T1 %in% c("neg","(1:400)","(1:800)") & 
                                   rK39_T1 == "neg", 0, 1), NA_integer_)]
dat_raw[,table(AS, useNA = "always")]
dat_raw[,table(FC, AS, useNA = "always")]
fisher.test(dat_raw[,table(FC, AS)])

# T2
dat_raw[,AS_T2 := fifelse(!is.na(DAT_T2) & !is.na(rK39_T2), 
                          fifelse(DAT_T2 %in% c("neg","(1:400)","(1:800)") & 
                                      rK39_T2 == "neg", 0, 1), NA_integer_)]
dat_raw[,table(AS_T2, useNA = "always")]
dat_raw[,table(FC, AS_T2, useNA = "always")]
fisher.test(dat_raw[,table(FC, AS_T2)])

# Create function for doing logistic regressions and printing ORs
fit_glm <- function(formula, data){
    mod <- glm(formula, family = binomial, data)
    print(summary(mod))
    print(exp(cbind(coef(mod), confint.default(mod))))
    return(mod)
}

## LOGISTIC REGRESSIONS ##
## Asymptomatic infection at T1
# Univariable
# Age
mod1a <- fit_glm(AS ~ age, dat_raw)

# Sex
mod1b <- fit_glm(AS ~ sex, dat_raw)

# Occupation
mod1c <- fit_glm(AS ~ occupation1, dat_raw)

# First comers
dat_raw[,table(FC, AS)]
mod1d <- fit_glm(AS ~ FC, dat_raw)

# No. of years coming
ggplot(dat_raw[!is.na(AS),], aes(x = as.factor(AS), y = multiple_time_in_years)) + 
    geom_boxplot() + 
    labs(x = "AS", y = "No. of years coming")
mod1e <- fit_glm(AS ~ multiple_time_in_years, dat_raw)

# Route to Korhumer
mod1f <- fit_glm(AS ~ route_to_korhumer, dat_raw)

# Knowledge of VL symptoms
mod1g <- fit_glm(AS ~ symptoms, dat_raw)

# Knowledge of VL transmission
mod1h <- fit_glm(AS ~ ka_transmission, dat_raw)

# Knowledge of VL prevention
mod1i <- fit_glm(AS ~ prevention, dat_raw)

# Knowledge of VL treatment
mod1j <- fit_glm(AS ~ treatment, dat_raw)

# Height
mod1k <- fit_glm(AS ~ height, dat_raw)

# Weight
mod1l <- fit_glm(AS ~ weight, dat_raw)

# BMI
ggplot(dat_raw[!is.na(AS),], aes(x = as.factor(AS), y = bmi)) + 
    geom_boxplot() + 
    labs(x = "AS", y = "BMI")
mod1m <- fit_glm(AS ~ bmi, dat_raw)

# MUAC
ggplot(dat_raw[!is.na(AS),], aes(x = as.factor(AS), y = muac)) + 
    geom_boxplot() + 
    labs(x = "AS", y = "MUAC")
mod1n <- fit_glm(AS ~ muac, dat_raw)

# Coming from endemic district
mod1o <- fit_glm(AS ~ endemic, dat_raw)

# Sandfly saliva antibody positive 
mod1p <- fit_glm(AS ~ SS_pos_T1, dat_raw)

# Multivariable
# Make data table of individuals with all risk factors and asymptomatic status recorded
dat_all <- na.omit(dat_raw, cols = c("age","sex","occupation1",
                                     "multiple_time_in_years",
                                     "route_to_korhumer",
                                     "symptoms","ka_transmission","prevention",
                                     "treatment","height","weight","bmi","muac",
                                     "endemic","AS"))
    
# Start with full model and do backwards selection
mod2 <- fit_glm(AS ~ age + sex + occupation1 + multiple_time_in_years + route_to_korhumer + symptoms + ka_transmission + prevention + treatment + height + weight + bmi + muac + endemic, dat_all)

# Drop (knowledge of) prevention
mod2a <- fit_glm(AS ~ age + sex + occupation1 + multiple_time_in_years + route_to_korhumer + symptoms + ka_transmission + treatment + height + weight + bmi + muac + endemic, dat_all)

# Drop (knowledge of) treatment
mod2b <- fit_glm(AS ~ age + sex + occupation1 + multiple_time_in_years + route_to_korhumer + symptoms + ka_transmission + height + weight + bmi + muac + endemic, dat_all)

# Drop sex
mod2c <- fit_glm(AS ~ age + occupation1 + multiple_time_in_years + route_to_korhumer + symptoms + ka_transmission + height + weight + bmi + muac + endemic, dat_all)

# Drop muac
mod2d <- fit_glm(AS ~ age + occupation1 + multiple_time_in_years + route_to_korhumer + symptoms + ka_transmission + height + weight + bmi + endemic, dat_all)

# Drop occupation
mod2e <- fit_glm(AS ~ age + multiple_time_in_years + height + weight + bmi + endemic, dat_all)

# Drop endemic
mod2f <- fit_glm(AS ~ age + multiple_time_in_years + height + weight + bmi, dat_all)

# Drop weight
mod2g <- fit_glm(AS ~ age + multiple_time_in_years + height + bmi, dat_all)

# Drop height
mod2h <- fit_glm(AS ~ age + multiple_time_in_years + bmi, dat_all)

# Drop bmi
mod2i <- fit_glm(AS ~ age + multiple_time_in_years, dat_all)

# Check intermediate model with bmi and without height and weight
mod2j <- fit_glm(AS ~ age + multiple_time_in_years + bmi + endemic, dat_all)
# Inclusion of height and weight doesn't seem to affect model selection

## Seroconversion between T1 and T2
dat_raw[,seroconversion := 
            fifelse(DAT_T1 %in% c("neg","(1:400)","(1:800)") & rK39_T1 == "neg" & !is.na(DAT_T2) & !is.na(rK39_T2),
                        fifelse(rK39_T2 == "pos" | DAT_T2 %in% c("(1:1600)","(1:3200)","(1:6400)","(1:12800)","(1:25600)","(1:51200)"), 1, 0),
                    NA_integer_)]
dat_raw[,table(seroconversion, useNA = "always")]
# Check against asymptomatic infection at T1
dat_raw[,table(AS,seroconversion, useNA = "always")]

dat_raw[,table(FC, seroconversion, useNA = "always")]
fisher.test(dat_raw[,table(FC, seroconversion)])

# Univariable
# Age
mod3a <- fit_glm(seroconversion ~ age, dat_raw)

# Sex
mod3b <- fit_glm(seroconversion ~ sex, dat_raw)

# Occupation
mod3c <- fit_glm(seroconversion ~ occupation1, dat_raw)

# First comers
dat_raw[,table(FC, seroconversion)]
mod3d <- fit_glm(seroconversion ~ FC, dat_raw)

# No. of years coming
ggplot(dat_raw[!is.na(seroconversion),], aes(x = as.factor(seroconversion), y = multiple_time_in_years)) + 
    geom_violin() + 
    labs(x = "seroconversion", y = "No. of years coming")
mod3e <- fit_glm(seroconversion ~ multiple_time_in_years, dat_raw)

# Route to Korhumer
dat_raw[,table(route_to_korhumer,seroconversion)]
mod3f <- fit_glm(seroconversion ~ route_to_korhumer, dat_raw)

# Knowledge of VL symptoms
mod3g <- fit_glm(seroconversion ~ symptoms, dat_raw)

# Knowledge of VL transmission
mod3h <- fit_glm(seroconversion ~ ka_transmission, dat_raw)

# Knowledge of VL prevention
mod3i <- fit_glm(seroconversion ~ prevention, dat_raw)

# Knowledge of VL treatment
mod3j <- fit_glm(seroconversion ~ treatment, dat_raw)

# Height
mod3k <- fit_glm(seroconversion ~ height, dat_raw)

# Weight
mod3l <- fit_glm(seroconversion ~ weight, dat_raw)

# BMI
ggplot(dat_raw[!is.na(seroconversion),], aes(x = as.factor(seroconversion), y = bmi)) + 
    geom_boxplot() + 
    labs(x = "seroconversion", y = "BMI")
mod3m <- fit_glm(seroconversion ~ bmi, dat_raw)

# MUAC
ggplot(dat_raw[!is.na(seroconversion),], aes(x = as.factor(seroconversion), y = muac)) + 
    geom_boxplot() + 
    labs(x = "seroconversion", y = "MUAC")
mod3n <- fit_glm(seroconversion ~ muac, dat_raw)

# Coming from endemic district
mod3o <- fit_glm(seroconversion ~ endemic, dat_raw)

# # Sandfly saliva antibody positive 
# mod3p <- fit_glm(seroconversion ~ SS_pos_T1, dat_raw)

# dat_raw[,seroconversion1 := 
#             fifelse(DAT_T1 %in% c("neg","(1:400)","(1:800)") & !is.na(DAT_T2),
#                     fifelse(DAT_T2 %in% c("neg","(1:400)","(1:800)"),0,1),
#                     NA_integer_)]
# dat_raw[,table(seroconversion1, useNA = "always")]

# Make data table of individuals with all risk factors and asymptomatic status recorded
dat_all_sero <- na.omit(dat_raw, cols = c("age","sex","occupation1",
                                          "multiple_time_in_years",
                                          "route_to_korhumer",
                                          "symptoms","ka_transmission","prevention",
                                          "treatment","height","weight","bmi","muac",
                                          "endemic","seroconversion"))

# Model with factors closest to being significant from univariable regressions
mod4a <- fit_glm(seroconversion ~ route_to_korhumer + bmi, dat_all_sero)

# Drop bmi
mod4b  <- fit_glm(seroconversion ~ route_to_korhumer, dat_all_sero)


## TEST AGREEMENT ##
# Pairwise two-way agreement
calculate_cohens_kappa1 <- function(dt, x, y){
    print(table(dt[, c(x,y), with = F]))
    kappa <- kappa2(dt[, c(x,y), with = F])
    print(kappa)
    return(kappa)
}

calculate_cohens_kappa <- function(dt, x, y){
    kappas <- mapply(calculate_cohens_kappa1, x = x, y = y, 
                  MoreArgs = list(dt = dt), SIMPLIFY = F, USE.NAMES = F)
    names(kappas) <- paste(x, "vs", y)
    return(kappas)
}

dat_raw_FC <- dat_raw[!is.na(FC)]

## T1
cols1 <- c("rK39_pos_T1","rK39_pos_T1","DAT_pos_T1")
cols2 <- c("DAT_pos_T1","PCR_pos_T1","PCR_pos_T1")
kappas_T1 <- calculate_cohens_kappa(dat_raw_FC, cols1, cols2)
write.csv(do.call(cbind, kappas_T1), "kappas_T1.csv", row.names = F)

# FC
kappas_FC_T1 <- calculate_cohens_kappa(dat_raw_FC[FC == 1], cols1, cols2)
write.csv(do.call(cbind, kappas_FC_T1), "kappas_FC_T1.csv", row.names = F)

# RC
kappas_RC_T1 <- calculate_cohens_kappa(dat_raw_FC[FC == 0], cols1, cols2)
write.csv(do.call(cbind, kappas_RC_T1), "kappas_RC_T1.csv", row.names = F)

## T2
cols3 <- c("rK39_pos_T2","rK39_pos_T2","DAT_pos_T2")
cols4 <- c("DAT_pos_T2","PCR_pos_T2","PCR_pos_T2")
kappas_T2 <- calculate_cohens_kappa(dat_raw_FC, cols3, cols4)
write.csv(do.call(cbind, kappas_T2), "kappas_T2.csv", row.names = F)

# FC
kappas_FC_T2 <- calculate_cohens_kappa(dat_raw_FC[FC == 1], cols3, cols4)
write.csv(do.call(cbind, kappas_FC_T2), "kappas_FC_T2.csv", row.names = F)

# RC
kappas_RC_T2 <- calculate_cohens_kappa(dat_raw_FC[FC == 0], cols3, cols4)
write.csv(do.call(cbind, kappas_RC_T2), "kappas_RC_T2.csv", row.names = F)

# Three-way agreement
calculate_fleiss_kappa <- function(dt,cols){
    return(kappam.fleiss(dt[, cols, with = F]))
}

## T1
cols_T1 <- c("rK39_pos_T1", "DAT_pos_T1", "PCR_pos_T1")
kappa_fleiss_T1 <- calculate_fleiss_kappa(dat_raw_FC, cols_T1)
write.csv(do.call(cbind, kappa_fleiss_T1), "kappa_fleiss_T1.csv", row.names = F)

# FC
kappa_fleiss_FC_T1 <- calculate_fleiss_kappa(dat_raw_FC[FC == 1], cols_T1)
write.csv(do.call(cbind, kappa_fleiss_FC_T1), "kappa_fleiss_FC_T1.csv", row.names = F)

# RC
kappa_fleiss_RC_T1 <- calculate_fleiss_kappa(dat_raw_FC[FC == 0], cols_T1)
write.csv(do.call(cbind, kappa_fleiss_RC_T1), "kappa_fleiss_RC_T1.csv", row.names = F)

## T2
cols_T2 <- c("rK39_pos_T2", "DAT_pos_T2", "PCR_pos_T2")
kappa_fleiss_T2 <- calculate_fleiss_kappa(dat_raw_FC, cols_T2)
write.csv(do.call(cbind, kappa_fleiss_T2), "kappa_fleiss_T2.csv", row.names = F)

# FC
kappa_fleiss_FC_T2 <- calculate_fleiss_kappa(dat_raw_FC[FC == 1], cols_T2)
write.csv(do.call(cbind, kappa_fleiss_FC_T2), "kappa_fleiss_FC_T2.csv", row.names = F)

# RC
kappa_fleiss_RC_T2 <- calculate_fleiss_kappa(dat_raw_FC[FC == 0], cols_T2)
write.csv(do.call(cbind, kappa_fleiss_RC_T2), "kappa_fleiss_RC_T2.csv", row.names = F)
