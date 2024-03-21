if(!require('dplyr')) install.packages('dplyr'); require(dplyr)
if(!require('binom')) install.packages('binom'); require(binom)
if(!require('questionr')) install.packages('questionr'); require(questionr)
if(!require('wesanderson')) install.packages('wesanderson'); require(wesanderson)
if(!require('ggpubr')) install.packages('ggpubr'); require(ggpubr)
#if(!require('ggstatsplot')) install.packages('ggstatsplot'); require(ggstatsplot)
if(!require('scales')) install.packages('scales'); require(scales)



options(digits=5) # Confirm default digits.
options(scipen=999)# Suppress scientific notation.
options(width=60) # Confirm output width.


getwd() # repertoire courant
repertoire<-"C:/Users/hp/Desktop/koama/database_4"
setwd(repertoire)

mq_femal <- read.csv("Mq_female_male.csv",header=T,dec=".",sep=",")

View(mq_femal)

mq_femal <- mq_femal %>%
  mutate_if(is.character,as.factor)

str(mq_femal)

mq_femal$Treatment <- factor(mq_femal$Treatment,
                         levels = c("ctrl_py","Delta_0,05"),
                         labels = c("Control","Deltametrin 0.05%"),ordered = T)

mq_femal$Mq_strain <- factor(mq_femal$Mq_strain,
                         levels = c("Aedes_aegypti_(borabora)","Aedes_aegypti_(bobo)","An_gambiae_VKLab"),
                         labels = c("Aedes_aegypti_(borabora)","Aedes_aegypti_(bobo)","An.coluzzii"),ordered = T)

#### overall mortality  

mq_femal<-transform(mq_femal,Mrate_24h=(Killed/Nb_mq_tested)*100)

View(mq_femal)



p <- ggboxplot(mq_femal, x = "Treatment", y = "Mortality_rate_24h",
               color = "Treatment", palette = c("#0000FF", "#FC4E07"),
               add = "jitter",
               facet.by = c("Mq_strain","Sex"),
               
                 short.panel.labs = FALSE)
               

p


ggsave("mortality_24h.tiff",
       p, dpi = 500, width = 28,
       height = 28, units = "cm")

mq_femal.sel <- select(mq_femal,
                       Mq_strain,
                       Treatment,
                       Sex,
                       Killed,
                       Nb_mq_tested,
                       Mrate_24h)
dim(mq_femal.sel)
View(mq_femal.sel)

mq_femal.bd_gb <- group_by(mq_femal.sel,
                           Mq_strain,
                           Treatment,
                           Sex)

res1 <- summarise(mq_femal.bd_gb,
                  N = n(),
                  Nb.Tested = sum(Nb_mq_tested),
                  Dead24 = sum(Killed),
                  Mortality_rate_24h= round((Dead24/Nb.Tested)*100,2),
                  SE24=(sd(Mrate_24h)/sqrt(N)))
View(res1) 
str(res1) 
##################################################################################

#res1$Treatment <- factor(res1$Treatment,
#                         levels = c("ctrl_py","Delta_0,05"),
#                        labels = c("Control","Deltametrin 0.05%"),ordered = T)

#res1$Mq_strain <- factor(res1$Mq_strain,
#                         levels = c("Aedes_aegypti_(borabora)","Aedes_aegypti_(bobo)","An_gambiae_VKLab"),
#                        labels = c("Aedes_aegypti_(borabora)","Aedes_aegypti_(bobo)","An.coluzzii"),ordered = T)
##

theme <-  theme_bw()+
          theme(text = element_text(size=10), 
          legend.position="top",
          axis.title.x = element_text(size=10),
          axis.text.x = element_text(angle=0, hjust=.5, vjust=0, size=10),
          axis.text.y = element_text(size=10), title = element_text(size=10), 
          plot.title = element_text(hjust = 0.5), 
          legend.title = element_text(size=10),
          legend.text = element_text(size=10))

View(res1)

Plt_Mortality <- ggplot(res1, aes(Treatment,Mortality_rate_24h,fill=Treatment))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#00AFBB","#FC4E07")) +
  geom_errorbar(aes(ymin = Mortality_rate_24h-SE24, ymax = Mortality_rate_24h+SE24),
                width=.05, size=1, 
                position=position_dodge(.1))+
  theme+
  xlab("Treatments")+
  ylab("% Mortality 24 h after exposure")+
  facet_wrap(Mq_strain~Sex,nrow=3)
Plt_Mortality


ggsave("overall_mortality_sex_2024.png",
       Plt_Mortality, dpi = 500, width = 28,
       height = 18, units = "cm")

#######################################################################
#
# Wing size section
#
#########################################################################

mq_wings <- read.csv("wings.csv",header=T,dec=".",sep=",")


View(mq_wings)
str(mq_wings)

########################################################

mq_wings$treat <- as.character(mq_wings$treatment)
mq_wings$treat[mq_wings$treatment %in% c("deltametrin 0.05%","deltametrin0.05%",
                                         "deltametrin0.05")] <- "Deltametrin 0.05%"
mq_wings$treat[mq_wings$treatment %in% c("contol","ctrl","control")] <- "Control"

table(mq_wings$treat)

####### recoded 

mq_wings$rec_sex <- as.character(mq_wings$sex)
mq_wings$rec_sex[mq_wings$sex %in% c("male","mâle")] <- "male"
mq_wings$rec_sex[mq_wings$sex =="female"] <- "female"

#############################################################
tapply(mq_wings$Length,list(mq_wings$espèce,mq_wings$rec_sex), mean)
tapply(mq_wings$Length,list(mq_wings$espèce,mq_wings$rec_sex), median)

tapply(mq_wings$Length,list(mq_wings$espèce,mq_wings$rec_sex), length)

### aegypti

aedes <- subset(mq_wings,espèce =="aedes")
aedes <- subset(aedes,espèce !="Control")

aedes <- droplevels(aedes)

p <- ggboxplot(aedes, x = "rec_sex", y = "Length",
               color = "rec_sex", palette = "jco",
               add = "jitter")
p
#  Add p-value
p + stat_compare_means()
# Change method
p + stat_compare_means(method = "t.test")
### gambiae


#Change the column name - id to c1

m_wings <- subset(mq_wings,treat !="Control")
m_wings <- droplevels(m_wings)

df <- m_wings %>% 
      select(rec_sex,espèce,Length)%>% 
      rename("Wings_size" = "Length",
             "Species" = "espèce",
             "Sex" = "rec_sex")


df$Species <- factor(df$Species,
                     levels = c("aedes","gambiae"),
                     labels = c("Aedes_aegypti_(bobo)","An.coluzzii"),ordered = T)


p <- ggboxplot(df, x = "Sex", y = "Wings_size",
               color = "Sex", palette = c("#0000FF", "#FC4E07"),
               add = "jitter",
               facet.by = "Species", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
#p + stat_compare_means(label = "p.format")

p
###

ggsave("wing_size.png",
       p, dpi = 500, width = 28,
       height = 18, units = "cm")

################################


mq_wings$espèce <- factor(mq_wings$espèce,
                     levels = c("aedes","gambiae"),
                     labels = c("Aedes_aegypti_(bobo)","An.coluzzii"),
                     ordered = T)


mq_wings$rec_status <- as.character(mq_wings$status)
mq_wings$rec_status[mq_wings$status %in% c("dead","mort")] <- "dead"
mq_wings$rec_status[mq_wings$status %in% c("survival","vivant")] <- "alive"



mq_wings <- mq_wings %>%
  mutate_if(is.character,as.factor)

m_wings <- subset(mq_wings,treat !="Control")
m_wings <- droplevels(m_wings)


############# Aedes 

# male
m_aedes <- subset(m_wings,espèce =="Aedes_aegypti_(bobo)" & rec_sex =="male")
compare_means(Length ~ rec_status, data = m_aedes,exact = FALSE)

#female
f_aedes <- subset(m_wings,espèce =="Aedes_aegypti_(bobo)" & rec_sex =="female")
compare_means(Length ~ rec_status, data = f_aedes,exact = FALSE)

#female # male

ads <- subset(m_wings,espèce =="Aedes_aegypti_(bobo)")
ads <- droplevels(ads)
compare_means(Length ~ rec_sex, data = ads,exact = FALSE)

te <- ads %>%
      select(Length,rec_sex)

View(te)

tapply(ads$Length,list(ads$espèce,ads$rec_sex), mean)
tapply(ads$Length,list(ads$espèce,ads$rec_sex), sd)


res_bsex <- wilcox.test(Length ~ rec_sex, data = ads, exact = FALSE)
res_bsex$statistic
res_bsex$p.value


###### both sex

res_bsex <- wilcox.test(Length ~ rec_sex, data = m_wings, exact = FALSE)
res_bsex

###### death and alive

res_da <- wilcox.test(Length ~ rec_status, data = m_wings, exact = FALSE)
res_da

############ Coluzzii 

# male
m_colu <- subset(m_wings,espèce =="An.coluzzii" & rec_sex =="male")
compare_means(Length ~ rec_status, data = m_colu,exact = FALSE)

#female
f_colu <- subset(m_wings,espèce =="An.coluzzii" & rec_sex =="female")
compare_means(Length ~ rec_status, data = f_colu,exact = FALSE)

#female # male 
col <- subset(m_wings,espèce =="An.coluzzii")
col <- droplevels(col)
compare_means(Length ~ rec_sex, data = col,exact = FALSE)

tapply(col$Length,list(col$espèce,col$rec_sex), mean)
tapply(col$Length,list(col$espèce,col$rec_sex), sd)

res_bsex <- wilcox.test(Length ~ rec_sex, data = col, exact = FALSE)
res_bsex


######

tapply(mq_wings$Length,list(mq_wings$espèce,mq_wings$rec_status,mq_wings$rec_sex), mean)
tapply(mq_wings$Length,list(mq_wings$espèce,mq_wings$rec_status,mq_wings$rec_sex), sd)
tapply(mq_wings$Length,list(mq_wings$espèce,mq_wings$rec_status,mq_wings$rec_sex), median)

tapply(mq_wings$Length,list(mq_wings$espèce,mq_wings$rec_status,mq_wings$rec_sex), length)
############################################################################################


