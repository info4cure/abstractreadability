# --- 
# title: "Systematic reviews of best methodological quality achieve low altmetrics when their abstracts are non-structured, low readable, and of poor reporting completeness" 
# author: "Juan Ruano" 
# date: "12 Mmay 2017" 
# institutions: Department of Dermatology, IMIBIC/Reina Sofia University Hospital/University of Cordoba, Cordoba, Spain
# analysis: 01_abstract and full text readability
# --- 
# 
# R version 3.3.1 (2016-06-21)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: OS X 10.9.5 (Mavericks)


######## 0: packages ----------------

library(tidyverse)

########  1: reading .csv files ----------------
setwd("~/Documents/mac book air carpetas/juanruanoruiz/Documents/Dermatología/investigacion/grupo_emergente_IMIBIC/nuevos proyectos/calidad de revisiones sistemáticas/articulo_MFA_journals_grupos")
DB1<-read_csv("readability_file.csv")
names(DB1)

########  2: reading .csv files ----------------
by_year_all <- group_by(DB1, year)
indices_by_year_all<- summarise(by_year_all, 
                    Flesch_Kincaid_Reading_Ease_Level = mean(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = mean(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = mean(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = mean(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = mean(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = mean(Average_Grade_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score = mean(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = mean(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = mean(New_Dale_Chall_Score, na.rm = TRUE),
                    Reading_Time = mean(Reading_Time, na.rm = TRUE),
                    Speaking_Time = mean(Speaking_Time, na.rm = TRUE),
                    total_score = mean(total_score, na.rm = TRUE)
                    )
table_indices_by_year_all<- indices_by_year_all %>% 
  gather(-year, key = "indice", value = "value")
indices_all <- ggplot(table_indices_by_year_all, aes(year, value, color=indice)) +
  geom_line()+
  xlim(1997, 2017) +
  geom_smooth()+
  theme(legend.position="none")+
  geom_point()+
  facet_wrap(~indice, scales = "free")


#########  BY MEDIAN
indices_by_year_all<- summarise(by_year_all, 
                    Flesch_Kincaid_Reading_Ease_Level = median(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = median(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = median(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = median(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = median(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = median(Average_Grade_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score = median(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = median(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = median(New_Dale_Chall_Score, na.rm = TRUE),
                    Reading_Time = median(Reading_Time, na.rm = TRUE),
                    Speaking_Time = median(Speaking_Time, na.rm = TRUE),
                    total_score = median(total_score, na.rm = TRUE)
                    )
table_indices_by_year_all<- indices_by_year_all %>% 
  gather(-year, key = "indice", value = "value")
indices_all <- ggplot(table_indices_by_year_all, aes(year, value, color=indice)) +
  geom_line()+
  xlim(1997, 2017) +
  geom_smooth()+
  theme(legend.position="none")+
  geom_point()+
  facet_wrap(~indice, scales = "free")

#########  boxplot all data BY year
indices_by_year_all<- summarise(by_year_all, 
                    Flesch_Kincaid_Reading_Ease_Level = median(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = median(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = median(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = median(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = median(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = median(Average_Grade_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score = median(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = median(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = median(New_Dale_Chall_Score, na.rm = TRUE),
                    Reading_Time = median(Reading_Time, na.rm = TRUE),
                    Speaking_Time = median(Speaking_Time, na.rm = TRUE),
                    total_score = median(total_score, na.rm = TRUE)
                    )
table_indices_all<- DB1[,c(1,6,8:18)] %>% 
  gather(key , value, -article_id, -AMSTAR_levels)

indices_boxplots<- ggplot(table_indices_all, aes(key, log(abs(value),2), color=AMSTAR_levels)) +
  geom_boxplot(aes(key, log(abs(value),2), color=AMSTAR_levels),outlier.alpha = 0.3)+
  #geom_point(position = position_dodge(width = 0.75)) +
  theme(legend.position="none")+
  facet_wrap(~key, scales = "free")





### all reviews
by_year <- group_by(DB1, year)

### AMSTAR_levels == "high_quality"
high_quality <- filter(DB1, AMSTAR_levels == "high_quality") 
by_year_high <- group_by(high_quality, year)
indices_by_year_high <- summarise(by_year_high, 
                    Flesch_Kincaid_Reading_Ease_Level = mean(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = mean(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = mean(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = mean(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = mean(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = mean(Average_Grade_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score = mean(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = mean(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = mean(New_Dale_Chall_Score, na.rm = TRUE),
                    Reading_Time = mean(Reading_Time, na.rm = TRUE),
                    Speaking_Time = mean(Speaking_Time, na.rm = TRUE),
                    total_score = mean(total_score, na.rm = TRUE)
                    )


table_indice_by_year_high <- indices_by_year_high %>% 
  gather(-year, key = "indice", value = "value")
indices_high <- ggplot(table_indice_by_year_high, aes(year, value, color=indice)) +
  geom_line()+
  xlim(1997, 2017) +
  geom_smooth()+
  theme(legend.position="none")+
  geom_point()+
  facet_wrap(~indice, scales = "free")

indices_by_year_high_2 <- summarise(by_year_high, 
                    Flesch_Kincaid_Reading_Ease_Level = mean(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = mean(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = mean(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = mean(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = mean(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = mean(Average_Grade_Level, na.rm = TRUE),
                    # Flesch_Kincaid_Reading_Ease_Score = mean(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = mean(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = mean(New_Dale_Chall_Score, na.rm = TRUE),
                    # Reading_Time = mean(Reading_Time, na.rm = TRUE),
                    # Speaking_Time = mean(Speaking_Time, na.rm = TRUE),
                    total_score = mean(total_score, na.rm = TRUE)
                    )

table_indices_by_year_high_2 <- indices_by_year_high_2 %>% 
  gather(-year, key = "indice", value = "value")
indices_high_2 <- ggplot(table_indices_by_year_high_2, aes(year, value, color=indice)) +
  geom_line()+
  xlim(1997, 2017) +
  ylim(0, 25) +
  geom_hline(yintercept = c(10,15,20), color="gray", linetype=2)+
  geom_smooth()+
  theme(legend.position="none")+
  geom_point()+
  facet_wrap(~indice, scales = "free")

### AMSTAR_levels == "moderate_quality"
moderate_quality <- filter(DB1, AMSTAR_levels == "moderate_quality")
by_year_moderate <- group_by(moderate_quality, year)
indices_by_year_moderate <- summarise(by_year_moderate, 
                    Flesch_Kincaid_Reading_Ease_Level = mean(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = mean(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = mean(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = mean(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = mean(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = mean(Average_Grade_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score = mean(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = mean(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = mean(New_Dale_Chall_Score, na.rm = TRUE),
                    Reading_Time = mean(Reading_Time, na.rm = TRUE),
                    Speaking_Time = mean(Speaking_Time, na.rm = TRUE),
                    total_score = mean(total_score, na.rm = TRUE)
                    )

table_indice_by_year_moderate <- indices_by_year_moderate %>% 
  gather(-year, key = "indice", value = "value")
indices_moderate <- ggplot(table_indice_by_year_moderate, aes(year, value, color=indice)) +
  geom_line(linetype=3)+
  xlim(1997, 2017) +
  geom_smooth()+
  theme(legend.position="none")+
  geom_point()+
  facet_wrap(~indice, scales = "free")

indices_by_year_moderate_2 <- summarise(by_year_moderate, 
                    Flesch_Kincaid_Reading_Ease_Level = mean(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = mean(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = mean(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = mean(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = mean(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = mean(Average_Grade_Level, na.rm = TRUE),
                    # Flesch_Kincaid_Reading_Ease_Score = mean(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = mean(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = mean(New_Dale_Chall_Score, na.rm = TRUE),
                    # Reading_Time = mean(Reading_Time, na.rm = TRUE),
                    # Speaking_Time = mean(Speaking_Time, na.rm = TRUE),
                    total_score = mean(total_score, na.rm = TRUE)
                    )

table_indices_by_year_moderate_2 <- indices_by_year_moderate_2 %>% 
  gather(-year, key = "indice", value = "value")
indices_moderate_2 <- ggplot(table_indices_by_year_moderate_2, aes(year, value, color=indice)) +
  geom_line()+
  xlim(1997, 2017) +
  ylim(0, 25) +
  geom_hline(yintercept = c(10,15,20), color="gray", linetype=2)+
  geom_smooth()+
  theme(legend.position="none")+
  geom_point()+
  facet_wrap(~indice, scales = "free")



### AMSTAR_levels == "low_quality"
low_quality <- filter(DB1, AMSTAR_levels == "low_quality")
by_year_low <- group_by(low_quality, year)
indices_by_year_low <- summarise(by_year_low, 
                    Flesch_Kincaid_Reading_Ease_Level = mean(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = mean(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = mean(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = mean(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = mean(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = mean(Average_Grade_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score = mean(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = mean(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = mean(New_Dale_Chall_Score, na.rm = TRUE),
                    Reading_Time = mean(Reading_Time, na.rm = TRUE),
                    Speaking_Time = mean(Speaking_Time, na.rm = TRUE),
                    total_score = mean(total_score, na.rm = TRUE)
                    )

table_indice_by_year_low<- indices_by_year_low %>% 
  gather(-year, key = "indice", value = "value")
indices_low <- ggplot(table_indice_by_year_low, aes(year, value, color=indice)) +
  geom_line(linetype=2)+
  xlim(1997, 2017) +
  geom_smooth()+
  theme(legend.position="none")+
  geom_point()+
  facet_wrap(~indice, scales = "free")

indices_by_year_low_2 <- summarise(by_year_low, 
                    Flesch_Kincaid_Reading_Ease_Level = mean(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = mean(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = mean(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = mean(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = mean(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = mean(Average_Grade_Level, na.rm = TRUE),
                    # Flesch_Kincaid_Reading_Ease_Score = mean(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = mean(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = mean(New_Dale_Chall_Score, na.rm = TRUE),
                    # Reading_Time = mean(Reading_Time, na.rm = TRUE),
                    # Speaking_Time = mean(Speaking_Time, na.rm = TRUE),
                    total_score = mean(total_score, na.rm = TRUE)
                    )

table_indices_by_year_low_2 <- indices_by_year_low_2 %>% 
  gather(-year, key = "indice", value = "value")
indices_low_2 <- ggplot(table_indices_by_year_low_2, aes(year, value, color=indice)) +
  geom_line()+
  xlim(1997, 2017) +
  ylim(0, 25) +
  geom_hline(yintercept = c(10,15,20), color="gray", linetype=2)+
  geom_smooth()+
  theme(legend.position="none")+
  geom_point()+
  facet_wrap(~indice, scales = "free")

###  ALL
by_year<- group_by(DB1, year)
indices_by_year<- summarise(by_year, 
                    Flesch_Kincaid_Reading_Ease_Level = mean(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Gunning_Fog_Index = mean(Gunning_Fog_Index, na.rm = TRUE),
                    Coleman_Liau_Index = mean(Coleman_Liau_Index, na.rm = TRUE),
                    SMOG_Index = mean(SMOG_Index, na.rm = TRUE),
                    Automated_Readability_Index = mean(Automated_Readability_Index, na.rm = TRUE),
                    Average_Grade_Level = mean(Average_Grade_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score = mean(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    Spache_Score = mean(Spache_Score, na.rm = TRUE),
                    New_Dale_Chall_Score = mean(New_Dale_Chall_Score, na.rm = TRUE),
                    Reading_Time = mean(Reading_Time, na.rm = TRUE),
                    Speaking_Time = mean(Speaking_Time, na.rm = TRUE),
                    total_score = mean(total_score, na.rm = TRUE)
                    )


indices_all<- summarise(DB1, 
                    Flesch_Kincaid_Reading_Ease_Level_mean = median(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Level_max = max(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Level_min = min(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    
                    Gunning_Fog_Index_mean = median(Gunning_Fog_Index, na.rm = TRUE),
                    Gunning_Fog_Index_max = max(Gunning_Fog_Index, na.rm = TRUE),
                    Gunning_Fog_Index_min = min(Gunning_Fog_Index, na.rm = TRUE),

                    Coleman_Liau_Index_mean = median(Coleman_Liau_Index, na.rm = TRUE),
                    Coleman_Liau_Index_max = max(Coleman_Liau_Index, na.rm = TRUE),
                    Coleman_Liau_Index_min = min(Coleman_Liau_Index, na.rm = TRUE),

                    SMOG_Index_mean = median(SMOG_Index, na.rm = TRUE),
                    SMOG_Index_max = max(SMOG_Index, na.rm = TRUE),
                    SMOG_Index_min = min(SMOG_Index, na.rm = TRUE),

                    Automated_Readability_Index_mean = median(Automated_Readability_Index, na.rm = TRUE),
                    Automated_Readability_Index_max = max(Automated_Readability_Index, na.rm = TRUE),
                    Automated_Readability_Index_min = min(Automated_Readability_Index, na.rm = TRUE),

                    Average_Grade_Level_mean = median(Average_Grade_Level, na.rm = TRUE),
                    Average_Grade_Level_max = max(Average_Grade_Level, na.rm = TRUE),
                    Average_Grade_Level_min = min(Average_Grade_Level, na.rm = TRUE),

                    Flesch_Kincaid_Reading_Ease_Score_mean = median(Flesch_Kincaid_Reading_Ease_Score, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score_max = max(Flesch_Kincaid_Reading_Ease_Score, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score_min = min(Flesch_Kincaid_Reading_Ease_Score, na.rm = TRUE),

                    Spache_Score_mean = median(Spache_Score, na.rm = TRUE),
                    Spache_Score_max = max(Spache_Score, na.rm = TRUE),
                    Spache_Score_min = min(Spache_Score, na.rm = TRUE),

                    New_Dale_Chall_Score_mean = median(New_Dale_Chall_Score, na.rm = TRUE),
                    New_Dale_Chall_Score_max = max(New_Dale_Chall_Score, na.rm = TRUE),
                    New_Dale_Chall_Score_min = min(New_Dale_Chall_Score, na.rm = TRUE),
                    
                    Reading_Time_mean = median(Reading_Time, na.rm = TRUE),
                    Reading_Time_max = max(Reading_Time, na.rm = TRUE),
                    Reading_Time_min = min(Reading_Time, na.rm = TRUE),
                    
                    Speaking_Time_mean = median(Speaking_Time, na.rm = TRUE),
                    Speaking_Time_max = max(Speaking_Time, na.rm = TRUE),
                    Speaking_Time_min = min(Speaking_Time, na.rm = TRUE),
                    
                    total_score_mean = median(total_score, na.rm = TRUE),
                    total_score_max = max(total_score, na.rm = TRUE),
                    total_score_min = min(total_score, na.rm = TRUE)
                    )

write.csv(indices_all, file="statistisc_readability.csv")

high_quality <- filter(DB1, AMSTAR_levels == "high_quality")
moderate_quality <- filter(DB1, AMSTAR_levels == "moderate_quality")
low_quality <- filter(DB1, AMSTAR_levels == "low_quality")

indices_all_moderate<- summarise(moderate_quality, 
                    Flesch_Kincaid_Reading_Ease_Level_mean = median(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Level_max = max(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Level_min = min(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    
                    Gunning_Fog_Index_mean = median(Gunning_Fog_Index, na.rm = TRUE),
                    Gunning_Fog_Index_max = max(Gunning_Fog_Index, na.rm = TRUE),
                    Gunning_Fog_Index_min = min(Gunning_Fog_Index, na.rm = TRUE),

                    Coleman_Liau_Index_mean = median(Coleman_Liau_Index, na.rm = TRUE),
                    Coleman_Liau_Index_max = max(Coleman_Liau_Index, na.rm = TRUE),
                    Coleman_Liau_Index_min = min(Coleman_Liau_Index, na.rm = TRUE),

                    SMOG_Index_mean = median(SMOG_Index, na.rm = TRUE),
                    SMOG_Index_max = max(SMOG_Index, na.rm = TRUE),
                    SMOG_Index_min = min(SMOG_Index, na.rm = TRUE),

                    Automated_Readability_Index_mean = median(Automated_Readability_Index, na.rm = TRUE),
                    Automated_Readability_Index_max = max(Automated_Readability_Index, na.rm = TRUE),
                    Automated_Readability_Index_min = min(Automated_Readability_Index, na.rm = TRUE),

                    Average_Grade_Level_mean = median(Average_Grade_Level, na.rm = TRUE),
                    Average_Grade_Level_max = max(Average_Grade_Level, na.rm = TRUE),
                    Average_Grade_Level_min = min(Average_Grade_Level, na.rm = TRUE),

                    Flesch_Kincaid_Reading_Ease_Score_mean = median(Flesch_Kincaid_Reading_Ease_Score, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score_max = max(Flesch_Kincaid_Reading_Ease_Score, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score_min = min(Flesch_Kincaid_Reading_Ease_Score, na.rm = TRUE),

                    Spache_Score_mean = median(Spache_Score, na.rm = TRUE),
                    Spache_Score_max = max(Spache_Score, na.rm = TRUE),
                    Spache_Score_min = min(Spache_Score, na.rm = TRUE),

                    New_Dale_Chall_Score_mean = median(New_Dale_Chall_Score, na.rm = TRUE),
                    New_Dale_Chall_Score_max = max(New_Dale_Chall_Score, na.rm = TRUE),
                    New_Dale_Chall_Score_min = min(New_Dale_Chall_Score, na.rm = TRUE),
                    
                    Reading_Time_mean = median(Reading_Time, na.rm = TRUE),
                    Reading_Time_max = max(Reading_Time, na.rm = TRUE),
                    Reading_Time_min = min(Reading_Time, na.rm = TRUE),
                    
                    Speaking_Time_mean = median(Speaking_Time, na.rm = TRUE),
                    Speaking_Time_max = max(Speaking_Time, na.rm = TRUE),
                    Speaking_Time_min = min(Speaking_Time, na.rm = TRUE),
                    
                    total_score_mean = median(total_score, na.rm = TRUE),
                    total_score_max = max(total_score, na.rm = TRUE),
                    total_score_min = min(total_score, na.rm = TRUE)
                    )

write.csv(indices_all_moderate, file="statistisc_readability_moderate.csv")


model <- lm(log(Speaking_Time,2)~AMSTAR_levels, data=DB1)
summary(model)
TukeyHSD(aov(model))

table_indices_by_year<- indices_by_year %>% 
  gather(-year, key = "indice", value = "value")
indices_all <- ggplot(table_indices_by_year, aes(year, value, color=indice)) +
  geom_line(linetype=1)+
  geom_point()+
   geom_smooth()+
  theme(legend.position="none")+
  facet_wrap(~indice, scales = "free")


prisma_by_year<- summarise(by_year, 
                    total_score = mean(total_score, na.rm = TRUE)
                    )
table_prisma_by_year<- prisma_by_year %>% 
  gather(-year, key = "indice", value = "value")
prisma_all <- ggplot(table_prisma_by_year, aes(year, value, color=indice)) +
  geom_line(linetype=1)+
  xlim(1997, 2017) +
  geom_point()+
   geom_smooth()+
  theme(legend.position="none")+
  facet_wrap(~indice, scales = "free")

library(ggfortify)
autoplot(indices_high, indices_moderate, indices_low)




####  abstract_format
install.packages("ggpubr")
library("ggpubr")
library("ggsignif")


DB1$AMSTAR_levels<-factor(DB1$AMSTAR_levels, levels=c("high_quality", "moderate_quality", "low_quality"))
DB1$abstract_format<-factor(DB1$abstract_format, levels=c("8-headings", "IMRAD", "free"))
DB1$conflict_of_interest<-as.numeric(DB1$conflict_of_interest)

lm_abstract_format <- glm(total_score~abstract_format+AMSTAR_levels , data=DB1)
summary(lm_abstract_format)

compare_means(total_score ~ abstract_format,  data = DB1, method = "anova", group.by = "AMSTAR_levels")
p <- ggboxplot(DB1, x = "abstract_format", y = "total_score",
          color = "abstract_format", palette = "jco",
          add = "jitter",
          facet.by = "AMSTAR_levels", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(total_score ~ abstract_format,  data = DB1, method = "anova", group.by = "abstract_format")
p <- ggboxplot(DB1, x = "AMSTAR_levels", y = "total_score",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(total_score ~ funding_academic,  data = DB1, method = "anova", group.by = "funding_academic")
p <- ggboxplot(DB1, x = "AMSTAR_levels", y = "total_score",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "funding_academic", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(log(total_score,2) ~ journal_PRISMA_endorsement,  data = na.omit(DB1[,c("total_score","journal_PRISMA_endorsement","AMSTAR_levels")]), method = "anova", group.by = "journal_PRISMA_endorsement")
p <- ggboxplot(na.omit(DB1[,c("total_score","journal_PRISMA_endorsement","AMSTAR_levels")]), x = "AMSTAR_levels", y = "log(total_score,2)",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "journal_PRISMA_endorsement", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(total_score ~ journal_PRISMA_endorsement,  data = na.omit(DB1[,c("total_score","journal_PRISMA_endorsement","abstract_format")]), method = "anova", group.by = "journal_PRISMA_endorsement")
p <- ggboxplot(na.omit(DB1[,c("total_score","journal_PRISMA_endorsement","abstract_format")]), x = "abstract_format", y = "total_score",
          color = "abstract_format", palette = "jco",
          add = "jitter",
          facet.by = "journal_PRISMA_endorsement", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")


########  PRISMA total_score vs RoB and abstract_format
compare_means(total_score ~ RoB_ROBIS,  data = na.omit(DB1[,c("total_score","RoB_ROBIS","abstract_format")]), group.by = "RoB_ROBIS", ref.group=".all.",method = "t.test")
p <- ggboxplot(na.omit(DB1[,c("total_score","RoB_ROBIS","abstract_format")]), x = "abstract_format", y = "total_score",
          color = "abstract_format", palette = "jco",
          add = "jitter",
          facet.by = "RoB_ROBIS", short.panel.labs = FALSE)
p + stat_compare_means(method="anova", label.y=15) + stat_compare_means(label = "p.signif", method="t.test", ref.group = ".all.")


########  PRISMA total_score vs AMSTAR_levels and abstract_format
compare_means(total_score ~ AMSTAR_levels,  data = na.omit(DB1[,c("total_score","AMSTAR_levels","abstract_format")]), method = "anova", group.by = "AMSTAR_levels")
p <- ggboxplot(na.omit(DB1[,c("total_score","AMSTAR_levels","abstract_format")]), x = "abstract_format", y = "total_score",
          color = "abstract_format", palette = "jco",
          add = "jitter",
          facet.by = "AMSTAR_levels", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")


########  PRISMA total_score vs AMSTAR_levels and RoB
compare_means(total_score ~ AMSTAR_levels,  data = na.omit(DB1[,c("total_score","AMSTAR_levels","RoB_ROBIS")]), method = "anova", group.by = "RoB_ROBIS")
p <- ggboxplot(na.omit(DB1[,c("total_score","AMSTAR_levels","RoB_ROBIS")]), x = "RoB_ROBIS", y = "total_score",
          color = "RoB_ROBIS", palette = "jco",
          add = "jitter",
          facet.by = "AMSTAR_levels", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")


compare_means(total_score ~ RoB_ROBIS,  data = na.omit(DB1[,c("total_score","AMSTAR_levels","RoB_ROBIS")]), method = "anova", group.by = "AMSTAR_levels")
p <- ggboxplot(na.omit(DB1[,c("total_score","AMSTAR_levels","RoB_ROBIS")]), x = "AMSTAR_levels", y = "total_score",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "RoB_ROBIS", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")


compare_means(total_score ~ funding_industry,  data = na.omit(DB1[,c("total_score","funding_industry","AMSTAR_levels")]), method = "anova", group.by = "funding_industry")
p <- ggboxplot(na.omit(DB1[,c("total_score","funding_industry","AMSTAR_levels")]), x = "AMSTAR_levels", y = "total_score",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "funding_industry", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(total_score ~ RoB_ROBIS,  data = na.omit(DB1[,c("total_score","RoB_ROBIS","AMSTAR_levels")]), method = "anova", group.by = "RoB_ROBIS")
p <- ggboxplot(na.omit(DB1[,c("total_score","RoB_ROBIS","AMSTAR_levels")]), x = "AMSTAR_levels", y = "total_score",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "RoB_ROBIS", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")



############## readability scores vs AMSTAR_levels and abstract_format
DB1$abstract_format<-factor(DB1$abstract_format, levels=c("8-headings", "IMRAD", "free"))
compare_means(Flesch_Kincaid_Reading_Ease_Level ~ AMSTAR_levels,  data = na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Level","abstract_format","AMSTAR_levels")]), method = "anova", group.by = "abstract_format")
p <- ggboxplot(na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Level","abstract_format","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Flesch_Kincaid_Reading_Ease_Level",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")


compare_means(Gunning_Fog_Index ~ AMSTAR_levels,  data = na.omit(DB1[,c("Gunning_Fog_Index","abstract_format","AMSTAR_levels")]), method = "anova", group.by = "abstract_format")
p <- ggboxplot(na.omit(DB1[,c("Gunning_Fog_Index","abstract_format","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Gunning_Fog_Index",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(Coleman_Liau_Index ~ AMSTAR_levels,  data = na.omit(DB1[,c("Coleman_Liau_Index","abstract_format","AMSTAR_levels")]), method = "anova", group.by = "abstract_format")
p <- ggboxplot(na.omit(DB1[,c("Coleman_Liau_Index","abstract_format","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Coleman_Liau_Index",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(SMOG_Index ~ AMSTAR_levels,  data = na.omit(DB1[,c("SMOG_Index","abstract_format","AMSTAR_levels")]), method = "anova", group.by = "abstract_format")
p <- ggboxplot(na.omit(DB1[,c("SMOG_Index","abstract_format","AMSTAR_levels")]), x = "AMSTAR_levels", y = "SMOG_Index",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")


compare_means(Automated_Readability_Index ~ AMSTAR_levels,  data = na.omit(DB1[,c("Automated_Readability_Index","abstract_format","AMSTAR_levels")]), method = "anova", group.by = "abstract_format")
p <- ggboxplot(na.omit(DB1[,c("Automated_Readability_Index","abstract_format","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Automated_Readability_Index",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")


compare_means(Average_Grade_Level ~ AMSTAR_levels,  data = na.omit(DB1[,c("Average_Grade_Level","abstract_format","AMSTAR_levels")]), method = "anova", group.by = "abstract_format")
p <- ggboxplot(na.omit(DB1[,c("Average_Grade_Level","abstract_format","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Average_Grade_Level",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(total_score ~ AMSTAR_levels,  data = na.omit(DB1[,c("total_score","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("total_score","AMSTAR_levels")]), x = "AMSTAR_levels", y = "total_score",
          color = "AMSTAR_levels", fill="AMSTAR_levels", palette = "jco")
p + stat_compare_means(comparisons=my_comparisons)
p + stat_compare_means(label.y=5) 

compare_means(total_score ~ RoB_ROBIS,  data = na.omit(DB1[,c("total_score","RoB_ROBIS")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("total_score","RoB_ROBIS")]), x = "RoB_ROBIS", y = "total_score",
          color = "RoB_ROBIS", fill = "RoB_ROBIS", alpha=0.3, palette = "pancake",
           add = "jitter")
p + stat_compare_means(comparisons=my_comparisons_2)
p + stat_compare_means(label.y=13) 

compare_means(total_score ~ AMSTAR_levels,  data = na.omit(DB1[,c("total_score","RoB_ROBIS", "AMSTAR_levels")]), method = "anova", group.by="RoB_ROBIS")
p <- ggboxplot(na.omit(DB1[,c("total_score","RoB_ROBIS", "AMSTAR_levels")]), x = "AMSTAR_levels", y = "total_score",
          color = "RoB_ROBIS", fill="RoB_ROBIS", palette = "jco",
           add = "jitter")
p + stat_compare_means(comparisons=my_comparisons_2)
p + stat_compare_means(label = "p.format", method = "t.test",
                     ref.group = ".all.")



my_comparisons <- list(c("moderate_quality", "low_quality"), c("high_quality", "moderate_quality"), c("high_quality", "low_quality"))
my_comparisons_2 <- list(c("high", "low"))


##### index BY AMSTAR
DB1$AMSTAR_levels<-factor(DB1$AMSTAR_levels, levels=c("low_quality", "moderate_quality", "high_quality"))
palette_2<-c("coral", "green", "blue")

compare_means(log(SMOG_Index,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("SMOG_Index","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("SMOG_Index","AMSTAR_levels")]), x = "AMSTAR_levels", y = "SMOG_Index",
          color = "AMSTAR_levels", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(comparisons=my_comparisons)


compare_means(log(Automated_Readability_Index,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("Automated_Readability_Index","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Automated_Readability_Index","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Automated_Readability_Index",
          color = "AMSTAR_levels", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(comparisons=my_comparisons)


compare_means(log(Average_Grade_Level,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("Average_Grade_Level","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Average_Grade_Level","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Average_Grade_Level",
          color = "AMSTAR_levels", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(comparisons=my_comparisons)


compare_means(log(Gunning_Fog_Index,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("Gunning_Fog_Index","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Gunning_Fog_Index","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Gunning_Fog_Index",
          color = "AMSTAR_levels", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(comparisons=my_comparisons)

compare_means(log(Coleman_Liau_Index,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("Coleman_Liau_Index","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Coleman_Liau_Index","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Coleman_Liau_Index",
          color = "AMSTAR_levels", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(comparisons=my_comparisons)

compare_means(log(Spache_Score,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("Spache_Score","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Spache_Score","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Spache_Score",
          color = "AMSTAR_levels", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(comparisons=my_comparisons)

compare_means(log(Flesch_Kincaid_Reading_Ease_Level,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Level","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Level","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Flesch_Kincaid_Reading_Ease_Level",
          color = "AMSTAR_levels", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(comparisons=my_comparisons)

compare_means(log(New_Dale_Chall_Score,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("New_Dale_Chall_Score","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("New_Dale_Chall_Score","AMSTAR_levels")]), x = "AMSTAR_levels", y = "New_Dale_Chall_Score",
          color = "AMSTAR_levels", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(comparisons=my_comparisons)

compare_means(log(Reading_Time,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("Reading_Time","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Reading_Time","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Reading_Time",
          color = "AMSTAR_levels", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(comparisons=my_comparisons)


######index BY abstract format
DB1$abstract_format<-factor(DB1$abstract_format, levels=c("8-headings", "IMRAD", "free"))
palette_new<-c("grey", "yellow", "orange")
my_comparisons_3 <- list(c("8-headings", "IMRAD"), c("IMRAD", "free"), c("8-headings", "free") )


compare_means(log(SMOG_Index,2)~ abstract_format,  data = na.omit(DB1[,c("SMOG_Index","abstract_format")]), method = "anova")
p1 <- ggboxplot(na.omit(DB1[,c("SMOG_Index","abstract_format")]), x = "abstract_format", y = "SMOG_Index",
          color = "abstract_format",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p1 + stat_compare_means(comparisons=my_comparisons_3)

compare_means(log(Automated_Readability_Index,2)~ abstract_format,  data = na.omit(DB1[,c("Automated_Readability_Index","abstract_format")]), method = "anova")
p1 <- ggboxplot(na.omit(DB1[,c("Automated_Readability_Index","abstract_format")]), x = "abstract_format", y = "Automated_Readability_Index",
          color = "abstract_format",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p1 + stat_compare_means(comparisons=my_comparisons_3)

compare_means(log(Average_Grade_Level,2)~ abstract_format,  data = na.omit(DB1[,c("Average_Grade_Level","abstract_format")]), method = "anova")
p1 <- ggboxplot(na.omit(DB1[,c("Average_Grade_Level","abstract_format")]), x = "abstract_format", y = "Average_Grade_Level",
          color = "abstract_format",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p1 + stat_compare_means(comparisons=my_comparisons_3)

compare_means(log(Gunning_Fog_Index,2)~ abstract_format,  data = na.omit(DB1[,c("Gunning_Fog_Index","abstract_format")]), method = "anova")
p1 <- ggboxplot(na.omit(DB1[,c("Gunning_Fog_Index","abstract_format")]), x = "abstract_format", y = "Gunning_Fog_Index",
          color = "abstract_format",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p1 + stat_compare_means(comparisons=my_comparisons_3)


compare_means(log(Coleman_Liau_Index,2)~ abstract_format,  data = na.omit(DB1[,c("Coleman_Liau_Index","abstract_format")]), method = "anova")
p1 <- ggboxplot(na.omit(DB1[,c("Coleman_Liau_Index","abstract_format")]), x = "abstract_format", y = "Coleman_Liau_Index",
          color = "abstract_format",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p1 + stat_compare_means(comparisons=my_comparisons_3)


compare_means(log(Spache_Score,2)~ abstract_format,  data = na.omit(DB1[,c("Spache_Score","abstract_format")]), method = "anova")
p1 <- ggboxplot(na.omit(DB1[,c("Spache_Score","abstract_format")]), x = "abstract_format", y = "Spache_Score",
          color = "abstract_format",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p1 + stat_compare_means(comparisons=my_comparisons_3)


compare_means(log(Flesch_Kincaid_Reading_Ease_Level,2)~ abstract_format,  data = na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Level","abstract_format")]), method = "anova")
p1 <- ggboxplot(na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Level","abstract_format")]), x = "abstract_format", y = "Flesch_Kincaid_Reading_Ease_Level",
          color = "abstract_format",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p1 + stat_compare_means(comparisons=my_comparisons_3)


compare_means(log(New_Dale_Chall_Score,2)~ abstract_format,  data = na.omit(DB1[,c("New_Dale_Chall_Score","abstract_format")]), method = "anova")
p1 <- ggboxplot(na.omit(DB1[,c("New_Dale_Chall_Score","abstract_format")]), x = "abstract_format", y = "New_Dale_Chall_Score",
          color = "abstract_format",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p1 + stat_compare_means(comparisons=my_comparisons_3)

compare_means(log(Reading_Time,2)~ abstract_format,  data = na.omit(DB1[,c("Reading_Time","abstract_format")]), method = "anova")
p1 <- ggboxplot(na.omit(DB1[,c("Reading_Time","abstract_format")]), x = "abstract_format", y = "Reading_Time",
          color = "abstract_format",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p1 + stat_compare_means(comparisons=my_comparisons_3)


#######  index vs AMSTAR x format
DB1$abstract_format<-factor(DB1$abstract_format, levels=c("8-headings", "IMRAD", "free"))
palette_new<-c("grey", "yellow", "orange")
my_comparisons_3 <- list(c("8-headings", "IMRAD"), c("IMRAD", "free"), c("8-headings", "free") )

compare_means(log(SMOG_Index,2)~ AMSTAR_levels, data = na.omit(DB1[,c("SMOG_Index","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("SMOG_Index","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "SMOG_Index",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         #facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")


compare_means(log(Automated_Readability_Index,2)~ AMSTAR_levels, data = na.omit(DB1[,c("Automated_Readability_Index","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Automated_Readability_Index","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "Automated_Readability_Index",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         #facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")


compare_means(log(Average_Grade_Level,2)~ AMSTAR_levels, data = na.omit(DB1[,c("Average_Grade_Level","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Average_Grade_Level","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "Average_Grade_Level",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         #facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")


compare_means(log(Gunning_Fog_Index,2)~ AMSTAR_levels, data = na.omit(DB1[,c("Gunning_Fog_Index","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Gunning_Fog_Index","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "Gunning_Fog_Index",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         #facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")



compare_means(log(Coleman_Liau_Index,2)~ AMSTAR_levels, data = na.omit(DB1[,c("Coleman_Liau_Index","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Coleman_Liau_Index","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "Coleman_Liau_Index",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         #facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")


compare_means(log(Spache_Score,2)~ AMSTAR_levels, data = na.omit(DB1[,c("Spache_Score","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Spache_Score","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "Spache_Score",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         #facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")



compare_means(log(Flesch_Kincaid_Reading_Ease_Level,2)~ AMSTAR_levels, data = na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Level","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Level","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "Flesch_Kincaid_Reading_Ease_Level",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         #facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")


compare_means(log(New_Dale_Chall_Score,2)~ AMSTAR_levels, data = na.omit(DB1[,c("New_Dale_Chall_Score","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("New_Dale_Chall_Score","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "New_Dale_Chall_Score",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         #facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")

compare_means(log(Reading_Time,2)~ AMSTAR_levels, data = na.omit(DB1[,c("Reading_Time","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Reading_Time","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "Reading_Time",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         #facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")




##### PRISMA-A BY AMSTAR x format


library("ggpmisc")

compare_means(log(total_score,2)~ AMSTAR_levels,  data = na.omit(DB1[,c("total_score","AMSTAR_levels","abstract_format")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("total_score","AMSTAR_levels","abstract_format")]), x = "AMSTAR_levels", y = "total_score",
          color = "abstract_format", palette = "palette",
          add = "jitter",short.panel.labs = TRUE)+
          scale_color_brewer(palette = "Set2")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(aes(group = abstract_format), label = "p.format")



ggplot(data = na.omit(DB1[,c("total_score","abstract_format","AMSTAR_levels", "Flesch_Kincaid_Reading_Ease_Score")]), aes(y=total_score, x=Flesch_Kincaid_Reading_Ease_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Flesch_Kincaid_Reading_Ease_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=FALSE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  facet_wrap(~AMSTAR_levels)



########### subset of free format
free_format_db<-subset(DB1, abstract_format=="free")


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Flesch_Kincaid_Reading_Ease_Score")]), aes(y=total_score, x=Flesch_Kincaid_Reading_Ease_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Flesch_Kincaid_Reading_Ease_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Spache_Score")]), aes(y=total_score, x=Spache_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Spache_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "New_Dale_Chall_Score")]), aes(y=total_score, x=New_Dale_Chall_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(New_Dale_Chall_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Automated_Readability_Index")]), aes(y=total_score, x=Automated_Readability_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Automated_Readability_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Reading_Time")]), aes(y=total_score, x=Reading_Time, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Reading_Time, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Coleman_Liau_Index")]), aes(y=total_score, x=Coleman_Liau_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Coleman_Liau_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Gunning_Fog_Index")]), aes(y=total_score, x=Gunning_Fog_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Gunning_Fog_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "SMOG_Index")]), aes(y=total_score, x=SMOG_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(SMOG_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Automated_Readability_Index")]), aes(y=total_score, x=Automated_Readability_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Automated_Readability_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Average_Grade_Level")]), aes(y=total_score, x=Average_Grade_Level, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Average_Grade_Level, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Spache_Score")]), aes(y=total_score, x=Spache_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Spache_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Reading_Time")]), aes(y=total_score, x=Reading_Time, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Reading_Time, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "New_Dale_Chall_Score")]), aes(y=total_score, x=New_Dale_Chall_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(New_Dale_Chall_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Coleman_Liau_Index")]), aes(y=total_score, x=Coleman_Liau_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Coleman_Liau_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Flesch_Kincaid_Reading_Ease_Score")]), aes(y=total_score, x=Flesch_Kincaid_Reading_Ease_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Flesch_Kincaid_Reading_Ease_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)





########### subset of 8-headings format
imrad_headings_format_db<-subset(DB1, abstract_format=="IMRAD")


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Flesch_Kincaid_Reading_Ease_Score")]), aes(y=total_score, x=Flesch_Kincaid_Reading_Ease_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Flesch_Kincaid_Reading_Ease_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Spache_Score")]), aes(y=total_score, x=Spache_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Spache_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "New_Dale_Chall_Score")]), aes(y=total_score, x=New_Dale_Chall_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(New_Dale_Chall_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Automated_Readability_Index")]), aes(y=total_score, x=Automated_Readability_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Automated_Readability_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Reading_Time")]), aes(y=total_score, x=Reading_Time, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Reading_Time, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Coleman_Liau_Index")]), aes(y=total_score, x=Coleman_Liau_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Coleman_Liau_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
    theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Gunning_Fog_Index")]), aes(y=total_score, x=Gunning_Fog_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Gunning_Fog_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "SMOG_Index")]), aes(y=total_score, x=SMOG_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(SMOG_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Automated_Readability_Index")]), aes(y=total_score, x=Automated_Readability_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Automated_Readability_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Average_Grade_Level")]), aes(y=total_score, x=Average_Grade_Level, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Average_Grade_Level, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Spache_Score")]), aes(y=total_score, x=Spache_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Spache_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Reading_Time")]), aes(y=total_score, x=Reading_Time, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Reading_Time, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(free_format_db[,c("total_score","abstract_format","AMSTAR_levels", "New_Dale_Chall_Score")]), aes(y=total_score, x=New_Dale_Chall_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(New_Dale_Chall_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_color_brewer(palette = "Set2")+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)

ggplot(data = na.omit(imrad_headings_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Coleman_Liau_Index")]), aes(y=total_score, x=Coleman_Liau_Index, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Coleman_Liau_Index, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_colour_manual(values = brewer.pal(4, "Set2")[2:3])+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)


ggplot(data = na.omit(imrad_headings_format_db[,c("total_score","abstract_format","AMSTAR_levels", "Flesch_Kincaid_Reading_Ease_Score")]), aes(y=total_score, x=Flesch_Kincaid_Reading_Ease_Score, color=abstract_format, shape=abstract_format))+
  geom_point(aes(Flesch_Kincaid_Reading_Ease_Score, total_score,  color=abstract_format, shape = abstract_format))+
  geom_smooth(method = lm, se=TRUE, position = "identity", fullrange=TRUE)+
  scale_colour_manual(values = brewer.pal(4, "Set2")[2:3])+
  ylab("PRISMA for Abstracts")+
  theme_bw()+
  facet_wrap(~AMSTAR_levels)





compare_means(Automated_Readability_Index ~ AMSTAR_levels,  data = na.omit(DB1[,c("Automated_Readability_Index","AMSTAR_levels")]), method = "anova")
p <- ggboxplot(na.omit(DB1[,c("Automated_Readability_Index","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Automated_Readability_Index",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter")
         # facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(Flesch_Kincaid_Reading_Ease_Score ~ AMSTAR_levels,  data = na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Score","abstract_format","AMSTAR_levels")]), method = "anova", group.by = "abstract_format")
p <- ggboxplot(na.omit(DB1[,c("Flesch_Kincaid_Reading_Ease_Score","abstract_format","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Flesch_Kincaid_Reading_Ease_Score",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(Spache_Score ~ AMSTAR_levels,  data = na.omit(DB1[,c("Spache_Score","abstract_format","AMSTAR_levels")]), method = "anova", group.by = "abstract_format")
p <- ggboxplot(na.omit(DB1[,c("Spache_Score","abstract_format","AMSTAR_levels")]), x = "AMSTAR_levels", y = "Spache_Score",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")

compare_means(New_Dale_Chall_Score ~ AMSTAR_levels,  data = na.omit(DB1[,c("New_Dale_Chall_Score","abstract_format","AMSTAR_levels")]), method = "anova", group.by = "abstract_format")
p <- ggboxplot(na.omit(DB1[,c("New_Dale_Chall_Score","abstract_format","AMSTAR_levels")]), x = "AMSTAR_levels", y = "New_Dale_Chall_Score",
          color = "AMSTAR_levels", palette = "jco",
          add = "jitter",
          facet.by = "abstract_format", short.panel.labs = FALSE)
p + stat_compare_means(label = "p.format")


abstract_format <- ggplot(DB1, aes(abstract_format, total_score, color=AMSTAR_levels, fill=AMSTAR_levels)) +
  geom_boxplot()+
  scale_y_continuous("PRISMA for abstract score")

lm_abstract_format_2 <- glm(total_score~AMSTAR_levels+abstract_format, data=DB1)
summary(lm_abstract_format_2)
abstract_format_2 <- ggplot(DB1, aes(AMSTAR_levels, total_score, color=abstract_format, fill=abstract_format)) +
  geom_boxplot()+
  scale_y_continuous("PRISMA for abstract score")

lm_Reading_Time <- glm(Reading_Time~AMSTAR_levels+abstract_format, data=DB1)
summary(lm_Reading_Time)
reading_Time <- ggplot(DB1, aes(AMSTAR_levels, Reading_Time, color=abstract_format, fill=abstract_format)) +
  geom_boxplot()+
  scale_y_continuous("PRISMA for abstract score")

library(plyr)
DB1$funding_academic<-recode(DB1$funding_academic, "0" ="no", "1" ="yes")
lm_funding_academic <- glm(total_score~funding_academic+abstract_format, data=DB1)
summary(lm_funding_academic)
funding_academic <- ggplot(DB1, aes(abstract_format, total_score, color=funding_academic, fill=funding_academic)) +
  geom_boxplot()+
  scale_y_continuous("PRISMA for abstract score")

DB1$funding_industry<-recode(DB1$funding_industry, "0" ="no", "1" ="yes")
lm_funding_industry <- glm(total_score~funding_industry+abstract_format, data=DB1)
summary(lm_funding_industry)
funding_industry <- ggplot(DB1, aes(abstract_format, total_score, color=funding_industry, fill=funding_industry)) +
  geom_boxplot()+
  scale_y_continuous("PRISMA for abstract score")


lm_journal_PRISMA_endorsement<- glm(total_score~journal_PRISMA_endorsement+abstract_format, data=na.omit(DB1[,c("total_score","journal_PRISMA_endorsement","abstract_format")]))
summary(lm_journal_PRISMA_endorsement)
journal_PRISMA_endorsement <- ggplot(na.omit(DB1[,c("total_score","journal_PRISMA_endorsement","abstract_format")]), aes(abstract_format, total_score, color=journal_PRISMA_endorsement, fill=journal_PRISMA_endorsement)) +
  geom_boxplot()+
  scale_y_continuous("PRISMA for abstract score")



lm_abstract_word_count<- glm(total_score~abstract_format+log(abstract_word_count,2), data=na.omit(DB1[,c("total_score","abstract_word_count","abstract_format")]))
summary(lm_abstract_word_count)
journal_abstract_word_count <- ggplot(na.omit(DB1[,c("total_score","abstract_word_count","abstract_format")]), aes(log(abstract_word_count, 2), total_score, color=abstract_format, fill=abstract_format)) +
  geom_point()+
  geom_smooth()+
  scale_y_continuous("PRISMA for abstract score")
  scale_x_continuous("log2(abstract word count)")
  
  
lm_conflict_of_interest<- glm(total_score~abstract_format+conflict_of_interest, data=na.omit(DB1[,c("total_score","conflict_of_interest","abstract_format")]))
summary(lm_conflict_of_interest)
journal_conflict_of_interest <- ggplot(na.omit(DB1[,c("total_score","conflict_of_interest","abstract_format")]), aes(log(conflict_of_interest, 2), total_score, color=abstract_format, fill=abstract_format)) +
  geom_point()+
  scale_y_continuous("PRISMA for abstract score")
  scale_x_continuous("log2(conflict_of_interest)")
  
lm_journal_impact_factor<- glm(total_score~abstract_format+journal_impact_factor, data=na.omit(DB1[,c("total_score","journal_impact_factor","abstract_format")]))
summary(lm_journal_impact_factor)
journal_journal_impact_factor <- ggplot(na.omit(DB1[,c("total_score","journal_impact_factor","abstract_format")]), aes(log(journal_impact_factor, 2), total_score, color=abstract_format, fill=abstract_format)) +
  geom_point()+
  scale_y_continuous("PRISMA for abstract score")
  scale_x_continuous("log2(conflict_of_interest)")


  
##### PRISMA for abstracts Likerst scales ALL
  
DB4_all<- na.omit(DB1[,c(18:29, 4, 6, 50)])
desired.order <- c("No", "Yes")
DB4_all$PEA1 <- as.factor(DB4_all$PEA1)
DB4_all$PEA1 <- revalue(DB4_all$PEA1, c("0"="No", "1"="Yes"))
DB4_all$PEA1 <- factor(DB4_all$PEA1, levels=desired.order, ordered=TRUE)

DB4_all$PEA2 <- as.factor(DB4_all$PEA2)
DB4_all$PEA2 <- revalue(DB4_all$PEA2, c("0"="No", "1"="Yes"))
DB4_all$PEA2 <- factor(DB4_all$PEA2, levels=desired.order, ordered=TRUE)

DB4_all$PEA3 <- as.factor(DB4_all$PEA3)
DB4_all$PEA3 <- revalue(DB4_all$PEA3, c("0"="No", "1"="Yes"))
DB4_all$PEA3 <- factor(DB4_all$PEA3, levels=desired.order, ordered=TRUE)

DB4_all$PEA4 <- as.factor(DB4_all$PEA4)
DB4_all$PEA4 <- revalue(DB4_all$PEA4, c("0"="No", "1"="Yes"))
DB4_all$PEA4 <- factor(DB4_all$PEA4, levels=desired.order, ordered=TRUE)

DB4_all$PEA5 <- as.factor(DB4_all$PEA5)
DB4_all$PEA5 <- revalue(DB4_all$PEA5, c("0"="No", "1"="Yes"))
DB4_all$PEA5 <- factor(DB4_all$PEA5, levels=desired.order, ordered=TRUE)

DB4_all$PEA6 <- as.factor(DB4_all$PEA6)
DB4_all$PEA6 <- revalue(DB4_all$PEA6, c("0"="No", "1"="Yes"))
DB4_all$PEA6 <- factor(DB4_all$PEA6, levels=desired.order, ordered=TRUE)

DB4_all$PEA7 <- as.factor(DB4_all$PEA7)
DB4_all$PEA7 <- revalue(DB4_all$PEA7, c("0"="No", "1"="Yes"))
DB4_all$PEA7 <- factor(DB4_all$PEA7, levels=desired.order, ordered=TRUE)

DB4_all$PEA8 <- as.factor(DB4_all$PEA8)
DB4_all$PEA8 <- revalue(DB4_all$PEA8, c("0"="No", "1"="Yes"))
DB4_all$PEA8 <- factor(DB4_all$PEA8, levels=desired.order, ordered=TRUE)

DB4_all$PEA9 <- as.factor(DB4_all$PEA9)
DB4_all$PEA9 <- revalue(DB4_all$PEA9, c("0"="No", "1"="Yes"))
DB4_all$PEA9 <- factor(DB4_all$PEA9, levels=desired.order, ordered=TRUE)

DB4_all$PEA10 <- as.factor(DB4_all$PEA10)
DB4_all$PEA10 <- revalue(DB4_all$PEA10, c("0"="No", "1"="Yes"))
DB4_all$PEA10 <- factor(DB4_all$PEA10, levels=desired.order, ordered=TRUE)

DB4_all$PEA11 <- as.factor(DB4_all$PEA11)
DB4_all$PEA11 <- revalue(DB4_all$PEA11, c("0"="No", "1"="Yes"))
DB4_all$PEA11 <- factor(DB4_all$PEA11, levels=desired.order, ordered=TRUE)

DB4_all$PEA12 <- as.factor(DB4_all$PEA12)
DB4_all$PEA12 <- revalue(DB4_all$PEA12, c("0"="No", "1"="Yes"))
DB4_all$PEA12 <- factor(DB4_all$PEA12, levels=desired.order, ordered=TRUE)

attach(DB4_all)

##### likert scale ROBIS subgrupos
require(likert)
require(grid)
require(lattice)
require(latticeExtra)
detach("package:HH", unload=TRUE)
detach("package:ggfortify", unload=TRUE)

items <- as.data.frame(DB4_all[1:12])
items <- as.data.frame(rename(items, c(PEA12 = "Registration", PEA11 = "Funding", PEA10 = "Interpretation", PEA9 = "Strenghts and Limitations of evidence", PEA8 = "Description of the effect", PEA7 = "Synthesis of results", PEA6 = "Included studies", PEA5 = "Risk of bias", PEA4 = "Information sources", PEA3 = "Eligibility criteria", PEA2 = "Objectives", PEA1 = "Title")))
plot(likert::likert(rev(items),grouping=DB4_all$RoB_ROBIS),
     main="All SRs (n=139) ~ RoB subgroups", 
     ylab="PRISMA for abstracts items")
likert<-likert(items)
likert:Item
likert::likert.bar.plot(likert::likert(items), 
                centered = FALSE, 
                main="All SRs (n=139)", 
                ylab="PRISMA for abstracts items", 
                legend="", 
                legend.position = "right", 
                ordered=TRUE, 
                low.color="lightsalmon1", 
                high.color="skyblue3", 
                neutral.color="seagreen3")


##### likert scale AMSTAR subgrupos

items <- as.data.frame(DB4_all[1:12])
items <- as.data.frame(rename(items, c(PEA1 = "Title", PEA2 = "Objectives", PEA3 = "Eligibility criteria", PEA4 = "Information sources", PEA5 = "Risk of bias", PEA6 = "Included studies", PEA7 = "Synthesis of results", PEA8 = "Description of the effect", PEA9 = "Strenghts and Limitations of evidence", PEA10 = "Interpretation", PEA11 = "Funding", PEA12 = "Registration")))

plot(likert::likert(rev(items)),grouping=DB4_all$AMSTAR_levels),
     main="All SRs (n=139) ~ AMSTAR levels", 
     ylab="PRISMA for abstracts items")
##### likert scale AMSTAR subgrupos

items <- as.data.frame(DB4_all[1:12])
items <- as.data.frame(rename(items, c(PEA1 = "Title", PEA2 = "Objectives", PEA3 = "Eligibility criteria", PEA4 = "Information sources", PEA5 = "Risk of bias", PEA6 = "Included studies", PEA7 = "Synthesis of results", PEA8 = "Description of the effect", PEA9 = "Strenghts and Limitations of evidence", PEA10 = "Interpretation", PEA11 = "Funding", PEA12 = "Registration")))

##### likert scale abstract format

plot(likert::likert(rev(items),grouping=DB4_all$abstract_format),
     main="All SRs (n=139) ~ abstract format", 
     ylab="PRISMA for abstracts items")


####### new regression analysis

####### univariate with total PRISMA
m1<-glm(data=DB1, total_score~journal_impact_factor_3)
summary(m1)

####### multivariate with total PRISMA
DB1$cochrane_affiliation<-as.factor(DB1$cochrane_affiliation)
DB1$total_score<-as.factor(DB1$total_score)
DB1$abstract_format_8<-DB1$abstract_format=="8-headings"
DB1$abstract_word_count_300<-DB1$abstract_word_count > 300
DB1$journal_impact_factor_3<-DB1$journal_impact_factor >3

m2<-polr(total_score~   num_authors + funding_academic + journal_PRISMA_endorsement , data = DB1, Hess=TRUE, na.action=na.omit)
summary(m2)
(ctable <- coef(summary(m2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ci <- confint(m2))
confint.default(m2)
exp(coef(m2))
exp(cbind(OR = coef(m2), ci))

library(MASS)
stap<-stepAIC(m2, direction="both")



#########  decision trees
library("rpart.plot")
library("plotmo")
library("car")

DB1<-as.data.frame(DB1)
DB1$total_score<-as.numeric(DB1$total_score)
DB1$funding_academic<-as.factor(DB1$funding_academic)
DB1$funding_academic<-as.factor(DB1$funding_academic)
DB1$PEA1<-as.factor(DB1$PEA1)
DB1$PEA5<-as.factor(DB1$PEA5)
DB1$AMSTAR_levels_new<-DB1$AMSTAR_levels

DB1$AMSTAR_levels_10 <- car::recode(DB1$AMSTAR_levels, "'low_quality'='low'; c('high_quality', 'moderate_quality')='high_moderate'")


###############   Regression trees
######   For a regression tree the summary lists the distribution 
######   of residuals, rather than a classification error rate.

tree <- rpart(AMSTAR_levels_10 ~ PEA1+PEA2+PEA3+PEA4+PEA5+PEA6+PEA7+PEA8+PEA9+PEA10+PEA11+PEA12+total_score,data=DB1,method="class")
rsq.rpart(tree)
rpart.plot(tree, extra=104, box.palette=list( "Blues", "Reds"),branch.lty=3, shadow.col="gray", nn=TRUE)
prp(tree, branch.type=5, yesno=FALSE, faclen=0)
plotmo(tree, type="prob", nresponse="low")
plotcp(tree)
text(tree, pretty=0)

demo1.cv <- cv.tree(tree)
plot(demo3.cv)

tree <- rpart(RoB_ROBIS ~ PEA1+PEA2+PEA3+PEA4+PEA5+PEA6+PEA7+PEA8+PEA9+PEA10+PEA11+PEA12+total_score,data=DB1,method="class")
rsq.rpart(tree)
rpart.plot(tree, extra=104, box.palette=list("Browns", "Greens"),branch.lty=3, shadow.col="gray", nn=TRUE)
prp(tree, branch.type=5, yesno=FALSE, faclen=0)
plotmo(tree, type="prob", nresponse="low")
plotcp(tree)
text(tree, pretty=0)

DB1$amstar_robis<-factor(DB1$amstar_robis)
tree <- rpart(amstar_robis ~ PEA1+PEA2+PEA3+PEA4+PEA5+PEA6+PEA7+PEA8+PEA9+PEA10+PEA11+PEA12+total_score,data=DB1,control = rpart.control(minsplit = 1, minbucket = 0))
rsq.rpart(tree)
pfit<- prune(tree, cp=0.01)
plot(pfit)
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
rpart.plot(tree, extra=104, box.palette=list("Reds", "Green"),branch.lty=3, shadow.col="gray", nn=TRUE)
prp(tree, branch.type=5, yesno=FALSE, faclen=0)
plotmo(tree, type="prob", nresponse="low")
plotcp(tree)
text(tree, pretty=0)


##########  Classification trees
data      <- na.omit(DB1[,c("AMSTAR_levels_10","RoB_ROBIS","amstar_robis","PEA1", "PEA2", "PEA3","PEA4","PEA5","PEA6","PEA7","PEA8","PEA9","PEA10","PEA11","PEA12","total_score" )])
set.seed(101)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(data), alpha * nrow(data))
train.set <- data[inTrain,]
test.set  <- data[-inTrain,]

library("tree")

#### AMSTAR_levels_10
tree.model <- tree(AMSTAR_levels_10 ~PEA1+PEA2+PEA3+PEA4+PEA5+PEA6+PEA7+PEA8+PEA9+PEA10+PEA11+PEA12+total_score, data=train.set)
tree.model
summary(tree.model)
my.prediction <- predict(tree.model, test.set)
maxidx <- function(arr) {
    return(which(arr == max(arr)))
}
idx <- apply(my.prediction, c(1), maxidx)
prediction <- c('high_moderate', 'low')[idx]
table(prediction, test.set$AMSTAR_levels_10)
plot(tree.model)
text(tree.model)
pruned.tree <- prune.tree(tree.model, best=4)
plot(pruned.tree)
text(pruned.tree)


#########  randomForest
library("randomForest")
library("party")

output.forest <- randomForest(AMSTAR_levels_10 ~ PEA1+PEA5+total_score,data=na.omit(DB1[,c("AMSTAR_levels_10",  "PEA1", "PEA5", "total_score")]), ntree = 5000, method="class", mtry = 2, sampsize=c(100,20), replace=TRUE )
print(output.forest)
print(importance(output.forest, type=2))
varImpPlot(output.forest)


###### CARET package

library("AppliedPredictiveModeling")
library("caret")

transparentTheme(trans = .4)
featurePlot(x = DB1[,6], 
            y = DB1[,30], 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))



#######   Altmetrics analysis ---------------------------------
####### packages -----------
install.packages('rAltmetric')
devtools::install_github("ropensci/rAltmetric")
library(rAltmetric)
library(magrittr)
library(purrr)

####### list of D.O.I.s -----------
ids                <- list(c(
                      "10.1038/nature09210",
                      "10.1126/science.1187820",
                      "10.1016/j.tree.2011.01.009",
                      "10.1086/664183"
))

####### fetch the altmetrics data ------------------
alm                <- function(x)  altmetrics(doi = x) %>% altmetric_data()

####### results to .csv file --------------------
results_altmetrics <- pmap_df(ids, alm)
readr::write_csv(results, path = 'results_altmetrics.csv')


#######   abstract structure analysis ---------------------------------
####### word count x abstract format  x AMSTAR
DB1$AMSTAR_levels <- factor(DB1$AMSTAR_levels, levels=c("low_quality", "moderate_quality", "high_quality"))
DB1$abstract_format <- factor(DB1$abstract_format, levels=c("free", "IMRAD", "8-headings"))
ggplot(DB1, aes(abstract_format, abstract_word_count, fill=abstract_format, size=Gunning_Fog_Index))+
  geom_violin()+
  geom_jitter(width = 0.7)+
  geom_point(aes( color=abstract_format, size=Gunning_Fog_Index), shape=3)+
  facet_grid(~AMSTAR_levels)


ggplot(DB1, aes(abstract_format, Gunning_Fog_Index, fill=abstract_format, size=total_score))+
  geom_violin()+
  geom_jitter(width = 0.7)+
  geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid(~AMSTAR_levels)


ggplot(DB1, aes(abstract_format, total_score, fill=abstract_format))+
  geom_boxplot()+
  #geom_jitter(width = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid(~AMSTAR_levels)


ggplot(DB1, aes(Gunning_Fog_Index, total_score, color=abstract_format))+
  geom_point(aes(color=abstract_format))+
  #geom_jitter(width = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid(AMSTAR_levels ~abstract_format)+  
  geom_smooth(method=lm)


ggplot(DB1, aes(Gunning_Fog_Index, total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(Gunning_Fog_Index, total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("Gunning Fog Index")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()

ggplot(DB1, aes(SMOG_Index, total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(SMOG_Index, total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("SMOG Index")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()


ggplot(DB1, aes(Coleman_Liau_Index, total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(Coleman_Liau_Index, total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("Coleman Liau Index")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()

ggplot(DB1, aes(Automated_Readability_Index, total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(Automated_Readability_Index, total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("Automated Readability Index")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()



ggplot(DB1, aes(log(Speaking_Time, 2), total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(log(Speaking_Time, 2), total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("Speaking Time")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()

ggplot(DB1, aes(Flesch_Kincaid_Reading_Ease_Level, total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(Flesch_Kincaid_Reading_Ease_Level, total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("Flesch Kincaid Reading Ease Level")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()

ggplot(DB1, aes(Spache_Score, total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(Spache_Score, total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("Spache Score")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()

ggplot(DB1, aes(New_Dale_Chall_Score, total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(New_Dale_Chall_Score, total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("New Dale Chall Score")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()

ggplot(DB1, aes(Average_Grade_Level, total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(Average_Grade_Level, total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("Average Grade Level")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()


ggplot(DB1, aes(log(Reading_Time,2), total_score, color=AMSTAR_levels,linetype=abstract_format), size=abstract_word_count*0.3, alpha=abstract_word_count*100)+
  geom_point(aes(size=abstract_word_count*0.3))+
  geom_jitter(width = 0.4, height = 0.7)+
  #geom_point(aes( color=abstract_format, size=total_score), shape=3)+
  facet_grid( ~AMSTAR_levels)+  
  geom_smooth(aes(log(Reading_Time,2), total_score, linetype=abstract_format), method=lm,  fullrange = TRUE )+
  xlab("log2(Reading Time)")+
  ylab("PRISMA for Abstract (total score)")+
  theme_bw()

                    #Flesch_Kincaid_Reading_Ease_Level = mean(Flesch_Kincaid_Reading_Ease_Level, na.rm = TRUE),
                    #Gunning_Fog_Index = mean(Gunning_Fog_Index, na.rm = TRUE),
                    #Coleman_Liau_Index = mean(Coleman_Liau_Index, na.rm = TRUE),
                    #SMOG_Index = mean(SMOG_Index, na.rm = TRUE),
                    #Automated_Readability_Index = mean(Automated_Readability_Index, na.rm = TRUE),
                    #Average_Grade_Level = mean(Average_Grade_Level, na.rm = TRUE),
                    Flesch_Kincaid_Reading_Ease_Score = mean(abs(Flesch_Kincaid_Reading_Ease_Score), na.rm = TRUE),
                    #Spache_Score = mean(Spache_Score, na.rm = TRUE),
                    #New_Dale_Chall_Score = mean(New_Dale_Chall_Score, na.rm = TRUE),
                    Reading_Time = mean(Reading_Time, na.rm = TRUE),
                    #Speaking_Time = mean(Speaking_Time, na.rm = TRUE),
                    total_score = mean(total_score, na.rm = TRUE)

###########  boxplot de valores de indices  -----
db_indices <- DB1[, c(1, 6, 8:18, 31, 34, 52, 35, 38, 40, 41, 33, 57,31)]
db_indices$AMSTAR_levels <-factor(db_indices$AMSTAR_levels, levels=c("high_quality","moderate_quality",  "low_quality"))
db_indices$year <-factor(db_indices$year, levels=c("2017","2016","2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000", "1999", "1997"))

db_indices[, c(1:11)] %>%
  gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(x = reorder(index, score, FUN=mean), score, color=AMSTAR_levels))+
  geom_boxplot(aes(x = reorder(index, score, FUN=mean), score, color=AMSTAR_levels))+
  geom_jitter(alpha=0.3, width = 0.5, height = 2)+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))+
  theme(legend.position="none")


db_indices[, c(1:11)] %>%
  gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(index, score, fill=AMSTAR_levels))+
  geom_boxplot(aes(index, score, fill=AMSTAR_levels))+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))

db_indices[, c(1:11, 15)] %>%
  gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(x=index, y=score, fill=year))+
  geom_boxplot(aes(x=index, y=score, fill=year))+
  geom_smooth(formula=y~x, aes(x=index, y=score,fill=year), method="loess", se=F) +
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))

db_indices[, c(1:11, 16)] %>%
  gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(index, score, fill=abstract_format))+
  geom_boxplot(aes(index, score, fill=abstract_format))+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))

db_indices[, c(1:11, 17)] %>%
  mutate(
    Country=recode_factor(Country, "Arabian Emirates"="Others","Brazil"="Others", "China"="Others", "Denmark"="Others", "Germany"="Others", "India"="Others", "Saudi Arabia"="Others","Italy"="Others", "Spain"="Others", "Sweden"="Others", "Switzerland"="Others", "United Kingdom"="UK", "United States"="USA")
  )%>%
  gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(index, score, fill=Country))+
  geom_boxplot(aes(index, score, fill=Country))+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))

na.omit(db_indices[, c(1:11, 18)]) %>%
  gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(index, score, fill=funding))+
  geom_boxplot(aes(index, score, fill=funding))+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))


na.omit(db_indices[, c(1:11, 19)]) %>%
  gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(index, score, fill=funding_academic))+
  geom_boxplot(aes(index, score, fill=funding_academic))+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))

na.omit(db_indices[, c(1:11, 20)]) %>%
  gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(index, score, fill=funding_industry))+
  geom_boxplot(aes(index, score, fill=funding_industry))+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))

db_indices$journal_impact_factor <- as.numeric(db_indices$journal_impact_factor)
db_indices$Gunning_Fog_Index <- as.numeric(db_indices$Gunning_Fog_Index)

na.omit(db_indices[, c(1,2,4,9,10, 16, 17, 21,22,23)]) %>%
  mutate(
    Country=recode_factor(Country, "Arabian Emirates"="Others","Brazil"="Others", "China"="Others", "Denmark"="Others", "Germany"="Others", "India"="Others", "Saudi Arabia"="Others","Italy"="Others", "Spain"="Others", "Sweden"="Others", "Switzerland"="Others", "United Kingdom"="UK", "United States"="USA")
  )%>%
#na.omit(db_indices[, c(1,3,9,10,21,22)]) %>%
 #gather("Flesch_Kincaid_Reading_Ease_Score","Spache_Score",key="index", value="score") %>%
  #gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor),color=abstract_format))+
  #geom_boxplot(aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor)))+
  geom_point(aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor), color=abstract_format, size=total_score,shape=Country))+
  #scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))+
  theme(legend.position="right")+
  xlab("Gunning Fox Index")+
  ylab("")


na.omit(db_indices[, c(1,4,9,10, 16, 17, 21,22,23)]) %>%
  mutate(
    Country=recode_factor(Country, "Arabian Emirates"="Others","Brazil"="Others", "China"="Others", "Denmark"="Others", "Germany"="Others", "India"="Others", "Saudi Arabia"="Others","Italy"="Others", "Spain"="Others", "Sweden"="Others", "Switzerland"="Others", "United Kingdom"="UK", "United States"="USA")
  )%>%
#na.omit(db_indices[, c(1,3,9,10,21,22)]) %>%
 #gather("Flesch_Kincaid_Reading_Ease_Score","Spache_Score",key="index", value="score") %>%
  #gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor),color=Country))+
  #geom_boxplot(aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor)))+
  geom_point(aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor), color=Country, size=total_score,shape=abstract_format))+
  #scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))+
  theme(legend.position="right")+
  xlab("Gunning Fox Index")+
  ylab("")


na.omit(db_indices[, c(1,2,4,6, 8, 9,10, 16, 17, 21,22,23)]) %>%
  mutate(
    Country=recode_factor(Country, "Arabian Emirates"="Others","Brazil"="Others", "China"="Others", "Denmark"="Others", "Germany"="Others", "India"="Others", "Saudi Arabia"="Others","Italy"="Others", "Spain"="Others", "Sweden"="Others", "Switzerland"="Others", "United Kingdom"="UK", "United States"="USA")
  )%>%
#na.omit(db_indices[, c(1,3,9,10,21,22)]) %>%
 #gather("Flesch_Kincaid_Reading_Ease_Score","Spache_Score",key="index", value="score") %>%
  #gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(x = Average_Grade_Level, fill=AMSTAR_levels, color=AMSTAR_levels))+
  geom_density(alpha = 0.1)+
  #geom_boxplot(aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor)))+
  #scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))+
  facet_wrap( ~ abstract_format)+
  theme(legend.position="right")+
  xlab("Average Grade Level")+
  ylab("")


na.omit(db_indices[, c(1,2,4,6, 8, 9,10, 16, 17, 21,22,23)]) %>%
  mutate(
    Country=recode_factor(Country, "Arabian Emirates"="Others","Brazil"="Others", "China"="Others", "Denmark"="Others", "Germany"="Others", "India"="Others", "Saudi Arabia"="Others","Italy"="Others", "Spain"="Others", "Sweden"="Others", "Switzerland"="Others", "United Kingdom"="UK", "United States"="USA")
  )%>%
#na.omit(db_indices[, c(1,3,9,10,21,22)]) %>%
 #gather("Flesch_Kincaid_Reading_Ease_Score","Spache_Score",key="index", value="score") %>%
  #gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(x = SMOG_Index, fill=AMSTAR_levels, color=AMSTAR_levels))+
  geom_density(alpha = 0.1)+
  #geom_boxplot(aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor)))+
  #scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))+
  facet_wrap( ~ abstract_format)+
  theme(legend.position="right")+
  xlab("SMOG Index")+
  ylab("")


na.omit(db_indices[, c(1,2,3, 4,6,7, 8, 9,10, 16, 17, 21,22,23)]) %>%
  mutate(
    Country=recode_factor(Country, "Arabian Emirates"="Others","Brazil"="Others", "China"="Others", "Denmark"="Others", "Germany"="Others", "India"="Others", "Saudi Arabia"="Others","Italy"="Others", "Spain"="Others", "Sweden"="Others", "Switzerland"="Others", "United Kingdom"="UK", "United States"="USA")
  )%>%
#na.omit(db_indices[, c(1,3,9,10,21,22)]) %>%
 #gather("Flesch_Kincaid_Reading_Ease_Score","Spache_Score",key="index", value="score") %>%
  #gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(x = Automated_Readability_Index, fill=AMSTAR_levels, color=AMSTAR_levels))+
  geom_density(alpha = 0.1)+
  #geom_boxplot(aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor)))+
  #scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))+
  facet_wrap( ~ abstract_format)+
  theme(legend.position="right")+
  xlab("Automated_Readability_Index")+
  ylab("")




na.omit(db_indices[, c(1,2,3, 4,5,6,7, 8, 9,10, 16, 17, 21,22,23)]) %>%
  mutate(
    Country=recode_factor(Country, "Arabian Emirates"="Others","Brazil"="Others", "China"="Others", "Denmark"="Others", "Germany"="Others", "India"="Others", "Saudi Arabia"="Others","Italy"="Others", "Spain"="Others", "Sweden"="Others", "Switzerland"="Others", "United Kingdom"="UK", "United States"="USA")
  )%>%
#na.omit(db_indices[, c(1,3,9,10,21,22)]) %>%
 #gather("Flesch_Kincaid_Reading_Ease_Score","Spache_Score",key="index", value="score") %>%
  #gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(x = Coleman_Liau_Index, fill=AMSTAR_levels, color=AMSTAR_levels))+
  geom_density(alpha = 0.1)+
  #geom_boxplot(aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor)))+
  #scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))+
  facet_wrap( ~ abstract_format)+
  theme(legend.position="right")+
  xlab("Coleman_Liau_Index")+
  ylab("")


na.omit(db_indices[, c(1,2,3, 4,5,6,7, 8, 9,10,12, 16, 17, 21,22,23)]) %>%
  mutate(
    Country=recode_factor(Country, "Arabian Emirates"="Others","Brazil"="Others", "China"="Others", "Denmark"="Others", "Germany"="Others", "India"="Others", "Saudi Arabia"="Others","Italy"="Others", "Spain"="Others", "Sweden"="Others", "Switzerland"="Others", "United Kingdom"="UK", "United States"="USA")
  )%>%
#na.omit(db_indices[, c(1,3,9,10,21,22)]) %>%
 #gather("Flesch_Kincaid_Reading_Ease_Score","Spache_Score",key="index", value="score") %>%
  #gather("Flesch_Kincaid_Reading_Ease_Level", "Gunning_Fog_Index", "Coleman_Liau_Index", "SMOG_Index", "Automated_Readability_Index", "Average_Grade_Level", "Flesch_Kincaid_Reading_Ease_Score", "Spache_Score", "New_Dale_Chall_Score", key="index", value="score") %>%
  ggplot(., aes(x = Flesch_Kincaid_Reading_Ease_Level, fill=AMSTAR_levels, color=AMSTAR_levels))+
  geom_density(alpha = 0.1)+
  #geom_boxplot(aes(x = Gunning_Fog_Index, y=reorder(journal, journal_impact_factor)))+
  #scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(face="bold", angle=90, margin = margin(0), hjust=0.95,vjust=0.2, size=7))+
  facet_wrap( ~ abstract_format)+
  theme(legend.position="right")+
  xlab("Flesch_Kincaid_Reading_Ease_Level")+
  ylab("")


