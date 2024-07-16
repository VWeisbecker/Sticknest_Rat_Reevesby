library(readxl)
library(tidyverse)


library(rstudioapi) #  Access the RStudio API
setwd(dirname(getActiveDocumentContext()$path))

#reading in data 
read_excel ("Reevesby vegetation and stick nest rats.xlsx", sheet=1)-> Reevesbypointintercept
read_excel ("Reevesby vegetation and stick nest rats.xlsx", sheet=3)-> Reevesbycuticle



#count each species in each plot - this is the basis for the "available' in Fig. 4.
interceptcount <- Reevesbypointintercept %>% 
  dplyr::select(Plotname = `Plot Name`, Transectno=`Trasect No.`, Pointno = `Point number`, Species) %>% 
  group_by(Plotname, Species) %>% 
  summarise(Count = n ()) %>% 
  filter(!is.na(Species)) %>% 
  identity()

#counts of cuticle in the scats (the "used" in Fig. 4). 
Reevesbycuticle %>% dplyr::select(Plotname = `Plot Name`, Samplecount = `sample count` , Species) %>% group_by(Plotname, Species) %>% summarise (Count = sum(Samplecount)) %>% 
  filter(!is.na(Species)) -> cuticlecount

#Just looking at 9001 because that is the plot where all Leporillus activity was.
interceptcount %>% filter(Plotname==9001) %>% .$Count %>% set_names(filter(interceptcount,Plotname==9001)$Species) -> interceptcount9001

interceptcount9001[sort(names(interceptcount9001))] -> interceptcount9001

#Ratio of single species counts divided by the number of overall counts; and sort alphabetically
interceptcount9001/(sum(interceptcount9001)) -> interceptratio9001
interceptratio9001[sort(names(interceptratio9001))] -> interceptratio9001

#species that were available as per interceptcount but were not found in the scats
interceptcount %>% filter(Plotname==9001) %>% anti_join(filter(cuticlecount, Plotname==9001), by = "Species") -> notmatched


#Adding the species with a species count of 0 to the list to get a full cuticle count table
cuticlecount %>% filter(Plotname==9001) %>% .$Count %>% append(rep(0,7)) %>%  
  set_names(c(filter(cuticlecount,Plotname==9001)$Species, notmatched$Species)) -> cuticlecount9001

cuticlecount9001 <- cuticlecount9001[sort(names(cuticlecount9001))]

#Ratio of cuticle counts for each species relative to total cuticle count.
cuticlecount9001/sum(cuticlecount9001) -> cuticleratio9001


#pull out count for 9001, add 10 0's. name the first part after the species there, 34 others after not matched
library(ade4)
library(adehabitatHS)

#Manyly Selection ratio - this is in Table 4 and base data for Fig. 5

widesI(cuticlecount9001, interceptcount9001, avknown = FALSE, alpha = 0.05) -> results9001
summary(results9001)
print(results9001)

library(gt)
library(broom)


library(ggtext)

#Available vs used
theme <- theme(legend.title = element_blank(),
               axis.text.y = element_markdown(color = "black", face = "italic"))

plot_df1 <- bind_cols(results9001[1:4], Species = names(results9001[[1]])) %>% 
  rename(used.se = se.used, avail.se = se.avail) %>%
  pivot_longer(c(used.prop:avail.se), names_to = c("Type", "Metric"), values_to = "Value", names_sep = "\\.") %>%
  pivot_wider(names_from = "Metric", values_from = "Value") %>%
  mutate(Type = if_else(Type=="avail", "Available", "Used"), prop = prop*100, se = se*100) %>%
  identity()

ggplot(plot_df1, aes(y = Species, x = prop, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbarh(aes(xmin = prop-se, xmax = prop+se), position = "dodge") +
  scale_fill_brewer(type = "qual", palette = "Accent") +
  theme_bw() +
  theme +
  xlab("Percentage") +
  geom_blank()



#Fig. 5 - Selection ratio - SE



plot_df2 <- bind_cols(results9001[5:6], Species = names(results9001[[1]]))

ggplot(plot_df2, aes(y = Species, x = wi)) +
  geom_point() +
  geom_linerange(aes(xmin = wi-se.wi, xmax = wi + se.wi)) +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_bw() +
  theme +
  xlab("Selection Ratio")


#Fig. 5 - Selection ratio

#computing conficence interval as CI=mean+se*qnorm(0.975) [https://stats.stackexchange.com/questions/512789/converting-between-confidence-interval-and-standard-error/512794]


plot_df2 <- bind_cols(results9001[5:6], Species = names(results9001[[1]]))

ggplot(plot_df2, aes(y = Species, x = wi)) +
  geom_point() +
  geom_linerange(aes(
    xmin = wi-(wi-se.wi*qnorm(0.975)), 
    xmax = wi + (wi-se.wi*qnorm(0.975)))
    ) +
  geom_vline(xintercept = 1, linetype = "dotted") +
  theme_bw() +
  theme +
  xlab("Selection Ratio")










