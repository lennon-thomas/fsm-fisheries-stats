
#Read in and plot FSM catch data from FIRMS, level 0 https://zenodo.org/records/11410529
# 7/12/24
#Lennon Thomas

library(tidyr)
library(tidyverse)
library(ggplot2)
source("data-directory-base.R")
# make function. plot for species purr
dat<-read.csv(paste0(data_directory_base,"/blue-prosperity-coalition/data/FSM/in-eez-catch-gear/fsm_eez_catch.csv"))


tuna_catch<-dat %>%
dplyr:: filter(ez_id=="FM") %>%
  select(yr:yft_c) %>%
gather (key="species",value="catch",alb_c,bet_c,blm_c,bum_c,mls_c,skj_c,swo_c,yft_c)


bycatch<-tuna_catch %>%
 dplyr:: filter(species=="blm_c"|species== "bum_c"|species=="mls_c"|species=="swo_c") %>%
  dplyr::group_by(yr)%>%
  dplyr::mutate(total_catch=sum(catch))

bycatch_sp<-bycatch %>%
  dplyr::group_by(yr,species) %>%
  dplyr::summarise(total_catch=sum(catch))

by<-ggplot(bycatch_sp,aes(x=yr,y=total_catch,fill=species)) +
  geom_col()+
  labs(x="Year",y="Total catch (mt)") +
  theme_bw()

by + scale_fill_discrete(name="Species",
                         labels= c("Black marlin",
                                   "Blue marlin",
                                   "Striped marlin",
                                   "Swordfish"))+
  # values=c("#F8766D","#00BA38","#619CFF","#C77CFF")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,450)) +
  scale_x_continuous(expand=c(0,0))

bycatch_gr<-bycatch %>%
  dplyr::group_by(yr,gr_id) %>%
  dplyr::summarise(total_catch=sum(catch))


by<-ggplot(bycatch_gr,aes(x=yr,y=total_catch,fill=gr_id)) +
  geom_col()+
  labs(x="Year",y="Total catch (mt)") +
  theme_bw()

by + scale_fill_discrete(name="Species",
                         labels= c("Black marlin",
                                   "Blue marlin",
                                   "Striped marlin",
                                   "Swordfish"))+
  # values=c("#F8766D","#00BA38","#619CFF","#C77CFF")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,450)) +
  scale_x_continuous(expand=c(0,0))



conditions<-c("blm_c","bum_c","mls_c","swo_c")
replacement<-c("other","other","other","other")
tuna_catch$species<-replace(tuna_catch$species, tuna_catch$species %in% conditions,replacement)

catch_gr_sp<-tuna_catch %>%
  dplyr::group_by(yr,species,gr_id)%>%
  dplyr::summarise (total_catch=sum(catch,na.rm=TRUE))


gear<-catch_gr_sp %>%
 dplyr::group_by(yr,gr_id) %>%
  dplyr::summarise(total_gear=sum(total_catch))



total_annual_gr_sp<-catch_sp %>%
  dplyr:: group_by(yr,species,gr_id) %>%
  dplyr::mutate (all_total_catch=sum(total_catch,na.rm=TRUE)) 
perc_total_gr_sp<-total_annual_gr_sp %>%
  dplyr::mutate(percent_total=total_catch/all_total_catch*100)



catch_gr<-tuna_catch %>%
  dplyr::group_by(yr,gr_id)%>%
  dplyr::summarise (total_catch=sum(catch,na.rm=TRUE))

total_annual_gr<-catch_gr %>%
  dplyr:: group_by(yr) %>%
  dplyr::mutate (all_total_catch=sum(total_catch,na.rm=TRUE)) 
perc_total_gr<-total_annual_gr%>%
  dplyr::mutate(percent_total=total_catch/all_total_catch*100)


sp<-ggplot(perc_total_gr,aes(yr,total_catch,fill=gr_id)) +
  geom_col() +
  labs(x="Year",y="Total catch (mt)") +
theme_bw () 

sp<- sp+
  scale_fill_discrete(name="Gear Type",
                    labels= c("Longline",
                              "Pole and line",
                              "Purse seine"))+
                    #values=c("#F8766D", "#7CAE00","navy", "#00BFC4", "#C77CFF"))+
  scale_y_continuous(expand=c(0,0),limits=c(0,3.5e+05)) +
  scale_x_continuous(expand=c(0,0)) 




sp<-tuna_catch %>%
dplyr:: group_by(yr,species) %>%
  dplyr::summarise(total_catch=sum(catch))



total_annual<-sp %>%
  dplyr:: group_by(yr) %>%
  dplyr::mutate (all_total_catch=sum(total_catch,na.rm=TRUE)) 
perc_total_sp<-total_annual %>%
  dplyr::mutate(percent_total=total_catch/all_total_catch*100)


               
View(perc_total_sp)


ps<-catch_gr_sp%>%
  dplyr::filter(gr_id=="S") %>%
  dplyr::filter(total_catch!= 0) %>%
  dplyr:: group_by(yr) %>%
  dplyr::mutate(total_annual_ll=sum(total_catch)) %>%
  dplyr::mutate(perc_catch=total_catch/total_annual_ll*100)

ll<-catch_gr_sp%>%
  dplyr::filter(gr_id=="L") %>%
  dplyr::filter(total_catch!= 0)%>%
  dplyr:: group_by(yr) %>%
  dplyr::mutate(total_annual_ll=sum(total_catch)) %>%
  dplyr::mutate(perc_catch=total_catch/total_annual_ll*100)



p<-catch_sp%>%
  dplyr::filter(gr_id=="P") %>%
  dplyr::filter(total_catch!= 0)

gear<-catch_sp %>%
  dplyr::group_by(yr,gr_id)%>%
  dplyr::summarise(total_gear_catch=sum(total_catch))



bet_total<-catch_sp%>%
  dplyr::filter(species=="bet_c") %>%
  dplyr::filter(total_catch!= 0) %>%
  dplyr::group_by(yr)%>%
    dplyr::mutate(total_gear=sum(total_catch))

bet<-bet_total %>%
  dplyr::mutate(perc_total=total_catch/total_gear*100)

bet_plot<-ggplot(bet,aes(x=yr,y=total_catch,fill=gr_id))+ 
  geom_col()+
  labs(x="Year",y="Bigeye tuna total catch (mt)") +
  theme_bw() 
  
bet_plot+  
  scale_fill_discrete(name="Gear",
                      labels= c("longline",
                                "pole and line",
                                "purse seince"))+
  # values=c("#F8766D","#00BA38","#619CFF","#C77CFF")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,15000)) +
  scale_x_continuous(expand=c(0,0))
  

yft_c_total<-catch_sp%>%
  dplyr::filter(species=="yft_c") %>%
  dplyr::filter(total_catch!= 0) %>%
  dplyr::group_by(yr)%>%
  dplyr::mutate(total_gear=sum(total_catch))

yft<-yft_c_total %>%
  dplyr::mutate(perc_total=total_catch/total_gear*100)

yft_plot<-ggplot(yft,aes(x=yr,y=total_catch,fill=gr_id))+ 
  geom_col()+
  labs(x="Year",y="Yellowfin tuna total catch (mt)") +
  theme_bw() 

yft_plot+  
  scale_fill_discrete(name="Gear",
                      labels= c("longline",
                                "pole and line",
                                "purse seine"))+
  # values=c("#F8766D","#00BA38","#619CFF","#C77CFF")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,50000)) +
  scale_x_continuous(expand=c(0,0))

skj_plot+  
  scale_fill_discrete(name="Gear",
                      labels= c("longline",
                                "pole and line",
                                "purse seine"))+
  # values=c("#F8766D","#00BA38","#619CFF","#C77CFF")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,250000)) +
  scale_x_continuous(expand=c(0,0))




alb_c_total<-catch_sp%>%
  dplyr::filter(species=="alb_c") %>%
  dplyr::filter(total_catch!= 0) %>%
  dplyr::group_by(yr)%>%
  dplyr::mutate(total_gear=sum(total_catch))

alb<-alb_c_total %>%
  dplyr::mutate(perc_total=total_catch/total_gear*100)

alb_plot<-ggplot(alb,aes(x=yr,y=total_catch,fill=gr_id))+ 
  geom_col()+
  labs(x="Year",y="Albacore tuna total catch (mt)") +
  theme_bw() 

alb_plot+  
  scale_fill_discrete(name="Gear",
                      labels= c("longline",
                                "purse seine"))+
  # values=c("#F8766D","#00BA38","#619CFF","#C77CFF")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,500)) +
  scale_x_continuous(expand=c(0,0))



sp<-ggplot(catch_sp,aes(yr,total_catch,fill=species)) +
  geom_col() +
  labs(x="Year",y="Total catch (mt)")
  theme_bw () 
  
 sp<- sp+
 scale_fill_manual(name="Species",
                   labels= c("albacore",
                              "bigeye",
                               "other",
                               "skipjack",
                                "yellowfin"),
                   values=c("#F8766D", "#7CAE00","navy", "#00BFC4", "#C77CFF"))+
  scale_y_continuous(expand=c(0,0),limits=c(0,3.5e+05)) +
   scale_x_continuous(expand=c(0,0)) +
   theme_bw()
 

 
 

 ps_plot<-ggplot(ps,aes(yr,total_catch,fill=species)) +
   geom_col() +
   labs(x="Year",y="Purse seine total catch (mt)") +
 theme_bw() +
scale_fill_discrete(name="Species",
                       labels= c("albacore",
                                 "bigeye",
                                 "skipjack",
                                 "yellowfin"))+
                   # values=c("#F8766D","#00BA38","#619CFF","#C77CFF")) +
   scale_y_continuous(expand=c(0,0),limits=c(0,350000)) +
   scale_x_continuous(expand=c(0,0)) +
   theme_bw()
 hex=hue_pal()(5)
 
 ll_plot<-ggplot(ll,aes(yr,total_catch,fill=species)) +
   geom_col() +
   labs(x="Year",y="Longline total catch (mt)") +
   theme_bw() +
   scale_fill_viridis_b()+
   scale_fill_manual(name="Species",
                       labels= c("albacore",
                                 "bigeye",
                                 "other",
                                 "skipjack",
                                 "yellowfin"),
                      values=c("#F8766D", "#7CAE00","navy", "#00BFC4", "#C77CFF")
                       ) +
   scale_y_continuous(expand=c(0,0),limits=c(0,8500)) +
   scale_x_continuous(expand=c(0,0)) +
   theme_bw()
 
 p_plot<-ggplot(p,aes(yr,total_catch,fill=species)) +
   geom_col() +
   labs(x="Year",y="Pole and line total catch (mt)") +
   theme_bw() +
   scale_fill_viridis_b()+
   scale_fill_manual(name="Species",
                     labels= c(
                               "bigeye",
                               "skipjack",
                               "yellowfin"),
                     values=c("#7CAE00", "#00BFC4", "#C77CFF")
   ) +
   scale_y_continuous(expand=c(0,0),limits=c(0,5000)) +
   scale_x_continuous(expand=c(0,0)) +
   theme_bw()
 
 
ggplot(skj,aes(x=yr,y=catch,col=gr_id)) +
  geom_line() 

ps$yr<-as.factor(ps$yr)

quick_check<-ps %>%
  dplyr::filter(yr=="2021")

sum(quick_check$total_catch)
