library(tidyverse)
library(plyr)
library(reshape2)
library(scales)
fruitfly<-read.csv("Fruit-Fly_clean2.csv")

#A) Drop the pilot data
ffc1<-subset(fruitfly, X==23 | X>24)

#B) Creating dummy variable for rushed surveys, then drop rushed surveys
ffc1$rushed_survey <- with(ffc1, sec_Aq1 == "Justus kimatu mwova" | sec_Aq1 == "Pius muema kimeu" | sec_Aq1 == "John M. Matheka" | sec_Aq1 == "Wanza mutua")
ffc<-subset(ffc1, rushed_survey=="FALSE")

#BC) Creating dummy variables for mango tree variety
ffc$ngowe<-as.numeric(ffc$sec_Aq144_1=="Ngowe")
ffc$apple<-as.numeric(ffc$sec_Aq144_1=="Apple" | ffc$sec_Aq144_2=="Apple")
ffc$kent<-as.numeric(ffc$sec_Aq144_1=="Kent" | ffc$sec_Aq144_2=="Kent" | ffc$sec_Aq144_3=="Kent")

#BBC) Transforming Yes/No into 1/0 for ownership proxies
f_yes_no <- function(x) {as.numeric(as.character(plyr::mapvalues(x, from = c("Yes", "No"),to = c(1, 0))))}
vars_ownership<-c("sec_Aass_q1411", "sec_Aass_q1412", "sec_Aass_q1413", "sec_Aass_q1414", "sec_Aass_q1415", "sec_Aass_q1416", "sec_Aass_q1417", "sec_Aass_q1418", "sec_Aass_q1419", "sec_Aass_q1420", "sec_Aass_q1421", "sec_Aass_q1422", "sec_Aass_q1423", "sec_Aass_q1424", "sec_Aass_q1425", "sec_Aass_q1426", "sec_Aass_q1427", "sec_Aass_q1428")
ffc <- ffc %>% mutate_at(vars_ownership, f_yes_no)

#C) Cleaning the Data
ffc <- ffc %>% mutate(sec_Aq10=ifelse(sec_Aq10>99,2019-sec_Aq10,sec_Aq10))
ffc <- ffc %>% mutate(sec_Aq14=ifelse(sec_Aq14<1000, NA,sec_Aq14))
ffc <- ffc %>% mutate(sec_Aq140=ifelse(sec_Aq140<1000, NA,sec_Aq140))
ffc <- ffc %>% mutate(sec_Bsec_B_aq133=ifelse(sec_Bsec_B_aq133<1, -99 ,sec_Bsec_B_aq133))
ffc <- ffc %>% mutate(sec_Bsec_B_aq133=ifelse(sec_Bsec_B_aq133==99, -99 ,sec_Bsec_B_aq133))
ffc <- ffc %>% mutate(sec_Bsect_B_bq151=ifelse(sec_Bsect_B_bq151<1, -99 , sec_Bsect_B_bq151))
ffc <- ffc %>% mutate(sec_Bsect_B_bq151=ifelse(sec_Bsect_B_bq151==99, -99 , sec_Bsect_B_bq151))
ffc <- ffc %>% mutate(sec_CUse_q168=ifelse(sec_CUse_q168<1, -99 , sec_CUse_q168))
ffc <- ffc %>% mutate(sec_CUse_q168=ifelse(sec_CUse_q168==99, -99 , sec_CUse_q168))
ffc <- ffc %>% mutate(sec_Bsec_B_aq134=ifelse(sec_Bsec_B_aq134<0, NA , sec_Bsec_B_aq134))
ffc <- ffc %>% mutate(sec_Bsec_B_aq134=ifelse(sec_Bsec_B_aq134==99, NA , sec_Bsec_B_aq134))
ffc <- ffc %>% mutate(sec_Bsec_B_aq135=ifelse(sec_Bsec_B_aq135<0, NA , sec_Bsec_B_aq135))
ffc <- ffc %>% mutate(sec_Bsec_B_aq135=ifelse(sec_Bsec_B_aq135==99, NA , sec_Bsec_B_aq135))
ffc <- ffc %>% mutate(sec_Bsect_B_bq152=ifelse(sec_Bsect_B_bq152<0, NA , sec_Bsect_B_bq152))
ffc <- ffc %>% mutate(sec_Bsect_B_bq152=ifelse(sec_Bsect_B_bq152==99, NA , sec_Bsect_B_bq152))
ffc <- ffc %>% mutate(sec_Bsect_B_bq153=ifelse(sec_Bsect_B_bq153<0, NA , sec_Bsect_B_bq153))
ffc <- ffc %>% mutate(sec_Bsect_B_bq153=ifelse(sec_Bsect_B_bq153==99, NA , sec_Bsect_B_bq153))
ffc <- ffc %>% mutate(sec_Cq187=ifelse(sec_Cq187>99, NA , sec_Cq187))

#CD) Creating dummy variables for each challenge that farmers might be facing
ffc$pests<-as.numeric(ffc$sec_Bq21_1=="Pests and diseases")
ffc$high_cost_traps<-as.numeric(ffc$sec_Bq21_1=="High cost of fly traps" | ffc$sec_Bq21_2=="High cost of fly traps")
ffc$road_infrastructure<-as.numeric(ffc$sec_Bq21_1=="Poor road infrastructure" | ffc$sec_Bq21_2=="Poor road infrastructure" | ffc$sec_Bq21_3=="Poor road infrastructure")
ffc$exploitation<-as.numeric(ffc$sec_Bq21_1=="Exploitation from middlemen/brokers" | ffc$sec_Bq21_2=="Exploitation from middlemen/brokers" | ffc$sec_Bq21_3=="Exploitation from middlemen/brokers" | ffc$sec_Bq21_4=="Exploitation from middlemen/brokers")
ffc$low_prices<-as.numeric(ffc$sec_Bq21_1=="Low selling prices for the mangoes" | ffc$sec_Bq21_2=="Low selling prices for the mangoes" | ffc$sec_Bq21_3=="Low selling prices for the mangoes" | ffc$sec_Bq21_4=="Low selling prices for the mangoes" | ffc$sec_Bq21_5=="Low selling prices for the mangoes")
ffc$inadequate_land<-as.numeric(ffc$sec_Bq21_1=="Inadequate land to farm mangoes" | ffc$sec_Bq21_2=="Inadequate land to farm mangoes" | ffc$sec_Bq21_3=="Inadequate land to farm mangoes" | ffc$sec_Bq21_4=="Inadequate land to farm mangoes" | ffc$sec_Bq21_5=="Inadequate land to farm mangoes" | ffc$sec_Bq21_6=="Inadequate land to farm mangoes")
ffc$inadequate_market<-as.numeric(ffc$sec_Bq21_1=="Inadequate ready market" | ffc$sec_Bq21_2=="Inadequate ready market" | ffc$sec_Bq21_3=="Inadequate ready market" | ffc$sec_Bq21_4=="Inadequate ready market" | ffc$sec_Bq21_5=="Inadequate ready market" | ffc$sec_Bq21_6=="Inadequate ready market" | ffc$sec_Bq21_7=="Inadequate ready market")
ffc$inadequate_labour<-as.numeric(ffc$sec_Bq21_1=="Inadequate labour - for spraying, weeding, harvesting, transportation" | ffc$sec_Bq21_2=="Inadequate labour - for spraying, weeding, harvesting, transportation" | ffc$sec_Bq21_3=="Inadequate labour - for spraying, weeding, harvesting, transportation" | ffc$sec_Bq21_4=="Inadequate labour - for spraying, weeding, harvesting, transportation" | ffc$sec_Bq21_5=="Inadequate labour - for spraying, weeding, harvesting, transportation" | ffc$sec_Bq21_6=="Inadequate labour - for spraying, weeding, harvesting, transportation" | ffc$sec_Bq21_7=="Inadequate labour - for spraying, weeding, harvesting, transportation" | ffc$sec_Bq21_8=="Inadequate labour - for spraying, weeding, harvesting, transportation")
ffc$weather_climate<-as.numeric(ffc$sec_Bq21_1=="Unfavourable weather/climatic conditions" | ffc$sec_Bq21_2=="Unfavourable weather/climatic conditions" | ffc$sec_Bq21_3=="Unfavourable weather/climatic conditions" | ffc$sec_Bq21_4=="Unfavourable weather/climatic conditions" | ffc$sec_Bq21_5=="Unfavourable weather/climatic conditions" | ffc$sec_Bq21_6=="Unfavourable weather/climatic conditions" | ffc$sec_Bq21_7=="Unfavourable weather/climatic conditions" | ffc$sec_Bq21_8=="Unfavourable weather/climatic conditions" | ffc$sec_Bq21_9=="Unfavourable weather/climatic conditions")
ffc$other<-ffc$sec_Bq22

#Replacing NAs with 0s in dummy variables for challenges
vars_challenges<-c("pests", "high_cost_traps", "road_infrastructure", "exploitation", "low_prices", "inadequate_land", "inadequate_market", "inadequate_labour", "weather_climate")
replace_na_challenges<- function(x) { replace(x, is.na(x), 0) }
ffc<- ffc %>% mutate_at(vars_challenges, replace_na_challenges)

#Creating a correlation matrix to see which challenges are correlated
ffc_challenges<-select(ffc, pests:weather_climate)
cor(ffc_challenges)

#CDD) Creating dummy variables for mango pests that farmers are aware of
ffc$fruit_fly<-as.numeric(ffc$sec_Bq123_1=="Fruit fly/Nzi wa matunda/Yiui ya Maembe")
ffc$mango_seed_wevil<-as.numeric(ffc$sec_Bq123_1=="Mango seed weevil/Kingolondo" | ffc$sec_Bq123_2=="Mango seed weevil/Kingolondo")
ffc$thrips<-as.numeric(ffc$sec_Bq123_1=="Thrips/Mbaa" | ffc$sec_Bq123_2=="Thrips/Mbaa" | ffc$sec_Bq123_3=="Thrips/Mbaa")
ffc$aphids<-as.numeric(ffc$sec_Bq123_1=="Aphids/Ndaa ya Maembe" | ffc$sec_Bq123_2=="Aphids/Ndaa ya Maembe" | ffc$sec_Bq123_3=="Aphids/Ndaa ya Maembe" | ffc$sec_Bq123_4=="Aphids/Ndaa ya Maembe")

#D) Creating new variables for knowledge of Pests and PCT
ffc$knowledge1<-as.numeric(as.character(mapvalues(ffc$sec_Bq123_1,from = c("Fruit fly/Nzi wa matunda/Yiui ya Maembe","Thrips/Mbaa", "Mango seed weevil/Kingolondo", "Aphids/Ndaa ya Maembe", "Other (specify)"), c(1, 1, 1, 1, 1))))
ffc$knowledge2<-as.numeric(as.character(mapvalues(ffc$sec_Bq123_2,from = c("Fruit fly/Nzi wa matunda/Yiui ya Maembe","Thrips/Mbaa", "Mango seed weevil/Kingolondo", "Aphids/Ndaa ya Maembe", "Other (specify)"), c(1, 1, 1, 1, 1))))
ffc$knowledge3<-as.numeric(as.character(mapvalues(ffc$sec_Bq123_3,from = c("Fruit fly/Nzi wa matunda/Yiui ya Maembe","Thrips/Mbaa", "Mango seed weevil/Kingolondo", "Aphids/Ndaa ya Maembe", "Other (specify)"), c(1, 1, 1, 1, 1))))
ffc$knowledge4<-as.numeric(as.character(mapvalues(ffc$sec_Bq123_4,from = c("Fruit fly/Nzi wa matunda/Yiui ya Maembe","Thrips/Mbaa", "Mango seed weevil/Kingolondo", "Aphids/Ndaa ya Maembe", "Other (specify)"), c(1, 1, 1, 1, 1))))
ffc$knowledge5<-as.numeric(as.character(mapvalues(ffc$sec_Bq123_5,from = c("Fruit fly/Nzi wa matunda/Yiui ya Maembe","Thrips/Mbaa", "Mango seed weevil/Kingolondo", "Aphids/Ndaa ya Maembe", "Other (specify)"), c(1, 1, 1, 1, 1))))
ffc$knowledge6<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 1, 1, 1, 1))))
ffc$knowledge7<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124b,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 1, 1, 1, 1))))
ffc$knowledge8<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124c,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 1, 1, 1, 1))))
ffc$knowledge9<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124d,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 1, 1, 1, 1))))
ffc$knowledge10<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124e,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 1, 1, 1, 1))))
ffc$knowledge11<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124f,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 1, 1, 1, 1))))
ffc$knowledge12<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124g,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 1, 1, 1, 1))))

#E) Creating groups of farmers with similar knowledge levels on Pests and PCT. First index is pests and PCTs, second index is only pests, third index is only PCTs (all of them)
ffc$knowledge_groups <- rowSums(ffc[,c("knowledge1", "knowledge2", "knowledge3", "knowledge4", "knowledge5", "knowledge6", "knowledge7", "knowledge8")], na.rm=TRUE)
ffc$pest_knowledge_groups <- rowSums(ffc[,c("knowledge1", "knowledge2", "knowledge3", "knowledge4", "knowledge5")], na.rm=TRUE)
ffc$pct_knowledge_groups <- rowSums(ffc[,c("knowledge6", "knowledge7", "knowledge8", "knowledge9", "knowledge10", "knowledge11", "knowledge12")], na.rm=TRUE)

#F) Create variables for farmers engagement with PCT
ffc$engagement1<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 0, 0, 1, 2))))
ffc$engagement2<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124b,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 0, 0, 1, 2))))
ffc$engagement3<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124c,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 0, 0, 1, 2))))
ffc$engagement4<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124d,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 0, 0, 1, 2))))
ffc$engagement5<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124e,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 0, 0, 1, 2))))
ffc$engagement6<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124f,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 0, 0, 1, 2))))
ffc$engagement7<-as.numeric(as.character(mapvalues(ffc$sec_Bengage_q124g,from = c("I do not know about it","I have heard of it", " have actually used it once", " I sometimes use it", "I use it all the time"), c(0, 0, 0, 1, 2))))
ffc$engagement_pesticides<-ffc$engagement1
ffc$engagement_traps<-ffc$engagement2
ffc$engagement_baits<-ffc$engagement3

#G) Creating groups of farmers with similar engagement with PCTs (excluding orchard bags, soil treatments etc.) by summing ffc$engagement1:ffc$engagement7
ffc$engagement_groups <- rowSums(ffc[,c("engagement1", "engagement2", "engagement3")], na.rm=TRUE)

#GHH) Creating dummies for reasons farmers use PCT
ffc$traps_effective<-as.numeric(ffc$sec_Bsec_B_aq138_1=="They have been successful and effective")
ffc$traps_cheaper<-as.numeric(ffc$sec_Bsec_B_aq138_1=="They are cheaper than other pest management strategies" | ffc$sec_Bsec_B_aq138_2=="They are cheaper than other pest management strategies")
ffc$traps_other_farmers<-as.numeric(ffc$sec_Bsec_B_aq138_1==" I have seen other farmers successfully use them" | ffc$sec_Bsec_B_aq138_2==" I have seen other farmers successfully use them" | ffc$sec_Bsec_B_aq138_3==" I have seen other farmers successfully use them")
ffc$traps_neighbours<-as.numeric(ffc$sec_Bsec_B_aq138_1=="All my neighbours are using them" | ffc$sec_Bsec_B_aq138_2=="All my neighbours are using them" | ffc$sec_Bsec_B_aq138_3=="All my neighbours are using them")

ffc$pesticides_effective<-as.numeric(ffc$sec_Bsect_B_bq156_1=="They have been successful and effective")
ffc$pesticides_cheaper<-as.numeric(ffc$sec_Bsect_B_bq156_1=="They are cheaper than other pest management strategies" | ffc$sec_Bsect_B_bq156_2=="They are cheaper than other pest management strategies")
ffc$pesticides_other_farmers<-as.numeric(ffc$sec_Bsect_B_bq156_1==" I have seen other farmers successfully use them" | ffc$sec_Bsect_B_bq156_2==" I have seen other farmers successfully use them" | ffc$sec_Bsect_B_bq156_3==" I have seen other farmers successfully use them")
ffc$pesticides_neighbours<-as.numeric(ffc$sec_Bsect_B_bq156_1=="All my neighbours are using them" | ffc$sec_Bsect_B_bq156_2=="All my neighbours are using them" | ffc$sec_Bsect_B_bq156_3=="All my neighbours are using them")

#GH) Creating dummy variables for reasons farmers are not using PCT
ffc$traps_ineffective<-as.numeric(ffc$sec_Bsec_B_aq141_1=="They do not work/ineffective")
ffc$traps_expensive<-as.numeric(ffc$sec_Bsec_B_aq141_1=="They are expensive/high cost of the traps" | ffc$sec_Bsec_B_aq141_2=="They are expensive/high cost of the traps")
ffc$traps_dont_know_use<-as.numeric(ffc$sec_Bsec_B_aq141_1=="I don<cd>t know how to use them" | ffc$sec_Bsec_B_aq141_2=="I don<cd>t know how to use them" | ffc$sec_Bsec_B_aq141_3=="I don<cd>t know how to use them")
ffc$traps_comfortable<-as.numeric(ffc$sec_Bsec_B_aq141_1=="I am comfortable using my current method of pest control" | ffc$sec_Bsec_B_aq141_2=="I am comfortable using my current method of pest control" | ffc$sec_Bsec_B_aq141_3=="I am comfortable using my current method of pest control")
ffc$traps_bad_experience<-as.numeric(ffc$sec_Bsec_B_aq141_1==" I had a previous bad experience " | ffc$sec_Bsec_B_aq141_2==" I had a previous bad experience " | ffc$sec_Bsec_B_aq141_3==" I had a previous bad experience ")
ffc$traps_noone_uses_them<-as.numeric(ffc$sec_Bsec_B_aq141_1=="No one else is using them" | ffc$sec_Bsec_B_aq141_2=="No one else is using them" | ffc$sec_Bsec_B_aq141_3=="No one else is using them")
ffc$traps_chemical_runs_out<-as.numeric(ffc$sec_Bsec_B_aq141_1=="The chemical in the trap runs out too soon /expires" | ffc$sec_Bsec_B_aq141_2=="The chemical in the trap runs out too soon /expires" | ffc$sec_Bsec_B_aq141_3=="The chemical in the trap runs out too soon /expires")

ffc$biopesticides_comfortable<-as.numeric(ffc$sec_Bsect_B_bq159_1=="I am comfortable using my current method of pest control" | ffc$sec_Bsect_B_bq159_2=="I am comfortable using my current method of pest control")
ffc$biopesticides_bad_experience<-as.numeric(ffc$sec_Bsect_B_bq159_1=="I had a previous bad experience" | ffc$sec_Bsect_B_bq159_2=="I had a previous bad experience")
ffc$biopesticides_noone_uses_them<-as.numeric(ffc$sec_Bsect_B_bq159_1=="No one else is using them" | ffc$sec_Bsect_B_bq159_2=="No one else is using them")
 
ffc$baits_dont_know_use<-as.numeric(ffc$sec_Cq176_1=="I don<cd>t know how to use them" | ffc$sec_Cq176_2=="I don<cd>t know how to use them")
ffc$baits_noone_else<-as.numeric(ffc$sec_Cq176_1=="No one else is using them")
ffc$baits_expensive<-as.numeric(ffc$sec_Cq176_1=="They are expensive/high cost of the biopesticides")
ffc$baits_dont_work<-as.numeric(ffc$sec_Cq176_1=="They do not work/ineffective") 

#GGH) Creating dummy variables for stages where farmers use FFT
ffc$traps_planting<-as.numeric(ffc$sec_Bsec_B_aq140_1=="Planting the seeds")
ffc$traps_sprouting<-as.numeric(ffc$sec_Bsec_B_aq140_1=="When the mango sprouts" | ffc$sec_Bsec_B_aq140_2=="When the mango sprouts")
ffc$traps_maturing<-as.numeric(ffc$sec_Bsec_B_aq140_1=="When the mango starts maturing" | ffc$sec_Bsec_B_aq140_2=="When the mango starts maturing" | ffc$sec_Bsec_B_aq140_3=="When the mango starts maturing")
ffc$traps_weeding_pruning<-as.numeric(ffc$sec_Bsec_B_aq140_1=="During weeding and pruning" | ffc$sec_Bsec_B_aq140_2=="During weeding and pruning" | ffc$sec_Bsec_B_aq140_3=="During weeding and pruning" | ffc$sec_Bsec_B_aq140_4=="During weeding and pruning")
ffc$traps_all_stages<-as.numeric(ffc$sec_Bsec_B_aq140_1=="At all of these stages" | ffc$sec_Bsec_B_aq140_2=="At all of these stages" | ffc$sec_Bsec_B_aq140_3=="At all of these stages" | ffc$sec_Bsec_B_aq140_4=="At all of these stages")
ffc$traps_some_stages<-as.numeric(ffc$sec_Bsec_B_aq140_1=="At some of the stages" | ffc$sec_Bsec_B_aq140_2=="At some of the stages" | ffc$sec_Bsec_B_aq140_3=="At some of the stages" | ffc$sec_Bsec_B_aq140_4=="At some of the stages")

ffc$pesticides_planting<-as.numeric(ffc$sec_Bsect_B_bq158_1=="Planting the seeds")
ffc$pesticides_sprouting<-as.numeric(ffc$sec_Bsect_B_bq158_1=="When the mango sprouts" | ffc$sec_Bsect_B_bq158_2=="When the mango sprouts")
ffc$pesticides_maturing<-as.numeric(ffc$sec_Bsect_B_bq158_1=="When the mango starts maturing" | ffc$sec_Bsect_B_bq158_2=="When the mango starts maturing" | ffc$sec_Bsect_B_bq158_3=="When the mango starts maturing")
ffc$pesticides_weeding_pruning<-as.numeric(ffc$sec_Bsect_B_bq158_1=="During weeding and pruning" | ffc$sec_Bsect_B_bq158_2=="During weeding and pruning" | ffc$sec_Bsect_B_bq158_3=="During weeding and pruning" | ffc$sec_Bsect_B_bq158_4=="During weeding and pruning")
ffc$pesticides_all_stages<-as.numeric(ffc$sec_Bsect_B_bq158_1=="At all of these stages" | ffc$sec_Bsect_B_bq158_2=="At all of these stages" | ffc$sec_Bsect_B_bq158_3=="At all of these stages" | ffc$sec_Bsect_B_bq158_4=="At all of these stages" | ffc$sec_Bsect_B_bq158_5=="At all of these stages")
ffc$pesticides_some_stages<-as.numeric(ffc$sec_Bsect_B_bq158_1=="At some of the stages" | ffc$sec_Bsect_B_bq158_2=="At some of the stages" | ffc$sec_Bsect_B_bq158_3=="At some of the stages" | ffc$sec_Bsect_B_bq158_4=="At some of the stages")

ffc$baits_all_stages<-as.numeric(ffc$sec_CUse_q175_1=="At all of these stages")
ffc$baits_some_stages<-as.numeric(ffc$sec_CUse_q175_1=="At some of the stages")
ffc$baits_maturing<-as.numeric(ffc$sec_CUse_q175_1=="When the mango starts maturing")
ffc$baits_planting<-as.numeric(ffc$sec_CUse_q175_1=="Planting the seeds")

#GGGH) Creating dummy variables for sources of information
ffc$agrovet<-as.numeric(ffc$sec_Cq186_1=="Agrovet/Agro dealer")
ffc$spray_service_provider<-as.numeric(ffc$sec_Cq186_1=="Spray service provider" | ffc$sec_Cq186_2=="Spray service provider")
ffc$fellow_farmers<-as.numeric(ffc$sec_Cq186_1=="Fellow farmers" | ffc$sec_Cq186_2=="Fellow farmers" | ffc$sec_Cq186_3=="Fellow farmers")
ffc$cooperatives<-as.numeric(ffc$sec_Cq186_1=="Cooperatives" | ffc$sec_Cq186_2=="Cooperatives" | ffc$sec_Cq186_3=="Cooperatives" | ffc$sec_Cq186_4=="Cooperatives")
ffc$brokers_middlemen<-as.numeric(ffc$sec_Cq186_1=="Brokers/ Middlemen" | ffc$sec_Cq186_2=="Brokers/ Middlemen" | ffc$sec_Cq186_3=="Brokers/ Middlemen" | ffc$sec_Cq186_4=="Brokers/ Middlemen" | ffc$sec_Cq186_5=="Brokers/ Middlemen")
ffc$exporters<-as.numeric(ffc$sec_Cq186_1=="Exporters (contractual agreement)" | ffc$sec_Cq186_2=="Exporters (contractual agreement)" | ffc$sec_Cq186_3=="Exporters (contractual agreement)" | ffc$sec_Cq186_4=="Exporters (contractual agreement)" | ffc$sec_Cq186_5=="Exporters (contractual agreement)")
ffc$extension_officers<-as.numeric(ffc$sec_Cq186_1=="Agricultural extension officers" | ffc$sec_Cq186_2=="Agricultural extension officers" | ffc$sec_Cq186_3=="Agricultural extension officers" | ffc$sec_Cq186_4=="Agricultural extension officers" | ffc$sec_Cq186_5=="Agricultural extension officers")
ffc$ward_administrators<-as.numeric(ffc$sec_Cq186_1=="Ward administrators" | ffc$sec_Cq186_2=="Ward administrators" | ffc$sec_Cq186_3=="Ward administrators" | ffc$sec_Cq186_4=="Ward administrators" | ffc$sec_Cq186_5=="Ward administrators"| ffc$sec_Cq186_6=="Ward administrators")

#H) Creating variables for the total increase in yields after using PCT
ffc$increased_yields_trap<-ffc$sec_Bsec_B_aq135 - ffc$sec_Bsec_B_aq134
ffc$increased_yields_pesticides<-(ffc$sec_Bsect_B_bq153 - ffc$sec_Bsect_B_bq152)
ffc$increased_yields_baits<-(ffc$sec_CUse_q170 - ffc$sec_CUse_q169)

#I) Creating variables for the percentage increase in yields after using PCT
ffc$increased_yields_trap_perc<-ffc$increased_yields_trap/ffc$sec_Bsec_B_aq134
ffc$increased_yields_pesticides_perc<-(ffc$sec_Bsect_B_bq153 - ffc$sec_Bsect_B_bq152)/ffc$sec_Bsect_B_bq152
ffc$increased_yields_baits_perc<-(ffc$sec_CUse_q170 - ffc$sec_CUse_q169)/ffc$sec_CUse_q169

#Run t-tests for difference in yields for three PCT (I don't know why they're not significant and what happens with NA values)
ffc_trap_yields<-subset(ffc, ffc$sec_Bsec_B_aq134>0)
t.test(ffc$sec_Bsec_B_aq135, ffc$sec_Bsec_B_aq134)
ffc_pesticides_yields<-subset(ffc, ffc$sec_Bsect_B_bq153>0)
t.test(ffc_pesticides_yields$sec_Bsect_B_bq153, ffc_pesticides_yields$sec_Bsect_B_bq152)
ffc_baits_yields<-subset(ffc, ffc$sec_CUse_q170>0)
t.test(ffc_baits_yields$sec_CUse_q170, ffc_baits_yields$sec_CUse_q169)

#J) Group by self-perceived risk profile
ffc<-unite(ffc, "self_risk", c("Sec_Dq51", "Sec_Dq52"))

#K) Standardize risk-index variables and create standardized risk-profiles
ffc$lottery<-(ffc$Sec_Dq53-mean(ffc$Sec_Dq53, na.rm=TRUE))/sd(ffc$Sec_Dq53, na.rm=TRUE)
ffc$trees1<-(ffc$Sec_Dstate1_q54_pio - mean(ffc$Sec_Dstate1_q54_pio, na.rm=TRUE))/sd(ffc$Sec_Dstate1_q54_pio, na.rm=TRUE)
ffc$trees2<-(ffc$Sec_Dstate2_q55_pio - mean(ffc$Sec_Dstate2_q55_pio, na.rm=TRUE))/sd(ffc$Sec_Dstate2_q55_pio, na.rm=TRUE)
ffc$trees3<-(ffc$Sec_Dstate4_q57_pio - mean(ffc$Sec_Dstate4_q57_pio, na.rm=TRUE))/sd(ffc$Sec_Dstate4_q57_pio, na.rm=TRUE)
ffc$trees4<-(ffc$Sec_Dstate5_q58_pio - mean(ffc$Sec_Dstate5_q58_pio, na.rm=TRUE))/sd(ffc$Sec_Dstate5_q58_pio, na.rm=TRUE)
ffc$risk_profile<-(ffc$lottery+ffc$trees1+ffc$trees2+ffc$trees3+ffc$trees4)/sd(ffc$lottery+ffc$trees1+ffc$trees2+ffc$trees3+ffc$trees4)

#KL) Creating present bias discount factors
#Transforming Present Bias data from labels into numbers
ffc <- ffc %>% mutate(sec_D2sec_D2aq59=ifelse(sec_D2sec_D2aq59=="KSh. 200 today",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2aq60=ifelse(sec_D2sec_D2aq60=="KSh. 200 today",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2aq61=ifelse(sec_D2sec_D2aq61=="KSh. 200 today",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2aq62=ifelse(sec_D2sec_D2aq62=="KSh. 200 today",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2aq63=ifelse(sec_D2sec_D2aq63=="KSh. 200 today",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2aq64=ifelse(sec_D2sec_D2aq64=="KSh. 200 today",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2aq65=ifelse(sec_D2sec_D2aq65=="KSh. 200 today",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2bq66=ifelse(sec_D2sec_D2bq66=="KSh. 200 in 2 weeks",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2bq67=ifelse(sec_D2sec_D2bq67=="KSh. 200 in 2 weeks",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2bq68=ifelse(sec_D2sec_D2bq68=="KSh. 200 in 2 weeks",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2bq69=ifelse(sec_D2sec_D2bq69=="KSh. 200 in 2 weeks",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2bq70=ifelse(sec_D2sec_D2bq70=="KSh. 200 in 2 weeks",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2bq71=ifelse(sec_D2sec_D2bq71=="KSh. 200 in 2 weeks",0,1))
ffc <- ffc %>% mutate(sec_D2sec_D2bq72=ifelse(sec_D2sec_D2bq72=="KSh. 200 in 2 weeks",0,1))

#Creating dummies for people who switch back from X>200 Ksh in 2 weeks to 200 Ksh now (set a) or from X>200 Ksh in 4 weeks to 200 Ksh now (set b)
ffc$pb_set_a_dummy<-as.numeric(ffc$sec_D2sec_D2aq59-ffc$sec_D2sec_D2aq60==1 | ffc$sec_D2sec_D2aq60-ffc$sec_D2sec_D2aq61==1 | ffc$sec_D2sec_D2aq61-ffc$sec_D2sec_D2aq62==1 | ffc$sec_D2sec_D2aq62-ffc$sec_D2sec_D2aq63==1 | ffc$sec_D2sec_D2aq63-ffc$sec_D2sec_D2aq64==1 | ffc$sec_D2sec_D2aq64-ffc$sec_D2sec_D2aq65==1)
ffc$pb_set_b_dummy<-as.numeric(ffc$sec_D2sec_D2bq66-ffc$sec_D2sec_D2bq67==1 | ffc$sec_D2sec_D2bq67-ffc$sec_D2sec_D2bq68==1 | ffc$sec_D2sec_D2bq68-ffc$sec_D2sec_D2bq69==1 | ffc$sec_D2sec_D2bq69-ffc$sec_D2sec_D2bq70==1 | ffc$sec_D2sec_D2bq70-ffc$sec_D2sec_D2bq71==1 | ffc$sec_D2sec_D2bq71-ffc$sec_D2sec_D2bq72==1)

#Creating each person's two-week discount factor (including the people who switched back), using 200/700 for people who took 200 all the way
ffc$a220_over_200<-(200/210)*ffc$sec_D2sec_D2aq59
ffc$a240_over_200<-(200/230)*as.numeric(ffc$sec_D2sec_D2aq60-ffc$sec_D2sec_D2aq59==1)
ffc$a260_over_200<-(200/250)*as.numeric(ffc$sec_D2sec_D2aq61-ffc$sec_D2sec_D2aq60==1)
ffc$a300_over_200<-(200/280)*as.numeric(ffc$sec_D2sec_D2aq62-ffc$sec_D2sec_D2aq61==1)
ffc$a350_over_200<-(200/325)*as.numeric(ffc$sec_D2sec_D2aq63-ffc$sec_D2sec_D2aq62==1)
ffc$a400_over_200<-(200/375)*as.numeric(ffc$sec_D2sec_D2aq64-ffc$sec_D2sec_D2aq63==1)
ffc$a600_over_200<-(200/500)*as.numeric(ffc$sec_D2sec_D2aq65-ffc$sec_D2sec_D2aq64==1)
ffc$a200_all_the_way<-(200/700)*as.numeric(ffc$sec_D2sec_D2aq65==0)

ffc$b220_over_200<-(200/210)*ffc$sec_D2sec_D2bq66
ffc$b240_over_200<-(200/230)*as.numeric(ffc$sec_D2sec_D2bq67-ffc$sec_D2sec_D2bq66==1)
ffc$b260_over_200<-(200/250)*as.numeric(ffc$sec_D2sec_D2bq68-ffc$sec_D2sec_D2bq67==1)
ffc$b300_over_200<-(200/275)*as.numeric(ffc$sec_D2sec_D2bq69-ffc$sec_D2sec_D2bq68==1)
ffc$b350_over_200<-(200/325)*as.numeric(ffc$sec_D2sec_D2bq70-ffc$sec_D2sec_D2bq69==1)
ffc$b400_over_200<-(200/375)*as.numeric(ffc$sec_D2sec_D2bq71-ffc$sec_D2sec_D2bq70==1)
ffc$b600_over_200<-(200/500)*as.numeric(ffc$sec_D2sec_D2bq72-ffc$sec_D2sec_D2bq71==1)
ffc$b200_all_the_way<-(200/700)*as.numeric(ffc$sec_D2sec_D2bq72==0)

#Creating variable for present bias by subtracting 2 to 4 week bias from 0 to 2 week bias (people with multiple switching points are included, but their data should be removed before using this variable for further analysis)
ffc$present_bias<-(ffc$a220_over_200+ffc$a240_over_200+ffc$a260_over_200+ffc$a300_over_200+ffc$a350_over_200+ffc$a400_over_200+ffc$a600_over_200+ffc$a200_all_the_way)-(ffc$b220_over_200+ffc$b240_over_200+ffc$b260_over_200+ffc$b300_over_200+ffc$b350_over_200+ffc$b400_over_200+ffc$b600_over_200+ffc$b200_all_the_way)

#Subsetting data to people who didn't switch back in present bias questions
ffc_pb<-subset(ffc, ffc$pb_set_a_dummy==0 & ffc$pb_set_b_dummy==0)

#L) locus of control: transforming answers into 0 and 1 (for the self-reported questions, "False" and "It's in God's will" are coded as "0" and all other answers as "1")
ffc$Sec_Esec_E1q73<-mapvalues(ffc$Sec_Esec_E1q73,from = c("It is in God<cd>s will / God will help","I will use new methods I have never tried before", "I will use the trusted methods I already know of", "I will look at how fellow farmers manage this"), c(0, 1, 1, 1))
ffc$Sec_Esec_E1q74<-mapvalues(ffc$Sec_Esec_E1q74,from = c("TRUE","FALSE"), c(1, 0))
ffc$Sec_Esec_E1q75<-mapvalues(ffc$Sec_Esec_E1q75,from = c("People's misfortunes result from the mistakes they make","Many of the unhappy things in people's lives are partly due to bad luck"), c(1, 0))
ffc$Sec_Esec_E1q76<-mapvalues(ffc$Sec_Esec_E1q76,from = c("One of the major reasons why we have wars is because people don't take enough interest in politics.","There will always be wars, no matter how hard people try to prevent them."), c(1, 0))
ffc$Sec_Esec_E1q77<-mapvalues(ffc$Sec_Esec_E1q77,from = c("In the long run, people get the respect they deserve in this world.","Unfortunately, an individual's worth often passes unrecognized no matter how hard he tries."), c(1, 0))
ffc$Sec_Esec_E1q78<-mapvalues(ffc$Sec_Esec_E1q78,from = c("The idea that teachers are unfair to students is nonsense.","Most students don't realize the extent to which their grades are influenced by accidental happenings"), c(1, 0))
ffc$Sec_Esec_E1q79<-mapvalues(ffc$Sec_Esec_E1q79,from = c("Capable people who fail to become leaders have not taken advantage of their opportunities","Without the right breaks, one cannot be an effective leader."), c(1, 0))
ffc$Sec_Esec_E1q80<-mapvalues(ffc$Sec_Esec_E1q80,from = c("People who can't get others to like them don't understand how to get along with others.","No matter how hard you try, some people just don't like you."), c(1, 0))
ffc$Sec_Esec_E1q81<-mapvalues(ffc$Sec_Esec_E1q81,from = c("Trusting to fate has never turned out as well for me as making a decision to take a definite course of action.","I have often found that what is going to happen will happen."), c(1, 0))
ffc$Sec_Esec_E1q82<-mapvalues(ffc$Sec_Esec_E1q82,from = c("In the case of the well prepared student, there is rarely, if ever, such a thing as an unfair test.","Many times exam questions tend to be so unrelated to course work that studying is really useless."), c(1, 0))
ffc$Sec_Esec_E1q83<-mapvalues(ffc$Sec_Esec_E1q83,from = c("Becoming a success is a matter of hard work; luck has little or nothing to do with it.","Getting a good job depends mainly on being in the right place at the right time."), c(1, 0))
ffc$Sec_Esec_E1q84<-mapvalues(ffc$Sec_Esec_E1q84,from = c("The average citizen can have an influence in government decisions.","This world is run by the few people in power, and there is not much the little guy can do about it."), c(1, 0))
ffc$Sec_Esec_E1q85<-mapvalues(ffc$Sec_Esec_E1q85,from = c("When I make plans, I am almost certain that I can make them work.","It is not always wise to plan too far ahead because many things turn out to be a matter of luck anyway."), c(1, 0))
ffc$Sec_Esec_E1q86<-mapvalues(ffc$Sec_Esec_E1q86,from = c("In my case, getting what I want has little or nothing to do with luck.","Many times we might just as well decide what to do by flipping a coin."), c(1, 0))
ffc$Sec_Esec_E1q87<-mapvalues(ffc$Sec_Esec_E1q87,from = c("What happens to me is my own doing.","Sometimes I feel that I don't have enough control over the direction my life is taking."), c(1, 0))
ffc$Sec_Esec_E1q73<-as.numeric(ffc$Sec_Esec_E1q73)
ffc$Sec_Esec_E1q74<-as.numeric(ffc$Sec_Esec_E1q74)
ffc$Sec_Esec_E1q75<-as.numeric(ffc$Sec_Esec_E1q75)
ffc$Sec_Esec_E1q76<-as.numeric(ffc$Sec_Esec_E1q76)
ffc$Sec_Esec_E1q77<-as.numeric(ffc$Sec_Esec_E1q77)
ffc$Sec_Esec_E1q78<-as.numeric(ffc$Sec_Esec_E1q78)
ffc$Sec_Esec_E1q79<-as.numeric(ffc$Sec_Esec_E1q79)
ffc$Sec_Esec_E1q80<-as.numeric(ffc$Sec_Esec_E1q80)
ffc$Sec_Esec_E1q81<-as.numeric(ffc$Sec_Esec_E1q81)
ffc$Sec_Esec_E1q82<-as.numeric(ffc$Sec_Esec_E1q82)
ffc$Sec_Esec_E1q83<-as.numeric(ffc$Sec_Esec_E1q83)
ffc$Sec_Esec_E1q84<-as.numeric(ffc$Sec_Esec_E1q84)
ffc$Sec_Esec_E1q85<-as.numeric(ffc$Sec_Esec_E1q85)
ffc$Sec_Esec_E1q86<-as.numeric(ffc$Sec_Esec_E1q86)
ffc$Sec_Esec_E1q87<-as.numeric(ffc$Sec_Esec_E1q87)

#M) Creating locus of control index including the two self-reported questions
ffc$locus_control_index1<-ffc$Sec_Esec_E1q73+ffc$Sec_Esec_E1q74+ffc$Sec_Esec_E1q75+ffc$Sec_Esec_E1q76+ffc$Sec_Esec_E1q77+ffc$Sec_Esec_E1q78+ffc$Sec_Esec_E1q79+ffc$Sec_Esec_E1q80+ffc$Sec_Esec_E1q81+ffc$Sec_Esec_E1q82+ffc$Sec_Esec_E1q83+ffc$Sec_Esec_E1q84+ffc$Sec_Esec_E1q85+ffc$Sec_Esec_E1q86+ffc$Sec_Esec_E1q87
#Standardized index
ffc$locus_control_index1_std<-(ffc$locus_control_index1-mean(ffc$locus_control_index1))/sd(ffc$locus_control_index1)

#N) Excluding the two self-reported questions
ffc$locus_control_index2<-ffc$Sec_Esec_E1q75+ffc$Sec_Esec_E1q76+ffc$Sec_Esec_E1q77+ffc$Sec_Esec_E1q78+ffc$Sec_Esec_E1q79+ffc$Sec_Esec_E1q80+ffc$Sec_Esec_E1q81+ffc$Sec_Esec_E1q82+ffc$Sec_Esec_E1q83+ffc$Sec_Esec_E1q84+ffc$Sec_Esec_E1q85+ffc$Sec_Esec_E1q86+ffc$Sec_Esec_E1q87
#Standardized index
ffc$locus_control_index2_std<-(ffc$locus_control_index2-mean(ffc$locus_control_index2))/sd(ffc$locus_control_index2)

#O) Create conscientiousness and influence of peers indexes
#Transform Likert labels into 1 - 5 for q113, 115, 116, 117, 118, 119 and 120
f_1_5 <- function(x) {as.numeric(as.character(plyr::mapvalues(x, from = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),to = c(1,2,3,4,5))))}
vars_1_5<-c("Sec_Esec_E2set1q113_1", "Sec_Esec_E2set2q113_2", "Sec_Esec_E2set3q113_3", "Sec_Esec_E2set4q113_4", "Sec_Esec_E2set5q113_5", "Sec_Esec_E2set1q115_1", "Sec_Esec_E2set2q115_2", "Sec_Esec_E2set3q115_3", "Sec_Esec_E2set4q115_4", "Sec_Esec_E2set5q115_5", "Sec_Esec_E2set1q116_1", "Sec_Esec_E2set2q116_2", "Sec_Esec_E2set3q116_3", "Sec_Esec_E2set4q116_4", "Sec_Esec_E2set5q116_5", "Sec_Esec_E3set1bq117_1", "Sec_Esec_E3set2bq117_2", "Sec_Esec_E3set3bq117_3","Sec_Esec_E3set4bq117_4", "Sec_Esec_E3set1bq118_1", "Sec_Esec_E3set2bq118_2", "Sec_Esec_E3set3bq118_3","Sec_Esec_E3set4bq118_4", "Sec_Esec_E3set1bq119_1", "Sec_Esec_E3set2bq119_2", "Sec_Esec_E3set3bq119_3","Sec_Esec_E3set4bq119_4", "Sec_Esec_E3set1bq120_1", "Sec_Esec_E3set2bq120_2", "Sec_Esec_E3set3bq120_3","Sec_Esec_E3set4bq120_4")
ffc <- ffc %>% mutate_at(vars_1_5, f_1_5)

#Transform Likert labels into 5 - 1 for q112 and 114
f_5_1 <- function(x) {as.numeric(as.character(plyr::mapvalues(x, from = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"),to = c(5,4,3,2,1))))}
vars_5_1<-c("Sec_Esec_E2set1q112_1", "Sec_Esec_E2set2q112_2", "Sec_Esec_E2set3q112_3", "Sec_Esec_E2set4q112_4", "Sec_Esec_E2set5q112_5", "Sec_Esec_E2set1q114_1", "Sec_Esec_E2set2q114_2", "Sec_Esec_E2set3q114_3", "Sec_Esec_E2set4q114_4", "Sec_Esec_E2set5q114_5")
ffc <- ffc %>% mutate_at(vars_5_1, f_5_1)

#Create conscientiousness index
ffc$conscientiousness_index1<-rowSums(ffc[,c("Sec_Esec_E2set1q113_1", "Sec_Esec_E2set2q113_2", "Sec_Esec_E2set3q113_3", "Sec_Esec_E2set4q113_4", "Sec_Esec_E2set5q113_5", "Sec_Esec_E2set1q115_1", "Sec_Esec_E2set2q115_2", "Sec_Esec_E2set3q115_3", "Sec_Esec_E2set4q115_4", "Sec_Esec_E2set5q115_5", "Sec_Esec_E2set1q116_1", "Sec_Esec_E2set2q116_2", "Sec_Esec_E2set3q116_3", "Sec_Esec_E2set4q116_4", "Sec_Esec_E2set5q116_5", "Sec_Esec_E2set1q112_1", "Sec_Esec_E2set2q112_2", "Sec_Esec_E2set3q112_3", "Sec_Esec_E2set4q112_4", "Sec_Esec_E2set5q112_5", "Sec_Esec_E2set1q114_1", "Sec_Esec_E2set2q114_2", "Sec_Esec_E2set3q114_3", "Sec_Esec_E2set4q114_4", "Sec_Esec_E2set5q114_5")], na.rm=TRUE)
#Create standardized conscientiousness index
ffc$conscientiousness_index_std<-(ffc$conscientiousness_index1-mean(ffc$conscientiousness_index1))/sd(ffc$conscientiousness_index1)

#Peer index can't be computed as values for q117_3 to q120_3 are missing

#Starting with Data Analysis
#1) Age of respondents
ggplot(ffc, mapping = aes(x = sec_Aq3), fill="#0033a1") +
  geom_histogram(binwidth = 8, fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "Age", title = "How old are the farmers?")+
  theme(plot.title = element_text(hjust = 0.4))

#2) Education of respondents
education <- c("None", "Some primary level education", "Completed primary", "Some secondary level education", "Completed secondary", "Vocational training", "Tertiary level education/University/College")
ggplot(ffc) +
  geom_bar(mapping = aes(x = sec_Aq7), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Education of farmers")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + scale_x_discrete(limits = education)

#2.1) Boxplot/violins for income
ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }
ffc$log_income<-log(ffc$sec_Aq14)
ffc$log_mango_income<-log(ffc$sec_Aq140)

ggplot(ffc) +
  geom_boxplot(mapping = aes(y = sec_Aq14))+
  theme(panel.background=element_blank())+
  labs(y = "", x = "", title = "Income of respondents (in KSH)")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))+coord_flip()+scale_y_continuous(labels = ks)

ggplot(ffc) +
  geom_boxplot(mapping = aes(y = sec_Aq140))+
  theme(panel.background=element_blank())+
  labs(y = "", x = "", title = "Income of respondents from mango farming (in KSH)")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))+coord_flip()+scale_y_continuous(labels = ks)

ggplot(ffc) +
  geom_bar(mapping = aes(x=sec_Aq6, y = sec_Aq14), fill="#0033a1", stat="summary", fun.y=mean)+
  theme(panel.background=element_blank())+
  labs(y = "Income (KSH)", x = "", title = "Income of farmers by county")+
  theme(plot.title = element_text(hjust = 0.5))+scale_y_continuous(labels = ks)

ggplot(ffc) +
  geom_violin(mapping = aes(y = sec_Aq14, x=1), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Income in KSH", x = "Density", title = "Yearly income of farmers")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))+scale_y_continuous(labels = ks)+coord_flip()

ggplot(ffc) +
  geom_violin(mapping = aes(y = sec_Aq140, x=1), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Income in KSH", x = "Density", title = "Yearly income of farmers from mango farming")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))+coord_flip()+scale_y_continuous(labels = ks)


#3) Varieties of trees older and younger than 4 years
ngowe<-subset(ffc, q144d1=="Ngowe")
mean(ngowe$q144f1)
mean(ngowe$q144g1)
apple<-subset(ffc, q144d1=="Apple")
apple2<-subset(ffc, q144d2=="Apple")
(291*mean(apple$q144f1)+59*mean(apple2$q144f1))/350
(291*mean(apple$q144g1)+59*mean(apple2$q144g1))/350
kent<-subset(ffc, q144d1=="Kent")
kent2<-subset(ffc, q144d2=="Kent")
kent3<-subset(ffc, q144d3=="Kent")
(2*mean(kent$q144f1)+36*mean(kent2$q144f2)+20*mean(kent3$q144f3))/58
(2*mean(kent$q144g1)+36*mean(kent2$q144g2)+20*mean(kent3$q144g3))/58

#4) Create graph for ownership
ffc_ownership<-select(ffc, sec_Aass_q1411:sec_Aass_q1428)
vars_ownership<-c("sec_Aass_q1411", "sec_Aass_q1412", "sec_Aass_q1413", "sec_Aass_q1414", "sec_Aass_q1415", "sec_Aass_q1416", "sec_Aass_q1417", "sec_Aass_q1418", "sec_Aass_q1419", "sec_Aass_q1420", "sec_Aass_q1421", "sec_Aass_q1422", "sec_Aass_q1423", "sec_Aass_q1424", "sec_Aass_q1425", "sec_Aass_q1426", "sec_Aass_q1427", "sec_Aass_q1428")
ffc_ownership<-gather(ffc_ownership, "variables", "yes", vars_ownership)
labels_ownership<-c("Smart phone", "Feature phone", "Television set", "Radio", "Video/DVD player", "Furniture", "Satellite dish", "Computer", "Stove", "Gas cooker", "Refrigerator", "Bicycle", "Motorbike", "Car", "Jewelry", "Livestock", "House", "Farmland")
ggplot(ffc_ownership) +
  geom_bar(mapping = aes(x = variables, y=yes), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Percent", x = "", title = "What do the farmers own?")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels= labels_ownership)

#5) Creating graphs of challenges
ffc_challenges2<-gather(ffc_challenges, "vars", "yes", vars_challenges) 
labels_challenges<-c("Exploitation from middlemen", "High cost of fly traps", "Inadequate labour", "Inadequate farm land", "Inadequate ready market", "Low mango prices", "Pests and diseases", "Poor road infrastructure", "Weather and climate")
ggplot(ffc_challenges2) +
  geom_bar(mapping = aes(x = vars, y=yes), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Percent", x = "", title = "Challenges for farmers")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_challenges)+
  scale_y_continuous(labels = scales::percent)
  
labels_mainchallenge<-c("Exploitation from middlemen", "High cost of fly traps", "Inadequate labour", "Inadequate ready market", "Low mango prices", "Other", "Pests and diseases", "Weather and climate")
ggplot(ffc) +
  geom_bar(mapping = aes(x = sec_Bq23b), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Main challenge for farmers")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_mainchallenge)

#6) Creating graphs with challenges per sub county 
ffc_kibwezi<-subset(ffc, sec_Aq6=="Kibwezi West")
ffc_matiliku<-subset(ffc, sec_Aq6=="Matiliku")
ffc_mbooni<-subset(ffc, sec_Aq6=="Mbooni East")
ffc_challenges_kibwezi<-gather(ffc_kibwezi, "vars", "yes", vars_challenges) 
ffc_challenges_matiliku<-gather(ffc_matiliku, "vars", "yes", vars_challenges)
ffc_challenges_mbooni<-gather(ffc_matiliku, "vars", "yes", vars_challenges) 

ggplot(ffc_challenges_kibwezi) +
  geom_bar(mapping = aes(x = vars, y=yes), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Percent", x = "", title = "Challenges for farmers from Kibwezi West")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_challenges)+
  scale_y_continuous(labels = scales::percent)

ggplot(ffc_challenges_matiliku) +
  geom_bar(mapping = aes(x = vars, y=yes), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Percent", x = "", title = "Challenges for farmers from Matiliku")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_challenges)+
  scale_y_continuous(labels = scales::percent)

ggplot(ffc_challenges_mbooni) +
  geom_bar(mapping = aes(x = vars, y=yes), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Percent", x = "", title = "Challenges for farmers from Mbooni East")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_challenges)+
  scale_y_continuous(labels = scales::percent)

#7) Creating graph with main challenge per subcounty
ffc_mainchallenge<-select(ffc, sec_Aq6, sec_Avillage, sec_Bq23b)
ffc_mainchallenge<-gather(ffc_mainchallenge, "vars", "yes", sec_Aq6) 
ffc_mainchallenge$sec_Bq23b<-mapvalues(ffc_mainchallenge$sec_Bq23b,from = c("Exploitation from middlemen/brokers", "High cost of fly traps", "Inadequate labour - for spraying, weeding, harvesting, transportation", "Inadequate ready market", "Low selling prices for the mangoes", "Other (specify)", "Pests and diseases", "Unfavourable weather/climatic conditions"), to= c("Exploitation", "High cost of fly traps", "Inadequate labour", "Inadequate ready market", "Low mango prices", "Other", "Pests and diseases", "Weather and climate"))
ffc_mainchallenge$MainChallenge<-ffc_mainchallenge$sec_Bq23b
ggplot(ffc_mainchallenge)+
  geom_bar(aes(yes, fill=MainChallenge), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "", x = "", title = "Main challenges for farmers by sub-county")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="YlGnBu")

#8) Checking if people older than 50 report different challenges
ffc$older_than_50 <- with(ffc, sec_Aq3>50)
ffc$older_than_50<-mapvalues(ffc$older_than_50, from=c("TRUE", "FALSE"), to=c("Older than 50", "50 and younger"))
ffc_50<-select(ffc, c(older_than_50, sec_Bq23b))
ffc_50$sec_Bq23b<-mapvalues(ffc_50$sec_Bq23b,from = c("Exploitation from middlemen/brokers", "High cost of fly traps", "Inadequate labour - for spraying, weeding, harvesting, transportation", "Inadequate ready market", "Low selling prices for the mangoes", "Other (specify)", "Pests and diseases", "Unfavourable weather/climatic conditions"), to= c("Exploitation", "High cost of fly traps", "Inadequate labour", "Inadequate ready market", "Low mango prices", "Other", "Pests and diseases", "Weather and climate"))
ffc_50<-gather(ffc_50, "vars", "yes", older_than_50) 
ffc_50$MainChallenge<-ffc_50$sec_Bq23b

ggplot(ffc_50)+
  geom_bar(aes(yes, fill=MainChallenge), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "", x = "", title = "Main challenges by age of farmers")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="YlGnBu")

#9) Creating graph for knowledge of pests
ffc_pest_knowledge<-select(ffc, fruit_fly:aphids)
replace_na<- function(x) { replace(x, is.na(x), 0) }
ffc_pest_knowledge<- mutate_all(ffc_pest_knowledge, replace_na)
ffc_pest_knowledge<-melt(ffc_pest_knowledge)
labels_pest<-c("Fruit Fly", "Mango seed wevil", "Thrips", "Aphids")

ggplot(ffc_pest_knowledge)+
  geom_bar(aes(variable, value), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Percent", x = "", title = "Farmers awareness of mango pests")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_pest)+
  scale_y_continuous(labels = scales::percent)

#10) Correlating average knowledge with main challenge
ffc_challenge_knowledge<-select(ffc, sec_Bq23b, knowledge_groups)
ffc_challenge_knowledge<-gather(ffc_challenge_knowledge, "vars", "yes", sec_Bq23b)
ggplot(ffc_challenge_knowledge)+
  geom_bar(aes(yes, knowledge_groups), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Knowledge of pests and PCT", x = "", title = "Average knowledge of farmers with different main challenges")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_challenges)

#10.1) Correlating average knowledge with all challenges
ffc_challenges5<-select(ffc, pests:weather_climate, knowledge_groups) 
ffc_challenges5<-melt(ffc_challenges5, "knowledge_groups")
ffc_challenges5<-subset(ffc_challenges5, value==1)
labels_challenges<-c("Exploitation from middlemen", "High cost of fly traps", "Inadequate labour", "Inadequate farm land", "Inadequate ready market", "Low mango prices", "Pests and diseases", "Poor road infrastructure", "Weather and climate")
ggplot(ffc_challenges5) +
  geom_bar(mapping = aes(x = variable, y=knowledge_groups), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Knowledge of pests and PCTs", x = "", title = "Average knowledge of farmers with different challenges", 
       caption = "Knowledge index: number of 
       mango pests and PCTs that 
       farmers are aware of (out of 8)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_challenges)

ffc_challenges6<-select(ffc, pests:weather_climate, pest_knowledge_groups) 
ffc_challenges6<-melt(ffc_challenges6, "pest_knowledge_groups")
ffc_challenges6<-subset(ffc_challenges6, value==1)
labels_challenges<-c("Exploitation from middlemen", "High cost of fly traps", "Inadequate labour", "Inadequate farm land", "Inadequate ready market", "Low mango prices", "Pests and diseases", "Poor road infrastructure", "Weather and climate")
ggplot(ffc_challenges6) +
  geom_bar(mapping = aes(x = variable, y=pest_knowledge_groups), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Knowledge of pests", x = "", title = "Average pests knowledge of farmers with different challenges", 
       caption = "Number of mango pests that 
       farmers are aware of (out of 5)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_challenges)

ffc_challenges7<-select(ffc, pests:weather_climate, pct_knowledge_groups) 
ffc_challenges7<-melt(ffc_challenges7, "pct_knowledge_groups")
ffc_challenges7<-subset(ffc_challenges7, value==1)
labels_challenges<-c("Exploitation from middlemen", "High cost of fly traps", "Inadequate labour", "Inadequate farm land", "Inadequate ready market", "Low mango prices", "Pests and diseases", "Poor road infrastructure", "Weather and climate")
ggplot(ffc_challenges7) +
  geom_bar(mapping = aes(x = variable, y=pct_knowledge_groups), fill="#0033a1", stat="summary", fun.y="mean")+
  theme(panel.background=element_blank())+
  labs(y = "Knowledge of PCTs", x = "", title = "Average PCTs knowledge of farmers with different challenges", 
       caption = "PCT index: number of PCTs that 
       farmers are aware of (out of 7)")+
  theme(axis.text.x = element_text(angle = 70, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_challenges)

#11) Farmers engagement with different technologies
ffc_engagement<-select(ffc,sec_Bengage_q124:sec_Bengage_q124c)
ffc_engagement$a<-1
ffc_engagement<-melt(ffc_engagement, "a")
labels_technologies<-c("Bio pesticides", "Fruit fly traps", "Baits")
ffc_engagement$value<-mapvalues(ffc_engagement$value, from = c(" have actually used it once", " I sometimes use it"), to=c("I have used it once", "I sometimes use it"))

ggplot(ffc_engagement)+
  geom_bar(aes(variable, fill=value), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Farmers engagement with pest control technologies")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = -0.5))+
  scale_fill_brewer(palette="YlGnBu")+
  scale_x_discrete(labels= labels_technologies)

ffc_engagement1<-select(ffc,sec_Bengage_q124d:sec_Bengage_q124g)
ffc_engagement1$a<-1
ffc_engagement1<-melt(ffc_engagement1, "a")
labels_technologies1<-c("Solar bags/Orchard sanitization", "Natural enemies/parasitoids", "Pathogenic fungi", "Soil treatments")
ffc_engagement1$value<-mapvalues(ffc_engagement1$value, from = c(" have actually used it once", " I sometimes use it"), to=c("I have used it once", "I sometimes use it"))

ggplot(ffc_engagement1)+
  geom_bar(aes(variable, fill=value), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Farmers engagement with pest control technologies")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = -0.5))+
  scale_fill_brewer(palette="YlGnBu")+
  scale_x_discrete(labels= labels_technologies1)

#12) Correlating engagement level with challenges
ffc_challenge_engagement<-select(ffc, pests:weather_climate, engagement_groups)
ffc_challenge_engagement<-melt(ffc_challenge_engagement, "engagement_groups")
ffc_challenge_engagement<-subset(ffc_challenge_engagement, value==1)
labels_challenges2<-c("Pests and diseases", "High cost of fly traps", "Poor road infrastructure", "Exploitation from middlemen", "Low mango prices", "Inadequate farm land", "Inadequate ready market", "Inadequate labour", "Weather and climate")
ggplot(ffc_challenge_engagement)+
  geom_bar(aes(variable, engagement_groups), fill="#0033a1", stat="summary", fun.y=mean)+
  theme(panel.background=element_blank())+
  labs(y = "Engagement with PCT", x = "", title = "Farmers level of engagement with PCT by challenges",
       caption="Engagement index: farmers engagement 
       with fly traps, bio pesticides 
       and baits (from 0 to 6)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_challenges2)

#13) Why don't farmers use traps?
ffc_no_traps<-select(ffc, traps_ineffective:traps_chemical_runs_out)
ffc_no_traps$a<-1
ffc_no_traps<-melt(ffc_no_traps, "a")
ffc_no_traps<-subset(ffc_no_traps, value==1)
labels_traps<-c("Traps are ineffective", "Traps are expensive", "I don't know how to use traps", "I am comfortable with my current methods", "I had a bad experience", "No one uses them", "The chemical runs out too soon")

ggplot(ffc_no_traps)+
  geom_bar(aes(variable), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Uptake barriers to fruit fly traps",
       caption="Based on answers
       from 151 farmers")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_traps)

#14) Why don't farmers use biopesticides?
ffc_no_pesticides<-select(ffc, biopesticides_comfortable:biopesticides_noone_uses_them)
ffc_no_pesticides$a<-1
ffc_no_pesticides<-melt(ffc_no_pesticides, "a")
ffc_no_pesticides<-subset(ffc_no_pesticides, value==1)
labels_pesticides<-c("Comfortable with current methods", "Bad experience", "No one uses them")

ggplot(ffc_no_pesticides)+
  geom_bar(aes(variable), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Uptake barriers to bio pesticides",
       caption="Based on answers
       from 60 farmers")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_pesticides)

#15) Why don't farmers use baits?
ffc_no_baits<-select(ffc, baits_dont_know_use:baits_dont_work)
ffc_no_baits$a<-1
ffc_no_baits<-melt(ffc_no_baits, "a")
ffc_no_baits<-subset(ffc_no_baits, value==1)
labels_baits<-c("I don't know how to use them", "No one else uses them", "They are expensive", "They don't work")

ggplot(ffc_no_baits)+
  geom_bar(aes(variable), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Uptake barriers to baits",
       caption="Based on answers 
       from 16 farmers")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_baits)+
  scale_y_continuous(breaks=c(2,4,6,8,10))

#16) How often and when do farmers use fly traps?
ffc_use_traps<-subset(ffc, sec_Bsec_B_aq139 != "NA")
labels_use_traps<-c("Never", "Daily", "During budding stage only", "During flowering stage only", "Used a few times, then stopped", "Once the fruit emerges till harvest")

ggplot(ffc_use_traps)+
  geom_bar(aes(sec_Bsec_B_aq139), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "How often do farmers use fly traps?",
       caption="Based on answers
       from 149 farmers")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_use_traps)

ffc_when_traps<-select(ffc_use_traps, traps_planting:traps_all_stages)
ffc_when_traps$a<-1
ffc_when_traps<-melt(ffc_when_traps, "a")
ffc_when_traps<-subset(ffc_when_traps, value==1)
labels_when_traps<-c("Planting stage", "Sprouting stage", "Maturing stage", "Weeding and pruning stage", "All stages")

ggplot(ffc_when_traps)+
  geom_bar(aes(variable), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "When do farmers use fly traps?",
       caption="Based on answers
       from 149 farmers")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_when_traps)

#17) How often and when do farmers use biopesticides?
ffc_use_pesticides<-subset(ffc, sec_Bsect_B_bq157 != "NA")
labels_use_pesticides<-c("Bi-weekly", "Daily", "During budding stage only", "During flowering stage only", "Never", "Used a few times, then stopped", "Monthly", "Once the fruit emerges till harvest")

ggplot(ffc_use_pesticides)+
  geom_bar(aes(sec_Bsect_B_bq157), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "How often do farmers use bio pesticides?",
       caption="Based on answers
       from 61 farmers")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_use_pesticides)

ffc_when_pesticides<-select(ffc_use_traps, pesticides_planting:pesticides_all_stages)
ffc_when_pesticides$a<-1
ffc_when_pesticides<-melt(ffc_when_pesticides, "a")
ffc_when_pesticides<-subset(ffc_when_pesticides, value==1)
labels_when_pesticides<-c("Planting stage", "Sprouting stage", "Weeding and pruning stage", "Maturing stage", "All stages")

ggplot(ffc_when_pesticides)+
  geom_bar(aes(variable), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "When do farmers use bio pesticides?",
       caption="Based on answers
       from 61 farmers")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_when_pesticides)+
  scale_y_continuous(breaks=c(2,4,6,8,10))

#18) Where do farmers get their information from?
ffc_information<-select(ffc, agrovet:ward_administrators)
ffc_information$a<-1
ffc_information<-melt(ffc_information, "a")
ffc_information<-subset(ffc_information, value==1)
labels_information<-c("Agrovet", "Spray service provider", "Fellow farmers", "Cooperatives", "Brokers/middlemen", "Exporters", "Extension officers", "Ward administrators")

ggplot(ffc_information)+
  geom_bar(aes(variable), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Where do farmers get their information on PCT?")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_information)

#19) Why do farmers use PCT?
ffc_reasons_traps<-select(ffc, traps_effective:traps_neighbours)
ffc_reasons_traps$a<-1
ffc_reasons_traps<-melt(ffc_reasons_traps, "a")
ffc_reasons_traps<-subset(ffc_reasons_traps, value==1)
labels_reasons<-c("They have been successful", "Cheaper than other PCTs", "Other farmers successfully use them", "All my neighbours use them")

ggplot(ffc_reasons_traps)+
  geom_bar(aes(variable), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Why do farmers use fruit fly traps?")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_reasons)

ffc_reasons_pesticides<-select(ffc, pesticides_effective:pesticides_neighbours)
ffc_reasons_pesticides$a<-1
ffc_reasons_pesticides<-melt(ffc_reasons_pesticides, "a")
ffc_reasons_pesticides<-subset(ffc_reasons_pesticides, value==1)

ggplot(ffc_reasons_pesticides)+
  geom_bar(aes(variable), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Why do farmers use bio pesticides?")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_reasons)

#20) how do you think the use of PCT would affect your farming outputs?
ggplot(ffc)+
  geom_bar(aes(sec_Cq184), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "How would the use of PCT effect your farming outputs?")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

#21) What brings you together with other farmers?
ffc_other_farmers<-subset(ffc, Sec_FSec_F2q196 != "NA")
labels_other_farmers<-c("Barazas", "Exhibitions, trade fairs, field days", "Cooperative meetings", "Market days", "Merry go rounds", "Seminars/trainings")
ggplot(ffc_other_farmers)+
  geom_bar(aes(Sec_FSec_F2q196), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "What brings you together with other farmers?")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= labels_other_farmers)

#22) Most available communication channels (also by sub-county)
ffc_media<-select(ffc, Sec_Gq198_1:Sec_Gq198_3)
ffc_media$a<-1
ffc_media<-melt(ffc_media, "a")
ffc_media$value<-mapvalues(ffc_media$value, from = "Neighbors<e6>", to= "Neighbours")
ffc_media1<-subset(ffc_media, value != "Social Media: Facebook")
ffc_media1<-subset(ffc_media1, value != "Social Media: WhatsApp")
labels_media<-c("#1", "#2", "#3")

ggplot(ffc_media1)+
  geom_bar(aes(variable, fill=value), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "Top 3", title = "Most available communication channels")+
  theme(plot.title = element_text(hjust = -0.5))+
  scale_fill_brewer(palette="YlGnBu")+
  scale_x_discrete(label=labels_media)

ggplot(ffc_media1)+
  geom_bar(aes(value), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Most available communication channels (#1+#2+#3)")+
  theme(axis.text.x = element_text(angle = 55, hjust = 1), plot.title = element_text(hjust = 0.5))

ffc_media2<-select(ffc, Sec_Gq198_1:Sec_Gq198_3, sec_Aq6)
ffc_media2$Channel<-ffc_media2$Sec_Gq198_1
ffc_media2<-subset(ffc_media2, Channel != "Family members")
ffc_media2<-subset(ffc_media2, Channel != "Friends")
ggplot(ffc_media2)+
  geom_bar(aes(sec_Aq6, fill=Channel), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Most available communication channel by county")+
  theme(plot.title = element_text(hjust = 0.0))+
  scale_fill_brewer(palette="YlGnBu")

ffc_media3<-select(ffc, Sec_Gq198_1:Sec_Gq198_3, sec_Aq6)
ffc_media3<-melt(ffc_media3, "sec_Aq6")
ffc_media3$value<-mapvalues(ffc_media3$value, from = "Neighbors<e6>", to= "Neighbours")
ffc_media3<-subset(ffc_media3, value != "Social Media: Facebook")
ffc_media3<-subset(ffc_media3, value != "Social Media: WhatsApp")
ffc_media3<-subset(ffc_media3, value != "Friends")
ffc_media3<-subset(ffc_media3, value != "Media: TV")
ffc_media3<-subset(ffc_media3, value != "Neighbours")
ffc_media3<-subset(ffc_media3, value != "Family members")
ffc_media3$Channel<-ffc_media3$value
ggplot(ffc_media3)+
  geom_bar(aes(sec_Aq6, fill=Channel), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Three most available communication channels by county")+
  theme(plot.title = element_text(hjust = 0.1))+
  scale_fill_brewer(palette="YlGnBu")

ffc_media6<-subset(ffc_media1, value != "Friends")
ffc_media6<-subset(ffc_media6, value != "Media: TV")
ffc_media6<-subset(ffc_media6, value != "Neighbours")
ffc_media6<-subset(ffc_media6, value != "Family members")
ffc_media6$Channel<-ffc_media6$value
ggplot(ffc_media6)+
  geom_bar(aes(variable, fill=Channel), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "Top 3", title = "Three most available communication channels")+
  theme(plot.title = element_text(hjust = -0.5))+
  scale_fill_brewer(palette="YlGnBu")+
  scale_x_discrete(label=labels_media)


#23) How long would you be willing to walk/travel to a meeting?
ffc_willingness<-select(ffc, sec_Hq202, sec_Hq203)
ffc_willingness$a<-1
ffc_willingness<-melt(ffc_willingness, "a")
labels_willingness<-c("Walk", "Travel")

ggplot(ffc_willingness)+
  geom_bar(aes(variable, fill=value), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "How long would you be willing to walk/travel to a meeting?")+
  theme(plot.title = element_text(hjust = 0.1))+
  scale_fill_brewer(palette="Blues")+
  scale_x_discrete(labels= labels_willingness)
  
#24) Linking income and knowledge
ffc_income<-subset(ffc, log_income != "NA")
lm1<-lm(log_income ~ knowledge_groups, ffc_income)
lm2<-lm(log_mango_income ~ knowledge_groups, ffc_income)

ggplot(ffc_income)+
  geom_smooth(aes(knowledge_groups, log_income), color="#0033a1", fill="steelblue")+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Knowledge of pests and PCTs", y = "Log Income", title = "Knowledge of PCTs/pests and overall income")

ggplot(ffc_income)+
  geom_smooth(aes(knowledge_groups, log_mango_income), color="#0033a1", fill="steelblue")+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Knowledge of pests and PCTs", y = "Log Income", title = "Knowledge of PCTs/pests and income from mango farming")

ggplot(ffc_income)+
  geom_smooth(aes(knowledge_groups, sec_Aq14), color="#0033a1", se=FALSE)+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Knowledge of pests and PCTs", y = "Income", title = "Knowledge of PCTs/pests and yearly income",
       caption = "Knowledge index: number of 
       mango pests and PCTs that 
       farmers are aware of (out of 8)")+
  scale_y_continuous(labels = ks)

ggplot(ffc_income)+
  geom_smooth(aes(pest_knowledge_groups, sec_Aq14), color="#0033a1", se=FALSE)+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Knowledge of pests", y = "Income", title = "Knowledge of mango pests and yearly income",
       caption = "Pests index: number of mango pests that 
       farmers are aware of (out of 5)")+
  scale_y_continuous(labels = ks)

ggplot(ffc_income)+
  geom_smooth(aes(pct_knowledge_groups, sec_Aq14), color="#0033a1", se=FALSE)+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Knowledge of PCTs", y = "Income", title = "Knowledge of PCTs and yearly income",
       caption = "PCT index: number of PCTs that 
       farmers are aware of (out of 7)")+
  scale_y_continuous(labels = ks)

ggplot(ffc_income)+
  geom_smooth(aes(knowledge_groups, sec_Aq140), color="#0033a1", se=FALSE)+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Knowledge of pests and PCTs", y = "Income", title = "Knowledge of PCTs/pests and yearly income from mango farming",
       caption = "Knowledge index: number of 
       mango pests and PCTs that 
       farmers are aware of (out of 8)")+
  scale_y_continuous(labels = ks)

ggplot(ffc_income)+
  geom_smooth(aes(pest_knowledge_groups, sec_Aq140), color="#0033a1", se=FALSE)+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Knowledge of pests", y = "Income", title = "Knowledge of pests and yearly income from mango farming",
       caption = "Pests index: number of mango pests that 
       farmers are aware of (out of 5)")+
  scale_y_continuous(labels = ks)

ggplot(ffc_income)+
  geom_smooth(aes(pct_knowledge_groups, sec_Aq140), color="#0033a1", se=FALSE)+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Knowledge of PCTs", y = "Income", title = "Knowledge of PCTs and yearly income from mango farming",
       caption = "PCT index: number of PCTs that 
       farmers are aware of (out of 7)")+
  scale_y_continuous(labels = ks)

#25) Linking income and engagement
lm1<-lm(log_income ~ engagement_groups, ffc_income)
lm2<-lm(log_mango_income ~ engagement_groups, ffc_income)

ggplot(ffc_income)+
  geom_smooth(aes(engagement_groups, log_income), color="#0033a1", fill="steelblue")+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Engagement with PCTs", y = "Log Income", title = "Engagement with PCTs and overall income")

ggplot(ffc_income)+
  geom_smooth(aes(engagement_groups, log_mango_income), color="#0033a1", fill="steelblue")+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Engagement with PCTs", y = "Log income", title = "Engagement with PCTs and income from mango farming")

ggplot(ffc_income)+
  geom_smooth(aes(engagement_groups, sec_Aq14), color="#0033a1", se=FALSE)+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Engagement with PCTs", y = "Income", title = "Engagement with PCTs and yearly income",
       caption="Engagement index: farmers engagement 
       with fly traps, bio pesticides 
       and baits (from 0 to 6)")+
  scale_y_continuous(labels = ks)

ggplot(ffc_income)+
  geom_smooth(aes(engagement_groups, sec_Aq140), color="#0033a1", se=FALSE)+
  theme(panel.background=element_blank(), plot.title = element_text(hjust = 0.3))+
  labs(x = "Engagement with PCTs", y = "Income", title = "Engagement with PCTs and yearly income from mango farming",
       caption="Engagement index: farmers engagement 
       with fly traps, bio pesticides 
       and baits (from 0 to 6)")+
  scale_y_continuous(labels = ks)

#26) Uptake of PCT by county
lm4<-lm(engagement_groups ~ sec_Aq6, ffc)
ffc_uptake_county<-select(ffc, engagement_groups, sec_Aq6)
ffc_uptake_county<-melt(ffc_uptake_county, "sec_Aq6")

ggplot(ffc_uptake_county)+
  geom_bar(aes(sec_Aq6, value), fill="#0033a1", stat="summary", fun.y=mean)+
  theme(panel.background=element_blank())+
  labs(y = "Engagement level", x = "", title = "Farmers engagement with fly traps, baits and bio pesticides",
       caption="Engagement index: farmers engagement 
       with fly traps, bio pesticides 
       and baits (from 0 to 6)")+
  theme(plot.title = element_text(hjust = 0.5))

#27) Knowledge of PCT by county
lm5<-lm(knowledge_groups ~ sec_Aq6, ffc)
ffc_knowledge_county<-select(ffc, knowledge_groups, sec_Aq6)
ffc_knowledge_county<-melt(ffc_knowledge_county, "sec_Aq6")

ggplot(ffc_knowledge_county)+
  geom_bar(aes(sec_Aq6, value), fill="#0033a1", stat="summary", fun.y=mean)+
  theme(panel.background=element_blank())+
  labs(y = "Knowledge level", x = "", title = "Farmers knowledge of pests and PCTs",
       caption = "Knowledge index: number of 
       mango pests and PCTs that 
       farmers are aware of (out of 8)")+
  theme(plot.title = element_text(hjust = 0.5))

ffc_pest_knowledge_county<-select(ffc, pest_knowledge_groups, sec_Aq6)
ffc_pest_knowledge_county<-melt(ffc_pest_knowledge_county, "sec_Aq6")

ggplot(ffc_pest_knowledge_county)+
  geom_bar(aes(sec_Aq6, value), fill="#0033a1", stat="summary", fun.y=mean)+
  theme(panel.background=element_blank())+
  labs(y = "Knowledge level", x = "", title = "Farmers knowledge of pests by county",
       caption = "Pests index: number of mango pests that 
       farmers are aware of (out of 5)")+
  theme(plot.title = element_text(hjust = 0.5))

ffc_pct_knowledge_county<-select(ffc, pct_knowledge_groups, sec_Aq6)
ffc_pct_knowledge_county<-melt(ffc_pct_knowledge_county, "sec_Aq6")

ggplot(ffc_pct_knowledge_county)+
  geom_bar(aes(sec_Aq6, value), fill="#0033a1", stat="summary", fun.y=mean)+
  theme(panel.background=element_blank())+
  labs(y = "Knowledge level", x = "", title = "Farmers knowledge of PCTs by county",
       caption = "PCT index: number of mango pests that 
       farmers are aware of (out of 7)")+
  theme(plot.title = element_text(hjust = 0.5))


#28) Checking individual or group uptake by county and village
ffc_county_uptake<-select(ffc, sec_Aq6, sec_Hq204)
ffc_village_uptake<-select(ffc, sec_Avillage, sec_Hq204)
ffc_county_uptake<-melt(ffc_county_uptake, "sec_Aq6")
ffc_village_uptake<-melt(ffc_village_uptake, "sec_Avillage")

ggplot(ffc_county_uptake)+
  geom_bar(aes(sec_Aq6, fill=value), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "How do farmers want to take up PCTs?")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Blues")

ggplot(ffc_village_uptake)+
  geom_bar(aes(sec_Avillage, fill=value), position="dodge")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "How do farmers want to take up PCTs?")+
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1) )+
  scale_fill_brewer(palette="Blues")

#29) Correlate engagement and knowledge
ggplot(ffc)+
  geom_smooth(aes(knowledge_groups, engagement_groups), fill="#0033a1", se=FALSE)+
  theme(panel.background=element_blank())+
  labs(x = "Knowledge of pests and PCTs", y = "Engagement with PCTs", title = "Knowledge of pests and PCTs and engagement with PCTs")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(ffc_mbooni)+
  geom_smooth(aes(knowledge_groups, engagement_groups), fill="#0033a1", se=FALSE)+
  theme(panel.background=element_blank())+
  labs(x = "Knowledge of pests and PCTs", y = "Engagement with PCTs", title = "Knowledge of pests and PCTs and engagement with PCTs")+
  theme(plot.title = element_text(hjust = 0.5))

#30) Estimated prices of PCTs
ffc_price_traps<-subset(ffc, sec_Bsec_B_aq133>0)
ffc_price_pesticides<-subset(ffc, sec_Bsect_B_bq151>0)
ffc_price_baits<-subset(ffc, sec_CUse_q168>0)

ggplot(ffc_price_traps) +
  geom_violin(mapping = aes(y = sec_Bsec_B_aq133, x=1), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Price in KSH", x = "Density", title = "Farmers estimates of fly traps prices")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))+
   labs(caption="Based on answers of 
                122 farmers. The other farmers 
                didn't know the price.")+coord_flip()

ggplot(ffc_price_pesticides) +
  geom_violin(mapping = aes(y = sec_Bsec_B_aq133, x=1), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Price in KSH", x = "Density", title = "Farmers estimates of bio pesticide prices")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))+
  labs(caption="Based on answers of 
       14 farmers. The other farmers 
       didn't know the price.")+coord_flip()

ggplot(ffc_price_baits) +
  geom_violin(mapping = aes(y = sec_Bsec_B_aq133, x=1), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Price in KSH", x = "Density", title = "Farmers estimates of baits prices")+
  theme(axis.ticks.x = element_blank(), axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))+
  labs(caption="Based on answers of 
       7 farmers. The other farmers 
       didn't know the price.")+coord_flip()

#31) Creating graphs for engagement and knowledge groups
ggplot(ffc, mapping = aes(x = knowledge_groups)) +
  geom_bar(fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x="Knowledge groups", title = "How much do the farmers know about pests and PCTs?",
       caption="Knowledge index: number of 
       mango pests and PCTs that 
       farmers are aware of (out of 8)")+
  theme(plot.title = element_text(hjust = 0.4))

ggplot(ffc, mapping = aes(x = engagement_groups)) +
  geom_bar(fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x="Engagement groups", title = "How much are the farmers engaged with PCTs?",
       caption="Engagement index: farmers engagement 
       with fly traps, bio pesticides and baits (0, 1 or 2 for each)")+
  theme(plot.title = element_text(hjust = 0.4))

ggplot(ffc, mapping = aes(x = pest_knowledge_groups)) +
  geom_bar(fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x="Knowledge groups", title = "How many mango pests do the farmers know?",
       caption="Number of mango pests that 
       farmers are aware of (out of 5)")+
  theme(plot.title = element_text(hjust = 0.4))

ggplot(ffc, mapping = aes(x = pct_knowledge_groups)) +
  geom_bar(fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x="Knowledge groups", title = "How much do the farmers know about PCTs?",
       caption="Number of PCTs that 
       farmers are aware of (out of 7)")+
  theme(plot.title = element_text(hjust = 0.4))

#32) Checking fly trap uptake by county
ffc_trap_county<-select(ffc, sec_Aq6, engagement_traps)
ffc_trap_county<-melt(ffc_trap_county, "sec_Aq6")
ggplot(ffc_trap_county) +
  geom_bar(mapping = aes(x = sec_Aq6, y=value), fill="#0033a1", stat="summary", fun.y=mean)+
  theme(panel.background=element_blank())+
  labs(y = "Average engagement", x="Knowledge groups", title = "Farmers engagement with fly traps",
       caption="Farmers who always use fruit fly traps: 2
       Farmers who sometimes use fruit fly traps: 1")+
  theme(plot.title = element_text(hjust = 0.4))

#33) Farmers awareness of pest control technologies
ffc_knowledge2<-select(ffc, knowledge6:knowledge12)
ffc_knowledge2$a<-1
ffc_knowledge2<-melt(ffc_knowledge2, "a")
labels_pcts<-c("Bio pesticides", "Fly traps", "Baits", "Solar bags", "Natural enemies/parasitoids", "Pathogenic fungi", "Soil treatments" )
ggplot(ffc_knowledge2) +
  geom_bar(mapping = aes(x = variable, y=value), fill="#0033a1", stat="summary", fun.y=mean)+
  theme(panel.background=element_blank())+
  labs(y = "Awareness of PCTs", x="", title = "Which PCTs do the farmers use or have they heard of?")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.2))+
    scale_y_continuous(labels = scales::percent)+
    scale_x_discrete(labels= labels_pcts)

#34) What else do farmers learn from agrovets?
ffc_agrovet<-subset(ffc, sec_Cq189!="Other (specify)")
labels_agrovet<-c("Advice on where to source for market", "Information on good agricultural practices", "Information on new technologies", "Linkages to spray service providers")
ggplot(ffc_agrovet) +
  geom_bar(mapping = aes(x = sec_Cq189), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x="", title = "What else are farmers receiving from Agrovets?")+
  theme(axis.text.x = element_text(angle = 55, hjust = 1), plot.title = element_text(hjust = 0.2))+
  scale_x_discrete(labels= labels_agrovet)

#35) Most trusted communication channels
ffc_media4<-select(ffc, Sec_Gq199_1:Sec_Gq199_3)
ffc_media4$a<-1
ffc_media4<-melt(ffc_media4, "a")
ffc_media4$value<-mapvalues(ffc_media4$value, from = "Neighbors<e6>", to= "Neighbours")
ffc_media4<-subset(ffc_media4, value != "Social Media: Facebook")
ffc_media4<-subset(ffc_media4, value != "Social Media: WhatsApp")
labels_media<-c("#1", "#2", "#3")

ggplot(ffc_media4)+
  geom_bar(aes(value), fill="#0033a1")+
  theme(panel.background=element_blank())+
  labs(y = "Observations", x = "", title = "Most trusted communication channels")+
  theme(axis.text.x = element_text(angle = 55, hjust = 1), plot.title = element_text(hjust = 0.5))
