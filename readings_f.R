
#install.packages(gender)

library(tidyverse)
library(stringr)
library(gender)


#this should go first

split_readings<-function(complete_df,no_c){ #1
  for (i in 22:33){ #2
    add_readings<-as.data.frame(complete_df[no_c,i])
    names(add_readings)[1]<-"readings"
    
    if (i==22) { #3
      split_readings_df<-rbind(add_readings)
    }else{  #3 - #4
      split_readings_df<-rbind(split_readings_df,add_readings)
    }  #4  
    
  } #2

  
  split_readings_df<-as.data.frame(unlist(split_readings_df))
  names(split_readings_df)[1]<-"reading_list"
  
  split_readings_df<-split_readings_df%>%
    mutate(reading_list=str_trim(reading_list))
  
  split_readings_df
  
} 

split_names<-function(readings_l){
  
  for (i in 1:nrow(readings_l)) {
    
    a<-word(readings_l[i,1],1) %>%
      str_remove(.,"[:punct:]") %>%
      str_trim(.)
    
    b<-word(readings_l[i,1],2) %>%
      str_remove(.,"[:punct:]")%>%
      str_trim(.)
    
    names_df<-data.frame(a,b)
    names(names_df)[1]<-"last_name"
    names(names_df)[2]<-"name_name"
    
    if (i==1) { 
      final_names_df<-rbind(names_df)
    }else{
      final_names_df<-rbind(final_names_df,names_df)
    }  
  }
  
  final_names_df
}


percentage_female<-function(reading_names) {
  
  list_names<-reading_names$name_name
  
  list_names<-list_names %>%
    str_remove(.,"[:punct:]")%>%
    str_trim(.)
  
  count_a<-gender(list_names)%>%
    group_by(gender) %>%
    summarise(n=n()/nrow(.))
  
  if (nrow(count_a)==2) {
    result<-unlist(count_a[1,2])
  }else{
   result<-0
    }
  
  result
}


#count_a<-gender(reading_list_parrado$name_name)%>%
#  group_by(gender) %>%
#  summarise(n=n()/nrow(.))


#reading the database and breaking it


complete_df<-readr::read_csv("div_stats_last.csv")

complete_df<-complete_df %>%
  mutate(sep_read_1=str_split(Readings_1,";"))

complete_df<-complete_df %>%
  mutate(sep_read_2=str_split(Readings_2,";"))

complete_df<-complete_df %>%
  mutate(sep_read_3=str_split(Readings_3,";"))

complete_df<-complete_df %>%
  mutate(sep_read_4=str_split(Readings_4,";"))

complete_df<-complete_df %>%
  mutate(sep_read_5=str_split(Readings_5,";"))

complete_df<-complete_df %>%
  mutate(sep_read_6=str_split(Readings_6,";"))

complete_df<-complete_df %>%
  mutate(sep_read_7=str_split(Readings_7,";"))

complete_df<-complete_df %>%
  mutate(sep_read_8=str_split(Readings_8,";"))

complete_df<-complete_df %>%
  mutate(sep_read_9=str_split(Readings_9,";"))

complete_df<-complete_df %>%
  mutate(sep_read_10=str_split(Readings_10,";"))

complete_df<-complete_df %>%
  mutate(sep_read_11=str_split(Readings_11,";"))

complete_df<-complete_df %>%
  mutate(sep_read_12=str_split(Readings_12,";"))






readings_bauer<-split_readings(complete_df,1)
reading_list_bauer<-split_names(readings_bauer)


readings_ahrens<-split_readings(complete_df,2)
reading_list_ahrens<-split_names(readings_ahrens)


readings_gruener<-split_readings(complete_df,3)
reading_list_gruener<-split_names(readings_gruener)

readings_lancker<-split_readings(complete_df,4)
reading_list_lancker<-split_names(readings_lancker)

readings_patz<-split_readings(complete_df,5)
reading_list_patz<-split_names(readings_patz)

readings_jachten<-split_readings(complete_df,6)
reading_list_jachten<-split_names(readings_jachten)

readings_costello<-split_readings(complete_df,7)
reading_list_costello<-split_names(readings_costello)

readings_bosch<-split_readings(complete_df,8)
reading_list_bosch<-split_names(readings_bosch)

readings_bobic<-split_readings(complete_df,9)
reading_list_bobic<-split_names(readings_bobic)

readings_wucher<-split_readings(complete_df,10) #this is missing
reading_list_wucher<-split_names(readings_wucher)


readings_dkt<-split_readings(complete_df,11) #this is missing
reading_list_dkt<-split_names(readings_dkt)

readings_hassel<-split_readings(complete_df,12)
reading_list_hassel<-split_names(readings_hassel)


readings_hickman<-split_readings(complete_df,13)
reading_list_hickman<-split_names(readings_hickman)

readings_graf<-split_readings(complete_df,14)
reading_list_graf<-split_names(readings_graf)

readings_migliorati<-split_readings(complete_df,15)
reading_list_migliorati<-split_names(readings_migliorati)


readings_hustedt<-split_readings(complete_df,16)
reading_list_hustedt<-split_names(readings_hustedt)

readings_parrado<-split_readings(complete_df,17)
reading_list_parrado<-split_names(readings_parrado)

readings_wegrich<-split_readings(complete_df,18)
reading_list_wegrich<-split_names(readings_wegrich)

readings_kayser<-split_readings(complete_df,19)
reading_list_kayser<-split_names(readings_kayser)

readings_munzert<-split_readings(complete_df,20)
reading_list_munzert<-split_names(readings_munzert)


bauer_female<-percentage_female(reading_list_bauer)

ahrens_female<-percentage_female(reading_list_ahrens)

costello_female<-percentage_female(reading_list_costello)
gruener_female<-percentage_female(reading_list_gruener)

lancker_female<-reading_list_lancker%>%
  percentage_female(.)

patz_female<-patz_female<-reading_list_patz%>%
  percentage_female(.)

jacthen_female<-reading_list_jachten%>%
  percentage_female(.)

costello_female<-reading_list_costello%>%
  percentage_female(.)

bosch_female<-reading_list_bosch%>%
  percentage_female(.)

bobic_female<-reading_list_bobic%>%
  percentage_female(.)

wucher_female<-reading_list_wucher%>%
  percentage_female(.)

dkt_female<-reading_list_dkt%>%
  percentage_female(.)

hassel_female<-reading_list_hassel%>%
  percentage_female(.)

hickman_female<-reading_list_hickman%>%
  percentage_female(.)

graf_female<-reading_list_graf%>%
  percentage_female(.)

migliorati_female<-reading_list_migliorati%>%
  percentage_female(.)

hustedt_female<-reading_list_hustedt%>%
  percentage_female(.)

parrado_female<-reading_list_parrado%>%
  percentage_female(.)

wegrich_female<-reading_list_wegrich%>%
  percentage_female(.)

kayser_female<-reading_list_kayser%>%
  percentage_female(.)

munzert_female<-reading_list_munzert%>%
  percentage_female(.)




classes_1<-c("Bauer_internationalorganizations.pdf","econIIeconomiccrises_Ahrens.pdf",
             "econIIfinmarketpolicy_Gruener.pdf","econIIsustainability_Lancker.pdf",
             "globalgov_Patz_Rev.pdf","globalgoveu_Jachtenfuchs.pdf",
             "international_law_CaliCostello.pdf",
             "introduction_to_economics_Bosch-Rosa.pdf",
             "lawgovernanceruleoflawcrisis_Bobic.pdf",
             "NOinternationalsecurity_Gohdes_Wucherpfennig.pdf",
             "NOlawgov_Dawson-Kurban-Thielborger.pdf","policyprocess_welfareemployment_Hassel.pdf",
             "policyprocessclimate_Hickmann_Fuhr.pdf","policyprocesseduclaborecon_Graf.pdf",
             "policyprocesseu_Migliorati.pdf","public_mgmt_hustedt.pdf","public_mgmt_Parrado.pdf",
             "public_mgmt_Wegrich.pdf","statistics1_Kayser.pdf","statisticsII_Munzert.pdf")


female_list<-c(bauer_female,ahrens_female,gruener_female,lancker_female,patz_female,
            jacthen_female,costello_female,bosch_female,bobic_female,wucher_female,
            dkt_female,hassel_female,hickman_female,graf_female,migliorati_female,
            hustedt_female,parrado_female,wegrich_female,kayser_female,munzert_female)


female_df<-data.frame(classes_1,female_list)

write.csv(female_df,"female_percentage.csv")

#count_a<-gender(reading_list_bauer$name_name)%>%
#  group_by(gender) %>%
#  summarise(n=n()/nrow(.))

#group_by(observation_y_n) %>%
#  summarise(n=n()) %>%

#s<- "Charles,"

#s<-s %>%
 # str_remove(.,"[:punct:]")%>%
#  str_trim(.)


trial<-"Jänicke, Martin; Schreurs, Miranda; Töpfer, Klaus (2015)"
a<-str_split(trial,"[(]")
string_names_final<-a[[1]][1] %>%
  str_trim(.)

no_words<-str_count(string_names_final,'\\w+')

for (i in seq(from=1, to=no_words,by=2)) {
  
  a<-word(string_names_final,i) %>%
    str_remove(.,"[:punct:]") %>%
    str_trim(.)
  
  b<-word(string_names_final,i+1) %>%
    str_remove(.,"[:punct:]")%>%
    str_trim(.)
  
  names_df<-data.frame(a,b)
  names(names_df)[1]<-"last_name"
  names(names_df)[2]<-"name_name"
  
  if (i==1) { 
    final_names_df<-rbind(names_df)
  }else{
    final_names_df<-rbind(final_names_df,names_df)
  }  
  
}


detect_authors<-function(trial) {
  
  
  a<-str_split(trial,"[(]")
  string_names_final<-a[[1]][1] %>%
    str_trim(.)
  
  no_words<-str_count(string_names_final,'\\w+')
  
  for (i in seq(from=1, to=no_words,by=2)) {
    
    a<-word(string_names_final,i) %>%
      str_remove(.,"[:punct:]") %>%
      str_trim(.)
    
    b<-word(string_names_final,i+1) %>%
      str_remove(.,"[:punct:]")%>%
      str_trim(.)
    
    names_df<-data.frame(a,b)
    names(names_df)[1]<-"last_name"
    names(names_df)[2]<-"name_name"
    
    if (i==1) { 
      final_names_df<-rbind(names_df)
    }else{
      final_names_df<-rbind(final_names_df,names_df)
    }  
    
  }
  
  final_names_df
  
}

split_names_b<-function(readings_l){
  
  for (i in 1:nrow(readings_l)) {
  
    string_names<-readings_l[i,1]
    
    if (is.na(string_names)){
      string_names<-"NULL, NULL(2021)"
        }
    
    names_df<-detect_authors(string_names)
    
    
    
    if (i==1) { 
      final_names_df<-rbind(names_df)
    }else{
      final_names_df<-rbind(final_names_df,names_df)
    }  
  }
  
  final_names_df
}

