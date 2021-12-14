
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
  
  count_a<-gender(list_names)%>%
    group_by(gender) %>%
    summarise(n=n()/nrow(.))
  
  result<-unlist(count_a[2,2])
  result
}




#reading the database and breaking it


complete_df<-readr::read_csv("div_statements_complete.csv")

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


#count_a<-gender(reading_list_bauer$name_name)%>%
#  group_by(gender) %>%
#  summarise(n=n()/nrow(.))

#group_by(observation_y_n) %>%
#  summarise(n=n()) %>%


