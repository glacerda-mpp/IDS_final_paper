
#load packages
library(pdftools)
library(tidyverse)
library(stringr)


##1. Diversity statement function##

read_div_statement <- function(syllabus_pdf){
  
  #Cut it by lines
  syllabus_l<-scan(textConnection(syllabus_pdf), what="character",sep = "\n")
  
  #make it a dataframe 
  syllabus_df<-as.data.frame(unlist(syllabus_l)) 
  

  #change name of column
  names(syllabus_df)[1] <- "lines"
  
  #syllabus_df<-tolower(lines)
  
  #find the Diversity statement line
  diversity_line<-grep("Diversity Statement", syllabus_df$lines)
  
  if (length(diversity_line)==0) {
    #Diversity statement
    diversity_line<-grep("Diversity statement", syllabus_df$lines)
  } 
  
  
  #find the Grading line
  assignment_line<- grep("Grading and Assignments", syllabus_df$lines)
  
  if(length(diversity_line)!=0)
  {
  #the diversity statement is between these tw
   diversity_df<-syllabus_df %>%
    slice((diversity_line+1):(assignment_line-1))
  
  #take out those lines that have exactly one character 
  diversity_df<-diversity_df %>%
    filter(nchar(as.character(lines))!=1)

  #create a string
  div_statement<-paste(diversity_df$lines,collapse=" ")
  
  div_statement<- div_statement %>%str_remove(.,"[:digit:]") %>% str_trim(.)
  } else{
    div_statement="NULL"
  }
  
  div_statement
  
}

#@ readings function 

get_readings <- function(syllabus_pdf){
  
  #Cut it by lines
  syllabus_l<-scan(textConnection(syllabus_pdf), what="character",sep = "\n")
  
  #make it a dataframe 
  syllabus_df<-as.data.frame(unlist(syllabus_l))
  
  #change name of column
  names(syllabus_df)[1] <- "lines"
  
  #find the reading and optional reading lines
  reading_lines<- grep("Required Readings", syllabus_df$lines)
  
  if (length(reading_lines)==0){
    reading_lines<-grep("Basic",syllabus_df$lines)
  }
  
  optional_lines<-grep("Optional Readings",syllabus_df$lines)
  
  if (length(optional_lines)==0){
    optional_lines<-grep("Further",syllabus_df$lines)
  }
  
  #split them into vectors
  reading_lines_split<-str_split(reading_lines," ")
  optional_lines_split<-str_split(optional_lines," ")
  
  if (length(reading_lines_split)==length(optional_lines_split)){
  #make a dataframe with all the readings
  for (i in 1:length(reading_lines_split)) {
    slices<-syllabus_df %>%
      slice(as.numeric(reading_lines_split[[i]]):as.numeric(optional_lines_split[[i]]))
    
      if (i==1) {
      readings_df<-rbind(slices)
      } else {
       readings_df<-rbind(readings_df,slices)
      }
    
  }
    warning(paste0("Length required:",length(reading_lines_split)," Length optional:",length(optional_lines_split),"\n Succesful scan"))
  }else{
    
    min_no<-min(length(reading_lines_split),length(optional_lines_split))
    
    for (i in 1:min_no) {
      slices<-syllabus_df %>%
        slice(as.numeric(reading_lines_split[[i]]):as.numeric(optional_lines_split[[i]]))
      
      if (i==1) {
        readings_df<-rbind(slices)
      } else {
        readings_df<-rbind(readings_df,slices)
      }
      
    }
    warning(paste0("Length required:",length(reading_lines_split)," Length optional:",length(optional_lines_split),"\n Unsuccesful scan"))
  }
  
  if (length(reading_lines_split)==length(optional_lines_split)){
  #delete optional reading line
  readings_df<-readings_df %>%
    filter(!str_detect(lines,"Optional Readings"))
  
  readings_df<-readings_df %>%
    mutate(lines=str_replace_all(lines,"Required Readings","")) %>%
    mutate(lines=str_trim(lines))
  } 

  #take out those lines that have exactly one character 
  readings_df<-readings_df %>%
    filter(nchar(as.character(lines))!=1)
  
  #create a string
  readings_str<-paste(readings_df$lines,collapse=" ")
  
  readings_str
  
}


working_directory<-getwd()
list_syllabi<-list.files("./syllabi_pdfs")
pdfs<-map(paste0(working_directory,"/syllabi_pdfs/",list_syllabi),pdf_text)
a<-map(pdfs,read_div_statement)

div_statement_df<-data.frame(unlist(list_syllabi),unlist(a))

#b<-map(pdfs,get_readings)


get_readings(bauer_io_pdf) #subscript out of bounds
get_readings(ahrens_ec_pdf)
get_readings(gruener_fmp_pdf) 
get_readings(lancker_ec2_pdf)
get_readings(patz_gg_pdf)
get_readings(jacthen_gg_pdf) #subscript out of bounds"""" no hay optional
get_readings(cali_gg_pdf) #subscript out of bounds
get_readings(econ_ciril_pdf)
get_readings(lg_bobi_pdf) 
get_readings(inter_sec_gohdes_pdf) #subscript out of bounds
get_readings(law_dkt_pdf) 
get_readings(welfare_pp_pdf)  #subscript out of bounds
get_readings(climate_hf_pdf) #subscript out of bounds !!!! dice Basic
get_readings(process_graf_pdf) #subscript out of bounds
get_readings(processeu_migliorati_pdf) #subscript out of bounds !!!! en dos lineas
get_readings(public_hustedt_pdf) 
get_readings(public_parrado_pdf)
get_readings(public_wegrich_pdf)
get_readings(stats_kayser_pdf) #subscript out of bounds !!!!! #sólo hay required
get_readings(stats_munzert_pdf)



#a<-str_extract_all(a,"(?>=Session)+(?=Required)")


#x1<-"Tutorialspoint is the best resource for tutorials and courses"

#str_extract_all(x1,"(?<=Tutorialspoint).+(?=courses)")




