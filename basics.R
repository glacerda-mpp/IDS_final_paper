
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
  
  #find the Diversity statement line
  diversity_line<-grep("Diversity Statement", syllabus_df$lines)
  
  #find the Grading line
  assignment_line<- grep("Grading and Assignments", syllabus_df$lines)
  
  #the diversity statement is between these tw
  diversity_df<-syllabus_df %>%
    slice((diversity_line+1):(assignment_line-1))
  
  #take out those lines that have exactly one character 
  diversity_df<-diversity_df %>%
    filter(nchar(as.character(lines))!=1)
  
  
  #create a string
  div_statement<-paste(diversity_df$lines,collapse=" ")
  
  
  div_statement<- div_statement %>%str_remove(.,"[:digit:]") %>% str_trim(.)
  
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
  optional_lines<-grep("Optional Readings",syllabus_df$lines)
  
  #split them into vectors
  reading_lines_split<-str_split(reading_lines," ")
  optional_lines_split<-str_split(optional_lines," ")
  
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
  
  #delete optional reading line
  readings_df<-readings_df %>%
    filter(!str_detect(lines,"Optional Readings"))
  
  readings_df<-readings_df %>%
    mutate(lines=str_replace_all(lines,"Required Readings","")) %>%
    mutate(lines=str_trim(lines))
  
  #take out those lines that have exactly one character 
  
  readings_df<-readings_df %>%
    filter(nchar(as.character(lines))!=1)
  
  #create a string
  readings_str<-paste(readings_df$lines,collapse=" ")
  
  readings_str
  
}

working_directory<-getwd()

list_syllabi<-list.files("./syllabi_pdfs")


bauer_io_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[1]))
ahrens_ec_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[2]))  
gruener_fmp_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[3]))
lancker_ec2_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[4]))
patz_gg_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[5]))
jacthen_gg_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[6]))
cali_gg_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[7]))
econ_ciril_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[8]))
lg_bobi_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[9]))
inter_sec_gohdes_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[10]))
law_dkt_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[11]))
welfare_pp_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[12]))
climate_hf_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[13]))
process_graf_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[14]))
processeu_migliorati_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[15]))
public_hustedt_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[16]))
public_parrado_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[17]))
public_wegrich_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[18]))
stats_kayser_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[19]))
stats_munzert_pdf<-pdf_text(paste0(working_directory,"/syllabi_pdfs/",list_syllabi[20]))

read_div_statement(bauer_io_pdf)
read_div_statement(ahrens_ec_pdf)
read_div_statement(gruener_fmp_pdf) #no diversity statmeent (poner el error)
read_div_statement(lancker_ec2_pdf)
read_div_statement(patz_gg_pdf)
read_div_statement(jacthen_gg_pdf)
read_div_statement(cali_gg_pdf)
read_div_statement(econ_ciril_pdf)
read_div_statement(lg_bobi_pdf) #no diversity statement
read_div_statement(inter_sec_gohdes_pdf)
read_div_statement(law_dkt_pdf) #si hay pero es en minúsculas
read_div_statement(welfare_pp_pdf) #no diversity statement
read_div_statement(climate_hf_pdf) #si hay pero es en minúsculas
read_div_statement(process_graf_pdf)
read_div_statement(processeu_migliorati_pdf)
read_div_statement(public_hustedt_pdf)
read_div_statement(public_parrado_pdf)
read_div_statement(public_wegrich_pdf)
read_div_statement(stats_kayser_pdf) #no diversity statemente
read_div_statement(stats_munzert_pdf)


get_readings(bauer_io_pdf) #subscript out of bounds
get_readings(ahrens_ec_pdf)
get_readings(gruener_fmp_pdf) 
get_readings(lancker_ec2_pdf)
get_readings(patz_gg_pdf)
get_readings(jacthen_gg_pdf) #subscript out of bounds
get_readings(cali_gg_pdf) #subscript out of bounds
get_readings(econ_ciril_pdf)
get_readings(lg_bobi_pdf) 
get_readings(inter_sec_gohdes_pdf) #subscript out of bounds
get_readings(law_dkt_pdf) 
get_readings(welfare_pp_pdf)  #subscript out of bounds
get_readings(climate_hf_pdf) #subscript out of bounds
get_readings(process_graf_pdf) #subscript out of bounds
get_readings(processeu_migliorati_pdf) #subscript out of bounds
get_readings(public_hustedt_pdf) 
get_readings(public_parrado_pdf)
get_readings(public_wegrich_pdf)
get_readings(stats_kayser_pdf) #subscript out of bounds
get_readings(stats_munzert_pdf)

