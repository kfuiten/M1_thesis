# Script A. KEREBEL

library(jsonlite)
library(data.table) # il faut utiliser la fonction fread pour les fichiers ou il y a du json ...
library(stringr)
library(tidyverse) # important: importer tidyverse en dernier pour eviter les conflits avec les fonctions homonymes de stats/signal, etc
#library(lme4)

rm(list = ls())
wd <- "/Users/kassandrafuiten/"
setwd(wd)

# Path to save the data
path_save <- "/Users/kassandrafuiten/"

# Path to load the data
path_questionnaires_data = "/Users/kassandrafuiten/ASRS"
#path_questionnaires_data = "C:/Users/kerebel/Documents/Projet_JA/Passations deuxieme vague/Questionnaires/"

# Full ASRS or not (i.e. A and B or just A)
full=TRUE

# Save name
save_name="ASRS.csv"

# Functions to be used here
adapt_to_json <- function(my_str){
  paste (unlist (strsplit (my_str, '\"')[[1]][-1]), collapse='\"')
}

label_to_nb_asrs<-function(my_lab){
  scale<-c("Never", "Rarely", "Sometimes", "Often", "Very_often")
  my_nb<-which(scale==my_lab)
  return(my_nb)
}

labels_to_score_stai <- function(all_rep, labels_in_order, rev_list){
  # rev_list: vector of the nb of the questions that are formulatd in reverse (formulation positive pour l'anxiete)
  
  all_scores <- c()
  for (r in 1:length(all_rep)){
    if (r %in% rev_list){
      labs <- rev(labels_in_order)
    }else{
      labs <- labels_in_order
    }
    tmp_score <- which(labs==all_rep[[r]])
    all_scores <- c(all_scores, tmp_score)
  }
  
  rep_score <- sum(all_scores)
  
  return(rep_score)
}

############################################################################################################
# Pre-processing des reponses aux questionnaires
##################################################

# Import data
file_list <- list.files(path=path_questionnaires_data)
quest_data <- data.frame()

for (i in 1:length(file_list)){
  #i=1
  temp_fname <- file_list[i]
  temp_data <- read.csv(paste0(path_questionnaires_data, "/", temp_fname), encoding="UTF-8", stringsAsFactors = FALSE)

  # Compute ASRS score
  hit_set_high <- c("Sometimes", "Often", "Very_often")
  hit_set_low <- c("Often", "Very_often")
  tmp_id <- temp_data%>%filter(screen=="subject_ID")%>%pull(responses)%>%fromJSON()%>%as.numeric()
  
  asrs <- temp_data%>%filter(screen=="asrs_scale")%>%pull(responses)%>%fromJSON()
  
  q_low_th <- c(1,2,3,9,12,16,18) # questions dont le seuil est "Parfois"/"Sometimes"
  q_high_th <- c(4,5,6,7,8,10,11,13,14,15,17) # questions dont le seuil est "Souvent"/"Often"
  asrs_nb<-lapply(asrs, label_to_nb_asrs)%>%unlist()%>%t()%>%t()%>%data.frame()
  colnames(asrs_nb)<-"response"
  asrs_nb$q_label<-row.names(asrs_nb)
  asrs_nb<-asrs_nb%>%
    mutate(q_nb=substr(q_label, 8, nchar(q_label))%>%as.integer()) # juste au cas ou les questions ne seraient pas le bon ordre
  
  th_recap<-data.frame(q_nb=c(q_low_th, q_high_th),
                       th=c(rep(2, length(q_low_th)),
                            rep(3, length(q_high_th))))
  
  asrs_nb<-plyr::join(asrs_nb, th_recap, by="q_nb")
  asrs_nb<-asrs_nb%>%mutate(hit=as.integer(response>th))
  
  tmp_asrs_A_hit_score<-asrs_nb%>%filter(q_nb<=6)%>%pull(hit)%>%sum()
  tmp_asrs_adhd_detected <- (tmp_asrs_A_hit_score>=4)
  tmp_asrs_A_raw_score <- asrs_nb%>%filter(q_nb<=6)%>%pull(response)%>%sum()
  
  tmp_asrs_AB_hit_score <- asrs_nb%>%pull(hit)%>%sum()
  tmp_asrs_AB_raw_score <- asrs_nb%>%pull(response)%>%sum()
  
  q_A_h<-c(5,6)
  q_A_a<-c(1,2,3,4)
  tmp_asrs_A_att_hit_score<-asrs_nb%>%filter(q_nb%in%q_A_a)%>%pull(hit)%>%sum()
  tmp_asrs_A_att_raw_score <- asrs_nb%>%filter(q_nb%in%q_A_a)%>%pull(response)%>%sum()
  
  tmp_asrs_A_hyp_hit_score<-asrs_nb%>%filter(q_nb%in%q_A_h)%>%pull(hit)%>%sum()
  tmp_asrs_A_hyp_raw_score <- asrs_nb%>%filter(q_nb%in%q_A_h)%>%pull(response)%>%sum()
  
  # Compute STAI scores
  #stai_t <- temp_data%>%filter(screen=="STAI_t_scale")%>%pull(responses)%>%fromJSON()
  
  # reversed = si le sujet repond "beaucoup" ca veut dire qu'il n'est pas anxieux -> c'est les enonces positifs
  #rev_stai_t <- c(1,3,6,7,10,13,14,16,19)
  
  #labels_stai_t <- c("presque_jamais", "quelquefois", "souvent", "presque_toujours") 

  #tmp_stai_t_score <- labels_to_score_stai(stai_t, labels_stai_t, rev_stai_t)
 
  tmp_res <- data.frame(subject_id = tmp_id,
                        asrs_A_raw_score = tmp_asrs_A_raw_score,
                        asrs_A_hit_score = tmp_asrs_A_hit_score,
                        asrs_adhd_detected = tmp_asrs_adhd_detected, 
                        #stai_t_score = tmp_stai_t_score,
                        asrs_A_att_hit_score = tmp_asrs_A_att_hit_score,
                        asrs_A_att_raw_score = tmp_asrs_A_att_raw_score,
                        asrs_A_hyp_hit_score = tmp_asrs_A_hyp_hit_score,
                        asrs_A_hyp_raw_score = tmp_asrs_A_hyp_raw_score,
                        
                        # normalize
                        asrs_A_raw_score_norm = tmp_asrs_A_raw_score/6,
                        asrs_A_hit_score_norm = tmp_asrs_A_hit_score/6,
                        
                        asrs_A_att_hit_score_norm = tmp_asrs_A_att_hit_score/length(q_A_a),
                        asrs_A_att_raw_score_norm = tmp_asrs_A_att_raw_score/length(q_A_a),
                        asrs_A_hyp_hit_score_norm = tmp_asrs_A_hyp_hit_score/length(q_A_h),
                        asrs_A_hyp_raw_score_norm = tmp_asrs_A_hyp_raw_score/length(q_A_h)
                        )
  
  if(full){
    tmp_res$asrs_AB_raw_score = tmp_asrs_AB_raw_score
    tmp_res$asrs_AB_hit_score = tmp_asrs_AB_hit_score
    
    q_AB_h<-c(5,6,12,13,14,15,16,17,18)
    q_AB_a<-c(1,2,3,4,7,8,9,10,11)
    
    tmp_res$asrs_AB_att_hit_score<-asrs_nb%>%filter(q_nb%in%q_AB_a)%>%pull(hit)%>%sum()
    tmp_res$asrs_AB_att_raw_score <- asrs_nb%>%filter(q_nb%in%q_AB_a)%>%pull(response)%>%sum()
    
    tmp_res$asrs_AB_hyp_hit_score<-asrs_nb%>%filter(q_nb%in%q_AB_h)%>%pull(hit)%>%sum()
    tmp_res$asrs_AB_hyp_raw_score <- asrs_nb%>%filter(q_nb%in%q_AB_h)%>%pull(response)%>%sum()
    
    # normalize
    tmp_res$asrs_AB_raw_score_norm = tmp_asrs_AB_raw_score/18
    tmp_res$asrs_AB_hit_score_norm = tmp_asrs_AB_hit_score/18
    
    tmp_res$asrs_AB_att_hit_score_norm<-tmp_res$asrs_AB_att_hit_score/length(q_AB_a)
    tmp_res$asrs_AB_att_raw_score_norm<-tmp_res$asrs_AB_att_raw_score/length(q_AB_a)
  
    tmp_res$asrs_AB_hyp_hit_score_norm<-tmp_res$asrs_AB_hyp_hit_score/length(q_AB_h)
    tmp_res$asrs_AB_hyp_raw_score_norm<-tmp_res$asrs_AB_hyp_raw_score/length(q_AB_h)
    
  }
  quest_data <- rbind(quest_data, tmp_res)
}

# Write to file
#write.csv(quest_data,paste0(path_save, "questionnaires_data_corr.csv"), fileEncoding = "UTF-8", row.names=FALSE)
write.csv(quest_data,paste0(path_save, save_name), fileEncoding = "UTF-8", row.names=FALSE)

#check for correlation between ASRS A (partial score) and ASRS AB (full score) 
library(ggplot2)
ggplot(quest_data, aes(x=asrs_A_hit_score, y=asrs_AB_hit_score)) + geom_point() + geom_jitter()

