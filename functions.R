interaction_scraper <- function(drug = "tocilizumab"){
  library("dplyr")
  library('rvest')
  for(i in 1:3){
    #Specifying the url for desired website to be scraped
    url <- paste0("https://www.drugs.com/drug-interactions/",drug,"-index.html?filter=",i)
    # url <- "https://www.drugs.com/drug-interactions/tocilizumab-index.html?filter=2"
    #Reading the HTML code from the website
    webpage <- read_html(url)
    #Using CSS selectors to scrape the rankings section
    if(i == 1 ){
      rank_data_html <- html_nodes(webpage,'.column-list-3 .int_1') #minor
      #Converting the ranking data to text
      rank_data <- html_text(rank_data_html)
      interactions_minor <- cbind(id = rank_data, interaction = "Minor", interaction_num = 1, color_num = "yellow")
    }
    else if(i == 2 ){
      rank_data_html <- html_nodes(webpage,".column-list-3 .int_2") #moderate
      #Converting the ranking data to text
      rank_data <- html_text(rank_data_html)
      interactions_moderate <- cbind(id = rank_data, interaction = "Moderate", interaction_num = 5, color_num = "orange")
    }
    else if(i == 3 ){
      rank_data_html <- html_nodes(webpage, ".column-list-3 .int_3") #major
      #Converting the ranking data to text
      rank_data <- html_text(rank_data_html)
      interactions_major<- cbind(id = rank_data, interaction = "Major", interaction_num = 18, color_num = "red")
    }
  }
  interactions <- as.data.frame(rbind(interactions_minor, interactions_moderate, interactions_major))
  interactions$id <- as.character(interactions$id)
  interactions$interaction <- factor(interactions$interaction,ordered = T, levels = c("Minor","Moderate","Major","MAIN DRUG") )
  interactions <- interactions[!duplicated(interactions),]
  interactions$interaction_num <- as.numeric(as.character(interactions$interaction_num))
  return(interactions)
}

edge_creator <- function(drug_1 = "tocilizumab", drug1_interactions, drug_2 = "azithromycin", drug2_interactions ){
  library("dplyr")
  edges <-  as.data.frame(rbind((cbind(Source = rep(drug_1,nrow(drug1_interactions)),  Target = as.character(drug1_interactions$id))),
                                (cbind(Source = rep(drug_2,nrow(drug2_interactions)),  Target = as.character(drug2_interactions$id)))))
  return(edges)
}

nodes_creator <- function(drug_1 = "tocilizumab", drug1_interactions ,drug_2 = "azithromycin", drug2_interactions){
  drug1_interactions$main_drug <- drug_1    #create main drug
  drug1_interactions <- rbind(drug1_interactions,c(drug_2,"Major",3,"red"))
  drug2_interactions$main_drug <- drug_2
  drug1_interactions <- rbind(drug1_interactions,c(drug_1,"Major",3,"red"))
  
  all_nodes <- rbind(drug1_interactions, drug2_interactions)   #drug all nodes of both drugs 
  
  duplicated_nodes <- all_nodes[duplicated(all_nodes$id),c("id","interaction", "interaction_num","color_num")]   #select the duplicated nodes
  
  duplicated_nodes_all <- all_nodes[all_nodes$id %in% duplicated_nodes$id,c("id","interaction", "interaction_num","color_num")] #select the two observations of duplicated
  
  duplicated_nodes_all2 <- duplicated_nodes_all[!duplicated(duplicated_nodes_all),]   #remove those that interaction is the same
  duplicated_nodes_all2$row_num <- rownames(duplicated_nodes_all2)

  if( nrow(duplicated_nodes_all2[duplicated_nodes_all2$id == drug_1,]) == 2){
    duplicated_nodes_all2 <- duplicated_nodes_all2[!(duplicated_nodes_all2$row_num == duplicated_nodes_all2[duplicated_nodes_all2$id == drug_1,5][1]),]
  }
  
  if( nrow(duplicated_nodes_all2[duplicated_nodes_all2$id == drug_2,]) == 2){
    duplicated_nodes_all2 <- duplicated_nodes_all2[!(duplicated_nodes_all2$row_num == duplicated_nodes_all2[duplicated_nodes_all2$id == drug_2,5][1]),]
  }

  
  
    
  duplicated_nodes_all3 <- duplicated_nodes_all2 %>%   #select the ones whose interaction is minimum
  group_by(id) %>%
  filter(interaction == min(interaction) )
  

  all_nodes_final <- all_nodes[-as.numeric(duplicated_nodes_all3$row_num),]  #remove intereaction that is minimum
  
  all_nodes_final$both <- "No"   
  all_nodes_final$both[all_nodes_final$id %in% duplicated_nodes$id] <- "Yes"  #those that are duplicated create a both variable
  
  
  all_nodes_final$order <- 1   #create order variable. It will color nodes depending on which drug they interact with, and color 3 if interact with both
  all_nodes_final$order[all_nodes_final$main_drug == drug_1] <- 1
  all_nodes_final$order[all_nodes_final$main_drug == drug_2] <- 2
  all_nodes_final$order[all_nodes_final$both == "Yes"] <- 3
  all_nodes_final$interaction <- as.character(all_nodes_final$interaction)
  # covid_drugs <- readxl::read_excel("data/drugs_used_in_trials.xlsx")
  # covid_drugs <- covid_drugs[!duplicated(covid_drugs$Drug),]
  return(all_nodes_final)
}

