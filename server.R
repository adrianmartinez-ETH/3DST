# drug_1 <- "tocilizumab"
# drug_2 <- "hydroxychloroquine"
library(shiny)
library(igraph)
library(networkD3)
library(shinydashboard)
library(shiny)
library(DT)
library(viridis)
library(shinyjs)
library(V8)


# Define server logic ----
server <- function(input, output) {
  observeEvent(input$instructions, {
    showModal(modalDialog(
      title = "Instructions",HTML(paste0(
      "Write the two drugs of your interest and click on <i>Search</i>. <B>Nodes</B> represent drugs and <B>links</B> indicate the interaction between them. 
      You can apply the coloring or filtering of your choice. A more detailed description of the drugs displayed can be found in the table Selected Drugs.
      The pre-selected medications used in COVID-19 setting can be seen in the table COVID-19 Drugs.",
      "<br>",
      "<br>",
      "<br>",
      "<font color=\"#FFA500\"><b>",
      "<B>NOTE:</B> The drug has to be written as it appears in www.drugs.com/drug-interactions",
      "<font color=\"#000000\"></b>",
      "<br>",
      "<br>",
      "Examples are:",
      "<br>",
      "<br>",
      "<B>simvastatin</B>: https://www.drugs.com/drug-interactions/<B>simvastatin</B>.html",
      "<br>",
      "<br>",
      "<B>lopinavir-ritonavir,kaletra</B>: https://www.drugs.com/drug-interactions/<B>lopinavir-ritonavir,kaletra</B>.html",
      "<br>",
      "<br>",
      "<B>midazolam</B>: https://www.drugs.com/drug-interactions/<B>midazolam</B>.html",
      "<br>",
      "<br>"
      # "Careful attention has to be made on those drugs that interact with the two drugs of interest!"
      )),
      easyClose = TRUE,
      fade = T 
      
    ))
  })
  

  drug_1 <- eventReactive(input$go,{as.character(input$drug1)})
  drug_2 <- eventReactive(input$go,{as.character(input$drug2)})
  
  
  
  # auto_checker <- function(drug1 , drug2, colornode){
  # rm(list = ls())
  # source("code/functions.R")
  # source("/Volumes/adriamar/Projects/COVID19/shiny/functions.R")
  source("functions.R")

  
  toci <- eventReactive(input$go,{interaction_scraper(drug = drug_1())})
  hq <- eventReactive(input$go,{interaction_scraper(drug =  drug_2())})
  
  # toci <- reactive({interaction_scraper(drug = drug_1())})
  # hq <- reactive({interaction_scraper(drug =  drug_2())})
  

  # selected_drugs <- reactive({
    # if(input$drugs_covid == 1){
      # selected_drugs <- read.csv("/Volumes/adriamar/Projects/COVID19/data/who_full_dictionary_with_covid_filter.csv")
      selected_drugs <- read.csv("who_full_dictionary_with_covid_filter.csv")
      # selected_drugs$Drug <- as.character(selected_drugs$Drug)
    # }else if(input$drugs_covid == 2){
      # selected_drugs <- read.csv("/Volumes/adriamar/Projects/COVID19/data/ATC_code_selection2.csv", row.names = 1)
    # }
  # }
  # )
     # selected_drugs2 <- eventReactive(input$go,{
     #  if(input$drugs_covid == "covid"){ 
     #    selected_drugs[selected_drugs$covid == "Yes",]
     #  }else if(input$drugs_covid == "all"){
     #    selected_drugs
     #  }
     # })

     selected_drugs2 <- eventReactive(input$go,{
         selected_drugs
     })
     
  # selected_drugs <- selected_drugs[!duplicated(selected_drugs$Drug),]
  # selected_drugs$ATC_5 <- substr(selected_drugs$Code5,1,5)
  # selected_drugs$ATC_4 <- substr(selected_drugs$Code5,1,4)
  # selected_drugs$ATC_3 <- substr(selected_drugs$Code5,1,3)
  # selected_drugs$ATC_2 <- substr(selected_drugs$Code5,1,2)
  # selected_drugs$ATC_1 <- substr(selected_drugs$Code5,1,1)
  # 
  # selected_drugs$Level1 <- as.character(selected_drugs$Level1)
  # selected_drugs$Level2 <- as.character(selected_drugs$Level2)
  # selected_drugs$Level3 <- as.character(selected_drugs$Level3)
  # selected_drugs$Level4 <- as.character(selected_drugs$Level4)
  # selected_drugs$Level5 <- as.character(selected_drugs$Level5)
  # selected_drugs$Code5 <- as.character(selected_drugs$Code5)
  
  
  all_nodes <- eventReactive(input$go,{nodes_creator(drug_1 = drug_1(), drug1_interactions = toci(), drug_2 = drug_2(),drug2_interactions = hq())})
  

    # 
  selected_nodes <- eventReactive(input$go,{all_nodes()[all_nodes()$id %in% selected_drugs2()$Drug,]})

  
  selected_nodes2 <- eventReactive(input$go,{merge(selected_nodes(), selected_drugs2(), by.x = "id", by.y = "Drug", all.x = T, all.y = F)})

  
  # drug_existing <- reactive({unique(selected_nodes()$main_drug)})
  # main_to_add <- reactive({drug_existing()[!unique(selected_nodes()$main_drug) %in% selected_nodes()$id]})
  # ncols <- reactive({ncol(selected_nodes2())})
  # 
  # 
  # if(length(main_to_add == 0)){
  #   selected_nodes3 <- reactive({selected_nodes2()})
  # }else if(length(main_to_add) == 1){
  #   row_1bind <- reactive({rep(c(main_to_add()[1],"MAIN DRUG"), c(1,ncols())) })
  #   selected_nodes3 <- reactive({rbind(selected_nodes2(),row_1bind())})
  # }else if(length(main_to_add) == 2){
  #   row_1bind <- reactive({rbind(rep(c(main_to_add()[1],"MAIN DRUG"), c(1,ncols())),rep(c(main_to_add()[2],"MAIN DRUG"), c(1,ncols()))) })
  #   selected_nodes3 <- reactive({rbind(selected_nodes2(),row_1bind())})
  # }

  selected_nodes3 <- eventReactive(input$go,{selected_nodes2()})
    id_num <- eventReactive(input$go,{0:(nrow(selected_nodes3())-1)})
    selected_nodes4 <- eventReactive(input$go,{cbind(selected_nodes3(),id_num = id_num())})
 
    


  edges <- eventReactive(input$go,{ edge_creator(drug_1 = drug_1(), drug1_interactions = toci(), drug_2 = drug_2(),drug2_interactions = hq())})


  selected_edges <-  eventReactive(input$go,{edges()[edges()$Target %in% selected_drugs2()$Drug,]})

  
  selected_edges2 <- eventReactive(input$go,{
    selected_edges3 <- merge(selected_nodes4(), selected_edges(), by.x = "id",by.y = "Target", all.x = F, all.y = T)
    selected_edges4 <- selected_edges3[,c("Source","id","id_num", "interaction_num","color_num")]
  })



  # selected_edges3 <- reactive({ 
  #   selected_edges4 <- selected_edges2()
  #   colnames(selected_edges4) <- c("Source","Target","Target_num")
  #   })

  
  selected_edges5 <- eventReactive(input$go,{
    # if(input$drugs_covid == "all"){
    selected_edges6 <- merge(selected_nodes4(), selected_edges2(), by.x = "id",by.y = "Source", all.x = F, all.y = T)
    # }else if(input$drugs_covid == "covid"){
      # selected_edges6 <- merge(selected_nodes4(), selected_edges2(), by.x = "id",by.y = "Source", all.x = T, all.y = T)
      
    # }
    # selected_edges7 <- selected_edges6[,c("id","Target","id_num","Target_num")]
    # colnames(selected_edges) <- c("Source","Target","Source_num","Target_num")
  })

  # output$table <- renderTable(selected_edges5(),
                              # filter = "top" )
# final_edges <-  reactive({data.frame(cbind(Source = selected_edges5()$id_num.x, Target = selected_edges5()$id_num.y))})


  
  selected_nodes5 <- eventReactive(input$go,{
    # if(input$drugs_covid == "all"){
      selected_nodes4()
    # }
    # else if(input$drugs_covid == "covid"){
    #   # selected_nodes4() %>% filter(covid == "Yes")
    #   # selected_nodes4()[selected_nodes4()$id %in% selected_nodes4()$id[selected_nodes4()$covid == "Yes"],]
    #   # selected_nodes4()[selected_nodes4()$covid == "Yes",]
    #   
    #   selected_nodes4()[selected_nodes4()$id %in% covid_drugs$Drug ,]
    # }
    # selected_nodes4()
  })
  
  selected_edges7 <- eventReactive(input$go,{
    # if(input$drugs_covid == "all"){
      selected_edges5()
    # }else if(input$drugs_covid == "covid"){
      # selected_edges5()[selected_edges5()$id.y %in% selected_nodes5()$id[selected_nodes5()$covid == "Yes"],]
      # selected_edges5()[as.character(selected_edges5()$id.y) %in% as.character(selected_drugs$Drug[selected_drugs$covid == "Yes"]) ,]
    # }
  })
  
  # selected_nodes5 <- eventReactive(input$go,{
  #   edg1 <- selected_nodes5()
  #   if(input$both_interaction == "all_drugs"){
  #     edg1[edg1$both == "Yes",]
  #   }else if(input$both_interaction == "interest"){
  #     edg1
  #   }
  # })
  # selected_edges8 <- eventReactive(input$go,{
  #   edg1 <- selected_edges7()
  #   if(input$both_interaction == "all_drugs"){
  #     edg1
  #   }else if(input$both_interaction == "interest"){
  #     
  #     ed_count <- edg1 %>% group_by(id_num.y) %>% tally()
  #     ed_count <- ed_count[ed_count$n == 2,1]
  #     edg1 <- edg1[edg1$id_num.y %in% ed_count,]
  #   }
  #   return(edg1)
  # })


  
  # selected_nodes6 <- reactive({
  #   if(input$drugs_covid == "all"){
  #     selected_nodes5()
  #   }else if(input$drugs_covid == "covid"){
  #     selected_nodes5()[selected_nodes5()$covid == "Yes",]
  #   }
  # })

  output$plot1 <- renderForceNetwork({
    f_e <- selected_edges7()
    f_n <- selected_nodes5()
# f_e <- f_e[f_e$interaction_num.x == 3,]
# f_e <- f_e[f_e$interaction == "Major",]
    # if(input$drugs_covid == "covid"){f_n[f_n$covid=="Yes",]}
    
    
    
    
    if(input$colors_covid == "red_col" & input$var_color == "interaction"){
  
    forceNetwork(Links = f_e, Nodes = f_n,
                 Source = "id_num.x",
                 Target = "id_num.y",
                 NodeID = "id",
                 # Group = "interaction",
                 Group = input$var_color,
                 Value =  "interaction_num.y",
                 linkColour = ifelse(f_e$color_num.y == "red", "red","grey"),
                 zoom = T, 
                 clickAction =    '      d3.select(this).select("circle").transition()
.duration(750)
.attr("r", 30)',
                 charge = -1000,
                 fontSize = 17,
                 opacity = 80,
                 legend = T,
                 opacityNoHover = 10, 
                 bounded = F,
                 colourScale = JS('d3.scaleOrdinal().domain(["Major", "Moderate", "Minor"]).range(["#FF0000",  "#0000FF","#009933"]);')
                 # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
                 # colourScale =  JS('force.alpha(1); force.restart(); d3.scaleOrdinal(d3.schemeCategory20);')
    )
      
    }else if(input$colors_covid == "all_same" & input$var_color == "interaction"){
      forceNetwork(Links = f_e, Nodes = f_n,
                   Source = "id_num.x",
                   Target = "id_num.y",
                   NodeID = "id",
                   # Group = "interaction",
                   Group = input$var_color,
                   Value =  "interaction_num.y",
                   linkColour = "grey",
                   zoom = T, 
                   clickAction = 'd3.select(this).select("circle").transition().duration(5).attr("r", 50)',
                   charge = -1000,
                   fontSize = 17, 
                   opacity = 80,
                   legend = T,
                   opacityNoHover = 10, 
                   bounded = F,
                   colourScale = JS('d3.scaleOrdinal().domain(["Major", "Moderate", "Minor"]).range(["#FF0000", "#0000FF","#009933"]);')
                   # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
                   # colourScale =  JS('force.alpha(1); force.restart(); d3.scaleOrdinal(d3.schemeCategory20);')
      )
      
    }else if(input$colors_covid == "red_col" & input$var_color == "Level1" ){
      
      forceNetwork(Links = f_e, Nodes = f_n,
                   Source = "id_num.x",
                   Target = "id_num.y",
                   NodeID = "id",
                   Group = "interaction",
                   # Group = input$var_color,
                   Value =  "interaction_num.y",
                   linkColour = ifelse(f_e$color_num.y == "red", "red","grey"),
                   zoom = T, 
                   clickAction =    '      d3.select(this).select("circle").transition()
.duration(750)
.attr("r", 30)',
                   charge = -1000,
                   fontSize = 17,
                   opacity = 80,
                   legend = T,
                   opacityNoHover = 10, 
                   bounded = F,
                   # colourScale = JS('d3.scaleOrdinal().domain(["Major", "Moderate", "Minor"]).range(["#FF0000", "#009933" , "#0000FF"]);')
                   # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
                   colourScale =  JS('force.alpha(1); force.restart(); d3.scaleOrdinal(d3.schemeCategory20);')
      )
        
    }else if(input$colors_covid == "all_same" & input$var_color == "Level1"){
      forceNetwork(Links = f_e, Nodes = f_n,
                   Source = "id_num.x",
                   Target = "id_num.y",
                   NodeID = "id",
                   # Group = "interaction",
                   Group = input$var_color,
                   Value =  "interaction_num.y",
                   linkColour = "grey",
                   zoom = T, 
                   clickAction = 'd3.select(this).select("circle").transition().duration(5).attr("r", 50)',
                   charge = -1000,
                   fontSize = 17, 
                   opacity = 80,
                   legend = T,
                   opacityNoHover = 10, 
                   bounded = F,
                   # colourScale = JS('d3.scaleOrdinal().domain(["Major", "Moderate", "Minor"]).range(["#FF0000", "#009933" , "#0000FF"]);')
                   # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
                   colourScale =  JS('force.alpha(1); force.restart(); d3.scaleOrdinal(d3.schemeCategory20);')
      )
      
    }
    
    
    
    
    
    
    
  })
  
  # output$plot1 <-renderSimpleNetwork({
  #   f_e <- final_edges()
  #   simpleNetwork(f_e, height="500px", width="500px", zoom = T, fontSize = 14, charge = -400, nodeColour = "Drug")
  # })
  # 
  final_interaction_table <- eventReactive(input$go,{
    nodes_final <- selected_nodes5()
    # nodes_final[,c("id","Interaction","main_drug","both","Code5","Level1","Level2","Level3","Level4","Level5","covid")]
    nodes_final <- nodes_final[,c(1,2,5,6,10,11,12,13,14,15,20)]
    colnames(nodes_final) <- c("Drug","Degree of Interaction","Interacts with","Interacts with both","ATC 5","Level 1","Level 2","Level 3","Level 4","Level 5","COVID-19 Drug")
    nodes_final$`Interacts with`[nodes_final[,4]=="Yes"] <- "Both"
    return(nodes_final)
  })
  
  output$table <- renderDT(final_interaction_table(),
                           filter = "top" )
  # output$table <- renderTable(selected_nodes5(),
                           # filter = "top" )
  interact_with_2 <- eventReactive(input$go,{
    nodes_final <- selected_nodes5()
    # nodes_final[,c("id","Interaction","main_drug","both","Code5","Level1","Level2","Level3","Level4","Level5","covid")]
    nodes_final2 <- nodes_final[nodes_final$both == "Yes",c(1,10)]
    colnames(nodes_final2) <- c("Drug", "Level 1")
    # nodes_final3 <- nodes_final2[,c(1,2)]
    # nodes_final2 <- nodes_final2[nodes_final2$both == "Yes",1]
    # colnames(nodes_final) <- c("Drug","Interacts with","Interacts with both","ATC 5")
    # nodes_final <- nodes_final[nodes_final$`interacts with` == "Both",]
    return(nodes_final2)
  })
  
  
  output$interact_with_2_table <- renderDT(interact_with_2(), filter = "top")
  
  covid_drugs_final <- selected_drugs[selected_drugs$covid == "Yes",2:ncol(selected_drugs)]
  colnames(covid_drugs_final)[ncol(covid_drugs_final)] <- "COVID-19 Drug"
  covid_drugs_final <- covid_drugs_final[,c("Drug","Level1","Level2","Level3","Level4","Code5")]

  output$covid_table <- renderDT(covid_drugs_final, filter = "top")
  
  
  output$drug1_interactions <- renderValueBox({
    valueBox(
      length(unique(selected_edges7()$id_num.y[selected_edges7()$id == drug_1()])),
      paste0("Interactions with ",drug_1()),
      # drug_2(),
      icon = icon("pills"),
      color = "orange"
    )
  })
  output$drug2_interactions <- renderValueBox({
    valueBox(
      length(unique(selected_edges7()$id_num.y[selected_edges7()$id == drug_2()])),
      paste0("Interactions with ",drug_2()),
            icon = icon("pills"),
      color = "orange"
    )
  })

  output$both_interactions <- renderValueBox({
    valueBox(
      length(unique(selected_nodes5()$id[selected_nodes5()$both == "Yes"])),
      "Interactions with both drugs",
      icon = icon("pills"),
      color = "red"
    )
  })

  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("interacting_drugs.csv", sep = ";")
    },
    content = function(file) {
      write.csv(final_interaction_table(), file, row.names = FALSE)
    }
  )
  
  output$downloadCovidData <- downloadHandler(
    filename = function() {
      paste("COVID_drugs.csv", sep = ";")
    },
    content = function(file) {
      write.csv(covid_drugs_final, file, row.names = FALSE)
    }
  )
  
  output$text_title <- eventReactive(input$go,{renderText
      "Medications that interact with both drugs of interest"
  })
  
  
}