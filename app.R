library(shiny)
library(visNetwork)
library(dplyr)
library(tidyr)
library(shinyalert)
library(shinyjs)
library(DT)
library(stringr)
library(arules)


# df <- read.csv("all_rules.csv", header = TRUE, sep = ",")
# View(df)
# str(df)
# names(df)
# df[1,]
# 
# Groceries <- read.transactions("groceries.csv", sep=",")
# a<-Groceries@itemInfo
# str(a)
# a[5,]
# b<-as.vector(a$labels)
# b[1:5]
# is.vector(b)
# 
# inspect(Groceries[,167])
# itemFrequency(Groceries[,167])
# selector<-b == "whole milk"
# b[selector]
# str(selector)
# 
# selectorA<-b == "rolls/buns"
# which(selectorA %in% c(T))
# itemFrequency(Groceries[,123])
# View(selectorA)
# 
# b[which()]
# 
# gr<-read.csv(file="groceries.csv", header=TRUE, sep=",")
# View(gr)

rulepairing <- function(d) {
  pairRules <- d[c(1:2)]
  names(pairRules)[1] <- "id"
  pairRules <-
    data.frame(lapply(pairRules, as.character), stringsAsFactors = FALSE)
  pairRules$body <-
    unlist(lapply(strsplit(pairRules[[2]], split = " => "), "[", 1))
  pairRules$head <-
    unlist(lapply(strsplit(pairRules[[2]], split = " => "), "[", 2))
  pairRules$support = df$support
  pairRules$confidence = df$confidence
  pairRules$lift = df$lift
  pairRules$count = df$count
  
  return (pairRules)
}

prepareTotal <-
  function(nonFilteredTotal,
           supportInput,
           confidenceInput,
           liftInput) {
    filteredTotal <- data.frame()
    filteredTotal <- subset(
      nonFilteredTotal,
      nonFilteredTotal$support > supportInput[1] &
        nonFilteredTotal$support < supportInput[2] &
        nonFilteredTotal$confidence > confidenceInput[1] &
        nonFilteredTotal$confidence < confidenceInput[2] &
        nonFilteredTotal$lift > liftInput[1] &
        nonFilteredTotal$lift < liftInput[2]
    )
    
    return(filteredTotal)
    
  }

nodesAllRules <- function(topRules, status) {
  if (status == "in") {
    totalRows <- length(unique(topRules$head)) + nrow(topRules)
    Nodes <-
      data.frame(
        id = rep(0, totalRows),
        node = rep(0, totalRows),
        label = rep(0, totalRows)
      )
    Nodes$node <- c(unique(topRules$head), topRules$body)
    Nodes[, "label"] <- Nodes[, "node"]
    for (row in 1:length(unique(topRules$head))) {
      Nodes[row, "id"] <- -row
      
    }
    for (row in 1:nrow(topRules)) {
      Nodes[(row + length(unique(topRules$head))), "id"] <-
        topRules[row, "id"]
      
      
    }
    
    
  }
  else if (status == "out") {
    totalRows <- length(unique(topRules$body)) + nrow(topRules)
    Nodes <-
      data.frame(
        id = rep(0, totalRows),
        node = rep(0, totalRows),
        label = rep(0, totalRows)
      )
    Nodes$node <- c(unique(topRules$body), topRules$head)
    Nodes[, "label"] <- Nodes[, "node"]
    for (row in 1:length(unique(topRules$body))) {
      Nodes[row, "id"] <- -row
      
    }
    for (row in 1:nrow(topRules)) {
      Nodes[(row + length(unique(topRules$body))), "id"] <-
        topRules[row, "id"]
      
      
    }
    
  }
  
  
  
  return(Nodes)
  
}

edgesAllRules <- function(topRules, status, Nodes) {
  if (status == "in") {
    Edges <-
      data.frame(
        from = rep(1, nrow(topRules)),
        to = rep(1, nrow(topRules)),
        arrows = c("from")
      )
    
    for (row in 1:nrow(topRules)) {
      test <- data.frame(filter(Nodes, node == topRules[row, "head"]))
      Edges[row, "from"] <- test[1, "id"]
      Edges[row, "to"] <- topRules[row, "id"]
    }
    
  }
  else if (status == "out") {
    Edges <-
      data.frame(
        from = rep(1, nrow(topRules)),
        to = rep(1, nrow(topRules)),
        arrows = c("to")
      )
    for (row in 1:nrow(topRules)) {
      test <- data.frame(filter(Nodes, node == topRules[row, "body"]))
      Edges[row, "from"] <- test[1, "id"]
      Edges[row, "to"] <- topRules[row, "id"]
    }
    
    
    
  }
  
  
  return(Edges)
  
}
nodesOneRuleBody <- function(topRules, ruleBody , flag) {
  print(paste0("to diplo click einai keno :" , flag))
  
  if (flag) {
    ruleBody <- str_remove_all(ruleBody, "[{}]")
    oneRuleBody <- topRules %>%
      filter(str_detect(body, ruleBody))
    totalRows <-
      length(unique(oneRuleBody$body)) + nrow(oneRuleBody)
  } else{
    oneRuleBody <- subset(topRules , topRules$body == ruleBody)
    totalRows <-  1 + nrow(oneRuleBody)
  }
  
  
  
  # View(oneRuleBody)
  
  Nodes <-
    data.frame(
      id = rep(0, totalRows),
      node = rep(0, totalRows),
      label = rep(0, totalRows)
    )
  Nodes$node <- c(unique(oneRuleBody$body), oneRuleBody$head)
  Nodes[, "label"] <- Nodes[, "node"]
  for (row in 1:length(unique(oneRuleBody$body))) {
    Nodes[row, "id"] <- -row
    
  }
  for (row in 1:nrow(oneRuleBody)) {
    Nodes[(row + length(unique(oneRuleBody$body))), "id"] <-
      oneRuleBody[row, "id"]
    
    
  }
  
  
  # View(Nodes)
  return(Nodes)
}

edgesOneRuleBody <- function(topRules, Nodes, ruleBody) {
  ruleBody <- str_remove_all(ruleBody, "[{}]")
  oneRuleBody <- topRules %>%
    filter(str_detect(body, ruleBody))
  # View(oneRuleBody)
  Edges <-
    data.frame(
      from = rep(1, nrow(oneRuleBody)),
      to = rep(1, nrow(oneRuleBody)),
      arrows = c("to")
    )
  for (row in 1:nrow(oneRuleBody)) {
    test <- data.frame(filter(Nodes, node == oneRuleBody[row, "body"]))
    Edges[row, "from"] <- test[1, "id"]
    Edges[row, "to"] <- oneRuleBody[row, "id"]
  }
  
  return(Edges)
  
}

nodesOneRuleHead <- function(topRules, ruleHead) {
  oneRuleHead <- subset(topRules, topRules$head == ruleHead)
  # View(oneRuleHead)
  totalRows <- 1 + nrow(subset(topRules, topRules$head == ruleHead))
  
  Nodes <-
    data.frame(
      id = rep(0, totalRows),
      node = rep(0, totalRows),
      label = rep(0, totalRows)
    )
  Nodes$node <-
    c(ruleHead, topRules$body[which(topRules$head == ruleHead)])
  Nodes[, "label"] <- Nodes[, "node"]
  
  Nodes[1, "id"] <- -1
  
  for (row in 2:nrow(Nodes)) {
    Nodes[row , "id"] <-
      oneRuleHead[row - 1, "id"]
    
    
  }
  # View(Nodes)
  return(Nodes)
}

edgesOneRuleHead <- function(topRules, Nodes, ruleHead) {
  oneRuleHead <- subset(topRules, topRules$head == ruleHead)
  Edges <-
    data.frame(
      from = rep(1, nrow(oneRuleHead)),
      to = rep(1, nrow(oneRuleHead)),
      arrows = c("to")
    )
  for (row in 1:nrow(oneRuleHead)) {
    Edges[row, "to"] <- Nodes[1, "id"]
    Edges[row, "from"] <- Nodes[row + 1, "id"]
  }
  # View(Edges)
  return(Edges)
  
}

ui <- fluidPage(
  useShinyjs(),
  
  # App title ----
  titlePanel("Assosiation Rules with visNetwork"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      id = "leftPanel",
      radioButtons(
        "ruleParameter",
        label = h3("Primary rule measure"),
        choices = list(
          "Support" = "1",
          "Confidence" = "2",
          "Lift" = "3"
        ),
        selected = 1
      ),
      
      tableOutput("supportB"),
      tableOutput("confidenceB"),
      tableOutput("liftB"),
      sliderInput(
        "numRules",
        label = h3("Number of top rules (N)"),
        min = 0,
        max = nrow(df),
        value = 10,
        step = 1
      ),
      radioButtons(
        "fanMode",
        label = h4("Cluster presentation mode"),
        choices = list("Fan In" = "in", "Fan Out" = "out"),
        selected = "in"
      )
      ,
      width = 2
      
    )
    
    ,
    
    # Main panel for displaying outputs ----
    mainPanel(
      id = "midPanel",
      
      fluidRow(column(4,
                      tableOutput("bodies")),
               column(4, offset = 0, tableOutput("heads"))),
      
      visNetworkOutput("network", height = "55vh"),
      useShinyalert(),
      uiOutput('dt_UI')
      
    )
  )
)


server <- function(input, output, session) {
  values <- reactiveValues(
    rulespair = NULL,
    currentfan = NULL,
    Nodes = NULL,
    Edges = NULL,
    compleTotal = NULL,
    currentBody = NULL,
    currentHead = NULL,
    currentRules = NULL,
    specificRules = NULL
  )
  
  
  output$supportB <- renderUI({
    values$rulespair <- rulepairing(df)
    sliderInput(
      "nodeSupport",
      label = h3("Support"),
      min = min(values$rulespair$support),
      max = max(values$rulespair$support),
      value = c(
        min(values$rulespair$support),
        max(values$rulespair$support)
      ),
      step = 0.005
    )
    
  })
  
  output$confidenceB <- renderUI({
    sliderInput(
      "nodeConfidence",
      label = h3("Confidence"),
      min = min(values$rulespair$confidence),
      max = max(values$rulespair$confidence),
      value = c(
        min(values$rulespair$confidence),
        max(values$rulespair$confidence)
      ),
      step = 0.005
    )
    
  })
  
  output$liftB <- renderUI({
    sliderInput(
      "nodeLift",
      label = h3("Lift"),
      min = min(values$rulespair$lift),
      max = max(values$rulespair$lift),
      value = c(min(values$rulespair$lift), max(values$rulespair$lift)),
      step = 0.005
    )
    
    
  })
  
  
  output$bodies <- renderUI({
    compleTotal <- data.frame()
    compleTotal <-
      prepareTotal(values$rulespair,
                   input$nodeSupport,
                   input$nodeConfidence,
                   input$nodeLift)
    values$compleTotal <- compleTotal
    selectInput(
      "bodyRule",
      label = h4("Select Itemset for Body Rule from current network"),
      choices =  c("All Body Rules", values$currentBody),
      selected = "All Body Rules"
    )
    
  })
  
  output$heads <- renderUI({
    selectInput(
      "headRule",
      label = h4("Select Itemset for Head Rule from current network"),
      choices =  c("All Head Rules", values$currentHead),
      selected = "All Head Rules"
    )
    
  })
  
  
  output$network <- renderVisNetwork({
    req(input$ruleParameter)
    req(input$numRules)
    topRules <- data.frame()
    topRules <- switch(
      input$ruleParameter,
      "1" = values$compleTotal[order(-values$compleTotal$support),],
      "2" = values$compleTotal[order(-values$compleTotal$confidence),],
      "3" = values$compleTotal[order(-values$compleTotal$lift),]
    )
    
    topRules <- head(topRules, input$numRules)
    values$currentRules = topRules
    
    values$currentBody <- unique(topRules$body)
    values$currentHead <- unique(topRules$head)
    if (input$bodyRule == "All Body Rules" &&
        input$headRule == "All Head Rules") {
      values$Nodes <- nodesAllRules(topRules, input$fanMode)
      values$Edges <-
        edgesAllRules(topRules, input$fanMode, values$Nodes)
      
    }
    
    else if (input$bodyRule != "All Body Rules") {
      values$Nodes <-
        nodesOneRuleBody(topRules, input$bodyRule , is.null(clickedNode$clicked))
      
      
      values$Edges <-
        edgesOneRuleBody(topRules, values$Nodes, input$bodyRule)
      
      
      
    } else if (input$headRule != "All Head Rules") {
      values$Nodes <- nodesOneRuleHead(topRules, input$headRule)
      values$Edges <-
        edgesOneRuleHead(topRules, values$Nodes, input$headRule)
      
    }
    
    visNetwork(values$Nodes, values$Edges)  %>%
      visEvents(
        select = "function(values$Nodes) {
                Shiny.onInputChange('current_node_id', values$Nodes.nodes);
                ;}",
        doubleClick = "function(values$Nodes) {
                Shiny.onInputChange('clicked_node_id', values$Nodes.nodes);
                 ;}"
        
      )
    
  })
  
  
  myNode <- reactiveValues(selected = NULL)
  
  
  
  observeEvent(input$current_node_id, {
    myNode$selected <<- input$current_node_id
  })
  
  clickedNode <- reactiveValues(clicked = NULL)
  
  
  #observing the double Clicked Node
  #Case 1 : Outer Node means a positive id. Fetching the rule with that Positive id . It must be one.
  # SubCase 1 : If the body is in BodyRUles and we were watching a fan In mode ->
  #             Set itemset to BodyRule DropDown , update fan mode to Out.
  # SubCase 2 : If the head is in HeadRUles and we were watching a fan Out mode ->
  #             Set itemset to HeadRule DropDown , update fan mode to In.
  #Case 2 : Inner Node means a negative id. Fetching the rules who have as head or body the clickedNode label/node.
  #temp = information about the node with negative id
  #Subcase 1 : if node is in bodyRules dropDown and fan mode is set to In then
  # assign a value to clickedNode so its not null to prevent further conflicts
  # and assign the proper rules to specificRules
  #Subcase 2 :
  observeEvent(input$clicked_node_id, {
    if (input$clicked_node_id > 0) {
      clickedNode$clicked <-
        subset(values$currentRules,
               values$currentRules$id == input$clicked_node_id)
      
      
      if (clickedNode$clicked$body %in% values$currentBody &&
          input$fanMode == "in") {
        updateSelectInput(session, "bodyRule",
                          selected = clickedNode$clicked$body)
        
        updateRadioButtons(session, "fanMode", selected = "out")
        
        
      } else if (clickedNode$clicked$head %in% values$currentHead &&
                 input$fanMode == "out") {
        updateSelectInput(session, "headRule",
                          selected = clickedNode$clicked$head)
        updateRadioButtons(session, "fanMode", selected = "in")
      }
      
    } else if (input$clicked_node_id < 0) {
      temp <-
        subset(values$Nodes , values$Nodes$id == input$clicked_node_id)
      if (input$fanMode == "in") {
        updateSelectInput(session, "headRule",
                          selected = temp$node)
        updateRadioButtons(session, "fanMode", selected = "out")
        clickedNode$clicked <-
          subset(values$currentRules,
                 values$currentRules$head == temp$node)
        values$specificRules <-
          subset(values$currentRules,
                 values$currentRules$head == temp$node)
      } else if (input$fanMode == "out") {
        updateSelectInput(session, "bodyRule",
                          selected = temp$node)
        if (input$bodyRule != "All Body Rules" &&
            input$bodyRule %in% values$currentHead) {
          print("den egine allagi")
        } else {
          updateRadioButtons(session, "fanMode", selected = "in")
        }
        
        clickedNode$clicked <-
          subset(values$currentRules,
                 values$currentRules$body == temp$node)
        values$specificRules <-
          subset(values$currentRules,
                 values$currentRules$body == temp$node)
        
      }
      
      dataTableOutput('allCurrentRules')
      
    }
  })
  
  #Observing Body Rule Drop Down , preparing the proper dataframe with the proper Rules.
  #1st Case : User selects a body rule from dropDown , without making any clicks before on the nodes.
  # The table should show all the rules that the selected itemset is alone or is a part of an itemset
  #2ond Case : User Double Clicks a Node , the table should show only the rules that refers only to that itemset
  #Reseting HeadRule DropDown
  #Sending Data to TableOutput
  #Updating Fan Mode
  observeEvent(input$bodyRule, {
    print("allagi bodyRule")
    if (input$bodyRule != "All Body Rules" &&
        is.null(clickedNode$clicked)) {
      print("apo dropDown xwris diplo click")
      ruleBody <- str_remove_all(input$bodyRule, "[{}]")
      values$specificRules <- values$currentRules %>%
        filter(str_detect(values$currentRules$body, ruleBody))
      
      reset("headRule")
      dataTableOutput('table')
      
      print("DES TO FAN MODE ISWS NA MIN ALLAKSE")
      updateRadioButtons(session, "fanMode", selected = "out")
      
    } else
      if (input$bodyRule != "All Body Rules" &&
          !is.null(clickedNode$clicked))
      {
        View(clickedNode$clicked)
        print("apo dropDown me diplo click")
        values$specificRules <-
          subset(values$currentRules,
                 values$currentRules$body == input$bodyRule)
        clickedNode$clicked = NULL
        reset("headRule")
        
        dataTableOutput('table')
        updateRadioButtons(session, "fanMode", selected = "out")
      }
    
    if (!is.null(clickedNode$clicked) &&
        input$bodyRule == "All Body Rules") {
      print("katharizw to diplo click")
      clickedNode$clicked = NULL
    }
    
  })
  
  #Observing the head of the Rules dropDown , creating the proper dataframe with proper Rules.
  #Sending the dataframe to Table Output
  #reseting Body Rules drop Down
  #Updating Fan Mode
  observeEvent(input$headRule, {
    print("allagi headRule")
    if (input$headRule != "All Head Rules") {
      values$specificRules <-
        subset(values$currentRules,
               values$currentRules$head == input$headRule)
      
      dataTableOutput('table')
      reset("bodyRule")
      updateRadioButtons(session, "fanMode", selected = "in")
      
    }
    
  })
  #Observing both dropDowns
  # Clearing specificRules , in order to show all rules from network at allCurrentRules output
  observeEvent({
    input$headRule
    input$bodyRule
  }, {
    if (input$bodyRule == "All Body Rules" &&
        input$headRule == "All Head Rules") {
      shinyjs::enable("fanMode")
      print("specific Rules Cleaned!")
      values$specificRules = NULL
      dataTableOutput('allCurrentRules')
      
    } else{
      shinyjs::disable("fanMode")
    }
  })
  
  output$table <- renderDataTable({
    tempEdges <-
      data.frame(subset(values$Edges, myNode$selected == values$Edges$from))
    
    if (!is.null(input$current_node_id) &&
        length(values$specificRules) > 0) {
      print("egine click kai prohgithike epilogi apo dropdown")
      if (input$current_node_id > 0) {
        subset(values$currentRules,
               values$currentRules$id == myNode$selected)
      } else if (input$current_node_id < 0) {
        print("+ click + epilogi -> olous tous sxetikous kanones -id ")
        
        temp <-
          subset(values$Nodes ,
                 values$Nodes$id == input$current_node_id)
        
        if (input$fanMode == "in") {
          values$specificRules <-
            subset(values$currentRules,
                   values$currentRules$head == temp$node)
        } else if (input$fanMode == "out") {
          values$specificRules <-
            subset(values$currentRules,
                   values$currentRules$body == temp$node)
        }
      }
      
    } else if (is.null(input$current_node_id) &&
               length(values$specificRules) > 0) {
      print("X click + epilogi -> olous tous sxetikous kanones")
      
      values$specificRules
    } else if (!is.null(input$current_node_id) &&
               length(values$specificRules) == 0) {
      if (input$current_node_id > 0) {
        subset(values$currentRules,
               values$currentRules$id == myNode$selected)
      } else if (input$current_node_id < 0) {
        subset(values$currentRules,
               values$currentRules$id %in% tempEdges$to)
      }
    }
    else if (input$bodyRule == "All Body Rules" &&
             input$headRule == "All Head Rules") {
      print("ta drop Downs einai default ara fere mou olous tous kanones")
      values$currentRules
    }
    
  })
  
  
  
  output$dt_UI <- renderUI({
    print(length(values$specificRules))
    if (nrow(values$Nodes[which(myNode$selected == values$Nodes$id),]) != 0) {
      print("exei ginei click")
      dataTableOutput('table')
    } else if (is.null(input$current_node_id)) {
      print("den exei ginei klik")
      if (length(values$specificRules) > 0) {
        print("exw epileksei kati apo to dropdown")
        dataTableOutput('table')
      } else if (is.null(values$specificRules)) {
        print(" molis fortwse i selida xwris click kai dropdown selection")
        dataTableOutput('allCurrentRules')
      }
      
    }
    
    
  })
  
  output$allCurrentRules <- renderDataTable({
    if (length(values$specificRules) > 0) {
      print("specific Rules not empty")
      
      values$specifcRules
    } else {
      print("specific Rules empty")
      values$currentRules
    }
    
  })
}
shinyApp(ui, server)