library(shiny)
library(igraph)

dates = list.files(path  = "log")
dates = gsub("RDS.oldcsvs.", "", dates)
dates = strptime(dates, format = "%Y-%m-%d.%H:%M:%S")

shinyServer(function(input,output){
  
  getClosestData <- reactive({
    d = dates[which.min(abs(input$day - dates))]
    return(d)
  })
  makeFilename <- reactive({
    
    filename = gsub(" ", ".", getClosestData())
    filename =  paste("log/RDS.oldcsvs.",filename, sep = "")
  })
  output$text <- renderText({
   
    paste(getClosestData())
  })
  output$plot <- renderPlot({
 
    data = read.csv(makeFilename(), stringsAsFactors =  FALSE,header = FALSE)
    data$V3[which(data$V3 == "")] = "researchers"
    data$V3[which(is.na(data$V3))] = "researchers"
    parents = data$V3
    children = data$V1
    begats = data.frame(parents = parents, children = children, stringsAsFactors = FALSE)
    if(input$removeInvalids){
     
     begats =  begats[-grep("[A-Z]",begats[,2]),]
     if(length(begats$children) == 0){
       begats = data.frame(parents = parents, children = children, stringsAsFactors = FALSE)
       data$V3[which(data$V3 == "")] = "researchers"
       data$V3[which(is.na(data$V3))] = "researchers"
     }
    }
   
    graph_begats = graph.data.frame(begats)
    edgeList = get.edgelist(graph_begats)
    secondEdgeList = c()
    if(!input$removeInvalidRefs){

      
      allnodes = c(begats$parents,begats$children)
      for (i in 1:length(data$V1)){
        if(length(grep(data$V1[i],allnodes)) > 0){
          for (j in 4:9){
            if(nchar(data[i,j]) > 1 & !is.na(data[i,j])){
            indices = grep(data$V1[i],begats$parents)
            found = FALSE
            if(length(indices) > 0){
              for (k in 1:length(indices)){
                
                if (data[i,j] == begats$children[indices[k]]){
                  
                  found = TRUE
                }
              }
            
              if (!found & (length(grep(data[i,j],allnodes) > 0))){
                #print('here')
                secondEdgeList = c(secondEdgeList,data[i,1],data[i,j])
                #graph_begats = add.edges(graph_begats, c(data[i,1], data[i,j]), attr = list(color = "red"))
              }
            }
            }
          }
            
        }
      }
    }
    graph_begats = graph.edgelist(edgeList) %>%
      set_edge_attr("color", value = "gray") %>%
      add.edges(secondEdgeList, color = "red")
    
    #states
    V(graph_begats)$state = rep('none', length(V(graph_begats)$name))
    indexesInData = sapply(X = V(graph_begats)$name, FUN = grep, x = data[,1], USE.NAMES = FALSE)
    start = 1
    if(min(unlist(lapply(X = indexesInData, FUN = length))) == 0){
      start = 2
    }
    V(graph_begats)$state[start:length(V(graph_begats)$state)] = data[unlist(indexesInData),2]
    V(graph_begats)$color = V(graph_begats)$state
    V(graph_begats)$color[V(graph_begats)$color == "none"] = "green"
    V(graph_begats)$color[V(graph_begats)$color == "d"] = "black"
    V(graph_begats)$color[V(graph_begats)$color == "s"] = "yellow"
    V(graph_begats)$color[V(graph_begats)$color == "q"] = "orange"
    V(graph_begats)$color[V(graph_begats)$color == "n"] = "grey"
    V(graph_begats)$color[V(graph_begats)$color == "c"] = "blue"
    if(input$asTree){
    plot(graph_begats, layout = layout_as_tree, vertex.label = NA)
    }
    else{
      plot(graph_begats,vertex.label = NA)
    }
    colors = c("gray", "red")
    colors2 = c("black", "yellow", "orange", "grey","blue", "green")
    labels = c("Valid referral", "Invalid Referral")
    labels2 = c("No participation", "Survey Sent", "Survey Queued", "Potential Participant", "Completed Survey", "Survey Originator") 
    
    legend(x =1, y =1.65,legend=labels, col=colors, lty = c(1,1),
           title="Referrals")
    legend(x = -2, y = 1.65, legend = labels2, col = colors2, pch = 19, title = "Participants")
    
    #e1 = matrix(c( data$V1, data$V3),ncol = 2, byrow = FALSE)
    #g = graph.data.frame(e1)
    #adjmat = get.adjacency(g,sparse = FALSE)
    #graph = graph_from_adjacency_matrix(adjmat, mode = "undirected")
    #plot.igraph(graph, layout=layout_as_tree)
   
    
  })
})