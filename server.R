
library(shiny)
library(stats)


shinyServer(
  
  function(input, output){
  

  
    
    output$trendPlot <- renderPlot({
      
      inFile <- input$file1
      
      if (is.null(inFile) & (input$frq == "Uploaded"))
        return(NULL)
      
      if ((input$frq == "Uploaded"))
      {
        Freq <- data.frame(read.csv(inFile$datapath, header = input$header,
                            sep = input$sep, quote = input$quote))
      }
      else{
      Freq <- readRDS(as.character(input$frq))
      }
      
      if (input$ptyp == "TS"){
      
        
        if ((input$frq == "Uploaded"))
        {  
          TimeSeries <- ts(Freq, frequency=12)
        }
        else{
          TimeSeries <- ts(Freq, frequency=365)
        }
      
      plot(decompose(TimeSeries)$trend,
           main=paste('Decomposed ', input$frq, ' Time Series trend plot', sep=''),
           ylab=paste('Time Series trend', sep=''),
           xlab=paste('Year + 1', sep=''))
      
      } else{
        
        if ((input$frq == "Uploaded"))
        {
          barplot(Freq$X,
               main=paste(input$frq, ' events per month barplot', sep=''),
               xlab=paste('Month', sep=''))
        }
        else
        {
          
        barplot(Freq,
                main=paste(input$frq, ' events per day barplot', sep=''),
                xlab=paste('Day', sep=''))
        }
      }
      
    })
      
    
    
    output$seasonPlot <- renderPlot({
      
      
      inFile <- input$file1
      
      if (is.null(inFile) & (input$frq == "Uploaded"))
        return(NULL)
      
      if ((input$frq == "Uploaded"))
      {
        Freq <- data.frame(read.csv(inFile$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote))
      }
      else{
        Freq <- readRDS(as.character(input$frq))
      }      
      

      
      if (input$ptyp == "TS"){
        
        
        if ((input$frq == "Uploaded"))
        {  
          TimeSeries <- ts(Freq, frequency=12)
        }
        else{
          TimeSeries <- ts(Freq, frequency=365)
        }
        

        
        plot(decompose(TimeSeries)$seasonal,
             main=paste('Decomposed ', input$frq, ' Time Series seasonal component plot', sep=''),
             ylab=paste('Time Series seasonal component', sep=''),
             xlab=paste('Period + 1', sep=''))
        
      } else{
        
        
        if ((input$frq == "Uploaded"))
        {
          barplot(Freq$X,
                  main=paste(input$frq, ' events per month barplot', sep=''),
                  xlab=paste('Month', sep=''))
        }
        else
        {        
        barplot(Freq,
                main=paste(input$frq, ' events per day barplot', sep=''),
                xlab=paste('Day', sep=''))
        }
      }
      
    })
    

    
    output$randomPlot <- renderPlot({
      
      
      inFile <- input$file1
      
      if (is.null(inFile) & (input$frq == "Uploaded"))
        return(NULL)
      
      if ((input$frq == "Uploaded"))
      {
        Freq <- data.frame(read.csv(inFile$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote))
      }
      else{
        Freq <- readRDS(as.character(input$frq))
      }            
      

      
      if (input$ptyp == "TS"){
        
        
        if ((input$frq == "Uploaded"))
        {  
          TimeSeries <- ts(Freq, frequency=12)
        }
        else{
          TimeSeries <- ts(Freq, frequency=365)
        }        
        

        
        plot(decompose(TimeSeries)$random,
             main=paste('Decomposed ', input$frq, ' Time Series random component plot', sep=''),
             ylab=paste('Time Series random component', sep=''),
             xlab=paste('Period + 1', sep=''))
        
      } else{
        
        
        if ((input$frq == "Uploaded"))
        {
          barplot(Freq$X,
                  main=paste(input$frq, ' events per month barplot', sep=''),
                  xlab=paste('Month', sep=''))
        }
        else
        {         
        barplot(Freq,
                main=paste(input$frq, ' events per day barplot', sep=''),
                xlab=paste('Day', sep=''))
        }
      }
      
    })
    
    

    output$structTSPlot <- renderPlot({
      
      
      inFile <- input$file1
      
      if (is.null(inFile) & (input$frq == "Uploaded"))
        return(NULL)
      
      if ((input$frq == "Uploaded"))
      {
        Freq <- data.frame(read.csv(inFile$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote))
      }
      else{
        Freq <- readRDS(as.character(input$frq))
      }      
      
      

      
      if (input$ptyp == "TS"){
        
        
        if ((input$frq == "Uploaded"))
        {  
          TimeSeries <- ts(Freq$X, frequency=12)
        }
        else{
          TimeSeries <- ts(Freq, frequency=365)
        }                
        

        
      if ((input$frq == "Uploaded"))
      {  
        tsY <- StructTS(Freq$X, type="trend")
      }
      else {
        tsY <- StructTS(Freq, type="trend")        
      }
        
        plot(cbind(fitted(tsY), resids=resid(tsY)),
             main=paste('Decomposed ', input$frq, ' Time Series trend and residuals plot', sep=''),
             ylab=paste('Time Series trend and residuals, StructTS', sep=''),
             xlab=paste('Period', sep=''))
        
      } else{
        
        if ((input$frq == "Uploaded"))
        {          
        barplot(Freq$X,
                main=paste(input$frq, ' events per month barplot', sep=''),
                xlab=paste('Month', sep=''))
        }
        else
        {
          barplot(Freq,
                  main=paste(input$frq, ' events per day barplot', sep=''),
                  xlab=paste('Day', sep=''))          
        }
        
      }
      
    })
    
    
    
    output$stlPlot <- renderPlot({
      
      
      inFile <- input$file1
      
      if (is.null(inFile) & (input$frq == "Uploaded"))
        return(NULL)
      
      if ((input$frq == "Uploaded"))
      {
        Freq <- data.frame(read.csv(inFile$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote))
      }
      else{
        Freq <- readRDS(as.character(input$frq))
      }      
      
      
      
      if (input$ptyp == "TS"){
        
        
        if ((input$frq == "Uploaded"))
        {  
          TimeSeries <- ts(Freq$X, frequency=12)
        }
        else{
          TimeSeries <- ts(Freq, frequency=365)
        }        
        

        
        plot(stl(TimeSeries, "per"))
        
      } else{
        
        
        if ((input$frq == "Uploaded"))
        {
          barplot(Freq$X,
                  main=paste(input$frq, ' events per month barplot', sep=''),
                  xlab=paste('Month', sep=''))
        }
        else
        {        
        
        barplot(Freq,
                main=paste(input$frq, ' events per day barplot', sep=''),
                xlab=paste('Day', sep=''))
        }
        
      }
      
    })    
    
    
    # Generate a summary of the data
    output$summary <- renderPrint({
      
      inFile <- input$file1
      
      if (is.null(inFile) & (input$frq == "Uploaded"))
        return(NULL)
      
      if ((input$frq == "Uploaded"))
      {
      
        Freq <- data.frame(read.csv(inFile$datapath, header = input$header,
                                    sep = input$sep, quote = input$quote))
        
        summary(Freq$X)
        
      }
      else
      {
        Freq <- readRDS(as.character(input$frq))
        summary(Freq)
      }
      

    })
    

    output$table <- renderDataTable({
      
      inFile <- input$file1
      
      if (is.null(inFile) & (input$frq == "Uploaded"))
        return(NULL)
      
      if ((input$frq != "Uploaded"))
      {
      Freq <- readRDS(as.character(input$frq))
      data.frame(x=Freq)
      }
      
    })  
 
  
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
    })    
    
    
  }
)




