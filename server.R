#to increase datafile upload capacity
options(shiny.maxRequestSize=30*1024^2)
app_name <- "multipage demo"
# is_logged <- FALSE
# app_key <- "application-key"
source("read.R")

render_page <- function(..., f, title = app_name, theme = shinytheme("cerulean")) {
  page <- f(...)
  renderUI({
    fluidPage(page, title = title, theme = theme)
  })
}

server <- function(input, output, session) {
  ######################################################
  ###################Linking pages ###################
  ######################################################
  ### Linking page 1 to page 2
  output$page <- render_page(f = ui_login)
  observeEvent(input$login_login, {
    
    output$page <- render_page(f = ui_application)
    
  })
  ### Linking page 2 to page 1
  observeEvent(input$application_back, {
    output$page <- render_page(f = ui_login)
  })
  ### Linking page 2 to page 3
  observeEvent(input$application_next, {
    output$page <- render_page(f = ui_algoPlot)
  })
  
  observeEvent(input$input_dis_next, {
    output$page <- render_page(f = ui_selectalgo)
  })
  
  ### Selection of preference
  observe({
    shinyjs::toggleState("Prefoption", input$Preference == FALSE)
  })
  
  
  
  ### On pressing Run Forecast
  observeEvent(input$run_forecast, {
    horizon = as.numeric(input$horizon)
    algo_methods <- c( "ARIMA","Holt Winter","Croston","Simple Moving Average")
    
    if(as.character(input$Preference) == "TRUE"){
      isRecommended <<- TRUE
      print("min_val")
      forecast_val_list<-as.list(rep(0,length(servicelist)))
      summary_algo<<-as.list(rep(0,length(servicelist)))
      for(i in 1:length(servicelist)){
        min_val = min(final_err[i,])
        
        min_index=which.min(final_err[i,])
        #print(min_index)
        data_orig <- call(servicelist[i],vector_freq)
        #print("Called")
        data_orig <- as.data.frame(data_orig[[1]])
        #print("Converted")
        data_orig <- cbind(vector_freq$Weeks,vector_freq$`Total Ticket`,data_orig)
        print("Binded")
        colnames(data_orig)[1] = "Weeks"
        colnames(data_orig)[2] = "Total Ticket"
        print(head(data_orig))
        if(min_index == 1){
          fore_val<-ARIMA(data_orig,horizon)
          horizon_week<-fore_val[,1]
          forecast_val_list[[i]] <-fore_val[,2:length(fore_val)]
          summary_algo[[i]]<<-c("ARIMA",min_val)
          print("ARIMA")
        }else if(min_index == 2){
          fore_val<-HWplot(data_orig,horizon)
          horizon_week<-fore_val[,1]
          forecast_val_list[[i]] <-fore_val[,2:length(fore_val)]
          print("HW")
          summary_algo[[i]]<<-c("Holt Winters",min_val)
        }else if(min_index == 3){
          fore_val<<-Croston(data_orig,horizon)
          horizon_week<-fore_val[,1]
          forecast_val_list[[i]] <-fore_val[,2:length(fore_val)]
          summary_algo[[i]]<<-c("Croston",min_val)
          print("Croston")
        }else if(min_index == 4){
          fore_val<<-MA(data_orig,horizon)
          horizon_week<-fore_val[,1]
          forecast_val_list[[i]] <-fore_val[,2:length(fore_val)]
          print("MA")
          summary_algo[[i]]<<-c("Simple Moving Average",min_val)
        }
        print(head(forecast_val_list[[i]]))
      }
      
      forecast_val<<-cbind(horizon_week)
      for(i in 1:length(servicelist)){
        forecast_val<<-cbind(forecast_val,forecast_val_list[[i]])
      }
      print(input$Preference)
    }else{
      forecast_val<<-calculateForecastVal(input$Prefoption,vector_freq,horizon)
    }
    
    forecast_timestamp = c(as.Date(max(week_timestamp$Timestamp)+days(7)))
    temp = forecast_timestamp[1]
    for( i in 2:horizon)
    {
      temp = temp + days(7)
      forecast_timestamp = c(forecast_timestamp,temp)
    }
    download_data<<-cbind(forecast_timestamp,forecast_val)
    final_vals<<-as.list(rep(0,length(servicelist)))
    for(i in 1:length(servicelist))
    {
      final_vals[[i]]<<-plotForecastData(servicelist[i],vector_freq,forecast_val,horizon)
    }
    output$page <- render_page(f = ui_forecastPlot)
  })
  
  
  
  ########################################################
  ################### Application Code ###################
  ########################################################
  ### Render plot
  tryCatch({
    output$contents <- renderTable({
      
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)
      
      ### Reads the file uploaded
      #print(inFile$name)
      data_val<<-read_file(inFile$datapath,inFile$name)
      vector_freq<<-modulrise_data(data_val)
      
      
      output$summary <- renderPrint({
        summary(data_val)
      })
      dval<-data_val
      colnames(dval)[5] <- "Weeks (Calculated)"
      #dval[,1][[1]] <- as.Date(dval[,1][[1]])
      #dval[,5][[1]]<- as.numeric(dval[,5][[1]])
      head(dval)
    })
    
    output$ui.action <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("login_login", "Submit", icon = icon("sign-in"), width = "100px")
    })
    
    output$text <- renderUI({
      attach(data_val)
      time<-data_val[order(Timestamp),]
      set_time<-unique(time$Timestamp)
      diff_time <- set_time[2] - set_time[1]
      str <- paste(strong("Source File: "), input$file1$name)
      str1 <- paste(strong("Account: "), input$acc)
      str2 <- paste(strong("Industry: "),input$ind)
      str3 <- paste(strong("Data Period From: "),min(data_val$Timestamp))
      str4 <- paste(strong("Data Period To: "),max(data_val$Timestamp))
      if(diff_time==1){
        str5 <- paste(strong("Frequency: "),"Daily")
      }else if(diff_time>1 && diff_time<=7){
        str5 <- paste(strong("Frequency: "),"Weekly")
      }else{
        str5 <- paste(strong("Frequency: "),"Monthly")
      }
      val1<-paste(str, str1, sep = '<br/><br/>')
      val2 <- paste(val1, str2, sep = '<br/><br/>')
      val3 <- paste(val2, str3, sep = '<br/><br/>')
      val4 <- paste(val3, str4, sep = '<br/><br/>')
      HTML(paste(val4,str5,sep = '<br/><br/>'))
    })
    
    output$text1 <- renderUI({
      inFile <- input$file1
      data_val1<-read_file2(inFile$datapath,inFile$name)
      TOTAL_TICKETS = dim(data_val1)[1]
      MISSING_PERCENT = (sum(is.na(data_val1$Timestamp))/TOTAL_TICKETS)*100
      data_val1 = na.omit(data_val1)
      
      str <- paste(strong("Total Observations: "), TOTAL_TICKETS)
      str1 <- paste(strong("Percentage of Missing data: "), paste(MISSING_PERCENT,"%"))
      HTML(paste(str,str1,sep = '<br/><br/>'))
    })
    
    ### Plot of trend
    tryCatch({
      output$trendplot<- renderPlot({
        decomp_data<-find_trend(data_val) 
        z<-data.frame(cbind.data.frame(data_val$Timestamp,data_val$Week))
        colnames(z) <- c("Timestamp","Weeks")
        k <- z[order(Timestamp),]
        cross_off = 0
        val<-c()
        for(x in 1:dim(k)[1])
        {
          if(k[x,2]==cross_off)
          {
            cross_off = cross_off + 1
            val<-rbind.data.frame(val,cbind.data.frame(k[x,1],k[x,2]))
          }
        }
        colnames(val) <- c("Timestamp","Weeks")
        week_timestamp<<-cbind.data.frame(decomp_data$time.series[,2],val$Timestamp)
        colnames(week_timestamp) <<- c("Freq","Timestamp")
        ggplot(week_timestamp, aes(x=as.Date(Timestamp), y=Freq, group=1)) + geom_line() + geom_point() + scale_x_date(date_labels="%d-%m-%y",date_breaks  ="1 month") +xlab("Timestamps") + ylab("Count of Tickets") + ggtitle("Trend of Count of Tickets") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
      })
    },error = function(e) {
      stop(e)
    },finally = {
      #stop("Stop Plotting Trend")
    })
    output$serviceLine_Tick <- renderPlot({
      
      #Calculate MASE & MAPE Values for all algo method
      ARIMA_mat<<- RMSE_ARIMA(vector_freq)
      HW_mat <<- RMSE_HW(vector_freq)
      Croston_mat <<- RMSE_Croston(vector_freq)
      MA_mat <<- RMSE_MA(vector_freq)
      
      #Get list of ServiceLine and Severity
      servicelist <<- unique(data_val$ServiceLine)
      severity <<- unique(data_val$Severity)
      
      severity<<-sort(severity, decreasing = TRUE) #Crirtical to Minor
      
      #Plot Count of ServiceLine 
      counts<-data.frame(table(factor(data_val$ServiceLine)))
      ggplot(counts, aes(x=Var1, y=Freq)) +
        geom_bar(stat="identity") + xlab("ServiceLine") + ylab("Count of Tickets") + ggtitle("ServiceLine Vs Count of Tickets") + theme(plot.title = element_text(hjust = 0.5))
      
    })
    output$severity_Tick <- renderPlot({
      
      #Plot Count of Severity
      counts<-data.frame(table(factor(data_val$Severity)))
      ggplot(counts, aes(x=Var1, y=Freq)) +
        geom_bar(stat="identity") + xlab("Severity") + ylab("Count of Tickets") + ggtitle("Severity Vs Count of Tickets") + theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$heat_wave <- renderPlot({

      #Plot Heat Map Count of tickets with a particuar ServiceLine and Severity    
  
      counts<-table(factor(data_val$Severity),factor(data_val$ServiceLine))
      counts <- data.frame(counts)
      ggplot(counts, aes(Var1,Var2, fill = Freq)) + 
        geom_tile(colour = "white") + #geom_raster() +
        coord_fixed(ratio = 1)  + 
        scale_fill_gradientn(colours = rev(rainbow(5))) + ylab("ServiceLine") +
        xlab("Severity") +
        ggtitle("Heat Map Plot of Serviceline and Severity against Count of Tickets")
    })
    
    output$table <- renderDataTable({
      #Values in Table Format for above heatmap
      
      b<-table(factor(data_val$Severity),factor(data_val$ServiceLine))
      cbind(b)
    })
    
    # output$Error<- renderUI({
    #   out <- tabelize(servicelist,err) 
    #   # additional options to make rendering possible
    #   return(div(HTML(out),class="shiny-html-output"))
    # })
    for (i in 1:MAX_servicelength) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      # print("Helo")
      # print(length(servicelist))
      # 
      local({
        
        my_i <- i
        tablename <- paste("table", my_i, sep="")
        col_names<-c()
        output[[tablename]] <- renderTable({
          
          #error_mat<-getError("Hardware",ARIMA_mat,HW_mat,Croston_mat,MA_mat)
          error_mat<<-getError(servicelist[my_i],ARIMA_mat,HW_mat,Croston_mat,MA_mat)
          # error_mat<<-t(error_mat)
          # error_select<-as.data.frame(error_mat)
          table_data<-list()
       
          for(k in 1:4)
          {
            table_data[["MAPE"]]<-c(table_data[["MAPE"]],mean(error_mat[[k]][,1]))
            table_data[["MASE"]]<-c(table_data[["MASE"]],mean(error_mat[[k]][,2]))
          }
         row<-do.call(rbind,list(table_data$MASE))
          final_err<<-rbind(final_err,row)
          final_err<<-as.data.frame(final_err)
          print(final_err)
          algo_methods <- c( "ARIMA","Holt Winter","Croston","Simple Moving Average")
          #combine<-cbind(error_mat[[1]],error_mat[[2]],error_mat[[3]],error_mat[[4]])
          #col_names<- c(col_names,colnames(error_mat)[i])
          
          st <- rbind(
            data.frame(error_mat[[1]], service=servicelist[my_i] ,station = algo_methods[1], what = factor(rownames(error_mat[[1]]), levels = rownames(error_mat[[1]])),
                         row.names= NULL, check.names = FALSE),
             data.frame(error_mat[[2]],service=servicelist[my_i],station = algo_methods[2],what = factor(rownames(error_mat[[2]]), levels = rownames(error_mat[[2]])),
                         row.names = NULL,check.names = FALSE),
            data.frame(error_mat[[3]],service=servicelist[my_i],station = algo_methods[3],what = factor(rownames(error_mat[[3]]), levels = rownames(error_mat[[3]])),
                      row.names = NULL,check.names = FALSE),
            data.frame(error_mat[[4]],service=servicelist[my_i],station = algo_methods[4],what = factor(rownames(error_mat[[4]]), levels = rownames(error_mat[[4]])),
                       row.names = NULL,check.names = FALSE)
             )
          mytable <- tabular(Heading()*what ~ Heading()*service*Heading()*station*(`MAPE` +`MASE`)*Heading()*(identity),data=st)
         finaltable<-data.matrix(mytable)
         severity2=c("","","",severity)
         finaltable[,1]=severity2
         finaltable
           # print(mytable)
          #xtable(mytable,caption = servicelist[my_i])
        
        
        # caption = servicelist[my_i],
        #caption.placement = getOption("xtable.caption.placement", "bottom"), 
        
        #caption.width = getOption("xtable.caption.width", NULL))
        #rownames(final_err)<<-servicelist
         
      },sanitize.text.function=function(x){x},colnames=FALSE)
    })
    
    }
    for( x in 1:MAX_servicelength){ #Some intial value to create array I think. I have no idea. It works!!
      local({
        my_i <- x
        plotname <- paste("plot", my_i, sep="")
        output[[plotname]] <- renderPlot({
          print(my_i)
          print(final_err)
          #print(input$horizon)
          #print(input$Preference)
          #print(input$Prefoption)
          #print(input$t)
          #horizon <- as.numeric(input$horizon)
          # if(as.character(input$Preference) == "TRUE"){
          #   print("min_val")
          #   
          #   min_val = min(final_err[my_i,])
          #   print(min_val)
          #   min_index=which.min(final_err[my_i,])
          #   print(min_index)
          #   if(min_index == 1){
          #     forecast_val<<-ARIMA(vector_freq,horizon)
          #     print("ARIMA")
          #   }else if(min_index == 2){
          #     forecast_val<<-HWplot(vector_freq,horizon)
          #     print("HW")
          #   }else if(min_index == 3){
          #     forecast_val<<-Croston(vector_freq,horizon)  #Change to Croston
          #     print("Croston")
          #   }else if(min_index == 4){
          #     forecast_val<<-MA(vector_freq,horizon)
          #     print("MA")
          #   }  
          #   print(input$Preference)
          # }else{
          #   if(input$Prefoption=="Croston"){
          #     forecast_val<<-Croston(vector_freq,horizon)
          #   }else if(input$Prefoption=="Simple Moving Average"){
          #     forecast_val<<-MA(vector_freq,horizon)
          #   }else if(input$Prefoption=="Holt Winter"){
          #     forecast_val<<-HWplot(vector_freq,horizon)
          #   }else if(input$Prefoption=="ARIMA"){
          #     forecast_val<<-ARIMA(vector_freq,horizon)
          #   }
          # }
          
          # orig_fore<-rbind(vector_freq,forecast_val)
          # 
          # #Uncomment to Include Original Data
          # #forecast_val <- orig_fore
          # 
          # d<<-call(input$t,forecast_val)
          # #print(head(d))
          # col_names_d = colnames(d$input$t)
          # #print()
          # d<<-na.omit(d)
          # d<<-as.data.frame(d)
          # 
          # colnames(d) <<- col_names_d
          # print(head(d))
           layout_array = c()
           for(i in 1:length(severity)){
             layout_array = c(layout_array,i)
           }
          # 
          # forecast_timestamp = c(max(week_timestamp$Timestamp)+days(7))
          # temp = forecast_timestamp[1]
          # for( i in 2:horizon)
          # {
          #   temp = temp + days(7)
          #   forecast_timestamp = c(forecast_timestamp,temp)
          # }
          # print(forecast_timestamp)
          # 
          # #Uncomment to Include Original Data
          # #forecast_timestamp <- c(week_timestamp$Timestamp,forecast_timestamp)
          # 
          # d<<-cbind(forecast_timestamp,d)
          layout(matrix(layout_array, nrow = length(severity), ncol = 1, byrow = TRUE))
          options(device="CairoWin")
          sapply(1:length(severity), function(i)
            plot(as.Date(final_vals[[my_i]][,1]),final_vals[[my_i]][,i+1],main=severity[i],xlab="Timestamps", ylab="Count of Tickets",type="l",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
            
          )
        },height = 1600, width = 900)
        output$winner <- renderTable({
          win_table<-list()
         for(i in 1:length(servicelist))
         # {
           win_table<-rbind(win_table,summary_algo[[i]])
         # str0 <- paste(strong(servicelist[i]))
         #  str <- paste(strong("Recommended Algorithm :"), summary_algo[[i]][1])
         #  str1 <- paste(strong("MASE:"), summary_algo[[i]][2])
         #  HTML(paste(str0,paste(str,str1,sep = '<br/><br/>'),sep = '<br/><br/>'))
         # }
         win_table<-as.data.frame(win_table)
         
         rownames(win_table)=c(servicelist)
         win_table<-cbind(rownames(win_table),win_table)
         colnames(win_table)=c("Service line","Recommended Algorithm","MASE")
          win_table
        },height = 1600, width = 1600)
      })
    }
    
    output$downloadData <- downloadHandler(
      filename = "forecast_val.csv",
      content = function(file="forecast_val.csv") {
        write.csv(download_data, file)
      }
    )
  })
}


#ggplot(d, aes(x=as.Date(forecast_timestamp), y=` Normal_Systems`, group=1)) + geom_line() + geom_point() + scale_x_date(date_labels="%d-%m-%y",date_breaks  ="1 month") +xlab("Timestamps") + ylab("Count of Tickets") + ggtitle(colnames(d)[i+1])+ theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggplot(d, aes(x=d[,1], y=d[,i+1], group=1)) + geom_line() + geom_point()  +xlab("Timestamps") + ylab("Count of Tickets") + ggtitle(severity[i]) + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#lines(vector_freq$Weeks,d[,i], type='l')
