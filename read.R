
WEEKS_TO_FORECAST = 12 
PERCENT = 30
#calls corresponding algo based on model code selected
model<-function(vector_freq,model_code)
{
  if(model_code=="ARIMA"){
    forecast_values<-ARIMA(vector_freq)
  }
  else if(model_code=="HW"){
    forecast_values<-HWplot(vector_freq)
  }
  else if(model_code=="Croston"){
    forecast_values<-Croston(vector_freq)
  }
  else if(model_code=="MA"){
    forecast_values<-MA(vector_freq) 
  }
  return(forecast_values)
}

#ARIMA algorithm
ARIMA<-function(vector_freq,horizon = WEEKS_TO_FORECAST){
  #TODO Write ARIMA function
  col_len <- dim(vector_freq)[2]
  col_names<- c('Weeks')
  forecast_val <- c()
  max_week<-as.numeric(vector_freq$Weeks[dim(vector_freq)[1]])
  for(i in 3:col_len){  
    r1<-cbind(vector_freq[,i])
    #r1 = tsclean(r1)
    count_r1<-ts(r1[,1],frequency=12)
    #decomp_data = stl(count_r1, s.window="periodic")
    #deseasonal_cnt <- seasadj(decomp_data)
    fit<-auto.arima(count_r1)
    fcast <- forecast(fit, h=horizon)

    forecast_val <- cbind(forecast_val,fcast$mean)
    col_names<- c(col_names,colnames(vector_freq)[i])
  }
  forecast_val <- data.table(forecast_val)
  forecast_val = cbind(c((max_week+1):(max_week+horizon)),forecast_val)
  colnames(forecast_val) = col_names
  return (forecast_val)
}

#Holt Winters algorithm
HWplot<- function(vector_freq,horizon = WEEKS_TO_FORECAST){
  col_len <- dim(vector_freq)[2]
  col_names<- c('Weeks')
  forecast_val <- c()
  max_week<-as.numeric(vector_freq$Weeks[dim(vector_freq)[1]])
  for(i in 3:col_len){  
    r1<-cbind(vector_freq[,i])
    r1 <- ts(r1,frequency=12)
    tickforecast=HoltWinters(r1)#,gamma=TRUE,l.start=5,b.start=1)
    #plot(tickforecast)
    crossvalHW<- forecast(tickforecast, h=horizon)
    #fit_no_holdout = HoltWinters(r1[-c(40:52)],gamma=FALSE,l.start=5,b.start=1)
    #crossvalHW<-forecast.HoltWinters(fit_no_holdout, h=12)
    forecast_val <- cbind(forecast_val,crossvalHW$mean)
    col_names<- c(col_names,colnames(vector_freq)[i])
  }
  forecast_val <- data.table(forecast_val)
  forecast_val = cbind(c((max_week+1):(max_week+horizon)),forecast_val)
  colnames(forecast_val) = col_names
  return (forecast_val)
  #plot.forecast(tickfor2)
  #acf(HWplot("tick3.csv")$residuals, lag.max=12)
}

#Crostons algorithm
Croston<-function(vector_freq,horizon = WEEKS_TO_FORECAST){
  col_len <- dim(vector_freq)[2]
  forecast_val <- c()
  col_names<- c('Weeks')
  max_week<-as.numeric(vector_freq$Weeks[dim(vector_freq)[1]])
  for(i in 3:col_len){  
    r2<-cbind(vector_freq[,i])
    crossvalCro <- croston(r2,h=horizon)
    forecast_val <- cbind(forecast_val,crossvalCro$mean)
    col_names<- c(col_names,colnames(vector_freq)[i])
  }
  forecast_val <- data.table(forecast_val)
  forecast_val = cbind(c((max_week+1):(max_week+horizon)),forecast_val)
  colnames(forecast_val) = col_names
  return(forecast_val)
}

#Moving average algorithm
MA<-function(vector_freq,horizon = WEEKS_TO_FORECAST){
  col_len <- dim(vector_freq)[2]
  forecast_val <- c()
  col_names<- c('Weeks')
  max_week<-as.numeric(vector_freq$Weeks[dim(vector_freq)[1]])
  for(i in 3:col_len){  
    r3<-cbind(vector_freq[,i])
    #r3 <- ts(r3,frquency = 12)
    SMAdf<-SMA(r3, 12)
    crossvalMA <- forecast(SMAdf,h=horizon)
    forecast_val <- cbind(forecast_val,crossvalMA$mean)
    col_names<- c(col_names,colnames(vector_freq)[i])
  }
  forecast_val <- data.table(forecast_val)
  forecast_val = cbind(c((max_week+1):(max_week+horizon)),forecast_val)
  colnames(forecast_val) = col_names
  return(forecast_val)
}

#RMSE for ARIMA
RMSE_ARIMA<-function(vector_freq,percent = PERCENT){
  col_len <- dim(vector_freq)[2]
  row_len <- dim(vector_freq)[1]
  withhold_length = ceiling((percent/100)*row_len)
  low_withhold = row_len - withhold_length
  high_withhold = row_len
  rmse_val_col <- c()
  col_names<-c()
  for(i in 3:col_len){  
    actual<-cbind(cbind(vector_freq[,i])[low_withhold+1:high_withhold])
    actual <- cbind(actual[1:withhold_length])
    r3 <- cbind(cbind(vector_freq[,i])[1:low_withhold])
    r3<-ts(r3[,1],frequency=12)
    
    fit<-auto.arima(r3)
    ARIMA_vals_withhold <- forecast(fit, h=withhold_length)
    #ARIMA_vals <- unclass(ARIMA_vals_withhold$mean)
    
    #MSE_val = mean((ARIMA_vals - actual) ^ 2, na.rm = TRUE)
    #RMSE_val = sqrt(MSE_val)
    #rmse_val_col <- cbind(rmse_val_col,RMSE_val)
    value<-accuracy(ARIMA_vals_withhold,actual)
    val<-t(value)
    val<-as.data.frame(val)
    col<-do.call(cbind,list(val$`Test set`))
    rmse_val_col <-cbind(rmse_val_col,col)
    col_names<- c(col_names,colnames(vector_freq)[i])
    #MSE_val = mean((HW_vals - actual) ^ 2, na.rm = TRUE)
    #RMSE_val = sqrt(MSE_val)
    #rmse_val_col <- cbind(rmse_val_col,RMSE_val)
  }
  #total_rmse = mean(rmse_val_col)
  colnames(rmse_val_col) = col_names
  rownames(rmse_val_col)=rownames(val)
  return(rmse_val_col)
  #total_rmse = mean(rmse_val_col)
  #return(total_rmse)
}

#RMSE for Holt Winters
RMSE_HW<-function(vector_freq,percent = PERCENT){
  col_len <- dim(vector_freq)[2]
  row_len <-dim(vector_freq)[1]
  withhold_length = ceiling((percent/100)*row_len)
  low_withhold = row_len - withhold_length
  high_withhold = row_len
  rmse_val_col <- c()
  col_names<-c()
  for(i in 3:col_len){  
    actual<-cbind(cbind(vector_freq[,i])[low_withhold+1:high_withhold])
    actual <- cbind(actual[1:withhold_length])
    r3 <- cbind(cbind(vector_freq[,i])[1:low_withhold])
    r3 <- ts(r3,frequency=12)
    
    tickforecast=HoltWinters(r3)
    HW_val_withhold<- forecast(tickforecast, h=withhold_length)
    #HW_vals <- unclass(HW_val_withhold$mean)
    value<-accuracy(HW_val_withhold,actual)
    val<-t(value)
    val<-as.data.frame(val)
    col<-do.call(cbind,list(val$`Test set`))
    rmse_val_col <-cbind(rmse_val_col,col)
    col_names<- c(col_names,colnames(vector_freq)[i])
    #MSE_val = mean((HW_vals - actual) ^ 2, na.rm = TRUE)
    #RMSE_val = sqrt(MSE_val)
    #rmse_val_col <- cbind(rmse_val_col,RMSE_val)
  }
  #total_rmse = mean(rmse_val_col)
  colnames(rmse_val_col) = col_names
  rownames(rmse_val_col)=rownames(val)
  return(rmse_val_col)
  #return(total_rmse)
}

#RMSE for Crostons
RMSE_Croston<-function(vector_freq,percent = PERCENT){
  col_len <- dim(vector_freq)[2]
  row_len <- dim(vector_freq)[1]
  withhold_length = ceiling((percent/100)*row_len)
  low_withhold = row_len - withhold_length
  high_withhold = row_len
  rmse_val_col <- c()
  col_names<-c()
  for(i in 3:col_len){  
    actual<-cbind(cbind(vector_freq[,i])[low_withhold+1:high_withhold])
    actual <- cbind(actual[1:withhold_length])
    r3 <- cbind(cbind(vector_freq[,i])[1:low_withhold])
    
    croston_withhold<-croston(r3,h=withhold_length)
    #croston_vals <- unclass(croston_withhold$mean)
    value<-accuracy(croston_withhold,actual)
    val<-t(value)
    val<-as.data.frame(val)
    col<-do.call(cbind,list(val$`Test set`))
    rmse_val_col <-cbind(rmse_val_col,col)
    col_names<- c(col_names,colnames(vector_freq)[i])
    #MSE_val = mean((croston_vals - actual) ^ 2, na.rm = TRUE)
    #RMSE_val = sqrt(MSE_val)
   
  }
  #total_rmse = mean(rmse_val_col)
  colnames(rmse_val_col) = col_names
  rownames(rmse_val_col)=rownames(val)
  return(rmse_val_col)
}

#RMSE for moving average
RMSE_MA<-function(vector_freq,percent = PERCENT){
  col_len <- dim(vector_freq)[2]
  row_len <- dim(vector_freq)[1]
  withhold_length = ceiling((percent/100)*row_len)
  low_withhold = row_len - withhold_length
  high_withhold = row_len
  rmse_val_col<-c()
  col_names<-c()
  for(i in 3:col_len){  
    actual<-cbind(cbind(vector_freq[,i])[low_withhold+1:high_withhold])
    actual <- cbind(actual[1:withhold_length])
    r3 <- cbind(cbind(vector_freq[,i])[1:low_withhold])
    SMAdf_withhold<-SMA(r3, 12)
    SMAdf_withhold[is.na(SMAdf_withhold)]<-0
    MA_withhold <- forecast(SMAdf_withhold,withhold_length)
    #MA_vals <- unclass(MA_withhold$mean)
    #MA_vals<-cbind(MA_vals)
    #MAE_val=mean(MA_vals - actual)
    #MSE_val = mean((MA_vals - actual) ^ 2)
    #RMSE_val = sqrt(MSE_val)
    value<-accuracy(MA_withhold,actual)
    val<-t(value)
    val<-as.data.frame(val)
    col<-do.call(cbind,list(val$`Test set`))
    rmse_val_col <-cbind(rmse_val_col,col)
    #rmse_val_col <- list.cbind(rmse_val_col,RMSE_)
    #mse_val_col<-cbind(mse_val_col,MSE_val)
    #mape_val_col<-cbind(mse_val_col,MSE_val)
    col_names<- c(col_names,colnames(vector_freq)[i])
  }
  colnames(rmse_val_col) = col_names
  rownames(rmse_val_col)=rownames(val)
  return(rmse_val_col)
  
}

#recommend function calls the algorithm with minimum RMSE
recommend<-function(vector_freq,percent = PERCENT)
{
  compare<-c(RMSE_ARIMA(vector_freq,percent),RMSE_HW(vector_freq,percent),RMSE_Croston(vector_freq,percent),RMSE_MA(vector_freq,percent))
  select<-min(compare)
  index<-match(select,compare)
  print(index)
  if(index==1){
    forecast_values<-ARIMA(vector_freq)
  }
  else if(index==2){
    forecast_values<-HWplot(vector_freq)
  }
  else if(index==3){
    forecast_values<-Croston(vector_freq)
  }
  else if(index==4){
    forecast_values<-MA(vector_freq) 
  }
  return(forecast_values)
}
#arg:service option, forecast values of four algos
#return value:error matrix for each service line, severity
getError<-function(service,ARIMA_mat,HW_mat,Croston_mat,MA_mat)
{
  #service="Systems"
  final_error_mat<-list()
  row_names=c("ARIMA","HW","Croston","MA")
  err_mat_service<-call(service,ARIMA_mat)
  col_names=colnames(err_mat_service)
  req_Error<-rbind(err_mat_service[[1]][5,],err_mat_service[[1]][6,])
  col<-c()
  col<-do.call(cbind,list(req_Error))
  col<-t(col)
  final_error_mat[[1]]<-col
  err_mat_service<-call(service,HW_mat)
  # mean_Error<-rowMeans(err_mat_service[[1]])
  req_Error<-rbind(err_mat_service[[1]][5,],err_mat_service[[1]][6,])
  col<-list()
  col<-do.call(cbind,list(req_Error))
  col<-t(col)
  final_error_mat[[2]]<-col
  err_mat_service<-call(service,Croston_mat)
  # mean_Error<-rowMeans(err_mat_service[[1]])
  req_Error<-rbind(err_mat_service[[1]][5,],err_mat_service[[1]][6,])
  col<-c()
  col<-do.call(cbind,list(req_Error))
  col<-t(col)
  final_error_mat[[3]]<-col
  err_mat_service<-call(service,MA_mat)
  # mean_Error<-rowMeans(err_mat_service[[1]])
  req_Error<-rbind(err_mat_service[[1]][5,],err_mat_service[[1]][6,])
  col<-c()
  col<-do.call(cbind,list(req_Error))
  #final_error_mat[1]=col1
  col<-t(col)
  final_error_mat[[4]]<-col
  
  
  return(final_error_mat)
}

#For a particular service, gives forecast vector for all severities
call=function(service,result){
  result<-as.data.frame(result)
  indx <- gsub(".*_", "", colnames(result))
  indx1 <- factor(indx, levels=unique(indx))
  b<-lapply(split(colnames(result),indx1 ), function(x) result[x])
  return(b[service])
}

#For a particular severity, gives forecast vector for all services
call2=function(severity,res){
  res<-as.data.frame(res)
  index<-gsub("_.*","", colnames(res))
  indx2<- factor(index, levels=unique(index))
  b<-lapply(split(colnames(res),indx2 ), function(x) res[x])
  return(b[severity])
}


count_tickets<-function(x){
  t <- count((x$Week))
  data_t <- data.table(t)
  return (t)
}


#To find trend of the given data
find_trend<-function(new_data){
  t<-count(format(new_data$Week))
  count_ts = ts(t[, c('freq')])
  #count_ts=count_ts[1:52]
  #Remove Outliers
  t$clean_cnt = tsclean(count_ts)
  
  ggplot(data = t, aes(x = x, y = clean_cnt)) +
    geom_line(data = t, aes(x = x, y = clean_cnt,group=1)) + ylab('Cleaned ticket Count')
  
  #t$cnt_ma = ma(t$clean_cnt, order=4)
  
  #ggplot(data = t, aes(x = Date, y = clean_cnt, colour = "Counts")) +
   # geom_line(data = t, aes(x = Date, y = clean_cnt, colour = "Counts",group=1)) +
  #  geom_line(data = t, aes(x = Date, y = cnt_ma, colour = "Monthly Moving Average",group=1))  +
   # ylab('Ticket Count')
  
  #count_ma = ts(na.omit(t$cnt_ma), frequency=4)
  count_clean_ts<-ts(t$clean_cnt,frequency=4)
  decomp_data = stl(count_clean_ts, s.window="periodic")
  #deseasonal_cnt <- seasadj(decomp_data)
  #plot(decomp_data$time.series[,2],main="Distribution of Count of Tickets over Months",xlab='Months',ylab='Trend')
  return(decomp_data)
}


modulrise_data<-function(new_data){
  #Get unique elements in ServiceLine and Severity
  set_serviceLine <- unique(new_data$ServiceLine)
  set_severity <- unique(new_data$Severity)
  
  set_severity<-sort(set_severity, decreasing = TRUE) #Crirtical to Minor
  
  #Store frequency values of each week for every subset
  ticket_freq <- data.table(cbind(count((new_data$Week))))
  vector_freq <- cbind(count((new_data$Week)))
  col_names <- c("Weeks","Total Ticket")
  for(x in set_serviceLine){
    for(y in set_severity){
      #Create the required subset to count tickets
      temp<-subset(new_data,subset=(ServiceLine==x & Severity==y))
      
      count_freq <- count_tickets(temp)
      count_freq<-data.table(count_freq)
      #Some weeks have 0 tickets hence missing, add those
      count_freq<-count_freq[ticket_freq,on=c("x")]
      count_freq<-count_freq[,i.freq:=NULL]
      count_freq[is.na(count_freq)]<-0
      
      #Append it to a table
      vector_freq <- cbind(vector_freq,count_freq$freq)
      y <- unlist(strsplit(y,'-'))[2]
      col_names <- c(col_names,paste(y,x,sep="_"))
      #(Week, Total ticket count, sevL[1] and sevr[1] tick_count, sevL[1] and sevr[2] tick_count ...)
    }
  }
  colnames(vector_freq) = col_names
  return(vector_freq)
}  

read_file2<-function(inp_path, inp_name){
  #Check if file is csv or xlsx, read accordingly
  file_format = unlist(strsplit(inp_name,'\\.'))[2]
  
  if(file_format == "csv"){  
    new_data = read.csv(inp_path)
  } else if(file_format == "xlsx" | file_format == "xls"){
    new_data <- read_excel(paste(inp_path, ".xlsx", sep=""))
  } else{stop("Unknown Format")}
  
  #TOTAL_TICKETS = dim(new_data)[1]
  #MISSING_TIMESTAMPS = sum(is.na(new_data$Timestamp))
  #new_data = na.omit(new_data)
  #Convert timestamp of the form d/m/y to weeks, have to chnage for other formats
  #new_data$Week <- as.factor(ceiling(difftime(new_data$Timestamp, min(new_data$Timestamp), units="weeks")))  
  return(new_data)
}

read_file1<-function(inp_path, inp_name){
  #Check if file is csv or xlsx, read accordingly
  file_format = unlist(strsplit(inp_name,'\\.'))[2]
  
  if(file_format == "csv"){  
    new_data = read.csv(inp_path)
  } else if(file_format == "xlsx" | file_format == "xls"){
    new_data <- read_excel(paste(inp_path, ".xlsx", sep=""))
  } else{stop("Unknown Format")}
  
  #TOTAL_TICKETS = dim(new_data)[1]
  #MISSING_TIMESTAMPS = sum(is.na(new_data$Timestamp))
  new_data = na.omit(new_data)
  #Convert timestamp of the form d/m/y to weeks, have to chnage for other formats
  new_data$Week <- as.factor(ceiling(difftime(new_data$Timestamp, min(new_data$Timestamp), units="weeks")))  
  return(new_data)
}

read_file<-function(inp_path, inp_name){
  #Check if file is csv or xlsx, read accordingly
  file_format = unlist(strsplit(inp_name,'\\.'))[2]
  
  if(file_format == "csv"){  
    new_data = read.csv(inp_path)
  } else if(file_format == "xlsx" | file_format == "xls"){
    file.rename(inp_path,paste(inp_path, ".xlsx", sep=""))
    new_data <- read_excel(paste(inp_path, ".xlsx", sep=""))
  } else{stop("Unknown Format")}
  
  TOTAL_TICKETS = dim(new_data)[1]
  MISSING_TIMESTAMPS = sum(is.na(new_data$Timestamp))
  new_data = na.omit(new_data)
  #Convert timestamp of the form d/m/y to weeks, have to chnage for other formats
  new_data$Week <- as.factor(ceiling(difftime(new_data$Timestamp, min(new_data$Timestamp), units="weeks")))  
  return(new_data)
}


calculateForecastVal<-function(method,vector_freq,horizon)
{
  if(method=="Croston"){
    forecast_val<<-Croston(vector_freq,horizon)
  }else if(method=="Simple Moving Average"){
    forecast_val<<-MA(vector_freq,horizon)
  }else if(method=="Holt Winter"){
    forecast_val<<-HWplot(vector_freq,horizon)
  }else if(method=="ARIMA"){
    forecast_val<<-ARIMA(vector_freq,horizon)
  }
  return(forecast_val)
}
  
plotForecastData<-function(service_method,vector_freq,forecast_val,horizon)
{
  #orig_fore<-rbind(vector_freq,forecast_val)
  
  #Uncomment to Include Original Data
  #forecast_val <- orig_fore
  
  final_val<<-call(service_method,forecast_val)
  #print(head(d))
  #col_names_final_val = colnames(final_val$service_method)
  #print()
  final_val<<-na.omit(final_val)
  final_val<<-as.data.frame(final_val[[1]])
  #print(head(final_val))
  layout_array = c()
  for(i in 1:length(severity)){
    layout_array = c(layout_array,i)
  }
  
  forecast_timestamp = c(as.Date(max(week_timestamp$Timestamp)+days(7)))
  temp = forecast_timestamp[1]
  for( i in 2:horizon)
  {
    temp = temp + days(7)
    forecast_timestamp = c(forecast_timestamp,temp)
  }
  #print(forecast_timestamp)
  
  #Uncomment to Include Original Data
  #forecast_timestamp <- c(week_timestamp$Timestamp,forecast_timestamp)
  
  final_val<<-cbind(forecast_timestamp,final_val)
  return(final_val)
}

main<-function()
{
  file <- readline(prompt="Enter file name: ")
  model_code<- readline(prompt="Enter model name:")
  
  new_data<-read_file(file) #Read file whether csv or xlsx
  decomp_data<-find_trend(new_data) #Plot Distribution of Data
  vector_freq<-modulrise_data(new_data) #Partition to required counts for plotting
  #forecast_values <- model(vector_freq,model_code) #Forecast based on chosen model
  #plot_data(forecast_values) #Plot
  return(vector_freq)
}  

