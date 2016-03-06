library(shiny)
library(dplyr)
library(tidyr)
library(xts)
library(dygraphs)
library(DT)

master.data <- read.csv('master-data.csv', header = T)

#format the date correctly
master.data$date <- as.Date(master.data$date, format = '%Y-%m-%d')

#remove commmas from the columns that are now read as factors

master.data$trades.vol <- gsub(',', '', master.data$trades.vol)
master.data$trades.vol <- as.numeric(master.data$trades.vol)

master.data$trades.num <- as.numeric(master.data$trades.num)



function(input, output) {
  
    output$avgPrice.graph <- renderDygraph({
      
      price.filtered <- master.data %>% select(date, coupon, avg.price, asset.subcl, mortgage.family) %>% filter(date >= input$dates[1], date <= input$dates[2]) %>% filter(asset.subcl == input$classes, mortgage.family == input$family, coupon %in% input$coupon, avg.price > 0) %>% spread(coupon, avg.price) %>% xts(x =  .[, (ncol(.)-length(input$coupon)+1):ncol(.)], order.by = .$date)
      
      if (input$smoother == FALSE) {
      
      dygraph(price.filtered) %>% dyRangeSelector(height = 35) %>% dyAxis('y', 'Price in $')
      
      }
      
      else {
      
        prices.ma <- data.frame()
        
        for (i in (ncol(price.filtered)-length(input$coupon)+1):ncol(price.filtered)) {
          
          if (ncol(price.filtered) == 1) {
          
          #remove incomplete cases - requirement for the SMA function
          price.filtered <- price.filtered[complete.cases(price.filtered),]
          prices.ma <- SMA(price.filtered[,1], n = input$smootherScale)
          colnames(prices.ma) <- trimws(colnames(price.filtered)[,1])
          }
          
          else {
          price.filtered <- price.filtered[complete.cases(price.filtered),]
          ma.interm <- SMA(price.filtered[,i], n = input$smootherScale)
          colnames(ma.interm) <- regmatches(as.character(colnames(price.filtered[,i])),gregexpr("\\d+\\.*\\d*", as.character(colnames(price.filtered[,i]))))
          prices.ma <- cbind(prices.ma, ma.interm)
          }
        }
      
      dygraph(prices.ma) %>% dyRangeSelector(height = 35) %>% dyAxis('y', 'Moving average price in $')
        
        
      }
      
    })
    
    output$wgtPrice.graph <- renderDygraph({
      
      wgtprice.filtered <- master.data %>% select(date, coupon, wgtavg.price, asset.subcl, mortgage.family) %>% filter(date >= input$dates[1], date <= input$dates[2]) %>% filter(asset.subcl == input$classes, mortgage.family == input$family, coupon %in% input$coupon, wgtavg.price > 0) %>% spread(coupon, wgtavg.price) %>% xts(x =  .[, (ncol(.)-length(input$coupon)+1):ncol(.)], order.by = .$date)
      
      if (input$smoother == FALSE) {
        
        dygraph(wgtprice.filtered) %>% dyRangeSelector(height = 35) %>% dyAxis('y', 'Weighted Average Price in $')
        
      }
      
      else {
        
        wgtprices.ma <- data.frame()
        
        for (i in (ncol(wgtprice.filtered)-length(input$coupon)+1):ncol(wgtprice.filtered)) {
          
          if (ncol(wgtprice.filtered) == 1) {
            
            #remove incomplete cases - requirement for the SMA function
            wgtprice.filtered <- wgtprice.filtered[complete.cases(wgtprice.filtered),]
            wgtprices.ma <- SMA(wgtprice.filtered[,1], n = input$smootherScale)
            colnames(wgtprices.ma) <- trimws(colnames(wgtprice.filtered)[,1])
          }
          
          else {
            wgtprice.filtered <- wgtprice.filtered[complete.cases(wgtprice.filtered),]
            ma.interm <- SMA(wgtprice.filtered[,i], n = input$smootherScale)
            colnames(ma.interm) <- regmatches(as.character(colnames(wgtprice.filtered[,i])),gregexpr("\\d+\\.*\\d*", as.character(colnames(wgtprice.filtered[,i]))))
            wgtprices.ma <- cbind(wgtprices.ma, ma.interm)
          }
        }
        
        dygraph(wgtprices.ma) %>% dyRangeSelector(height = 35) %>% dyAxis('y', 'Weighted moving average price in $')
        
        
      }
      
    })
    
    output$tradeVol.graph <- renderDygraph({
      
      tradeVol.filtered <- master.data %>% select(date, coupon, trades.vol, asset.subcl, mortgage.family) %>% filter(date >= input$dates[1], date <= input$dates[2]) %>% filter(asset.subcl == input$classes, mortgage.family == input$family, coupon %in% input$coupon, trades.vol > 0) %>% spread(coupon, trades.vol) %>% xts(x =  .[, (ncol(.)-length(input$coupon)+1):ncol(.)], order.by = .$date)
      
      if (input$smoother == FALSE) {
        
        dygraph(tradeVol.filtered) %>% dyRangeSelector(height = 35) %>% dyAxis('y', 'Trade volume in $', axisLabelFontSize = 10)
        
      }
      
      else {
        
        tradeVol.ma <- data.frame()
        
        for (i in (ncol(tradeVol.filtered)-length(input$coupon)+1):ncol(tradeVol.filtered)) {
          
          if (ncol(tradeVol.filtered) == 1) {
            
            #remove incomplete cases - requirement for the SMA function
            tradeVol.filtered <- tradeVol.filtered[complete.cases(tradeVol.filtered),]
            tradeVol.ma <- SMA(tradeVol.filtered[,1], n = input$smootherScale)
            colnames(tradeVol.ma) <- trimws(colnames(tradeVol.filtered)[,1])
          }
          
          else {
            tradeVol.filtered <- tradeVol.filtered[complete.cases(tradeVol.filtered),]
            ma.interm <- SMA(tradeVol.filtered[,i], n = input$smootherScale)
            colnames(ma.interm) <- regmatches(as.character(colnames(tradeVol.filtered[,i])),gregexpr("\\d+\\.*\\d*", as.character(colnames(tradeVol.filtered[,i]))))
            tradeVol.ma <- cbind(tradeVol.ma, ma.interm)
          }
        }
        
        dygraph(tradeVol.ma) %>% dyRangeSelector(height = 35) %>% dyAxis('y', 'Moving average of trade volume in $', axisLabelFontSize = 10)
        
        
      }
      
    })
    
    output$tradeNum.graph <- renderDygraph({
      
      tradeNum.filtered <- master.data %>% select(date, coupon, trades.num, asset.subcl, mortgage.family) %>% filter(date >= input$dates[1], date <= input$dates[2]) %>% filter(asset.subcl == input$classes, mortgage.family == input$family, coupon %in% input$coupon, trades.num > 0) %>% spread(coupon, trades.num) %>% xts(x =  .[, (ncol(.)-length(input$coupon)+1):ncol(.)], order.by = .$date)
      
      if (input$smoother == FALSE) {
        
        dygraph(tradeNum.filtered) %>% dyRangeSelector(height = 35) %>% dyAxis('y', 'Number of trades')
        
      }
      
      else {
        
        tradeNum.ma <- data.frame()
        
        for (i in (ncol(tradeNum.filtered)-length(input$coupon)+1):ncol(tradeNum.filtered)) {
          
          if (ncol(tradeNum.filtered) == 1) {
            
            #remove incomplete cases - requirement for the SMA function
            tradeNum.filtered <- tradeNum.filtered[complete.cases(tradeNum.filtered),]
            tradeNum.ma <- SMA(tradeNum.filtered[,1], n = input$smootherScale)
            colnames(tradeNum.ma) <- trimws(colnames(tradeNum.filtered)[,1])
          }
          
          else {
            tradeNum.filtered <- tradeNum.filtered[complete.cases(tradeNum.filtered),]
            ma.interm <- SMA(tradeNum.filtered[,i], n = input$smootherScale)
            colnames(ma.interm) <- regmatches(as.character(colnames(tradeNum.filtered[,i])),gregexpr("\\d+\\.*\\d*", as.character(colnames(tradeNum.filtered[,i]))))
            tradeNum.ma <- cbind(tradeNum.ma, ma.interm)
          }
        }
        
        dygraph(tradeNum.ma) %>% dyRangeSelector(height = 35) %>% dyAxis('y', 'Moving Average of Trade Volume in $', axisLabelFontSize = 10)
        
        
      }
      
    })
    
  output$tabularView <- renderDataTable({
    
    dataTable.filtered <- master.data %>% select(2:15) %>% filter(date >= input$dates[1], date <= input$dates[2], asset.subcl == input$classes, mortgage.family == input$family, coupon %in% input$coupon) %>% select(1:12)
    datatable(dataTable.filtered, rownames = FALSE, colnames = c('Date', 'Coupon', 'Average Price', 'Weighted Average Price', 'Low Tail Price', '25th Percentile Price', '50th Percentile Price', '75th Percentile Price', 'High Tail Price', 'SD', 'Volume of Trades', 'Number of Trades'))
    
  })
  
}
