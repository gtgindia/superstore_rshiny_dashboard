#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

shinyServer(function(input, output,session) {
    
    output$contents <- DT::renderDataTable({
        
        req(input$file1)
        inFile <- input$file1
        ext <- tools::file_ext(inFile$name)
        switch(ext,
               xlsx = as.data.table(read_excel(inFile$datapath, 1,n_max = 100)),
               xls =as.data.table(read_excel(inFile$datapath, 1,n_max = 100)),
               sendSweetAlert(
                   session = session,
                   title = "Invalid File type",
                   text = "Please select xlsx/xls file",
                   type = "info"
               )
               
        )
        
    })
    
    observeEvent(input$switch_tab, {
        updateTabsetPanel(session, "inTabset",selected = "INPUT DATASET REVIEW")
    })
    observeEvent(input$switch_tab2, {
        updateTabsetPanel(session, "inTabset",selected = "K-MEANS CLUSTERING ANALYSIS")
    })
    
    
    output$start_year_filter <- renderUI({
        airMonthpickerInput(
            inputId = "start_month_year", label = "Select range of months:",
            range = TRUE,  update_on = "close",minDate = "2016-01-01", 
            maxDate = "2019-12-31",value = c("2016-01-01","2019-12-31"),
            addon = "none", width = "100%",clearButton = TRUE,
            toggleSelected = FALSE)
    })
    
    input_data <- function(){
        inFile <- input$file1
        df<- read_excel(inFile$datapath, 1)
        df <- as.data.frame(df)
        
        if (length(input$start_month_year) == 1) {
            sendSweetAlert(
                session = session, title = "Invalid Month Range", text = "Please select a valid range of Months.",
                type = "warning")
            return()
        }
        
        if (!is.null(input$start_month_year)) {
            
            df <- as.data.table(df)
            if (length(input$start_month_year) != 1)
            {df <- df[(`Order Date` %between% input$start_month_year)]}
            df <- as.data.frame(df)
            
        }
        
        if(!is.null(input$category) & !is.null(input$segment) ){
            sdf <- df %>% filter(Category %in% input$category & Segment %in% input$segment)
        } else if(!is.null(input$category) & is.null(input$segment) ){ 
            sdf <- df %>% filter( Category %in% input$category)  
        }else if(is.null(input$category) & !is.null(input$segment) ){
            sdf <- df %>% filter( Segment %in% input$segment)
        }else {
            sdf <- df
        }
        
        return(sdf)
    }
    
    output$plot <- renderPlot({
        sdf <- input_data()
        
        if(is.null(sdf)) {return()}
        
        sdf_date <- sdf %>% select(`Order Date`,Sales,Profit) %>% group_by(`Order Date`) %>% 
            summarise(sales = sum(Sales) , profit = sum(Profit))
        
        sdf_date$month <- as.Date(cut(sdf_date$`Order Date`,breaks = "month"))
        
        sdf_month <- sdf_date %>% group_by(month) %>% 
            summarise(sales_m = sum(sales) , profit_m = sum(profit))
        
        ggplot(data = sdf_month, aes(x = month, y =sales_m )) +
            geom_line() +
            labs(y = 'Monthly Sales',x = "" )  +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            scale_x_date(date_labels = "%b %Y", date_breaks = "3 month")
        
        
    })
    
    output$download <- downloadHandler(
        filename = function() { paste0("input_dataset",Sys.time(),".xlsx")},
        content = function(file) {
            
            sdf <- input_data()
            
            write_xlsx(sdf, path = file)}
    )
    
    
    clustering <- function(){
        sdf <- input_data()
        sdf_data <- sdf %>% select(Sales,Profit)
        sdf_data_scale <- scale(sdf_data)
        #sdf_data <- dist(sdf_data_scale)
        
        
        
        
        # fviz_nbclust(sdf_data_scale,kmeans,method = "wss")
        
        
        set.seed(123)
        k3 <- kmeans(sdf_data_scale,centers = input$nclus,nstart = 25)
        return(k3)
        
    }
    post_processing <- function(){
        sdf <- input_data()
        k <- clustering()
        sdf$cluster <- k$cluster
        
        cust <- sdf %>% select(`Customer Name`,cluster) %>% group_by(cluster) %>%
            summarise(n_cust = n_distinct(`Customer Name`))
        
        sales <- sdf %>% select(Sales,cluster) %>% group_by(cluster) %>%
            summarise(sales = sum(Sales))
        
        order <- sdf %>% select(`Order ID`,cluster) %>% group_by(cluster) %>% 
            summarise(n_order = n_distinct(`Order ID`))
        
        quantity <- sdf %>% select(Quantity,cluster) %>% group_by(cluster) %>% 
            summarise(quantity = sum(Quantity))
        
        
        temp <- cbind(cust,sales,order,quantity)
        temp <- temp[, !duplicated(colnames(temp))]
        temp$sales_per_cust <- sales$sales/cust$n_cust
        
        sol<- temp
        colnames(sol) <- c('Cluster','No of Customers','Total Sales','No of Orders','Total Quantity','Sales per Customer')
        
        return(sol)
        
    }
    
    output$summary <- renderTable({
        
        post_processing()
        
    })
    
    output$kmeans_plot <- renderPlot({
        
        
        sdf <- input_data()
        sdf_data <- sdf %>% select(Sales,Profit)
        sdf_data_scale <- scale(sdf_data)
        #sdf_data <- dist(sdf_data_scale)
        
        
        
        
        # fviz_nbclust(sdf_data_scale,kmeans,method = "wss")
        
        
        set.seed(123)
        k3 <- kmeans(sdf_data_scale,centers = input$nclus,nstart = 25)
        # row.names(sdf_data_scale) <- sdf$Category
        k3.clusters <- k3$cluster
        
        fviz_cluster(k3,sdf_data_scale, ggtheme = theme_classic())
        
        
    })
    
    output$text <- renderText({ 
        print(paste(paste0("Between Sum of Squares / Total Sum of Squares : ",
                           round(clustering()$betweenss / clustering()$totss *100,digits = 2),"%"),
                    paste0("Sizes of clusters : ",
                           list(clustering()$size)),sep = "\n"))
        
    })
    
    
})
