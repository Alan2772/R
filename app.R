library(shiny)
library(shinydashboard)
library(DT)
library(rpivotTable)
library(tidyverse)
library(magrittr)
library(broom)
options(shiny.maxRequestSize = 9*1024^2)
dataset = mtcars
# symbol_type = read.csv(".csv", stringsAsFactors = FALSE,na.strings = "")
# symbol_type %<>%  separate(col=symbol,into = c("symbol","suffix") ,sep="\\.") %>% select(-suffix) %>%
#                   separate(col=symbol,into = c("symbol","suffix") ,sep="\\-") %>% select(-suffix) 
# symbol_type$symbol = gsub("[[:punct:]]","", symbol_type$symbol)
# 
# symbol_type %<>% mutate(symbol = replace(symbol,symbol=="CL","CL-OIL"))

MLR_AIC = function(mydata,y,method){
  if(method == "stepwise" ){
    method = "leapSeq"
    AIC.direction = "both"
    
  } else if(method == "forward" ){
    method = "leapForward" 
    AIC.direction = "forward"
    
  } else {
    method = "leapBackward"
    AIC.direction = "backward"
  }
  
  formulas = paste0(y,"~.")
  reg_model = lm(formula(formulas) , data=mydata) 
  step_model = MASS::stepAIC(reg_model,direction =  AIC.direction)
  tidy_model = broom::tidy(step_model)
  return(list(tidy_model,step_model[["anova"]]))
  
}

#---- Shiny ui ----
sidebar = dashboardSidebar(
  width = 180,
  hr(),
  sidebarMenu(id="tabs",
              fileInput('upload_data','Upload CSV',
                        accept = c('text/csv')
                        ),
              hr(),
              menuItem("Analyse", tabName="analyse", icon=icon("line-chart"), selected=TRUE),
              hr()

  )
  
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = "analyse",
            fluidRow(
              column(width = 12,
                     tabBox( width = 12,
                             tabPanel(h5("Data"),
                                      dataTableOutput("rawdata")
                             ),
                             # tabPanel(h5("Pivot"),
                             #          rpivotTableOutput("pivottable",height="auto")
                             # ),
                             # 
                             # tabPanel(h5("Regression"),
                             #          column(width=6,dataTableOutput("stats1")),
                             #          column(width=6,dataTableOutput("stats2")),
                             #          dataTableOutput("stats3")
                             # ),
                             tabPanel(h5("Multiple Linear Regression"),
                                      box( width = NULL, collapsed = FALSE, collapsible = TRUE,title = "Setting", status = "primary", solidHeader = TRUE,
                                           column(width=6,selectInput(inputId = "dv", label = "Dependent Variable", choices = colnames(dataset),selected=colnames(dataset)[1])),
                                           column(width=6,selectInput(inputId = "MLR_method", label = "MLR method", choices = c("stepwise","forward","backward"),selected="stepwise"))
                                           ),
                                      hr(),
                                      verbatimTextOutput("result1",placeholder = TRUE),
                                      hr(),
                                      dataTableOutput("result2"),
                                      hr(),
                                      verbatimTextOutput("descriptive",placeholder = TRUE)
                             )
                     )
              )
            )
    )
  )
)

ui = dashboardPage(
  dashboardHeader(title = "Fucking Critical Analytical Tools",titleWidth = 400),
  sidebar,
  body
)

#---- Shiny Server ----
server = function(input,output,session){
  
  raw = reactiveValues(data = dataset)
  observeEvent(input$upload_data,{
    if(is.null(input$upload_data)){
      raw$data = mtcars
    } else {
      raw$data = read.csv(inFile,header=TRUE,sep=',',quote="\"")
      dataset = read.csv(inFile,header=TRUE,sep=',',quote="\"")
    }
  })
  

  dv = reactive(input$dv)
  method = reactive(input$MLR_method)

  MLR_model = reactive(MLR_AIC(raw$data,dv(),method()))
  #---- descriptive stat , with console text ----

  # output$MLR_result = renderPrint(MLR_AIC(raw$data,dv,"stepwise"))
  output$result1 = renderText({
    print(paste("Initial:",input$dv,"~"))
  })
  output$result2 = renderDataTable({
    datatable(data.frame(MLR_AIC(raw$data,dv(),method())[1]),options = list(paging=FALSE,searching=FALSE))
  })

  # return(list(tidy_model,step_model[["anova"]]))
  
  output$descriptive = renderPrint({
    MLR_AIC(raw$data,dv(),method())
  })

  # MLR_AIC(raw$data,dv,"both")
  # MLR_AIC(raw$data,dv,"both")

  #---- customer's data ----
  observeEvent(input$run,{raw_data = popu_raw(isolate(input$server),isolate(input$id))
  

  output$pivottable = renderRpivotTable({rpivotTable(data = raw_data %>% mutate(duration = close_time - open_time, profit_com_swap = profit + commission + swaps ))})
  
    ana_raw_data$data = {raw_data%>% 
        mutate(duration = ifelse((close_time - open_time)<0,"N/A",close_time-open_time))%>%
        select(server,ticket,login,symbol,cmd,volume,open_time,close_time,close_price,commission,swaps,margin_rate,profit,duration,comment)
    }
  
  }
  )
  
  #---- data table of raw trading data ----
  output$rawdata = renderDataTable({datatable({raw$data
    # %>% arrange(desc(close_time))
    # %>%     mutate(close_time= replace(close_time,TRUE,as.character(close_time)),
    #              open_time= replace(open_time,TRUE,as.character(open_time)))
    # print((k[,"close_time"]))
  }
  ,
  options = list(lengthMenu = list(c(20,25, 50, 100,200,-1), c('20','25', '50','100','200', 'All')),
                 pageLength = 20,
                 columnDefs = list(list(className = 'dt-center', targets = 0:ncol(raw$data))))
  )
  })
  
  
  output$NOP  = renderRpivotTable({rpivotTable(data = NOP_data$data)})
  
  
  #---- filtering NOP Data ----
  
  # observeEvent(input$filter_reset,{
  #   updateTextInput(session,inputId = "filter_book",value = "")
  #   updateTextInput(session,inputId = "filter_server",value = "")
  #   updateTextInput(session,inputId = "filter_login",value = "")
  #   updateTextInput(session,inputId = "filter_group",value = "")
  #   updateTextInput(session,inputId = "filter_symbol",value = "")
  #   updateTextInput(session,inputId = "filter_direction",value = "")
  #   updateTextInput(session,inputId = "filter_type",value = "")
  #   updateCheckboxInput(session,inputId = "server_exact",value = FALSE)
  #   updateCheckboxInput(session,inputId = "login_exact",value = FALSE)
  #   updateCheckboxInput(session,inputId = "group_exact",value = FALSE)
  # })
  
  # observeEvent(input$ana_filter_reset,{
  #   updateTextInput(session,inputId = "ana_filter_ticket",value = "")
  #   updateTextInput(session,inputId = "ana_filter_symbol",value = "")
  #   updateTextInput(session,inputId = "ana_filter_cmd",value = "0,1,6")
  # })

  # filter_to = reactive ({
  #   NOP_data$data %>% filter_apply(df=.,
  #                                  input$filter_book,
  #                                  input$filter_server,
  #                                  input$filter_login,
  #                                  input$filter_group,
  #                                  input$filter_symbol,
  #                                  input$filter_direction,
  #                                  input$filter_type,
  #                                  input$server_exact,
  #                                  input$login_exact,
  #                                  input$group_exact
  #   )
  # })
  
  # output$NOP_DT  = renderDataTable({datatable(filter_to()%>% select(book,server,login, group, symbol,direction,NOP,type),
  #                                             options = list(pageLength = 50,columnDefs = list(list(className = 'dt-center', targets = 0:7)))
  # )
  # })
  
  # output$NOP_method = renderImage({ 
  #   return(list( src = file.path("NOP_check.png"),contentType = "image/png"))
  # },
  # deleteFile = FALSE
  # )
}
shinyApp(ui,server)
# runApp(port=7210, host='192.168.8.5')