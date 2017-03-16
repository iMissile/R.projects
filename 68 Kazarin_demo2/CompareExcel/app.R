#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(magrittr)

# devtools::install_github("tidyverse/readxl") -- используем фичи dev версии
# в частности, чтение без расширения файла
# https://github.com/tidyverse/readxl/issues/85
# https://github.com/leeper/rio/issues/130

eval(parse("funcs.R", encoding="UTF-8"))
# source("funcs.R", encoding="UTF-8", local=TRUE)



#Определяем макет окна приложения
ui <- fluidPage(
  #Определяем название приложения
  titlePanel("Uploading Files"),
  #Определяем название и функции боковой панели управления
  sidebarLayout(
    sidebarPanel(
      #Добавляем виджет для загрузки исходной версии файла         
      fileInput('old_file', 'Choose Old Excel File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      #Добавляем виджет (элемент интерфейса приложения) для загрузки новой версии файла  
      fileInput('new_file', 'Choose New Excel File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      #Добавляем виджет для выбора ключа для сопроставления          
      selectInput ("key_name", "Key", choices = "Pending upload"),
      #Добавляем виджет для выбора переменной для расчета отклонений  
      selectInput ("val_name", "Val", choices = "Pending upload"),
      #Добавляем виджет запуска расчетов
      actionButton ("go", "Go")
    ),
    #Определяем название и функции главной панели      
    mainPanel(
      #Выводим исходную таблицу        
      tableOutput('contents'),
      #Выводим новую таблицу        
      tableOutput('contents2'),
      #Выводим расчеты        
      dataTableOutput("changes2")
    ))
)


server <- function(input, output, session) {
  
  old_df <- reactive({
    #browser()
    read_xlsx(req(input$old_file$datapath))
  })

  new_df <- reactive({
    #browser()
    read_xlsx(req(input$new_file$datapath))
  })

  observe({
    updateSelectInput(session, "key_name", choices=names(req(old_df())))
    updateSelectInput(session, "val_name", choices=names(req(old_df())))
  })

  output$contents <- renderTable({old_df()})
  output$contents2 <- renderTable({new_df()})
  
  
  output$changes2 <- renderDataTable({
    if(input$key_name != input$val_name){
      cmpExcelCols(old_df=old_df(), new_df=new_df(), key_name=input$key_name, val_name=input$val_name)
    }
  })
}

# Run the application 
shinyApp(ui=ui, server=server)

