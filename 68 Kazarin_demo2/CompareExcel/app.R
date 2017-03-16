#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

eval(parse("funcs.R", encoding="UTF-8"))

#Определяем макет окна приложения
ui <- fluidPage(
  #Определяем название приложения
  titlePanel("Uploading Files"),
  #Определяем название и функции боковой панели управления
  sidebarLayout(
    sidebarPanel(
      #Добавляем виджет для загрузки исходной версии файла         
      fileInput('file1', 'Choose Old CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      #Добавляем виджет (элемент интерфейса приложения) для загрузки новой версии файла  
      fileInput('file2', 'Choose New CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      #Добавляем виджет для выбора ключа для сопроставления          
      selectInput ("Select", "Key", choices = "Pending upload"),
      #Добавляем виджет для выбора переменной для расчета отклонений  
      selectInput ("Select1", "Val", choices = "Pending upload"),
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


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

server <- function(input, output, session) {
  contentsrea<-reactive({
    inFile<-input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = TRUE, sep = ";")
  })
  output$contents <- renderTable({contentsrea()})
  observe({
    updateSelectInput(session, "Select", choices = names(contentsrea()))
    updateSelectInput(session, "Select1", choices = names(contentsrea()))
  })
  output$contents2 <- renderTable({
    inFile2<-input$file2
    if (is.null(inFile2))
      return(NULL)
    read.csv(inFile2$datapath, header = TRUE, sep = ";")
  })
  output$changes2 <- renderDataTable({
    cmpExcelCols(old_df=input$file1, new_df=input$file2, key_name=input$Select, val_name=input$Select1)
  })
}

# Run the application 
shinyApp(ui=ui, server=server)

