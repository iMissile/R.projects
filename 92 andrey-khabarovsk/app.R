#  Рабочее место для прогноза продаж с учетом цены продажи и промо-акций 
#  необходимо добавить графики для отображения графиков фактических продаж, моделей прогноза с помощью или сделать доп страницы по группам товара navbarPage
#  в таблице динамика остатков необходимо добавить подсветку в зависимости от критичности остатков в днях продаж
#  в таблице настройки прогноза необходимо добавить возможность добавления данных о днях оборачиваемости, выбор модели прогноза
#  проработать возможность по номенклатурной фильтрации по магазинам, чтобы можно было учитывать разные магазины для разных позциий на данный момент для всех позиций фильтрация по магазинам одинаковая
#  в блоке модели добавить моделей с последующим отбором
#  добавить возможность применения и сохранения группировки номенклатуры на данные в таблицах товары в пути и остатки

library(dplyr)
library(reshape2)
library(rhandsontable)
library(readxl)
library(forecast)
library(smooth)




# setwd("./forecast") #  Устанавливаем директорию куда будут сохраняться все файлы и от куда они будут читаться

ui <- fluidPage(
  
  titlePanel("Прогноз продаж"),
  
  sidebarLayout(
    
    sidebarPanel( width = 2,

             dateRangeInput("dates", label = "Период", min = "2016-01-01", start = "2017-01-01", language = 'ru'), # создаём ввод периода для фильтрации отображения фактических продаж
                   
             actionButton("Save_Btn_Change", "Сохранить и применить изменения"), # Создаём кному для сохранения и применения изменений в данных для передачи их в процедуру прогноза.
             
             uiOutput("Item"), # Создаём список номенклатуры который будет создаваться на основе уникальных значений в колонке номенклатура из загруженных данных
  
             uiOutput("Store"), # Создаём список магазинов который будет создаваться на основе уникальных значений в колонке магазины из загруженных данных
             
             radioButtons("radio", ("Выбрать тип файла"), # Выбираем тип загружаемых файлов
                          choices = list("CSV" = 1, "Excel" = 2), selected = 1),
             fileInput("file", ("Загрузить файл"), buttonLabel = "Загрузите файл...", placeholder = "Загрузите файл...",multiple = TRUE), #Элемент для загрузки изначального файла по фактическим продажам
             
             actionButton("Save_Btn_Item_group", "Сохранить и применить группировку"), # Кнопка по применению и сохранению группировки номенклатуры

             fileInput("add_file", ("Добавить файл"), buttonLabel = "Найти файл...", placeholder = "Найти файл", multiple = TRUE), #Элемент для загрузки дополнительного файла с данными по фактическим продажам 

             radioButtons("radio_add_data", ("Выбрать тип файла"), # Элемент выбора типа файла для загрузки доп данных по продажам 
                          choices = list("CSV" = 1, "Excel" = 2), selected = 1),
             bookmarkButton(),
             
             fileInput("Stock", ("Загрузить данные по остаткам товара"), buttonLabel = "Найти файл...", placeholder = "Найти файл", multiple = TRUE), # Create Input file to load a stock's data 
             
             radioButtons("radio_stock_data", ("Выбрать тип файла"), # create radioButtons to chose type of file of stock data
                          choices = list("CSV" = 1, "Excel" = 2), selected = 1),
             
             
             fileInput("Inflow", ("Загрузить данные по поступлению товара"), buttonLabel = "Найти файл...", placeholder = "Найти файл", multiple = TRUE), # Create Input file to load an Inflow items data 
             
             radioButtons("radio_Inflow", ("Выбрать тип файла"), # create radioButtons to chose type of file of inflow data
                          choices = list("CSV" = 1, "Excel" = 2), selected = 1),
             
             
             bookmarkButton() # create a buttons to save a link of sate
             
             
                ),  

    
  mainPanel(

       
        verbatimTextOutput('selected'), 
        rHandsontableOutput("View_tbl"), #Таблица для отображения фактических данных по продажам
        actionButton("Forecast_Btn", "Сформировать прогноз"), # Кнопка для передачи данных для расчёта прогноза
        rHandsontableOutput("Forecast_tbl"), # Таблица для настройки данных для передачи их в расчёт прогноза
        rHandsontableOutput("Item_group"), # Таблица для настройки группировки номенклатуры
        rHandsontableOutput("Buffer_GroupItem_filter_tbl"), # Таблица для отображения промежуточных расчётов
        titlePanel("Поступления"),
        rHandsontableOutput("Inflow_tbl"), # create an  inflow stock dynamic table  
        titlePanel("Остатки"),
        rHandsontableOutput("Stock_tbl"), # create a stock  table
        titlePanel("Таблица динамика остатков"),
        rHandsontableOutput("Stock_dynamic_tbl"), # create a stock dynamic table  
        rHandsontableOutput("Buffer"),
        rHandsontableOutput("Restore_tbl"), # Таблица для отображения расчёта восстановления данных
        rHandsontableOutput("Merge_foreacast_and_rezult_forecast_tbl"), # create merged table to check merged total table
        rHandsontableOutput("Rezult_of_forecast")
        )

)
)

server <- function(input, output) {
  
  
  output$selected=renderPrint({
    cat('\nSelected Cell Value:',
        input$View_tbl_select$data[[
          input$View_tbl_select$select$r]][[input$View_tbl_select$select$c]]) })  # отображаем выделенный элемент на таблицах

  # Загружаем файл с данными по продажам, если файл уже существует, то читаем из файла
  dataInput <- reactive({
    
  
    if (file.exists("Data_total.csv")) # Проверяем существует ли ранее загруженный файл
      
      { inFile <- read.csv("Data_total.csv", sep=",", header = TRUE); 
        return(inFile)
      
      } # Если файл существует то читаем данный из него, необходимо добавить исключение по выводу сообщения если нет файла и он не загружен
    
    req(input$file)
    inFile <- input$file

    if (is.null(inFile)) 
      return(NULL)

    if (input$radio == 1)
      { 
      inFile <- read.csv(inFile$datapath, sep = ",");
        write.csv(inFile, file = "Data_total.csv", row.names = FALSE);
        return(inFile)
      }

    else
    { inFile <- read_excel(inFile$datapath);
       write.csv(inFile, file = "Data_total.csv", row.names = FALSE);
      return(inFile)
    }

  })
  
  
  # Процедура по добавлению данных к существующем
  
 dataInput_add_file <- reactive({
                                
                                 in_add_File <- input$add_file 
                                 if (is.null(in_add_File)) 
                                   return(NULL)
                                 
                                 if (input$radio_add_data == 1)
                                 {
                                   in_add_File <- read.csv(in_add_File$datapath, sep = ","); 
                                   return(in_add_File)
                                  }
                                 
                                 else
                                 {
                                   in_add_File <- read_excel(in_add_File$datapath); 
                                   return(in_add_File)
                                  }
                                 
                                })
  
 # create operators to load stock's data from file
 
 dataInput_Stock <- reactive({
   
   if (file.exists("Stock.xls")) # Проверяем существует ли ранее загруженный файл
     
   { inFile_Stock <- read_excel("Stock.xls")}; 
   return(inFile_Stock)
   
   
   req(input$Stock)
   inFile_Stock <- input$Stock
   
   if (is.null(inFile_Stock)) 
     return(NULL)

   if (input$radio_stock_data == 1)
   { inFile <- read.csv(inFile_Stock$datapath, sep = ",");
   return(inFile_Stock)
   }
   
   else
   { inFile_Stock <- read_excel(inFile_Stock$datapath);
   return(inFile_Stock)
   }
   
 })
 
 
 # create a Stock table
 
 output$Stock_tbl <- renderRHandsontable({
   
   rhandsontable (dataInput_Stock(), width = 1000, height = 300)%>%hot_cols (fixedColumnsLeft = 2)
   
   
 })
 
 
 # create a procedure to load an iflow's data
 
 dataInput_Inflow <- reactive({
   
   
   if (file.exists("Inflow.xls")) # Проверяем существует ли ранее загруженный файл, необходимо привести файлы к общему формату
     
   { 
   inFile_Inflow <- read_excel("Inflow.xls"); 
   inFile_Inflow$Date <- as.Date(inFile_Inflow$Date);
   inFile_Inflow <- left_join(Calendar, inFile_Inflow, by = "Date"); # Joint Calendar table to shown NA data #further
   inFile_Inflow <- melt(inFile_Inflow, id = c("GroupItem", "Date"));
   inFile_Inflow <- dcast(inFile_Inflow, ... ~ Date);
   inFile_Inflow  <- filter(inFile_Inflow , is.na(GroupItem)==FALSE);
   inFile_Inflow  <- data.frame(inFile_Inflow) # переводим в формат data.frame чтобы при отображениив в rhandsontable не было смещения 
   };
   return(inFile_Inflow)
   
   
   req(input$Inflow)
   inFile_Inflow <- input$Inflow
   
   if (is.null(inFile_Inflow)) 
     return(NULL)
   
   if (input$radio_Inflow == 1)
   { inFile_Inflow <- read.csv(inFile_Inflow$datapath, sep = ",", head = TRUE);
   return(inFile_Inflow)
   }
   
   else
   { inFile_Inflow <- read_excel(inFile_Inflow$datapath, col_names = TRUE, col_types = c("text", "date", "numeric", "numeric"));
   
   inFile_Inflow$Date <- as.Date(inFile_Inflow$Date);
   inFile_Inflow <- left_join(Calendar, inFile_Inflow, by = "Date"); # Joint Calendar table to shown NA data #further
   inFile_Inflow <- melt(inFile_Inflow, id = c("GroupItem", "Date"));
   inFile_Inflow <- dcast(inFile_Inflow, ... ~ Date);
   inFile_Inflow  <- filter(inFile_Inflow , is.na(Item)==FALSE);
   inFile_Inflow  <- data.frame(inFile_Inflow);
   return(inFile_Inflow)
   }
   
 })
  
   # create a procedure to show an inflow dynamics, need to make relation with Group_Item function 

   output$Inflow_tbl <- renderRHandsontable({
     
     rhandsontable (dataInput_Inflow (), width = 1000, height = 300)%>%hot_cols (fixedColumnsLeft = 2)
     
     
   })
   
  
  data_dynamic_Stock <- reactive({
    
  Inflow_data <- dataInput_Inflow ();
  colnames(Inflow_data)[colnames(Inflow_data)=="variable"] <- "Indicator";
  Inflow_data <- melt(Inflow_data, id=c("GroupItem", "Indicator"));
  Inflow_data <- dcast(Inflow_data, ...~Indicator);
  Inflow_data$variable <- gsub("[X]", "", Inflow_data$variable);
  Inflow_data$variable <- gsub("[.]", "/", Inflow_data$variable);
  Inflow_data$variable <- as.Date(Inflow_data$variable);
  colnames(Inflow_data)[colnames(Inflow_data)=="variable"] <- "Date";
  
  dynamic_Stock <- dataInput_Stock ();
  dynamic_Stock$Date <- as.Date(dynamic_Stock$Date);
  
  dynamic_Stock <- right_join(dynamic_Stock, Inflow_data, by = c ("Date", "GroupItem"));
  
  colnames(dynamic_Stock)[colnames(dynamic_Stock)=="Qty.y"] <- "Inflow_Qty";
  colnames(dynamic_Stock)[colnames(dynamic_Stock)=="Qty.x"] <- "Stock_Qty";
  
  Forecast_rezult <- hot_to_r(input$Rezult_of_forecast);
  Forecast_rezult$Date <- as.Date(Forecast_rezult$Date);
  
  dynamic_Stock <- left_join(dynamic_Stock, Forecast_rezult, by= c ("Date", "GroupItem"));
  colnames(dynamic_Stock)[colnames(dynamic_Stock)=="Qty"] <- "Forecast_Qty";
  dynamic_Stock <- filter(dynamic_Stock, Date>= as.Date(dataInput_Stock()$Date[1]));
  dynamic_Stock$Forecast_Stock_Qty <- c(0);
  dynamic_Stock$Stock_Qty[is.na(dynamic_Stock$Stock_Qty)] <- 0;
  dynamic_Stock$Inflow_Qty[is.na(dynamic_Stock$Inflow_Qty)] <- 0;
  dynamic_Stock$Forecast_Qty[is.na(dynamic_Stock$Forecast_Qty)] <- 0;

  q <- data.frame(unique(dynamic_Stock$GroupItem));
  dynamic_Stock_total <- data.frame();

  # расчитываем прогнозные остатки по позициям, прогнозный остаток на текущий день расчитываем как сегодняшний остаток + приход - прогноз продаж на завтра
  # отрицательные значения остатков убираем т.к. возникает эффект отложенного спроса
  # почему через if else расчет не работает выдаёт NA

  for (a in 1:nrow(q))
  {
    
    dynamic_Stock_unique <- filter(dynamic_Stock, GroupItem==q[a, "unique.dynamic_Stock.GroupItem."])
 
    dynamic_Stock_unique$Forecast_Stock_Qty[1] <- (dynamic_Stock_unique$Stock_Qty[1]+dynamic_Stock_unique$Inflow_Qty[1]-dynamic_Stock_unique$Forecast_Qty[2])

    for (t in 2:nrow(dynamic_Stock_unique))
      
    {
      dynamic_Stock_unique$Forecast_Stock_Qty[t]<- (dynamic_Stock_unique$Forecast_Stock_Qty[t-1]+dynamic_Stock_unique$Inflow_Qty[t]-dynamic_Stock_unique$Forecast_Qty[t+1]);
 
    }

    dynamic_Stock_unique$Forecast_Stock_Qty[dynamic_Stock_unique$Forecast_Stock_Qty <= 0] <- 0;
    
    dynamic_Stock_total <-rbind(dynamic_Stock_total,dynamic_Stock_unique);

    }

  return(dynamic_Stock_total)

  })
   
  # таблица по выводу данных по динамике остатков     
  output$Stock_dynamic_tbl <- renderRHandsontable({
    
    rhandsontable (data_dynamic_Stock(), width = 1000, height = 300)%>%hot_cols (fixedColumnsLeft = 2) 
    
  }) 
 
 data_total <- reactive({ ## реактивное выражение для проверки загруженного файла с доп данными по продажам

   if (is.null(dataInput_add_file()))
      {
        data_tbl_total <- dataInput(); 
        return(data_tbl_total)
      }
   
    else 
        { 
          data_tbl_add_file <- dataInput_add_file();
          data_tbl_total <- merge(dataInput(), data_tbl_add_file, by=c("Item", "Date", "Store", "Qty", "Turn_over_VAT", "Promo"), all=TRUE);
          write.csv(data_tbl_total, file = "Data_total.csv", row.names = FALSE);
          return(data_tbl_total)
        }
   
                       })
 
  #
  dataInput_with_group_Item <- reactive({

    buffer_tbl <- hot_to_r(input$Item_group);

    tbl_with_group_Item_final <- left_join(data_total(), buffer_tbl, by = "Item")

  })
  
  #создаём список номенклатуры 
  
  output$Item <- renderUI({checkboxGroupInput('Item_list', 'Номенклатура', selected = unique(na.omit(data_total()$Item)),  unique(na.omit(data_total()$Item)), 
                                               inline = FALSE) })
  #создаём список магазинов на основе загруженных данных
  
  output$Store <- renderUI({checkboxGroupInput('Store_list', 'Магазины', selected = unique(na.omit(data_total()$Store)), unique(na.omit(data_total()$Store)), 
                                                 inline = FALSE)})
  #Выводим данные по продажам
  
  output$View_tbl <- renderRHandsontable( {
    
                    rhandsontable(filter_table_function_v2(dataInput_with_group_Item(), input$Store_list, input$Item_list, input$dates[1], input$dates[2]), selectCallback = TRUE, width = 1000, height = 300)%>%hot_cols (fixedColumnsLeft = 4)})
 
  # Выводим данные по сгруппированной номенклатуре
  
  output$Item_group <- renderRHandsontable( {
    
    rhandsontable(Group_Item_fn(data_total()))
  }) 
  
  # Создаём событие для применения и сохранения группировки номеклатуры
  
  observeEvent(input$Save_Btn_Item_group,
                 write.csv(hot_to_r(input$Item_group), file = "Item_Group.csv", row.names = FALSE)
                )

  # выводим таблицу для отображения сохранённых изменений сделанных в таблице продажи

    
    
  observeEvent(input$Save_Btn_Change, # создаём событие для применения и сохранения изменений из таблицы с данными по продажам 
                                      # в таблице по продажам мы вводим информацию по промо
              {
                    write.csv(hot_to_r(input$View_tbl), file = "Buffer_tbl.csv", row.names = FALSE);# сохраняем данные в файле 
                    x <- data.frame (read.csv("Buffer_tbl.csv", sep=",", header = TRUE)); 
                    y <- data.frame(hot_to_r(input$View_tbl)); 
                    z <- merge(x,y, by= c("Date_end", "Date_start", "GroupItem",  "h_forecast", "Indicator"), all.y= TRUE); # объединяем с текущий файл с новыми данными в таблице по продажам 
                    output$Buffer <- renderRHandsontable( { 
                      rhandsontable(z, width = 1000, height = 300)%>%hot_cols (fixedColumnsLeft = 4)})
              }

    )
                 

   # Создаём таблицу для отображения процесса восстановления данных
   
   output$Restore_tbl <- renderRHandsontable({

     z <- read.csv("Buffer_tbl.csv", sep=",", header = TRUE);
     z <- melt(z, id=c("Date_end", "Date_start", "GroupItem", "h_forecast", "Indicator"));
     z <- dcast(z, ...~Indicator);
     q <- data.frame(unique(z$GroupItem));
     total <- data.frame();
     

     for (a in 1:nrow(q))
     {
       first <- 0; # счетчик первого вхождения
       last <- 0; # счетчик последнего вхождения
       
       u <- filter(z, GroupItem==q[a, "unique.z.GroupItem."]) # фильтруем таблицу по номенлатуре
       
       for (w in 1:nrow(z))
       {
         u$i <- c(1:nrow(u));
         if (is.na(u[w,"Qty"])==FALSE & first == 0) 
         {first <- w} else 
         {
           if 
           (is.na(u[w,"Qty"])==FALSE & first > 0)  
           {last <- w} else
           {
             if(is.na(u[w,"Qty"])==FALSE & first > 0 & last > 0) {last <-w}
           }
         }
       }
       u$first <- first;
       u$last <- last;
       u$Date_end <- as.Date(u$Date_end);
       u$Date_start <- as.Date(u$Date_start);
       u$variable <- gsub("[X]", "", u$variable);
       u$variable <- gsub("[.]", "/", u$variable);
       u$variable <- as.Date(u$variable);
       u <- filter(u, i>=first & i<=last);# фильтруем по первому и последнему вхождениям 
       u_date_end <- filter(u, variable<=Date_end); # фильтруем по дате до который идет активный сезон данной позиции
       u_date_start <- filter(u, variable>=Date_start); # фильтруем по дате с которой начинается следующий активный сезон
       u <- rbind(u_date_end,u_date_start);
       u$weekday <- weekdays(u$variable);
       u$weekday <- recode(u$weekday, 
                                          "понедельник"=1,
                                          "вторник"=2,
                                          "среда"=3,
                                          "четверг"=4,
                                          "пятница"=5,
                                          "суббота"=6,
                                          "воскресенье"=7
                           ); # добавляем дни недели чтобы потом при соединении данных учесть недельную сезонность
       
       u$Qty <- na.interp(u$Qty, lambda = NULL); # восстанавливаем пропущенные данные по кол-ву продаж на выбранном промежутке 
       u$Mean_Price <- na.interp(u$Mean_Price, lambda = NULL); # восстанавливаем пропущенные данные цене продажи на выбранном промежутке
       u$Promo[is.na(u$Promo)] <- 0;# данные по промо не восстанавливаем, а присваеваем ноль
       total <- bind_rows(total,u)
       
     };              
   
     
     rhandsontable (total, width = 1000, height = 300)%>%hot_cols (fixedColumnsLeft = 4)

   })
   

   output$Forecast_tbl <- renderRHandsontable( {
     
     o<-data.frame(read.csv("Buffer_tbl.csv", sep=",", header = TRUE));
     o[is.na(o)] <- ""; # пропущенные значения меняем на пропуски так почему появляются логические значения 
     o$Date_start <- gsub("[-]", "/", o$Date_start);
     o$Date_start <- as.Date(o$Date_start);
     o$Date_end <- gsub("[-]", "/", o$Date_end);
     o$Date_end <- as.Date(o$Date_end);

     
     rhandsontable (o, width = 1000, height = 300)%>%hot_cols (fixedColumnsLeft = 5)
     
     
   }) 

   observeEvent(input$Forecast_Btn, # при нажатии кнопки запускаем подготовку данных для прогноза
                
                {
                  p <- data.frame(hot_to_r(input$Forecast_tbl));
                  p <- melt(p, id=c("Date_end", "Date_start", "GroupItem", "h_forecast", "Indicator"));
                  p <- dcast(p, ...~Indicator);
                  p$variable <- gsub("[X]", "", p$variable);
                  p$variable <- gsub("[.]", "/", p$variable);
                  p$variable <- as.Date(p$variable);
                  p$Date_start <- gsub("[X]", "", p$Date_start);
                  p$Date_start <- gsub("[.]", "/", p$Date_start);
                  p$Date_start <- as.Date(p$Date_start);
                  p$Date_end <- gsub("[X]", "", p$Date_end);
                  p$Date_end <- gsub("[.]", "/", p$Date_end);
                  p$Date_end <- as.Date(p$Date_end);
                  p_unique <- data.frame(unique(p$GroupItem));
                  restore_data <- data.frame(hot_to_r(input$Restore_tbl));
                  total_for_forecast <- data.frame();
                  total_forecast_rezult <- data.frame();
                  
                  for (m in 1:nrow(data.frame(unique(p$GroupItem))))
                  {
                    p_buffer <- filter(p, GroupItem==p_unique[m, "unique.p.GroupItem."]);
                    Day_week_of_forecast_tbl <- weekdays(as.Date(p_buffer$Date_end[1],'%d-%m-%Y'));
                    Day_week_of_forecast_tbl <- recode(Day_week_of_forecast_tbl, 
                                                       "понедельник"=1,
                                                       "вторник"=2,
                                                       "среда"=3,
                                                       "четверг"=4,
                                                       "пятница"=5,
                                                       "суббота"=6,
                                                       "воскресенье"=7);

                    
                    p_buffer <- filter(p_buffer, variable >= p_buffer$Date_end & variable <= p_buffer$Date_start); # 
                    h_forecast <- as.numeric(p$Date_start[1]-p$Date_end[1]+1); # считаем горизонт прогнозирования
                    restore_data_buffer <- filter(restore_data, GroupItem==p_unique[m, "unique.p.GroupItem."]);
                    
                    # create a variable to equalization of the end of the day of week in the restore table and forecast tsble
                    
                    if (Day_week_of_forecast_tbl==1)
                    {
                      buffer_for_weekday_of_restoretbl <- filter(restore_data_buffer, weekday==7);
                      buffer_for_weekday_of_restoretbl <- max(buffer_for_weekday_of_restoretbl$variable)
                      restore_data_buffer <- filter(restore_data_buffer, variable <= buffer_for_weekday_of_restoretbl)}
                      
                      else 
                      {
                        buffer_for_weekday_of_restoretbl <- filter(restore_data_buffer, weekday==(Day_week_of_forecast_tbl-1));
                        buffer_for_weekday_of_restoretbl <- max(buffer_for_weekday_of_restoretbl$variable);
                        restore_data_buffer <- filter(restore_data_buffer, variable <= buffer_for_weekday_of_restoretbl)
                      }
                    
                    
                    day_of_week_in_restore_tbl_of_last <- filter(restore_data_buffer, i==restore_data_buffer$last);
                    

                    restore_data_buffer <- restore_data_buffer[, -c(9:12)]; #удаляем ненужные столбцы
                    total_buffer <- rbind(restore_data_buffer, p_buffer);
                    total_for_forecast <- rbind(total_for_forecast, total_buffer);
                    ts_buffer <- filter(total_for_forecast, Qty>0);
                    ts_buffer <- data.frame(ts_buffer$Qty);
                    Promo <- data.frame(as.numeric(total_for_forecast$Promo)); # преобразовываем векторы экзогенных переменных для передачи в прогноз
                    Mean_Price <- data.frame(as.numeric(total_for_forecast$Mean_Price)); # преобразовываем векторы экзогенных переменных для передачи в прогноз
                    ts_model <- ts(ts_buffer, frequency = 7, start = 1);# создаем временной ряд
                    forecast_model <- auto.ces(ts_model,  h=h_forecast, xreg = cbind(Promo, Mean_Price), intervals = "np", silent = FALSE);
                    colnames(forecast_model$forecast) <- c('Qty');
                    rezult_forecast <- data.frame(variable=seq.Date(p_buffer$Date_end[1], p_buffer$Date_start[1], by = "day"), Qty = forecast_model$forecast, GroupItem = p_buffer$GroupItem);
        
                    total_forecast_rezult <- rbind(total_forecast_rezult, rezult_forecast);

                  };
                  
                  final_forecast_tbl_view <- (merge(p,total_forecast_rezult, by = c("variable", "GroupItem"), all.x = TRUE));
                  final_forecast_tbl_view$Qty.y <- as.numeric(final_forecast_tbl_view$Qty.y);
                  final_forecast_tbl_view$Qty.x <- as.numeric(final_forecast_tbl_view$Qty.x);
                  final_forecast_tbl_view$Qty <- rowSums(final_forecast_tbl_view[,c("Qty.y", "Qty.x")], na.rm=TRUE);
                  final_forecast_tbl_view <- final_forecast_tbl_view[, -c(8:9)];
                  colnames(final_forecast_tbl_view)[colnames(final_forecast_tbl_view)=="variable"] <- "Date";
                  final_forecast_tbl_view <- melt(final_forecast_tbl_view, id=c("Date_end", "Date_start", "GroupItem", "Date"));
                  final_forecast_tbl_view <- dcast(final_forecast_tbl_view, ... ~ Date);
                  colnames(final_forecast_tbl_view)[colnames(final_forecast_tbl_view)=="variable"] <- "Indicator";

                  output$Merge_foreacast_and_rezult_forecast_tbl <- renderRHandsontable({
                    
                    rhandsontable (final_forecast_tbl_view, width = 1000, height = 500)%>%hot_cols (fixedColumnsLeft = 4) 
                  });


                  output$Rezult_of_forecast <- renderRHandsontable({
                    colnames(total_forecast_rezult)[colnames(total_forecast_rezult)=="variable"] <- "Date";
                    rhandsontable (total_forecast_rezult, width = 1000, height = 500)%>%hot_cols (fixedColumnsLeft = 4) 
                  });
                  
                  

                }               
              )


}




shinyApp(ui, server, enableBookmarking = "server")

