module3UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    box(
      title = "Модуль 2: Статистические расчеты",
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      
      fluidRow(
        column(6,
               textInput(ns("data_input"), "Введите данные (через запятую):", "1,2,3,4,5"),
               numericInput(ns("multiplier"), "Множитель:", value = 1)
        ),
        column(6,
               actionButton(ns("calc_stats"), "Рассчитать статистику", class = "btn-warning"),
               br(), br(),
               verbatimTextOutput(ns("stats_output"))
        )
      )
    )
  )
}

module3Server <- function(input, output, session) {
  observeEvent(input$calc_stats, {
    data <- as.numeric(unlist(strsplit(input$data_input, ",")))
    data <- data * input$multiplier
    
    if(all(!is.na(data))) {
      stats <- list(
        Среднее = mean(data),
        Медиана = median(data),
        "Стандартное отклонение" = sd(data),
        "Сумма" = sum(data)
      )
      
      output$stats_output <- renderPrint({
        stats
      })
    } else {
      output$stats_output <- renderPrint({
        "Ошибка: введите корректные числовые данные"
      })
    }
  })
}