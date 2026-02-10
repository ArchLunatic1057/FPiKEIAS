library(DT)

module1UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      tags$style(HTML("
        .box-header { background-color: #3c8dbc; color: white; }
        .nav-tabs-custom { margin-top: 20px; }
        .result-box { 
          background-color: #f9f9f9; 
          border-left: 4px solid #3c8dbc;
          padding: 15px;
          margin: 10px 0;
        }
      "))
    ),
    
    box(
      title = "Определение показателей экономической эффективности ИАС",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      
      tabsetPanel(
        id = ns("mainTabs"),
        type = "tabs",
        
        # Вкладка 1: Общие показатели эффективности
        tabPanel(
          "1. Общие показатели",
          br(),
          fluidRow(
            column(4,
                   box(
                     title = "Входные данные",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     numericInput(ns("annual_saving"), "Годовая экономия (П), тыс. руб.:", value = 500),
                     numericInput(ns("total_costs"), "Суммарные затраты (К), тыс. руб.:", value = 1000),
                     numericInput(ns("one_time_costs"), "Единовременные затраты (Р), тыс. руб.:", value = 800),
                     numericInput(ns("en_norm"), "Норматив эффективности (Eн):", value = 0.2, step = 0.01),
                     actionButton(ns("calc_general"), "Рассчитать", class = "btn-success")
                   )
            ),
            column(8,
                   box(
                     title = "Результаты расчетов",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     div(class = "result-box",
                         h4("Годовой экономический эффект:"),
                         uiOutput(ns("annual_effect_output")),
                         hr(),
                         h4("Коэффициент экономической эффективности:"),
                         uiOutput(ns("efficiency_coeff_output")),
                         hr(),
                         h4("Срок окупаемости:"),
                         uiOutput(ns("payback_period_output")),
                         hr(),
                         h4("Оценка эффективности:"),
                         uiOutput(ns("efficiency_assessment"))
                     ),
                     br(),
                     h4("Используемые формулы:"),
                     withMathJax(),
                     helpText("$$\\text{Годовой экономический эффект: } Э = П - К$$"),
                     helpText("$$\\text{Коэффициент эффективности: } E_k = \\frac{П}{P}$$"),
                     helpText("$$\\text{Срок окупаемости: } T = \\frac{P}{П}$$")
                   )
            )
          )
        ),
        
        # Вкладка 2: Расчет экономии
        tabPanel(
          "2. Расчет годовой экономии",
          br(),
          fluidRow(
            column(4,
                   box(
                     title = "Составляющие экономии",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     numericInput(ns("saving_p1"), "Экономия П1 (ресурсы), тыс. руб.:", value = 200),
                     numericInput(ns("saving_p2"), "Экономия П2 (качество), тыс. руб.:", value = 150),
                     numericInput(ns("saving_p3"), "Доп. прибыль П3, тыс. руб.:", value = 150),
                     numericInput(ns("dt_reduction"), "Сокращение длительности процесса (ΔT), лет:", value = 0.5, step = 0.1),
                     actionButton(ns("calc_saving"), "Рассчитать экономию", class = "btn-success")
                   )
            ),
            column(8,
                   box(
                     title = "Итоговая годовая экономия",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     div(class = "result-box",
                         h4("Общая годовая экономия (П):"),
                         uiOutput(ns("total_saving_output")),
                         hr(),
                         h4("Расчет по формуле:"),
                         withMathJax(),
                         uiOutput(ns("saving_formula_calc"))
                     ),
                     br(),
                     h4("Формула расчета:"),
                     withMathJax(),
                     helpText("$$П = (П_1 + П_2 + П_3) \\times (1 + E_н \\times \\Delta T)$$"),
                     helpText("где:"),
                     helpText("П₁ – экономия от сокращения ресурсов"),
                     helpText("П₂ – экономия от повышения качества"),
                     helpText("П₃ – дополнительная прибыль"),
                     helpText("Eн – норматив эффективности"),
                     helpText("ΔT – сокращение длительности процесса")
                   )
            )
          )
        ),
        
        # Вкладка 3: Расчет суммарных затрат
        tabPanel(
          "3. Суммарные затраты (K)",
          br(),
          fluidRow(
            column(4,
                   box(
                     title = "Входные данные для расчета K",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     numericInput(ns("annual_operating_costs"), "Годовые текущие издержки (И₂), тыс. руб.:", value = 300),
                     numericInput(ns("service_life"), "Срок службы ТС (Тсл), лет:", value = 5, min = 1, max = 15),
                     actionButton(ns("calc_total_costs"), "Рассчитать затраты", class = "btn-success")
                   ),
                   box(
                     title = "Коэффициент реновации",
                     width = 12,
                     solidHeader = TRUE,
                     status = "warning",
                     tableOutput(ns("kp_table")),
                     helpText("Значение kp для выбранного срока службы:"),
                     textOutput(ns("selected_kp"))
                   )
            ),
            column(8,
                   box(
                     title = "Расчет суммарных затрат",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     div(class = "result-box",
                         h4("Коэффициент реновации (kp):"),
                         uiOutput(ns("kp_calculated")),
                         hr(),
                         h4("Суммарные затраты (K):"),
                         uiOutput(ns("total_K_output")),
                         hr(),
                         h4("Расчет по формуле:"),
                         withMathJax(),
                         uiOutput(ns("K_formula_calc"))
                     ),
                     br(),
                     h4("Используемые формулы:"),
                     withMathJax(),
                     helpText("$$K = И_2 + (k_p + E_н) \\times P$$"),
                     helpText("$$k_p = \\frac{E_н}{(1 + E_н)^{T_{сл}} - 1}$$")
                   )
            )
          )
        ),
        
        # Вкладка 4: Коэффициенты приведения
        tabPanel(
          "4. Приведение разновременных затрат",
          br(),
          fluidRow(
            column(6,
                   box(
                     title = "Таблица коэффициентов приведения",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     DTOutput(ns("alpha_table"))
                   )
            ),
            column(6,
                   box(
                     title = "Расчет экономического эффекта за период",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     numericInput(ns("calc_years"), "Период расчета (лет):", value = 5, min = 1, max = 10),
                     actionButton(ns("calc_period"), "Рассчитать", class = "btn-success"),
                     hr(),
                     div(class = "result-box",
                         h4("Суммарный экономический эффект за период:"),
                         uiOutput(ns("total_effect_output")),
                         hr(),
                         h4("Формула расчета:"),
                         withMathJax(),
                         helpText("$$\\Omega = P_0 - K_0$$"),
                         helpText("$$P_0 = \\sum P_t \\times a_t$$"),
                         helpText("$$K_0 = \\sum K_t \\times a_t$$")
                     )
                   )
            )
          )
        ),
        
        # Вкладка 5: Единовременные затраты
        tabPanel(
          "5. Единовременные затраты (P)",
          br(),
          fluidRow(
            column(4,
                   box(
                     title = "Предпроизводственные затраты",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     numericInput(ns("cost_design"), "Проектирование (Pпр), тыс. руб.:", value = 100),
                     numericInput(ns("cost_programming"), "Программирование (Pпо), тыс. руб.:", value = 200),
                     numericInput(ns("cost_info"), "Инф. обеспечение (Pио), тыс. руб.:", value = 50),
                     numericInput(ns("cost_debug"), "Отладка (Pвв), тыс. руб.:", value = 50)
                   )
            ),
            column(4,
                   box(
                     title = "Капитальные затраты",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     numericInput(ns("cost_ktc"), "КТС (Рктс), тыс. руб.:", value = 300),
                     numericInput(ns("cost_install"), "Монтаж (Рмонт), тыс. руб.:", value = 50),
                     numericInput(ns("cost_inventory"), "Инвентарь (Ринв), тыс. руб.:", value = 20),
                     numericInput(ns("cost_buildings"), "Здания (Рзд), тыс. руб.:", value = 100)
                   )
            ),
            column(4,
                   box(
                     title = "Расчет",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     actionButton(ns("calc_one_time"), "Рассчитать P", class = "btn-success"),
                     hr(),
                     div(class = "result-box",
                         h4("Предпроизводственные затраты (Pпп):"),
                         uiOutput(ns("preprod_costs")),
                         hr(),
                         h4("Капитальные затраты (Pк):"),
                         uiOutput(ns("capital_costs")),
                         hr(),
                         h4("Общие единовременные затраты (P):"),
                         uiOutput(ns("total_one_time_costs"))
                     )
                   )
            )
          )
        ),
        
        # Вкладка 6: Затраты на ПО
        tabPanel(
          "6. Затраты на разработку ПО",
          br(),
          fluidRow(
            column(4,
                   box(
                     title = "Исходные данные",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     numericInput(ns("source_commands"), "Число исходных команд (тыс.):", value = 10, min = 1),
                     actionButton(ns("calc_po"), "Рассчитать", class = "btn-success")
                   )
            ),
            column(8,
                   box(
                     title = "Результаты расчета",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     div(class = "result-box",
                         h4("Трудоемкость разработки:"),
                         uiOutput(ns("labor_intensity")),
                         hr(),
                         h4("Длительность разработки:"),
                         uiOutput(ns("development_time")),
                         hr(),
                         h4("Производительность труда:"),
                         uiOutput(ns("productivity")),
                         hr(),
                         h4("Среднее число исполнителей:"),
                         uiOutput(ns("avg_workers"))
                     ),
                     br(),
                     h4("Используемые формулы:"),
                     withMathJax(),
                     helpText("$$t = 3,6 \\times n^{1,2} \\text{ (чел.-мес.)}$$"),
                     helpText("$$T = 2,5 \\times t^{0,32} \\text{ (мес.)}$$"),
                     helpText("$$Пр = \\frac{1000 \\times n}{t} \\text{ (исх. команд/чел.-мес.)}$$"),
                     helpText("$$Ч_п = \\frac{t}{T} \\text{ (чел.)}$$")
                   )
            )
          )
        ),
        
        # Вкладка 7: Текущие затраты
        tabPanel(
          "7. Текущие затраты на функционирование",
          br(),
          fluidRow(
            column(6,
                   tabBox(
                     title = "Методы расчета",
                     width = 12,
                     side = "left",
                     tabPanel(
                       "Метод 1",
                       numericInput(ns("cost_kca"), "Затраты на КСА, тыс. руб.:", value = 200),
                       numericInput(ns("cost_salary"), "Зарплата специалистов, тыс. руб.:", value = 100),
                       actionButton(ns("calc_method1"), "Рассчитать (Метод 1)", class = "btn-info")
                     ),
                     tabPanel(
                       "Метод 2",
                       numericInput(ns("num_tasks"), "Число задач (n):", value = 10),
                       numericInput(ns("cost_per_task"), "Затраты на задачу, тыс. руб.:", value = 20),
                       numericInput(ns("system_costs"), "Общесистемные затраты, тыс. руб.:", value = 50),
                       actionButton(ns("calc_method2"), "Рассчитать (Метод 2)", class = "btn-info")
                     )
                   )
            ),
            column(6,
                   box(
                     title = "Годовые текущие затраты (Иг)",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     div(class = "result-box",
                         h4("По методу 1:"),
                         uiOutput(ns("method1_result")),
                         hr(),
                         h4("По методу 2:"),
                         uiOutput(ns("method2_result"))
                     ),
                     br(),
                     h4("Формулы расчета:"),
                     withMathJax(),
                     helpText("$$\\text{Метод 1: } И_г = И_{кса} + И_з$$"),
                     helpText("$$\\text{Метод 2: } И_г = \\sum_{i=1}^{n} И_i + И_{сист}$$")
                   )
            )
          )
        ),
        
        # Вкладка 8: Общие результаты
        tabPanel(
          "8. Сводный отчет",
          br(),
          fluidRow(
            column(12,
                   box(
                     title = "Итоговые показатели эффективности ИАС",
                     width = 12,
                     solidHeader = TRUE,
                     status = "primary",
                     actionButton(ns("generate_report"), "Сформировать отчет", class = "btn-success"),
                     hr(),
                     div(class = "result-box",
                         h4("Основные экономические показатели:"),
                         tableOutput(ns("summary_table")),
                         hr(),
                         h4("Рекомендации:"),
                         uiOutput(ns("recommendations"))
                     ),
                     br(),
                     downloadButton(ns("download_report"), "Скачать отчет")
                   )
            )
          )
        )
      )
    )
  )
}

module1Server <- function(input, output, session) {
  
  # Таблица коэффициента реновации
  kp_data <- data.frame(
    "Срок службы" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
    "kp" = c(1.0000,0.4762,0.3021,0.2156,0.1638,0.1296,0.1054,0.0874,0.0736,0.0627,0.0540,0.0468,0.0408,0.0352,0.0315)
  )
  
  # Таблица коэффициентов приведения
  alpha_data <- data.frame(
    "Годы до" = c(5,4,3,2,1,0),
    "α (до)" = c(1.6105,1.4641,1.3310,1.2100,1.1000,1.0000),
    "Годы после" = c(1,2,3,4,5,6),
    "α (после)" = c(0.9091,0.8264,0.7513,0.6830,0.6209,0.5645)
  )
  
  # 1. Общие показатели эффективности
  observeEvent(input$calc_general, {
    annual_saving <- input$annual_saving
    total_costs <- input$total_costs
    one_time_costs <- input$one_time_costs
    en_norm <- input$en_norm
    
    # Расчет показателей
    annual_effect <- annual_saving - total_costs
    efficiency_coeff <- annual_saving / one_time_costs
    payback_period <- one_time_costs / annual_saving
    
    output$annual_effect_output <- renderUI({
      tags$div(
        tags$h3(paste0(round(annual_effect, 2), " тыс. руб.")),
        tags$p(paste("Э =", annual_saving, "-", total_costs, "=", round(annual_effect, 2)))
      )
    })
    
    output$efficiency_coeff_output <- renderUI({
      tags$div(
        tags$h3(paste0(round(efficiency_coeff, 3))),
        tags$p(paste("Eₖ =", annual_saving, "/", one_time_costs, "=", round(efficiency_coeff, 3)))
      )
    })
    
    output$payback_period_output <- renderUI({
      tags$div(
        tags$h3(paste0(round(payback_period, 2), " лет")),
        tags$p(paste("T =", one_time_costs, "/", annual_saving, "=", round(payback_period, 2)))
      )
    })
    
    output$efficiency_assessment <- renderUI({
      if (efficiency_coeff >= en_norm) {
        tags$div(
          style = "color: green;",
          tags$h4("✓ Капитальные вложения ЭФФЕКТИВНЫ"),
          tags$p(paste("Eₖ =", round(efficiency_coeff, 3), "≥ Eн =", en_norm))
        )
      } else {
        tags$div(
          style = "color: red;",
          tags$h4("✗ Капитальные вложения НЕЭФФЕКТИВНЫ"),
          tags$p(paste("Eₖ =", round(efficiency_coeff, 3), "< Eн =", en_norm))
        )
      }
    })
  })
  
  # 2. Расчет годовой экономии
  observeEvent(input$calc_saving, {
    p1 <- input$saving_p1
    p2 <- input$saving_p2
    p3 <- input$saving_p3
    dt <- input$dt_reduction
    en <- input$en_norm
    
    total_saving <- (p1 + p2 + p3) * (1 + en * dt)
    
    output$total_saving_output <- renderUI({
      tags$div(
        tags$h3(paste0(round(total_saving, 2), " тыс. руб.")),
        tags$p("Общая годовая экономия (П)")
      )
    })
    
    output$saving_formula_calc <- renderUI({
      withMathJax(
        sprintf("$$П = (%d + %d + %d) \\times (1 + %.2f \\times %.1f) = %.2f$$",
                p1, p2, p3, en, dt, total_saving)
      )
    })
  })
  
  # 3. Расчет суммарных затрат
  output$kp_table <- renderTable({
    kp_data
  })
  
  observeEvent(input$calc_total_costs, {
    i2 <- input$annual_operating_costs
    t_sl <- input$service_life
    en <- input$en_norm
    p <- input$one_time_costs
    
    # Расчет kp по формуле
    kp <- en / ((1 + en)^t_sl - 1)
    
    # Находим ближайшее табличное значение
    table_kp <- kp_data$kp[which.min(abs(kp_data$Срок.службы - t_sl))]
    
    total_K <- i2 + (kp + en) * p
    
    output$kp_calculated <- renderUI({
      tags$div(
        tags$p(paste("По формуле:", round(kp, 4))),
        tags$p(paste("Табличное значение:", round(table_kp, 4)))
      )
    })
    
    output$selected_kp <- renderText({
      paste("Для срока службы", t_sl, "лет: kp =", round(table_kp, 4))
    })
    
    output$total_K_output <- renderUI({
      tags$h3(paste0(round(total_K, 2), " тыс. руб."))
    })
    
    output$K_formula_calc <- renderUI({
      withMathJax(
        sprintf("$$K = %.0f + (%.4f + %.2f) \\times %.0f = %.2f$$",
                i2, kp, en, p, total_K)
      )
    })
  })
  
  # 4. Коэффициенты приведения
  output$alpha_table <- renderDT({
    datatable(alpha_data, 
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE) %>%
      formatRound(columns = c(2,4), digits = 4)
  })
  
  observeEvent(input$calc_period, {
    annual_saving <- input$annual_saving
    total_costs <- input$total_costs
    years <- input$calc_years
    
    # Упрощенный расчет (можно усложнить с учетом коэффициентов)
    total_effect <- years * (annual_saving - total_costs)
    
    output$total_effect_output <- renderUI({
      tags$div(
        tags$h3(paste0(round(total_effect, 2), " тыс. руб.")),
        tags$p(paste("За период", years, "лет"))
      )
    })
  })
  
  # 5. Единовременные затраты
  observeEvent(input$calc_one_time, {
    preprod <- input$cost_design + input$cost_programming + 
      input$cost_info + input$cost_debug
    
    capital <- input$cost_ktc + input$cost_install + 
      input$cost_inventory + input$cost_buildings
    
    total_one_time <- preprod + capital
    
    output$preprod_costs <- renderUI({
      tags$h4(paste0(round(preprod, 2), " тыс. руб."))
    })
    
    output$capital_costs <- renderUI({
      tags$h4(paste0(round(capital, 2), " тыс. руб."))
    })
    
    output$total_one_time_costs <- renderUI({
      tags$h3(paste0(round(total_one_time, 2), " тыс. руб."))
    })
  })
  
  # 6. Затраты на разработку ПО
  observeEvent(input$calc_po, {
    n <- input$source_commands
    
    # Расчет по формулам из PDF
    t <- 3.6 * (n)^1.2  # трудоемкость в чел-мес
    T_months <- 2.5 * (t)^0.32  # длительность в мес
    productivity <- 1000 * n / t  # производительность
    avg_workers <- t / T_months  # среднее число исполнителей
    
    output$labor_intensity <- renderUI({
      tags$div(
        tags$h4(paste0(round(t, 2), " чел.-мес.")),
        tags$p(paste("t = 3,6 ×", n, "¹·² =", round(t, 2)))
      )
    })
    
    output$development_time <- renderUI({
      tags$div(
        tags$h4(paste0(round(T_months, 2), " мес.")),
        tags$p(paste("T = 2,5 ×", round(t, 2), "⁰·³² =", round(T_months, 2)))
      )
    })
    
    output$productivity <- renderUI({
      tags$div(
        tags$h4(paste0(round(productivity, 2), " команд/чел.-мес.")),
        tags$p(paste("Пр = 1000 ×", n, "/", round(t, 2), "=", round(productivity, 2)))
      )
    })
    
    output$avg_workers <- renderUI({
      tags$div(
        tags$h4(paste0(round(avg_workers, 2), " чел.")),
        tags$p(paste("Чп =", round(t, 2), "/", round(T_months, 2), "=", round(avg_workers, 2)))
      )
    })
  })
  
  # 7. Текущие затраты
  observeEvent(input$calc_method1, {
    method1_result <- input$cost_kca + input$cost_salary
    
    output$method1_result <- renderUI({
      tags$h4(paste0(round(method1_result, 2), " тыс. руб./год"))
    })
  })
  
  observeEvent(input$calc_method2, {
    method2_result <- input$num_tasks * input$cost_per_task + input$system_costs
    
    output$method2_result <- renderUI({
      tags$h4(paste0(round(method2_result, 2), " тыс. руб./год"))
    })
  })
  
  # 8. Сводный отчет
  observeEvent(input$generate_report, {
    # Собираем все рассчитанные данные
    summary_data <- data.frame(
      Показатель = c("Годовая экономия (П)", "Суммарные затраты (К)", 
                     "Единовременные затраты (Р)", "Годовой экономический эффект (Э)",
                     "Коэффициент эффективности (Eₖ)", "Срок окупаемости (T)"),
      Значение = c(
        input$annual_saving,
        input$total_costs,
        input$one_time_costs,
        input$annual_saving - input$total_costs,
        round(input$annual_saving / input$one_time_costs, 3),
        round(input$one_time_costs / input$annual_saving, 2)
      ),
      Единицы = c("тыс. руб.", "тыс. руб.", "тыс. руб.", "тыс. руб.", "", "лет")
    )
    
    output$summary_table <- renderTable({
      summary_data
    })
    
    efficiency_coeff <- input$annual_saving / input$one_time_costs
    
    output$recommendations <- renderUI({
      tags$div(
        if(efficiency_coeff >= input$en_norm) {
          tags$p(style = "color: green; font-weight: bold;",
                 "✓ РЕКОМЕНДУЕТСЯ внедрение ИАС. Проект экономически эффективен.")
        } else {
          tags$p(style = "color: red; font-weight: bold;",
                 "✗ НЕ РЕКОМЕНДУЕТСЯ внедрение ИАС. Проект не соответствует нормативу эффективности.")
        },
        tags$hr(),
        tags$p("Дополнительные рекомендации:"),
        tags$ul(
          tags$li("Провести оптимизацию единовременных затрат"),
          tags$li("Увеличить ожидаемую годовую экономию"),
          tags$li("Рассмотреть альтернативные варианты реализации")
        )
      )
    })
  })
  
  # Функция для скачивания отчета
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Отчет_эффективности_ИАС_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(
        c(
          "ОТЧЕТ ПО ОЦЕНКЕ ЭКОНОМИЧЕСКОЙ ЭФФЕКТИВНОСТИ ИАС",
          paste("Дата формирования:", Sys.Date()),
          "",
          "1. Основные показатели:",
          paste("Годовая экономия (П):", input$annual_saving, "тыс. руб."),
          paste("Суммарные затраты (К):", input$total_costs, "тыс. руб."),
          paste("Единовременные затраты (Р):", input$one_time_costs, "тыс. руб."),
          paste("Годовой экономический эффект (Э):", input$annual_saving - input$total_costs, "тыс. руб."),
          paste("Коэффициент эффективности (Eₖ):", round(input$annual_saving / input$one_time_costs, 3)),
          paste("Срок окупаемости (T):", round(input$one_time_costs / input$annual_saving, 2), "лет"),
          "",
          "2. Вывод:",
          if(input$annual_saving / input$one_time_costs >= input$en_norm) {
            "Проект внедрения ИАС является экономически эффективным."
          } else {
            "Проект внедрения ИАС не соответствует нормативу эффективности."
          }
        ),
        file
      )
    }
  )
}