module2UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      tags$style(HTML("
        .box-header { background-color: #00a65a; color: white; }
        .result-box { 
          background-color: #f9f9f9; 
          border-left: 4px solid #00a65a;
          padding: 15px;
          margin: 10px 0;
        }
        .formula-box {
          background-color: #ecf0f5;
          padding: 10px;
          border-radius: 5px;
          margin: 10px 0;
        }
      "))
    ),
    
    box(
      title = "Оценка экономической эффективности создания АРМ",
      width = 12,
      solidHeader = TRUE,
      status = "success",
      collapsible = TRUE,
      
      tabsetPanel(
        id = ns("mainTabs"),
        type = "tabs",
        
        # ================= Вкладка 1: Исходные данные =================
        tabPanel(
          "1. Исходные данные",
          br(),
          fluidRow(
            column(6,
                   box(
                     title = "Параметры для расчёта единовременных затрат",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     
                     h5("Затраты на программирование (Pпо):"),
                     numericInput(ns("cnp"), "Стоимость 1 чел.-мес. (Cnp), руб.:", value = 200, min = 0),
                     numericInput(ns("tn"), "Длительность разработки (Tn), мес.:", value = 3.7, step = 0.1, min = 0),
                     p("Pпо = Cnp × Tn × 24"),
                     
                     hr(),
                     h5("Затраты на информационное обеспечение (Pио):"),
                     numericInput(ns("cm_io"), "Стоимость 1 чел.-дня (Cm), руб.:", value = 150, min = 0),
                     numericInput(ns("td_io"), "Затраты времени (tδ), дней:", value = 4, min = 0),
                     p("Pио = Cm × tδ"),
                     
                     hr(),
                     h5("Затраты на ввод в эксплуатацию (Pвв):"),
                     numericInput(ns("cm_ve"), "Стоимость 1 чел.-дня (Cm), руб.:", value = 150, min = 0),
                     numericInput(ns("td_ve"), "Затраты времени (tδ), дней:", value = 4, min = 0),
                     p("Pвв = Cm × tδ")
                   ),
                   
                   box(
                     title = "Параметры для расчёта текущих затрат",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     
                     numericInput(ns("iktc"), "Годовые затраты на эксплуатацию КТС (Иктс), руб./год:", value = 10307.5, min = 0),
                     numericInput(ns("salary_month"), "Заработная плата специалиста, руб./мес.:", value = 12890, min = 0),
                     p("Из (годовая зарплата) = зарплата × 12")
                   )
            ),
            
            column(6,
                   box(
                     title = "Параметры для расчёта экономии",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     
                     h5("Экономия от сокращения штатной единицы (П1):"),
                     numericInput(ns("p1_salary"), "Месячная зарплата секретаря-машинистки, руб.:", value = 6710, min = 0),
                     
                     hr(),
                     h5("Экономия от сокращения сроков выполнения задач (П2):"),
                     numericInput(ns("cc"), "Стоимость 1 часа работы специалиста (Cc), руб./час:", value = 67, min = 0),
                     numericInput(ns("t_p2"), "Время на задачу (t), час:", value = 20, min = 0),
                     numericInput(ns("n2"), "Количество задач (N2):", value = 50, min = 0),
                     numericInput(ns("cm_p2"), "Стоимость 1 дня (Cm), руб.:", value = 150, min = 0),
                     p("П2 = Cc × t × N2 - Cm × 0.15 × N2"),
                     
                     hr(),
                     h5("Экономия от сокращения времени на печать (П3):"),
                     numericInput(ns("cm_p3"), "Стоимость 1 часа (Cm), руб./час:", value = 33, min = 0),
                     numericInput(ns("t_p3"), "Время на один документ (t), час:", value = 3, min = 0),
                     numericInput(ns("n3"), "Количество документов (N3):", value = 200, min = 0),
                     p("П3 = Cm × t × N3 - Cm × 0.1 × N3"),
                     
                     hr(),
                     h5("Общие параметры:"),
                     numericInput(ns("en"), "Норматив эффективности (Eн):", value = 0.15, min = 0, step = 0.01),
                     numericInput(ns("dt"), "Сокращение длительности процесса (ΔT), лет:", value = 2, min = 0, step = 0.1)
                   ),
                   
                   box(
                     title = "Период расчёта",
                     width = 12,
                     solidHeader = TRUE,
                     status = "warning",
                     numericInput(ns("years"), "Количество лет функционирования:", value = 3, min = 1, max = 6)
                   )
            )
          )
        ),
        
        # ================= Вкладка 2: Результаты расчётов =================
        tabPanel(
          "2. Результаты расчётов",
          br(),
          fluidRow(
            column(6,
                   box(
                     title = "1. Единовременные затраты на создание АРМ",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     actionButton(ns("calc_p"), "Рассчитать P", class = "btn-success btn-sm"),
                     br(), br(),
                     div(class = "result-box",
                         h4("Затраты на программирование (Pпо):"),
                         uiOutput(ns("p_po_out")),
                         h4("Затраты на информационное обеспечение (Pио):"),
                         uiOutput(ns("p_io_out")),
                         h4("Затраты на ввод в эксплуатацию (Pвв):"),
                         uiOutput(ns("p_ve_out")),
                         hr(),
                         h3("ИТОГО: Единовременные затраты (P):"),
                         uiOutput(ns("p_total_out"))
                     ),
                     div(class = "formula-box",
                         withMathJax(),
                         p("Формулы:"),
                         helpText("$$P = P_{по} + P_{ио} + P_{вв}$$"),
                         helpText("$$P_{по} = C_{np} \\times T_n \\times 24$$"),
                         helpText("$$P_{ио} = C_m \\times t_\\delta$$"),
                         helpText("$$P_{вв} = C_m \\times t_\\delta$$")
                     )
                   ),
                   
                   box(
                     title = "2. Текущие затраты на функционирование",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     actionButton(ns("calc_ig"), "Рассчитать текущие затраты", class = "btn-success btn-sm"),
                     br(), br(),
                     div(class = "result-box",
                         h4("Годовые текущие затраты (Иг):"),
                         uiOutput(ns("ig_out")),
                         h4("Суммарные текущие затраты за период с приведением (И):"),
                         uiOutput(ns("i_sum_out"))
                     ),
                     div(class = "formula-box",
                         p("Формулы:"),
                         helpText("$$И_г = И_{ктс} + И_з, \\quad И_з = \\text{зарплата} \\times 12$$"),
                         helpText("$$И = И_г \\times (a_0 + a_1 + ... + a_{n-1})$$")
                     )
                   ),
                   
                   box(
                     title = "3. Суммарные затраты",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     div(class = "result-box",
                         h4("За год (К₂ = P + Иг):"),
                         uiOutput(ns("k2_out")),
                         h4("За период (К = P + И):"),
                         uiOutput(ns("k_total_out"))
                     )
                   )
            ),
            
            column(6,
                   box(
                     title = "4. Расчёт экономии (прибыли)",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     actionButton(ns("calc_profit"), "Рассчитать экономию", class = "btn-success btn-sm"),
                     br(), br(),
                     div(class = "result-box",
                         h4("Экономия от сокращения штатной единицы (П1):"),
                         uiOutput(ns("p1_out")),
                         h4("Экономия от сокращения сроков (П2):"),
                         uiOutput(ns("p2_out")),
                         h4("Экономия от сокращения времени печати (П3):"),
                         uiOutput(ns("p3_out")),
                         hr(),
                         h4("Годовая прибыль (П):"),
                         uiOutput(ns("p_annual_out")),
                         h4("Экономия за период (P₀):"),
                         uiOutput(ns("p0_out"))
                     )
                   ),
                   
                   box(
                     title = "5. Показатели эффективности",
                     width = 12,
                     solidHeader = TRUE,
                     status = "success",
                     # Убрали кнопку "Рассчитать эффективность" - показатели обновляются автоматически
                     br(),
                     div(class = "result-box",
                         h4("Годовой экономический эффект:"),
                         uiOutput(ns("effect_annual_out")),
                         h4("Экономический эффект за период:"),
                         uiOutput(ns("effect_total_out")),
                         h4("Коэффициент экономической эффективности (Eₖ):"),
                         uiOutput(ns("ek_out")),
                         h4("Срок окупаемости (T), лет:"),
                         uiOutput(ns("payback_out"))
                     )
                   ),
                   
                   box(
                     title = "Вывод",
                     width = 12,
                     solidHeader = TRUE,
                     status = "primary",
                     uiOutput(ns("conclusion_out"))
                   )
            )
          )
        ),
        
        # ================= Вкладка 3: Коэффициенты приведения =================
        tabPanel(
          "3. Коэффициенты приведения α",
          br(),
          fluidRow(
            column(6,
                   box(
                     title = "Таблица коэффициентов приведения",
                     width = 12,
                     solidHeader = TRUE,
                     status = "warning",
                     tableOutput(ns("alpha_table"))
                   )
            ),
            column(6,
                   box(
                     title = "Пояснение",
                     width = 12,
                     solidHeader = TRUE,
                     status = "info",
                     p("Коэффициенты приведения используются для приведения разновременных затрат и результатов к расчётному году (первому году функционирования АРМ)."),
                     p("В расчётах используются коэффициенты для лет, следующих за расчётным годом (a0, a1, a2 ...).")
                   )
            )
          )
        )
      )
    )
  )
}

module2Server <- function(input, output, session) {
  
  # Таблица коэффициентов приведения
  alpha_data <- data.frame(
    "Год (после расчётного)" = 0:6,
    "Коэффициент α" = c(1.0000, 0.9091, 0.8264, 0.7513, 0.6830, 0.6209, 0.5645)
  )
  
  output$alpha_table <- renderTable({
    alpha_data
  })
  
  # Реактивные значения для хранения результатов расчётов
  values <- reactiveValues(
    P = NA, P_po = NA, P_io = NA, P_ve = NA,
    Ig = NA, I_sum = NA,
    P1 = NA, P2 = NA, P3 = NA, P_annual = NA, P0 = NA,
    K2 = NA, K_total = NA
  )
  
  # --- 1. Расчёт единовременных затрат P ---
  observeEvent(input$calc_p, {
    # Pпо
    P_po <- input$cnp * input$tn * 24
    # Pио
    P_io <- input$cm_io * input$td_io
    # Pвв
    P_ve <- input$cm_ve * input$td_ve
    # Общее P
    P_total <- P_po + P_io + P_ve
    
    values$P_po <- P_po
    values$P_io <- P_io
    values$P_ve <- P_ve
    values$P <- P_total
  })
  
  # --- 2. Текущие затраты ---
  observeEvent(input$calc_ig, {
    Iz <- input$salary_month * 12
    Ig <- input$iktc + Iz
    
    years <- input$years
    alpha_sum <- sum(alpha_data[1:years, 2])
    I_sum <- Ig * alpha_sum
    
    values$Ig <- Ig
    values$I_sum <- I_sum
  })
  
  # --- Автоматическое обновление K2 и K_total при изменении P, Ig, I_sum ---
  observe({
    if (!is.na(values$P) && !is.na(values$Ig)) {
      values$K2 <- values$P + values$Ig
    } else {
      values$K2 <- NA
    }
  })
  
  observe({
    if (!is.na(values$P) && !is.na(values$I_sum)) {
      values$K_total <- values$P + values$I_sum
    } else {
      values$K_total <- NA
    }
  })
  
  # --- 4. Расчёт экономии ---
  observeEvent(input$calc_profit, {
    # П1
    P1 <- input$p1_salary * 12
    
    # П2
    P2 <- input$cc * input$t_p2 * input$n2 - input$cm_p2 * 0.15 * input$n2
    
    # П3
    P3 <- input$cm_p3 * input$t_p3 * input$n3 - input$cm_p3 * 0.1 * input$n3
    
    # Годовая прибыль П
    P_annual <- (P1 + P2 + P3) * (1 + input$en * input$dt)
    
    # Экономия за период P0
    years <- input$years
    alpha_sum <- sum(alpha_data[1:years, 2])
    P0 <- P_annual * alpha_sum
    
    values$P1 <- P1
    values$P2 <- P2
    values$P3 <- P3
    values$P_annual <- P_annual
    values$P0 <- P0
  })
  
  # --- ВЫВОДЫ (все output с проверкой на наличие данных) ---
  
  # Вывод для раздела 1
  output$p_po_out <- renderUI({
    if (is.na(values$P_po)) tags$p("—") else tags$p(style = "font-size:16px;", paste0(round(values$P_po, 2), " руб."))
  })
  
  output$p_io_out <- renderUI({
    if (is.na(values$P_io)) tags$p("—") else tags$p(style = "font-size:16px;", paste0(round(values$P_io, 2), " руб."))
  })
  
  output$p_ve_out <- renderUI({
    if (is.na(values$P_ve)) tags$p("—") else tags$p(style = "font-size:16px;", paste0(round(values$P_ve, 2), " руб."))
  })
  
  output$p_total_out <- renderUI({
    if (is.na(values$P)) tags$h4("—") else tags$h4(style = "color:#00a65a;", paste0(round(values$P, 2), " руб."))
  })
  
  # Вывод для раздела 2
  output$ig_out <- renderUI({
    if (is.na(values$Ig)) tags$p("—") else tags$p(style = "font-size:16px;", paste0(round(values$Ig, 2), " руб./год"))
  })
  
  output$i_sum_out <- renderUI({
    if (is.na(values$I_sum)) {
      tags$p("—")
    } else {
      tags$p(style = "font-size:16px;", paste0(round(values$I_sum, 2), " руб. за ", input$years, " лет"))
    }
  })
  
  # Вывод для раздела 3
  output$k2_out <- renderUI({
    if (is.na(values$K2)) tags$p("—") else tags$p(style = "font-size:16px;", paste0(round(values$K2, 2), " руб."))
  })
  
  output$k_total_out <- renderUI({
    if (is.na(values$K_total)) tags$p("—") else tags$p(style = "font-size:16px;", paste0(round(values$K_total, 2), " руб."))
  })
  
  # Вывод для раздела 4
  output$p1_out <- renderUI({
    if (is.na(values$P1)) tags$p("—") else tags$p(paste0(round(values$P1, 2), " руб./год"))
  })
  
  output$p2_out <- renderUI({
    if (is.na(values$P2)) tags$p("—") else tags$p(paste0(round(values$P2, 2), " руб./год"))
  })
  
  output$p3_out <- renderUI({
    if (is.na(values$P3)) tags$p("—") else tags$p(paste0(round(values$P3, 2), " руб./год"))
  })
  
  output$p_annual_out <- renderUI({
    if (is.na(values$P_annual)) tags$h4("—") else tags$h4(paste0(round(values$P_annual, 2), " руб./год"))
  })
  
  output$p0_out <- renderUI({
    if (is.na(values$P0)) {
      tags$p("—")
    } else {
      tags$p(paste0(round(values$P0, 2), " руб. за ", input$years, " лет"))
    }
  })
  
  # Вывод для раздела 5 (показатели эффективности)
  output$effect_annual_out <- renderUI({
    req(!is.na(values$P_annual), !is.na(values$K2))
    effect <- values$P_annual - values$K2
    tags$h4(paste0(round(effect, 2), " руб."))
  })
  
  output$effect_total_out <- renderUI({
    req(!is.na(values$P0), !is.na(values$K_total))
    effect <- values$P0 - values$K_total
    tags$h4(paste0(round(effect, 2), " руб."))
  })
  
  output$ek_out <- renderUI({
    req(!is.na(values$P_annual), !is.na(values$Ig), !is.na(values$P))
    ek <- (values$P_annual - values$Ig) / values$P
    tags$h4(round(ek, 2))
  })
  
  output$payback_out <- renderUI({
    req(!is.na(values$P_annual), !is.na(values$Ig), !is.na(values$P))
    payback <- values$P / (values$P_annual - values$Ig)
    tags$h4(paste0(round(payback, 2), " лет"))
  })
  
  # Вывод
  output$conclusion_out <- renderUI({
    req(!is.na(values$P_annual), !is.na(values$Ig), !is.na(values$P), !is.na(values$K2))
    
    ek <- (values$P_annual - values$Ig) / values$P
    effect_annual <- values$P_annual - values$K2
    
    if (ek >= input$en) {
      conclusion_text <- "Проект является экономически эффективным (Eₖ ≥ Eн). Рекомендуется к внедрению."
      color <- "green"
    } else {
      conclusion_text <- "Проект не соответствует нормативу эффективности (Eₖ < Eн). Требуется оптимизация."
      color <- "red"
    }
    
    tags$div(
      style = paste0("color:", color, "; font-weight:bold;"),
      tags$p(conclusion_text),
      tags$p(paste("Срок окупаемости:", round(values$P / (values$P_annual - values$Ig), 2), "лет")),
      tags$p(paste("Годовой экономический эффект:", round(effect_annual, 2), "руб."))
    )
  })
}