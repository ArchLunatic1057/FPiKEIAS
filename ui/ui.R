source("server/module1.R")
source("server/module2.R")
source("server/module3.R")
source("server/module4.R")
source("server/module5.R")
source("server/module6.R")
source("server/module7.R")
source("server/module8.R")

ui <- dashboardPage(
  dashboardHeader(title = "Приложение с 8 модулями"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Модуль 1", tabName = "mod1", icon = icon("calculator")),
      menuItem("Модуль 2", tabName = "mod2", icon = icon("chart-line")),
      menuItem("Модуль 3", tabName = "mod3", icon = icon("table")),
      menuItem("Модуль 4", tabName = "mod4", icon = icon("chart-bar")),
      menuItem("Модуль 5", tabName = "mod5", icon = icon("percentage")),
      menuItem("Модуль 6", tabName = "mod6", icon = icon("cogs")),
      menuItem("Модуль 7", tabName = "mod7", icon = icon("balance-scale")),
      menuItem("Модуль 8", tabName = "mod8", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "mod1", module1UI("mod1")),
      tabItem(tabName = "mod2", module2UI("mod2")),
      tabItem(tabName = "mod3", module3UI("mod3")),
      tabItem(tabName = "mod4", module4UI("mod4")),
      tabItem(tabName = "mod5", module5UI("mod5")),
      tabItem(tabName = "mod6", module6UI("mod6")),
      tabItem(tabName = "mod7", module7UI("mod7")),
      tabItem(tabName = "mod8", module8UI("mod8"))
    )
  )
)