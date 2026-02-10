# Загрузка модулей
source("server/module1.R")
source("server/module2.R")
source("server/module3.R")
source("server/module4.R")
source("server/module5.R")
source("server/module6.R")
source("server/module7.R")
source("server/module8.R")

server <- function(input, output, session) {
  callModule(module1Server, "mod1")
  callModule(module2Server, "mod2")
  callModule(module3Server, "mod3")
  callModule(module4Server, "mod4")
  callModule(module5Server, "mod5")
  callModule(module6Server, "mod6")
  callModule(module7Server, "mod7")
  callModule(module8Server, "mod8")
}