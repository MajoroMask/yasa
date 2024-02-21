# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

library(shiny)
library(yasa)

options(golem.app.prod = TRUE)

yasa::run_my_esquisse()
