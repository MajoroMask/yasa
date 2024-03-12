# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

options(shiny.port = 3838, shiny.host = '0.0.0.0')

library(yasa)

set_i18n('cn')
set_i18n(system.file('ext/i18n_cn.csv', package = 'yasa'), packages = 'yasa')

run_app()
