# Start ShinyApp of Democracy Matrix
source("Setup/BaseFunctions.R")
source("Setup/Packages.R")
library(rsconnect)

runApp(appDir = "ShinyApp/")

# deployApp("ShinyApp", appName="Demokratiematrix")


