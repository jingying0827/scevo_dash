dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
.libPaths(Sys.getenv("R_LIBS_USER"))

install.packages("remotes")
install.packages("rgdal", repos="https://packagemanager.posit.co/cran/2023-04-20/")
remotes::install_github("dbca-wa/rivRmon")

install.packages(c("golem","dockerfiler"))
install.packages(c("devtools","roxygen2"))
install.packages(c("EnvStats","RPostgreSQL","aws.s3","ggh4x","ggprism","leaflet","openxlsx","patchwork","plotly","rio","rsconnect","shinycssloaders","shinydashboard","shinyjs","slickR","svglite"))

golem::document_and_reload()
devtools::build()

golem::add_dockerfile_shinyproxy(from="rocker/r-ver")

