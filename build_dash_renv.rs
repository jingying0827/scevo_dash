dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
.libPaths(Sys.getenv("R_LIBS_USER"))

install.packages("remotes")
install.packages("foreign")
install.packages("rgdal", repos="https://packagemanager.posit.co/cran/2023-04-20/")
remotes::install_github("dbca-wa/rivRmon")

install.packages("golem")
golem::document_and_reload()
install.packages("renv", repos="https://cloud.r-project.org/")
renv::init()
install.packages(c("attachment","dockerfiler"))
golem::add_dockerfile_with_renv_shinyproxy(output_dir="../deploy", from="rocker/r-ver")
