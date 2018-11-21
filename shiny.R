installation = function(){
  list.of.packages <- c("shiny","shinydashboard","DT","rpivotTable","magrittr","DT","tidyverse","broom")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
}

load_Shiny = function(){
  library(shiny)
  args = commandArgs(trailingOnly = TRUE)
  if (length(args)==0) {
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
  } else if (length(args)==1) {
    # default output file
    runApp(port=8964, host=args[1])
  }
  # runApp(port=8964, host='192.168.8.5')
}

installation()
load_Shiny()