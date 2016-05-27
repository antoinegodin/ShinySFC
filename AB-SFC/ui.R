
shinyUI(fluidPage(
  titlePanel("Model SIM AB"),

  sidebarLayout(
    sidebarPanel(
      helpText("Model description"),
      tabsetPanel(
        #tabPanel("Source Code", tableOutput("sourceCode")),
        tabPanel("Parameters",fluidPage(
          sliderInput("alpha11",
                      label = "Propensity to consume out of income: constant",
                      min = 0, max = 1, value = 0.4),
          sliderInput("alpha12",
          						label = "Propensity to consume out of income: variable",
          						min = 0, max = 1, value = 0.25),

          sliderInput("alpha2",
                      label = "Propensity to consume out of wealth:",
                      min = 0, max = 1, value = 0.4),

          sliderInput("theta",
                      label = "Taxation out of income rate:",
                      min = 0, max = 1, value = 0.2),

          sliderInput("G_d",
                      label = "Government Expenditure",
                      min = 0, max = 100, value = 20),
          sliderInput("sigmap",
          						label = "Standard deviation prices",
          						min = 0, max = 0.2, value = 0.00)
        ))
        #tabPanel("DAG", plotOutput("dag"))
      )),


    mainPanel(
      tabsetPanel(
        tabPanel("Plot",fluidRow(
          column(3,checkboxGroupInput("checkGroup",
                                      label = h3("Variables"),
                                      choices = list("Consumption" = "C","Disposable Income" = "YD",
                                                     "Taxes" = "TAX", "Government Expenditure" = "G",
                                                     "GDP" = "Y", "Labor Force" = "LF", "Household wealth" = "H", 
                                      							 "Propensity to consume"="CY"),
                                      selected = c("YD"))),
          column(9, plotOutput("plot"))
        )
        ),
        tabPanel("Plot Scenario",fluidRow(
          column(3,checkboxGroupInput("checkGroup_scen",
                                      label = h3("Variables"),
                                      choices = list("Consumption" = "C","Disposable Income" = "YD",
                                      							 "Taxes" = "TAX", "Government Expenditure" = "G",
                                      							 "GDP" = "Y", "Labor Force" = "LF", "Household wealth" = "H", 
                                      							 "Propensity to consume"="CY"),
                                      selected = c("YD")),
                 numericInput("init",label = h3("Scenario init"),value = 50)),
          column(9, plotOutput("plotscen")),
          column(6,sliderInput("alpha11_scen",
                               label = "Propensity to consume out of income: constant",
                               min = 0, max = 1, value = 0.4),
          			 sliderInput("alpha12_scen",
          			 						label = "Propensity to consume out of income: variable",
          			 						min = 0, max = 1, value = 0.25),
                 sliderInput("alpha2_scen",
                             label = "Propensity to consume out of wealth:",
                             min = 0, max = 1, value = 0.4)),
          column(6,
                 sliderInput("theta_scen",
                             label = "Taxation out of income rate:",
                             min = 0, max = 1, value = 0.2),

                 sliderInput("G_d_scen",
                             label = "Government Expenditure",
                             min = 0, max = 100, value = 20),
          			 sliderInput("sigmap_scen",
          			 						label = "Standard deviation prices",
          			 						min = 0, max = 0.2, value = 0.05)
          )
        )
        )
      )
    )
  )
))

