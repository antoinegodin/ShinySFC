
shinyUI(fluidPage(
  titlePanel("Agent-Based version of model SIM"),

  sidebarLayout(
    sidebarPanel(
      helpText("Model description"),
      tabsetPanel(
        tabPanel("Algorithm",
        				 p("Algo here")
        				 ),
        tabPanel("Parameters AB",fluidPage(
          sliderInput("alpha11",
                      label = "Propensity to consume out of income: constant",
                      min = 0, max = 1, value = 0.6),
          sliderInput("alpha12",
          						label = "Propensity to consume out of income: variable",
          						min = 0, max = 1, value = 0.0),

          sliderInput("sigmap",
          						label = "Standard deviation prices",
          						min = 0, max = 0.2, value = 0.00),
          sliderInput("nF",
          						label = "Number of Firms",
          						min = 0, max = 1000, value = 100),
          sliderInput("N",
          						label = "Number of Households",
          						min = 0, max = 1000, value = 120),
          numericInput("seed",label = "Seed number",value = 50)
        )),
        tabPanel("Parameters SFC",fluidPage(
        	sliderInput("alpha1",
        							label = "Propensity to consume out of income:",
        							min = 0, max = 1, value = 0.6),
        	
        	sliderInput("alpha2",
        							label = "Propensity to consume out of wealth:",
        							min = 0, max = 1, value = 0.4),
        	
        	sliderInput("theta",
        							label = "Taxation out of income rate:",
        							min = 0, max = 1, value = 0.2),
        	
        	sliderInput("G_d",
        							label = "Government Expenditure",
        							min = 0, max = 100, value = 20)
        	
        ))
      )),


    mainPanel(
      tabsetPanel(
        tabPanel("Plot",fluidRow(
          column(3,checkboxGroupInput("checkGroup",
                                      label = h3("Common Variables"),
                                      choices = list("Consumption" = "C_s","Disposable Income" = "Yd",
                                                     "Taxes" = "T_s", "Government Expenditure" = "G_s",
                                                     "GDP" = "Y", "Labor Force" = "LF", "Household wealth" = "H_s", 
                                      							 "Propensity to consume"="CY"),
                                      selected = c("Y"))),
          column(9, plotOutput("plot"))
        )
        ),
        tabPanel("Plot Scenario",fluidRow(
          column(3,checkboxGroupInput("checkGroup_scen",
                                      label = h3("Variables"),
                                      choices = list("Consumption" = "C_s","Disposable Income" = "Yd",
                                      							 "Taxes" = "T_s", "Government Expenditure" = "G_s",
                                      							 "GDP" = "Y", "Labor Force" = "LF", "Household wealth" = "H_s", 
                                      							 "Propensity to consume"="CY"),
                                      selected = c("Y")),
                 numericInput("init",label = h3("Scenario init"),value = 50)),
          column(9, plotOutput("plotscen")),
          column(6,sliderInput("alpha11_scen",
                               label = "Propensity to consume out of income: constant",
                               min = 0, max = 1, value = 0.6),
          			 sliderInput("alpha12_scen",
          			 						label = "Propensity to consume out of income: variable",
          			 						min = 0, max = 1, value = 0),
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
          			 						min = 0, max = 0.2, value = 0)
          )
        )
        )
      )
    )
  )
))

