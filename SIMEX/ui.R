
shinyUI(fluidPage(
  titlePanel("Model SIMEX"),

  sidebarLayout(
    sidebarPanel(
      helpText("Model description"),
      tabsetPanel(
        tabPanel("Equations", tableOutput("equations")),
        tabPanel("Parameters",fluidPage(
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
        )),
        tabPanel("DAG", plotOutput("dag"))
      )),


    mainPanel(
      tabsetPanel(
        tabPanel("Plot",fluidRow(
          column(3,checkboxGroupInput("checkGroup",
                                      label = h3("Variables"),
                                      choices = list("Consumption" = "C_s","Disposable Income" = "Yd",
                                                     "Taxes" = "T_s", "Government Expenditure" = "G_s",
                                                     "GDP" = "Y", "Expected disposable income" = "Yd_e",
                                                     "Household wealth" = "H_s"),
                                      selected = c("Yd_e","Yd"))),
          column(9, plotOutput("plot"))
        )
        ),
        tabPanel("Plot Scenario",fluidRow(
          column(3,checkboxGroupInput("checkGroup_scen",
                                      label = h3("Variables"),
                                      choices = list("Consumption" = "C_s","Disposable Income" = "Yd",
                                                     "Taxes" = "T_s", "Government Expenditure" = "G_s",
                                                     "GDP" = "Y", "Expected disposable income" = "Yd_e",
                                                     "Household wealth" = "H_s"),
                                      selected = c("Yd_e","Yd")),
                 numericInput("init",label = h3("Scenario init"),value = 1975)),
          column(9, plotOutput("plotscen")),
          column(6,sliderInput("alpha1_scen",
                               label = "Propensity to consume out of income:",
                               min = 0, max = 1, value = 0.6),

                 sliderInput("alpha2_scen",
                             label = "Propensity to consume out of wealth:",
                             min = 0, max = 1, value = 0.4)),
          column(6,
                 sliderInput("theta_scen",
                             label = "Taxation out of income rate:",
                             min = 0, max = 1, value = 0.2),

                 sliderInput("G_d_scen",
                             label = "Government Expenditure",
                             min = 0, max = 100, value = 20)
          )
        )
        )
      )
    )
  )
))

