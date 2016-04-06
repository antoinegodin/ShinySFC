#Place 1 Run once
library(PKSFC)
library(graph)
library(Rgraphviz)
library(grid)

shinyServer(
  function(input,output){
    #Place 2 Run once for every visitor of the app
    simex<-sfc.model("data/SIMEX.txt",modelName="SIMplest model")
    datasimex<-simulate(simex)

    output$text1<-renderText({
      simex$name
    })

    output$equations <- renderTable({
      temp<-as.data.frame(simex$equations[,c(1,2)])
      colnames(temp)<-c("Variable","Equation")
      temp
    })

    output$dag <- renderPlot({plot_graph_hierarchy(graph=generate.DAG.collaspe( adjacency = t(simex$matrix) )$orginal_graph,main="SIMEX")})

    output$plot <- renderPlot({
      run=FALSE
      if(input$alpha1!=as.numeric(simex$variables[13,2])){
        simex<-sfc.editVar(simex,var="alpha1",init=input$alpha1)
        run=TRUE
      }
      if(input$alpha2!=as.numeric(simex$variables[14,2])){
        simex<-sfc.editVar(simex,var="alpha2",init=input$alpha2)
        run=TRUE
      }
      if(input$theta!=as.numeric(simex$variables[15,2])){
        simex<-sfc.editVar(simex,var="theta",init=input$theta)
        run=TRUE
      }
      if(input$G_d!=as.numeric(simex$variables[16,2])){
        simex<-sfc.editVar(simex,var="G_d",init=input$G_d)
        run=TRUE
      }
      if(run)
        datasimex<-simulate(simex)
      #PLace 3, runs every time somtheing changes in the widgets
      myData<-as.data.frame(datasimex$baseline)
      varnames<-input$checkGroup
      matplot(myData[c(varnames)],type="l",xlab="",ylab="",lwd=2,lty=1)
      legend("topleft",col=1:length(varnames),bty='n',lwd=2,lty=1,legend=varnames)
      #
      #
    })

    output$plotscen <- renderPlot({
      run=FALSE
      if(input$alpha1!=as.numeric(simex$variables[13,2])){
        simex<-sfc.editVar(simex,var="alpha1",init=input$alpha1)
        run=TRUE
      }
      if(input$alpha2!=as.numeric(simex$variables[14,2])){
        simex<-sfc.editVar(simex,var="alpha2",init=input$alpha2)
        run=TRUE
      }
      if(input$theta!=as.numeric(simex$variables[15,2])){
        simex<-sfc.editVar(simex,var="theta",init=input$theta)
        run=TRUE
      }
      if(input$G_d!=as.numeric(simex$variables[16,2])){
        simex<-sfc.editVar(simex,var="G_d",init=input$G_d)
        run=TRUE
      }
      vars<-c()
      values<-c()
      if(input$alpha2_scen!=input$alpha2){
        vars<-c(vars,"alpha2")
        values<-c(values,input$alpha2_scen)
      }
      if(input$alpha1_scen!=input$alpha1){
        vars<-c(vars,"alpha1")
        values<-c(values,input$alpha1_scen)
      }
      if(input$theta_scen!=input$theta){
        vars<-c(vars,"theta")
        values<-c(values,input$theta_scen)
      }
      if(input$G_d_scen!=input$G_d){
        vars<-c(vars,"G_d")
        values<-c(values,input$G_d_scen)
      }
      if(length(vars)>0){
        simex<-sfc.addScenario(model=simex,vars=list(vars),values=list(values),inits=input$init,ends=2010)
        datasimex<-simulate(simex)
        myData<-as.data.frame(datasimex$scenario_1)
      }else{
        if(run)
          datasimex<-simulate(simex)
        myData<-as.data.frame(datasimex$baseline)
      }
      #PLace 3, runs every time somtheing changes in the widgets
      varnames<-input$checkGroup_scen
      matplot(rownames(myData),myData[c(varnames)],type="l",xlab="",ylab="",lwd=2,lty=1)
      legend("topleft",col=1:length(varnames),bty='n',lwd=2,lty=1,legend=varnames)
      #
      #
    })
  }
)