#Place 1 Run once
source("data/SourceCode.R")
#library(expm)
#library(Matrix)

shinyServer(
	function(input,output){
		#Place 2 Run once for every visitor of the app
		
		simex<-{}
		simex$phi_init = 1
		simex$alpha11_init = 0.4
		simex$alpha12_init = 0.25
		simex$alpha2_init = 0.4
		simex$theta_init = 0.2
		simex$sigmap_init = 0.00
		simex$sigmaw_init = 0.00
		simex$G_init = 20
		simex$phi_baseline = 1
		simex$alpha11_baseline = 0.4
		simex$alpha12_baseline = 0.25
		simex$alpha2_baseline = 0.4
		simex$theta_baseline = 0.2
		simex$sigmap_baseline = 0.00
		simex$sigmaw_baseline = 0.00
		simex$G_baseline = 20
		simex$phi_shock = 1
		simex$alpha11_shock = 0.4
		simex$alpha12_shock = 0.25
		simex$alpha2_shock = 0.4
		simex$theta_shock = 0.2
		simex$sigmap_shock = 0.05
		simex$sigmaw_shock = 0.05
		simex$G_shock = 20
		simex$shock = 50
		simex$incomeDistr=TRUE
		
		datasimex<-simulateAB()
		
		#		output$sourceCode <- renderText({
		#			readLines("data/SourceCode.R")
		#		})
		
		#		output$dag <- renderPlot({
		#			plot_graph_hierarchy(graph=generate.DAG.collaspe( adjacency = t(simex$matrix) )$orginal_graph,main="SIMEX")
		#		})
		
		output$plot <- renderPlot({
			run=FALSE
			if(input$alpha11!=as.numeric(simex$alpha11_init)){
				simex$alpha11_baseline<-input$alpha11
				run=TRUE
			}
			if(input$alpha12!=as.numeric(simex$alpha12_init)){
				simex$alpha12_baseline<-input$alpha12
				run=TRUE
			}
			if(input$alpha2!=as.numeric(simex$alpha2_init)){
				simex$alpha2_baseline<-input$alpha2
				run=TRUE
			}
			if(input$theta!=as.numeric(simex$theta_init)){
				simex$theta_baseline<-input$theta
				run=TRUE
			}
			if(input$G_d!=as.numeric(simex$G_init)){
				simex$G_baseline<-input$G_d
				run=TRUE
			}
			if(input$sigmap!=as.numeric(simex$sigmap_init)){
				simex$sigmap_baseline<-input$sigmap
				run=TRUE
			}
			if(run){
				datasimex<-simulateAB(alpha11_baseline = simex$alpha11_baseline, alpha12_baseline = simex$alpha12_baseline, alpha2_baseline = simex$alpha2_baseline,theta_baseline = simex$theta_baseline,G_baseline = simex$G_baseline,sigmap_baseline = simex$sigmap_baseline, incomeDistr = simex$incomeDistr)
			}
			#PLace 3, runs every time somtheing changes in the widgets
			myData<-as.data.frame(datasimex$baseline)
			varnames<-input$checkGroup
			matplot(myData[c(varnames)],type="l",xlab="",ylab="",lwd=2,lty=1)
			legend("topleft",col=1:length(varnames),bty='n',lwd=2,lty=1,legend=varnames)
		})
		
		output$plotscen <- renderPlot({
			run=FALSE
			runScen=FALSE
			if(input$alpha11!=as.numeric(simex$alpha11_init)){
				simex$alpha11_baseline<-input$alpha11
				run=TRUE
			}
			if(input$alpha12!=as.numeric(simex$alpha12_init)){
				simex$alpha12_baseline<-input$alpha12
				run=TRUE
			}
			if(input$alpha2!=as.numeric(simex$alpha2_init)){
				simex$alpha2_baseline<-input$alpha2
				run=TRUE
			}
			if(input$theta!=as.numeric(simex$theta_init)){
				simex$theta_baseline<-input$theta
				run=TRUE
			}
			if(input$G_d!=as.numeric(simex$G_init)){
				simex$G_baseline<-input$G_d
				run=TRUE
			}
			if(input$alpha11_scen!=as.numeric(simex$alpha11_baseline)){
				simex$alpha11_schock<-input$alpha11_scen
				simex$shock<-input$init
				runScen=TRUE
			}
			if(input$alpha12_scen!=as.numeric(simex$alpha12_baseline)){
				simex$alpha12_schock<-input$alpha12_scen
				simex$shock<-input$init
				runScen=TRUE
			}
			if(input$alpha2_scen!=as.numeric(simex$alpha2_baseline)){
				simex$alpha2_schock<-input$alpha2_scen
				simex$shock<-input$init
				runScen=TRUE
			}
			if(input$theta_scen!=as.numeric(simex$theta_baseline)){
				simex$theta_schock<-input$theta_scen
				simex$shock<-input$init
				runScen=TRUE
			}
			if(input$G_d_scen!=as.numeric(simex$G_baseline)){
				simex$G_schock<-input$G_scen
				simex$shock<-input$init
				runScen=TRUE
			}
			if(input$sigmap_scen!=as.numeric(simex$sigmap_baseline)){
				simex$sigmap_shock<-input$sigmap_scen
				simex$shock<-input$init
				runScen=TRUE
			}
			if(runScen){
				datasimex<-simulateAB(alpha11_baseline = simex$alpha11_baseline, alpha12_baseline = simex$alpha12_baseline,
															alpha2_baseline = simex$alpha2_baseline, theta_baseline = simex$theta_baseline,
															G_baseline = simex$G_baseline, sigmap_baseline = simex$sigmap_baseline,
															alpha11_shock = simex$alpha11_shock, alpha12_shock = simex$alpha12_shock,
															alpha2_shock = simex$alpha2_shock, theta_shock = simex$theta_shock,
															G_shock = simex$G_shock, shock = simex$shock, sigmap_shock = simex$sigmap_shock, 
															incomeDistr = simex$incomeDistr)
				myData<-as.data.frame(datasimex$scenario_1)
			}else{
				if(run){
					datasimex<-simulateAB(alpha11_baseline = simex$alpha11_baseline,alpha12_baseline = simex$alpha12_baseline,alpha2_baseline = simex$alpha2_baseline,theta_baseline = simex$theta_baseline,G_baseline = simex$G_baseline,sigmap_baseline = simex$sigmap_baseline,
																alpha11_shock = simex$alpha11_shock,alpha12_shock = simex$alpha12_shock,alpha2_shock = simex$alpha2_shock,theta_shock = simex$theta_shock,G_shock = simex$G_shock,shock = simex$shock,sigmap_shock = simex$sigmap_shock, incomeDistr = simex$incomeDistr)
				}
				myData<-as.data.frame(datasimex$baseline)
			}
			#PLace 3, runs every time somtheing changes in the widgets
			varnames<-input$checkGroup_scen
			matplot(rownames(myData),myData[c(varnames)],type="l",xlab="",ylab="",lwd=2,lty=1)
			legend("topleft",col=1:length(varnames),bty='n',lwd=2,lty=1,legend=varnames)
		})
	}
)