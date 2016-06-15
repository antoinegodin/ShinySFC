#Place 1 Run once
source("data/SourceCode.R")
source("data/SourceCodeAB.R")
#library(expm)
#library(Matrix)

shinyServer(
	function(input,output){
		#Place 2 Run once for every visitor of the app
		
		modelab<-{}
		modelab$phi_init = 1
		modelab$alpha11_init = 0.4
		modelab$alpha12_init = 0.25
		modelab$alpha2_init = 0.4
		modelab$theta_init = 0.2
		modelab$sigmap_init = 0.00
		modelab$sigmaw_init = 0.00
		modelab$G_init = 20
		modelab$N = 120
		modelab$nF = 100
		modelab$seed=9
		modelab$phi_baseline = 1
		modelab$alpha11_baseline = 0.4
		modelab$alpha12_baseline = 0.25
		modelab$alpha2_baseline = 0.4
		modelab$theta_baseline = 0.2
		modelab$sigmap_baseline = 0.00
		modelab$sigmaw_baseline = 0.00
		modelab$G_baseline = 20
		modelab$phi_shock = 1
		modelab$alpha11_shock = 0.4
		modelab$alpha12_shock = 0.25
		modelab$alpha2_shock = 0.4
		modelab$theta_shock = 0.2
		modelab$sigmap_shock = 0.05
		modelab$sigmaw_shock = 0.05
		modelab$G_shock = 20
		modelab$shock = 50
		modelab$incomeDistr=TRUE
		modelab$consUnit=TRUE
		
		dataab<-simulateAB()
		simex<-sfc.model("data/SIMEX.txt",modelName="SIMplest model")
		datasimex<-simulate(simex)
		#		output$sourceCode <- renderText({
		#			readLines("data/SourceCode.R")
		#		})
		
		#		output$dag <- renderPlot({
		#			plot_graph_hierarchy(graph=generate.DAG.collaspe( adjacency = t(simex$matrix) )$orginal_graph,main="SIMEX")
		#		})
		
		output$plot <- renderPlot({
			run=FALSE
			if(input$alpha11!=as.numeric(modelab$alpha11_init)){
				modelab$alpha11_baseline<-input$alpha11
				run=TRUE
			}
			if(input$alpha12!=as.numeric(modelab$alpha12_init)){
				modelab$alpha12_baseline<-input$alpha12
				run=TRUE
			}
			if(input$alpha2!=as.numeric(modelab$alpha2_init)){
				modelab$alpha2_baseline<-input$alpha2
				run=TRUE
			}
			if(input$theta!=as.numeric(modelab$theta_init)){
				modelab$theta_baseline<-input$theta
				run=TRUE
			}
			if(input$G_d!=as.numeric(modelab$G_init)){
				modelab$G_baseline<-input$G_d
				run=TRUE
			}
			if(input$sigmap!=as.numeric(modelab$sigmap_init)){
				modelab$sigmap_baseline<-input$sigmap
				run=TRUE
			}
			if(input$seed!=as.numeric(modelab$seed)){
				modelab$seed<-input$seed
				run=TRUE
			}
			if(input$N!=as.numeric(modelab$N)){
				modelab$N<-input$N
				run=TRUE
			}
			if(input$nF!=as.numeric(modelab$nF)){
				modelab$nF<-input$nF
				run=TRUE
			}
			runsimex=FALSE
			if(input$alpha1!=as.numeric(simex$variables[13,2])){
				simex<-sfc.editVar(simex,var="alpha1",init=input$alpha1)
				runsimex=TRUE
			}
			if(input$alpha2!=as.numeric(simex$variables[14,2])){
				simex<-sfc.editVar(simex,var="alpha2",init=input$alpha2)
				runsimex=TRUE
			}
			if(input$theta!=as.numeric(simex$variables[15,2])){
				simex<-sfc.editVar(simex,var="theta",init=input$theta)
				runsimex=TRUE
			}
			if(input$G_d!=as.numeric(simex$variables[16,2])){
				simex<-sfc.editVar(simex,var="G_d",init=input$G_d)
				runsimex=TRUE
			}
			if(runsimex)
				datasimex<-simulate(simex)
			if(run){
				dataab<-simulateAB(alpha11_baseline = modelab$alpha11_baseline, alpha12_baseline = modelab$alpha12_baseline, alpha2_baseline = modelab$alpha2_baseline,theta_baseline = modelab$theta_baseline,G_baseline = modelab$G_baseline,sigmap_baseline = modelab$sigmap_baseline, incomeDistr = modelab$incomeDistr, seedNb=modelab$seed, nHh = modelab$N, nFirms=modelab$nF)
			}
			#PLace 3, runs every time somtheing changes in the widgets
			myData<-as.data.frame(dataab$baseline)
			varnames<-input$checkGroup
			myDatasimex<-as.data.frame(datasimex$baseline)
			matplot(cbind(myData[c(varnames)],myDatasimex[c(varnames)]),type="l",xlab="",ylab="",lwd=2,lty=1)
			legend("topleft",col=1:(2*length(varnames)),bty='n',lwd=2,lty=1,legend=c(varnames,paste(varnames,"- SIM")))
		})
		
		output$plotscen <- renderPlot({
			run=FALSE
			runScen=FALSE
			if(input$alpha11!=as.numeric(modelab$alpha11_init)){
				modelab$alpha11_baseline<-input$alpha11
				run=TRUE
			}
			if(input$alpha12!=as.numeric(modelab$alpha12_init)){
				modelab$alpha12_baseline<-input$alpha12
				run=TRUE
			}
			if(input$alpha2!=as.numeric(modelab$alpha2_init)){
				modelab$alpha2_baseline<-input$alpha2
				run=TRUE
			}
			if(input$theta!=as.numeric(modelab$theta_init)){
				modelab$theta_baseline<-input$theta
				run=TRUE
			}
			if(input$G_d!=as.numeric(modelab$G_init)){
				modelab$G_baseline<-input$G_d
				run=TRUE
			}
			if(input$seed!=as.numeric(modelab$seed)){
				modelab$seed<-input$seed
				run=TRUE
			}
			if(input$N!=as.numeric(modelab$N)){
				modelab$N<-input$N
				run=TRUE
			}
			if(input$nF!=as.numeric(modelab$nF)){
				modelab$nF<-input$nF
				run=TRUE
			}
			if(input$sigmap!=as.numeric(modelab$sigmap_init)){
				modelab$sigmap_baseline<-input$sigmap
				run=TRUE
			}
			if(input$alpha11_scen!=as.numeric(modelab$alpha11_baseline)){
				modelab$alpha11_schock<-input$alpha11_scen
				modelab$shock<-input$init
				runScen=TRUE
			}
			if(input$alpha12_scen!=as.numeric(modelab$alpha12_baseline)){
				modelab$alpha12_schock<-input$alpha12_scen
				modelab$shock<-input$init
				runScen=TRUE
			}
			if(input$alpha2_scen!=as.numeric(modelab$alpha2_baseline)){
				modelab$alpha2_schock<-input$alpha2_scen
				modelab$shock<-input$init
				runScen=TRUE
			}
			if(input$theta_scen!=as.numeric(modelab$theta_baseline)){
				modelab$theta_schock<-input$theta_scen
				modelab$shock<-input$init
				runScen=TRUE
			}
			if(input$G_d_scen!=as.numeric(modelab$G_baseline)){
				modelab$G_schock<-input$G_scen
				modelab$shock<-input$init
				runScen=TRUE
			}
			if(input$sigmap_scen!=as.numeric(modelab$sigmap_baseline)){
				modelab$sigmap_shock<-input$sigmap_scen
				modelab$shock<-input$init
				runScen=TRUE
			}
			if(runScen){
				dataab<-simulateAB(alpha11_baseline = modelab$alpha11_baseline, alpha12_baseline = modelab$alpha12_baseline,
															alpha2_baseline = modelab$alpha2_baseline, theta_baseline = modelab$theta_baseline,
															G_baseline = modelab$G_baseline, sigmap_baseline = modelab$sigmap_baseline,
															alpha11_shock = modelab$alpha11_shock, alpha12_shock = modelab$alpha12_shock,
															alpha2_shock = modelab$alpha2_shock, theta_shock = modelab$theta_shock,
															G_shock = modelab$G_shock, shock = modelab$shock, sigmap_shock = modelab$sigmap_shock, 
															incomeDistr = modelab$incomeDistr, seedNb=modelab$seed, nHh = modelab$N, nFirms=modelab$nF)
				myData<-as.data.frame(dataab$baseline)
				myDataScen<-as.data.frame(dataab$scenario_1)
				varnames<-input$checkGroup_scen
				matplot(rownames(myData),cbind(myData[c(varnames)],myDataScen[c(varnames)]),type="l",xlab="",ylab="",lwd=2,lty=1)
				legend("topleft",col=1:(2*length(varnames)),bty='n',lwd=2,lty=1,legend=c(varnames,paste(varnames,"- SCEN")))
			}else{
				if(run){
					dataab<-simulateAB(alpha11_baseline = modelab$alpha11_baseline, alpha12_baseline = modelab$alpha12_baseline, 
														 alpha2_baseline = modelab$alpha2_baseline, theta_baseline = modelab$theta_baseline,
														 G_baseline = modelab$G_baseline, sigmap_baseline = modelab$sigmap_baseline, 
														 alpha11_shock = modelab$alpha11_shock, alpha12_shock = modelab$alpha12_shock,
														 alpha2_shock = modelab$alpha2_shock, theta_shock = modelab$theta_shock,
														 G_shock = modelab$G_shock, shock = modelab$shock, sigmap_shock = modelab$sigmap_shock, 
														 incomeDistr = modelab$incomeDistr, seedNb=modelab$seed, nHh = modelab$N, nFirms=modelab$nF)
				}
				myData<-as.data.frame(dataab$baseline)
				varnames<-input$checkGroup_scen
				matplot(rownames(myData),myData[c(varnames)],type="l",xlab="",ylab="",lwd=2,lty=1)
				legend("topleft",col=1:length(varnames),bty='n',lwd=2,lty=1,legend=varnames)
			}
			#PLace 3, runs every time somtheing changes in the widgets
			
		})
	}
)