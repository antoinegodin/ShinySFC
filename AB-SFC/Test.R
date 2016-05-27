source("data/SourceCode.R")

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
simex$sigmap_shock = 0.14
simex$sigmaw_shock = 0.05
simex$G_shock = 20
simex$shock = 50
simex$incomeDistr=TRUE

datasimex<-simulateAB(alpha11_baseline = simex$alpha11_baseline,alpha12_baseline = simex$alpha12_baseline, 
											alpha2_baseline = simex$alpha2_baseline,theta_baseline = simex$theta_baseline,
											G_baseline = simex$G_baseline,sigmap_baseline = simex$sigmap_baseline, 
											sigmaw_baseline = simex$sigmaw_baseline, alpha11_shock = simex$alpha11_shock, 
											alpha12_shock = simex$alpha12_shock, alpha2_shock = simex$alpha2_shock,
											theta_shock = simex$theta_shock,G_shock = simex$G_shock,
											sigmap_shock = simex$sigmap_shock, sigmaw_shock = simex$sigmaw_shock,
											shock = simex$shock,incomeDistr=simex$incomeDistr)
#PLace 3, runs every time somtheing changes in the widgets
myData<-as.data.frame(datasimex$baseline)
varnames<-c("CY")
matplot(myData[c(varnames)],type="l",xlab="",ylab="",lwd=2,lty=1)
legend("topleft",col=1:length(varnames),bty='n',lwd=2,lty=1,legend=varnames)
lines(datasimex$scenario_1["CY"],col=3,lwd=2)
