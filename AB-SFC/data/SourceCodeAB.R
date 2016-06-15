# basicSFCABM
# This is a code originially written by Alberto Russo from the University of Ancona (alberto.russo@univpm.it)
#library("Matrix")

simulateAB<-function(phi_baseline = 1, alpha11_baseline = 0.8, alpha12_baseline = 0.25, alpha2_baseline = 0.4, theta_baseline = 0.2, sigmap_baseline = 0.00, sigmaw_baseline = 0.00, G_baseline = 20, phi_shock = 1, 
										 alpha11_shock = 0.8, alpha12_shock = 0.25, alpha2_shock = 0.4, theta_shock = 0.2, sigmap_shock = 0.00, sigmaw_shock = 0.00, G_shock = 10, shock = 0, incomeDistr=FALSE,
										 seedNb=9, nHh=120, nFirms=100){
	
	# time span and number of agents
	Time = 150
	if(shock>0){
		MC = 2
	}else{
		MC = 1
	}
	N = nHh
	nF = nFirms
	
	# parameters
	
	
	#Data structures
	
	results<-{}
	
	for (mc in 1:MC) {
		
		res_mc = as.data.frame(matrix(0,ncol=16,nrow=Time,dimnames = list(NULL,c("CY","G_s","C_s","CD","WB","Y","LF","UR","Q","PROF","Yd","H_s","T_s","PDef","RATIONING","RATLIQ"))))
		
		# initial conditions
		p = matrix(data = 1, ncol = nF)
		lf = matrix(data = 0, ncol = nF)
		wb = matrix(data = 0, ncol = nF)
		w = matrix(data = 1, ncol = N)
		pHh = matrix(data = 1, ncol = N)
		woff = matrix(data = 1, ncol = nF)
		y = matrix(data = 0, ncol = N)
		yd = matrix(data = 0, ncol = N)
		c = matrix(data = 0, ncol = N)
		h = matrix(data = 0, ncol = N)
		unemp = matrix(data = 0, ncol = N)
		rev = matrix(data = 0, ncol = nF)
		
		set.seed(seedNb)
		
		# main program
		for (t in 1:Time) {
			
			#Updating parameters if a shock is present
			
			if (mc==1 | (shock>0 & t <= shock)) {
				phi = phi_baseline
				cy = alpha11_baseline-alpha12_baseline*yd
				ch = alpha2_baseline
				theta = theta_baseline
				sigmap = sigmap_baseline
				sigmaw = sigmaw_baseline
				Gexog = G_baseline
			} else {
				phi = phi_shock
				cy = alpha11_shock-alpha12_shock*yd
				ch = alpha2_shock
				theta = theta_shock
				sigmap = sigmap_shock
				sigmaw = sigmaw_shock
				Gexog = G_shock
			}
			
			res_mc$CY[t] = mean(cy)
			
			#Setting up government expenditures
			res_mc$G_s[t] = Gexog
			
			# firms update price, by default rnorm (random extraction from normal distribution) has mean = 0 and sd = 1, by default, all prices are set at 1.
			if(incomeDistr){
				p = 1 * (1 + sigmap * matrix(rnorm(nF),ncol = nF))
				#If prices are too low, set to a minimal value
				# p[p<0.001] = 0.001
				# Wage offered is such that there are no profits for the firms
				woff = p/phi
			}
			
			# H decide the expected nominal demand ...
			cd = cy * yd/pHh + ch * h/pHh
			# We assume that households can only consume in units, because of that, rationing exists
			c = floor(cd)

			rationing = cd - c
			res_mc$RATIONING[t]=sum(rationing)
			res_mc$CD[t]=sum(cd)
			
			# H look at firms' prices randomly (uniform distribution, default value over the [0,1] interval)
			indpf = matrix(NA,ncol=N)
			for(i in 1:N){
				indTempPf = sample(nF,5)
				indpf[i]<-indTempPf[which.min(p[indTempPf])]
				pHh[i]<-p[indpf[i]]
				# given the liquidity constraint
				rationing[i] = c[i]-pmin(h[i]/pHh[i],c[i])
				c[i] = pmin(h[i]/pHh[i],c[i])
			}
			
			res_mc$RATLIQ[t]=sum(rationing)
			res_mc$C_s[t] = sum(c*pHh)
	
			
			# F gather expected orders from H and G,decide production and labour demand
			# Assumption: G is spread uniformly accross firms.
			# Random order of firms
			rev = matrix(data = 0, ncol = nF)
			vecf = sample(nF)
			Gtemp=res_mc$G_s[t]
			for(f in 1:nF){
				indf=vecf[f]
				rev[indf]=p[indf]
				Gtemp=Gtemp-1
				if(Gtemp==0)
					break
			}
			
			# Here firms collects the consumption decision of households
			for (j in 1:nF) {
				rev[j] = rev[j] + sum(c[indpf==j]*p[j])
			}
			# Quantity is equal to nominal demand divided by prices
			qexp = rev / p
			
			#Qexp[t, mc] = sum(qexp)
			# Labor demand to produce these goods is equal to output divided by poductivity
			ld = ceiling(qexp / phi)
			#LD[t, mc] = sum(ld)
			
			# F hire workers based on ...
			# ... a random matching 
			# Random order of housholds
			vecw = sample(N)
			# Random order of firms
			vecf = sample(nF)
			# All households are unemployed
			unemp[1:N] = 1
			# Labor force per firm
			lf[1:nF] = 0
			# Wage bill per firm
			wb[1:nF] = 0
			# income per household
			y[1:N] = 0
			# How much money is available to pay the workers
			cash = rev
			# going over the households
			for (j in 1:N) {
				# Geting the randmoly ordered household
				indw = vecw[j]
				# Finding the firms that search for workers and have enough cash to pay for the wage
				vecf = which(lf < ld & cash >= woff)
				if (length(vecf) > 0) {
					if(incomeDistr){
						# if income distribution, take the highest wage
						indf<-vecf[which.max(woff[vecf])]
					}else{
						# If there is more than one, chose one randomly (uniform distribution to extract)
						indf1 = round(runif(1,min=1,max=length(vecf)))
						indf = vecf[indf1]
					}
					# Updating that firm's labor force
					lf[indf] = lf[indf] + 1
					# Setting the status of that worker to employed.
					unemp[indw] = 0
					# That houshold has an income equal to its wage
					w[indw] = woff[indf]
					y[indw] = w[indw]
					# The wage bill of the firm is updated accordingly
					wb[indf] = wb[indf] + w[indw]
					# And the cash available is reduced as well
					cash[indf] = cash[indf]-w[indw]
				}
			} 
			# Computing the aggregates
			res_mc$WB[t] = sum(wb)
			res_mc$Y[t] = sum(y)
			res_mc$LF[t] = sum(lf)
			res_mc$UR[t] = (N - res_mc$LF[t]) / N
			
			# F produce goods, calculate profits
			q = phi * lf
			res_mc$Q[t] = sum(q)
			# Profits are either equal to the sales minus the wage bill
			prof = rev - wb;
			res_mc$PROF[t] = sum(prof)
			
			# H pay taxes and update wealth
			yd = (1 - theta) * y
			res_mc$Yd[t] = sum(yd)
			res_mc$T_s[t] = sum(theta * y)
			h = h + yd - c
			res_mc$H_s[t] = sum(h)
			
			# Public deficit
			res_mc$PDef[t] = res_mc$G_s[t] - res_mc$T_s[t]
			
		} # end of main program
		
		#wealth[1:N,mc] = h
		
		if(mc==1)
			results$baseline<-res_mc
		else
			results$scenario_1<-res_mc
		
	} # end of MC simulation
	return(results)
}