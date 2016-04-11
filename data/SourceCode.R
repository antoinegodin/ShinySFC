sfc.model <-function(fileName,dataFile=NA,modelName="SFCmodel",fill=F){
  options(warn=-1)

  model<-sfc.create(modelName)

  if(!is.na(dataFile)){
    data <- read.csv(dataFile)
    variablesName <- names(data)
    for(i in 2:length(data[1,])){
      model<-sfc.addVar(model,var=variablesName[i],init=data[1,i],desc="")
    }
    model<-sfc.setYear(model,as.numeric(data[1,1]),as.numeric(data[length(data[,1]),1]))
  }


  options(warn=-1)
  modelFile <- file(fileName)
  modelText <- readLines(modelFile, n = -1)
  close(modelFile)
  firstEquation=TRUE

  for (i in 1:length(modelText)) {
    lineText = modelText[i]
    if (!grepl("#", lineText)) {
      if(grepl("timeline", lineText)){
        myTable = strsplit(lineText, " ")
        initYears=myTable[[1]][2]
        endYears=myTable[[1]][3]
        model<-sfc.setYear(model,initYears,endYears)
      }else{
        myTable = strsplit(lineText, "=")

        #This is to manage the case when there are other = in the equation (use of logical operators)
        if(length(myTable[[1]])>2){
          tempStr<-myTable[[1]][2]
          for(iter in 3:length(myTable[[1]])){
            tempStr<-paste(tempStr,myTable[[1]][3],sep="=")
            myTable[[1]]=myTable[[1]][-3]
          }
          myTable[[1]][2]=tempStr
        }
        #Replacing all reseverved words, for now only in-> inv
        nameEnd = myTable[[1]][1]
        equation = myTable[[1]][2]
        equation<-trim(equation)
        value<-as.double(equation)
        if(!is.na(value)){
          nameEndSearch = gsub("\\bin\\b","inv",nameEnd)
          nameEndSearch<-trim(nameEndSearch)
          indEnd<-sfc.getIndex(model,end=nameEndSearch)
          if(indEnd>0){
            model<-sfc.editEnd(model,ind=indEnd,init=value)
          }else{
            indVar<-sfc.getIndex(model,var=nameEndSearch)
            if(indVar>0){
              model<-sfc.editVar(model,ind=indEnd,init=value)
            }else{
              if(grepl("#",modelText[i-1])){
                display=substring(modelText[i-1],2)
              }else{
                display=""
              }
              model<-sfc.addVar(model,var=nameEnd,init=value,desc=display)
            }
          }
        }else{
          if(grepl("#",modelText[i-1])){
            displayEquation=substring(modelText[i-1],2)
          }else{
            displayEquation=""
          }
          model<-sfc.addEqu(model,nameEnd,equation,displayEquation)
        }
      }
    }
  }

  model<-sfc.check(model,fill=fill)

  options(warn=0)
  return(model)
}

sfc.create<-function(modelName=stop("Need a model name")){
	model<-{}
	model$name<-modelName
	model$simulated<-FALSE
	class(model)<-"sfc"
	return(model)
}

trim<-function(text){
	return(gsub("[[:space:]]","",text))
}

sfc.addEqu<-function(model=stop("Need a model"),var=stop("Need a variable name!"),eq=stop("Need an equation!"),desc=""){
	
	variable = gsub("\\bin\\b","inv",var)
	variable<-trim(variable)
	equation = gsub("\\bin\\b","inv",eq)
	equation<-trim(equation)
	#replacing all lags. This might be an issue if there are a lag of 2 but not of 1.
	lag=1
	while(grepl(paste("(-",lag,")",sep=""),equation)){
		equation=gsub(paste("(-",lag,")",sep=""), paste("_",lag,sep=""),equation , fixed = T)
		lag=lag+1
	}
	
	if(is.null(model$equations)){
		equations<-matrix(nrow=1,ncol=3,dimnames=list(NULL,c("endogenous value","equation","description")))
		equations[1,1]<-variable
		equations[1,2]<-equation
		equations[1,3]<-desc
	}else{
		equations<-model$equations
		equations<-rbind(equations,c(variable,equation,desc))
	}
	ind=sfc.getIndex(model,end=variable)
	if(ind<0){
		model<-sfc.addEnd(model,var=var,init=NA,lag=0,desc=desc)
	}
	model$equations<-equations
	return(model)
}

sfc.getIndex<-function(model=stop("Need a model"),var=NA,eq=NA,end=NA){
	if(!is.na(var)){
		ind = which(model$variables[,1]==var,arr.ind=T)
		if(length(ind)==0){ind=-1}
		return(ind)
	}else if(!is.na(eq)){
		ind = which(model$equations[,1]==eq,arr.ind=T)
		if(length(ind)==0){ind=-1}
		return(ind)
	}else if(!is.na(end)){
		ind = which(model$endogenous[,1]==end,arr.ind=T)	
		if(length(ind)==0){ind=-1}
		return(ind)
	}else{
		stop("Need either a variable (var), and endogenous (end) or an equation (eq)!")
	}
}

sfc.addEnd<-function(model=stop("Need a model"),var=stop("Need a variable name!"),init=NA,lag=0,desc=NA){
	
	variable = gsub("\\bin\\b","inv",var)
	variable<-trim(variable)
	
	if(is.null(model$endogenous)){
		endogenous<-matrix(nrow=1,ncol=4,dimnames=list(NULL,c("name","initial value","lag","description")))
		endogenous[1,1]<-variable
		endogenous[1,2]<-init
		endogenous[1,3]<-lag
		endogenous[1,4]<-desc
	}else{
		endogenous<-model$endogenous
		endogenous<-rbind(endogenous,c(variable,init,lag,desc))
	}
	ind=sfc.getIndex(model,var=variable)
	if(ind<0){
		model<-sfc.addVar(model,var=var,init=NA,desc=desc)
	}
	
	model$endogenous<-endogenous
	return(model)
}

sfc.addVar<-function(model=stop("Need a model"),var=stop("Need a variable name!"),init="NA",desc=""){
	var = gsub("\\bin\\b","inv",var)
	var<-trim(var)
	
	if(is.null(model$variables)){
		variables<-matrix(nrow=1,ncol=3,dimnames=list(NULL,c("name","initial value","description")))
		variables[1,1]<-var
		variables[1,2]<-init
		variables[1,3]<-desc
	}else{
		variables<-model$variables
		variables<-rbind(variables,c(var,init,desc))
	}
	model$variables<-variables
	return(model)
}

sfc.editEnd<-function(model=stop("Need a model"),ind=NA,var=NA,init=NA,lag=NA,desc=NA){
	if(is.na(ind)&&is.na(var)){
		stop("Need an index or a varname")
	}
	if(is.null(model$endogenous)){
		stop("The model doesn't have endogenous variables yet")
	}else{
		if(is.na(ind)){
			ind=sfc.getIndex(model,end=var)
			if(ind<0){
				stop("The model does not contain the endogenous variable ",var)
			}
		}
		if(!is.na(init)){model$endogenous[ind,2]<-init}
		if(!is.na(lag)){model$endogenous[ind,3]<-lag}
		if(!is.na(desc)){model$endogenous[ind,4]<-desc}
		indVar<-sfc.getIndex(model,var=model$endogenous[ind,1])
		if(indVar>0&&(!is.na(desc)||!is.na(init))){
			model<-sfc.editVar(model,ind=indVar,init=init,desc=desc)
		}
	}
	return(model)
}

sfc.editVar<-function(model=stop("Need a model"),ind=NA,var=NA,init=NA,desc=NA){
	if(is.na(ind)&&is.na(var)){
		stop("Need an index or a varname")
	}
	if(is.null(model$variables)){
		stop("The model doesn't have variables yet")
	}else{
		if(is.na(ind)){
			ind=sfc.getIndex(model,var=var)
			if(ind<0){
				stop("The model does not contain a variable ", var)
			}
		}
		if(!is.na(init)){model$variables[ind,2]<-init}
		if(!is.na(desc)){model$variables[ind,3]<-desc}
	}
	return(model)
}

sfc.setYear<-function(model=stop("Need a model"),init=stop("Need an initial year"),end=stop("Need and ending year")){
	model$time<-c(init:end)
	return(model)
}

sfc.check<-function(model=stop("Need a model"),fill=F){
	if(is.null(model$equations)){stop("The model doesn't contain any equations")}
	if(is.null(model$endogenous)){warning("Need to define endogenous variables for the model")}
	if(is.null(model$variables)){warning("Need to define variables for the model")}
	if(length(model$endogenous[,1])!=length(model$equations[,1])){
		warning("Number of equations not equal to number of endogenous variables, check the model!")
		return(model)
	}
	#This part deals with the value of variables that depends on other variables values
	notDefined = matrix(nrow = 1, ncol = 4)
	firstNotDefined = TRUE
	firstVar = TRUE
	varStr = "varTemp <- list(\n"
	for(i in 1:length(model$variables[,1])){
		if(!is.na(model$variables[i, 2])){
			value = as.double(model$variables[i, 2])
			if (is.na(value)) {
				if (firstNotDefined) {
					notDefined[1, 1] = model$variables[i, 1]
					notDefined[1, 2] = model$variables[i, 2]
					notDefined[1, 3] = model$variables[i, 3]
					firstNotDefined = FALSE
				} else {
					notDefined = rbind(notDefined, c(model$variables[i, 1], model$variables[i, 2], model$variables[i, 3]))
				}
			} else {
				if (firstVar) {
					varStr <- paste(varStr, model$variables[i, 1], "=", model$variables[i, 2], sep = "")
					firstVar = FALSE
				} else {
					varStr <- paste(varStr, ",", model$variables[i, 1], "=", model$variables[i, 2], sep = "")
				}
			}
		}
	}
	varStr2 <- paste(varStr, ")", sep = "")
	eval(parse(text = varStr2))
	textUndefined = "value<- eval(substitute(substitute(var, varTemp), list(var =quote("
	i=1
	while(!is.na(notDefined[1,1])&&i<=length(notDefined[, 1])) {
		textUndefined2 = paste(textUndefined, notDefined[i, 2], "))))", sep = "")
		eval(parse(text = textUndefined2))
		options(show.error.messages = FALSE)
		value<-try(eval(value))
		options(show.error.messages = TRUE)
		if(!is.double(value)){
			row=notDefined[i,]
			notDefined=notDefined[-i,]
			notDefined=rbind(notDefined,c(row[1],row[2],row[3],row[4]))
			i=i-1
		}else{
			indVar=sfc.getIndex(model,var=notDefined[i,1])
			indEnd=sfc.getIndex(model,end=notDefined[i,1])
			model<-sfc.editVar(model,ind=indVar,init=value)
			if(indEnd>0){model<-sfc.editEnd(model,ind=indEnd,init=value)}
			varStr <- paste(varStr, ",", notDefined[i, 1], "=", eval(value), sep = "")
			varStr2 <- paste(varStr, ")", sep = "")
			eval(parse(text = varStr2))
		}
		i=i+1
	}
	#This section deals with the lags
	for (j in 1:length(model$endogenous[,1])) {
		for (i in 1:length(model$equations[,1])) {
			temp <- gsub("[ \t\n\r\f\v()/\\+\\<\\>\\*-]+"," ",paste(" ",model$equations[i,2]))
			ind <- regexpr(paste("([ \t\n\r\f\v])",model$endogenous[j, 1],"_([0-9])",sep=""), temp)
			while (ind[1] > -1) {
				temp <- substring(temp, ind[1] + nchar(model$endogenous[j, 1])+1)
				if (nchar(temp) > 0 && substr(temp, 0, 1) == "_") {
					value <-as.integer(substr(temp,2,3))
					if (!is.na(value)) {
						model<-sfc.editEnd(model,ind=j,lag=max(model$endogenous[j,3],value))
					}
				}
				ind <- regexpr(paste("([ \t\n\r\f\v])",model$endogenous[j, 1],"_([0-9])",sep=""), temp)
			}
		}
	}#end for (j in 1:length(endogenous[,1]))
	ind <- which(model$endogenous[, 3] > 0&is.na(model$endogenous[, 2]))
	if(length(ind)>0){
		for(i in 1:length(ind)){
			indVar<-sfc.getIndex(model,var=model$endogenous[ind[i],1])
			model$endogenous[ind[i],2]<-model$variables[indVar,2]
		}
	}
	ind <- which(model$endogenous[, 3] > 0&is.na(model$endogenous[, 2]))
	if(length(ind)>0){
		if(fill){
			for(i in 1:length(ind)){
				cat("Initial value for",model$endogenous[ind[i],1],"[0]?")
				ans = scan(what=double(),nlines=1,quiet=TRUE)
				if(length(ans)==0){ans=0}
				model$endogenous[ind[i],2]<-ans
				indVar<-sfc.getIndex(model,var=model$endogenous[ind[i],1])
				model$variables[indVar,2]<-ans
			}
		}else{
			warning("The following variables have lags but no initial values:",paste(" - ",model$endogenous[ind,1]))
		}
	}
	#this computes the matrix containing the relations between endogenous values
	equEndMatrix = matrix(data = 0, nrow = length(model$endogenous[, 1]), ncol = length(model$equations[, 1]),dimnames=list(c(model$endogenous[,1]),c(model$equations[,1])))
	for (j in 1:length(model$endogenous[, 1])) {
		ind <- which(model$equations[, 1] == model$endogenous[j, 1])
		temp <- gsub("[ \t\n\r\f\v()/\\<\\>\\+\\*-]+", " ", model$equations[ind, 2])
		for (i in 1:length(model$endogenous[, 1])) {
			ind2 <- regexpr(paste("([ \t\n\r\f\v]|^)", model$endogenous[i, 1], "([ \t\n\r\f\v]|$)", sep = ""), temp)
			if (length(ind2)>0&&ind2 > -1) {
				equEndMatrix[ind[1], i] = 1
			}
		}
	}
	#this creates the block of independant equations.
	#TODO: If there are no equations with no independencies, all equations left are treated as one block
	matrix<-equEndMatrix
	blocks<-{}
	total<-matrix(data=0,nrow=length(matrix[,1]),ncol=1)
	for(i in 1:length(matrix[,1])){
		total[i]=sum(matrix[i,])
	}
	indexes<-sort.int(total,index.return=T)
	maxBlocks=max(indexes$x)
	alreadyFound=c()
	iter=1
	while(length(alreadyFound)!=length(matrix[,1])){
		blockIndex=c()
		eqsInd<-indexes$ix[which(indexes$x==0)]
		if(length(eqsInd)>0){
			for(j in 1:length(eqsInd)){
				if(length(which(alreadyFound==eqsInd[j]))==0){
					matrix[,eqsInd[j]]=0
					alreadyFound=c(alreadyFound,eqsInd[j])
					blockIndex=c(blockIndex,eqsInd[j])
				}
			}
		}
		if(length(blockIndex)==0){
			eqsInd<-indexes$ix[which(indexes$x>0)]
			for(j in 1:length(eqsInd)){
				if(length(which(alreadyFound==eqsInd[j]))==0){
					matrix[,eqsInd[j]]=0
					alreadyFound=c(alreadyFound,eqsInd[j])
					blockIndex=c(blockIndex,eqsInd[j])
				}
			}
		}
		if(!is.list(blocks)){
			blocks=as.list(blocks)	
		}
		blocks[[iter]]<-blockIndex
		total<-matrix(data=0,nrow=length(matrix[,1]),ncol=1)
		for(i in 1:length(matrix[,1])){
			total[i]=sum(matrix[i,])
		}
		indexes<-sort.int(total,index.return=T)
		iter=iter+1
	}
	eval(parse(text=sfc.eval(model,check=T)))
	errEq=c()
	for(i in 1:length(equations)){
		eq <- eval(substitute(substitute(var, prev), list(var = equations[[i]])))
		eq <- eval(substitute(substitute(var, variables), list(var = eq)))		
		value=NULL
		try(value <- eval(eq),silent=TRUE)
		if(is.null(value)){
			#			errEq=c(errEq,equations[[i]])
			errEq<-c(errEq,eq)
		}
	}
	if(fill&length(errEq)>0){
		exit=FALSE
		while(length(errEq)>0&&!exit){	
			cat("One or more exogenous variables are not defined in the following equations ",paste(errEq,"\n")," do you want to insert these manually [Yes]/No?")
			ans = scan(what=character(),nlines=1,quiet=TRUE)
			if(length(ans)==0||ans=="Yes"){
				while(length(ans)==0||ans=="Yes"){
					cat("Insert name, value and description (return after each value)")
					ans = scan(what=character(),nlines=3,quiet=TRUE)
					name=ans[1]
					value=as.double(ans[2])
					desc=""
					for(i in 3:length(ans)){
						desc=paste(desc,ans[i],sep=" ")
					}
					model<-sfc.addVar(model,var=name,init=value,desc=desc)
					cat("Are there other variables to add [Yes]/No?")
					ans = scan(what=character(),nlines=1,quiet=TRUE)
					if(length(ans)>0&&ans=="No"){exit=TRUE}
				}
				eval(parse(text=sfc.eval(model,check=T)))
				errEq=c()
				for(i in 1:length(equations)){
					eq <- eval(substitute(substitute(var, prev), list(var = equations[[i]])))
					eq <- eval(substitute(substitute(var, variables), list(var = eq)))		
					value=NA
					try(value <- eval(eq),silent=TRUE)
					if(is.na(value)){
						errEq=c(errEq,equations[[i]])
					}
				}
			}else{
				warning("Equations ",paste("\n",errEq),"\ncontain variables that are not defined, check the model")
			}
		}
	}else if(length(errEq)>0){
		warning("Equations ",paste("\n",errEq),"\ncontain variables that are not defined, check the model")
	}
	
	if(is.null(model$time)){
		if(fill){
			cat("Years are not set, do you want to inser these manually [Yes]/No?")
			ans = scan(what=character(),nlines=1,quiet=TRUE)
			if(length(ans)==0||ans=="Yes"){
				cat("Insert initial period and final period (return after each value)")
				ans = scan(what=double(),nlines=2,quiet=TRUE)
				model<-sfc.setYear(model,init=ans[1],end=ans[2])
			}else{
				warning("No year defined")
			}
		}else{
			warning("No year defined")
		}
	}
	
	#creation of the result
	model$matrix<-equEndMatrix
	model$blocks<-blocks
	return(model)
	
}

sfc.eval <- function(object=stop("Need a model to evaluate"),check=F) {
	options(warn=-1)
	interLine="\n"
	
	if(!is.sfc(object)){
		stop("The object to be evaluated is not an SFC model")
	}else{	
		
		
		varStr = "variables <- list(\n"
		endVarStr<-")\n"
		firstVar=TRUE
		variables<-object$variables
		for (i in 1:length(variables[, 1])) {
			value = as.double(variables[i, 2])
			if(firstVar){
				varStr <- paste(varStr,variables[i, 1],"=",variables[i, 2],sep = "")
				firstVar=FALSE
			}else{
				varStr <- paste(varStr,",\n",variables[i, 1],"=",variables[i, 2],sep = "")
			}
		}
		varStr <- paste(varStr,")\n", sep = "")
		
		if(!check){
			endogenous = object$endog
			firstPrev=TRUE
			prevStr = "prev <- list(\n"
			for (i in 1:length(endogenous[, 1])) {
				value = as.double(endogenous[i, 2])
				if(endogenous[i,3]>0){
					for(l in 1:endogenous[i,3]){
						if(firstPrev){
							prevStr <- paste(prevStr, endogenous[i, 1], "_",l, "=", endogenous[i, 2], sep = "")
							firstPrev=FALSE
						}else{
							prevStr <- paste(prevStr,",",interLine, endogenous[i, 1], "_",l, "=", endogenous[i, 2], sep = "")
						}
					}
				}
			}
			prevStr <- paste(prevStr,interLine, ")\n", sep = "")
			
			equations = object$equations
			blocks=object$blocks
			eqStr = "equations <- list(\n"
			blockStr<-"c(\n"
			intraEqStr = " = quote("
			endEqStr = "),\n"
			lastEqStr = ")\n"
			endBlockStr = "),\n"
			lastBlockStr = ")\n)\n"
			for(j in 1:length(blocks)){
				indexBlock=blocks[[j]]
				eqStr=paste(eqStr,blockStr,sep="")
				for (i in 1:length(indexBlock)) {
					equ=object$equations[indexBlock[i], 2]
					if (i == length(indexBlock)) {
						eqStr = paste(eqStr, equations[indexBlock[i], 1], intraEqStr, equ , lastEqStr, sep = "")
					} else {
						eqStr = paste(eqStr, equations[indexBlock[i], 1], intraEqStr, equ , endEqStr, sep = "")
					}
				}
				if(j==length(blocks)){
					eqStr=paste(eqStr,lastBlockStr,sep="")
				}else{
					eqStr=paste(eqStr,endBlockStr,sep="")
				}
			}
			
			blockStr<-"blocks<-list(\n"
			innerBlockStr<-"c("
			endInnerBlockStr<-"),\n"
			lastInnerBlockStr<-")\n)\n"
			for(i in 1:length(blocks)){
				innerBlock=blocks[[i]]
				blockStr<-paste(blockStr,innerBlockStr,sep="")
				for(j in 1:length(innerBlock)){
					if(j==length(innerBlock)){
						blockStr<-paste(blockStr,innerBlock[j],sep="")
					}else{
						blockStr<-paste(blockStr,innerBlock[j],",",sep="")
					}
				}
				if(i==length(blocks)){
					blockStr<-paste(blockStr,lastInnerBlockStr,sep="")
				}else{
					blockStr<-paste(blockStr,endInnerBlockStr,sep="")
				}
				
			}
		}else{
			endogenous = object$endog
			firstPrev=TRUE
			prevStr = "prev <- list(\n"
			for (i in 1:length(endogenous[, 1])) {
				value = 0
				if(firstPrev){
					prevStr <- paste(prevStr, endogenous[i, 1],"=", value, sep = "")
					firstPrev=FALSE
				}else{
					prevStr <- paste(prevStr,",",interLine, endogenous[i, 1], "=", value, sep = "")
				}
				if(endogenous[i,3]>0){
					for(l in 1:endogenous[i,3]){
						prevStr <- paste(prevStr,",",interLine, endogenous[i, 1], "_",l, "=", endogenous[i, 2], sep = "")
					}
				}
			}
			prevStr <- paste(prevStr,interLine, ")\n", sep = "")
			
			equations = object$equations
			eqStr = "equations <- list(\n"
			intraEqStr = " = quote("
			endEqStr = "),\n"
			lastEqStr = "))\n"
			for(j in 1:length(equations[,1])){
				if(j==length(equations[,1])){
					eqStr = paste(eqStr, equations[j, 1], intraEqStr, equations[j, 2],lastEqStr, sep = "")
				}else{
					eqStr = paste(eqStr, equations[j, 1], intraEqStr, equations[j, 2],endEqStr, sep = "")
				}
			}
			blockStr=""
		}
	}
	options(warn=0)
	return(paste(c(eqStr,blockStr,varStr,prevStr),sep=""))
}

is.sfc <- function(x) inherits(x,"sfc")

simulate.sfc <- function(model, tolValue = 1e-10, maxIter=10000) {
	eval(parse(text=sfc.eval(model)))
	variablesMat<-matrix(data=0,nrow=length(model$time),ncol=length(model$variables[,1]),dimnames=list(c(model$time),c(model$variables[,1])))
	for(i in 1:length(model$variables[,1])){
		variablesMat[,i]=as.double(model$variables[i,2])
	}
	baseline<-sfc.runScenario(model,variablesMat,tolValue,maxIter,equations,blocks,variables,prev)
	result<-list(baseline=baseline)
	if(!is.null(model$scenarios)){
		for(i in 1:length(model$scenarios)){
			variablesMatTemp<-variablesMat
			scen<-model$scenarios[[i]]
			if(sum(is.na(scen$start))!=length(scen$start)){
				for(j in 1:length(names(scen$start))){
					if(!grepl("block",names(scen$start)[j])){
						variablesMatTemp[as.character(seq(from=model$time[1],to=scen$init)),names(scen$start)[j]]=as.double(scen$start[j])	
					}
				}
			}
			for(j in 1:length(scen$var)){
				variablesMatTemp[as.character(seq(from=scen$init,to=scen$end)),scen$var[j]]=as.double(scen$value[j])
			}
			scenName<-paste("scenario_",i,sep="")
			result[[scenName]]<-sfc.runScenario(model,variablesMatTemp,tolValue,maxIter,equations,blocks,variables,prev)
		}
	}
	
	return(result)
}

sfc.GaussSeidel <- function(eqs, tol,maxIter, initialValues, prev) {
	vars <- vector(mode = "double", length = length(eqs))
	tols <- vector(mode = "double", length = length(eqs))
	tols[] <- tol
	vars[] <- Inf
	init <- initialValues
	iter<-0
	endogName <- names(eqs)
	matrixToPrint=matrix(0,nrow=1,ncol=length(init))
	for (j in 1:maxIter) {
		newinit <- init
		for (i in 1:length(eqs)) {
			eq <- eval(substitute(substitute(var, init), list(var = eqs[[i]])))
			eq <- eval(substitute(substitute(var, prev), list(var = eq)))
			newinit[endogName[i]] <- eval(eq)
			vars[i] <- abs((newinit[[endogName[i]]] - init[[endogName[i]]])/init[[endogName[i]]])
			if (is.na(vars[i])) {
				vars[i] <- 0
			}
		}
		matrixToPrint<-rbind(matrixToPrint,t(as.matrix(newinit)))
		init <- newinit
		iter<-j
		if (all(vars < tols)) {
			break
		}
	}
	res<-{}
	res$values<-init
	res$iterations<-iter
	return(res)
}

sfc.runScenario<-function(model,data,tolValue,maxIter,equations,blocks,variables,prev){
	iterations <- length(model$time)
	GSiterations<-matrix(0,nrow=iterations,ncol=length(equations),dimnames=list(model$time,paste("iter block",seq(1:length(equations)))))
	
	for (t in 2:iterations) {
		
		for(i in 1:length(model$variables[,1])){
			varName<-model$variables[i,1]
			if(is.na(data[t-1,varName])){
				variables[varName]=1
			}else{
				variables[varName]=data[t-1,varName]
			}
		}
		for(i in 1:length(model$endog[,3])){
			if(model$endog[i,3]>0){
				for(j in 1:model$endog[i,3]){
					varName<-model$endog[i,1]
					prevName=paste(varName,"_",j,sep="")
					lag<-t-j
					if(lag<1){
						lag<-1
					}
					prev[prevName]=data[lag,varName]
				}
			}
		}
		for (b in 1:length(equations)) {
			blockOfEquations <- equations[[b]]
			varNames<-names(blockOfEquations)
			resultGS<-sfc.GaussSeidel(blockOfEquations, tolValue,maxIter,variables,prev)
			values<-resultGS$values
			GSiterations[t,b]<-resultGS$iterations
			for(i in 1:length(blockOfEquations)){
				#				print(variables[varNames[i]])
				variables[varNames[i]]<-values[[varNames[i]]]
				#				print(variables[varNames[i]])
				data[t,varNames[i]]=values[[varNames[i]]]
			}
			
		}
	}	
	
	data<-cbind(data,GSiterations)
	return(data)
}

sfc.addScenario <- function(model=stop("Need a model"),vars=stop("Need variables"),values=stop("Need values"),inits=stop("Need starting values"),ends=stop("Need ending values"),starts=NA){
	if(length(vars)!=length(values)&&length(vars)!=length(inits)&&length(vars)!=length(ends)){
		stop("Check vars, values, inits and ends")
	}else{
		if(is.null(model$scenarios)){
			scenario<-{}
			j=0
		}
		else{
			scenario<-model$scenarios
			j=length(scenario)
		}
		for(i in 1:length(vars)){
			scen<-{}
			scen$var=vars[[i]]
			scen$value=values[[i]]
			scen$init=inits[i]-1
			scen$end=ends[i]
			scen$start=starts
			scenario[[i+j]]<-scen
		}
	}
	model$scenarios<-scenario
	return(model)
}

plot_graph_hierarchy = function(graph,...){
	if( require(Rgraphviz) ){
		graph_nel = igraph.to.graphNEL(graph)
		graph_attributes = makeNodeAttrs(g = graph_nel,label = unlist(V(graph)$name),fillcolor = V(graph)$color)
		plot(graph_nel, nodeAttrs = graph_attributes,...)  
	}else{
		stop( "Rgraphviz could not be loaded. Please install it to use this function." )
	}
	
}

generate.DAG.collaspe = function(adjacency){
	# function will return the resulting DAG graph by collasping problem SCC nodes into one (these nodes will)
	# also be returned
	
	g = graph.adjacency(adjacency) # generte the graph object from the adj matrix
	offending_nodes = V(g)[ find.loop.nodes(adjacency)]$name # find the nodes part of SCC
	
	V(g)$color = "lightgreen" # color all nodes green
	V(g)[ V(g)$name %in% V(g)[ find.loop.nodes(adjacency) ]$name ]$color = "pink" # any nodes that is part of a SCC
	# will be pink
	
	# find and plot the SCC graph
	SCC_graph = induced.subgraph(graph = g,vids = V(g)[ find.loop.nodes(adjacency) ]$name)
	
	# create a vector that will mapall the vertex's that are part of the SCC to the first node in the SCC
	SCC_comp = table( clusters(graph = g,mode = "strong")$membership ) # find out how many nodes are in each strongly connected
	
	vertex_seq = 1:length(V(g)) # this will do the mapping part, for the condensatation graph
	
	nodes_to_be_mapped = as.numeric( names(SCC_comp[ SCC_comp > 1 ]) ) # pull the names of nodes that are greater than one
	for( q in nodes_to_be_mapped ){
		# so now want to run throught these and map their vector to the vector sequence nodes
		vertex_seq[ clusters(graph = g,mode = "strong")$membership == q ] = q
	}
	
	# genereate the graph object from the above mapping
	g_DAG = contract.vertices( graph = g, mapping = vertex_seq, vertex.attr.comb = list(name = "first", color = "first"))
	g_DAG = simplify(graph = g_DAG) # remove multiple edges from nodes
	g_DAG = induced.subgraph( graph = g_DAG,vids =  V(g_DAG)[ igraph:::degree(g_DAG)> 0] ) # more housekeeping: remove nodes
	# no degree
	
	if(is.DAG(g_DAG, IGRAPH = TRUE) == TRUE){
		print("We now have a DAG to work with!")
		results = list()
		results$orginal_graph = g
		results$SCC_graph = SCC_graph
		results$DAG = g_DAG
		return(results)
	}else{print("woops! Somethings after going wrong, function did not results in a DAG!")}
}
