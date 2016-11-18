

	dataset1<-"/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /AdaBoost Project/test0-1.txt"	

	

	

ada<-function(dataset1){
	
	install.packages("plyr")
	install.packages("sqldf")
	library("plyr")
	library(sqldf)
	
	
 	d1<-as.data.frame(d1)
 	
 	#t: no. of iterations
 	
 	t<-d1[1,1]
 	
 	#n: no. of examples
 	
 	n<-d1[1,2]
 	
 	#eps: epsilon
 	
 	eps<-d1[1,3]
 	
 	#x: examples
 	
 	x<-d1[2,]
 	rownames(x)<-1
 	
 	
 	#y: labels
 	
 	y<-d1[3,]
 	
 	rownames(y)<-1
 	
 	#p0: initial probabilities
 	#p: probabilities after each iteration
 	
 	p<-matrix(0,t+1,n)
 	
 	for(i in 1:n){
 		
 		p[1,i]<-d1[4,i]
 		
 	}
 	
 	
 	
 	rownames(p0)<-1
 	
 	#error1: errors for every threshold: -1 on right and 1 on left
 	
 	error1<-matrix(0,t,n+1)
 	
 	#error2: errors for every threshold: 1 on right and -1 on left
 	
 	error2<-matrix(0,t,n+1)
 	
 	#errorfinal: holding the minimum error of each iteration
 	
 	errorfinal<-matrix(0,t,1)
 	
 	#error: holding the final error
 	
 	#thresholds: n+1
 	
 	# h: array of selected classifiers
 	
 	h<-matrix(0,t,1)
 	
 	#symbol: holds 1 or 2: 1 = '<' and 2 = '>'
 	
 	symbol<-matrix(0,t,1)
 	
 	#minerr: array of minimum errors in each iteration
 	
 	minerr<-matrix(0,t,1)
 	
 	#alpha: array of alphas in each iteration
 	
 	alpha<-matrix(0,t,1)
 	
 	#q: array of Qs e to the power of alphas
 	
 	q<-matrix(0,t,n)
 	
 	#indexerr1: array of indexes that are wrong in error1
 	
 	indexerr1<-c()
 	
 	#indexerr2: array of indexes that are wrong in error2
 	
 	indexerr2<-c()
 	
 	
 	
 	
 	#z: normalization factor
 	
 	z<-matrix(0,t,1)
 	
 	#f: boosted classifier
 	
 	f<-matrix(0,t,n)
 	
 	#ferror: f error
 	
 	ferror<-matrix(0,t,1)
 	
 	bound<-matrix(0,t,1)
 	
 	# for loop for 10 iterations
 	
 	for(i in 1:t){
 		
 		for(j in 1:(n+1)){
 			
 			if(j == 1){
 				
 				for(k in 1:n){
 					
 					#threshold condition below for -1 on right and 1 on left
 					
 					if((x[k] > 0 ) && (y[k] == 1 )){
 						
 						error1[i,j]<-error1[i,j] + p[i,k]
 						
 						indexerr1<-c(indexerr1,k)
 						
 					}
 					
 					#threshold condition below for 1 on right and -1 on left
 					
 					if((x[k] > 0 ) && (y[k] == -1 )){
 						
 						error2[i,j]<-error2[i,j] + p[i,k]
 						
 						indexerr2<-c(indexerr2,k)
 						
 					}	
 					
 				}
 				
 			}
 			
 			else if(j==(n+1)){
 				
 				for(k in 1:n){
 					
 					#threshold condition below for -1 on right and 1 on left

 					if((x[k] < 1000 ) && (y[k] != 1 )){
 						
 						error1[i,j]<-error1[i,j] + p[i,k]
 						
 						indexerr1<-c(indexerr1,k)
 						
 					}
 					
 					#threshold condition below for 1 on right and -1 on left
 					
 					if((x[k] < 1000 ) && (y[k] == 1 )){
 						
 						error2[i,j]<-error2[i,j] + p[i,k]
 						
 						indexerr2<-c(indexerr2,k)
 						
 					}
 					
 				}
 				
 			}
 			
 			else {
 				
 				for(k in 1:n){
 					
 					#threshold condition below for -1 on right and 1 on left
 					
 					if((x[k] < x[j] ) && (y[k] != 1 )){
 						
 						error1[i,j]<-error1[i,j] + p[i,k]
 						
 						indexerr1<-c(indexerr1,k)
 						
 					}
 					
 					else if((x[k] >= x[j]) && (y[k] == 1)){
 						
 						error2[i,j]<-error2[i,j] + p[i,k]
 						
 						indexerr2<-c(indexerr2,k)	
 						
 					}
 					
 					#threshold condition below for 1 on right and -1 on left
 					
 					if((x[k] < x[j] ) && (y[k] == 1 )){
 						
 						error1[i,j]<-error1[i,j] + p[i,k]
 						
 						indexerr1<-c(indexerr,k)
 						
 					}
 					
 					else if((x[k] >= x[j]) && (y[k] != 1)){
 						
 						error2[i,j]<-error2[i,j] + p[i,k]
 						
 						indexerr2<-c(indexerr,k)	
 						
 					}
 					
 					
 				}
 					
 			}
 			
 			
 				
 		}
 		
 		#j ends here 
 		
 		
 		errorfinal[i,1]<-min(min(error1[i,]),min(error2[i,]))
 		
 		symbol[i,1]<-which.min(min(error1[i,]),min(error2[i,]))
 		
 		if(symbol[i,1]==1){
 			
 			h[i,1]<-x[which.min(error1[i,]),1]
 			
 		}
 		else if(symbol[i,1]==2){
 			
 			h[i,1]<-x[which.min(error2[i,]),1]
 			
 		}
 		
 		alpha[i,1]<-(1/2)*(log((1-errorfinal[i,1])/errorfinal[i,1])/log(2.718))
 		
 		
 		#calculating q
 		
 		for(l in 1: n){
 			
 			# x < 2.5 condition
 			
 			if(symbol[i,1] == 1){
 				
 				if( ((x[l] < h[i,1]) && (y[l] == 1)) || ( (x[l] >= h[i,1]) && (y[l] != 1)) ){
 				
 					q[i,l]<-2.718^(-1*errorfinal[i,1])
 					
 				
 				
 				}
 				else{
 				
 					q[i,l]<-2.718^(errorfinal[i,1])
 				
 				}
 				
 				
 			}
 			
 			# x > 2.5 condition
 			
 			else if(symbol[i,1] == 2){
 				
 				if( ((x[l] < h[i,1]) && (y[l] != 1)) || ( (x[l] >= h[i,1]) && (y[l] == 1)) ){
 				
 					q[i,l]<-2.718^(-1*errorfinal[i,1])
 				
 				}
 				else {
 				
 					q[i,l]<-2.718^(errorfinal[i,1])
 				
 				}
 				
 				
 			}
 			
 			
 		}
 		
 		#calculating normalization factor
 		
 		for(j in 1:n){
 			
 			z[i,1]<-z[i,1] + p[i,j]*q[i,j]
 			
 		}
 		
 		#calculating new p
 		
 		for(j in 1:n){
 			
 			p[i+1,j]<-(p[i,j]*q[i,j])/z[i,1]
 			
 		}
 		
 		#making value of indexerr to 0
 		
 		indexerr1<-c()
 		indexerr2<-c()

 		#calculating f
 		
 		for(k in 1:n){
 		
 			for(j in 1:i){
 			
 				if(symbol[i,1] == 1){
 					
 					if(x[k] < h[i,1]){
 					
 						f[i,k]<-f[i,k] + alpha[i,1]*1
 						
 					
 					}
 					else if(x[k] >= h[i,1]){
 						
 						f[i,k]<-f[i,k] + alpha[i,1]*(-1)
 						
 					}
 		
 				}
 				
 				else if(symbol[i,1] == 2){
 					
 					if(x[k] < h[i,1]){
 					
 						f[i,k]<-f[i,k] + alpha[i,1]*(-1)
 						
 					
 					}
 					else if(x[k] >= h[i,1]){
 						
 						f[i,k]<-f[i,k] + alpha[i,1]*1
 						
 					}
 		
 				}
 		
 			}
 			
 		}
 		# f calculation ends here
 		
 		#error calculation of f starts here
 		
 		#ferror: error by wrong classification by f	
 		
 		for(k in 1:n){
 			
 			if(sign(f[i,k]!=sign(y[k]))){
 				
 				ferror[i,1]<-ferror[i,1] + 1 
 				
 			}
 			
 		}
 		
 		ferror[i,1]<-ferror[i,1]/n
 		
 		bound[i,1]<-bound[i,1]*z[i,1]


 		
 	}
 	
	# Binary ada boost finish here
	#
	#
	#
	#
	#
	#
	#
	#
 	# Real ada boosting starts here
 	
 	
 	#p1[0,]: initial probabilities
 	#p1: probabilities after each iteration
 	
 	p1<-matrix(0,t+1,n)
 	
 	for(i in 1:n){
 		
 		p1[1,i]<-d1[4,i]
 		
 	}
 	
 	#h1: matrix for holding classifiers
 	
 	h1<-matrix(0,t,1)
 	
 	# the 4 P's
 	
 	#ppr1: p plus right : summation of previous p for h=1 y=1
 	
 	ppr1<-matrix(0,t,n+1)
 	
 	#pmr1: p minus right : summation of previous p for h=-1 y=-1
 	
 	pmr1<-matrix(0,t,n+1)
 	
 	#ppw1: p plus wrong : summation of previous p for h=-1 y=1
 	
 	ppw1<-matrix(0,t,n+1)
 	
 	#pmw1: p minus wrong : summation of previous p for h=1 y=-1
 	
 	pmw1<-matrix(0,t,n+1)
 	
 	
 	#ppr2: p plus right : summation of previous p for h=1 y=1
 	
 	ppr2<-matrix(0,t,n+1)
 	
 	#pmr2: p minus right : summation of previous p for h=-1 y=-1
 	
 	pmr2<-matrix(0,t,n+1)
 	
 	#ppw2: p plus wrong : summation of previous p for h=-1 y=1
 	
 	ppw2<-matrix(0,t,n+1)
 	
 	#pmw2: p minus wrong : summation of previous p for h=1 y=-1
 	
 	pmw2<-matrix(0,t,n+1)
 	
 	#G: under root product and sum formula 
 	
 	G1<-matrix(0,t,n+1)
 	G2<-matrix(0,t,n+1)
 	
 	Gfinal1<-matrix(0,t,1)
 	Gfinal2<-matrix(0,t,1)
 	
 	Gfinal<-matrix(0,t,1)
 	
 	#symbol1: holds symbol of Gfinal
 	
 	symbol1<-matrix(0,t,1)
 	
 	#cp, cm
 	
 	cp<-matrix(0,t,1)
 	
 	cm<-matrix(0,t,1)
 	
 	#g
 	
 	g<-matrix(0,t,n)
 	
 	#ferror: f error
 	
 	ferror1<-matrix(0,t,1)
 	
 	bound1<-matrix(0,t,1)
 	
 
 	for(i in 1:t){
 		
 		for(j in 1:(n+1)){
 			
 			if(j == 1){
 				
 				for(k in 1:n){
 					
 					#threshold condition below for -1 on right and 1 on left
 					
 					#basically ppw
 					
 					if((x[k] > 0 ) && (y[k] == 1 )){
 						
 						ppw1[i,j]<-ppw1[i,j] + p1[i,k]
 						
 					}
 					
 					#basically pmr
 					
 					else if((x[k] > 0 ) && (y[k] == -1 )){
 						
 						pmr1[i,j]<-pmr1[i,j] + p1[i,k]
 						
 					}
 					
 					#threshold condition below for 1 on right and -1 on left
 					
 					if((x[k] > 0 ) && (y[k] == 1 )){
 						
 						ppr2[i,j]<-ppr2[i,j] + p1[i,k]
 						
 					}	
 					else if((x[k] > 0 ) && (y[k] == -1 )){
 						
 						pmw2[i,j]<-pmw2[i,j] + p1[i,k]
 						
 					}
 					
 				}
 				
 			}
 			
 			else if(j==(n+1)){
 				
 				for(k in 1:n){
 					
 					#threshold condition below for -1 on right and 1 on left

 					if((x[k] < 1000 ) && (y[k] == -1 )){
 						
 						pmw1[i,j]<-pmw1[i,j] + p1[i,k]
 						
 						
 					}
 					else if((x[k] < 1000 ) && (y[k] == 1 )){
 						
 						ppr1[i,j]<-ppr1[i,j] + p1[i,k]
 						
 					}
 					
 					#threshold condition below for 1 on right and -1 on left
 					
 					if((x[k] < 1000 ) && (y[k] == 1 )){
 						
 						ppw2[i,j]<-ppw2[i,j] + p1[i,j]
 						
 					}
 					else if((x[k] < 1000 ) && (y[k] == -1 )){
 						
 						pmr2[i,j]<-ppw2[i,j] + p1[i,j]
 					
 					}
 					
 				}
 				
 			}
 			
 			else {
 				
 				for(k in 1:n){
 					
 					#threshold condition below for -1 on right and 1 on left
 					
 					if((x[k] < x[j] ) && (y[k] == -1)){
 						
 						pmw1[i,j]<-pmw1[i,j] + p1[i,k]
 						
 					}
 					else if((x[k] < x[j] ) && (y[k] == 1)){
 						
 						ppr1[i,j]<-ppr1[i,j] + p1[i,k]
 					
 					}
 					else if((x[k] >= x[j]) && (y[k] == 1)){
 						
 						ppw1[i,j]<-ppw1[i,j] + p1[i,k]
 						
 					}
 					else if((x[k] >= x[j]) && (y[k] == -1)){
 						
 						pmr1[i,j]<-pmr1[i,j] + p1[i,k]
 						
 					}
 					
 					#threshold condition below for 1 on right and -1 on left
 					
 					if((x[k] < x[j] ) && (y[k] == 1 )){
 						
 						ppw2[i,j]<-ppw2[i,j] + p1[i,k]
 						
 					}
 					else if((x[k] < x[j] ) && (y[k] == -1 )){
 						
 						pmr2[i,j]<-pmr2[i,j] + p1[i,k]
 						
 					}
 					else if((x[k] >= x[j]) && (y[k] == -1)){
 						
 						pmw2[i,j]<-pmw2[i,j] + p1[i,k]
 						
 					}
 					else if((x[k] >= x[j]) && (y[k] == 1)){
 						
 						ppr2[i,j]<-ppr2[i,j] + p1[i,k]
 						
 					}
 					
 				}
 					
 			}
 			
 			G1[i,j]<-sqrt(ppr1[i,j]*pmw1[i,j]) + sqrt(ppw1[i,j]*pmr1[i,j])
 			G2[i,j]<-sqrt(ppr2[i,j]*pmw2[i,j]) + sqrt(ppw2[i,j]*pmr2[i,j])
 			
 		}
 		#j ends here 	
		
		Gfinal1[i,1]<-min(G1[i,])
		Gfinal2[i,1]<-min(G2[i,])
		
		Gfinal[i,1]<-min(Gfinal1[i,1],Gfinal[i,1])
		
		#symbol1
		
		symbol1[i,1]<-which.min(Gfinal1[i,1],Gfinal[i,1])
		
		#calculating h
		
		if(symbol1[i,1]==1){
			
			if(which.min(G1[i,]) == n+1){
				
				h1[i,1]<-1000
				
			}
 			else{
 				
 				h1[i,1]<-x[which.min(G1[i,]),1]
 			}
 		}
 		else if(symbol[i,1]==2){
 			
 			if(which.min(G2[i,]) == 1){
 				
 				h1[i,1]<-0
 				
 			}
 			else{
 				
 				h1[i,1]<-x[which.min(G2[i,])-1,1]
 			
 			}
 		}

		# calculating cp, cm
		
		k = which.min(G1[i,])
		
		if(symbol1[i,1]==1){
			
			cp[i,1]<- 0.5*log((ppr1[i,k] + eps)/(pmw1[i,k] + eps))/log(2.718)
			cm[i,1]<- 0.5*log((ppw1[i,k] + eps)/(pmr1[i,k] + eps))/log(2.718)

		}
		
		else if	(symbol1[i,1]==1){
			
			cp[i,1]<- 0.5*log((ppr2[i,k] + eps)/(pmw2[i,k] + eps))/log(2.718)
			cm[i,1]<- 0.5*log((ppw2[i,k] + eps)/(pmr2[i,k] + eps))/log(2.718)
			
			
		}
		
		# calculating g
		
		if(symbol1[i,1]==1){
			
			for(k in 1:n ){
				
				if(x[k] < h[i,1]){
					
					g[i,k]<-cp[i,1]
					
				}
				else{
					
					g[i,k]<-cm[i,1]
					
				}
				
			}
			
		else if(symbol1[i,1]==2){
			
				for(k in 1:n ){
				
				if(x[k] >= h[i,1]){
					
					g[i,k]<-cp[i,1]
					
				}
				else{
					
					g[i,k]<-cm[i,1]
					
				}
			
			
			}

		}
		
		#calculate z1
		
		#calculating normalization factor
 		
 		for(j in 1:n){
 			
 			z1[i,1]<-z1[i,1] + p1[i,j]*(2.718^((-1)*y[i,1]*g[i,j]))
 			
 		}
 		
 		#calculating new p
 		
 		for(j in 1:n){
 			
 			p1[i+1,j]<-(p1[i,j]*(2.718^((-1)*y[i,1]*g[i,j])))/z[i,1]
 			
 		}

		
		#calculating f1
 		
 		for(k in 1:n){
 			
 			if(symbol1[i,1] == 1){
 					
 				if(x[k] < h1[i,1]){
 				
 					f1[i,k]<-f1[i,k] + cp[i,1]
 						
 					
 				}
 				else if(x[k] >= h[i,1]){
 						
 					f1[i,k]<-f1[i,k] + cm[i,1]
 						
 				}
 		
 			}
 				
 			else if(symbol1[i,1] == 2){
 					
 				if(x[i] < h[i,1]){
 					
 					f1[i,k]<-f1[i,k] + cm[i,1]
 						
 					
 				}
 				else if(x[k] >= h[i,1]){
 						
 					f1[i,k]<-f1[i,k] + cp[i,1]
 						
 				}
 		
 			}
 		
 		}
 			
 	
 		# f1 calculation ends here
 		
 		#error calculation of f1 starts here
 		
 		#ferror1: error by wrong classification by f	1
 		
 		for(k in 1:n){
 			
 			if(sign(f1[i,k]!=sign(y[k]))){
 				
 				ferror1[i,1]<-ferror1[i,1] + 1 
 				
 			}
 			
 		}
 		
 		ferror1[i,1]<-ferror1[i,1]/n
 		
 		bound1[i,1]<-bound1[i,1]*z1[i,1]

		
				
	}
	
}









