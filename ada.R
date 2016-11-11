

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
 	
 	#error: errors for every threshold
 	
 	error<-matrix(0,t,n+1)
 	
 	#errorfinal: holding the minimum error of each iteration
 	
 	errorfinal<-matrix(0,t,1)
 	
 	#thresholds: n+1
 	
 	# h: array of selected classifiers
 	
 	h<-matrix(0,t,1)
 	
 	#minerr: array of minimum errors in each iteration
 	
 	minerr<-matrix(0,t,1)
 	
 	#alpha: array of alphas in each iteration
 	
 	alpha<-matrix(0,t,1)
 	
 	#q: array of Qs e to the power of alphas
 	
 	q<-matrix(0,t,n)
 	
 	#indexerr: array of indexes that are wrong
 	
 	indexerr<-c()
 	
 	
 	#z: normalization factor
 	
 	z<-matrix(0,t,1)
 	
 	
 	# for loop for 10 iterations
 	
 	for(i in 1:t){
 		
 		for(j in 1:(n+1)){
 			
 			if(j == 1){
 				
 				for(k in 1:n){
 					
 					#threshold condition below
 					
 					if((x[k] > 0 ) && (y[k] == 1 )){
 						
 						error[i,j]<-error[i,j] + p[i,k]
 						
 						indexerr<-c(indexerr,k)
 						
 					}
 				}
 			}
 			
 			else if(j==(n+1)){
 				
 				for(k in 1:n){
 					
 					#threshold condition below
 					
 					if((x[k] < 1000 ) && (y[k] != 1 )){
 						
 						error[i,j]<-error[i,j] + p[i,k]
 						
 						indexerr<-c(indexerr,k)
 						
 					}
 					
 				}
 				
 			}
 			
 			else {
 				
 				for(k in 1:n){
 					
 					#threshold condition below
 					
 					if((x[k] < x[j] ) && (y[k] != 1 )){
 						
 						error[i,j]<-error[i,j] + p[i,k]
 						
 						indexerr<-c(indexerr,k)
 						
 					}
 					
 					else if((x[k] >= x[j]) && (y[k] == 1)){
 						
 						error[i,j]<-error[i,j] + p[i,k]
 						
 						indexerr<-c(indexerr,k)

 						
 						
 					}
 					
 				}
 					
 			}
 			
 			
 				
 		}
 		
 		#j ends here 
 		
 		h[i,1]<-x[which.min(a),1]
 		
 		errorfinal[i,1]<-min(error[i,])
 		
 		alpha[i,1]<-(1/2)*(log((1-errorfinal[i,1])/errorfinal[i,1])/log(2.718))
 		
 		
 		#calculating q
 		
 		for(l in 1: n){
 			
 			if( l %in% indexerr){
 				
 				q[i,l]<-2.718^(errorfinal[i,1])
 				
 			}
 			else {
 				q[i,l]<-2.718^(-errorfinal[i,1])
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
 		
 		indexerr<-c()
 		
 		
 	}	
	
}