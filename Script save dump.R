#-------------------------------------------------------------------------------
#SAVED SCRIPT FROM INDEX.QMD SAVED BECAUSE I MIGHT NEED IT
#POTENTIALLY FIX AND READD AFTER FIXING FORMATTING
#-------------------------------------------------------------------------------

```{r}

#Attempt at microcosm programming for the sake of later inclusion
#in the overall model

#first need to identify st1, st2, st3
#cannot take as given
#need to calculate additional terms
#Beta is given by A.bar

#Does gamma come in anywhere under a different name?
#Lecture slides use S and ν for IW values as well as M, Σ, P
#Paper uses ψ and d for IW values as well as b, Σ, Ω

#Directions to derive St seem unclear
#Theta is (s1, s2, s3, rho)
#is it just p(gamma, theta|y)
#But how is p(y|gamma, theta) derived?
#IT seems to depend on already having st
#Does it mean the prior of st? 

#set new yt based on given st
#curent y lacks date
#rebind date to verify
#There must be a better way to select the appropriate dates
#I just don't know offhand



#set up for Metropolis-Hastings
S = 1000

#create c and w for covariance
c = 0.0001
W = diag(4)
p=4
#create empty variables for accepted and iteration count
accept = 0
itter = 1

omegaS = rep(0, 4)
omegaco = c*W

#is s just equal to omegastar

#write separate function for posterior kernal
#throw into for loop below for pomega

#write function for evaluating st
#generate theta vector from candidate generating density
#use theta vector to generate st
tildecalc()

#run this later using an artificial s

#Create function for generating p(y|Omega)

#create function for generating tilde values
#dataset, variance shift, lags

#This is verified to work up until xtilde
#must manually reset t for some reason
tildecalc = function(){
  #generate ytilde
  
  #get length of y
  obs = nrow(y)
  
  #Initialize matrix with all attributes of y to overwrite
  ytilde = y
  
  #set up for loop to recalculate each row of y
  #I am doing this because I do not recall if division of matrix by vector works
  #so better to do a manual method I know works
  for (i in 1:obs) {
    
    ytilde[i,]=y[i,]/s[i]
    
  } 
  #save new ytilde for future, keep old one to calculate xtilde
  ytilde2 = ytilde[5:176,]
  
  #generate xtilde
  #create new length variable
  obsn = obs - p
  
  #initialize empty matrices
  
  xtildet = matrix(nrow = ncol(y),ncol = p+1)
  
  #xtilde is an array given that it is each set of xtildet
  xtilde = array(dim = c(ncol(y),p+1,obsn))
  
  xtildet[,1] = 1
  #create t variable to track current timeperiod
  t = 1+p
  
  #populate xtilde
  for (i in 1:obsn){
    xtildet[,1] = 1
    
    
    #update xtildet for new period based on number of lags
    for (j in 1:p){
      #Use loop count to select aprropriate rows of y
      #lcount + p - j ensures rows populate in proper order
      
      #this returns out of bounds error because yes
      #it seems like it doesn't properly reset t before checkingthis
      #The code works if I manually reset t then run the loop
      xtildet[,j+1] = t(y[t-j,])
    }
    
    
    #perform math to make xtildet be correct
    xtildet = xtildet/s[p+i]
    #now fill in xtilde with the val
    
    xtilde[,,i] = xtildet
    #increment current time
    t = t + 1  
  }
  #is transposing an array even necessary? It's an array
  t = 1+p
  
  
  #Create a transpose of array xtilde
  xtildetrans = ta(xtilde)
  
  btildehat = (xtildetrans%*%xtilde + solve(omega)) %*% (xtildetrans%*%ytilde2 + solve(omega)%*%bhat)
  
  epsiltildehat = ytilde2 - xtilde%*%btildehat
}
#I need to find some 

#try for loop version
for (i in 1:S) {
  #prior density
  rhostar = dnorm(0,0.5)
  s = dpareto()
  
  
  #candidate generating density
  omegastar = mvrnorm(1, omegaS, omegaco)
  
  
  pomega = det(diag(s)) ^ /2 * #p(omega) Ask prof about this, I can't quite read handwriting
    u = runif(0,1)
  alphaS = min(pomega,1)
  
  
  if (u < alpahaS){
    omegaS = omegastar
    #save kernal of omegaS
    accept = accept + 1
  } else {
    omegaS = omegaS
    #save kernal of omegaS
  }
  itteration = itteration + 1
  
  
  #calculate rejection rate
  rrate = 1 - accept/itteration
  
  #do I actually need this or can I hand-calc it above?
  coda::rejectionRate()
  
  
  
  
  
  
  rho = rnorm(mean = 0, sd = 0.5)
  
  s = rep(1,160)
  
  #initialize s's
  s0 = 1
  s1 = 2
  s2 = 3
  #initialize empty rho vector
  rhos = as.matrix(rep(0,9))
  rhos[1,]
  #for loop to find all variances
  for (i in 1:9){
    #can chage j-2 to just i since i is j-2 already
    #itterates through each time period to calculate new value based on s2 and rho
    rhos[i,] = 1 + (s2-1) * rho ^ i
  }
  
  #extend this to include all future values determined by rho
  s = c(s, s0, s1, s2, rhos)
  sdia = diag(s)
  #Check back later
  #may not need to actually use these.
  y = cbind(urate[,1], y)
  yt1 = y[165,2:7]/s1
  yt2 = y[166,2:7]/s2
  yt3 = y[167,2:7]/s3
  
  #reinsert these rows back into the original y matrix
  #only separate for verification purposes here to ensure math was done correctly
  #directly give correct output in final code
  
  
  #should be able to construct a for loop of some kind
  #try to do that later
  
  
  #generate Xt
  
  
  #set new Xt based on given st
  
  #regenerate Beta and Sigma using MLE of new values
  A.hat       = solve(t(X)%*%sdia%*%X)%*%t(X)%*%sdia%*%Y
  Sigma.hat   = t(Y-X%*%A.hat)%*%(Y-X%*%A.hat)/nrow(Y)
  
  #This is all just given and can be reused infinitely
  V.bar.inv   = t(X)%*%diag(1/s)%*%X + diag(1/diag(V.prior))
  V.bar       = solve(V.bar.inv)
  A.bar       = V.bar%*%(t(X)%*%diag(1/s)%*%Y + diag(1/diag(V.prior))%*%A.prior)
  nu.bar      = nrow(Y) + nu.prior
  S.bar       = S.prior + t(Y)%*%diag(1/s)%*%Y + t(A.prior)%*%diag(1/diag(V.prior))%*%A.prior - t(A.bar)%*%V.bar.inv%*%A.bar
  S.bar.inv   = solve(S.bar)
  ```