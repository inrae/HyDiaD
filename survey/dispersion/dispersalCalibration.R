# reference points
dispersal = data.frame(D = c(40.47, 255.51), pAfter = c(.5,.1))
dispersal$pBefore = 1 - dispersal$pAfter

# calibration by linear regression
lm1 = lm(log(-log(pAfter))~log(D), data = dispersal)

# lead to the same values as those found by Excel solver
(alpha = unname(exp(lm1$coefficients[1])))
(beta = unname(lm1$coefficients[2]))


# the kernel function
 disperse = function(D, alpha, beta){
   return(exp(-alpha * (D^beta)))
 }
 
 # compute the propotion  to settle before a distance
 # based on the integrale of the disperse function....
 settleBefore = function(D, alpha, beta) {
   pPeudoInf = 10000 * pgamma( (10000^beta)*alpha, 1/beta)  / (beta * ((10000^beta * alpha)^(1 / beta)))
   res = (D * pgamma( (D^beta)*alpha, 1/beta)  / (beta * ((D^beta * alpha)^(1 / beta)))) / pPeudoInf
   res[D == 0] = 0
   return(res)
 }
 
 # sum of squared error
 SCE = function(par, obsDispersal){
   return(sum((settleBefore(obsDispersal$D, par[1], par[2]) - obsDispersal$pBefore)^2))
 }
 
 sol = optim(c(0.06, .652), SCE, obsDispersal = dispersal)
sol$par

D = 0:1000
p = disperse(D, alpha, beta)

plot(D, disperse(D, alpha, beta), type = 'l')
points(dispersal$D, dispersal$pAfter, col = 'red')


plot(D, settleBefore(D, sol$par[1], sol$par[2]) , type = 'l', yaxp= c(0,1,10), )
# proortion that 
cbind(dispersal$D, settleBefore(dispersal$D, alpha, beta))



sol = optim(c(0.06, .652), SCE, obsDispersal = dispersal)
sol$par
settleBefore(dispersal$D, sol$par[1], sol$par[2] )
proportion = settleBefore(D, alpha, beta) 
plot(D, settleBefore(D, alpha, beta) , type = 'l',  panel.first = grid())
abline(v = dispersal$D, col = 'red')

plot(D, settleBefore(D, sol$par[1], sol$par[2]) , type = 'l', yaxp= c(0,1,10), )


abline(v = dispersal$D, col = 'red')
