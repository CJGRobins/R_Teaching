SEIR <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Population size based on the sizes of each of the resevoirs
    num <- s.num + e.num + i.num + r.num
    
    # Effective contact rate and force of infection (lambda = beta*i) from a rearrangement of Beta * c * D
    ce <- R0 / i.dur
    lambda <- ce * i.num/num
    
    #dX is just the rate of change of X
    dS <- -lambda*s.num #the infected people at the next time step = FOI * number of susceptibles
    dE <- lambda*s.num - (1/e.dur)*e.num #The rate of movement out of pre-infectiousness is 1/duration in days - seems simple right?
    dI <- (1/e.dur)*e.num - (1 - cfr)*(1/i.dur)*i.num - cfr*(1/i.dur)*i.num #cfr here is the death of patients,
    #if patients are dying they shouldn't be able to affect the force of infection
    dR <- (1 - cfr)*(1/i.dur)*i.num
    
    # Compartments and flows are part of the derivative vector
    # Other calculations to be output are outside the vector, but within the containing list
    list(c(dS, dE, dI, dR, 
           se.flow = lambda * s.num,
           ei.flow = (1/e.dur) * e.num,
           ir.flow = (1 - cfr)*(1/i.dur) * i.num,
           d.flow = cfr*(1/i.dur)*i.num),
         num = num,
         i.prev = i.num / num, #prevalence of infectious people
         ei.prev = (e.num + i.num)/num) #prevalence of infectED people
  })
}
