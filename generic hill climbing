#Implemented in R, written as derived from Modern Optimization With R by Paulo Cortez. The code is structured from his hill-climbing algorithm.

#maxIterations - the number of iterations you will tolerate without a change in performance
#evalFunction- the function (ie. profit) that you are evaluating your solutions on
#minormaxoption- are you trying to minimize or maximize? Takes either "min" or "max"
#reportInterval- provides a report at every nth iteration
#initialSolution- helps improve perforamnce, or with local minimum optimization problems incorporates priors
# lower / upperbounds- tells the script the thresholds to explore

hillclimber = function(evalFunction, minormaxoption, maxIterations, changeFunction, initialSolution, reportInterval, lowerbounds, upperbounds){
  
  stallCounter = 0 #this counter keeps track of how many iterations have occured without a change in solution 
  iterationsCounter = 0 # this counter keeps track of how many iterations have occured total
  currentSolution = initialSolution
  currentEvaluation = evalFunction(currentSolution)
while (stallCounter < maxIterations)    {
   nextSolution = changeFunction(currentSolution, lowerbounds, upperbounds) 
   nextEvaluation = evalFunction(nextSolution)
   
   if (reportInterval > 0 && i%%reportInterval = 0){
      cat("iteration: ", i, " current solution: ", nextSolution, " current evaluation: ", nextEvaluation)}
  
   if (minormaxoption == "min" && nextEvaluation < currentEvaluation || minormaxoption == "max" && nextEvaluation > currentEvaluation){
    currentSolution = nextSolution
    currentEvalution = nextEvaluation
    iterationsCounter = iterationsCounter + 1 }
   else {
    stallCounter = stallCounter + 1 # increment the stall counter if no progress is made
    iterationsCounter = iterationsCounter + 1
    } 
   
}   
  cat("Final solution achieved after ", iterationsCounter, " iterations. The best evaluation was ", currentEvaluation,". The best solution is ", currentSolution) 
    }
}

Hill climbing using random step algorithm- code for change function:

randomNormalStep function(learningRate, lengthVector, lowerbounds, upperbounds){
step =rnorm(lengthVector, sd = learningRate)
nextSolution = currentSolution + step}
