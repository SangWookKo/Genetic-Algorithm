# Genetic-Algorithm

**Genetic Algorithm is a search heuristic. GAs can generate a vast number of possible model solutions and use these to evlove towards an approximation of the best solution of the model. Hereby it mimics evolution in nature.

**GA generates a population, the individuals in this population(often called chromosomes) have a given states. Once the population is generated, the state of these individuals is evaluated and graded on their value. The best individuals are then taken and crossed-over -in order to hopefully generate 'better' offspring - to form the new population. In some cases the best individuals in the population are preserved in order to gurantee 'good individuals' in the new generation (this is called *elitism*)

## To explain the example I will use my version of the *Knapsack probelm*.

| You are going to spend a month in the wilderness. You're taking a backpack with you, however, the maximum weight it can carry is 20  |
| kilograms. You have a number of survival items available, each with its own number of "survival points". You're objective is to maxi-| 
| -mize the number of survival points.                                                                                                 |



Item            |   SurvivalPoints  |   Weight
                |                   |    
pocketknife     |       10.00       |    1.00
beans           |       20.00       |    5.00
potatoes        |       15.00       |   10.00
unions          |        2.00       |    1.00
sleeping bag    |       30.00       |    7.00
rope            |       10.00       |    5.00
compass         |       30.00       |    1.00


## In R I have used the package *genalg* to set-up the model. Later on, ggplot2 will be used to visualize the evolution of the model.

## Let's define the dataset and weight constraint;

ibrary(genalg)
library(ggplot2)

dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "unions","sleeping bag", "rope", "compass"), survivalpoints = c(10, 20, 15, 2, 30, 10, 30), weight = c(1, 5, 10, 1, 7, 5, 1))
weightlimit <- 20

## Before creating the model we have to set-up an evaluation function. The evaluation function will evaluate the different individuals
## (chromosomes) of the population on the value of their gene configuration.

## An individual can for example have the following gene configuration: *1001100* .
## Each number in this binary string represents whether or not to take an item with you. A value of 1 refers to putting the specific item in the knapsack while a 0 refers to leave the item at home. Given the example gene configuration we would take the following items;

chromosome = c(1, 0, 0, 1, 1, 0, 0)
dataset[ choromosome ==1, ]

We can check to what amount of survival points this configuration sums up.

cat(chromosome %*% dataset$survivalpoints)

>>> result
## 42

## Above we gave a value to the gene configuration of a given chromosome. This is exactly what the evaluation function does.

## The *genalg* algorithm tries to optimize towards the minimum value. Therefore, the value is calculated as above and multiplied with -1. A configuration which leads to exceeding the weight constraint returns a value of 0(a higher value can also be given).

## We define the evaluation function as follows.

evalFunc <- function(x) {
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
    if (current_solution_weight > weightlimit)
      return(0)
    else
      return(-current_solution_survivalpoints)

}

## Next, we choose the number of iterations, design and run the model.

iter = 100
GAmodel <- rbga.bin(size = 7, popSize = 200, iters = iter, mutationChance = 0.01, elitism = TRUE, evalFunc = evalFunc)
cat(summary.rbga(GAmodel)) ### There is some error

### To solve *summary.rbga* error, the following codes are suggested.

summary.rbga <- function(object, echo=FALSE, ...) {
  
  # should do object type checking here
  
  rbga.object = object
  
  
  
  output = paste(
    
    "GA Settings", "\n",
    
    "  Type                  = ", rbga.object$type, "\n",
    
    "  Population size       = ", rbga.object$popSize, "\n",
    
    "  Number of Generations = ", rbga.object$iters, "\n",
    
    "  Elitism               = ", rbga.object$elitism, "\n",
    
    "  Mutation Chance       = ", rbga.object$mutationChance, "\n",
    
    "\n",
    
    "Search Domain", "\n", sep="")
  
  for (var in 1:length(rbga.object$stringMin)) {
    
    minVar = rbga.object$stringMin[var]
    
    maxVar = rbga.object$stringMax[var]
    
    output = paste(output,
                   
                   "  Var ", var, " = [", minVar, ",", maxVar, "]\n", sep="");
    
  }
  
  output = paste(output, "\n", sep="");
  
  if (!is.null(rbga.object$suggestions)) {
    
    optionPart = paste("Suggestions", "\n", sep="");
    
    for (suggestion in 1:dim(rbga.object$suggestions)[1]) {
      
      optionPart = paste(optionPart,
                         
                         "  ", suggestion, " = (", sep="");
      
      for (var in 1:(dim(rbga.object$suggestions)[2]-1)) {
        
        optionPart = paste(optionPart,
                           
                           rbga.object$suggestions[suggestion,var], ", ",
                           
                           sep="");
        
      }
      
      # and the last one
      
      optionPart = paste(optionPart,
                         
                         rbga.object$suggestions[suggestion,dim(rbga.object$suggestions)[2]],
                         
                         ")\n", sep="");
      
    }
    
    output = paste(output, optionPart, "\n", sep="");
    
  }
  
  if (!is.null(rbga.object$population)) {
    
    optionPart = paste("GA Results", "\n", "  Best Solution : ", sep="");
    
    # ok, deal with the situation that more than one object is best
    
    filter = rbga.object$evaluations == min(rbga.object$evaluations);
    
    
    
    bestSolution = rbga.object$population[filter, , drop= FALSE][1,];  
    
    # drop= FALSE: needed if one dimension is 1 (vars)
    
    # this statement takes all cases
    
    
    
    for (var in 1:length(bestSolution)) {
      
      optionPart = paste(optionPart,
                         
                         bestSolution[var], " ",
                         
                         sep="");
      
    }
    
    output = paste(output, optionPart, "\n", sep="");
    
  }
  
  if (echo) cat(output);
  
  invisible(output);
  
}



## The best solution is found to be 1111101. This leads us to take the following items with us on our trip into the wild.

solution = c(1,1,1,1,1,0,1)

dataset[solution ==1, ]


## This in turn gives us the total number of survival points.

## solution VS available
  cat(paste(solution %*% dataset$survivalpoints, "/" , sum(dataset$survivalpoints)))
  
## Let's visualize how the model evolves.

animate_plot <- function(x) {
  for (i in seq(1, iter)) {
    temp <- data.frame(Generation = c(seq(1, i), seq(1, i)), Variable = c(rep("mean", 
                                                                              i), rep("best", i)), Survivalpoints = c(-GAmodel$mean[1:i], -GAmodel$best[1:i]))
    
    pl <- ggplot(temp, aes(x = Generation, y = Survivalpoints, group = Variable,
              colour = Variable)) + geom_line() + scale_x_continuous(limits = c(0,iter)) + scale_y_continuous(limits = c(0, 110)) +
              geom_line(y = max(temp$Survivalpoints), lty = 2) + annotate("text", x = 1, y = max(temp$Survivalpoints) + 
              2, hjust = 0, size = 3, color = "black", label = paste("Best solution:", 
              max(temp$Survivalpoints))) + scale_colour_brewer(palette = "Set1") + 
              labs(title = "Evolution Knapsack optimization model")
    
    print(pl)
  }
}


## The x-axis denotes the different generations. The blue line shows the mean solution of the entire population of that generation, while the red line shows the best solution of that generation. As you can see, it takes the model only a few generations to hit the best solution, after that it takes the model only a few generations to hit the best solution, after that it is just a matter of time until the mean of the population of subsequent generations evolves towards the best solution.
