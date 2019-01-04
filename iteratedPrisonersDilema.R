

optimist <- list(
  name = 'optimist',
  f = function(prev) {
    return(0)
  }
)

pessimist <- list(
  name = 'pessimist',
  f = function(prev) {
    return(1)
  }
)

random_ <- list(
  name = 'random',
  f = function(prev) {
    return(sample(c(0, 1), 1, replace = T))
  }
)

tit_4_tat <- list(
  name = 'tit_4_tat',
  f = function(prev) {
    return(prev)
  }
)

pd <- list(
  outcome = function(me, you) {
    return(me - you)
  },
  
  round = function(r1, r2) {
    return(c(pd$outcome(r1, r2), pd$outcome(r2, r1)))
  },
  
  game = function(f1,
                  f2,
                  rounds = 10,
                  random = T,
                  start = NA) {
    names <- list(c(f1$name, f2$name))
    turns <- array(dim = c(2, rounds + 1), dimnames = names)
    outcomes <- array(dim = c(2, rounds), dimnames = names)
    prev <- array(dim = c(2, 1), dimnames = names)
    
    if (!random && !is.na(start) && length(start) == 2) {
      prev[1:2] <- start
    } else {
      prev[1:2] <- sample(c(0, 1), 2, replace = T)
    }
    
    turns[1:2, 1] <- prev
    for (i in 1:rounds) {
      r1 <- f1$f(prev[2])
      r2 <- f2$f(prev[1])
      turns[1:2, i + 1] <- c(r1, r2)
      outcomes[1:2, i] <- pd$round(r1, r2)
      prev <- turns[1:2, i + 1]
    }
    game <- list(
        scores = rowSums(outcomes, na.rm = T),
        turns = turns,
        outcomes = outcomes
      )
    return(game)
  }
)