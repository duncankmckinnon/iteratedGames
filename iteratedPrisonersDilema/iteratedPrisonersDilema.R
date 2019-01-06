
### Strategies

optimist <- list(
  name = 'optimist',
  f = function(prev, n) {
    return(0)
  }
)

pessimist <- list(
  name = 'pessimist',
  f = function(prev, n) {
    return(1)
  }
)


vengeful <- list(
  name = 'vengful',
  f = function(prev, n) {
    return(1 %in% prev)
  }
)

forgiving <- list(
  name = 'forgiving',
  f = function(prev, n) {
    return(!(0 %in% prev))
  }
)

random_choice <- list(
  name = 'random_choice',
  f = function(prev, n) {
    return(sample(c(0, 1), 1, replace = T))
  }
)

tit_for_tat <- list(
  name = 'tit_for_tat',
  f = function(prev, n) {
    return(prev[n])
  }
)


pre_tit_for_tat <- list(
  name = 'pre_tit_for_tat',
  f = function(prev, n) {
    return(ifelse(n == 1, 0, prev[n-1]))
  }
)

### Game

pd <- list(
  outcome = function(me, you) {
    return(ifelse(me && you, -1, me - you))
  },
  
  round = function(r1, r2) {
    return(c(pd$outcome(r1, r2), pd$outcome(r2, r1)))
  },
  
  game = function(f1, f2,
                  rounds = 10,
                  random = T,
                  start = NA) {
    names <- c(f1$name, f2$name)
    turns <- array(dim = c(2, rounds + 1), dimnames = list(names, c('init', 1:rounds)))
    outcomes <- array(dim = c(2, rounds), dimnames = list(names, 1:rounds))
    
    if (!random && !is.na(start) && length(start) == 2) {
      turns[1:2, 1] <- start
    } else {
      turns[1:2, 1] <- sample(c(0, 1), 2, replace = T)
    }
  
    for (i in 1:rounds) {
      r1 <- f1$f(turns[2,], i)
      r2 <- f2$f(turns[1,], i)
      turns[1:2, i + 1] <- c(r1, r2)
      outcomes[1:2, i] <- pd$round(r1, r2)
    }
    game <- list(
        scores = rowSums(outcomes, na.rm = T),
        turns = turns,
        outcomes = outcomes
      )
    return(game)
  }
)