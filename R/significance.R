significance <- function(p) {
  if (p > 0.1) return(" ")
  else if (p > 0.05) return(".")
  else if (p > 0.01) return("*")
  else if (p > 0.001) return("**")
  else return("***")
}

