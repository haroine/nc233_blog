
#' Discrete uniform distribution
#' @param x 
#' @param min
#' @param max 
dunifdisc <- function(x, min=0, max=1){
  ifelse(x>=min & x<=max & round(x)==x, 1/(max-min+1), 0)
}

#' Random sample from discrete uniform distribution
#' @param n Number of draws
#' @param k Number of possible (equiprobable) outcomes
runifdisc <-function(n,k){
  sample(1:k,n,replace=T)
} 


#' Plot prior for the number of games
plot_clue_prior <- function(x, dprior) {
  plot_prior <- ggplot(tibble(x=x, prior=dprior), aes(x=x, y=prior)) +
    geom_line(colour="#007BA7") +
    geom_area(fill="#7DF9FF") +
    # scale_x_log10(breaks=c(1,10,25,50,100,200), limits=c(1,200)) +
    scale_x_continuous(breaks=c(3,10,50,100,150,200), limits=c(1,200)) +
    xlab("Number of games") +
    ylab("Prior probability")
  print(plot_prior)
}

#' Plot posterior for the number of games
plot_clue_posterior <- function(x, dposterior) {
  plot_prior <- ggplot(tibble(x=x, prior=dposterior), aes(x=x, y=dposterior)) +
    geom_line(colour="#FF0000") +
    geom_area(fill="#DD0000") +
    scale_x_log10(breaks=c(1,10,25,50,100,200), limits=c(1,200)) +
    xlab("Number of games") +
    ylab("Posterior probability")
  print(plot_prior)
}


#' Plot prior for the number of games
plot_clue_prior2000 <- function(x, dprior) {
  plot_prior <- ggplot(tibble(x=x, prior=dprior), aes(x=x, y=prior)) +
    geom_line(colour="#007BA7") +
    geom_area(fill="#7DF9FF") +
    scale_x_log10(breaks=c(1,10,100,500,1000,2000), limits=c(1,2000)) +
    xlab("Number of games") +
    ylab("Prior probability")
  print(plot_prior)
}

#' Plot posterior for the number of games
plot_clue_posterior2000 <- function(x, dposterior) {
  plot_prior <- ggplot(tibble(x=x, prior=dposterior), aes(x=x, y=dposterior)) +
    geom_line(colour="#FF0000") +
    geom_area(fill="#DD0000") +
    scale_x_log10(breaks=c(1,10,100,500,1000,2000), limits=c(1,2000)) +
    xlab("Number of games") +
    ylab("Posterior probability")
  print(plot_prior)
}
