#' Generate multiple generations of simulations of genetic drive
#'
#' @param popsize Population size (start w half to each allele); default 50.
#' @param tidepools Number of populations; default 3.
#' @param ngen Number of generations to simulate; default 10.
#' @return Dataframe of number of snails for each allele for each pop for each gen.
#' @examples
#' sim_snails()
#' sim_snails(100, 10, 100)
#'
#' @export
sim_snails <- function(popsize=50, tidepools = 3, ngen = 10){
  snail_df <- generate_gen1(popsize,tidepools)
  snail_df <- many_new_gens(snail_df, ngen)
  snail_df$tidepool <- as.factor(snail_df$tidepool)

  return(snail_df)
}

#' @export
generate_gen1 <- function(popsize=50, tidepools = 3){
  tidepool <- rep(1:tidepools, each=2)
  generation <- rep(0, each=tidepools*2)
  snailcolor <- rep(c("white","red"),tidepools)
  snails <- rep(popsize/2,tidepools)
  data.frame(tidepool,generation,snailcolor,snails)
}

#' @export
generate_new_gen <- function(snail_df){
  popsize <- snail_df$snails[1]*2
  prior_gen <- max(snail_df$generation)
  new_gen <- prior_gen +1
  just_prior_gen_df <- filter(snail_df, generation == prior_gen) %>% filter(snailcolor == "white")
  df_rows <- nrow(just_prior_gen_df)
  for(i in 1:df_rows){
    snails <- c(rep("w",just_prior_gen_df$snails[i]),
                rep("r",popsize-just_prior_gen_df$snails[i])) #vector of snails
    newsnails <- sample(snails, size = popsize/2, replace = FALSE)
    new_white_count <- 2* sum(newsnails=="w") #find whites and double
    new_row1 <- list(just_prior_gen_df[i,1],new_gen, "white", new_white_count)
    new_row2 <- list(just_prior_gen_df[i,1],new_gen, "red", popsize-new_white_count)
    snail_df <- rbind(snail_df,new_row2,new_row1)
  }
  return(snail_df)
}

#' @export
many_new_gens <- function(snail_df, ngen = 10){
  for(i in 1:ngen){
    snail_df <- generate_new_gen(snail_df)
  }
  return(snail_df)
}


#snail_df <- sim_snails(popsize=500, tidepools = 5, ngen = 20)

#prob want multiple samples from one pop

#ggplot(filter(snail_df,snailcolor=="white"), aes(generation,snails, color=tidepool))+  geom_line()

