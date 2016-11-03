## speed-acc block psuedo-randomization
## we get the blocks and shuffle to get order 1
## then we reverse that order to get order 2
set.seed(007)
blocks <- c("face", "text", "text-no-audio", "bullseye")
block_order1 <- sample(blocks)
block_order2 <- rev(block_order1)
