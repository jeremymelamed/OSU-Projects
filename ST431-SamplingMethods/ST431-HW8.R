### Problem 8
# Part a
SST <- 15^2*65 + 66*32^2 + 19^2*57 + 58*41^2 + 25^2*25 + 26*54^2
var <- (SST - 150*39.293^2)/149
SE <- sqrt((1-150/2000)*(var/150))

SSB <- ((32-39.293)^2 + (41-39.293)^2 + (54 - 39.293)^2)*150
SSW <- SST - SSB

MSB <- SSB/2
MSW <- SSW/147

F_stat <- MSB/MSW
        
        
# Part b
respondents <- c(66,58,26) 
non_respondents <- c(9,14,27)
group <- c('low', 'med', 'high')
gpa_dat <- data.frame(cbind(group, respondents, non_respondents))
gpa_dat$respondents <- as.numeric(as.character(gpa_dat$respondents))
gpa_dat$non_respondents <- as.numeric(as.character(gpa_dat$non_respondents))

# chisq.test(gpa_dat)

# Part c

# Part d
t_post <- 10.606*32*66 + 13.793*41*58 + 19.231*54*26
t_post / 2000

# Problem 7
respondents <- c(636, 451, 481, 611, 493, 575, 588)
non_response <- c(279, 182, 177, 244, 174, 258, 236)
total <- respondents + non_response

expected_resp <- total*.712
expected_nonresp <- total*.288

chi_sq <- sum((respondents - expected_resp)^2 / expected_resp +
        (non_response - expected_nonresp)^2 / expected_nonresp)

response_rate <- c(.695, .712, .731, .715, .739, .69, .714)
non_resp_rate <- 1-response_rate
female_membership <- c(.38, .27, .18, .19, .36, .13, .26)

r <- cor(response_rate, female_membership)

t_stat <- r*sqrt(length(response_rate)-2) / sqrt(1-r^2)
t_stat
