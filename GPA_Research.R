library(sampling)
library(kSamples)
library(ggplot2)
library(reshape2)
library(cowplot)

########################## Load data #####################################
setwd('C:\\Users\\Jeremy\\OneDrive\\Research\\Grade Research')
degree_strata <- read.csv('Course Tracks.csv')
course_tracks <- read.csv('Degree Requirements.csv')
grade_data1 <- read.csv('Course Grades_15-16.csv')
grade_data2 <- read.csv('Course Grades_14.csv')
grade_data3 <- read.csv('Course Grades_13.csv')

# Concatenate grade data frames
grade_data1 <- grade_data1[, -c(length(grade_data1))]
grade_data <- rbind(grade_data1, grade_data2, grade_data3)


########################## Sampling of Degrees #################################
set.seed(4)
samp_degrees <- strata(data = degree_strata, stratanames = c("School"), size = c(rep(1, 7)), method = c('srswor'), description = TRUE)
# List of sampled degrees
degree_strata$Degree[samp_degrees$ID_unit]
# Subset course tracks data frame to look at only Microbiology BA for initial analysis
course_tracks <- subset(course_tracks, Degree == 'Microbiology BA')

########################## Course Data Aggregation #############################
agg_data <- matrix(nrow = length(course_tracks$Course.Number), ncol = 15)

# Aggregate data for courses from sampled degrees
for (i in 1:length(course_tracks$Course.Number)) {
        
        # Store course subject and number in aggregated table
        agg_data[i, 1] <- as.character(course_tracks$Subject[i])
        agg_data[i, 2] <- course_tracks$Course.Number[i]
        
        # Subset degree data to aggregate course data of interest
        temp_dat <- subset(grade_data, SUBJECT == agg_data[i, 1] & COURSE == agg_data[i, 2])
        
        # Iterate through letter grades for the course
        for (j in 3:15) {
               agg_data[i, j] <- sum(temp_dat[ , j + 18])
        }
}

# Format agg_data columns to be numeric
agg_data <- data.frame(agg_data)
for(i in c(3:ncol(agg_data))) {
        agg_data[ , i] <- as.numeric(as.character(agg_data[ , i]))
}
colnames(agg_data) <- c('Course Name', 'Course Number', 'A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-', 'F')



# Turn grade data from the number of grades in a category to the number of grades at and below category in new data frame.
cum_data <- data.frame(matrix(nrow = nrow(agg_data), ncol = ncol(agg_data) - 2))
for (i in 1:nrow(agg_data)) {
        for (j in 3:(ncol(agg_data))) {
                cum_data[i, j - 2] <- as.numeric(sum(agg_data[i, j:15]))
        }
}
# Calculate percentiles of total
for (i in 1:nrow(cum_data)) {
        for (j in ncol(cum_data):1) {
                cum_data[i, j] <- cum_data[i, j] / cum_data[i, 1]
        }
}

# Add course number and department information columns and column labels
colnames(cum_data) <- c('A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-', 'F')




# Plot of course percentile cuttoffs
boxplot(cum_data, use.cols = TRUE, xlab='Letter Grade', ylab='Course Percentile', main='Boxplots of Course Percentiles vs Letter Grades')

# Add course department and number information to data frame
# cum_data <- cbind(agg_data[ ,1:2], cum_data)
# colnames(cum_data) <- c('Course Department', 'Course Number', 'A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D+', 'D', 'D-', 'F')

########################## GPA Priors Simulation ###################################
set.seed(4)
# GPAs simulated from Beta Distribution to force values to be in range 0-4
gpas <- rbeta(10000, 6, 2) * 4

# Create data frame with simulated gpas with percentiles, and mean percents
gpa_sim <- data.frame(gpas, percentiles)
summary(gpa_sim)

par(mfrow=c(1,2))
barplot(rev(colSums(agg_data[,3:ncol(agg_data)])), col = 'white', main = 'Microbiology Letter Grade Distribution')
# hist(gpas, breaks = 20, xlim = c(0,4), xlab = 'GPA', main = 'Histogram of Simulated GPAs')
plot(gpa_sim$gpas, gpa_sim$percentiles, xlab = 'GPAs', ylab = 'Percentile', main = 'Aptitude Percentile Curve vs GPA')
par(mfrow=c(1,1))

# Determine quantile for each student's gpa
percentiles <- ecdf(gpas)(gpas)


########################## Student Course Grade Simulation ######################
# Initialize matrix letter grades
grade_sim <- data.frame(matrix(nrow = nrow(gpa_sim), ncol = nrow(course_tracks) * 2))
# Course percentages using range 1 cutoffs
percent_sim1.1 <- data.frame(matrix(nrow = nrow(gpa_sim), ncol = nrow(course_tracks)))
percent_sim1.2 <- data.frame(matrix(nrow = nrow(gpa_sim), ncol = nrow(course_tracks)))
percent_sim1.3 <- data.frame(matrix(nrow = nrow(gpa_sim), ncol = nrow(course_tracks)))
# Course percentages using range 2 cutoffs
percent_sim2.1 <- data.frame(matrix(nrow = nrow(gpa_sim), ncol = nrow(course_tracks)))
percent_sim2.2 <- data.frame(matrix(nrow = nrow(gpa_sim), ncol = nrow(course_tracks)))
percent_sim2.3 <- data.frame(matrix(nrow = nrow(gpa_sim), ncol = nrow(course_tracks)))

letter_grades <- c('F', 'D-', 'D', 'D+', 'C-', 'C', 'C+', 'B-', 'B', 'B+', 'A-', 'A', 'A+')
gpa_points <- c(0.0, 0.7, 1.0, 1.3, 1.7, 2.0, 2.3, 2.7, 3.0, 3.3, 3.7, 4.0, 4.0)

# Simulation for a students letter grade and grade percentile in each class
set.seed(4)
for (i in 1:nrow(grade_sim)) {
        # Determine whether to systematically simulate student in high or low range for percent_sim1.3
        hi_lo <- rbinom(1, 1, 0.5)
        
        for (j in 1:nrow(course_tracks)) {
                # Noise added to gpa prior percentile
                noise_var <- rbinom(1, 1, .5)*rnorm(1, 0, .5) + rnorm(1, 0, .1)
                p <- gpa_sim[i, 2] + noise_var
                # Ensure that percentile is bounded within [0,0.999]
                if (p >= 1) {
                        p <- runif(1, .8, .99) 
                } else if (p <= 0){
                        p <- runif(1, .01, .2) 
                }
                # List of course letter grade percentiles
                x <- as.numeric(as.character(cum_data[j, 3:ncol(cum_data)]))
                # Determine which letter grade to assign based on the student's percentile rank
                grade_sim[i, j] <- letter_grades[rank(c(p, x))[1]]
                # Add grade point value for letter grade 
                grade_sim[i, j + nrow(course_tracks)] <- as.numeric(gpa_points[rank(c(p, x))[1]])
                
                # Determine whether to simulate from low, middle, or high end of range for percent_sim1.2
                temp <- runif(1, 0, 1)
                if (temp < 0.4) {
                        range_loc <- 0
                } else if (temp < 0.6) {
                        range_loc <- 1
                } else {
                        range_loc <- 2
                }  
                
                # Add grade percentage from uniform distribution
                if (grade_sim[i, j] == 'A+') {
                        # Uniform simulation of percentages
                        percent_sim1.1[i, j] <- runif(1, .96, 1.0)
                        percent_sim2.1[i, j] <- runif(1, .96, 1.0)
                        
                        # Tiered uniform simulation with low and high range more likely
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.96, 0.96 + .04/3)
                                percent_sim2.2[i, j] <- runif(1, 0.96, 0.96 + .04/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, 0.96 + .04/3, 0.96 + .4/1.5)
                                percent_sim2.2[i, j] <- runif(1, 0.96 + .04/3, 0.96 + .4/1.5)
                        } else {
                                percent_sim1.2[i, j] <- runif(1, 0.96 + .04/1.5, 1)
                                percent_sim2.2[i, j] <- runif(1, 0.96 + .04/1.5, 1)
                        }
                        
                        # Systematic hi or low percentage
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.96 + .04/2, 1.0)
                                percent_sim2.3[i, j] <- runif(1, 0.96 + .04/2, 1.0)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.96, .96 + 0.04/2)
                                percent_sim2.3[i, j] <- runif(1, 0.96, .96 + 0.04/2)
                        }
                        
                } else if (grade_sim[i, j] == 'A') {
                        percent_sim1.1[i, j] <- runif(1, .92, .96)
                        percent_sim2.1[i, j] <- runif(1, .93, .96)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.92, .92 + .04/3)
                                percent_sim2.2[i, j] <- runif(1, 0.93, .93 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .92 + .04/3, .92 + .04/1.5)
                                percent_sim2.2[i, j] <- runif(1, .93 + .03/3, .93 + .03/1.5)
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .92 + .04/1.5, .96)
                                percent_sim2.2[i, j] <- runif(1, .93 + .03/1.5, .96)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.92 + .04/2, .96)
                                percent_sim2.3[i, j] <- runif(1, 0.93 + .03/2, .96)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.92, .92 + 0.04/2)
                                percent_sim2.3[i, j] <- runif(1, 0.93, .92 + 0.03/2)
                        }
                        
                } else if (grade_sim[i, j] == 'A-') {
                        percent_sim1.1[i, j] <- runif(1, .90, .92)
                        percent_sim2.1[i, j] <- runif(1, .90, .93)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.90, .9 + .02/3)
                                percent_sim2.2[i, j] <- runif(1, 0.90, .9 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .9 + .02/3, .9 + .02/1.5)
                                percent_sim2.2[i, j] <- runif(1, .9 + .03/3, .9 + .03/1.5) 
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .9 + .02/1.5, .92)
                                percent_sim2.2[i, j] <- runif(1, .9 + .03/1.5, .93)
                                }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.9 + .02/2, .92)
                                percent_sim2.3[i, j] <- runif(1, 0.9 + .03/2, .93)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.9, .9 + 0.02/2)
                                percent_sim2.3[i, j] <- runif(1, 0.9, .9 + 0.03/2)
                                }
                        
                } else if (grade_sim[i, j] == 'B+') {
                        percent_sim1.1[i, j] <- runif(1, .88, .90)
                        percent_sim2.1[i, j] <- runif(1, .87, .90)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.88, .88 + .02/3)
                                percent_sim2.2[i, j] <- runif(1, 0.87, .87 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .88 + .02/3, .88 + .02/1.5) 
                                percent_sim2.2[i, j] <- runif(1, .87 + .03/3, .87 + .03/1.5) 
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .88 + .02/1.5, .9)
                                percent_sim2.2[i, j] <- runif(1, .87 + .03/1.5, .9)
                                }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.88 + .02/2, .9)
                                percent_sim2.3[i, j] <- runif(1, 0.87 + .03/2, .9)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.88, .88 + 0.02/2)
                                percent_sim2.3[i, j] <- runif(1, 0.87, .87 + 0.03/2)
                        }
                        
                } else if (grade_sim[i, j] == 'B') {
                        percent_sim1.1[i, j] <- runif(1, .82, .88)
                        percent_sim2.1[i, j] <- runif(1, .83, .87)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.82, .82 + .06/3)
                                percent_sim2.2[i, j] <- runif(1, 0.83, .83 + .04/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .82 + .06/3, .82 + .06/1.5)
                                percent_sim2.2[i, j] <- runif(1, .83 + .04/3, .83 + .04/1.5) 
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .82 + .06/1.5, .88)
                                percent_sim2.2[i, j] <- runif(1, .83 + .04/1.5, .87)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.82 + .06/2, .88)
                                percent_sim2.3[i, j] <- runif(1, 0.83 + .04/2, .87)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.82, .82 + 0.06/2)
                                percent_sim2.3[i, j] <- runif(1, 0.83, .83 + 0.04/2)
                        }
                        
                } else if (grade_sim[i, j] == 'B-') {
                        percent_sim1.1[i, j] <- runif(1, .80, .82)
                        percent_sim2.1[i, j] <- runif(1, .80, .83)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.8, .8 + .02/3)
                                percent_sim2.2[i, j] <- runif(1, 0.8, .8 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .8 + .02/3, .8 + .02/1.5)
                                percent_sim2.2[i, j] <- runif(1, .8 + .03/3, .8 + .03/1.5)
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .8 + .02/1.5, .82)
                                percent_sim2.2[i, j] <- runif(1, .8 + .03/1.5, .83)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.8 + .02/2, .82)
                                percent_sim2.3[i, j] <- runif(1, 0.8 + .03/2, .83)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.8, .8 + 0.02/2)
                                percent_sim2.3[i, j] <- runif(1, 0.8, .8 + 0.03/2)
                        }
                        
                } else if (grade_sim[i, j] == 'C+') {
                        percent_sim1.1[i, j] <- runif(1, .78, .80)
                        percent_sim2.1[i, j] <- runif(1, .77, .80)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.78, .78 + .02/3)
                                percent_sim2.2[i, j] <- runif(1, 0.77, .77 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .78 + .02/3, .78 + .02/1.5)
                                percent_sim2.2[i, j] <- runif(1, .77 + .03/3, .77 + .03/1.5) 
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .78 + .02/1.5, .80)
                                percent_sim2.2[i, j] <- runif(1, .77 + .03/1.5, .80)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.78 + .02/2, .8)
                                percent_sim2.3[i, j] <- runif(1, 0.77 + .03/2, .8)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, .78, 0.78 + 0.02/2)
                                percent_sim2.3[i, j] <- runif(1, .77, 0.77 + 0.03/2)
                        }
                        
                } else if (grade_sim[i, j] == 'C') {
                        percent_sim1.1[i, j] <- runif(1, .72, .78)
                        percent_sim2.1[i, j] <- runif(1, .73, .77)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.72, .72 + .06/3)
                                percent_sim2.2[i, j] <- runif(1, 0.73, .73 + .04/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .72 + .06/3, .72 + .06/1.5) 
                                percent_sim2.2[i, j] <- runif(1, .73 + .04/3, .73 + .04/1.5)
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .72 + .06/1.5, .78)
                                percent_sim2.2[i, j] <- runif(1, .73 + .04/1.5, .77)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.72 + .06/2, .78)
                                percent_sim2.3[i, j] <- runif(1, 0.73 + .04/2, .77)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.72, .72 + 0.06/2)
                                percent_sim2.3[i, j] <- runif(1, 0.73, .73 + 0.04/2)
                        }
                        
                } else if (grade_sim[i, j] == 'C-') {
                        percent_sim1.1[i, j ] <- runif(1, .70, .72)
                        percent_sim2.1[i, j ] <- runif(1, .70, .73)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.70, .7 + .02/3)
                                percent_sim2.2[i, j] <- runif(1, 0.70, .7 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .7 + .02/3, .7 + .02/1.5) 
                                percent_sim2.2[i, j] <- runif(1, .7 + .03/3, .7 + .03/1.5) 
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .7 + .02/1.5, .72)
                                percent_sim2.2[i, j] <- runif(1, .7 + .03/1.5, .73)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.7 + .02/2, .72)
                                percent_sim2.3[i, j] <- runif(1, 0.7 + .03/2, .73)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.7, .7 + 0.02/2)
                                percent_sim2.3[i, j] <- runif(1, 0.7, .7 + 0.03/2)
                        }
                        
                } else if (grade_sim[i, j] == 'D+') {
                        percent_sim1.1[i, j ] <- runif(1, .68, .70)
                        percent_sim2.1[i, j ] <- runif(1, .67, .70)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.68, .68 + .02/3)
                                percent_sim2.2[i, j] <- runif(1, 0.67, .67 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .68 + .02/3, .68 + .02/1.5) 
                                percent_sim2.2[i, j] <- runif(1, .67 + .03/3, .67 + .03/1.5) 
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .68 + .02/1.5, .70)
                                percent_sim2.2[i, j] <- runif(1, .67 + .03/1.5, .70)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.68 + .02/2, .7)
                                percent_sim2.3[i, j] <- runif(1, 0.67 + .03/2, .7)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.68, .68 + 0.02/2)
                                percent_sim2.3[i, j] <- runif(1, 0.67, .67 + 0.03/2)
                        }
                        
                } else if (grade_sim[i, j] == 'D') {
                        percent_sim1.1[i, j] <- runif(1, .62, .68)
                        percent_sim2.1[i, j] <- runif(1, .63, .67)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.62, .62 + .06/3)
                                percent_sim2.2[i, j] <- runif(1, 0.63, .63 + .04/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .62 + .06/3, .62 + .06/1.5)
                                percent_sim2.2[i, j] <- runif(1, .63 + .04/3, .63 + .04/1.5)
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .62 + .06/1.5, .68)
                                percent_sim2.2[i, j] <- runif(1, .63 + .04/1.5, .67)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.62 + .06/2, .68)
                                percent_sim2.3[i, j] <- runif(1, 0.63 + .04/2, .67)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.62, .62 + 0.06/2)
                                percent_sim2.3[i, j] <- runif(1, 0.63, .63 + 0.04/2)
                        }
                        
                } else if (grade_sim[i, j] == 'D-') {
                        percent_sim1.1[i, j ] <- runif(1, .60, .62)
                        percent_sim2.1[i, j ] <- runif(1, .60, .63)
                        
                        if (range_loc == 0) {
                                percent_sim1.2[i, j] <- runif(1, 0.60, .6 + .02/3)
                                percent_sim2.2[i, j] <- runif(1, 0.60, .6 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim1.2[i, j] <- runif(1, .6 + .02/3, .6 + .02/1.5)
                                percent_sim2.2[i, j] <- runif(1, .6 + .03/3, .6 + .03/1.5) 
                        } else {
                                percent_sim1.2[i, j] <- runif(1, .6 + .02/1.5, .62)
                                percent_sim2.2[i, j] <- runif(1, .6 + .03/1.5, .63)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim1.3[i, j] <- runif(1, 0.6 + .02/2, .62)
                                percent_sim2.3[i, j] <- runif(1, 0.6 + .03/2, .63)
                        } else {
                                percent_sim1.3[i, j] <- runif(1, 0.6, .6 + 0.02/2)
                                percent_sim2.3[i, j] <- runif(1, 0.6, .6 + 0.03/2)
                        }
                        
                } else if (grade_sim[i, j] == 'F') {
                        percent_sim1.1[i, j] <- 0
                        percent_sim1.2[i, j] <- 0
                        percent_sim1.3[i, j] <- 0
                        percent_sim2.1[i, j] <- 0
                        percent_sim2.2[i, j] <- 0
                        percent_sim2.3[i, j] <- 0
                }
        }
}

# Add column names to grade_sim and percent_sim and bind gpa priors
colnames(grade_sim) <- c(paste(as.character(course_tracks$Subject), as.character(course_tracks$Course.Number)),
                         paste(as.character(course_tracks$Subject), as.character(course_tracks$Course.Number)))

colnames(percent_sim1.1) <- c(paste(as.character(course_tracks$Subject), as.character(course_tracks$Course.Number)))
colnames(percent_sim1.2) <- c(paste(as.character(course_tracks$Subject), as.character(course_tracks$Course.Number)))
colnames(percent_sim1.3) <- c(paste(as.character(course_tracks$Subject), as.character(course_tracks$Course.Number)))

colnames(percent_sim2.1) <- c(paste(as.character(course_tracks$Subject), as.character(course_tracks$Course.Number)))
colnames(percent_sim2.2) <- c(paste(as.character(course_tracks$Subject), as.character(course_tracks$Course.Number)))
colnames(percent_sim2.3) <- c(paste(as.character(course_tracks$Subject), as.character(course_tracks$Course.Number)))


########################## Calculating GPA Using Current System ###########################
post_gpa <- numeric(nrow(grade_sim))
for (i in (1:nrow(grade_sim))) {
        points_earned <- sum(course_tracks$Units * grade_sim[i, (nrow(course_tracks) + 1):(nrow(course_tracks)*2)]) 
        tot_units <- sum(course_tracks$Units)
        post_gpa[i] <-  points_earned / tot_units
}

# Error in gpa prior to post
gpa_diff <- post_gpa - gpa_sim$gpas 
par(mfrow=c(1,2))
hist(gpa_diff, breaks = 20, xlim = c(-1, 1.5), main = '', xlab='GPA Difference')
boxplot(gpa_diff)
title("Difference Between Calculated GPA and GPA Prior", line = -2, outer = TRUE)
summary(gpa_diff)

# Summary of the differences in prior and calculated gpas using grade point mapping
summary(gpa_sim$gpas)
summary(post_gpa)
par(mfrow=c(1,2))
hist(post_gpa, breaks = 20, xlim = c(0, 4), main = 'GPAs Calculated Using Letter Grades', xlab = 'GPA')
# hist(gpa_sim$gpas, breaks = 20, xlim = c(0, 4), main = 'Prior GPAs From Beta(6,2)', xlab = 'GPA')
par(mfrow=c(1,1))
#title("Difference Between GPA Prior and Calculated GPA", line = -2, outer = TRUE)


########################## Step Function #1: 2-2-6 Percent Gaps #########################
## Step Function 1.1: Uniform percent ranges
steplinfunc_1.1 <- numeric(nrow(percent_sim1.1))
points_earned <- numeric(1)
for (i in (1:nrow(percent_sim1.1))) {
        for (j in (1:ncol(percent_sim1.1))) {
                if (percent_sim1.1[i,j] >= 0.92) {
                        points_earned <- points_earned + course_tracks$Units[j] * 4
                } else if (percent_sim1.1[i,j] >= 0.90) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.55 + percent_sim1.1[i,j]*(0.3/0.02) - (0.3/0.02)*.90)
                } else if (percent_sim1.1[i,j] >= 0.88) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.15 + percent_sim1.1[i,j]*(0.3/0.02) - (0.3/0.02)*.88)
                } else if (percent_sim1.1[i,j] >= 0.82) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.85 + percent_sim1.1[i,j]*(0.3/0.06) - (0.3/0.06)*.82)
                } else if (percent_sim1.1[i,j] >= 0.80) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.55 + percent_sim1.1[i,j]*(0.3/0.02) - (0.3/0.02)*.80)
                } else if (percent_sim1.1[i,j] >= 0.78) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.15 + percent_sim1.1[i,j]*(0.3/0.02) - (0.3/0.02)*.78)
                } else if (percent_sim1.1[i,j] >= 0.72) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.85 + percent_sim1.1[i,j]*(0.3/0.06) - (0.3/0.06)*.72)
                } else if (percent_sim1.1[i,j] >= 0.70) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.55 + percent_sim1.1[i,j]*(0.3/0.02) - (0.3/0.02)*.70)
                } else if (percent_sim1.1[i,j] >= 0.68) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.15 + percent_sim1.1[i,j]*(0.3/0.02) - (0.3/0.02)*.68)
                } else if (percent_sim1.1[i,j] >= 0.62) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.85 + percent_sim1.1[i,j]*(0.3/0.06) - (0.3/0.06)*.62)
                } else if (percent_sim1.1[i,j] >= 0.60) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.55 + percent_sim1.1[i,j]*(0.3/0.02) - (0.3/0.02)*.60)
                } else {
                        points_earned <- points_earned + 0
                }
        }
        tot_units <- sum(course_tracks$Units)
        steplinfunc_1.1[i] <- points_earned / tot_units
        points_earned <- 0
}
# Summary Statistics 
summary(steplinfunc_1.1)
hist(steplinfunc_1.1, breaks = 20, xlim=c(0,4), xlab='GPA', main='Histogram of Step Linear Function GPAs')
# Correlation
plot(post_gpa, steplinfunc_1.1, xlim=c(.5,4), ylim=c(0.5,4), main='Step Function 1 GPA vs Letter Grade GPA', xlab = 'Letter Grade GPA', ylab = 'Step Function GPA')
abline(a=0, b=1, col='red')
cor(steplinfunc_1.1, post_gpa)


## Step Function #1: Low, middle, and high percent ranges
steplinfunc_1.2 <- numeric(nrow(percent_sim1.2))
points_earned <- numeric(1)
for (i in (1:nrow(percent_sim1.2))) {
        for (j in (1:ncol(percent_sim1.2))) {
                if (percent_sim1.2[i,j] >= 0.92) {
                        points_earned <- points_earned + course_tracks$Units[j] * 4
                } else if (percent_sim1.2[i,j] >= 0.90) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.55 + percent_sim1.2[i,j]*(0.3/0.02) - (0.3/0.02)*.90)
                } else if (percent_sim1.2[i,j] >= 0.88) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.15 + percent_sim1.2[i,j]*(0.3/0.02) - (0.3/0.02)*.88)
                } else if (percent_sim1.2[i,j] >= 0.82) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.85 + percent_sim1.2[i,j]*(0.3/0.06) - (0.3/0.06)*.82)
                } else if (percent_sim1.2[i,j] >= 0.80) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.55 + percent_sim1.2[i,j]*(0.3/0.02) - (0.3/0.02)*.80)
                } else if (percent_sim1.2[i,j] >= 0.78) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.15 + percent_sim1.2[i,j]*(0.3/0.02) - (0.3/0.02)*.78)
                } else if (percent_sim1.2[i,j] >= 0.72) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.85 + percent_sim1.2[i,j]*(0.3/0.06) - (0.3/0.06)*.72)
                } else if (percent_sim1.2[i,j] >= 0.70) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.55 + percent_sim1.2[i,j]*(0.3/0.02) - (0.3/0.02)*.70)
                } else if (percent_sim1.2[i,j] >= 0.68) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.15 + percent_sim1.2[i,j]*(0.3/0.02) - (0.3/0.02)*.68)
                } else if (percent_sim1.2[i,j] >= 0.62) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.85 + percent_sim1.2[i,j]*(0.3/0.06) - (0.3/0.06)*.62)
                } else if (percent_sim1.2[i,j] >= 0.60) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.55 + percent_sim1.2[i,j]*(0.3/0.02) - (0.3/0.02)*.60)
                } else {
                        points_earned <- points_earned + 0
                }
        }
        tot_units <- sum(course_tracks$Units)
        steplinfunc_1.2[i] <- points_earned / tot_units
        points_earned <- 0
}
# Summary Statistics 
summary(steplinfunc_1.2)
hist(steplinfunc_1.2, breaks = 20, xlim=c(0,4), xlab='GPA', main='Histogram of Step Linear Function GPAs')
# Correlation
plot(post_gpa, steplinfunc_1.2, xlim=c(.5,4), ylim=c(0.5,4), main='Step Function 1 GPA vs Letter Grade GPA', xlab = 'Letter Grade GPA', ylab = 'Step Function GPA')
abline(a=0, b=1, col='red')
cor(steplinfunc_1.2, post_gpa)


## Step Function #1: systematic low and hi percentages
steplinfunc_1.3 <- numeric(nrow(percent_sim1.3))
points_earned <- numeric(1)
for (i in (1:nrow(percent_sim1.3))) {
        for (j in (1:ncol(percent_sim1.3))) {
                if (percent_sim1.3[i,j] >= 0.92) {
                        points_earned <- points_earned + course_tracks$Units[j] * 4
                } else if (percent_sim1.3[i,j] >= 0.90) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.55 + percent_sim1.3[i,j]*(0.3/0.02) - (0.3/0.02)*.90)
                } else if (percent_sim1.3[i,j] >= 0.88) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.15 + percent_sim1.3[i,j]*(0.3/0.02) - (0.3/0.02)*.88)
                } else if (percent_sim1.3[i,j] >= 0.82) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.85 + percent_sim1.3[i,j]*(0.3/0.06) - (0.3/0.06)*.82)
                } else if (percent_sim1.3[i,j] >= 0.80) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.55 + percent_sim1.3[i,j]*(0.3/0.02) - (0.3/0.02)*.80)
                } else if (percent_sim1.3[i,j] >= 0.78) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.15 + percent_sim1.3[i,j]*(0.3/0.02) - (0.3/0.02)*.78)
                } else if (percent_sim1.3[i,j] >= 0.72) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.85 + percent_sim1.3[i,j]*(0.3/0.06) - (0.3/0.06)*.72)
                } else if (percent_sim1.3[i,j] >= 0.70) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.55 + percent_sim1.3[i,j]*(0.3/0.02) - (0.3/0.02)*.70)
                } else if (percent_sim1.3[i,j] >= 0.68) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.15 + percent_sim1.3[i,j]*(0.3/0.02) - (0.3/0.02)*.68)
                } else if (percent_sim1.3[i,j] >= 0.62) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.85 + percent_sim1.3[i,j]*(0.3/0.06) - (0.3/0.06)*.62)
                } else if (percent_sim1.3[i,j] >= 0.60) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.55 + percent_sim1.3[i,j]*(0.3/0.02) - (0.3/0.02)*.60)
                } else {
                        points_earned <- points_earned + 0
                }
        }
        tot_units <- sum(course_tracks$Units)
        steplinfunc_1.3[i] <- points_earned / tot_units
        points_earned <- 0
}
# Summary Statistics 
summary(steplinfunc_1.3)
hist(steplinfunc_1.3, breaks = 20, xlim=c(0,4), xlab='GPA', main='Histogram of Step Linear Function GPAs')
# Correlation
plot(post_gpa, steplinfunc_1.3, xlim=c(.5,4), ylim=c(0.5,4), main='Step Function GPA vs Letter Grade GPA With Systematic Bias', xlab = 'Letter Grade GPA', ylab = 'Step Function GPA')
abline(a=0, b=1, col='red')
cor(steplinfunc_1.3, post_gpa)



########################## Step Function #2: 3-3-4 Percent Gaps ##########################
## Step Function #2: Uniform percent range
steplinfunc2.1 <- numeric(nrow(percent_sim2.1))
points_earned <- numeric(1)
for (i in (1:nrow(percent_sim2.1))) {
        for (j in (1:ncol(percent_sim2.1))) {
                if (percent_sim2.1[i,j] >= 0.93) {
                        points_earned <- points_earned + course_tracks$Units[j] * 4
                } else if (percent_sim2.1[i,j] >= 0.90) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.55 + percent_sim2.1[i,j]*(0.3/0.03) - (0.3/0.03)*.90)
                } else if (percent_sim2.1[i,j] >= 0.87) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.15 + percent_sim2.1[i,j]*(0.3/0.03) - (0.3/0.03)*.87)
                } else if (percent_sim2.1[i,j] >= 0.83) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.85 + percent_sim2.1[i,j]*(0.3/0.04) - (0.3/0.04)*.83)
                } else if (percent_sim2.1[i,j] >= 0.80) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.55 + percent_sim2.1[i,j]*(0.3/0.03) - (0.3/0.03)*.80)
                } else if (percent_sim2.1[i,j] >= 0.77) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.15 + percent_sim2.1[i,j]*(0.3/0.03) - (0.3/0.03)*.77)
                } else if (percent_sim2.1[i,j] >= 0.73) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.85 + percent_sim2.1[i,j]*(0.3/0.04) - (0.3/0.04)*.73)
                } else if (percent_sim2.1[i,j] >= 0.70) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.55 + percent_sim2.1[i,j]*(0.3/0.03) - (0.3/0.03)*.70)
                } else if (percent_sim2.1[i,j] >= 0.67) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.15 + percent_sim2.1[i,j]*(0.3/0.03) - (0.3/0.03)*.67)
                } else if (percent_sim2.1[i,j] >= 0.63) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.85 + percent_sim2.1[i,j]*(0.3/0.04) - (0.3/0.04)*.63)
                } else if (percent_sim2.1[i,j] >= 0.60) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.55 + percent_sim2.1[i,j]*(0.3/0.03) - (0.3/0.03)*.60)
                } else {
                        points_earned <- points_earned + 0
                }
        }
        tot_units <- sum(course_tracks$Units)
        steplinfunc2.1[i] <- points_earned / tot_units
        points_earned <- 0
}
# Summary of Step Function #2
summary(steplinfunc2.1)
hist(steplinfunc2.1, breaks = 20, xlim=c(0,4), xlab='GPA', main='Histogram of Step Linear Function #2 GPAs')
# Correlation
plot(post_gpa, steplinfunc2.1, xlim=c(1.2,4), ylim=c(1.2,4), main='Step Function GPA vs Letter Grade GPA', xlab = 'Letter Grade GPA', ylab = 'Step Function GPA')
abline(a=0, b=1, col='red')
cor(steplinfunc2.1, post_gpa)



## Step Function #2: low, middle, and high percent ranges
steplinfunc2.2 <- numeric(nrow(percent_sim2.2))
points_earned <- numeric(1)
for (i in (1:nrow(percent_sim2.2))) {
        for (j in (1:ncol(percent_sim2.2))) {
                if (percent_sim2.2[i,j] >= 0.93) {
                        points_earned <- points_earned + course_tracks$Units[j] * 4
                } else if (percent_sim2.2[i,j] >= 0.90) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.55 + percent_sim2.2[i,j]*(0.3/0.03) - (0.3/0.03)*.90)
                } else if (percent_sim2.2[i,j] >= 0.87) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.15 + percent_sim2.2[i,j]*(0.3/0.03) - (0.3/0.03)*.87)
                } else if (percent_sim2.2[i,j] >= 0.83) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.85 + percent_sim2.2[i,j]*(0.3/0.04) - (0.3/0.04)*.83)
                } else if (percent_sim2.2[i,j] >= 0.80) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.55 + percent_sim2.2[i,j]*(0.3/0.03) - (0.3/0.03)*.80)
                } else if (percent_sim2.2[i,j] >= 0.77) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.15 + percent_sim2.2[i,j]*(0.3/0.03) - (0.3/0.03)*.77)
                } else if (percent_sim2.2[i,j] >= 0.73) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.85 + percent_sim2.2[i,j]*(0.3/0.04) - (0.3/0.04)*.73)
                } else if (percent_sim2.2[i,j] >= 0.70) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.55 + percent_sim2.2[i,j]*(0.3/0.03) - (0.3/0.03)*.70)
                } else if (percent_sim2.2[i,j] >= 0.67) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.15 + percent_sim2.2[i,j]*(0.3/0.03) - (0.3/0.03)*.67)
                } else if (percent_sim2.2[i,j] >= 0.63) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.85 + percent_sim2.2[i,j]*(0.3/0.04) - (0.3/0.04)*.63)
                } else if (percent_sim2.2[i,j] >= 0.60) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.55 + percent_sim2.2[i,j]*(0.3/0.03) - (0.3/0.03)*.60)
                } else {
                        points_earned <- points_earned + 0
                }
        }
        tot_units <- sum(course_tracks$Units)
        steplinfunc2.2[i] <- points_earned / tot_units
        points_earned <- 0
}
# Summary of Step Function #2
summary(steplinfunc2.2)
hist(steplinfunc2.2, breaks = 20, xlim=c(0,4), xlab='GPA', main='Histogram of Step Linear Function #2 GPAs')
# Correlation
plot(post_gpa, steplinfunc2.2, xlim=c(1.2,4), ylim=c(1.2,4), main='Step Function GPA vs Letter Grade GPA', xlab = 'Letter Grade GPA', ylab = 'Step Function GPA')
abline(a=0, b=1, col='red')
cor(steplinfunc2.2, post_gpa)


## Step Function #2: systematic high and low percentages
steplinfunc2.3 <- numeric(nrow(percent_sim2.3))
points_earned <- numeric(1)
for (i in (1:nrow(percent_sim2.3))) {
        for (j in (1:ncol(percent_sim2.3))) {
                if (percent_sim2.3[i,j] >= 0.93) {
                        points_earned <- points_earned + course_tracks$Units[j] * 4
                } else if (percent_sim2.3[i,j] >= 0.90) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.55 + percent_sim2.3[i,j]*(0.3/0.03) - (0.3/0.03)*.90)
                } else if (percent_sim2.3[i,j] >= 0.87) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.15 + percent_sim2.3[i,j]*(0.3/0.03) - (0.3/0.03)*.87)
                } else if (percent_sim2.3[i,j] >= 0.83) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.85 + percent_sim2.3[i,j]*(0.3/0.04) - (0.3/0.04)*.83)
                } else if (percent_sim2.3[i,j] >= 0.80) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.55 + percent_sim2.3[i,j]*(0.3/0.03) - (0.3/0.03)*.80)
                } else if (percent_sim2.3[i,j] >= 0.77) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.15 + percent_sim2.3[i,j]*(0.3/0.03) - (0.3/0.03)*.77)
                } else if (percent_sim2.3[i,j] >= 0.73) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.85 + percent_sim2.3[i,j]*(0.3/0.04) - (0.3/0.04)*.73)
                } else if (percent_sim2.3[i,j] >= 0.70) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.55 + percent_sim2.3[i,j]*(0.3/0.03) - (0.3/0.03)*.70)
                } else if (percent_sim2.3[i,j] >= 0.67) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.15 + percent_sim2.3[i,j]*(0.3/0.03) - (0.3/0.03)*.67)
                } else if (percent_sim2.3[i,j] >= 0.63) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.85 + percent_sim2.3[i,j]*(0.3/0.04) - (0.3/0.04)*.63)
                } else if (percent_sim2.3[i,j] >= 0.60) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.55 + percent_sim2.3[i,j]*(0.3/0.03) - (0.3/0.03)*.60)
                } else {
                        points_earned <- points_earned + 0
                }
        }
        tot_units <- sum(course_tracks$Units)
        steplinfunc2.3[i] <- points_earned / tot_units
        points_earned <- 0
}
# Summary of Step Function #2
summary(steplinfunc2.3)
hist(steplinfunc2.3, breaks = 20, xlim=c(0,4), xlab='GPA', main='Histogram of Step Linear Function #2 GPAs')
# Correlation
plot(post_gpa, steplinfunc2.3, xlim=c(.5,4), ylim=c(0.5,4), main='Step Function GPA vs Letter Grade GPA', xlab = 'Letter Grade GPA', ylab = 'Step Function GPA')
abline(a=0, b=1, col='red')
cor(steplinfunc2.3, post_gpa)


############# GPA Comparison Exploratory Analysis ################## 
## Difference Between Step Function #1 and Letter Grades
steplinfunc_diff1.1 <- abs(steplinfunc_1.1 - post_gpa)
steplinfunc_diff1.2 <- abs(steplinfunc_1.2 - post_gpa)
steplinfunc_diff1.3 <- abs(steplinfunc_1.3 - post_gpa)
steplinfunc_diff2.1 <- abs(steplinfunc2.1 - post_gpa)
steplinfunc_diff2.2 <- abs(steplinfunc2.2 - post_gpa)
steplinfunc_diff2.3 <- abs(steplinfunc2.3 - post_gpa)

summary(steplinfunc_diff2.1)
summary(steplinfunc_diff2.2)
summary(steplinfunc_diff2.3)


x <- data.frame(post_gpa, steplinfunc_1.1, steplinfunc_1.2, steplinfunc_1.3, steplinfunc2.1, steplinfunc2.2, steplinfunc2.3)
               

ggplot(x, aes(post_gpa, steplinfunc2.1)) + geom_point() + 
        xlab('Letter Grade GPA') + ylab('Step Function GPA') + ggtitle('SF2-Unif GPA vs Letter Grade GPA')

x <- data.frame(steplinfunc_diff2.1, steplinfunc_diff2.2)
colnames(x) <- c('SF2-Equiprobable', 'SF2: Non-Unif')

data <-melt(x)
ggplot(data, aes(x=value, fill=variable)) + geom_density(alpha=.3) +
        xlab('Absolute GPA Difference') + ggtitle('Absolute Difference Based On Percentage Simualation Model')



######################### Single Student Analysis ########################
set.seed(1)
index <- sample(1:nrow(grade_sim), 1)
percent_sim3.1 <- data.frame(matrix(nrow = 1000, ncol = nrow(course_tracks)))

for (i in 1:1000) {
        for (j in 1:nrow(course_tracks)) {
                # Add grade percentage from uniform distribution
                if (grade_sim[index, j] == 'A+') {
                        # Uniform simulation of percentages
                        percent_sim3.1[i, j] <- runif(1, .96, 1.0)
                        
                        # Tiered uniform simulation with low and high range more likely
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.96, 0.96 + .04/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, 0.96 + .04/3, 0.96 + .4/1.5)
                        } else {
                                percent_sim3.2[i, j] <- runif(1, 0.96 + .04/1.5, 1)
                        }
                        
                } else if (grade_sim[index, j] == 'A') {
                        percent_sim3.1[i, j] <- runif(1, .93, .96)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.93, .93 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .93 + .03/3, .93 + .03/1.5)
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .93 + .03/1.5, .96)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.93 + .03/2, .96)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.93, .92 + 0.03/2)
                        }
                        
                } else if (grade_sim[index, j] == 'A-') {
                        percent_sim3.1[i, j] <- runif(1, .90, .93)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.90, .9 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .9 + .03/3, .9 + .03/1.5) 
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .9 + .03/1.5, .93)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.9 + .03/2, .93)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.9, .9 + 0.03/2)
                        }
                        
                } else if (grade_sim[index, j] == 'B+') {
                        percent_sim3.1[i, j] <- runif(1, .87, .90)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.87, .87 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .87 + .03/3, .87 + .03/1.5) 
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .87 + .03/1.5, .9)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.87 + .03/2, .9)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.87, .87 + 0.03/2)
                        }
                        
                } else if (grade_sim[index, j] == 'B') {
                        percent_sim3.1[i, j] <- runif(1, .83, .87)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.83, .83 + .04/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .83 + .04/3, .83 + .04/1.5) 
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .83 + .04/1.5, .87)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.83 + .04/2, .87)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.83, .83 + 0.04/2)
                        }
                        
                } else if (grade_sim[index, j] == 'B-') {
                        percent_sim3.1[i, j] <- runif(1, .80, .83)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.8, .8 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .8 + .03/3, .8 + .03/1.5)
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .8 + .03/1.5, .83)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.8 + .03/2, .83)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.8, .8 + 0.03/2)
                        }
                        
                } else if (grade_sim[index, j] == 'C+') {
                        percent_sim3.1[i, j] <- runif(1, .77, .80)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.77, .77 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .77 + .03/3, .77 + .03/1.5) 
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .77 + .03/1.5, .80)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.77 + .03/2, .8)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, .77, 0.77 + 0.03/2)
                        }
                        
                } else if (grade_sim[index, j] == 'C') {
                        percent_sim3.1[i, j] <- runif(1, .73, .77)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.73, .73 + .04/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .73 + .04/3, .73 + .04/1.5)
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .73 + .04/1.5, .77)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.73 + .04/2, .77)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.73, .73 + 0.04/2)
                        }
                        
                } else if (grade_sim[index, j] == 'C-') {
                        percent_sim3.1[i, j ] <- runif(1, .70, .73)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.70, .7 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .7 + .03/3, .7 + .03/1.5) 
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .7 + .03/1.5, .73)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.7 + .03/2, .73)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.7, .7 + 0.03/2)
                        }
                        
                } else if (grade_sim[index, j] == 'D+') {
                        percent_sim3.1[i, j ] <- runif(1, .67, .70)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.67, .67 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .67 + .03/3, .67 + .03/1.5) 
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .67 + .03/1.5, .70)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.67 + .03/2, .7)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.67, .67 + 0.03/2)
                        }
                        
                } else if (grade_sim[index, j] == 'D') {
                        percent_sim3.1[i, j] <- runif(1, .63, .67)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.63, .63 + .04/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .63 + .04/3, .63 + .04/1.5)
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .63 + .04/1.5, .67)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.63 + .04/2, .67)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.63, .63 + 0.04/2)
                        }
                        
                } else if (grade_sim[index, j] == 'D-') {
                        percent_sim3.1[i, j ] <- runif(1, .60, .63)
                        
                        if (range_loc == 0) {
                                percent_sim3.2[i, j] <- runif(1, 0.60, .6 + .03/3)
                        } else if (range_loc == 1) {
                                percent_sim3.2[i, j] <- runif(1, .6 + .03/3, .6 + .03/1.5) 
                        } else {
                                percent_sim3.2[i, j] <- runif(1, .6 + .03/1.5, .63)
                        }
                        
                        if (hi_lo == 1) {
                                percent_sim3.3[i, j] <- runif(1, 0.6 + .03/2, .63)
                        } else {
                                percent_sim3.3[i, j] <- runif(1, 0.6, .6 + 0.03/2)
                        }
                        
                } else if (grade_sim[index, j] == 'F') {
                        percent_sim3.1[i, j] <- 0
                        percent_sim3.2[i, j] <- 0
                        percent_sim3.3[i, j] <- 0
                }
        }
}        

## Step Function #3: Uniform percent range
steplinfunc3.1 <- numeric(nrow(percent_sim3.1))
points_earned <- numeric(1)
for (i in (1:nrow(percent_sim3.1))) {
        for (j in (1:ncol(percent_sim3.1))) {
                if (percent_sim3.1[i,j] >= 0.93) {
                        points_earned <- points_earned + course_tracks$Units[j] * 4
                } else if (percent_sim3.1[i,j] >= 0.90) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.55 + percent_sim3.1[i,j]*(0.3/0.03) - (0.3/0.03)*.90)
                } else if (percent_sim3.1[i,j] >= 0.87) {
                        points_earned <- points_earned + course_tracks$Units[j] * (3.15 + percent_sim3.1[i,j]*(0.3/0.03) - (0.3/0.03)*.87)
                } else if (percent_sim3.1[i,j] >= 0.83) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.85 + percent_sim3.1[i,j]*(0.3/0.04) - (0.3/0.04)*.83)
                } else if (percent_sim3.1[i,j] >= 0.80) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.55 + percent_sim3.1[i,j]*(0.3/0.03) - (0.3/0.03)*.80)
                } else if (percent_sim3.1[i,j] >= 0.77) {
                        points_earned <- points_earned + course_tracks$Units[j] * (2.15 + percent_sim3.1[i,j]*(0.3/0.03) - (0.3/0.03)*.77)
                } else if (percent_sim3.1[i,j] >= 0.73) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.85 + percent_sim3.1[i,j]*(0.3/0.04) - (0.3/0.04)*.73)
                } else if (percent_sim3.1[i,j] >= 0.70) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.55 + percent_sim3.1[i,j]*(0.3/0.03) - (0.3/0.03)*.70)
                } else if (percent_sim3.1[i,j] >= 0.67) {
                        points_earned <- points_earned + course_tracks$Units[j] * (1.15 + percent_sim3.1[i,j]*(0.3/0.03) - (0.3/0.03)*.67)
                } else if (percent_sim3.1[i,j] >= 0.63) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.85 + percent_sim3.1[i,j]*(0.3/0.04) - (0.3/0.04)*.63)
                } else if (percent_sim3.1[i,j] >= 0.60) {
                        points_earned <- points_earned + course_tracks$Units[j] * (0.55 + percent_sim3.1[i,j]*(0.3/0.03) - (0.3/0.03)*.60)
                } else {
                        points_earned <- points_earned + 0
                }
        }
        tot_units <- sum(course_tracks$Units)
        steplinfunc3.1[i] <- points_earned / tot_units
        points_earned <- 0
}
# Summary of Step Function #2
summary(steplinfunc3.1)
sd(steplinfunc3.1)
hist(steplinfunc3.1, breaks = 40,  xlab='GPA', main='SF2 Student GPAs Using Same Letter Grades')
abline(v=post_gpa[index], col='red')




########################## Critical Cutoff Analysis ########################
data <- data.frame(cbind(gpas, post_gpa, steplinfunc2.1))

## Designate grade cuttoff point
par(mfrow=c(2,2))

cutoff <- 2.0
cutoff_data <- subset(data, post_gpa < cutoff & steplinfunc2.1 > cutoff)

plot(1:nrow(cutoff_data), cutoff_data$post_gpa, ylim = c(cutoff -.04, cutoff + .04), 
     xlab = 'Index', ylab = 'GPA', main = '2.0 Grade Cutoff')
points(1:nrow(cutoff_data), cutoff_data$steplinfunc2.1, col='red')

par(mfrow=c(1,1))

prop_above <- c(nrow(subset(all_data, post_gpa < 2.0 & steplinfunc_gpa2 > 2.0)),
                nrow(subset(all_data, post_gpa < 2.5 & steplinfunc_gpa2 > 2.5)),
                nrow(subset(all_data, post_gpa < 3.0 & steplinfunc_gpa2 > 3.0)),
                nrow(subset(all_data, post_gpa < 3.5 & steplinfunc_gpa2 > 3.5)))

prop_below <- c(nrow(subset(all_data, post_gpa > 2.0 & steplinfunc_gpa2 < 2.0)),
                nrow(subset(all_data, post_gpa > 2.5 & steplinfunc_gpa2 < 2.5)),
                nrow(subset(all_data, post_gpa > 3.0 & steplinfunc_gpa2 < 3.0)),
                nrow(subset(all_data, post_gpa > 3.5 & steplinfunc_gpa2 < 3.5)))













