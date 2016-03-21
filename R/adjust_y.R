##' @title Adjust y coordinates for gaze data.
##'
##' @description Adjust y coordinates for gaze data, such as might occur in reading multi-line text
##'     or other tasks with a similar visual scanning component. While "ground truth" for fixation
##'     locations is not easily obtained, consensus among reading researchers who use eye tracking
##'     to understand on-line reading processes holds that data for y coordinates obtained through
##'     most (all) eye tracking devices is imperfect and often in need of adjustment prior to
##'     computing regional summaries of gaze data. This function serves that purpose, using an
##'     optimization approach similar to that used in aligning neurimaging data with standard
##'     templates.
##'
##' @details
##'
##' @param data A data.frame containing gaze data (fixations or samples), possibly from multiple subjects/trials.
##' @param FUN A function to optimize in order to compute adjusted y-values for a single trial.
##' @param lines A vector of known y positions (centroids) of text lines for each trial contained in
##'     \code{data}. This argument is passed to \code{FUN}. [maybe this should be a data.frame]
##' @param ... Additional arguments passed to \code{FUN}.
##' @return A copy of data enriched with adjusted y values.
##' @author

require(png)

k_bounds <- c(-.1, .1); o_bounds <- c(-32.5, 32.5); s_bounds <- c(1, 20)
den_sd_cutoff <- Inf # remove points for which the density is > this many sd away from mean density
den_ratio_cutoff <- 1 # remove points for which (max density)/(2nd max density) not high enough

data1 <- read.csv("./fix_data1.csv", na.strings = "NA")
data1$type <- as.character(data1$type)
lines1 <- read.csv("./start_pts1.csv", na.strings = "NA")

init_params1 <- c(0, 0, 0) # for create_lines: Intitial parameter values (slope, vertical offset, sd)
init_params2 <- rep(c(0,0,0), nrow(lines)) # Intitial parameter values (slope, vertical offset, sd) for each line of data


adjust_y <- function(data, lines, datafile_name, init_params, 
                     FUN, ...) {

    ## See fix_align.R, here: http://www.psych.umass.edu/PACLab/resources/
    ## And description of same in BRMIC: http://people.umass.edu/eyelab/CohenBRM.pdf
    ##
    ## Our version should differ from that one in
    ## 1. We take a data.frame rather than an SRR *ASC file as input.
    ## 2. We ADD modified y values, rather than replace existing ones.
    ## 3. We accomidate both sample data and fixation data.
    ## 4. Specific function used for optimisation is swappable.
  
  fit <- optim(init_params, FUN, data=data, start_pts=lines, 
               k_bounds=k_bounds, o_bounds=o_bounds, s_bounds=s_bounds, 
               den_sd_cutoff=den_sd_cutoff, den_ratio_cutoff=den_ratio_cutoff) # Optimize
  line_ret <- FUN(fit$par, fit_it=FALSE, data=data, start_pts=lines, k_bounds, o_bounds, s_bounds, den_sd_cutoff, den_ratio_cutoff) # Find the best fitting parameters
  write.csv(line_ret$data, datafile_name, row.names = FALSE) # Save results
  return (line_ret) # Return
}


# create_lines1: original paper's function 
# one slope, ver_offset, sd for all lines, using -sum(data_den_max) as target measure for optimization
create_lines1 <- function(params, fit_it=TRUE, data, start_pts, k_bounds, o_bounds, s_bounds, den_sd_cutoff, den_ratio_cutoff) {
  # fit_it: TRUE -> return fit measure, FALSE -> return fit information
  ys <- start_pts[,2] # The y-values for the lines
  n_lines <- length(ys) # The number of clusters is based off of the lines
  
  # Initialize matrices
  data_den <- matrix(numeric(0), nrow(data), n_lines)
  y_diff <- matrix(numeric(0), nrow(data), n_lines)
  
  # Unpack the parameters
  k <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[1])
  o <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[2])
  s <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[3])
  
  for (l in 1:n_lines) {
    y_on_line <- o + k*(data$x_pos - start_pts[l,1]) + start_pts[l,2] # The value of each point on each line
    data_den[,l] <- log(dnorm(data$y_pos, mean=y_on_line, sd=s)) # Log density value for each point based on the line and sd
    y_diff[,l] <- abs(data$y_pos - y_on_line) # Store the difference between the real and fitted value
  }
  
  # Find max density line for each point
  data_den_max <- apply(data_den, 1, max) # Assume all-or-none classification
  fit_den <- -sum(data_den_max) # The sum of the log densitities is the fit measure, using valid, in-bounds fixations
  # Find min y_diff line for each point
  y_diff_min <- apply(y_diff, 1, min) # The smallest difference between assigned line pos and original y_pos
  fit_y_diff <- sum(y_diff_min) # The sum of the smallest difference is the fit measure
  
  if (fit_it) {
    if (fit_den == Inf) fit_den = .Machine$integer.max # In case log density goes to infinity
    return(fit_den) # Method 1: Return the fit_den as the fit measure
  } else {
    # optimization is finished, processing the final results
    
    # Mark ambigous points
    data_den_sort <- t(apply(exp(data_den), 1, sort))
    data_den_ratio <- data_den_sort[,n_lines]/data_den_sort[,n_lines-1]
    
    ambig_rm <- data_den_ratio < den_ratio_cutoff
    ambig_rm <- ambig_rm & data$type != 'oob'
    data$type[ambig_rm] <- 'amb'
    
    # Mark points with very low density
    inv_dnorm <- function(x) {sqrt( -2*log(sqrt(2*pi) * x)) }
    density_rm <- inv_dnorm(exp(data_den_max)) > den_sd_cutoff
    density_rm <- density_rm & data$type != 'oob' & data$type != 'amb'
    data$type[density_rm] <- 'den'
    
    # Method 1: Line membership	based on data_den
    lines <- apply(data_den, 1, which.max)	
    data$line <- lines
    
    # Reclassify ambiguous pts based on surrounding fixations
    amb_ind <- which(data$type == 'amb') # Get indices of ambiguous pts
    # Go through each of these points
    for (i in amb_ind) {
      # Go backwards to get line membership of previous keeper
      j = i - 1
      repeat { 			
        if (j <= 0) prev_line = -1
        else if (data$type[j] == 'keep') prev_line = data$line[j]
        else if (data$type[j] == 'oob') prev_line = -1
        else if (data$type[j] == 'den') prev_line = -1
        else if (data$type[j] == 'amb') { j = j - 1; next }
        break
      }
      # Go forwards to get line membership of next keeper
      j = i + 1
      repeat { 
        if (j > length(data$type)) next_line = -1
        else if (data$type[j] == 'keep') next_line = data$line[j]						
        else if (data$type[j] == 'oob') next_line = -1
        else if (data$type[j] == 'den') next_line = -1
        else if (data$type[j] == 'amb') { j = j + 1; next }
        break
      }
        
      # If both before and after are from the same line, reclassify
      if (prev_line == next_line && prev_line != -1) {
        data$type[i] = 'keep'; data$line[i] = prev_line
      }
    }
    # Store the new category memberships
    lines <- data$line
    # add new y position values
    data$y_line <- start_pts[data$line,2]
    # add new y residule values
    data$y_res <- -1.0
    slope <- k; vert_offset <- o
    for (i in 1:n_lines) {
      y_on_value <- vert_offset + slope*(data$x_pos[data$type != 'oob' & data$line==i] - start_pts[i,1]) + start_pts[i,2]
      data$y_res[data$type != 'oob' & data$line==i] <- data$y_pos[data$type != 'oob' & data$line==i] - y_on_value
    }
    
    # Store the fit measures and fitted parameters
    data$fit_den <- fit_den
    data$fit_y_diff <- fit_y_diff
    data$slope <- k; data$offset <- o; data$sd <- s
    
    # Store category membership, untransformed parameters, fit measures, fixation data, start points
    line_ret <- list()
    line_ret$lines <- lines; line_ret$params <- c(k, o, s)
    line_ret$fit_den <- fit_den; line_ret$fit_y_diff <- fit_y_diff
    line_ret$data <- data; line_ret$start_pts <- start_pts
    
    return(line_ret) # Return it
  }
}


# create_lines2: original paper's function 
# one slope, ver_offset, sd for all lines, using sum(min y_diff) as target measure for optimization
create_lines2 <- function(params, fit_it=TRUE, data, start_pts, k_bounds, o_bounds, s_bounds, den_sd_cutoff, den_ratio_cutoff) {
  # fit_it: TRUE -> return fit measure, FALSE -> return fit information
  ys <- start_pts[,2] # The y-values for the lines
  n_lines <- length(ys) # The number of clusters is based off of the lines
  
  # Initialize matrices
  data_den <- matrix(numeric(0), nrow(data), n_lines)
  y_diff <- matrix(numeric(0), nrow(data), n_lines)
  
  # Unpack the parameters
  k <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[1])
  o <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[2])
  s <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[3])
  
  for (l in 1:n_lines) {
    y_on_line <- o + k*(data$x_pos - start_pts[l,1]) + start_pts[l,2] # The value of each point on each line
    data_den[,l] <- log(dnorm(data$y_pos, mean=y_on_line, sd=s)) # Log density value for each point based on the line and sd
    y_diff[,l] <- abs(data$y_pos - y_on_line) # Store the difference between the real and fitted value
  }
  
  # Find max density line for each point
  data_den_max <- apply(data_den, 1, max) # Assume all-or-none classification
  fit_den <- -sum(data_den_max) # The sum of the log densitities is the fit measure, using valid, in-bounds fixations
  # Find min y_diff line for each point
  y_diff_min <- apply(y_diff, 1, min) # The smallest difference between assigned line pos and original y_pos
  fit_y_diff <- sum(y_diff_min) # The sum of the smallest difference is the fit measure
  
  if (fit_it) {
    if (fit_y_diff == Inf) fit_y_diff = .Machine$integer.max # In case log density goes to infinity
    return(fit_y_diff) # Method 2L:: Return fit_y_diff as the fit measure
  } else {
    # optimization is finished, processing the final results
    
    # Mark ambigous points
    data_den_sort <- t(apply(exp(data_den), 1, sort))
    data_den_ratio <- data_den_sort[,n_lines]/data_den_sort[,n_lines-1]
    
    ambig_rm <- data_den_ratio < den_ratio_cutoff
    ambig_rm <- ambig_rm & data$type != 'oob'
    data$type[ambig_rm] <- 'amb'
    
    # Mark points with very low density
    inv_dnorm <- function(x) {sqrt( -2*log(sqrt(2*pi) * x)) }
    density_rm <- inv_dnorm(exp(data_den_max)) > den_sd_cutoff
    density_rm <- density_rm & data$type != 'oob' & data$type != 'amb'
    data$type[density_rm] <- 'den'
    
    # Method 2: Line membership	based on y_diff
    lines <- apply(y_diff, 1, which.min)	
    data$line <- lines
    
    # Reclassify ambiguous pts based on surrounding fixations
    amb_ind <- which(data$type == 'amb') # Get indices of ambiguous pts
    # Go through each of these points
    for (i in amb_ind) {
      # Go backwards to get line membership of previous keeper
      j = i - 1
      repeat { 			
        if (j <= 0) prev_line = -1
        else if (data$type[j] == 'keep') prev_line = data$line[j]
        else if (data$type[j] == 'oob') prev_line = -1
        else if (data$type[j] == 'den') prev_line = -1
        else if (data$type[j] == 'amb') { j = j - 1; next }
        break
      }
      # Go forwards to get line membership of next keeper
      j = i + 1
      repeat { 
        if (j > length(data$type)) next_line = -1
        else if (data$type[j] == 'keep') next_line = data$line[j]						
        else if (data$type[j] == 'oob') next_line = -1
        else if (data$type[j] == 'den') next_line = -1
        else if (data$type[j] == 'amb') { j = j + 1; next }
        break
      }
      
      # If both before and after are from the same line, reclassify
      if (prev_line == next_line && prev_line != -1) {
        data$type[i] = 'keep'; data$line[i] = prev_line
      }
    }
    # Store the new category memberships
    lines <- data$line
    # add new y position values
    data$y_line <- start_pts[data$line,2]
    # add new y residule values
    data$y_res <- -1.0
    slope <- k; vert_offset <- o
    for (i in 1:n_lines) {
      y_on_value <- vert_offset + slope*(data$x_pos[data$type != 'oob' & data$line==i] - start_pts[i,1]) + start_pts[i,2]
      data$y_res[data$type != 'oob' & data$line==i] <- data$y_pos[data$type != 'oob' & data$line==i] - y_on_value
    }
    
    # Store the fit measures and fitted parameters
    data$fit_den <- fit_den
    data$fit_y_diff <- fit_y_diff
    data$slope <- k; data$offset <- o; data$sd <- s
    
    # Store category membership, untransformed parameters, fit measure, fixation data, and start points
    line_ret <- list()
    line_ret$lines <- lines; line_ret$params <- c(k, o, s)
    line_ret$fit_den <- fit_den; line_ret$fit_y_diff <- fit_y_diff
    line_ret$data <- data; line_ret$start_pts <- start_pts
    
    return(line_ret) # Return it
  }
}


# create_lines3: modified code
# each line has its own slope, vert_offset, sd, using -sum(data_den_max) as target measure for optimization
create_lines3 <- function(params, fit_it=TRUE, data, start_pts, k_bounds, o_bounds, s_bounds, den_sd_cutoff, den_ratio_cutoff) {
  # fit_it: TRUE -> return fit measure, FALSE -> return fit information
  ys <- start_pts[,2] # The y-values for the lines
  n_lines <- length(ys) # The number of clusters is based off of the lines
  
  # Initialize matrices
  data_den <- matrix(numeric(0), nrow(data), n_lines)
  y_diff <- matrix(numeric(0), nrow(data), n_lines)
  
  for (l in 1:n_lines) {
    # Unpack the parameters
    k <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[(i-1)*3+1])
    o <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[(i-1)*3+2])
    s <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[(i-1)*3+3])
    
    y_on_line <- o + k*(data$x_pos - start_pts[l,1]) + start_pts[l,2] # The value of each point on each line
    data_den[,l] <- log(dnorm(data$y_pos, mean=y_on_line, sd=s)) # Log density value for each point based on the line and sd
    y_diff[,l] <- abs(data$y_pos - y_on_line) # Store the difference between the real and fitted value
  }
  
  # Find max density line for each point
  data_den_max <- apply(data_den, 1, max) # Assume all-or-none classification
  fit_den <- -sum(data_den_max) # The sum of the log densitities is the fit measure, using valid, in-bounds fixations
  # Find min y_diff line for each point
  y_diff_min <- apply(y_diff, 1, min) # The smallest difference between assigned line pos and original y_pos
  fit_y_diff <- sum(y_diff_min) # The sum of the smallest difference is the fit measure
  
  if (fit_it) {
    if (fit_den == Inf) fit_den = .Machine$integer.max # In case log density goes to infinity
    return(fit_den) # Method 3: Return fit_den as the fit measure
  } else {
    # optimization is finished, processing the final results
    
    # Mark ambigous points
    data_den_sort <- t(apply(exp(data_den), 1, sort))
    data_den_ratio <- data_den_sort[,n_lines]/data_den_sort[,n_lines-1]
    
    ambig_rm <- data_den_ratio < den_ratio_cutoff
    ambig_rm <- ambig_rm & data$type != 'oob'
    data$type[ambig_rm] <- 'amb'
    
    # Mark points with very low density
    inv_dnorm <- function(x) {sqrt( -2*log(sqrt(2*pi) * x)) }
    density_rm <- inv_dnorm(exp(data_den_max)) > den_sd_cutoff
    density_rm <- density_rm & data$type != 'oob' & data$type != 'amb'
    data$type[density_rm] <- 'den'
    
    # Method 3: Line membership	based on data_den
    lines <- apply(data_den, 1, which.max)	
    data$line <- lines
    
    # Reclassify ambiguous pts based on surrounding fixations
    amb_ind <- which(data$type == 'amb') # Get indices of ambiguous pts
    # Go through each of these points
    for (i in amb_ind) {
      # Go backwards to get line membership of previous keeper
      j = i - 1
      repeat { 			
        if (j <= 0) prev_line = -1
        else if (data$type[j] == 'keep') prev_line = data$line[j]
        else if (data$type[j] == 'oob') prev_line = -1
        else if (data$type[j] == 'den') prev_line = -1
        else if (data$type[j] == 'amb') { j = j - 1; next }
        break
      }
      # Go forwards to get line membership of next keeper
      j = i + 1
      repeat { 
        if (j > length(data$type)) next_line = -1
        else if (data$type[j] == 'keep') next_line = data$line[j]						
        else if (data$type[j] == 'oob') next_line = -1
        else if (data$type[j] == 'den') next_line = -1
        else if (data$type[j] == 'amb') { j = j + 1; next }
        break
      }
      
      # If both before and after are from the same line, reclassify
      if (prev_line == next_line && prev_line != -1) {
        data$type[i] = 'keep'; data$line[i] = prev_line
      }
    }
    # Store the new category memberships
    lines <- data$line
    # add new y position values
    data$y_line <- start_pts[data$line,2]
    # add new y residule values
    data$y_res <- -1.0
    
    newparams <- rep(c(0,0,0), n_lines)
    for (i in 1:n_lines) {
      newparams[(i-1)*3+1] <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[(i-1)*3+1])
      newparams[(i-1)*3+2] <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[(i-1)*3+2])
      newparams[(i-1)*3+3] <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[(i-1)*3+3])
      slope <- newparams[(i-1)*3+1]; vert_offset <- newparams[(i-1)*3+2]
      y_on_value <- vert_offset + slope*(data$x_pos[data$type != 'oob' & data$line==i] - start_pts[i,1]) + start_pts[i,2]
      data$y_res[data$type != 'oob' & data$line==i] <- data$y_pos[data$type != 'oob' & data$line==i] - y_on_value
    }
    
    # Store the fit measures and fitted parameters
    data$fit_den <- fit_den
    data$fit_y_diff <- fit_y_diff
    for (i in 1:n_lines) {
      data$slope[data$line==i] <- newparams[(i-1)*3+1]
      data$offset[data$line==i] <- newparams[(i-1)*3+2]
      data$sd[data$line==i] <- newparams[(i-1)*3+3]
    }
    
    # Store category membership, untransformed parameters, fit measure, fixation data
    line_ret <- list()
    line_ret$lines <- lines; line_ret$params <- newparams
    line_ret$fit_den <- fit_den; line_ret$fit_y_diff <- fit_y_diff
    line_ret$data <- data; line_ret$start_pts <- start_pts
    
    return(line_ret) # Return it
  }
}


# create_lines4: modified code
# each line has its own slope, vert_offset, sd, using sum(min y_diff) as target measure for optimization
create_lines4 <- function(params, fit_it=TRUE, data, start_pts, k_bounds, o_bounds, s_bounds, den_sd_cutoff, den_ratio_cutoff) {
  # fit_it: TRUE -> return fit measure, FALSE -> return fit information
  ys <- start_pts[,2] # The y-values for the lines
  n_lines <- length(ys) # The number of clusters is based off of the lines
  
  # Initialize matrices
  data_den <- matrix(numeric(0), nrow(data), n_lines)
  y_diff <- matrix(numeric(0), nrow(data), n_lines)
  
  for (l in 1:n_lines) {
    # Unpack the parameters
    k <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[(i-1)*3+1])
    o <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[(i-1)*3+2])
    s <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[(i-1)*3+3])
    
    y_on_line <- o + k*(data$x_pos - start_pts[l,1]) + start_pts[l,2] # The value of each point on each line
    data_den[,l] <- log(dnorm(data$y_pos, mean=y_on_line, sd=s)) # Log density value for each point based on the line and sd
    y_diff[,l] <- abs(data$y_pos - y_on_line) # Store the difference between the real and fitted value
  }
  
  # Find max density line for each point
  data_den_max <- apply(data_den, 1, max) # Assume all-or-none classification
  fit_den <- -sum(data_den_max) # The sum of the log densitities is the fit measure, using valid, in-bounds fixations
  # Find min y_diff line for each point
  y_diff_min <- apply(y_diff, 1, min) # The smallest difference between assigned line pos and original y_pos
  fit_y_diff <- sum(y_diff_min) # The sum of the smallest difference is the fit measure
  
  if (fit_it) {
    if (fit_y_diff == Inf) fit_y_diff = .Machine$integer.max # In case log density goes to infinity
    return(fit_y_diff) # Method 4: Return fit_y_diff as the fit measure
  } else {
    # optimization is finished, processing the final results
    
    # Mark ambigous points
    data_den_sort <- t(apply(exp(data_den), 1, sort))
    data_den_ratio <- data_den_sort[,n_lines]/data_den_sort[,n_lines-1]
    
    ambig_rm <- data_den_ratio < den_ratio_cutoff
    ambig_rm <- ambig_rm & data$type != 'oob'
    data$type[ambig_rm] <- 'amb'
    
    # Mark points with very low density
    inv_dnorm <- function(x) {sqrt( -2*log(sqrt(2*pi) * x)) }
    density_rm <- inv_dnorm(exp(data_den_max)) > den_sd_cutoff
    density_rm <- density_rm & data$type != 'oob' & data$type != 'amb'
    data$type[density_rm] <- 'den'
    
    # Method 3: Line membership	based on data_den
    lines <- apply(y_diff, 1, which.min)	
    data$line <- lines
    
    # Reclassify ambiguous pts based on surrounding fixations
    amb_ind <- which(data$type == 'amb') # Get indices of ambiguous pts
    # Go through each of these points
    for (i in amb_ind) {
      # Go backwards to get line membership of previous keeper
      j = i - 1
      repeat { 			
        if (j <= 0) prev_line = -1
        else if (data$type[j] == 'keep') prev_line = data$line[j]
        else if (data$type[j] == 'oob') prev_line = -1
        else if (data$type[j] == 'den') prev_line = -1
        else if (data$type[j] == 'amb') { j = j - 1; next }
        break
      }
      # Go forwards to get line membership of next keeper
      j = i + 1
      repeat { 
        if (j > length(data$type)) next_line = -1
        else if (data$type[j] == 'keep') next_line = data$line[j]						
        else if (data$type[j] == 'oob') next_line = -1
        else if (data$type[j] == 'den') next_line = -1
        else if (data$type[j] == 'amb') { j = j + 1; next }
        break
      }
      
      # If both before and after are from the same line, reclassify
      if (prev_line == next_line && prev_line != -1) {
        data$type[i] = 'keep'; data$line[i] = prev_line
      }
    }
    # Store the new category memberships
    lines <- data$line
    # add new y position values
    data$y_line <- start_pts[data$line,2]
    # add new y residule values
    data$y_res <- -1.0
    
    newparams <- rep(c(0,0,0), n_lines)
    for (i in 1:n_lines) {
      newparams[(i-1)*3+1] <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[(i-1)*3+1])
      newparams[(i-1)*3+2] <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[(i-1)*3+2])
      newparams[(i-1)*3+3] <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[(i-1)*3+3])
      slope <- newparams[(i-1)*3+1]; vert_offset <- newparams[(i-1)*3+2]
      y_on_value <- vert_offset + slope*(data$x_pos[data$type != 'oob' & data$line==i] - start_pts[i,1]) + start_pts[i,2]
      data$y_res[data$type != 'oob' & data$line==i] <- data$y_pos[data$type != 'oob' & data$line==i] - y_on_value
    }
    
    # Store the fit measures and fitted parameters
    data$fit_den <- fit_den
    data$fit_y_diff <- fit_y_diff
    for (i in 1:n_lines) {
      data$slope[data$line == i] <- newparams[(i-1)*3+1]
      data$offset[data$line == i] <- newparams[(i-1)*3+2]
      data$sd[data$line == i] <- newparams[(i-1)*3+3]
    }
    
    # Store category membership, untransformed parameters, fit measure, fixation data
    line_ret <- list()
    line_ret$lines <- lines; line_ret$params <- newparams
    line_ret$fit_den <- fit_den; line_ret$fit_y_diff <- fit_y_diff
    line_ret$data <- data; line_ret$start_pts <- start_pts
    
    return(line_ret) # Return it
  }
}


# trial_plots12: for single slope, offset, and sd cases (create_lines1 and create_lines2)
trial_plots12 <- function(line_ret, output_filehead, draw_type, bg_image_name=NULL, image_width=1280, image_height=1024) {
  
  # output_filehead: output image file head
  # draw_type: 'original' draw original y_pos, output file ends with '_ori'; 
  #            'modified' draw residulaized y_pos, output file ends with '_mod';
  # bg_image_name: background (text) image
  
  # Constants
  pt_size_min = 1; pt_size_max = 4
  # Line info
  start_pts <- line_ret$start_pts; n_lines <- nrow(start_pts)
  slope <- line_ret$params[1]; vert_offset <- line_ret$params[2]
  
  # Handle data
  data <- line_ret$data # Separate out the fixation data
  dur_five_num <- fivenum(data$duration) # Get five number summary of duration
  # Point sizes based on duration
  m <- (pt_size_max - pt_size_min)/(dur_five_num[5] - dur_five_num[1]) 
  data$pt_size <- m*(data$duration - dur_five_num[1]) + pt_size_min
  
  # Separate out some more data
  data_keep <- data[data$type == 'keep',]
  lines_keep <- line_ret$lines[data$type=='keep']
  data_oob <-  data[data$type == 'oob',]
  data_den <-  data[data$type == 'den',]
  data_amb <-  data[data$type == 'amb',]
  
  # Plot
  if (draw_type == 'original') png(file = paste(output_filehead, '_ori.png', sep=''), width = image_width, height = image_height, units = "px", res = 300, pointsize=4)
  if (draw_type == 'modified') png(file = paste(output_filehead, '_mod.png', sep=''), width = image_width, height = image_height, units = "px", res = 300, pointsize=4)
  
  if (is.null(bg_image_name)) {
    # draw a blank plot 
    plot(1, type='n', main='Fixations with Classifications', 
         xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
  } else {
    # Is there a specific image for this subject and trial?
    if (file.exists(bg_image_name)) {
      t_image <- readPNG(bg_image_name) # Load the image
      t_image_height <- dim(t_image)[1]; t_image_width <- dim(t_image)[2]
      if (image_width == t_image_width & image_height == t_image_height) {
        # draw upon a background figure
        plot(1:2, type = 'n', main='Fixations with Classifications', 
             xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
        lim <- par(); rasterImage(t_image, xleft = 0, ybottom = t_image_height, xright = t_image_width, ytop = 0)
      } else {
        warning(paste('Wrong resolution of background image for ', bg_image_name, sep='')) # If the image file is missing show the data without it
        # draw a blank plot 
        # Reverse y-limits so 0 at top
        plot(1, type='n', main='Fixations with Classifications', 
             xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
      }
    } else {
      warning(paste('Missing background image for ', bg_image_name, sep='')) # If the image file is missing show the data without it
      # draw a blank plot 
      # Reverse y-limits so 0 at top
      plot(1, type='n', main='Fixations with Classifications', 
           xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
    }
  }
  
  if (draw_type == 'original') {
    # Lines for fixation ordering
    points(data$x_pos, data$y_pos, cex=data$pt_size, col='yellow', pch=1, type='l', lty='dashed')
    # The kept fixations
    for (i in 1:n_lines) {
      cat <- lines_keep == i
      points(data_keep$x_pos[cat], data_keep$y_pos[cat], cex=data_keep$pt_size[cat], 
            col=c('blue', 'black')[i%%2+1], pch=1)
    }
    # The deleted fixations
    points(data_oob$x_pos, data_oob$y_pos, cex=data_oob$pt_size, col='red', pch=1) 
    points(data_den$x_pos, data_den$y_pos, cex=data_den$pt_size, col='orange', pch=1)
    points(data_amb$x_pos, data_amb$y_pos, cex=data_amb$pt_size, col='purple', pch=1)
  
    # Show fitted lines
    for (i in 1:n_lines) {
      lines(c(start_pts[i,1], max(data$x_pos)), c(start_pts[i,2] + vert_offset, start_pts[i,2] + slope*(max(data$x_pos) - start_pts[i,1]) + vert_offset), 
          col=c('blue', 'black')[i%%2+1])
    }
  }
  
  if (draw_type == 'modified') {
    # Lines for fixation ordering
    points(data$x_pos, data$y_line + data$y_res, cex=data$pt_size, col='yellow', pch=1, type='l', lty='dashed')
    # The kept fixations
    for (i in 1:n_lines) {
      cat <- lines_keep == i
      points(data_keep$x_pos[cat], data_keep$y_line[cat] + data_keep$y_res[cat], cex=data_keep$pt_size[cat], 
             col=c('blue', 'black')[i%%2+1], pch=1)
    }
    # The deleted fixations
    points(data_oob$x_pos, data_oob$y_line + data_oob$y_res, cex=data_oob$pt_size, col='red', pch=1) 
    points(data_den$x_pos, data_den$y_line + data_den$y_res, cex=data_den$pt_size, col='orange', pch=1)
    points(data_amb$x_pos, data_amb$y_line + data_amb$y_res, cex=data_amb$pt_size, col='purple', pch=1)
    
    # Show fitted lines
    for (i in 1:n_lines) {
      lines(c(start_pts[i,1], max(data$x_pos)), c(start_pts[i,2], start_pts[i,2]), 
            col=c('blue', 'black')[i%%2+1])
    }
  }
    
  # legends for fixations
  legend(x='topright', 
         legend=c('Kept', 'Kept', 'Out-of-bounds', 'Low density', 'Ambiguous'), 
         pch=c(1, 1, 1, 1, 1), col=c('black', 'blue', 'red', 'orange', 'purple'), cex = 1, bty='n')
  
  # legends for line parameters
  legend(x='bottomright',
         legend=c(paste('Slope =', round(line_ret$params[1], digits=3)),
                  paste('VOffset =', round(line_ret$params[2], digits=3)),
                  paste('SD =', round(line_ret$params[3], digits=3)),
                  paste('Fit_den =', round(line_ret$fit_den, digits=3)),
                  paste('Fit_ydiff = ', round(line_ret$fit_y_diff, digits=3))), cex = 1, bty='n')
  
  dev.off() # close plot
}


# trial_plots34: for single slope, offset, and sd cases (create_lines3 and create_lines4)
trial_plots34 <- function(line_ret, output_filehead, draw_type, bg_image_name=NULL, image_width=1280, image_height=1024) {
  
  # output_filehead: output image file head
  # draw_type: 'original' draw original y_pos, output file ends with '_ori'; 
  #            'modified' draw residulaized y_pos, output file ends with '_mod';
  # bg_image_name: background (text) image
  
  # Constants
  pt_size_min = 1; pt_size_max = 4
    # Line info
  start_pts <- line_ret$start_pts; n_lines <- nrow(start_pts)
  
  # Handle data
  data <- line_ret$data # Separate out the fixation data
  dur_five_num <- fivenum(data$duration) # Get five number summary of duration
  # Point sizes based on duration
  m <- (pt_size_max - pt_size_min)/(dur_five_num[5] - dur_five_num[1]) 
  data$pt_size <- m*(data$duration - dur_five_num[1]) + pt_size_min
  
  # Separate out some more data
  data_keep <- data[data$type == 'keep',]
  lines_keep <- line_ret$lines[data$type=='keep']
  data_oob <-  data[data$type == 'oob',]
  data_den <-  data[data$type == 'den',]
  data_amb <-  data[data$type == 'amb',]
  
  # Plot
  if (draw_type == 'original') png(file = paste(output_filehead, '_ori.png', sep=''), width = image_width, height = image_height, units = "px", res = 300, pointsize=4)
  if (draw_type == 'modified') png(file = paste(output_filehead, '_mod.png', sep=''), width = image_width, height = image_height, units = "px", res = 300, pointsize=4)
  
  if (is.null(bg_image_name)) {
    # draw a blank plot 
    plot(1, type='n', main='Fixations with Classifications', 
         xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
  } else {
    # Is there a specific image for this subject and trial?
    if (file.exists(bg_image_name)) {
      t_image <- readPNG(bg_image_name) # Load the image
      t_image_height <- dim(t_image)[1]; t_image_width <- dim(t_image)[2]
      if (image_width == t_image_width & image_height == t_image_height) {
        # draw upon a background figure
        plot(1:2, type = 'n', main='Fixations with Classifications', 
             xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
        lim <- par(); rasterImage(t_image, xleft = 0, ybottom = t_image_height, xright = t_image_width, ytop = 0)
      } else {
        warning(paste('Wrong resolution of background image for ', bg_image_name, sep='')) # If the image file is missing show the data without it
        # draw a blank plot 
        # Reverse y-limits so 0 at top
        plot(1, type='n', main='Fixations with Classifications', 
             xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
      }
    } else {
      warning(paste('Missing background image for ', bg_image_name, sep='')) # If the image file is missing show the data without it
      # draw a blank plot 
      # Reverse y-limits so 0 at top
      plot(1, type='n', main='Fixations with Classifications', 
           xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
    }
  }
  
  if (draw_type == 'original') {
    # Lines for fixation ordering
    points(data$x_pos, data$y_pos, cex=data$pt_size, col='yellow', pch=1, type='l', lty='dashed')
    # The kept fixations
    for (i in 1:n_lines) {
      cat <- lines_keep == i
      points(data_keep$x_pos[cat], data_keep$y_pos[cat], cex=data_keep$pt_size[cat], 
             col=c('blue', 'black')[i%%2+1], pch=1)
    }
    # The deleted fixations
    points(data_oob$x_pos, data_oob$y_pos, cex=data_oob$pt_size, col='red', pch=1) 
    points(data_den$x_pos, data_den$y_pos, cex=data_den$pt_size, col='orange', pch=1)
    points(data_amb$x_pos, data_amb$y_pos, cex=data_amb$pt_size, col='purple', pch=1)
    
    # Show fitted lines
    for (i in 1:n_lines) {
      slope <- line_ret$params[(i-1)*3+1]; vert_offset <- line_ret$params[(i-1)*3+2]
      lines(c(start_pts[i,1], max(data$x_pos)), c(start_pts[i,2] + vert_offset, start_pts[i,2] + slope*(max(data$x_pos) - start_pts[i,1]) + vert_offset), 
            col=c('blue', 'black')[i%%2+1])
    }
  }
  
  if (draw_type == 'modified') {
    # Lines for fixation ordering
    points(data$x_pos, data$y_line + data$y_res, cex=data$pt_size, col='yellow', pch=1, type='l', lty='dashed')
    # The kept fixations
    for (i in 1:n_lines) {
      cat <- lines_keep == i
      points(data_keep$x_pos[cat], data_keep$y_line[cat] + data_keep$y_res[cat], cex=data_keep$pt_size[cat], 
             col=c('blue', 'black')[i%%2+1], pch=1)
    }
    # The deleted fixations
    points(data_oob$x_pos, data_oob$y_line + data_oob$y_res, cex=data_oob$pt_size, col='red', pch=1) 
    points(data_den$x_pos, data_den$y_line + data_den$y_res, cex=data_den$pt_size, col='orange', pch=1)
    points(data_amb$x_pos, data_amb$y_line + data_amb$y_res, cex=data_amb$pt_size, col='purple', pch=1)
    
    # Show fitted lines
    for (i in 1:n_lines) {
      lines(c(start_pts[i,1], max(data$x_pos)), c(start_pts[i,2], start_pts[i,2]), 
            col=c('blue', 'black')[i%%2+1])
    }
  }
  
  # legends for fixations
  legend(x='topright', 
         legend=c('Kept', 'Kept', 'Out-of-bounds', 'Low density', 'Ambiguous'), 
         pch=c(1, 1, 1, 1, 1), col=c('black', 'blue', 'red', 'orange', 'purple'), cex = 1, bty='n')
  
  # legends for line parameters
  legend(x='bottomright',
         legend=c(paste('Slope (line1) =', round(line_ret$params[1], digits=3)),
                  paste('VOffset (line1) =', round(line_ret$params[2], digits=3)),
                  paste('SD (line1) =', round(line_ret$params[3], digits=3)),
                  paste('Fit_den =', round(line_ret$fit_den, digits=3)),
                  paste('Fit_ydiff = ', round(line_ret$fit_y_diff, digits=3))), cex = 1, bty='n')
  
  dev.off() # close plot
}
