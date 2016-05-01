# helper functions
if (FALSE) {
  require(png)

  k_bounds <- c(-.1, .1)
  o_bounds <- c(-30, 30) # about half distance between adjacent line
  s_bounds <- c(1, 12) # about 0.4 of o_bounds[0]; this is from normal attempt, usually items having
                       # 2.5 SD from the mean will be viewed as outliers.
  den_sd_cutoff <- Inf # remove points for which the density is > this many sd away from mean density
  den_ratio_cutoff <- 1 # remove points for which (max density)/(2nd max density) not high enough

  start_pts <- NULL
  start_pts <- rbind(start_pts, get_start_pts('story01.region.csv', 1))
  start_pts <- rbind(start_pts, get_start_pts('story02.region.csv', 2))
  start_pts <- rbind(start_pts, get_start_pts('story03.region.csv', 3))

  init_params_sing <- c(0, 0, 0) # for create_lines: Intitial parameter values (slope, vertical offset, sd)
  init_params_mult1 <- rep(c(0,0,0), nrow(start_pts[start_pts$trial_num==1,])) # Intitial parameter
                                                                               # values (slope,
                                                                               # vertical offset,
                                                                               # sd) for each line
                                                                               # of data
  init_params_mult2 <- rep(c(0,0,0), nrow(start_pts[start_pts$trial_num==2,]))
  init_params_mult3 <- rep(c(0,0,0), nrow(start_pts[start_pts$trial_num==3,]))

  xy_bounds <- NULL
  xy_bounds <- rbind(xy_bounds, get_xybounds('story01.region.csv', 1))
  xy_bounds <- rbind(xy_bounds, get_xybounds('story02.region.csv', 2))
  xy_bounds <- rbind(xy_bounds, get_xybounds('story03.region.csv', 3))

  subjs <- c('1950036', '1950090', '1950138', '1950149', '1950162', '1950163', '1950169', '1950184')

  for(subjID in subjs) {

    # get FixReport data
    fileName <- paste("./", subjID, "/", subjID, "-FixReportLines.txt", sep="")
    data <- read.table(fileName, header=TRUE, na.strings = ".", sep = "\t", quote = "\"'", dec = ".",)
    # remove missing values
    data <- subset(data, !is.na(CURRENT_FIX_X) & !is.na(CURRENT_FIX_Y))

    # add columns
    data$x_pos <- data$CURRENT_FIX_X; data$y_pos <- data$CURRENT_FIX_Y
    data$duration <- data$CURRENT_FIX_END - data$CURRENT_FIX_START
    data$type <- 'keep'
    data$hand_line <- data$CURRENT_FIX_LABEL

    # for each trial
    num_trial <- 3
    data_m1 <- NULL; data_m2 <- NULL; data_m3 <- NULL; data_m4 <- NULL
    for (cur_trial in 1:num_trial) {
      # get data
      subdata <- data[data$trial_num==cur_trial, c('trial_num', 'x_pos', 'y_pos', 'duration', 'type', 'hand_line')]
      # mark out of boundary fixations
      subdata <- mark_oob(subdata, xy_bounds, num_trial, cur_trial)

      # generate classified data
      if (cur_trial==1) init_params <- init_params_mult1
      if (cur_trial==2) init_params <- init_params_mult2
      if (cur_trial==3) init_params <- init_params_mult3

      data_m1 <- rbind(data_m1,
                       adjust_y(subdata, start_pts[start_pts$trial_num==cur_trial,],
                                init_params_sing, cat_lines1, k_bounds=k_bounds, o_bounds=o_bounds,
                                s_bounds=s_bounds, den_sd_cutoff=den_sd_cutoff,
                                den_ratio_cutoff=den_ratio_cutoff)
                       )
      data_m2 <- rbind(data_m2,
                       adjust_y(subdata, start_pts[start_pts$trial_num==cur_trial,],
                                init_params_sing, cat_lines2, k_bounds=k_bounds, o_bounds=o_bounds,
                                s_bounds=s_bounds, den_sd_cutoff=den_sd_cutoff,
                                den_ratio_cutoff=den_ratio_cutoff)
                       )
      data_m3 <- rbind(data_m3,
                       adjust_y(subdata, start_pts[start_pts$trial_num==cur_trial,], init_params,
                                cat_lines3, k_bounds=k_bounds, o_bounds=o_bounds, s_bounds=s_bounds,
                                den_sd_cutoff=den_sd_cutoff, den_ratio_cutoff=den_ratio_cutoff)
                       )
      data_m4 <- rbind(data_m4,
                       adjust_y(subdata, start_pts[start_pts$trial_num==cur_trial,], init_params,
                                cat_lines4, k_bounds=k_bounds, o_bounds=o_bounds, s_bounds=s_bounds,
                                den_sd_cutoff=den_sd_cutoff, den_ratio_cutoff=den_ratio_cutoff)
                       )
    }

    # store results
    write.csv(data_m1, paste(subjID, '_m1.csv', sep=""), row.names = FALSE)
    write.csv(data_m2, paste(subjID, '_m2.csv', sep=""), row.names = FALSE)
    write.csv(data_m3, paste(subjID, '_m3.csv', sep=""), row.names = FALSE)
    write.csv(data_m4, paste(subjID, '_m4.csv', sep=""), row.names = FALSE)
  }

  # draw picture
  num_methods <- 4
  bg_images <- c('story01.png', 'story02.png', 'story03.png')

  for(subjID in subjs) {
    # draw categorized fixations based on different methods
    for (method in 1:num_methods) {
      Alldata <- read.csv(paste(subjID, '_m', method, '.csv', sep=''), na.strings = 'NA')
      for (cur_trial in 1:num_trial) {
          trial_plots(Alldata[Alldata$trial_num==cur_trial,], start_pts[start_pts$trial_num==cur_trial,],
                      paste(subjID, '_t', cur_trial, '_m', method, sep=''), 'original', bg_images[cur_trial])
          trial_plots(Alldata[Alldata$trial_num==cur_trial,], start_pts[start_pts$trial_num==cur_trial,],
                      paste(subjID, '_t', cur_trial, '_m', method, sep=''), 'modified', bg_images[cur_trial])
      }
    }
    # draw hand made categorization
    Alldata <- read.csv(paste(subjID, '_m1.csv', sep=''), na.strings = 'NA')
    for (cur_trial in 1:num_trial) {
      trial_plots(Alldata[Alldata$trial_num==cur_trial,], start_pts[start_pts$trial_num==cur_trial,],
                  paste(subjID, '_t', cur_trial, sep=''), 'hand', bg_images[cur_trial])
    }
  }
}


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
##' @details The function first uses optim() to optimize the parameters for FUN (classify_lines).
##'     Then, it uses FUN with optimized parameters to classify fixations into different text lines.
##'     Finally, it returns the fixation dataframe with classified lines and optimized parameters.
##'
##' @param data A data.frame containing gaze data (fixations or samples), possibly from multiple subjects/trials.
##' @param lines A vector of known y positions (centroids) of text lines for each trial contained in
##'     \code{data}. This argument is passed to \code{FUN}. [maybe this should be a data.frame]
##' @param init_params A vector or a matrix (each row is a vector of parameters for one fitted line) containing the parameters to be optimized using different cat_line functions,
##' @param FUN A function to optimize in order to compute adjusted y-values for a single trial.
##' @param ... Additional arguments passed to \code{FUN}.
##' @return A copy of data enriched with adjusted y values and fit parameters.
##' @author Tao Gong \email{gtojty@@gmail.com}
##' @export
adjust_y <- function(data,
                     lines,
                     init_params,
                     FUN,
                     ...) {
  ## See fix_align.R, here: http://www.psych.umass.edu/PACLab/resources/
  ## And description of same in BRMIC: http://people.umass.edu/eyelab/CohenBRM.pdf
  ##
  ## Our version should differ from that one in
  ## 1. We take a data.frame rather than an SRR *ASC file as input.
  ## 2. We ADD modified y values, rather than replace existing ones.
  ## 3. We accomidate both sample data and fixation data.
  ## 4. Specific function used for optimisation is swappable.

  fit <- optim(init_params, FUN, data=data, start_pts=lines,
               k_bounds=k_bounds, o_bounds=o_bounds, s_bounds=s_bounds, den_sd_cutoff=den_sd_cutoff, den_ratio_cutoff=den_ratio_cutoff) # Optimize
  data <- FUN(fit$par, fit_it=FALSE, data=data, start_pts=lines, k_bounds, o_bounds, s_bounds, den_sd_cutoff, den_ratio_cutoff) # Find the best fitting parameters
  return (data) # Return
}


#' @title Get y positions of text lines from region file
#' @description This function extracts (x_pos, y_pos) of first word in each text line from the region file
#' @details This function reads region_file and gets (x_pos, y_pos) of first word in each text line.
#'          Then, it bounds (x_pos, y_pos) with trail_num into a data frame, and return the data frame.
#'
#' @param region_file A "*.csv" character list recording the name of the region file
#' @param trial_num An integer indicating the trial number of the current start_pts
#'
#' @return A data.frame recording the left position of the first word in each line of the text in each trial. It has three columns:
#'
#' \enumerate{
#'     \item x_pos: x position of the first word in each line of the text.
#'     \item y_pos: y position of the first word in each line of the text.
#'     \item trial_num: Integer indicating the trial number of the current start_pts.
#' }
#' @author Tao Gong \email{gtojty@@gmail.com}
#' @export
#'
get_start_pts <- function(region_file,
                          trial_num) {
  if (is.null(region_file)) stop('Region file is not inputted!')
  if(!file.exists(region_file)) stop(paste('Region file: ', region_file, ' doesn\'t exist!', sep=''))
  regionDF <- read.csv(region_file, na.strings='NA')

  n_lines <- max(regionDF$line_no)
  start_pts <- data.frame(matrix(ncol = 3, nrow = n_lines))
  names(start_pts) <- c('x_pos', 'y_pos', 'trial_num')

  for (curline in 1:n_lines) {
    start_pts$x_pos[curline] <- regionDF$x1_pos[regionDF$line_no==curline][1]
    start_pts$y_pos[curline] <- regionDF$baseline[regionDF$line_no==curline][1]
  }
  start_pts$trial_num <- trial_num

  return(start_pts)
}


#' @title Get xy_bounds from region file
#' @description This function extracts (x_min, x_max, y_min, y_max) from the region file
#' @details This function reads region_file and gets (x_min, x_max, y_min, y_max) of all text lines.
#'          Then, it bounds (x_min, x_max, y_min, y_max) with trail_num into a list, and return it.
#'
#' @param region_file region file name (csv file)
#' @param trial_num the trial number of the current start_pts
#' @param image_width default x_max boundary (1280)
#' @param image_height default y_max boundary (1024)
#'
#' @return A matrix recording the eye-movement recording boundary in the trial trial_num. Each row has five columns:
#'  \enumerate{
#'     \item x_min: Minimum x value across all text lines.
#'     \item x_max: Maximum x value across all text lines.
#'     \item y_min: Minimum y value across all text lines.
#'     \item y_max: Maximum y value across all text lines.
#'     \item trial_num: Integer indicating the trial number of the current start_pts.
#' }
#' @author Tao Gong \email{gtojty@@gmail.com}
#' @export
#'
get_xybounds <- function(region_file,
                         trial_num,
                         image_width=1280,
                         image_height=1024) {

  if (is.null(region_file)) stop('Region file is not inputted!')
  if(!file.exists(region_file)) stop(paste('Region file: ', region_file, ' doesn\'t exist!', sep=''))
  regionDF <- read.csv(region_file, na.strings='NA')

  xy_buffer <- 0.1
  x_range <- max(regionDF$x2_pos) - min(regionDF$x1_pos)
  y_range <- max(regionDF$y2_pos) - min(regionDF$y1_pos)

  x_min <- min(regionDF$x1_pos) - xy_buffer*x_range; if (x_min < 0.0) x_min <- 0.0
  x_max <- max(regionDF$x2_pos) + xy_buffer*x_range; if (x_max > image_width) x_min <- image_width
  y_min <- min(regionDF$y1_pos) - xy_buffer*y_range; if (y_min < 0.0) y_min <- 0.0
  y_max <- max(regionDF$y2_pos) + xy_buffer*y_range; if (y_max > image_height) y_max <- image_height

  xy_bounds <- c(x_min, x_max, y_min, y_max, trial_num)
  return(xy_bounds)
}


#' @title Mark out of boundary fixations
#' @description This function is modified from Cohen's paper
#' @details This function marks fixations outside boundary as 'oob'
#'
#' @param data A data.frame containing the fixation data
#' @param xy_bounds A mtraix containing the boundaries of fixations.
#'        If it has only one row, all trials use the same boundary.
#'        If it has many rows, the number of row should be equal to the number of trials,
#'        and each row corresponds to the boundary of the corresponding trial.
#'        In each row, it has four columns:
#'        \enumerate{
#'          \item x_min: Left boundary (minimum x value within the boundary).
#'          \item x_max: Right boundary (maximum x value within the boundary).
#'          \item y_min: Top boundary (minimum y value within the boundary).
#'          \item y_max: Bottom boundary (maximum y value within the boundary).
#'        }
#' @param trial_num Integer indicating the total number of trials
#' @param cur_trial Integer indicating the current trail
#'
#' @return A copy of data with out of boundary fixations marked as 'oob' in column type
#' @author Tao Gong \email{gtojty@@gmail.com}
#' @export
mark_oob <- function(data,
                     xy_bounds,
                     trial_num,
                     cur_trial) {

  if (is.null(xy_bounds)) stop('xy_bounds is not inputted')
  else {
    # Bounds for this trial
    if (nrow(xy_bounds) == 1) xy_bounds_trial <- xy_bounds
    else {
      # Make sure there are enough xy_bounds entries
      if (nrow(xy_bounds) != trial_num) stop(paste('Length of xy_bounds: ', nrow(xy_bounds), ' doesn\'t match trial_num: ', trial_num, sep=''))
      if (trial_num < cur_trial) stop(paste('cur_trial: ', cur_trial, ' is not in [1, ', trial_num, ']', sep=''))
      xy_bounds_trial <- xy_bounds[cur_trial,]
    }
    # Mark out-of-bounds fixations
    x_keep <- data$x_pos > xy_bounds_trial[1] & data$x_pos < xy_bounds_trial[2]
    y_keep <- data$y_pos > xy_bounds_trial[3] & data$y_pos < xy_bounds_trial[4]
    # Mark as out-of-bounds
    data$type[!(x_keep & y_keep)] <- 'oob'
  }
  return(data) # Return data
}

#' @title Categorize fixations into appropriate text lines
#'
#' @description This function follows closely upon that described in Cohen's paper
#'
#' @details This function optimizes the slope, offset and sd for all lines of fixations. It uses
#'     -sum(data_den_max) as fit measure for optimization.
#'
#' @param params A vector of parameters for optimization, the three values in it refer to slope, offset, and sd
#' @param fit_it A bollean variable; if it is TRUE, the function will return fit measure; if it is FALSE, the function will return fit information
#' @param data A data.frame storing the fixation data including at least the x_pos and y_pos of each fixation
#' @param start_pts A data.frame containing the starting position of each text line. It has three columns:
#'        \enumerate{
#'        \item x_pos: x position of the first word in each text line.
#'        \item y_pos: y position of the first word in each text line.
#'        \item trial_num: the trial number of the current start_pts
#'        }
#' @param k_bounds A list containing the lower and upper boundaries of slope;
#'                default value is [-0.1, 0.1]
#' @param o_bounds A list containing the lower and upper boundaries of offset;
#'                defaul value is [-0.5*dist of adjacent text lines, 0.5*dist of adjacent text line])
#' @param s_bounds A list containing the lower and upper boundaries of sd; default value is [1, 20]
#' @param den_sd_cutoff A float variable for cutoff threshold for density;
#'                If it is Inf, use mean(inv_dnorm(exp(data_den_max))) + 3*sd(inv_dnorm(exp(data_den_max))) as cutoff (99.7\% are accepted)
#' @param den_ratio_cutoff A float variable for cutoff threshold for density ratio (ratio between the maximum density and second maximum density)
#' @param num_checkFirst An integer denoting the number of starting fixations used for checking start-reading bound; default value is 5
#' @param num_checkLast An integer denoting the number of ending fixations used for checking end-reading bound; default value is 10
#'
#' @return A data.frame including fixation data, and fitting data including fit measures and fitted lines information.
#'        It adds the following columns:
#'        \enumerate{
#'        \item line: Text line that each fixation belongs to
#'        \item y_line: y position of the text line that each fixation is assigned to
#'        \item y_res: Residualized y position of each fixation y_line + y_res will give the original y position of each fixation
#'        \item slope: Optimized slope value for all fitted lines
#'        \item offset: Optimizaed offset value for all fitted lines
#'        \item sd: Optimized sd value for all fitted lines
#'        \item fit_den: fitted density value
#'        \item fit_y_diff: fitted y difference (accumulated y differences between each fixation and the fitted line)
#'        }
#' @author Tao Gong \email{gtojty@@gmail.com}
#' @export
cat_lines1 <- function(params,
                       fit_it=TRUE,
                       data,
                       start_pts,
                       k_bounds,
                       o_bounds,
                       s_bounds,
                       den_sd_cutoff,
                       den_ratio_cutoff,
                       num_checkFirst=5,
                       num_checkLast=10) {

  ys <- start_pts[,2] # The y-values for the lines
  n_lines <- length(ys) # The number of clusters is based off of the lines
  # Initialize matrices
  data_den <- matrix(numeric(0), nrow(data[data$type!='oob',]), n_lines)
  y_diff <- matrix(numeric(0), nrow(data[data$type!='oob',]), n_lines)

  # Unpack the parameters
  k <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[1])
  o <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[2])
  s <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[3])

  for (l in 1:n_lines) {
    y_on_line <- o + k*(data$x_pos[data$type!='oob'] - start_pts[l,1]) + start_pts[l,2] # The value of each point on each line
    data_den[,l] <- log(dnorm(data$y_pos[data$type!='oob'], mean=y_on_line, sd=s)) # Log density value for each point based on the line and sd
    y_diff[,l] <- abs(data$y_pos[data$type!='oob'] - y_on_line) # Store the difference between the real and fitted value
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
    data_den_sort <- t(apply(exp(data_den), 1, sort))
    # if one line has all 0, making them 1, so that data_den_ratio of that line is 1
    for (i in 1:nrow(data_den_sort)) {
      if (sum(data_den_sort[i,])==0) data_den_sort[i,] <- 1
    }
    data_den_ratio <- data_den_sort[,n_lines]/data_den_sort[,n_lines-1]

    # Mark ambigous points
    ambig_rm <- data_den_ratio <= den_ratio_cutoff
    ambig_rm[ambig_rm==FALSE] <- 'keep'; ambig_rm[ambig_rm==TRUE] <- 'amb'
    data$type[data$type!='oob'] <- ambig_rm

    # Mark points with very low density
    inv_dnorm <- function(x) {sqrt( -2*log(sqrt(2*pi) * x)) }
    mod_data_den <- inv_dnorm(exp(data_den_max))
    mod_data_den[mod_data_den==Inf] <- .Machine$integer.max
    if (den_sd_cutoff == Inf) den_sd_cutoff <- mean(mod_data_den) + 3*sd(mod_data_den)
    density_rm <- mod_data_den > den_sd_cutoff
    density_rm[density_rm==FALSE] <- 'keep'; density_rm[density_rm==TRUE] <- 'den'
    density_rm[ambig_rm=='amb'] <- 'amb' # an ambiguous point is still kept as ambiguity
    data$type[data$type!='oob'] <- density_rm

    # Method 1: Line membership	based on data_den
    data$line <- NA; data$line[data$type!='oob'] <- apply(data_den, 1, which.max)

    # mark out of reading bound fixations
    if (num_checkFirst != 0) { # for the first few fixations
      templine <- data$line[1:num_checkFirst]
      if (sum(unique(templine[!is.na(templine)])) != 1) {
        # there are no line 1 fixations, treating them as random elsewhere seeing.
        ind <- max(which(templine != 1), na.rm=TRUE)
        if (ind < num_checkFirst) {
          for (i in 1:ind) {
            if (!is.na(templine[i])) data$type[i] <- 'oob_r'
          }
        }
      }
    }
    if (num_checkLast != 0) { # for the last few fixations
      templine <- data$line[(nrow(data)-num_checkLast+1):nrow(data)]
      if (sum(unique(templine[!is.na(templine)])) != max(templine, na.rm=TRUE)) {
        # there are no last line fixations, treating them as random elsewhere seeing.
        # here, max(templine, na.rm=TRUE) is used instead of n_lines, in case that no fixation is categorized in the last line
        ind <- max(which(templine == max(templine, na.rm=TRUE)), na.rm=TRUE)
        if (ind < num_checkLast) {
          for (i in (nrow(data)-num_checkLast+ind+1):nrow(data)) {
            if (!is.na(templine[i-nrow(data)+num_checkLast])) data$type[i] <- 'oob_r'
          }
        }
      }
    }

    # Recategorize ambiguous pts based on surrounding fixations
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
        else if ((data$type[j] == 'amb') || (data$type[j] == 'oob_r')) { j = j - 1; next }
        break
      }
      # Go forwards to get line membership of next keeper
      j = i + 1
      repeat {
        if (j > length(data$type)) next_line = -1
        else if (data$type[j] == 'keep') next_line = data$line[j]
        else if (data$type[j] == 'oob') next_line = -1
        else if (data$type[j] == 'den') next_line = -1
        else if ((data$type[j] == 'amb') || (data$type[j] == 'oob_r')) { j = j + 1; next }
        break
      }

      # If both before and after are from the same line, reclassify
      if (prev_line == next_line && prev_line != -1) {
        data$type[i] = 'keep'; data$line[i] = prev_line
      }
    }
    # add new y position values
    data$y_line <- NA; data$y_line[data$type!='oob'] <- start_pts[data$line[data$type!='oob'],2]
    # add new y residule values
    data$y_res <- NA
    for (i in 1:n_lines) {
      y_on_value <- o + k*(data$x_pos[(data$type!='oob') & (data$line==i)] - start_pts[i,1]) + start_pts[i,2]
      data$y_res[(data$type!='oob') & (data$line==i)] <- data$y_pos[(data$type!='oob') & (data$line==i)] - y_on_value
    }
    # Store fitted parameters and fit measures
    data$slope <- k; data$offset <- o; data$sd <- s
    data$fit_den <- fit_den; data$fit_y_diff <- fit_y_diff

    return(data) # Return data
  }
}


#' @title Categorize fixations into appropriate text lines
#' @description Function to optimize slope, offset and sd for all lines of fixations (extended from
#'     original paper)This function uses one slope, offset, sd to fit all lines, and use sum(min
#'     y_diff) as target measure for optimization uses.
#' @details This function optimizes the slope, offset and sd for all lines of fixations. It uses
#'     sum(min(y_diff)) as fit measure for optimization.
#'
#' @param params A vector of parameters for optimization, the three values in it refer to slope, offset, and sd
#' @param fit_it A bollean variable; if it is TRUE, the function will return fit measure; if it is FALSE, the function will return fit information
#' @param data A data.frame storing the fixation data including at least the x_pos and y_pos of each fixation
#' @param start_pts A data.frame containing the starting position of each text line. It has three columns:
#'        \enumerate{
#'        \item x_pos: x position of the first word in each text line.
#'        \item y_pos: y position of the first word in each text line.
#'        \item trial_num: the trial number of the current start_pts
#'        }
#' @param k_bounds A list containing the lower and upper boundaries of slope;
#'                default value is [-0.1, 0.1]
#' @param o_bounds A list containing the lower and upper boundaries of offset;
#'                defaul value is [-0.5*dist of adjacent text lines, 0.5*dist of adjacent text line])
#' @param s_bounds A list containing the lower and upper boundaries of sd; default value is [1, 20]
#' @param den_sd_cutoff A float variable for cutoff threshold for density;
#'                If it is Inf, use mean(inv_dnorm(exp(data_den_max))) + 3*sd(inv_dnorm(exp(data_den_max))) as cutoff (99.7\% are accepted)
#' @param den_ratio_cutoff A float variable for cutoff threshold for density ratio (ratio between the maximum density and second maximum density)
#' @param num_checkFirst An integer denoting the number of starting fixations used for checking start-reading bound; default value is 5
#' @param num_checkLast An integer denoting the number of ending fixations used for checking end-reading bound; default value is 10
#'
#' @return A data.frame including fixation data, and fitting data including fit measures and fitted lines information.
#'        It adds the following columns:
#'        \enumerate{
#'        \item line: Text line that each fixation belongs to
#'        \item y_line: y position of the text line that each fixation is assigned to
#'        \item y_res: Residualized y position of each fixation y_line + y_res will give the original y position of each fixation
#'        \item slope: Optimized slope value for all fitted lines
#'        \item offset: Optimizaed offset value for all fitted lines
#'        \item sd: Optimized sd value for all fitted lines
#'        \item fit_den: fitted density value
#'        \item fit_y_diff: fitted y difference (accumulated y differences between each fixation and the fitted line)
#'        }
#' @author Tao Gong \email{gtojty@@gmail.com}
#' @export
cat_lines2 <- function(params,
                       fit_it=TRUE,
                       data,
                       start_pts,
                       k_bounds,
                       o_bounds,
                       s_bounds,
                       den_sd_cutoff,
                       den_ratio_cutoff,
                       num_checkFirst=5,
                       num_checkLast=10) {

  ys <- start_pts[,2] # The y-values for the lines
  n_lines <- length(ys) # The number of clusters is based off of the lines
  # Initialize matrices
  data_den <- matrix(numeric(0), nrow(data[data$type!='oob',]), n_lines)
  y_diff <- matrix(numeric(0), nrow(data[data$type!='oob',]), n_lines)

  # Unpack the parameters
  k <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[1])
  o <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[2])
  s <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[3])

  for (i in 1:n_lines) {
    y_on_line <- o + k*(data$x_pos[data$type!='oob'] - start_pts[i,1]) + start_pts[i,2] # The value of each point on each line
    data_den[,i] <- log(dnorm(data$y_pos[data$type!='oob'], mean=y_on_line, sd=s)) # Log density value for each point based on the line and sd
    y_diff[,i] <- abs(data$y_pos[data$type!='oob'] - y_on_line) # Store the difference between the real and fitted value
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
    data_den_sort <- t(apply(exp(data_den), 1, sort))
    # if one line has all 0, making them 1, so that data_den_ratio of that line is 1
    for (i in 1:nrow(data_den_sort)) {
      if (sum(data_den_sort[i,])==0) data_den_sort[i,] <- 1
    }
    data_den_ratio <- data_den_sort[,n_lines]/data_den_sort[,n_lines-1]

    # Mark ambigous points
    ambig_rm <- data_den_ratio <= den_ratio_cutoff
    ambig_rm[ambig_rm==FALSE] <- 'keep'; ambig_rm[ambig_rm==TRUE] <- 'amb'
    data$type[data$type!='oob'] <- ambig_rm

    # Mark points with very low density
    inv_dnorm <- function(x) {sqrt( -2*log(sqrt(2*pi) * x)) }
    mod_data_den <- inv_dnorm(exp(data_den_max))
    mod_data_den[mod_data_den==Inf] <- .Machine$integer.max
    if (den_sd_cutoff == Inf) den_sd_cutoff <- mean(mod_data_den) + 3*sd(mod_data_den)
    density_rm <- mod_data_den > den_sd_cutoff
    density_rm[density_rm==FALSE] <- 'keep'; density_rm[density_rm==TRUE] <- 'den'
    density_rm[ambig_rm=='amb'] <- 'amb' # an ambiguous point is still kept as ambiguity
    data$type[data$type!='oob'] <- density_rm

    # Method 2: Line membership	based on y_diff
    data$line <- NA; data$line[data$type!='oob'] <- apply(y_diff, 1, which.min)

    # mark out of reading bound fixations
    if (num_checkFirst != 0) { # for the first few fixations
      templine <- data$line[1:num_checkFirst]
      if (sum(unique(templine[!is.na(templine)])) != 1) {
        # there are no line 1 fixations, treating them as random elsewhere seeing.
        ind <- max(which(templine != 1), na.rm=TRUE)
        if (ind < num_checkFirst) {
          for (i in 1:ind) {
            if (!is.na(templine[i])) data$type[i] <- 'oob_r'
          }
        }
      }
    }
    if (num_checkLast != 0) { # for the last few fixations
      templine <- data$line[(nrow(data)-num_checkLast+1):nrow(data)]
      if (sum(unique(templine[!is.na(templine)])) != max(templine, na.rm=TRUE)) {
        # there are no last line fixations, treating them as random elsewhere seeing.
        # here, max(templine, na.rm=TRUE) is used instead of n_lines, in case that no fixation is categorized in the last line
        ind <- max(which(templine == max(templine, na.rm=TRUE)), na.rm=TRUE)
        if (ind < num_checkLast) {
          for (i in (nrow(data)-num_checkLast+ind+1):nrow(data)) {
            if (!is.na(templine[i-nrow(data)+num_checkLast])) data$type[i] <- 'oob_r'
          }
        }
      }
    }

    # Recategorize ambiguous pts based on surrounding fixations
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
        else if ((data$type[j] == 'amb') || (data$type[j] == 'oob_r')) { j = j - 1; next }
        break
      }
      # Go forwards to get line membership of next keeper
      j = i + 1
      repeat {
        if (j > length(data$type)) next_line = -1
        else if (data$type[j] == 'keep') next_line = data$line[j]
        else if (data$type[j] == 'oob') next_line = -1
        else if (data$type[j] == 'den') next_line = -1
        else if ((data$type[j] == 'amb') || (data$type[j] == 'oob_r')) { j = j + 1; next }
        break
      }

      # If both before and after are from the same line, reclassify
      if (prev_line == next_line && prev_line != -1) {
        data$type[i] = 'keep'; data$line[i] = prev_line
      }
    }
    # add new y position values
    data$y_line <- NA; data$y_line[data$type!='oob'] <- start_pts[data$line[data$type!='oob'],2]
    # add new y residule values
    data$y_res <- NA
    for (i in 1:n_lines) {
      y_on_value <- o + k*(data$x_pos[(data$type!='oob') & (data$line==i)] - start_pts[i,1]) + start_pts[i,2]
      data$y_res[(data$type!='oob') & (data$line==i)] <- data$y_pos[(data$type!='oob') & (data$line==i)] - y_on_value
    }
    # Store fitted parameters and fit measures
    data$slope <- k; data$offset <- o; data$sd <- s
    data$fit_den <- fit_den; data$fit_y_diff <- fit_y_diff

    return(data) # Return data
  }
}


#' @title Categorize fixations into appropriate text line unction to optimize slope, offset and sd for all lines of fixations (extedned from original paper)
#' @description This function uses many slopes, ver_offsets, sds to fit every lines, and use -sum(data_den_max)
#'  as target measure for optimizationuses
#' @details This function uses many slopes, ver_offsets, sds to fit every lines, and use -sum(data_den_max)
#'  as target measure for optimizationuses
#'
#' @param params A vector of parameters for optimization, the three values in it refer to slope, offset, and sd
#' @param fit_it A bollean variable; if it is TRUE, the function will return fit measure; if it is FALSE, the function will return fit information
#' @param data A data.frame storing the fixation data including at least the x_pos and y_pos of each fixation
#' @param start_pts A data.frame containing the starting position of each text line. It has three columns:
#'        \enumerate{
#'        \item x_pos: x position of the first word in each text line.
#'        \item y_pos: y position of the first word in each text line.
#'        \item trial_num: the trial number of the current start_pts
#'        }
#' @param k_bounds A list containing the lower and upper boundaries of slope;
#'                default value is [-0.1, 0.1]
#' @param o_bounds A list containing the lower and upper boundaries of offset;
#'                defaul value is [-0.5*dist of adjacent text lines, 0.5*dist of adjacent text line])
#' @param s_bounds A list containing the lower and upper boundaries of sd; default value is [1, 20]
#' @param den_sd_cutoff A float variable for cutoff threshold for density;
#'                If it is Inf, use mean(inv_dnorm(exp(data_den_max))) + 3*sd(inv_dnorm(exp(data_den_max))) as cutoff (99.7\% are accepted)
#' @param den_ratio_cutoff A float variable for cutoff threshold for density ratio (ratio between the maximum density and second maximum density)
#' @param num_checkFirst An integer denoting the number of starting fixations used for checking start-reading bound; default value is 5
#' @param num_checkLast An integer denoting the number of ending fixations used for checking end-reading bound; default value is 10
#'
#' @return A data.frame including fixation data, and fitting data including fit measures and fitted lines information.
#'        It adds the following columns:
#'        \enumerate{
#'        \item line: Text line that each fixation belongs to
#'        \item y_line: y position of the text line that each fixation is assigned to
#'        \item y_res: Residualized y position of each fixation y_line + y_res will give the original y position of each fixation
#'        \item slope: Optimized slope value for the fitted line that current fixation belongs to
#'        \item offset: Optimizaed offset value for the fitted line that current fixation belongs to
#'        \item sd: Optimized sd value for the fitted line that current fixation belongs to
#'        \item fit_den: fitted density value of the fitted line that current fixation belongs to
#'        \item fit_y_diff: fitted y difference of the fitted line that current fixation belongs to (accumulated y differences between each fixation and fitted lines)
#'        }
#' @author Tao Gong \email{gtojty@@gmail.com}
#' @export
cat_lines3 <- function(params,
                       fit_it=TRUE,
                       data,
                       start_pts,
                       k_bounds,
                       o_bounds,
                       s_bounds,
                       den_sd_cutoff,
                       den_ratio_cutoff,
                       num_checkFirst=5,
                       num_checkLast=10) {

  ys <- start_pts[,2] # The y-values for the lines
  n_lines <- length(ys) # The number of clusters is based off of the lines

  # Initialize matrices
  data_den <- matrix(numeric(0), nrow(data[data$type!='oob',]), n_lines)
  y_diff <- matrix(numeric(0), nrow(data[data$type!='oob',]), n_lines)

  for (l in 1:n_lines) {
    # Unpack the parameters
    k <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[(l-1)*3+1])
    o <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[(l-1)*3+2])
    s <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[(l-1)*3+3])

    y_on_line <- o + k*(data$x_pos[data$type!='oob'] - start_pts[l,1]) + start_pts[l,2] # The value of each point on each line
    data_den[,l] <- log(dnorm(data$y_pos[data$type!='oob'], mean=y_on_line, sd=s)) # Log density value for each point based on the line and sd
    y_diff[,l] <- abs(data$y_pos[data$type!='oob'] - y_on_line) # Store the difference between the real and fitted value
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
    data_den_sort <- t(apply(exp(data_den), 1, sort))
    # if one line has all 0, making them 1, so that data_den_ratio of that line is 1
    for (i in 1:nrow(data_den_sort)) {
      if (sum(data_den_sort[i,])==0) data_den_sort[i,] <- 1
    }
    data_den_ratio <- data_den_sort[,n_lines]/data_den_sort[,n_lines-1]

    # Mark ambigous points
    ambig_rm <- data_den_ratio <= den_ratio_cutoff
    ambig_rm[ambig_rm==FALSE] <- 'keep'; ambig_rm[ambig_rm==TRUE] <- 'amb'
    data$type[data$type!='oob'] <- ambig_rm

    # Mark points with very low density
    inv_dnorm <- function(x) {sqrt( -2*log(sqrt(2*pi) * x)) }
    mod_data_den <- inv_dnorm(exp(data_den_max))
    mod_data_den[mod_data_den==Inf] <- .Machine$integer.max
    if (den_sd_cutoff == Inf) den_sd_cutoff <- mean(mod_data_den) + 3*sd(mod_data_den)
    density_rm <- mod_data_den > den_sd_cutoff
    density_rm[density_rm==FALSE] <- 'keep'; density_rm[density_rm==TRUE] <- 'den'
    density_rm[ambig_rm=='amb'] <- 'amb' # an ambiguous point is still kept as ambiguity
    data$type[data$type!='oob'] <- density_rm

    # Method 3: Line membership	based on data_den
    data$line <- NA; data$line[data$type!='oob'] <- apply(data_den, 1, which.max)

    # mark out of reading bound fixations
    if (num_checkFirst != 0) { # for the first few fixations
      templine <- data$line[1:num_checkFirst]
      if (sum(unique(templine[!is.na(templine)])) != 1) {
        # there are no line 1 fixations, treating them as random elsewhere seeing.
        ind <- max(which(templine != 1), na.rm=TRUE)
        if (ind < num_checkFirst) {
          for (i in 1:ind) {
            if (!is.na(templine[i])) data$type[i] <- 'oob_r'
          }
        }
      }
    }
    if (num_checkLast != 0) { # for the last few fixations
      templine <- data$line[(nrow(data)-num_checkLast+1):nrow(data)]
      if (sum(unique(templine[!is.na(templine)])) != max(templine, na.rm=TRUE)) {
        # there are no last line fixations, treating them as random elsewhere seeing.
        # here, max(templine, na.rm=TRUE) is used instead of n_lines, in case that no fixation is categorized in the last line
        ind <- max(which(templine == max(templine, na.rm=TRUE)), na.rm=TRUE)
        if (ind < num_checkLast) {
          for (i in (nrow(data)-num_checkLast+ind+1):nrow(data)) {
            if (!is.na(templine[i-nrow(data)+num_checkLast])) data$type[i] <- 'oob_r'
          }
        }
      }
    }

    # Recategorize ambiguous pts based on surrounding fixations
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
        else if ((data$type[j] == 'amb') || (data$type[j] == 'oob_r')) { j = j - 1; next }
        break
      }
      # Go forwards to get line membership of next keeper
      j = i + 1
      repeat {
        if (j > length(data$type)) next_line = -1
        else if (data$type[j] == 'keep') next_line = data$line[j]
        else if (data$type[j] == 'oob') next_line = -1
        else if (data$type[j] == 'den') next_line = -1
        else if ((data$type[j] == 'amb') || (data$type[j] == 'oob_r')) { j = j + 1; next }
        break
      }

      # If both before and after are from the same line, reclassify
      if (prev_line == next_line && prev_line != -1) {
        data$type[i] = 'keep'; data$line[i] = prev_line
      }
    }
    # add new y position values
    data$y_line <- NA; data$y_line[data$type!='oob'] <- start_pts[data$line[data$type!='oob'],2]
    # add new y residule values
    data$y_res <- NA
    newparams <- rep(c(0,0,0), n_lines)
    for (i in 1:n_lines) {
      newparams[(i-1)*3+1] <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[(i-1)*3+1])
      newparams[(i-1)*3+2] <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[(i-1)*3+2])
      newparams[(i-1)*3+3] <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[(i-1)*3+3])
      k <- newparams[(i-1)*3+1]; o <- newparams[(i-1)*3+2]
      y_on_value <- o + k*(data$x_pos[(data$type!='oob') & (data$line==i)] - start_pts[i,1]) + start_pts[i,2]
      data$y_res[(data$type!='oob') & (data$line==i)] <- data$y_pos[(data$type!='oob') & (data$line==i)] - y_on_value
    }
    # Store fitted parameters and fit measures
    data$slope <- NA; data$offset <- NA; data$sd <- NA
    for (i in 1:n_lines) {
      data$slope[(data$type!='oob') & (data$line==i)] <- newparams[(i-1)*3+1]
      data$offset[(data$type!='oob') & (data$line==i)] <- newparams[(i-1)*3+2]
      data$sd[(data$type!='oob') & (data$line==i)] <- newparams[(i-1)*3+3]
    }
    data$fit_den <- fit_den; data$fit_y_diff <- fit_y_diff

    return(data) # Return data
  }
}


#' @title Function to optimize slope, offset and sd for all lines of fixations (extended from
#'     original paper)
#' @description This function uses many slopes, ver_offsets, sds for every lines, and use sum(min
#'     y_diff) as target measure for optimizationuses
#' @details This function uses many slopes, ver_offsets, sds for every lines, and use sum(min
#'     y_diff) as target measure for optimizationuses
#'
#' @param params A vector of parameters for optimization, the three values in it refer to slope, offset, and sd
#' @param fit_it A bollean variable; if it is TRUE, the function will return fit measure; if it is FALSE, the function will return fit information
#' @param data A data.frame storing the fixation data including at least the x_pos and y_pos of each fixation
#' @param start_pts A data.frame containing the starting position of each text line. It has three columns:
#'        \enumerate{
#'        \item x_pos: x position of the first word in each text line.
#'        \item y_pos: y position of the first word in each text line.
#'        \item trial_num: the trial number of the current start_pts
#'        }
#' @param k_bounds A list containing the lower and upper boundaries of slope;
#'                default value is [-0.1, 0.1]
#' @param o_bounds A list containing the lower and upper boundaries of offset;
#'                defaul value is [-0.5*dist of adjacent text lines, 0.5*dist of adjacent text line])
#' @param s_bounds A list containing the lower and upper boundaries of sd; default value is [1, 20]
#' @param den_sd_cutoff A float variable for cutoff threshold for density;
#'                If it is Inf, use mean(inv_dnorm(exp(data_den_max))) + 3*sd(inv_dnorm(exp(data_den_max))) as cutoff (99.7\% are accepted)
#' @param den_ratio_cutoff A float variable for cutoff threshold for density ratio (ratio between the maximum density and second maximum density)
#' @param num_checkFirst An integer denoting the number of starting fixations used for checking start-reading bound; default value is 5
#' @param num_checkLast An integer denoting the number of ending fixations used for checking end-reading bound; default value is 10
#'
#' @return A data.frame including fixation data, and fitting data including fit measures and fitted lines information.
#'        It adds the following columns:
#'        \enumerate{
#'        \item line: Text line that each fixation belongs to
#'        \item y_line: y position of the text line that each fixation is assigned to
#'        \item y_res: Residualized y position of each fixation y_line + y_res will give the original y position of each fixation
#'        \item slope: Optimized slope value for the fitted line that current fixation belongs to
#'        \item offset: Optimizaed offset value for the fitted line that current fixation belongs to
#'        \item sd: Optimized sd value for the fitted line that current fixation belongs to
#'        \item fit_den: fitted density value for the fitted line that current fixation belongs to
#'        \item fit_y_diff: fitted y difference for the fitted line that current fixation belongs to (accumulated y differences between each fixation and fitted lines)
#'        }
#' @author Tao Gong \email{gtojty@@gmail.com}
#' @export
cat_lines4 <- function(params,
                       fit_it=TRUE,
                       data,
                       start_pts,
                       k_bounds,
                       o_bounds,
                       s_bounds,
                       den_sd_cutoff,
                       den_ratio_cutoff,
                       num_checkFirst=5,
                       num_checkLast=10) {

  ys <- start_pts[,2] # The y-values for the lines
  n_lines <- length(ys) # The number of clusters is based off of the lines

  # Initialize matrices
  data_den <- matrix(numeric(0), nrow(data[data$type!='oob',]), n_lines)
  y_diff <- matrix(numeric(0), nrow(data[data$type!='oob',]), n_lines)

  for (l in 1:n_lines) {
    # Unpack the parameters
    k <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[(l-1)*3+1])
    o <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[(l-1)*3+2])
    s <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[(l-1)*3+3])

    y_on_line <- o + k*(data$x_pos[data$type!='oob'] - start_pts[l,1]) + start_pts[l,2] # The value of each point on each line
    data_den[,l] <- log(dnorm(data$y_pos[data$type!='oob'], mean=y_on_line, sd=s)) # Log density value for each point based on the line and sd
    y_diff[,l] <- abs(data$y_pos[data$type!='oob'] - y_on_line) # Store the difference between the real and fitted value
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
    data_den_sort <- t(apply(exp(data_den), 1, sort))
    # if one line has all 0, making them 1, so that data_den_ratio of that line is 1
    for (i in 1:nrow(data_den_sort)) {
      if (sum(data_den_sort[i,])==0) data_den_sort[i,] <- 1
    }
    data_den_ratio <- data_den_sort[,n_lines]/data_den_sort[,n_lines-1]

    # Mark ambigous points
    ambig_rm <- data_den_ratio <= den_ratio_cutoff
    ambig_rm[ambig_rm==FALSE] <- 'keep'; ambig_rm[ambig_rm==TRUE] <- 'amb'
    data$type[data$type!='oob'] <- ambig_rm

    # Mark points with very low density
    inv_dnorm <- function(x) {sqrt( -2*log(sqrt(2*pi) * x)) }
    mod_data_den <- inv_dnorm(exp(data_den_max))
    mod_data_den[mod_data_den==Inf] <- .Machine$integer.max
    if (den_sd_cutoff == Inf) den_sd_cutoff <- mean(mod_data_den) + 3*sd(mod_data_den)
    density_rm <- mod_data_den > den_sd_cutoff
    density_rm[density_rm==FALSE] <- 'keep'; density_rm[density_rm==TRUE] <- 'den'
    density_rm[ambig_rm=='amb'] <- 'amb' # an ambiguous point is still kept as ambiguity
    data$type[data$type!='oob'] <- density_rm

    # Method 4: Line membership	based on y_diff
    data$line <- NA; data$line[data$type!='oob'] <- apply(y_diff, 1, which.min)

    # mark out of reading bound fixations
    if (num_checkFirst != 0) { # for the first few fixations
      templine <- data$line[1:num_checkFirst]
      if (sum(unique(templine[!is.na(templine)])) != 1) {
        # there are no line 1 fixations, treating them as random elsewhere seeing.
        ind <- max(which(templine != 1), na.rm=TRUE)
        if (ind < num_checkFirst) {
          for (i in 1:ind) {
            if (!is.na(templine[i])) data$type[i] <- 'oob_r'
          }
        }
      }
    }
    if (num_checkLast != 0) { # for the last few fixations
      templine <- data$line[(nrow(data)-num_checkLast+1):nrow(data)]
      if (sum(unique(templine[!is.na(templine)])) != max(templine, na.rm=TRUE)) {
        # there are no last line fixations, treating them as random elsewhere seeing.
        # here, max(templine, na.rm=TRUE) is used instead of n_lines, in case that no fixation is categorized in the last line
        ind <- max(which(templine == max(templine, na.rm=TRUE)), na.rm=TRUE)
        if (ind < num_checkLast) {
          for (i in (nrow(data)-num_checkLast+ind+1):nrow(data)) {
            if (!is.na(templine[i-nrow(data)+num_checkLast])) data$type[i] <- 'oob_r'
          }
        }
      }
    }

    # Recategorize ambiguous pts based on surrounding fixations
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
        else if ((data$type[j] == 'amb') || (data$type[j] == 'oob_r')) { j = j - 1; next }
        break
      }
      # Go forwards to get line membership of next keeper
      j = i + 1
      repeat {
        if (j > length(data$type)) next_line = -1
        else if (data$type[j] == 'keep') next_line = data$line[j]
        else if (data$type[j] == 'oob') next_line = -1
        else if (data$type[j] == 'den') next_line = -1
        else if ((data$type[j] == 'amb') || (data$type[j] == 'oob_r')) { j = j + 1; next }
        break
      }

      # If both before and after are from the same line, reclassify
      if (prev_line == next_line && prev_line != -1) {
        data$type[i] = 'keep'; data$line[i] = prev_line
      }
    }
    # add new y position values
    data$y_line <- NA; data$y_line[data$type!='oob'] <- start_pts[data$line[data$type!='oob'],2]
    # add new y residule values
    data$y_res <- NA
    newparams <- rep(c(0,0,0), n_lines)
    for (i in 1:n_lines) {
      newparams[(i-1)*3+1] <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[(i-1)*3+1])
      newparams[(i-1)*3+2] <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[(i-1)*3+2])
      newparams[(i-1)*3+3] <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[(i-1)*3+3])
      k <- newparams[(i-1)*3+1]; o <- newparams[(i-1)*3+2]
      y_on_value <- o + k*(data$x_pos[(data$type!='oob') & (data$line==i)] - start_pts[i,1]) + start_pts[i,2]
      data$y_res[(data$type!='oob') & (data$line==i)] <- data$y_pos[(data$type!='oob') & (data$line==i)] - y_on_value
    }
    # Store fitted parameters and fit measures
    data$slope <- NA; data$offset <- NA; data$sd <- NA
    for (i in 1:n_lines) {
      data$slope[(data$type!='oob') & (data$line==i)] <- newparams[(i-1)*3+1]
      data$offset[(data$type!='oob') & (data$line ==i)] <- newparams[(i-1)*3+2]
      data$sd[(data$type!='oob') & (data$line==i)] <- newparams[(i-1)*3+3]
    }
    data$fit_den <- fit_den; data$fit_y_diff <- fit_y_diff

    return(data) # Return data
  }
}


#' @title This function draws original and modified fixations with fitted lines
#' @description This function is suitable for values optimized by cat_line functions; it can also draw results of hand-made categorization of fixations
#'
#' @details This function first draws the background text file, and then, draws different types of fixations categorized by hands or cat_line functions onto it
#'        And finally, it saves the plot.
#'
#' @param data A data.frame containing fixations and optimized information generated by different cut_line functions
#' @param start_pts A data.frame containing the starting position of each text line. It has three columns:
#'        \enumerate{
#'        \item x_pos: x position of the first word in each text line.
#'        \item y_pos: y position of the first word in each text line.
#'        \item trial_num: the trial number of the current start_pts
#'        }
#' @param output_filehead A string as the output figure file head, the remaining name is determined by draw_type, the output figure file is a png file
#' @param draw_type A string indicating the way of drawing. It can have the following three options:
#'        \enumerate{
#'          \item 'hand': hand-made fixation lines, output file ends with '_hand'
#'          \item 'original': original fixation lines, output file ends with '_ori'
#'          \item 'modified': residule fixation lines, output file ends with '_mod'
#'        }
#' @param bg_image_name Name of the background trial figure file (*.png); such file is optional, if no such file, the figure has a white background
#' @param image_width Integer indicating the width of image drawn; default value is 1280
#' @param image_height Integer indicating the height of the image drawn; default value is 1024
#'
#' @return None. A plot is automatically generated and stored.
#' @author Tao Gong \email{gtojty@@gmail.com}
#' @export
trial_plots <- function(data,
                        start_pts,
                        output_filehead,
                        draw_type,
                        bg_image_name=NULL,
                        image_width=1280,
                        image_height=1024) {

  pt_size_min = 1; pt_size_max = 4 # Constants
  n_lines <- nrow(start_pts) # Line info

  # Handle data
  dur_five_num <- fivenum(data$duration) # Get five number summary of duration
  # Point sizes based on duration
  m <- (pt_size_max - pt_size_min)/(dur_five_num[5] - dur_five_num[1])
  data$pt_size <- m*(data$duration - dur_five_num[1]) + pt_size_min

  # Open a plot device
  if (draw_type == 'hand') png(filename = paste(output_filehead, '_hand.png', sep=''), width = image_width, height = image_height, units = "px", res = 300, pointsize=4)
  if (draw_type == 'original') png(filename = paste(output_filehead, '_ori.png', sep=''), width = image_width, height = image_height, units = "px", res = 300, pointsize=4)
  if (draw_type == 'modified') png(filename = paste(output_filehead, '_mod.png', sep=''), width = image_width, height = image_height, units = "px", res = 300, pointsize=4)

  # Handle background picture
  if (is.null(bg_image_name)) plot(1, type='n', main='Fixations with Classifications', xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0)) # draw a blank plot
  else {
    # Is there a specific image for this subject and trial?
    if (file.exists(bg_image_name)) {
      t_image <- png::readPNG(bg_image_name) # Load the image
      t_image_height <- dim(t_image)[1]; t_image_width <- dim(t_image)[2]
      if (image_width == t_image_width & image_height == t_image_height) {
        # draw upon a background figure
        plot(1:2, type = 'n', main='Fixations with Classifications', xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
        lim <- par(); rasterImage(t_image, xleft = 0, ybottom = t_image_height, xright = t_image_width, ytop = 0)
      } else {
        warning(paste('Wrong resolution of background image for ', bg_image_name, sep='')) # If the image file is missing show the data without it
        # draw a blank plot; reverse y-limits so 0 at top
        plot(1, type='n', main='Fixations with Classifications', xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
      }
    } else {
      warning(paste('Missing background image for ', bg_image_name, sep='')) # If the image file is missing show the data without it
      # draw a blank plot; reverse y-limits so 0 at top
      plot(1, type='n', main='Fixations with Classifications', xlab='x_pos', ylab='y_pos', xlim=c(0, image_width), ylim=c(image_height, 0))
    }
  }

  if (draw_type == 'hand') {
    # for hand made categorization
    # separate different types of data
    data_hand <- data[data$hand_line %in% 1:n_lines,] # only draw hand categorized fixations
    data_oob_r <- data[data$hand_line==n_lines+1,] # end of reading fixations
    data_amb <- data[!(data$hand_line %in% 1:n_lines) & (data$hand_line!=n_lines+1),] # between line fixations

    # Drawing
    # 1) fixation ordering
    points(data$x_pos, data$y_pos, cex=data$pt_size, col='yellow', pch=1, type='l', lty='dashed')
    # 2) kept fixations
    for (i in 1:n_lines) {
      cat <- data_hand$hand_line == i
      points(data_hand$x_pos[cat], data_hand$y_pos[cat], cex=data_hand$pt_size[cat],
             col=c('blue', 'red')[i%%2+1], pch=1)
    }
    # 3) other types of fixations
    points(data_oob_r$x_pos, data_oob_r$y_pos, cex=data_oob_r$pt_size, col='cyan', pch=1)
    points(data_amb$x_pos, data_amb$y_pos, cex=data_amb$pt_size, col='purple', pch=1)
    # 4) fitted lines (base lines)
    for (i in 1:n_lines) {
      lines(c(start_pts[i,1], max(data$x_pos)), c(start_pts[i,2], start_pts[i,2]),
            col=c('blue', 'red')[i%%2+1])
    }
    # legends for fixations (no oob fixations)
    legend(x='topright',
           legend=c('Kept', 'Kept', 'Out-of-Reading', 'Ambiguous'),
           pch=c(1, 1, 1, 1), col=c('red', 'blue', 'cyan', 'purple'), cex = 1.2, bty='n')
  }

  if (draw_type == 'original') {
    # for original fixation lines
    # separate different types of data
    data_keep <- data[data$type == 'keep',] # normal fixations
    data_oob <-  data[data$type == 'oob',] # fixations out of text boundary
    data_oob_r <- data[data$type == 'oob_r',] # fixations out of reading boundary at the beginning and the ending
    data_den <-  data[data$type == 'den',] # fixations with low density
    data_amb <-  data[data$type == 'amb',] # ambiguous fixations

    # Drawing
    # 1) fixation ordering
    points(data$x_pos, data$y_pos, cex=data$pt_size, col='yellow', pch=1, type='l', lty='dashed')
    # 2) kept fixations
    for (i in 1:n_lines) {
      cat <- data_keep$line == i
      points(data_keep$x_pos[cat], data_keep$y_pos[cat], cex=data_keep$pt_size[cat],
             col=c('blue', 'red')[i%%2+1], pch=1)
    }
    # 3) deleted fixations
    points(data_oob$x_pos, data_oob$y_pos, cex=data_oob$pt_size, col='gray', pch=1)
    points(data_oob_r$x_pos, data_oob_r$y_pos, cex=data_oob_r$pt_size, col='cyan', pch=1)
    points(data_den$x_pos, data_den$y_pos, cex=data_den$pt_size, col='orange', pch=1)
    points(data_amb$x_pos, data_amb$y_pos, cex=data_amb$pt_size, col='purple', pch=1)
    # 4) fitted lines
    for (i in 1:n_lines) {
      slope <- data$slope[(i-1)*3+1]; offset <- data$offset[(i-1)*3+2]
      lines(c(start_pts[i,1], max(data$x_pos)), c(start_pts[i,2] + offset, start_pts[i,2] + slope*(max(data$x_pos) - start_pts[i,1]) + offset),
            col=c('blue', 'red')[i%%2+1])
    }
    # legends for fixations
    legend(x='topright',
           legend=c('Kept', 'Kept', 'Out-of-bounds', 'Out-of-Reading', 'Low density', 'Ambiguous'),
           pch=c(1, 1, 1, 1, 1, 1), col=c('red', 'blue', 'gray', 'cyan', 'orange', 'purple'), cex = 1.2, bty='n')
    # legends for line parameters
    legend(x='bottomright',
           legend=c(paste('Slope: ', round(data$slope[1], digits=3)),
                    paste('Offset: ', round(data$offset[2], digits=3)),
                    paste('SD: ', round(data$sd[3], digits=3)),
                    paste('Fit_den: ', round(data$fit_den[1], digits=3)),
                    paste('Fit_ydiff: ', round(data$fit_y_diff[1], digits=3))), cex = 1.2, bty='n')
  }

  if (draw_type == 'modified') {
    # for modified fixations
    # Separate out some more data
    data_keep <- data[data$type == 'keep',] # normal fixations
    data_oob_r <- data[data$type == 'oob_r',] # fixations out of reading boundary at the beginning and the ending
    data_den <-  data[data$type == 'den',] # fixations with low density
    data_amb <-  data[data$type == 'amb',] # ambiguous fixations

    # Drawing
    # 1) fixation ordering (no oob fixations)
    points(data$x_pos[data$type!='oob'], data$y_line[data$type!='oob'] + data$y_res[data$type!='oob'], cex=data$pt_size[data$type!='oob'], col='yellow', pch=1, type='l', lty='dashed')
    # 2) kept fixations
    for (i in 1:n_lines) {
      cat <- data_keep$line == i
      points(data_keep$x_pos[cat], data_keep$y_line[cat] + data_keep$y_res[cat], cex=data_keep$pt_size[cat],
             col=c('blue', 'red')[i%%2+1], pch=1)
    }
    # 3) deleted fixations
    points(data_oob_r$x_pos, data_oob_r$y_line + data_oob_r$y_res, cex=data_oob_r$pt_size, col='cyan', pch=1)
    points(data_den$x_pos, data_den$y_line + data_den$y_res, cex=data_den$pt_size, col='orange', pch=1)
    points(data_amb$x_pos, data_amb$y_line + data_amb$y_res, cex=data_amb$pt_size, col='purple', pch=1)
    # 4) fitted lines (base lines)
    for (i in 1:n_lines) {
      lines(c(start_pts[i,1], max(data$x_pos)), c(start_pts[i,2], start_pts[i,2]),
            col=c('blue', 'red')[i%%2+1])
    }
    # legends for fixations (no oob fixations)
    legend(x='topright',
           legend=c('Kept', 'Kept', 'Out-of-Reading', 'Low density', 'Ambiguous'),
           pch=c(1, 1, 1, 1, 1), col=c('red', 'blue', 'cyan', 'orange', 'purple'), cex = 1.2, bty='n')
    # legends for line parameters
    legend(x='bottomright',
           legend=c(paste('Slope: ', round(data$slope[1], digits=3)),
                    paste('Offset: ', round(data$offset[2], digits=3)),
                    paste('SD: ', round(data$sd[3], digits=3)),
                    paste('Fit_den: ', round(data$fit_den[1], digits=3)),
                    paste('Fit_ydiff: ', round(data$fit_y_diff[1], digits=3))), cex = 1.2, bty='n')
  }

  dev.off() # close plot
}
