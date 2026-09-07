#all sizes in mm
feather_width_mm <- 10
plywood_thickness_mm <- 3

# the below specs must be even multiplicities of feather length
case_width_in_feathers <- 26
case_depth_in_feathers <- 16
base_height_in_feathers <- 8
lid_height_in_feathers <- 2

if (feather_width_mm <= plywood_thickness_mm)
  stop("Feather length must be greater than plywood thickness")

if (case_depth_in_feathers %% 2 != 0 | case_width_in_feathers %% 2 !=0 |
    base_height_in_feathers %% 2 != 0 | lid_height_in_feathers %% 2 != 0)
  stop("The dimensions must be even multiplicities of the feather length")

if (case_depth_in_feathers <= 0 | case_width_in_feathers <=0 |
    base_height_in_feathers <= 0 | lid_height_in_feathers <= 0)
  stop("The dimensions must be positive multiplicities of the feather length")


draw_straight_line <- function(segment_cnt, coordinate, current_pos, increase_on_first_feather_side, increase_on_feather_width, smaller_first, smaller_last, skip_feather = FALSE) {
  new_pos <- current_pos
  direction <- 1
  for (i in 1:segment_cnt) {
    if ((smaller_last && i==segment_cnt) | (smaller_first && i==1)) {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * (feather_width_mm - plywood_thickness_mm)
    } else {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * feather_width_mm
    }
    
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
  }
  
  if (!skip_feather) {
    #and a final go up or down as if it were a one large feather
    new_pos[3-coordinate] <- current_pos[3-coordinate] + (-1)^(increase_on_first_feather_side + direction) * plywood_thickness_mm
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
  }
  
  return(current_pos)
}


draw_feathered_line <- function(segment_cnt, coordinate, current_pos, increase_on_first_feather_side, increase_on_feather_width, smaller_first, smaller_last) {
  new_pos <- current_pos
  direction <- 1
  for (i in 1:segment_cnt) {
    if ((smaller_last && i==segment_cnt) | (smaller_first && i==1)) {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * (feather_width_mm - plywood_thickness_mm)
    } else {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * feather_width_mm
    }
    
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
    
    if (i==segment_cnt) 
      return(current_pos)
    
    new_pos[3-coordinate] <- current_pos[3-coordinate] + (-1)^(increase_on_first_feather_side + direction) * plywood_thickness_mm
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
    direction <- direction + 1
  }
  return(current_pos)
}

draw_top_or_bottom <- function(width_segment_count, height_segment_count) {
  current_pos <- c(0,plywood_thickness_mm)
  
  current_pos <- draw_feathered_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE)
  
} 



draw_side_wall <- function(width_segment_count, height_segment_count) {
  current_pos <- c(0,0)
  # if the last segment is NOT A TOOTH, we should stop drawing a "plywood thickness" before it is finished,
  # so the next perpendicular not-a-tooth segment can be started with a small decline
  current_pos <- draw_straight_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=TRUE, smaller_first=TRUE, smaller_last=FALSE)
  current_pos <- draw_feathered_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=FALSE, smaller_first=TRUE, smaller_last=FALSE)
  
} 

# Draw rectangle by low-left corner, width w, height h
draw_rectangle <- function(x0, y0, w, h) {
  x1 <- x0 + w; y1 <- y0 + h
  lines(c(x0, x1), c(y0, y0))  
  lines(c(x1, x1), c(y0, y1))  
  lines(c(x1, x0), c(y1, y1))  
  lines(c(x0, x0), c(y1, y0))  
}


draw_outer_back<- function(width_segment_count, height_segment_count) {
  draw_rectangle(2, 2, width_segment_count*feather_width_mm, height_segment_count*feather_width_mm)
}

draw_inner_back<- function(width_segment_count, height_segment_count) {
  current_pos <- c(plywood_thickness_mm,0)
  current_pos <- draw_straight_line(width_segment_count, 1, current_pos, increase_on_feather_width=TRUE, smaller_first=TRUE, smaller_last=TRUE, skip_feather=TRUE)
  current_pos <- draw_straight_line(height_segment_count,2, current_pos, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE, skip_feather=TRUE)
  current_pos <- draw_straight_line(width_segment_count, 1, current_pos, increase_on_feather_width=FALSE, smaller_first=TRUE, smaller_last=TRUE, skip_feather=TRUE)
  current_pos <- draw_straight_line(height_segment_count,2, current_pos, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE, skip_feather=TRUE)
  
} 

# Draw an arc centered at (xc, yc), radius r,
# from angle alpha to beta using only lines().
draw_arc <- function(xc, yc, r, alpha, beta, n = 64) {
  th <- seq(alpha, beta, length.out = n)
  x <- xc + r * cos(th)
  y <- yc + r * sin(th)
  lines(x, y)
}

draw_line <- function(A, B) {
  lines(c(A[1], B[1]), c(A[2], B[2]))
}

# Draw a circle centered at (xc, yc), radius r.
#
# At ten equally spaced clock-face positions around the circle,
# draw rectangular indentations specified by indentation_vector,
# with side_length specifying the length of their outer side.
#
# The outer side of each rectangle lies on the circumference.
# The other two sides are parallel to the radius passing through
# the midpoint of the outer side.
#
# At the ten equally spaced half-step positions in our
# 10-position division, draw small circles of radius rs,
# with their centers at distance ds from the center.
#
# In the middle, draw an additional circle of radius ra.
draw_circle <- function(xc, yc, r,
                        indentation_vector,
                        side_length,
                        rs,
                        ds,
                        small_circle_vector,
                        ra) {
  
  n_pos <- 10
  
  stopifnot(length(indentation_vector) == n_pos)
  stopifnot(length(small_circle_vector) == n_pos)
  
  # ------------------------------------------------------------
  # 1. Clock-face positions
  # ------------------------------------------------------------
  
  # 12 o'clock = pi/2, then clockwise, one more than hourly points for the closure
  theta <- pi / 2 - (seq(0, len=n_pos+1)) * 2 * pi / n_pos
  
  # Angular spacing between positions
  dtheta <- 2 * pi / n_pos
  
  # ------------------------------------------------------------
  # 2. Outer circle, excluding rectangular indentations
  # ------------------------------------------------------------
  
  # For each position, determine whether an indentation exists.
  #
  # If there is no indentation, draw the whole corresponding
  # circular segment.
  #
  # If there is an indentation, leave a gap corresponding to
  # its outer side.
  
  alpha <- atan2(side_length / 2, r)  
  beta <- asin(side_length/2 / r)
  r_prime <- sqrt(side_length^2 / 4 + r^2)
  AB_length <- r_prime*cos(alpha) - r*cos(beta)
  for (i in seq_len(n_pos)) {
    pA <- c(xc + r_prime * cos(theta[i]+alpha), yc + r_prime * sin(theta[i]+alpha))
    pB <- c(xc + r * cos(theta[i]+beta), yc + r * sin(theta[i]+beta))
    pC <- pB + (pB-pA) * (indentation_vector[i] - AB_length)/AB_length
    
    pD <- c(xc + r_prime * cos(theta[i]-alpha), yc + r_prime * sin(theta[i]-alpha))
    pE <- c(xc + r * cos(theta[i]-beta), yc + r * sin(theta[i]-beta))
    pF <- pE + (pE-pD) * (indentation_vector[i] - AB_length)/AB_length
    
    if (indentation_vector[i] > 0) {
      #connect B, C, F, E
      draw_line(pB, pC)
      draw_line(pC, pF)
      draw_line(pF, pE)
    } else {
      draw_arc(xc, yc, r, theta[i] - beta, theta[i] + beta, n = 32)
    }
    
    # now draw the fragment of arc until the next clock-time position is reached
    draw_arc(xc, yc, r, theta[i+1] + beta, theta[i] - beta, n = 32)
  }
  
  
  # ------------------------------------------------------------
  # 4. Small circles at half-step positions
  # ------------------------------------------------------------
  
  # Shift by half of one 10-position interval = 18 degrees.
  theta_small <- theta - dtheta / 2
  
  for (i in seq_len(n_pos)) {
    
    if (small_circle_vector[i]) {
      
      # Center of the small circle
      x <- xc + ds * cos(theta_small[i])
      y <- yc + ds * sin(theta_small[i])
      
      draw_arc(x, y, rs, 0, 2 * pi, n = 64)
    }
  }
  
  
  # ------------------------------------------------------------
  # 5. Central circle
  # ------------------------------------------------------------
  
  draw_arc(xc, yc, ra, 0, 2 * pi, n = 256)
}

open.pdf <- function(title, width_in_mm, height_in_mm, margin_in_mm) {
  pdf(file=title, width=(width_in_mm + 2 * margin_in_mm) / 25.4, height=(height_in_mm + 2 * margin_in_mm) / 25.4 )   #units: inches
  par(mai=c(margin_in_mm / 25.4, margin_in_mm / 25.4, margin_in_mm / 25.4, margin_in_mm / 25.4))  #mai - margins in inches
  plot.new()
  plot.window(c(0, width_in_mm), c(0, height_in_mm), asp=1, xaxs="i", yaxs="i")  #="i" to avoid scale by 4%
}

close.pdf <- function() {
  dev.off()
}


open.pdf("design_PDFs/base_both_sides.pdf", case_depth_in_feathers*feather_width_mm, base_height_in_feathers*feather_width_mm, 5)
draw_side_wall(case_depth_in_feathers, base_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/base_front_or_back.pdf", case_width_in_feathers*feather_width_mm, base_height_in_feathers*feather_width_mm, 10)
draw_side_wall(case_width_in_feathers, base_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/base_outer_back.pdf", case_width_in_feathers*feather_width_mm+10, base_height_in_feathers*feather_width_mm+10, 10)
draw_outer_back(case_width_in_feathers, base_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/base_bottom.pdf", case_width_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_top_or_bottom(case_width_in_feathers, case_depth_in_feathers)
close.pdf()

open.pdf("design_PDFs/lid_both_sides.pdf", case_depth_in_feathers*feather_width_mm, lid_height_in_feathers*feather_width_mm, 10)
draw_side_wall(case_depth_in_feathers, lid_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/lid_front_or_back.pdf", case_width_in_feathers*feather_width_mm, lid_height_in_feathers*feather_width_mm, 10)
draw_side_wall(case_width_in_feathers, lid_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/lid_outer_back.pdf", case_width_in_feathers*feather_width_mm+10, lid_height_in_feathers*feather_width_mm+10, 10)
draw_outer_back(case_width_in_feathers, lid_height_in_feathers)
close.pdf()

open.pdf("design_PDFs/lid_top.pdf", case_width_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_top_or_bottom(case_width_in_feathers, case_depth_in_feathers)
close.pdf()




open.pdf("design_PDFs/circles.pdf", 100, 100, 10)
draw_circle(
  50, 50, 30,
  c(10, rep(5, 9)),
  9,
  1.5,
  25,
  rep(TRUE, 10),
  9.5
)
close.pdf()
