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
