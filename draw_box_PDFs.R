

#all sizes in mm
feather_width_mm <- 20

# the below specs must be even multiplicities of feather length
case_width_in_feathers <- 14
case_depth_in_feathers <- 8
case_height_in_feathers <- 8

smaller_magnet_3mm_radius <- 10

if (case_depth_in_feathers %% 2 != 0 | case_width_in_feathers %% 2 !=0 |
    case_height_in_feathers %% 2 != 0 )
  stop("The dimensions must be even multiplicities of the feather length")

if (case_depth_in_feathers <= 0 | case_width_in_feathers <=0 |
    case_height_in_feathers <= 0 )
  stop("The dimensions must be positive multiplicities of the feather length")


draw_arc <- function(xc, yc, r, alpha = 0, beta=2*pi, n = 256) {
  th <- seq(alpha, beta, length.out = n)
  x <- xc + r * cos(th)
  y <- yc + r * sin(th)
  lines(x, y)
}

draw_straight_line <- function(segment_cnt, coordinate, current_pos, increase_on_first_feather_side, increase_on_feather_width, smaller_first, smaller_last, plywood_start_drawing, plywood_finish_drawing, skip_feather = FALSE) {
  new_pos <- current_pos
  direction <- 1
  for (i in 1:segment_cnt) {
    if ((smaller_last && i==segment_cnt) | (smaller_first && i==1)) {
      if (smaller_last && i==segment_cnt) {
        plywood_thickness_mm <- plywood_finish_drawing
      } else {
        plywood_thickness_mm <- plywood_start_drawing
      }
      
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * (feather_width_mm - plywood_thickness_mm)
    } else {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * feather_width_mm
    }
    
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
  }
  
  if (!skip_feather) {
    #and a final go up or down as if it were a one large feather
    new_pos[3-coordinate] <- current_pos[3-coordinate] + (-1)^(increase_on_first_feather_side + direction) * plywood_finish_drawing
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
  }
  
  return(current_pos)
}


draw_feathered_line <- function(segment_cnt, coordinate, current_pos, increase_on_first_feather_side, increase_on_feather_width, smaller_first, smaller_last, plywood_start_drawing, plywood_main, plywood_finish_drawing) {
  new_pos <- current_pos
  direction <- 1
  for (i in 1:segment_cnt) {
    if ((smaller_last && i==segment_cnt) | (smaller_first && i==1)) {
      if (smaller_last && i==segment_cnt) {
        plywood_thickness_mm <- plywood_finish_drawing
      } else {
        plywood_thickness_mm <- plywood_start_drawing
      }
        
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * (feather_width_mm - plywood_thickness_mm)
    } else {
      new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * feather_width_mm
    }
    
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
    
    if (i==segment_cnt) 
      return(current_pos)
    
    new_pos[3-coordinate] <- current_pos[3-coordinate] + (-1)^(increase_on_first_feather_side + direction) * plywood_main
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
    direction <- direction + 1
  }
  return(current_pos)
}

draw_back <- function(width_segment_count, height_segment_count, plywood_back, plywood_left, plywood_front, plywood_right) {
  current_pos <- c(00,00+max(plywood_back, plywood_left, plywood_front, plywood_right))
  
  current_pos <- draw_feathered_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE, plywood_left, plywood_front, plywood_right)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE, plywood_front, plywood_right, plywood_back)
  current_pos <- draw_feathered_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE, plywood_right, plywood_back, plywood_left)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE, plywood_back, plywood_left, plywood_front)
  
} 



draw_wall_with_straight_bottom <- function(width_segment_count, height_segment_count, plywood_top, plywood_left, plywood_right) {
  plywood_bottom <- plywood_right # it is required so dimensions are kept, the bottom line goes straight
  current_pos <- c(0,0)
  # if the last segment is NOT A TOOTH, we should stop drawing a "plywood thickness" before it is finished,
  # so the next perpendicular not-a-tooth segment can be started with a small decline
  current_pos <- draw_straight_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=TRUE, smaller_first=FALSE, smaller_last=TRUE, plywood_left, plywood_right)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=TRUE, increase_on_feather_width=TRUE, smaller_first=TRUE, smaller_last=FALSE, plywood_bottom, plywood_right, plywood_top)
  current_pos <- draw_feathered_line(width_segment_count, 1, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=FALSE, smaller_first=FALSE, smaller_last=TRUE, plywood_right, plywood_top, plywood_left)
  current_pos <- draw_feathered_line(height_segment_count,2, current_pos, increase_on_first_feather_side=FALSE, increase_on_feather_width=FALSE, smaller_first=TRUE, smaller_last=FALSE, plywood_top, plywood_left, plywood_bottom)
  
} 

# Draw rectangle by low-left corner, width w, height h
draw_rectangle <- function(x0, y0, w, h) {
  x1 <- x0 + w; y1 <- y0 + h
  lines(c(x0, x1), c(y0, y0))  
  lines(c(x1, x1), c(y0, y1))  
  lines(c(x1, x0), c(y1, y1))  
  lines(c(x0, x0), c(y1, y0))  
}


draw_line <- function(A, B) {
  lines(c(A[1], B[1]), c(A[2], B[2]))
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


open.pdf("design_PDFs/plywood_3mm_case_top.pdf", case_width_in_feathers*feather_width_mm+10, case_depth_in_feathers*feather_width_mm+10, 5)
draw_wall_with_straight_bottom(case_width_in_feathers, case_depth_in_feathers, 10,9,10)
close.pdf()

open.pdf("design_PDFs/plywood_10mm_case_left.pdf", case_height_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_wall_with_straight_bottom(case_height_in_feathers, case_depth_in_feathers, 10,3,9)
close.pdf()

open.pdf("design_PDFs/plywood_6mm_case_right_A.pdf", case_height_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_wall_with_straight_bottom(case_height_in_feathers, case_depth_in_feathers, 10,9,3)
close.pdf()

open.pdf("design_PDFs/plywood_3mm_case_right_B.pdf", case_height_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_wall_with_straight_bottom(case_height_in_feathers, case_depth_in_feathers, 10,9,3)
close.pdf()


open.pdf("design_PDFs/plywood_3mm_case_bottom_A.pdf", case_width_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_wall_with_straight_bottom(case_width_in_feathers, case_depth_in_feathers, 10,10,9)
close.pdf()

open.pdf("design_PDFs/plywood_3mm_case_bottom_B.pdf", case_width_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_wall_with_straight_bottom(case_width_in_feathers, case_depth_in_feathers, 10,10,9)
draw_arc(280-22.5, 160-22.5, smaller_magnet_3mm_radius+0.05)
close.pdf()

open.pdf("design_PDFs/plywood_3mm_case_bottom_C.pdf", case_width_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_wall_with_straight_bottom(case_width_in_feathers, case_depth_in_feathers, 10,10,9)
close.pdf()


open.pdf("design_PDFs/plywood_10mm_case_back.pdf", case_width_in_feathers*feather_width_mm+10, case_height_in_feathers*feather_width_mm+10, 10)
draw_back(case_width_in_feathers, case_height_in_feathers, 3,10,9,9)
close.pdf()





