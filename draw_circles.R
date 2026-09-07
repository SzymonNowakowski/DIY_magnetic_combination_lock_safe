library(showtext)
library(xml2)

# Register your local DIN Stencil file 
font_add(family = "din_stencil", regular = "DINSRG__.TTF")
# Convert text to vector outlines for laser compatibility
showtext_auto()


# --- Extract character geometry for numbers and letters ---
extract_char_paths <- function(chars = c(as.character(0:9), "A", "B", "C", "D", "E")) {
  char_paths <- list()
  
  for (ch in chars) {
    tmp_svg <- tempfile(fileext = ".svg")
    svg(tmp_svg, width = 2, height = 2)
    plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE, xlab = "", ylab = "")
    text(0, 0, labels = ch, cex = 5, family = "din_stencil")
    dev.off()
    
    doc <- read_xml(tmp_svg)
    all_paths <- xml_find_all(doc, ".//d1:path", xml_ns(doc))
    
    # Filter out clipPath and defs elements
    paths <- all_paths[sapply(all_paths, function(p) {
      parent_name <- xml_name(xml_parent(p))
      !parent_name %in% c("clipPath", "defs")
    })]
    
    all_x <- c()
    all_y <- c()
    
    for (p in paths) {
      d_attr <- xml_attr(p, "d")
      tokens <- unlist(regmatches(d_attr, gregexpr("[a-zA-Z]|[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?", d_attr)))
      
      i <- 1
      n <- length(tokens)
      curr_x <- 0; curr_y <- 0
      start_x <- 0; start_y <- 0
      sub_x <- c(); sub_y <- c()
      last_cmd <- ""
      
      while (i <= n) {
        cmd <- tokens[i]
        if (grepl("[a-zA-Z]", cmd)) {
          i <- i + 1
        } else {
          cmd <- last_cmd
        }
        last_cmd <- cmd
        
        if (cmd == "M" || cmd == "m") {
          px <- as.numeric(tokens[i]); py <- as.numeric(tokens[i+1]); i <- i + 2
          curr_x <- if(cmd == "m") curr_x + px else px
          curr_y <- if(cmd == "m") curr_y + py else py
          start_x <- curr_x; start_y <- curr_y
          
          if (length(sub_x) > 0) {
            all_x <- c(all_x, sub_x, NA); all_y <- c(all_y, sub_y, NA)
            sub_x <- c(); sub_y <- c()
          }
          sub_x <- c(sub_x, curr_x); sub_y <- c(sub_y, curr_y)
          
        } else if (cmd == "L" || cmd == "l") {
          px <- as.numeric(tokens[i]); py <- as.numeric(tokens[i+1]); i <- i + 2
          curr_x <- if(cmd == "l") curr_x + px else px
          curr_y <- if(cmd == "l") curr_y + py else py
          sub_x <- c(sub_x, curr_x); sub_y <- c(sub_y, curr_y)
          
        } else if (cmd == "C" || cmd == "c") {
          p1x <- as.numeric(tokens[i]);   p1y <- as.numeric(tokens[i+1])
          p2x <- as.numeric(tokens[i+2]); p2y <- as.numeric(tokens[i+3])
          p3x <- as.numeric(tokens[i+4]); p3y <- as.numeric(tokens[i+5])
          i <- i + 6
          
          if (cmd == "c") {
            p1x <- curr_x + p1x; p1y <- curr_y + p1y
            p2x <- curr_x + p2x; p2y <- curr_y + p2y
            p3x <- curr_x + p3x; p3y <- curr_y + p3y
          }
          
          t <- seq(0, 1, length.out = 15)[-1]
          bx <- (1-t)^3 * curr_x + 3*(1-t)^2 * t * p1x + 3*(1-t) * t^2 * p2x + t^3 * p3x
          by <- (1-t)^3 * curr_y + 3*(1-t)^2 * t * p1y + 3*(1-t) * t^2 * p2x + t^3 * p3y
          
          sub_x <- c(sub_x, bx); sub_y <- c(sub_y, by)
          curr_x <- p3x; curr_y <- p3y
          
        } else if (cmd == "Z" || cmd == "z") {
          sub_x <- c(sub_x, start_x); sub_y <- c(sub_y, start_y)
          curr_x <- start_x; curr_y <- start_y
        }
      }
      
      if (length(sub_x) > 0) {
        all_x <- c(all_x, sub_x, NA); all_y <- c(all_y, sub_y, NA)
      }
    }
    
    unlink(tmp_svg)
    
    # Center glyph geometry based on bounding box
    valid_idx <- !is.na(all_x)
    mid_x <- (max(all_x[valid_idx]) + min(all_x[valid_idx])) / 2
    mid_y <- (max(all_y[valid_idx]) + min(all_y[valid_idx])) / 2
    
    norm_x <- (all_x - mid_x) / 100
    norm_y <- -(all_y - mid_y) / 100
    
    char_paths[[as.character(ch)]] <- list(x = norm_x, y = norm_y)
  }
  return(char_paths)
}

# Pre-load outlines for digits and letters (use LETTERS for all A-Z)
chars_to_extract <- c(as.character(0:9), "A", "B", "C", "D", "E")
char_data <- extract_char_paths(chars = chars_to_extract)




# Draw pure stroke lines directly onto active PDF plot ---
draw_char_outline <- function(char, x, y, scale = 1, col = "black", lwd = 1) {
  path <- char_data[[as.character(char)]]
  if (is.null(path)) return()
  
  px <- x + (path$x * scale)
  py <- y + (path$y * scale)
  
  # lines() only draws stroke paths (wireframes) and cannot fill shapes
  lines(px, py, col = col, lwd = lwd)
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
#
# At ten equally spaced clock-face positions around the circle, draw digits at radius r_digits
#
# Finally, mark the circle with a mark 
draw_circle <- function(xc, yc, r,
                        indentation_vector, side_length,
                        rs, ds, small_circle_vector,
                        ra,
                        r_digits,
                        mark = NA, mark_high_low = 0) {
  
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
  
  # ------------------------------------------------------------
  # 6. Digits 0-9
  # ------------------------------------------------------------
  if (r_digits > 0)
    for (i in seq_len(n_pos)) {
      
      x <- xc + r_digits * cos(theta[i])
      y <- yc + r_digits * sin(theta[i])
      
      draw_char_outline(char = i - 1, x = x, y = y, scale = 20, col = "black")
    }
  
  # ------------------------------------------------------------
  # 7. Mark
  # ------------------------------------------------------------
  if (!is.na(mark))
    draw_char_outline(char = mark, x = xc+ra+4, y = yc, scale = 13, col = "black")
    
  if (mark_high_low > 0) {
    draw_line(c(xc, yc + r), c(xc, yc + r - mark_high_low))
    draw_line(c(xc, yc - r), c(xc, yc - r + mark_high_low))
  }
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


open.pdf("design_PDFs/circles.pdf", 220, 150, 10)

draw_circle(
  xc = 40,
  yc = 40,
  r = 30,
  indentation_vector = c(10, rep(5, 9)),
  side_length = 9,
  rs = 3.1/2,
  ds = 25,
  small_circle_vector = rep(FALSE, 10),
  ra = 19/2,
  r_digits = 0,
  mark ='A'
)

draw_circle(
  xc = 110,
  yc = 40,
  r = 30,
  indentation_vector = c(10, rep(5, 9)),
  side_length = 9,
  rs = 3.1/2,
  ds = 25,
  small_circle_vector = rep(TRUE, 10),
  ra = 19/2,
  r_digits = 0,
  mark ='B'
)

draw_circle(
  xc = 180,
  yc = 40,
  r = 30,
  indentation_vector = rep(0, 10),
  side_length = 9,
  rs = 3.1/2,
  ds = 25,
  small_circle_vector = rep(TRUE, 10),
  ra = 19/2,
  r_digits = 0,
  mark ='C'
)

draw_circle(
  xc = 40,
  yc = 110,
  r = 30,
  indentation_vector = rep(0, 10),
  side_length = 9,
  rs = 3.1/2,
  ds = 25,
  small_circle_vector = c(T, F, F, T, F, F, T, F, F, F),
  ra = 14/2,
  r_digits = 0,
  mark ='D', mark_high_low=2
)

draw_circle(
  xc = 110,
  yc = 110,
  r = 30,
  indentation_vector = rep(0, 10),
  side_length = 9,
  rs = 3.1/2,
  ds = 25,
  small_circle_vector = rep(FALSE, 10),
  ra = 14/2,
  r_digits = 22,
  mark = NA, mark_high_low=2
)

# this circle shows all, make sure things don't overlap
# draw_circle(
#   xc = 180,
#   yc = 110,
#   r = 30,
#   indentation_vector = rep(10, 10),
#   side_length = 9,
#   rs = 3.1/2,
#   ds = 25,
#   small_circle_vector = rep(TRUE, 10),
#   ra = 14/2,
#   r_digits = 22,
#   mark = NA, mark_high_low=2
# )

close.pdf()
