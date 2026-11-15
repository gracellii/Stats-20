library(shiny)
library(ggplot2)

hypotrochoid <- function(R, r, d, theta) {
  # a version of hypocycloid where you could choose 
  # the point is distance d from center of small circle
  stopifnot(R > r, r > 0)

  x <- (R - r) * cos(theta) + d * cos((R - r) / r * theta)
  y <- (R - r) * sin(theta) - d * sin((R - r) / r * theta)

  data.frame(theta = theta, x = x, y = y)
}

epitrochoids <- function(R, r, d, theta) {
  stopifnot(R > r, r > 0)

  x <- (R + r) * cos(theta) - d * cos((R + r) / r * theta)
  y <- (R + r) * sin(theta) - d * sin((R + r) / r * theta)

  data.frame(theta = theta, x = x, y = y)
}

# when R/r = m/n, where m and n are both integers
# theta = 2pi n completes a full cycle for both
gcd <- function(a, b) {
  while (b != 0) {
    remainder <- a %% b
    a <- b
    b <- remainder
  }
  a
}

function(input, output, session) {
  # the maximum time changes depending on the maximum number of revolutions
  # where an entire curve is drawn back at the exact same point
  n <- reactive({
    R_integer <- round(input$R * 10) # note we times 10 because in input the step is 0.1
    r_integer <- round(input$r * 10)

    r_integer / gcd(R_integer, r_integer)
  })
  playing <- reactiveVal(FALSE) # check if the animation is ongoing
  # playing () Read the current value; playing(TRUE/FALSE) sets the value
  output$play_pause_button <- renderUI({
  #dynamically change pause/play button label and icon 
  if (playing()) {
    actionButton( "play_pause", "Pause", icon = icon("pause"))
  } else {
    actionButton( "play_pause", "Play", icon = icon("play"))
  }
  })
  animation_time <- reactiveVal(0)
  last_slider_value <- reactiveVal(0) # this is for synchronizing time with animation time

  observeEvent(input$play_pause, {
    playing(!playing())
  })

  # changing any settings forces playback to stop and recheck the time animation bar
  observeEvent(
    list(input$R, input$r, input$d, input$type, input$duration),
    {
      playing(FALSE)

      t <- min(animation_time(), input$duration)
      animation_time(t)

      slider_time <- floor((t + 1e-9) / 0.1) * 0.1
      last_slider_value(slider_time)

      updateSliderInput(
        session, "time",
        max = input$duration,
        value = slider_time
      )
    },
    priority = 100
  )

  # Advance simulation time while playing.
  observe({
    if (!playing()) return()

    current_time <- isolate(animation_time())
    duration <- isolate(input$duration)

    if (current_time >= duration) {
      playing(FALSE)
    }

    invalidateLater(100, session)
    animation_time(
      min(current_time + 0.1, duration)
    )
  })

  # moves slider to reflect the animation time
  observe({
    t <- animation_time()
    slider_time <- floor((t + 1e-9) / 0.1) * 0.1

    if (abs(slider_time - isolate(last_slider_value())) > 1e-8) {
      last_slider_value(slider_time)

      updateSliderInput(
        session, "time",
        value = slider_time
      )
    }
  })

  observeEvent(input$time_seek, {
    playing(FALSE)

    animation_time(
      min(input$duration, max(0, input$time_seek))
    )
  })

  full_curve <- reactive({
    R <- input$R
    r <- input$r
    d <- input$d

    validate(need(R > r, "Choose R greater than r."))

    theta_end <- 2 * pi * n()

    theta <- seq(
      0, theta_end,
      length.out = max(2, ceiling(theta_end / 0.005) + 1)
    )

    if (input$type == "hypo") {
      curve <- hypotrochoid(R, r, d, theta)
      center_distance <- R - r
    } else {
      curve <- epitrochoids(R, r, d, theta)
      center_distance <- R + r
    }

    curve$cx <- center_distance * cos(theta)
    curve$cy <- center_distance * sin(theta)

    list(
      points = curve,
      center_distance = center_distance
    )
  })

  output$curve_description <- renderUI({
    if (input$type == "hypo") {
      div( class = "curve-description",
        "A", strong("hypotrochoid"), "is a curve traced by a point attached
        to a circle rolling", strong("inside"), "a larger static circle."
      )
    } else {
      div( class = "curve-description",
        "An",strong("epitrochoid"), "is a curve traced by a point attached
        to a circle rolling", strong("outside"), "a larger static circle."
      )
    }
  })

  output$plot <- renderPlot({
    R <- input$R
    r <- input$r
    d <- input$d

    result <- full_curve()
    all_points <- result$points
    center_distance <- result$center_distance

    # duration <- input$duration
    # omega <- 2 * pi * n() / duration
    # duration = n * T, where n is the period 
    # needed for the curve to fall back on its original point
    # theta_now <- omega * input$time
    # theta <- seq(
      # 0,
      # theta_now,
      # length.out = max(2, ceiling(theta_now / 0.01) + 1)
    # )

    fraction <- min(1, max(0, animation_time() / input$duration))
    # fraction of points visible
    last_index <- 1L + floor(fraction * (nrow(all_points) - 1L))
    # all_points is a data frame with one row per point, (theta, x, y)

    curve <- all_points[seq_len(last_index), ]
    # theta_now <- tail(curve$theta, 1)

    # if (input$type == "hypo") {
      # curve <- hypotrochoid(R, r, d, theta)
      # center_distance <- R - r
    # } else if (input$type == "epi") {
      # curve <- epitrochoids(R, r, d, theta)
      # center_distance <- R + r
    # }

    # cx <- center_distance * cos(theta_now)
    cx <- all_points$cx[last_index]
    # cy <- center_distance * sin(theta_now)
    cy <- all_points$cy[last_index]

    # Current tracing point.
    # px <- tail(curve$x, 1)
    px <- all_points$x[last_index]
    # py <- tail(curve$y, 1)
    py <- all_points$y[last_index]

    phi <- seq(0, 2 * pi, length.out = 150)
    # Fixed limits include both circles and the entire possible trace.
    extent <- max(R, center_distance + r,
                  center_distance + d) * 1.1

    fixed_circle <- data.frame(
      x = R * cos(phi),
      y = R * sin(phi)
    )

    rolling_circle <- data.frame(
      x = cx + r * cos(phi),
      y = cy + r * sin(phi)
    )

    p <- ggplot() +
      geom_path(
        data = fixed_circle,
        aes(x, y),
        color = "black",
        linewidth = 0.8
      ) +
      geom_path(
        data = rolling_circle,
        aes(x, y),
        color = input$circle_color,
        linewidth = 0.8
      ) +
      {
        if (nrow(curve) >= 2) {
          geom_path(
            data = curve,
            aes(x, y),
            color = input$curve_color,
            linewidth = 1.2
          )
        }
      } +
      annotate(
        "segment",
        x = cx, y = cy,
        xend = px, yend = py,
        color = input$circle_color,
        linewidth = 0.6
      ) +
      annotate(
        "point",
        x = cx, y = cy,
        color = "black",
        size = 2
      ) +
      annotate(
        "point",
        x = px, y = py,
        color = input$curve_color,
        size = 3
      ) +
      coord_fixed(
        xlim = c(-extent, extent),
        ylim = c(-extent, extent),
        expand = FALSE
      ) +
      theme_void() +
      theme(
        plot.background = element_rect(
          fill = "#FAFAFC", color = NA
        ),
        plot.margin = margin(15, 15, 15, 15)
      )

    print(p)
  })
}