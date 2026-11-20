library(shiny)
library(colourpicker)

fluidPage(

  tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var slider = $('#time').data('ionRangeSlider');
        var originalChange = slider.options.onChange;

        slider.update({
          onChange: function(data) {
            if (originalChange) {
              originalChange.call(this, data);
            }

            Shiny.setInputValue(
              'time_seek',
              data.from,
              {priority: 'event'}
            );
          }
        });
      });
    ")),

  titlePanel("Cycloidal Curves"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "type", "Curve type",
        choices = c(
          "Hypotrochoid" = "hypo",
          "Epitrochoid" = "epi",
          "Pericycloid"= "peri",
          "Evolvent" = "evol",
          "Cycloid" = "cyc"
        )
      ),
      uiOutput("curve_description"),

      h4("Curve Parameters"),
      sliderInput(
        "R", "Fixed-circle radius (R)",
        min = 1, max = 10, value = 3, step = 0.1
      ),
      sliderInput(
        "r", "Rolling-circle radius (r)",
        min = 0.1, max = 10, value = 1, step = 0.1
      ),
      sliderInput(
        "d", "Tracing-point distance (d)",
        min = 0, max = 10, value = 1, step = 0.1
      ),
      actionButton(
        "make_cycloid",
        "Trace circumference (d = r)"
      ),
      h4("Appearance"),
      colourInput(
        "circle_color",
        "Rolling circle color",
        value = "#3B82F6",
        showColour = "both"
      ),
      colourInput(
        "curve_color",
        "Traced curve color",
        value = "#E85D75",
        showColour = "both"
      )
    ),


    mainPanel(
      plotOutput("plot", height = "550px"),

      sliderInput(
        "duration", "Duration to complete the entire curve",
        min = 1, max = 100, value = 10, step = 1,
        post = " s"
      ),

      sliderInput(
        "time", "Time elapsed",
        min = 0, max = 10, value = 0, step = 0.1,
        post = " s"
      ),
      actionButton("play_pause", "Play", icon = icon("play"))
    )
  ),    
  tags$style(HTML("
      .curve-description {
        padding: 10px 12px;
        margin: 10px 0 20px 0;
        border-radius: 4px;
        color: #334155;
        font-size: 13px;
        line-height: 1.5;
      }
  "))
  
)

