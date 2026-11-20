# Stats 20 Shiny App

An interactive R Shiny application for generating cycloidal curves with customizable graphs.

## Requirements

- [R](https://cran.r-project.org/)
- R packages: `shiny`, `ggplot2`, `colourpicker`

Install the packages once in R:

```r
install.packages(c("shiny", "ggplot2","colourpicker"))
```

## Run the app

1. Open a terminal in VS Code.
2. Navigate to the folder containing **both** `ui.R` and `server.R`:

   ```bash
   cd Stats-20
   ```

   Skip this step if your terminal is already in that folder.

3. Launch the app:

   ```bash
   Rscript -e "shiny::runApp('.', launch.browser=TRUE)"
   ```

   The app should open in your browser. If it doesn't, visit the local URL printed in the terminal (for example, `http://127.0.0.1:3838`).

4. To stop the app, press **Ctrl+C** in the terminal.

## Alternative: Run from an R session

From the app directory, start R:

```bash
R
```

Then run:

```r
shiny::runApp()
```

You can quit R's terminal session by typing `q()`.

## Project structure

```text
Stats-20/
├── ui.R       # User interface
├── server.R   # Server logic
└── README.md  # Setup and run instructions
```

> **Note:** Don't run `Rscript server.R` by itself. `server.R` defines the server logic; `shiny::runApp()` starts the complete application using `ui.R` and `server.R` together.
