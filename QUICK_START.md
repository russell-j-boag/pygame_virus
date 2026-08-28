---
output:
  pdf_document: default
  html_document: default
---
# Virus Task: Quick Start

## Prerequisite

Make sure **R and RStudio** are installed on the computer before starting.

## First-time setup

1. Open `pygame_virus_aid_onset.Rproj` in RStudio. This ensures that R is working from the correct project folder.
2. If the R package **reticulate** is not already installed, run this once in the R console:

   ```r
   install.packages("reticulate")
   ```

3. Open `0-setup_pygame.R` and run the entire script (click **Source** in RStudio). The script creates the `r-pygame` Python environment and installs Pygame. An internet connection is required, and the first setup may take a few minutes.
4. Wait until the script finishes and displays the Python configuration. You only need to repeat this setup if the environment is removed or an installation error occurs.

## Run the experiment

1. Open `1-run_virus_task.R` and run the entire script by clicking **Source**.
2. The pre-task instructions will open first. Follow the on-screen prompts to complete them.
3. The task will then open full-screen and ask for a numeric participant number. Enter the assigned number carefully; it determines the participant's block order and response-key mapping.
4. Continue through the practice block and all three main blocks. Use the `D` and `J` keys as shown in the on-screen instructions.
5. At the final **EXPERIMENT COMPLETE** screen, press `Esc` to close the task.

Results are saved automatically as CSV files in the project's `output/` folder. Do not move or rename these files during a session.

## If something goes wrong

- If R reports that it cannot find `0-helpers.R` or the `python/` files, reopen `pygame_virus_aid_onset.Rproj` and try again.
- For an emergency exit at any point, press `Option + Q` on macOS or `Alt + Q` on Windows/Linux. Data from an interrupted block may be incomplete, so tell the experiment supervisor.
