# Assignment 1 – Group 7

This project analyzes simulated 2024 profits for 26 Iceland fast-food branches to evaluate whether **improving staff retention** (lower turnover / higher experience) improves profitability, using regression models and visual analysis.

---

## Files
- `assignment-1.R` – full analysis script (**no console output**; all results saved to files)
- `data_group7.xlsx` – input dataset (must be in the project root)
- `outputs/` – folder automatically created with text/CSV results
- `outputs/figs/` – contains all generated figures (PNG format)

---

## Quick start

### Option 1 – Run in **RStudio**
1. Open the project folder in RStudio.  
2. Ensure R is installed (and internet available for first-time package installs).  
3. Make sure `data_group7.xlsx` is in the project root.  
4. Run the script:
   ```r
   source("assignment-1.R")
    ```

---

### Option 2 – Run in **VS Code**

1. Open the folder in VS Code.
2. Install the **R extension** (`REditorSupport.r`) and ensure R is in your system PATH.
3. Open `assignment-1.R`.
4. Run the full script using:

   * **Ctrl + Shift + S** (Source file), or
   * Type in the VS Code R terminal:

     ```r
     source("assignment-1.R")
     ```

---

### Option 3 – Run in **Terminal**

If you prefer command line execution:

```bash
Rscript assignment-1.R
```

All results will be written to:

* `outputs/` → CSV/TXT reports
* `outputs/figs/` → PNG plots

No output is printed in the terminal.

---

## Expected data structure

* **Categorical:**
  `region1`, `region2` (`Other` / `Capital Region`),
  `urban` (`No` / `Yes`), `branch_size` (`Medium` / `Small` / `Large`)
* **Numeric:**
  `profits`, `area_pop`, `staff_fte`, `staff_no`,
  `staff_turnover1`, `staff_turnover2`,
  `staff_mean_experience2`, `manager_experience`

All variable names are cleaned to `snake_case` using `janitor::clean_names()`.

---

## Output summary

* Clean data summaries and descriptive statistics
* Distribution, boxplot, and scatterplot visuals
* Model fitting (OLS, robust SEs, diagnostics)
* Model comparison (AIC/BIC/adj-R²)
* Robustness, influence, and “what-if” scenario analysis

Everything is stored automatically — **no manual saving or printing needed**.
