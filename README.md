# covid-gathering-size

Replication files for:

Boyer, C., Rumpler, E., Kissler, S., & Lipsitch, M. (2022). *Epidemics*, 40, 100620. https://doi.org/10.1016/j.epidem.2022.100620

## Abstract
Social gatherings can be an important locus of transmission for many pathogens including SARS-CoV-2. During an outbreak, restricting the size of these gatherings is one of several non-pharmaceutical interventions available to policy-makers to reduce transmission. Often these restrictions take the form of prohibitions on gatherings above a certain size. While it is generally agreed that such restrictions reduce contacts, the specific size threshold separating "allowed" from "prohibited" gatherings often does not have a clear scientific basis, which leads to dramatic differences in guidance across location and time. Building on the observation that gathering size distributions are often heavy-tailed, we develop a theoretical model of transmission during gatherings and their contribution to general disease dynamics. We find that a key, but often overlooked, determinant of the optimal threshold is the distribution of gathering sizes. Using data on pre-pandemic contact patterns from several sources as well as empirical estimates of transmission parameters for SARS-CoV-2, we apply our model to better understand the relationship between restriction threshold and reduction in cases. We find that, under reasonable transmission parameter ranges, restrictions may have to be set quite low to have any demonstrable effect on cases due to relative frequency of smaller gatherings. We compare our conceptual model with observed changes in reported contacts during lockdown in March of 2020.

## Repository Structure

```
├── __master_run.R              # Main script to reproduce all results
├── 0_functions/                # Helper functions
│   ├── simulation_functions.R  # Monte Carlo simulation functions
│   ├── plot_functions.R        # Plotting utilities
│   └── alternative_model.R     # Alternative modeling approaches
├── 1_data/                     # Data sources
│   ├── BBC_Pandemic/           # BBC Pandemic study contact data
│   ├── COMIX/                  # COMIX survey data
│   └── Sekara/                 # Sekara et al. gathering size data
├── 2_code/                     # Analysis scripts
│   ├── 0_packages.R            # Required R packages
│   ├── 1_functions.R           # Core mathematical functions
│   ├── 2_theory.R              # Theoretical model implementation
│   ├── 3_empirical_distributions.R  # Empirical data analysis
│   ├── 4_empirical_restrictions.R   # Restriction effectiveness analysis
│   └── 5_tau_sensitivity.R     # Sensitivity analysis
├── 3_results/                  # Generated figures and results
└── 4_manuscript/               # LaTeX manuscript files
```

## Requirements

### R Version
This code was developed and tested with R version 4.0+.

### Required R Packages
- `tidyverse` - Data manipulation and visualization
- `magrittr` - Pipe operators
- `ggpubr` - Publication-ready plots
- `poweRlaw` - Power law distribution functions
- `readxl` - Excel file reading
- `scales` - Plot scaling functions
- `progressr` - Progress bars for long computations
- `furrr` - Parallel processing

Install all dependencies by running:
```r
install.packages(c("tidyverse", "magrittr", "ggpubr", "poweRlaw", 
                   "readxl", "scales", "progressr", "furrr"))
```

## How to Run

### Quick Start
To reproduce all main results, simply run:
```r
source("__master_run.R")
```

This will execute the complete analysis pipeline and generate all figures in the `3_results/` directory.

### Step-by-Step Execution
For more control over the analysis, you can run individual components:

1. **Load packages and functions:**
   ```r
   source("2_code/0_packages.R")
   source("2_code/1_functions.R")
   ```

2. **Generate theoretical results (Figure 1):**
   ```r
   source("2_code/2_theory.R")
   ```

3. **Analyze empirical distributions (Figures 3-4):**
   ```r
   source("2_code/3_empirical_distributions.R")
   ```

4. **Evaluate restriction effectiveness (Figures 5-6):**
   ```r
   source("2_code/4_empirical_restrictions.R")
   ```

5. **Run sensitivity analyses (Supplementary figures):**
   ```r
   source("2_code/5_tau_sensitivity.R")
   ```

## Key Model Components

### Theoretical Framework
The core model calculates the expected number of secondary infections per gathering of size *k*:

```
E[X|K=k] = k × p_s × (1 - (1-τ)^(k×π))
```

Where:
- *k* = gathering size
- *τ* = transmission probability per contact
- *π* = proportion infectious in population
- *p_s* = proportion susceptible in population

### Data Sources
1. **BBC Pandemic Study**: Pre-pandemic contact patterns from the UK
   - Klepac, P., Kissler, S., & Gog, J. (2018). Contagion! The BBC Four Pandemic – The model behind the documentary. *Epidemics*, 24, 49-59.

2. **CoMix Survey**: Contact patterns during COVID-19 lockdowns  
   - CMMID COVID-19 working group, Jarvis, C. I., Van Zandvoort, K., Gimma, A., Prem, K., Klepac, P., Rubin, G. J., & Edmunds, W. J. (2020). Quantifying the impact of physical distance measures on the transmission of COVID-19 in the UK. *BMC Medicine*, 18(1), 124.

3. **Sekara et al.**: Large-scale gathering size distributions
   - Sekara, V., Stopczynski, A., & Lehmann, S. (2016). Fundamental structures of dynamic social networks. *Proceedings of the National Academy of Sciences*, 113(36), 9977-9982.

### Key Functions
- `expected_rate_per_gathering()`: Calculates transmission risk per gathering
- `restricted_rpldis()`: Simulates gathering sizes under restrictions
- `monte_carlo_simulation()`: Estimates reduction in cases under restrictions

## Output

All figures are saved as PDF files in the `3_results/` directory:

- `expected_infections_example.pdf`: Theoretical relationship between gathering size and infections
- `fig3a.pdf`, `fig3b.pdf`: Empirical gathering size distributions
- `fig4.pdf`: Fitted power law parameters
- `fig6.pdf`: Effectiveness of gathering size restrictions
- `rr_kmax_*.pdf`: Restriction effectiveness under various scenarios
- `*_sensitivity.pdf`: Sensitivity analysis results

## Citation

If you use this code, please cite:

```bibtex
@article{boyer2022epidemics,
  title={The effect of gathering restrictions on the spread of COVID-19},
  author={Boyer, Christopher and Rumpler, Elisabeth and Kissler, Stephen M and Lipsitch, Marc},
  journal={Epidemics},
  volume={40},
  pages={100620},
  year={2022},
  publisher={Elsevier},
  doi={10.1016/j.epidem.2022.100620}
}
```

## Contact

For questions about the code or analysis, please contact:
- Christopher Boyer: cboyer@hsph.harvard.edu

## License

This code is provided under MIT License for academic and research use.
