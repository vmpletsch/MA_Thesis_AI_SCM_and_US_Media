# **NOTE** 
This GitHub repository is not yet finalized and is subject to change.

# MA_Thesis_AI_SCM_and_US_Media
MAPSS Master Thesis for the University of Chicago. A Diachronic Analysis of Artificial Intelligence in U.S. Media in accordance with the Stereotype Content Model (2000 to 2024). Includes analysis of news articles and transcripts in R of CNN, Fox News, MS Now, NBC, the New York Times, and the Wall Street Journal. 

**Author:** Veronica Pletsch  
**Program:** MAPSS, University of Chicago  
**Date:** May 2026  
**Advisor:** [David Peterson, PhD]
## Citation Information
If you use code or methods from this repository, please cite:

Pletsch, V. (2026). A diachronic analysis of artificial intelligence in U.S. media [MA Thesis]. Department of Sociology, University of Chicago.

## Overview
This study applies the Stereotype Content Model (Fiske, Cuddy, Glick, & Xu, 2002) 
to a corpus of approximately 26,000 AI-referencing documents drawn from six major 
U.S. news organizations: CNN, FOX, MSNBC, NBC, NYT, and WSJ. Using the SADCAT 
dictionary (Nicolas, Bai, & Fiske, 2021), each document is scored along warmth 
and competence dimensions, and framing patterns are analyzed across medium, 
political orientation, source, and time.

### Research Questions
1. How has the framing of AI on SCM warmth and competence dimensions changed in 
   U.S. news media between 2000 and 2024?
2. Which SCM quadrant is AI framing within U.S. news media most predominantly 
   situated in across medium, political orientation, news source, and time?
3. How does AI framing within U.S. news media vary depending on the news 
   organization's political orientation and medium?
   
### Data Sources
- **NYT, WSJ, NBC:** Web-scraped from each organization's website
- **CNN, FOX, MSNBC:** Harvard Dataverse transcripts (Sood, 2017, 2022; 
  Sood & Laohaprapanon, 2022)

### Accessing the Data
The Harvard Dataverse transcripts are publicly available at:
- CNN: https://doi.org/10.7910/DVN/ISDPJU
- Fox News: https://doi.org/10.7910/DVN/Q2KIES
- MSNBC: https://doi.org/10.7910/DVN/UPJDE1

- The NYT, WSJ, and NBC data were manually web scraped for academic research and are not redistributed here.

  
## Requirements

### R Version
This project uses R version 4.3.0 or later.

### Required Packages
```r
library(tidyverse)        # Data manipulation and visualization
library(quanteda)         # Text analysis
library(tidytext)         # Tidy text processing
library(text2vec)         # Text vectorization
library(tokenizers)       # Tokenization
library(lubridate)        # Date handling
library(rstatix)          # Statistical tests and effect sizes
library(flextable)        # Publication-quality tables
library(ggrepel)          # Non-overlapping plot labels
```

### SADCAT Dictionary

The SADCAT dictionary (Nicolas, Bai, & Fiske, 2021) is available at 
https://osf.io/yx45f/. Download and place the dictionary files in 
`02_sadcat_pipeline/dictionaries/` before running the scoring pipeline. It is not redistributed here.


## Reproducing the Analysis

```bash
# 1. Clean source data and build the master corpus
Rscript 01_preprocessing/01_clean_source.R

# 2. Run Frequency Analysis
Rscript 02_Frequency

# 3. Run statistical analyses
Rscript 03_Comparative_Analysis

# 4. Run Word Embeddings (Optional)
Rscript 04_Word_Embeddings

```

**Note:** The full SADCAT pipeline may take several hours to run on the complete analysis, depending on computer storage and corpus size. Corpus size will multiply depending on the number of documents and context windows. To ensure R did not crash, outputs are chunked into 29 RDS 
files (5,000 windows each saved incrementally).

## Methodological Notes

### AI Detection Regex
The AI-referencing detection uses a strict word-boundary regex to minimize false positives:

```r
ai_pattern <- "\\bartificial intelligence\\b|\\bAI\\b"
```

### Aggregation Level
All statistical tests are conducted at the **source-year level** (n = 117 
source-year cells, filtered to those with ≥ 5 documents). Document-level 
testing inflates statistical power due to large sample sizes (~26,000 
documents); source-year aggregation provides more conservative and 
interpretable results.

### Non-Parametric Tests
Given the bounded nature of the dimensional scores and the modest sample size at 
the source-year level, non-parametric tests were used throughout:

- **Mann-Whitney U** for medium comparison (two groups)
- **Kruskal-Wallis** for political orientation and source (three+ groups)
- **Spearman's ρ** for temporal trends (monotonic, no linearity assumption)


## Contact
Veronica Pletsch  
Email: [vmpletsch@gmail.com]  
GitHub: [vmpletsch]
Website: vmpletsch.github.io
