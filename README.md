# 628hw4
628hw4Spotify
# Podcast Episode Clustering and Visualization

This repository contains all files and resources for the project on podcast episode clustering and visualization. The project aims to construct two novel metrics for podcast episodes, categorize episodes by duration and thematic content, and build an interactive Shiny app for exploring these metrics.

---

## Repository Structure

### 1. `data`
This folder contains the dataset used in the project:
- **`seed9999_no_kmeans有时长合理版.csv`**: Raw and preprocessed podcast data retrieved from Spotify's API. The data includes episode durations, thematic content, and metadata for clustering.

### 2. `code`
This folder contains all the R code for the analysis and visualization:
- **`Api.py`**: Code for getting the data from the spotify web.
- **`kmeans_time_metric.R`**: Code for constructing the time-based metric using KMeans clustering.
- **`lda_topic_model.R`**: Code for constructing the thematic metric using LDA (Latent Dirichlet Allocation).
- **`shiny_app.R`**: The Shiny app code for interactive exploration of the metrics.

### 3. `images`
This folder contains images generated during the analysis and screenshots of the Shiny app:
- **Example plots and clustering results**: Generated during data exploration and analysis.
- **Shiny app screenshots**: Visuals of the user interface and outputs.

### 4. `summary`
This folder contains the project summary and explanation:
- **`project_summary.md`**: A concise explanation of the constructed metrics, clustering approach, and findings.

### 5. `README.md`
This file summarizes the contents of the repository and provides directions for use.

---
