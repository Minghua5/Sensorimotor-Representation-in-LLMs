# Sensorimotor-Representation-in-LLMs


This repository contains the **data** and **prompts** for the research project: **"How does fine-tuning improve sensorimotor representations in large language models?"**.

The core codebase for LLM training and evaluation can be found in a separate, dedicated repository:  
**👉 [Psycholinguistics Framework](https://github.com/WordsGPT/psycholinguistics_framework.git)**

---

## 📁 Repository Structure

This repository is organized as follows:

| Folder/File | Description |
| :--- | :--- |
| **`/data`** | Contains the raw and processed datasets used for **fine-tuning** and **testing** the models. |
| **`/prompts`** | Stores the prompt templates used for **interacting with the large language models** across different experimental conditions. |
| **`/model_iutputs`** | Holds the **organized outputs** from model evaluation. |
| **`/analysis`** | Contains Jupyter Notebooks and scripts for statistical analysis: <br> • **`RSA_analysis.ipynb`**: Representational Similarity Analysis comparing human and model matrices. <br> • **`word_wise_analysis.ipynb`**: Word-level Euclidean similarity, boxplots, radar charts, and T-tests. <br> • **`dimension_analysis.ipynb`**: Analysis of correlation patterns across sensorimotor dimensions. <br> • **`rcomparison.r`**: R script using `cocor` for statistical comparison of correlations at the dimension level. |
