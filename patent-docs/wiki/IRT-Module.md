# IRT Module — Item Response Theory

The IRT module provides a comprehensive interface for fitting and evaluating **item response models**. It supports both unidimensional and multidimensional models for dichotomous and polytomous items.

---

## When to Use IRT

- You want to evaluate the **psychometric quality** of test/survey items
- You need to estimate **person ability** (latent trait) scores
- You want to analyze item difficulty, discrimination, and guessing parameters
- Examples: educational testing, psychological scale validation, adaptive testing

---

## Supported Models

### Dichotomous Items (0/1 responses)

| Model | Parameters | Description |
|-------|-----------|-------------|
| **Rasch** | Difficulty only | All items equally discriminating |
| **2PL** | Difficulty + Discrimination | Items vary in discrimination |
| **3PL** | Difficulty + Discrimination + Guessing | Accounts for guessing behavior |

### Polytomous Items (ordered categories)

| Model | Description |
|-------|-------------|
| **GRM** | Graded Response Model — ordered category thresholds |
| **GPCM** | Generalized Partial Credit Model — step parameters |
| **PCM** | Partial Credit Model — constrained GPCM |

### Multidimensional Models

- Supports exploratory and confirmatory multidimensional IRT
- 3D surface plots and heatmaps for joint information

---

## Workflow

### Step 1: Load Data

Upload item response data. Items should be coded as integers:
- **Dichotomous:** 0 (incorrect) and 1 (correct)
- **Polytomous:** 0, 1, 2, 3, ... (ordered categories)

### Step 2: Select Model Type

Choose the appropriate model based on your item type and research goals.

### Step 3: Run Analysis

Click **Run IRT** to estimate the model using the `mirt` package.

### Step 4: Review Results

#### Item Parameters

| Parameter | Symbol | Description |
|-----------|--------|-------------|
| Difficulty | *b* | Location on the trait continuum where P(correct) = 0.50 |
| Discrimination | *a* | How well the item differentiates between ability levels |
| Guessing | *c* | Lower asymptote (probability of correct response by guessing) |

#### Diagnostic Plots

- **Item Characteristic Curves (ICC)** — Probability of correct response across ability levels
- **Item Information Curves (IIC)** — How much information each item provides
- **Test Information Function (TIF)** — Total information across the ability range
- **Conditional SEM** — Standard error of measurement at each ability level

#### Multidimensional Diagnostics

- **3D Surface Plots** — Joint information across two latent dimensions
- **Heatmaps** — Information density visualization

### Step 5: Person Scoring

The module computes **Expected A Posteriori (EAP)** factor scores for each respondent, which can be downloaded for further analysis.

---

## Interpretation Guide

### Item Discrimination (*a*)

| Value | Interpretation |
|-------|---------------|
| < 0.50 | Very low — consider removing |
| 0.50–0.99 | Low |
| 1.00–1.49 | Moderate |
| 1.50–1.99 | High |
| ≥ 2.00 | Very high |

### Item Difficulty (*b*)

- Typically ranges from **−3 to +3** on the logit scale
- Negative values = easy items
- Positive values = difficult items
- Good tests have items spread across the difficulty range

### Item Fit

- Check that items fit the chosen model
- Misfit items may need to be revised or removed

---

## Computational Backend

| Package | Role |
|---------|------|
| `mirt` | Full-information IRT estimation (MML, EM algorithm) |

---

## References

- Chalmers, R. P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. *Journal of Statistical Software*, 48(6), 1–29.
- van der Linden, W. J. (Ed.). (2018). *Handbook of Item Response Theory*. Chapman & Hall/CRC.

---

⬅️ Back to [[Home]]
