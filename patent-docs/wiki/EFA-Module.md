# EFA Module — Exploratory Factor Analysis

The EFA module provides a structured workflow for discovering the **latent factor structure** underlying a set of observed variables. It guides users through diagnostic testing, factor extraction, and rotation.

---

## When to Use EFA

- You want to **explore** the underlying structure of your data (no prior hypothesis)
- You need to determine **how many factors** explain the correlations among variables
- You are developing or refining a questionnaire/scale
- Examples: survey development, construct exploration, data reduction

---

## Workflow

### Step 1: Load Data

Upload your dataset with **continuous variables**. EFA works best with interval/ratio scale data.

### Step 2: Preliminary Diagnostics

The module automatically computes:

#### Kaiser-Meyer-Olkin (KMO) Test

| KMO Value | Interpretation |
|-----------|---------------|
| ≥ 0.90 | Marvelous |
| 0.80–0.89 | Meritorious |
| 0.70–0.79 | Middling |
| 0.60–0.69 | Mediocre |
| 0.50–0.59 | Miserable |
| < 0.50 | Unacceptable — do not proceed |

#### Bartlett's Test of Sphericity

- Tests whether the correlation matrix is significantly different from an identity matrix
- Significant result (*p* < .05) indicates that factor analysis is appropriate
- Non-significant result suggests variables are uncorrelated and EFA is not suitable

#### Correlation Matrix Heatmap

- Visual inspection of inter-variable correlations
- Look for clusters of moderately correlated variables

### Step 3: Determine Number of Factors

#### Parallel Analysis

The module implements **Horn's parallel analysis**:
- Compares observed eigenvalues against eigenvalues from random data
- Factors are retained when observed eigenvalues **exceed** random eigenvalues
- Generally considered the **most accurate** method for determining factor number

#### Scree Plot

- Visual plot of eigenvalues in descending order
- Look for the "elbow" point where eigenvalues level off

### Step 4: Factor Extraction and Rotation

#### Extraction Methods

| Method | Description | Best For |
|--------|-------------|----------|
| **Principal Axis (PA)** | Iterative communality estimation | General use |
| **Maximum Likelihood (ML)** | Assumes multivariate normality | Normal data |
| **Minimum Residual (minres)** | Minimizes residual correlations | Non-normal data |

#### Rotation Methods

| Method | Type | Description |
|--------|------|-------------|
| **Varimax** | Orthogonal | Maximizes simple structure, factors uncorrelated |
| **Promax** | Oblique | Allows correlated factors |
| **Oblimin** | Oblique | Direct oblimin rotation |

> **Tip:** Use **oblique rotation** (promax/oblimin) when you expect factors to be correlated. Use **orthogonal rotation** (varimax) when you want uncorrelated factors.

### Step 5: Review Results

#### Factor Loading Matrix

- Loadings represent the correlation between each variable and each factor
- Loadings below a user-specified threshold are suppressed for clarity
- **Rule of thumb:** Loadings ≥ 0.40 are considered meaningful

#### Additional Output

| Output | Description |
|--------|-------------|
| **Communalities** | Proportion of variance explained by the factors for each variable |
| **Factor Correlations** | Correlations between factors (oblique rotation only) |
| **Factor Scores** | Estimated scores for each respondent on each factor |
| **Variance Explained** | Cumulative proportion of variance explained |

### Step 6: Export

- Factor loading matrix
- Factor scores for each respondent
- Clean HTML summary report

---

## Interpretation Tips

- **Cross-loading items** (loading ≥ 0.30 on multiple factors) may need to be removed
- Each factor should have **at least 3 items** loading on it
- **Total variance explained** should ideally be ≥ 50%
- Name factors based on the content of their highest-loading items
- Run **CFA** to confirm your EFA-derived factor structure on a separate sample

---

## Computational Backend

| Package | Role |
|---------|------|
| `psych` | Factor extraction, rotation, diagnostics, parallel analysis |

---

## References

- Revelle, W. (2025). psych: Procedures for Psychological, Psychometric, and Personality Research. Northwestern University.
- Watkins, M. W. (2018). Exploratory Factor Analysis: A Guide to Best Practice. *Journal of Black Psychology*, 44(3), 219–246.

---

⬅️ Back to [[Home]]
