# LPA Module — Latent Profile Analysis

The LPA module identifies **latent subgroups (profiles)** within a population based on **continuous indicator variables**. It uses mixture modeling to classify individuals into homogeneous groups that share similar response patterns.

---

## When to Use LPA

- Your indicator variables are **continuous** (scale/interval data)
- You want to identify **unobserved subgroups** in your sample
- Examples: identifying student learning profiles, consumer segments, psychological profiles

---

## Workflow

### Step 1: Load Data

Upload your dataset or select a built-in example. Ensure your variables are **numeric/continuous**.

### Step 2: Select Variables

Choose the indicator variables you want to use for profile identification. The module validates that selected variables are numeric and displays descriptive statistics.

### Step 3: Configure Model

| Parameter | Description | Options |
|-----------|-------------|---------|
| **Number of Profiles** | Range of profiles to evaluate | e.g., 2–6 |
| **Model Type** | Variance-covariance structure | See below |

#### Model Types (Variance-Covariance Structures)

| Model | Variances | Covariances |
|-------|-----------|-------------|
| Model 1 | Equal | Zero |
| Model 2 | Varying | Zero |
| Model 3 | Equal | Equal |
| Model 6 | Varying | Varying |

### Step 4: Run Analysis

Click **Run LPA** to estimate all models. The system fits multiple mixture models using the `tidyLPA` and `mclust` backends.

### Step 5: Compare Models

The model comparison table displays:

| Index | Description | Selection Rule |
|-------|-------------|---------------|
| **AIC** | Akaike Information Criterion | Lower is better |
| **BIC** | Bayesian Information Criterion | Lower is better |
| **SABIC** | Sample-size adjusted BIC | Lower is better |
| **Entropy** | Classification accuracy | Closer to 1.0 is better |
| **Log-Likelihood** | Model fit | Less negative is better |

### Step 6: Visualize

- **Profile plots** — Mean values across indicator variables for each profile
- Interactive plots with hover tooltips via `plotly`
- Customizable class names and color schemes

### Step 7: Export

- Download profile membership assignments
- Export descriptive statistics by profile
- Download model comparison table

---

## Interpretation Tips

- **Entropy > 0.80** indicates acceptable classification accuracy
- **BIC** is generally preferred for model selection over AIC
- Check that each profile has a **meaningful proportion** of cases (avoid very small classes < 5%)
- Profile solutions should be **substantively interpretable**

---

## Computational Backend

| Package | Role |
|---------|------|
| `tidyLPA` | High-level interface for LPA |
| `mclust` | Gaussian mixture modeling engine |

---

## References

- Rosenberg, J. M., Beymer, P. N., Anderson, D. J., Van Lissa, C. J., & Schmidt, J. A. (2018). tidyLPA: An R Package to Easily Carry Out Latent Profile Analysis. *JOSS*, 3(30), 978.
- Nylund-Gibson, K., & Choi, A. Y. (2018). Ten frequently asked questions about latent class analysis. *Translational Issues in Psychological Science*, 4(4), 440–461.

---

⬅️ Back to [[Home]]
