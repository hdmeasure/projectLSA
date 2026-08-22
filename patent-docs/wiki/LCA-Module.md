# LCA Module — Latent Class Analysis

The LCA module identifies **latent subgroups (classes)** based on **categorical indicator variables**. It supports both standard LCA and multilevel LCA for hierarchical data structures.

---

## When to Use LCA

- Your indicator variables are **categorical** (binary, ordinal, or nominal)
- You want to classify individuals into unobserved groups based on response patterns
- Examples: identifying student learning types, diagnostic categories, behavioral profiles

---

## Workflow

### Step 1: Load Data

Upload your dataset with categorical variables. Variables should be coded as integers (e.g., 1, 2, 3).

### Step 2: Select Engine

| Engine | Package | Best For |
|--------|---------|----------|
| **poLCA** | `poLCA` | Standard polytomous LCA |
| **glca** | `glca` | Multilevel LCA with group-level variables |

### Step 3: Configure Model

| Parameter | Description |
|-----------|-------------|
| **Indicator Variables** | Categorical variables for classification |
| **Number of Classes** | Range to evaluate (e.g., 2–5) |
| **Group Variable** | (glca only) Variable identifying groups/clusters |

### Step 4: Run Analysis

Click **Run LCA** to estimate models for each specified number of classes.

### Step 5: Compare Models

| Index | Description | Selection Rule |
|-------|-------------|---------------|
| **AIC** | Akaike Information Criterion | Lower is better |
| **BIC** | Bayesian Information Criterion | Lower is better |
| **Log-Likelihood** | Model fit | Less negative is better |
| **Entropy** | Classification accuracy | Closer to 1.0 is better |
| **Class Proportions** | Relative size of each class | Avoid very small classes |

### Step 6: Visualize

- **Class profile plots** — Conditional item-response probabilities per class
- Interactive visualization via `ggiraph` with hover tooltips
- Easily interpret which response patterns characterize each class

### Step 7: Export

- Class membership probabilities
- Posterior classifications
- Model comparison table

---

## Standard LCA vs. Multilevel LCA

| Feature | Standard (poLCA) | Multilevel (glca) |
|---------|-----------------|-------------------|
| Data structure | Single level | Hierarchical (students in schools) |
| Group covariates | Not supported | Supported |
| Random effects | No | Yes |
| Best for | Simple classification | Nested data structures |

---

## Interpretation Tips

- Focus on **conditional probabilities** to name/interpret classes
- Classes with probability > 0.70 on an item indicate that response is characteristic of that class
- Consider both statistical fit (BIC) and **substantive interpretability**
- Ensure each class has a **sufficient sample size** for reliable estimation

---

## References

- Linzer, D. A., & Lewis, J. B. (2011). poLCA: An R Package for Polytomous Variable Latent Class Analysis. *Journal of Statistical Software*, 42(10), 1–29.
- Kim, S., & Kim, S. (2024). glca: An R Package for Multiple-Group Latent Class Analysis. *Applied Psychological Measurement*.

---

⬅️ Back to [[Home]]
