# CFA/SEM Module — Confirmatory Factor Analysis & Structural Equation Modeling

The CFA/SEM module is the most comprehensive analytical tool in projectLSA. It supports **model specification**, **estimation**, **evaluation**, **iterative refinement**, **path diagram visualization**, and **automated report generation**.

---

## When to Use CFA/SEM

- You have a **hypothesized measurement model** to test (CFA)
- You want to test **structural relationships** between latent variables (SEM)
- You need to evaluate **construct validity** (AVE, CR, HTMT)
- You want publication-ready path diagrams and fit reports
- Examples: scale validation, theoretical model testing, mediation/moderation analysis

---

## CFA vs SEM

| Feature | CFA | SEM |
|---------|-----|-----|
| **Purpose** | Test measurement model | Test structural + measurement model |
| **Regression paths** | None (factors only) | Yes (factor → factor) |
| **Syntax operator** | `=~` only | `=~` and `~` |
| **Auto-detection** | ✅ projectLSA detects automatically | ✅ Based on presence of `~` |

---

## Workflow

### Step 1: Load Data

Upload your dataset with **continuous variables** that serve as indicators for your latent factors.

### Step 2: Specify Model

Use the **lavaan syntax editor** to define your model:

```
# CFA Example — Define latent factors
Factor1 =~ item1 + item2 + item3
Factor2 =~ item4 + item5 + item6

# SEM Example — Add structural paths
Factor2 ~ Factor1
```

#### Syntax Operators

| Operator | Meaning | Example |
|----------|---------|---------|
| `=~` | Factor loading (is measured by) | `F1 =~ x1 + x2 + x3` |
| `~` | Regression path | `F2 ~ F1` |
| `~~` | Covariance / variance | `F1 ~~ F2` |

### Step 3: Select Estimation Options

| Estimator | Best For |
|-----------|----------|
| **ML** | Normal continuous data |
| **MLR** | Non-normal data (robust SE) |
| **WLSMV** | Ordinal/categorical indicators |
| **ULS** | Small samples |

#### Missing Data Options

| Method | Description |
|--------|-------------|
| Listwise | Remove cases with any missing data |
| Pairwise | Use available data for each pair |
| FIML | Full Information Maximum Likelihood (recommended) |

### Step 4: Run Analysis

Click **Run CFA** to estimate the model using `lavaan`.

### Step 5: Evaluate Model Fit

The module displays fit indices with automatic interpretation:

| Index | Good Fit | Acceptable Fit | Interpretation |
|-------|----------|---------------|----------------|
| **χ²** p-value | > .05 | — | Non-significant = good |
| **RMSEA** | < .06 | < .08 | Root Mean Square Error |
| **CFI** | > .95 | > .90 | Comparative Fit Index |
| **TLI** | > .95 | > .90 | Tucker-Lewis Index |
| **SRMR** | < .08 | < .10 | Standardized Root Mean Residual |
| **GFI** | > .95 | > .90 | Goodness-of-Fit Index |
| **NFI** | > .95 | > .90 | Normed Fit Index |

### Step 6: Review Parameters

- **Standardized loadings** — Factor-indicator relationships
- **Standard errors** and **z-values** — Significance testing
- **R²** — Variance explained for each indicator
- **Modification indices** — Suggestions for model improvement

### Step 7: Construct Validity

| Metric | Threshold | Interpretation |
|--------|-----------|----------------|
| **AVE** (Average Variance Extracted) | ≥ .50 | Convergent validity established |
| **CR** (Composite Reliability) | ≥ .70 | Internal consistency adequate |
| **HTMT** (Heterotrait-Monotrait Ratio) | < .85 | Discriminant validity established |

### Step 8: Model Refinement

1. Modify your model based on fit indices and modification indices
2. Re-run the analysis
3. The **Model History** system automatically tracks all versions
4. Compare fit indices across all model iterations in the comparison table

---

## Path Diagram Visualization

### Customization Options

| Setting | Options |
|---------|---------|
| **Layout** | tree, tree2, tree3, spring, circle |
| **Color Palette** | 12 presets: Blue-Yellow, Ocean, Forest, Rainbow, Pastel, Greyscale, Earth, Vibrant, Monochrome, Sunset, Rose, Mint |
| **Node sizes** | Separate controls for latent and manifest variables |
| **Edge labels** | Standardized or unstandardized coefficients |
| **Significance markers** | Show * on significant paths |
| **Residuals** | Show/hide residual variances |

### Fit Index Display

- Selected fit indices are displayed in a **centered box** below the diagram
- Choose which indices to show via **checkbox toggles**
- Supports configurable **decimal separator** (period or comma)

---

## Decimal Separator

A unique feature for international users:
- Switch between **period** (0.95) and **comma** (0,95)
- Applies to: fit indices, path labels, tables, and reports
- Set in the sidebar before running analysis

---

## Automated HTML Report

Generate a publication-ready report containing:

- ✅ Automatic model-type detection (CFA vs SEM)
- ✅ Narrative interpretation of fit indices
- ✅ Model comparison table
- ✅ Side-by-side path diagrams (initial vs final model)
- ✅ Standardized parameter estimates table
- ✅ Construct validity diagnostics (AVE, CR, HTMT)
- ✅ APA formatting with Times New Roman
- ✅ Configurable decimal separator

---

## Model History

The system maintains a **complete history** of all fitted models:

- Each model is automatically indexed
- Add **notes** to document changes between iterations
- Compare fit indices side-by-side
- Delete models without affecting indexing
- Navigate between model versions

---

## Computational Backend

| Package | Role |
|---------|------|
| `lavaan` | SEM estimation engine |
| `semPlot` | Path diagram generation |
| `semptools` | Path diagram customization |
| `semTools` | Advanced diagnostics (HTMT, etc.) |
| `flextable` | APA-formatted tables |
| `rmarkdown` | HTML report generation |

---

## References

- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. *Journal of Statistical Software*, 48(2), 1–36.
- Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis. *Structural Equation Modeling*, 6(1), 1–55.
- Brown, T. A. (2015). *Confirmatory Factor Analysis for Applied Research* (2nd ed.). Guilford Press.

---

⬅️ Back to [[Home]]
