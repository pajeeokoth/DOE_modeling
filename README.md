# DOE_modeling
Design of Experiments (DOE) Modeling

A toolkit for building and comparing surrogate/metamodels from designed experiments, with a focus on **small datasets** typical of physical experiments or expensive simulations.

---

## Table of Contents

1. [Overview](#overview)
2. [Supported DOE Designs](#supported-doe-designs)
3. [Modeling Methods](#modeling-methods)
   - [Response Surface Methodology (RSM)](#response-surface-methodology-rsm)
   - [Gaussian Process / Kriging (GP)](#gaussian-process--kriging-gp)
   - [Artificial Neural Networks / Deep Learning (ANN/DL)](#artificial-neural-networks--deep-learning-anndl)
4. [Method Comparison for Small Datasets](#method-comparison-for-small-datasets)
5. [Choosing a Method by DOE Design](#choosing-a-method-by-doe-design)
6. [Getting Started](#getting-started)
7. [References](#references)

---

## Overview

Design of Experiments (DOE) provides structured approaches to collecting data efficiently. Once the experimental runs are completed, a **surrogate model** (metamodel) is fitted to the data so that the response can be predicted, optimized, or better understood without running additional experiments.

This repository compares three families of surrogate models commonly applied to DOE data:

| Abbreviation | Full Name |
|---|---|
| **RSM** | Response Surface Methodology (polynomial regression) |
| **GP** | Gaussian Process regression (Kriging) |
| **ANN/DL** | Artificial Neural Networks / Deep Learning |

The comparison is especially relevant for **small datasets** (typically 10–200 runs) that are characteristic of designed experiments in engineering, chemistry, manufacturing, and the life sciences.

---

## Supported DOE Designs

### Central Composite Design (CCD)
A second-order response surface design built by augmenting a two-level factorial design with center and axial (star) points. CCD supports curvature estimation and is well suited to RSM and GP models.

### Box-Behnken Design (BBD)
A three-level response surface design that avoids extreme corner points. BBD uses fewer runs than a full CCD while still supporting second-order models. Works well with RSM and GP.

### Definitive Screening Design (DSD)
A modern three-level design that provides estimates of main effects, two-factor interactions, and quadratic effects with very few runs. Particularly useful when the number of factors is large and experimental runs are expensive. GP and RSM are natural fits; ANN/DL requires care due to very limited data.

### Taguchi (Orthogonal Arrays)
Taguchi designs use orthogonal arrays to study many factors with a minimal number of experiments, focusing on robustness and signal-to-noise ratios. RSM (first- or second-order) and GP are typical modeling choices; ANN/DL is generally not recommended unless combined with data augmentation.

### Full Factorial Design
All combinations of all factor levels are tested. For two-level designs (2^k) this enables estimation of all main effects and interactions. Full factorials can grow large, but at moderate factor counts the dataset is still considered small. All three modeling families are applicable.

### Fractional Factorial Design
A carefully chosen subset of a full factorial (2^(k−p)) that maintains the ability to estimate the most important effects while using fewer runs. Due to aliasing/confounding, model complexity should match the resolution of the design. RSM (first-order + selected interactions) and GP are preferred; ANN/DL overfits easily at low run counts.

### Other DOE Methods
- **Latin Hypercube Sampling (LHS)** – space-filling design suitable for computer experiments; GP excels here.
- **D-Optimal / I-Optimal Designs** – model-based designs for irregular regions; RSM is the natural companion, GP also applicable.
- **Plackett-Burman Design** – screening design for main effects with very few runs; GP and first-order RSM are preferred.

---

## Modeling Methods

### Response Surface Methodology (RSM)

RSM fits polynomial regression models (first-order, second-order, or interaction models) to DOE data:

```
ŷ = β₀ + Σ βᵢxᵢ + Σ βᵢᵢxᵢ² + Σ βᵢⱼxᵢxⱼ + ε
```

**Strengths:**
- Interpretable coefficients directly linked to factor effects
- Computationally cheap; works well with very small datasets (≥ 10–20 runs)
- Well-established statistical inference (ANOVA, lack-of-fit tests, confidence intervals)
- Directly matched to classical DOE designs (CCD, BBD, DSD)

**Limitations:**
- Assumes a polynomial functional form; struggles with highly nonlinear responses
- Extrapolation outside the design region is unreliable
- Cannot capture complex interactions beyond what the polynomial terms allow

---

### Gaussian Process / Kriging (GP)

GP regression models the response as a realization of a Gaussian process, providing both a prediction mean and a prediction uncertainty (variance):

```
y(x) ~ GP(μ(x), k(x, x'))
```

where `k(x, x')` is a covariance (kernel) function (e.g., Matérn, RBF/Squared Exponential).

**Strengths:**
- Interpolates observed data exactly (noise-free) or smoothly (noisy observations)
- Provides built-in uncertainty quantification useful for sequential/adaptive DOE
- Highly flexible; can model nonlinear responses without specifying a parametric form
- Works well for space-filling designs (LHS) and computer experiments
- Competitive with RSM on CCD/BBD at small-to-moderate run counts

**Limitations:**
- Computational cost scales as O(n³); impractical for n > ~5 000 without approximations
- Hyperparameter estimation (likelihood optimization) can be difficult with very few points (n < 10)
- Less interpretable than RSM; effect sizes are not directly available
- Sensitive to kernel choice; poor choice leads to under/overfitting

---

### Artificial Neural Networks / Deep Learning (ANN/DL)

ANN/DL models learn a nonlinear mapping from inputs to outputs through layers of parameterized transformations:

```
ŷ = fL(... f2(W2 · f1(W1·x + b1) + b2) ...)
```

**Strengths:**
- Extremely flexible; can represent arbitrarily complex response surfaces
- No assumptions on functional form
- Can be enhanced with physics-informed constraints (PINNs) to compensate for data scarcity
- Suitable when data volume grows beyond the typical DOE range (transfer learning, data augmentation)

**Limitations:**
- Requires substantially more data than RSM or GP to generalize well; **small DOE datasets (< 100–200 runs) typically lead to overfitting**
- Hyperparameter tuning (architecture, learning rate, regularization) is non-trivial
- Lacks interpretability; no direct equivalent of ANOVA decomposition
- Training is stochastic; results may vary across runs without careful seeding
- Not recommended as a primary modeling choice for classical DOE designs unless regularization, Bayesian training, or data augmentation techniques are applied

---

## Method Comparison for Small Datasets

The table below summarizes the suitability of each method across key criteria relevant to small DOE datasets.

| Criterion | RSM | GP | ANN/DL |
|---|---|---|---|
| Minimum practical run count | ~10–20 | ~15–30 | ~100–200 (or with augmentation) |
| Handles nonlinearity | Moderate (polynomial) | High (kernel-defined) | Very high |
| Uncertainty quantification | Via confidence intervals | Native (predictive variance) | Requires Bayesian NN or ensembles |
| Interpretability | High (coefficients, ANOVA) | Moderate (hyperparameters, partial dependence) | Low |
| Extrapolation behavior | Polynomial (risky beyond design region) | Reverts to prior mean (safer) | Unpredictable |
| Computational cost | Very low | Low–Medium | High (training) |
| Integration with classical DOE | Native | Good | Limited |
| Overfitting risk (small n) | Low (with model selection) | Low–Moderate | High |
| Recommended for DOE (small n) | ✅ Primary choice | ✅ Primary choice | ⚠️ With caution |

---

## Choosing a Method by DOE Design

| DOE Design | Typical Run Count | Recommended Methods | Notes |
|---|---|---|---|
| Full Factorial (2^k, k ≤ 5) | 8–32 | RSM, GP | Second-order RSM if center points added |
| Fractional Factorial (2^(k-p)) | 4–32 | RSM (main effects / interactions), GP | Match model to design resolution |
| CCD | 2^k + 2k + cp | RSM (quadratic), GP | Classic second-order RSM setting |
| BBD | 2k(k−1) + cp | RSM (quadratic), GP | Avoids extreme points |
| DSD | 2k+1 | GP, RSM (quadratic) | Very few runs; GP often more flexible |
| Taguchi / Orthogonal Array | L4–L27+ | RSM (first-order + interactions), GP | Focus on signal-to-noise analysis |
| Latin Hypercube Sampling | n = (10 to 50)×k | GP | Designed for GP/Kriging metamodels |
| Plackett-Burman | k+1 to 2k | RSM (first-order), GP | Screening only; main effects |
| D-Optimal / I-Optimal | Model-dependent | RSM, GP | Custom design regions |

> **Rule of thumb:** For classical physical or chemical DOE designs with run counts below ~100, start with **RSM** for interpretability and then validate with **GP** for flexibility. Only consider **ANN/DL** if you can supplement the DOE data with additional observations, simulation output, or physical constraints.

---

## Getting Started

```bash
# Clone the repository
git clone https://github.com/pajeeokoth/DOE_modeling.git
cd DOE_modeling
```

Refer to individual module documentation as the codebase grows for instructions on fitting RSM, GP, and ANN/DL models to your DOE data.

---

## References

1. Box, G. E. P., & Draper, N. R. (2007). *Response Surfaces, Mixtures, and Ridge Analyses* (2nd ed.). Wiley.
2. Myers, R. H., Montgomery, D. C., & Anderson-Cook, C. M. (2016). *Response Surface Methodology* (4th ed.). Wiley.
3. Santner, T. J., Williams, B. J., & Notz, W. I. (2018). *The Design and Analysis of Computer Experiments* (2nd ed.). Springer.
4. Rasmussen, C. E., & Williams, C. K. I. (2006). *Gaussian Processes for Machine Learning*. MIT Press.
5. Jones, D. R., Schonlau, M., & Welch, W. J. (1998). Efficient global optimization of expensive black-box functions. *Journal of Global Optimization*, 13, 455–492.
6. Taguchi, G., Chowdhury, S., & Wu, Y. (2005). *Taguchi's Quality Engineering Handbook*. Wiley.
7. Jones, B., & Nachtsheim, C. J. (2011). A class of three-level designs for definitive screening in the presence of second-order effects. *Journal of Quality Technology*, 43(1), 1–15.
8. Goodfellow, I., Bengio, Y., & Courville, A. (2016). *Deep Learning*. MIT Press.
9. Raissi, M., Perdikaris, P., & Karniadakis, G. E. (2019). Physics-informed neural networks. *Journal of Computational Physics*, 378, 686–707.

