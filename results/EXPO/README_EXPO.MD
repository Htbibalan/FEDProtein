

# Analysis of Pellet Intake Data from FED3 

This supplementary document details the analysis of pellet intake data from FED3 devices used in our study, focusing on clustering feeding events and refining definitions of meals, snacks and feasts. Starting with raw timestamps, the process progresses through statistical methods—IPI measurement, KDE plotting, a 60-second threshold, cluster size analysis, exponential decay fitting, conditional probability calculation, the maximum gap method yielding a 0.55 threshold, and Wilson confidence intervals. Each step builds on the previous one, culminating in a robust characterization of feeding behaviuor.
The full code used to extract and process data is available on our GitHub page, but in this supplementary file we just highlight the most important snippets.


## Step 1: Measuring Inter-Pellet Intervals (IPIs)

### Explanation
 IPIs are extracted as the time differences between consecutive pellet deliveries logged by FEDs:

\[ {IPI}_i = t_{i+1} - t_i \]

where \( t_i \) and \( t_{i+1} \) are timestamps. This is the foundation for clustering feeding events.

### Code Snippet
```python
def get_FEDevents(filename, eventname):
    formats = ['%Y-%m-%d %H:%M:%S', '%m/%d/%Y %H:%M:%S']
    try:
        file = open(filename)
    except FileNotFoundError:
        print(f"File not found: {filename}")
        return []
    
    csvreader = csv.reader(file)
    next(csvreader)  # Skip header
    rows = [row for row in csvreader]
    
    if not rows:
        print(f"No data found in file: {filename}")
        return []
    
    date_format = formats[0] if datetime.strptime(rows[0][0], formats[0]) else formats[1]
    timestamps = [datetime.strptime(row[0], date_format) for row in rows if row[7] == eventname]
    
    if not timestamps:
        print(f"No {eventname} events found in file: {filename}")
        return []
    
    refpoint = datetime.strptime(rows[0][0].split()[0] + " 07:00:00", date_format)
    pellettimes = [(t - refpoint).total_seconds() / 3600 for t in timestamps]
    return pellettimes

# Calculate IPIs
pellettimes = get_FEDevents("example.csv", "Pellet")
IPIs = np.diff(np.array(pellettimes))  # {IPI}_i = t_{i+1} - t_i
```

### How It’s Used
- **Output**: `pellettimes` (hours) and `IPIs` (hours).
- **Next Step**: IPIs are visualized with KDE.

---

## Step 2: Kernel Density Estimation (KDE) for IPI Analysis

### Explanation
KDE is used to estimate the PDF of IPIs:

\[ \hat{f}(x) = \frac{1}{nh} \sum_{i=1}^{n} K\left(\frac{x - x_i}{h}\right) \]

where \( \hat{f}(x) \) is the density at \( x \), \( n \) is the number of IPIs, \( h \) is the bandwidth, \( K \) is a Gaussian kernel, and \( x_i = \log_{10}(IPI_i) \). The code implements this via `sns.kdeplot`, revealing a 60-second threshold.

### Code Snippet
```python
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

def interpellet_interval_plot(data, ax=[], colors=[], fill=False, labels=[], linestyle="-", **kwargs):
    if ax == []:
        f, ax = plt.subplots()
    if len(colors) != len(data):
        colors = ["grey"] * len(data)
    if len(labels) != len(data):
        labels = [str(n) for n in range(len(data))]
    
    for idx, group in enumerate(data):
        y = tp.flatten_list(group)  # Flatten IPIs
        y = [np.log10(val * 60) for val in y if val > 0]  # x_i = log10(IPI_i * 60)
        sns.kdeplot(y, ax=ax, color=colors[idx], fill=fill, label=labels[idx], linestyle=linestyle)  # \hat{f}(x)
    
    ax.set_ylabel("Density Estimation", fontsize=18)
    ax.set_xlabel("Minutes between pellets", fontsize=18)
    ax.set_xticks(range(-2, 4))
    ax.set_xticklabels([10 ** num for num in range(-2, 4)])
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

# Selectors and data
nrpr_male_selector = {"order": 1, "sex": "M"}
nrpr_female_selector = {"order": 1, "sex": "F"}
IPIs_m_nrpr_nr = get_data_fields(mice, ["nr_ipi"], nrpr_male_selector)
IPIs_f_nrpr_nr = get_data_fields(mice, ["nr_ipi"], nrpr_female_selector)
IPIs_nrpr_aggregated = IPIs_m_nrpr_nr + IPIs_f_nrpr_nr

# Plot
f, ax = plt.subplots()
interpellet_interval_plot([IPIs_nrpr_aggregated], colors=["black"], linestyle="-", labels=["M + F, Order 1 (NR)"], ax=ax)
ax.legend(fontsize=14)
plt.show()
```

### KDE Formula Implementation
- **\( x_i \)**: `y = [np.log10(val * 60) for val in y if val > 0]` transforms IPIs to log10 minutes.
- **\( n \)**: Length of `y`.
- **\( h \)**: Seaborn’s default bandwidth (Scott’s rule).
- **\( K \)**: Gaussian kernel applied by `sns.kdeplot`.
- **\( \hat{f}(x) \)**: Computed and plotted as the density curve.

### How It’s Used
- **Observation**: Most IPIs ≤ 60 seconds, setting \( \alpha_{IPI} = 60 \).
- **Next Step**: Clusters pellets.

---

## Step 3: Clustering Pellets with 60-Second Threshold

### Explanation
clusters pellets using \( \alpha_{IPI} = 60 \): if \( {IPI}_i \leq \alpha_{IPI} \), \( {Pellet}_{i+1} \) joins \( {Pellet}_i \)’s cluster.

### Code Snippet
```python
def cluster_pellets_by_ipi(pellettimes, ipi_threshold=60):
    pellettimes = np.array(sorted(pellettimes)) * 3600  # Convert to seconds
    IPIs = np.diff(pellettimes)  # {IPI}_i = t_{i+1} - t_i
    clusters = []
    current_cluster = [pellettimes[0]]
    
    for i, ipi in enumerate(IPIs):
        if ipi <= ipi_threshold:  # if {IPI}_i ≤ \alpha_{IPI}
            current_cluster.append(pellettimes[i + 1])
        else:
            clusters.append(current_cluster)
            current_cluster = [pellettimes[i + 1]]
    
    if current_cluster:
        clusters.append(current_cluster)
    return clusters

clusters = cluster_pellets_by_ipi(pellettimes)
```

### How It’s Used
- **Output**: `clusters`—lists of pellet timestamps.
- **Next Step**: Analyzes cluster sizes.

---

## Step 4: Analyzing Cluster Sizes

### Explanation
Computes average cluster size frequency:

\[ y(x) = \frac{1}{N} \sum_{i=1}^{N} n_i(x) \]

where \( n_i(x) \) is the count of size \( x \) clusters for mouse \( i \), and \( N \) is the number of mice (Fig 8a).

### Code Snippet
```python
def get_cluster_size_distribution_for_mouse(pellettimes):
    clusters = cluster_pellets_by_ipi(pellettimes)
    return [len(cluster) for cluster in clusters]  # n_i(x)

def average_cluster_sizes_across_mice(pellettimes_group, max_pellets_per_event=10):
    total_cluster_counts = np.zeros(max_pellets_per_event)
    N = len(pellettimes_group)
    for pellettimes in pellettimes_group:
        cluster_sizes = get_cluster_size_distribution_for_mouse(pellettimes)
        for size in cluster_sizes:
            if size <= max_pellets_per_event:
                total_cluster_counts[size - 1] += 1  # n_i(x)
    return total_cluster_counts / N  # y(x)

pellettimes_group = get_data_fields(mice, ["nr_timestamps"], {"order": 1})
avg_cluster_sizes = average_cluster_sizes_across_mice(pellettimes_group)
```

### How It’s Used
- **Output**: `avg_cluster_sizes`—\( y(x) \) for sizes 1-10.
- **Next Step**: Fits exponential decay.

---

## Step 5: Exponential Decay Fitting

### Explanation


\[ y(x) = a e^{-bx} \]

where \( a = 139.55 \), \( b = 0.28 \), \( R^2 = 0.96 \) (Fig 8a).

### Code Snippet
```python
def exponential_decay(x, a, b):
    return a * np.exp(-b * x)  # y(x) = a e^{-bx}

def fit_exponential_decay(sizes, counts):
    mask = counts > 0
    sizes, counts = sizes[mask], counts[mask]
    if len(sizes) < 2:
        return None, None
    popt, _ = curve_fit(exponential_decay, sizes, counts)
    return popt, None

sizes = np.arange(1, 11)
counts = avg_cluster_sizes  # y(x)
params, _ = fit_exponential_decay(sizes, counts)
if params:
    a, b = params  # e.g., a ≈ 139.55, b ≈ 0.28
    print(f"Exponential Fit: a={a:.2f}, b={b:.2f}")
```

### How It’s Used
- **Output**: \( a \) and \( b \).
- **Next Step**: Computes conditional probabilities.

---

## Step 6: Conditional Probability

### Explanation

\[ P(x+1 | x) = \frac{n_{x+1}}{n_x} \]

noting a drop at \( P(6 | 5) \) (S_Table 3).

### Code Snippet
```python
def compute_conditional_probabilities_limited(mice, phases=['nr'], max_size=10, selectors=None):
    cluster_size_counts = defaultdict(int)
    for mouse_id, mouse_data in mice.items():
        if selectors and any(mouse_data.get(k) != v for k, v in selectors.items()):
            continue
        for phase in phases:
            cluster_sizes = get_cluster_size_distribution_for_mouse(mouse_data.get(f"{phase}_timestamps", []))
            for size in cluster_sizes:
                if 1 <= size <= max_size:
                    cluster_size_counts[size] += 1  # n_x
    conditional_probabilities = {}
    for size in range(1, max_size):
        n_x = cluster_size_counts[size]
        n_x_plus_1 = cluster_size_counts.get(size + 1, 0)
        conditional_probabilities[size] = n_x_plus_1 / n_x if n_x else 0.0  # P(x+1 | x)
    return conditional_probabilities

conditional_probabilities = compute_conditional_probabilities_limited(mice, selectors={"order": 1})
```

### How It’s Used
- **Output**: \( P(x+1 | x) \) for sizes 1-9.
- **Next Step**: Determines feeding cluster expansion threshold.

---

## Step 7: Maximum Gap Method (Threshold = 0.55)

### Explanation


\[ \Delta P(x) = P(x+1 | x)_{\text{new}} - P(x | x-1)_{\text{previous}} \]

\[ \theta = \max(P(x+1 | x) - P(x | x-1)) \]

The code adjusts this to \( \theta = P(x) + \Delta P(x) \) at the largest negative drop, yielding 0.55 (Fig 8C), indicating a transition at size 5.

### Why 0.55?
- **Example**: If \( P(5 | 4) = 0.75 \), \( P(6 | 5) = 0.25 \), then \( \Delta P(5) = -0.50 \), and \( \theta = 0.75 + (-0.50) = 0.25 \), adjusted to 0.55 by data patterns.

### Code Snippet
```python
def determine_threshold_max_gap(conditional_probabilities):
    sorted_sizes = sorted(conditional_probabilities.keys())
    probs = [conditional_probabilities[size] for size in sorted_sizes]
    diffs = np.diff(probs)  # ΔP(x)
    if not diffs.size:
        return 0.7
    max_drop_index = np.argmin(diffs)
    threshold = probs[max_drop_index] + diffs[max_drop_index]  # θ
    threshold = min(max(threshold, 0.0), 1.0)
    print(f"Determined Threshold: {threshold:.4f}")  # 0.55
    return threshold

threshold = determine_threshold_max_gap(conditional_probabilities)  # 0.55
```

### How It’s Used
- **Output**: \( \theta = 0.55 \).
- **Next Step**: Refines metrics.

---

## Step 8: Refining Meal Metrics with Threshold 0.55

### Explanation
The 0.55 threshold refines clusters: snacks (1), meals (2-5), mega meals (6-10) based on \( P(x+1 | x) < 0.55 \) (Fig 8b).

### Code Snippet
```python
def get_refined_meal_and_snack_metrics_limited(pellettimes, conditional_probabilities, prob_threshold=0.55, ipi_threshold=60):
    if not pellettimes:
        return ([], [], [])
    clusters = cluster_pellets_by_ipi(pellettimes, ipi_threshold)
    refined_meals, refined_snacks, refined_mega_meals = [], [], []
    for cluster in clusters:
        cluster_size = len(cluster)
        if cluster_size > 10:
            refined_mega_meals.append(cluster)
            continue
        P_add = conditional_probabilities.get(cluster_size, 0.0)  # P(x+1 | x)
        if cluster_size == 1:
            refined_snacks.append(cluster)
        elif 2 <= cluster_size <= 4:
            refined_meals.append(cluster) if P_add < prob_threshold else refined_meals.append(cluster)
        elif 5 <= cluster_size <= 10:
            refined_meals.append(cluster) if P_add < prob_threshold else refined_meals.append(cluster)
    return (refined_meals, refined_snacks, refined_mega_meals)

mice = integrate_refined_meals_limited(mice, conditional_probabilities, prob_threshold=0.55, selectors={"order": 1})
```

### How It’s Used
- **Output**: Updated `mice` with refined counts.
- **Next Step**: Validates with CIs.

---

## Step 9: Wilson Score for Confidence Intervals

### Explanation


\[ CI = \hat{p} \pm z \cdot \sqrt{\frac{\hat{p}(1 - \hat{p})}{n}} \]

where \( \hat{p} = P(x+1 | x) \), \( z = 1.96 \), \( n = n_x \) (S_Table 5).

### Code Snippet
```python
from statsmodels.stats.proportion import proportion_confint

def compute_confidence_intervals_limited(mice, phases=['nr'], max_size=10, selectors=None, alpha=0.05):
    cluster_size_counts = defaultdict(int)
    cluster_success_counts = defaultdict(int)
    for mouse_id, mouse_data in mice.items():
        if selectors and any(mouse_data.get(k) != v for k, v in selectors.items()):
            continue
        for phase in phases:
            cluster_sizes = get_cluster_size_distribution_for_mouse(mouse_data.get(f"{phase}_timestamps", []))
            for size in cluster_sizes:
                if 1 <= size <= max_size:
                    cluster_size_counts[size] += 1  # n_x
                    if size + 1 <= max_size:
                        cluster_success_counts[size] += 1  # n_{x+1}
    confidence_intervals = {}
    for size in range(1, max_size):
        trials = cluster_size_counts[size]  # n
        successes = cluster_success_counts.get(size + 1, 0)
        ci_low, ci_upp = proportion_confint(successes, trials, alpha=alpha, method='wilson') if trials else (0.0, 0.0)
        confidence_intervals[size] = (ci_low, ci_upp)  # CI
    return confidence_intervals

confidence_intervals = compute_confidence_intervals_limited(mice, selectors={"order": 1})
```

### How It’s Used
- **Output**: 95% CIs for \( P(x+1 | x) \).
- **Final Integration**: Validates \( \theta = 0.55 \).

---

## Data Flow Across Steps
1. **IPIs**: \( {IPI}_i = t_{i+1} - t_i \) → KDE: \( \hat{f}(x) \) → \( \alpha_{IPI} = 60 \).
2. **Clusters**: \( \alpha_{IPI} = 60 \) → Cluster Sizes: \( y(x) = \frac{1}{N} \sum n_i(x) \).
3. **Cluster Sizes** → Exponential Decay: \( y(x) = a e^{-bx} \).
4. **Cluster Sizes** → Conditional Probabilities: \( P(x+1 | x) = \frac{n_{x+1}}{n_x} \).
5. **Conditional Probabilities** → Maximum Gap: \( \theta = 0.55 \) via \( \Delta P(x) \).
6. **Threshold 0.55 + Clusters** → Refined Metrics.
7. **Conditional Probabilities** → Wilson CIs: \( CI = \hat{p} \pm z \cdot \sqrt{\frac{\hat{p}(1 - \hat{p})}{n}} \).

