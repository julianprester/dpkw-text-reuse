# Aborted timestamp-age removal

The author initially requested removal of the timestamp-derived age spline, then reversed that instruction and retained the existing control.

The partial refit was stopped during Model 2. Its fixed-effects tables were produced without the age spline, but the multilevel fit and robustness sequence did not complete. These outputs are **not used in the manuscript or active result registry**.

The original scripts, Model 2 tables, robustness tables, and number registries were restored byte-for-byte from `../description_age_spec/`. Model 1, the qualitative judgments, and their raw/amendment records were verified unchanged.

The retained main specification distinguishes:

- **Platform experience:** completed-job groups, `log(1 + jobs)`, and `log(1 + hours)`.
- **Description/profile age:** the existing spline of `tenure_yrs`, computed from description creation to the latest observed description edit.

Because the author confirmed that the timestamps concern descriptions, age is not represented as account tenure. No new completed model is claimed for this aborted attempt.
