# Data provenance

These is CSV file supplied with the project.

- `processed/m2_cleaned_data.csv`: 39,643 rows, 67 columns. Initially copied as supplied; the portable notebook regenerates this path.

The notebook strips column-name whitespace; drops one row where a token-ratio field is outside [0, 1]; retains zero-content articles; and adds `empty_content`, `popular`, `channel`, `weekday`, `period`, and `temporal_split`. There are no missing cells or duplicate URLs in the supplied cleaned data.

`popular = 1` iff `shares > 1400`; exactly 1,400 shares is class 0. This is binary classification, not share-count regression.
