# Data provenance

These are the two CSV files supplied with the project. The original dataset source, citation, license and redistribution terms were not included among the six supplied files and have not been independently verified. Do not assume this repository grants a license to the data. Confirm attribution and redistribution terms before making the CSV files public.

- `raw/OnlineNewsPopularity.csv`: 39,644 rows, 61 columns. Preserved as supplied.
- `processed/m2_cleaned_data.csv`: 39,643 rows, 67 columns. Initially copied as supplied; the portable notebook regenerates this path.
- `reports/audit/source_manifest.json` records SHA-256 hashes of the supplied files before any regeneration.
- `reports/audit/data_verification.json` records the independent transformation check: identical column and URL order; no differing fields at numeric tolerance 1e-12.

The notebook strips column-name whitespace; drops one row where a token-ratio field is outside [0, 1]; retains zero-content articles; and adds `empty_content`, `popular`, `channel`, `weekday`, `period`, and `temporal_split`. There are no missing cells or duplicate URLs in the supplied cleaned data.

`popular = 1` iff `shares > 1400`; exactly 1,400 shares is class 0. This is binary classification, not share-count regression.
