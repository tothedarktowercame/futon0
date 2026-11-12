# EQ-6 — proved lemmas as closure rules

The frozen EQ-5 rows, row indices, attempt seeds, and budgets are unchanged. Each new equation is an ordinary closure rule: it can merge present terms but cannot construct a term.

## Solver rates

Each cell is `derived (EQ-5 base)`.

| derived theory | old needed | policy | budget | goals | mean solve rate |
|---|---:|---|---:|---:|---:|
| exponent_two+commutes | 1 | completing | 1n | 60 | 12.5% (13.1%) |
| exponent_two+commutes | 1 | completing | 2n | 60 | 24.2% (24.9%) |
| exponent_two+commutes | 1 | completing | 4n | 60 | 39.9% (43.4%) |
| exponent_two+commutes | 1 | merging | 1n | 60 | 12.9% (14.6%) |
| exponent_two+commutes | 1 | merging | 2n | 60 | 22.2% (26.1%) |
| exponent_two+commutes | 1 | merging | 4n | 60 | 38.8% (41.8%) |
| exponent_two+commutes | 1 | uniform | 1n | 60 | 6.8% (6.8%) |
| exponent_two+commutes | 1 | uniform | 2n | 60 | 10.9% (11.2%) |
| exponent_two+commutes | 1 | uniform | 4n | 60 | 19.7% (18.4%) |
| exponent_two+commutes | 2 | completing | 1n | 60 | 3.5% (1.8%) |
| exponent_two+commutes | 2 | completing | 2n | 60 | 8.4% (6.3%) |
| exponent_two+commutes | 2 | completing | 4n | 60 | 26.7% (24.8%) |
| exponent_two+commutes | 2 | merging | 1n | 60 | 3.5% (2.2%) |
| exponent_two+commutes | 2 | merging | 2n | 60 | 9.9% (8.4%) |
| exponent_two+commutes | 2 | merging | 4n | 60 | 30.6% (26.5%) |
| exponent_two+commutes | 2 | uniform | 1n | 60 | 2.5% (0.6%) |
| exponent_two+commutes | 2 | uniform | 2n | 60 | 3.8% (1.5%) |
| exponent_two+commutes | 2 | uniform | 4n | 60 | 5.8% (3.7%) |
| exponent_two+commutes | 3 | completing | 1n | 60 | 3.2% (1.1%) |
| exponent_two+commutes | 3 | completing | 2n | 60 | 21.2% (19.8%) |
| exponent_two+commutes | 3 | completing | 4n | 60 | 57.7% (54.8%) |
| exponent_two+commutes | 3 | merging | 1n | 60 | 3.2% (1.5%) |
| exponent_two+commutes | 3 | merging | 2n | 60 | 20.8% (18.4%) |
| exponent_two+commutes | 3 | merging | 4n | 60 | 67.9% (66.7%) |
| exponent_two+commutes | 3 | uniform | 1n | 60 | 2.2% (0.6%) |
| exponent_two+commutes | 3 | uniform | 2n | 60 | 6.6% (4.2%) |
| exponent_two+commutes | 3 | uniform | 4n | 60 | 13.6% (10.2%) |
| exponent_two+commutes | 4 | completing | 1n | 60 | 11.8% (1.6%) |
| exponent_two+commutes | 4 | completing | 2n | 60 | 33.3% (21.1%) |
| exponent_two+commutes | 4 | completing | 4n | 60 | 74.2% (58.2%) |
| exponent_two+commutes | 4 | merging | 1n | 60 | 11.7% (1.2%) |
| exponent_two+commutes | 4 | merging | 2n | 60 | 34.8% (22.0%) |
| exponent_two+commutes | 4 | merging | 4n | 60 | 88.9% (83.8%) |
| exponent_two+commutes | 4 | uniform | 1n | 60 | 10.2% (0.2%) |
| exponent_two+commutes | 4 | uniform | 2n | 60 | 13.7% (3.2%) |
| exponent_two+commutes | 4 | uniform | 4n | 60 | 18.5% (6.9%) |
| exponent_two+commutes | 5 | completing | 1n | 39 | 11.2% (2.3%) |
| exponent_two+commutes | 5 | completing | 2n | 39 | 38.3% (24.1%) |
| exponent_two+commutes | 5 | completing | 4n | 39 | 83.2% (60.3%) |
| exponent_two+commutes | 5 | merging | 1n | 39 | 10.6% (2.2%) |
| exponent_two+commutes | 5 | merging | 2n | 39 | 41.8% (29.4%) |
| exponent_two+commutes | 5 | merging | 4n | 39 | 97.1% (96.9%) |
| exponent_two+commutes | 5 | uniform | 1n | 39 | 3.7% (0.0%) |
| exponent_two+commutes | 5 | uniform | 2n | 39 | 8.2% (2.9%) |
| exponent_two+commutes | 5 | uniform | 4n | 39 | 10.0% (4.6%) |
| exponent_two+commutes | 6 | completing | 1n | 19 | 31.6% (4.5%) |
| exponent_two+commutes | 6 | completing | 2n | 19 | 64.7% (25.0%) |
| exponent_two+commutes | 6 | completing | 4n | 19 | 92.1% (66.1%) |
| exponent_two+commutes | 6 | merging | 1n | 19 | 32.4% (6.1%) |
| exponent_two+commutes | 6 | merging | 2n | 19 | 68.4% (33.4%) |
| exponent_two+commutes | 6 | merging | 4n | 19 | 99.5% (99.5%) |
| exponent_two+commutes | 6 | uniform | 1n | 19 | 27.6% (1.3%) |
| exponent_two+commutes | 6 | uniform | 2n | 19 | 29.5% (2.4%) |
| exponent_two+commutes | 6 | uniform | 4n | 19 | 35.0% (5.3%) |
| exponent_two+commutes | >6 | completing | 1n | 6 | 83.3% (0.0%) |
| exponent_two+commutes | >6 | completing | 2n | 6 | 87.5% (18.3%) |
| exponent_two+commutes | >6 | completing | 4n | 6 | 98.3% (78.3%) |
| exponent_two+commutes | >6 | merging | 1n | 6 | 83.3% (0.0%) |
| exponent_two+commutes | >6 | merging | 2n | 6 | 89.2% (58.3%) |
| exponent_two+commutes | >6 | merging | 4n | 6 | 100.0% (100.0%) |
| exponent_two+commutes | >6 | uniform | 1n | 6 | 83.3% (0.0%) |
| exponent_two+commutes | >6 | uniform | 2n | 6 | 83.3% (0.0%) |
| exponent_two+commutes | >6 | uniform | 4n | 6 | 83.3% (0.0%) |
| group+inverse_product | 1 | completing | 1n | 60 | 19.1% (17.8%) |
| group+inverse_product | 1 | completing | 2n | 60 | 34.1% (33.1%) |
| group+inverse_product | 1 | completing | 4n | 60 | 59.2% (57.4%) |
| group+inverse_product | 1 | merging | 1n | 60 | 18.0% (16.8%) |
| group+inverse_product | 1 | merging | 2n | 60 | 38.5% (36.7%) |
| group+inverse_product | 1 | merging | 4n | 60 | 60.1% (59.2%) |
| group+inverse_product | 1 | uniform | 1n | 60 | 6.8% (6.3%) |
| group+inverse_product | 1 | uniform | 2n | 60 | 13.8% (12.5%) |
| group+inverse_product | 1 | uniform | 4n | 60 | 21.8% (19.8%) |
| group+inverse_product | 2 | completing | 1n | 60 | 2.2% (1.8%) |
| group+inverse_product | 2 | completing | 2n | 60 | 7.5% (7.3%) |
| group+inverse_product | 2 | completing | 4n | 60 | 26.8% (26.2%) |
| group+inverse_product | 2 | merging | 1n | 60 | 2.8% (2.4%) |
| group+inverse_product | 2 | merging | 2n | 60 | 9.2% (9.0%) |
| group+inverse_product | 2 | merging | 4n | 60 | 36.0% (35.2%) |
| group+inverse_product | 2 | uniform | 1n | 60 | 0.4% (0.4%) |
| group+inverse_product | 2 | uniform | 2n | 60 | 0.8% (0.6%) |
| group+inverse_product | 2 | uniform | 4n | 60 | 2.1% (1.8%) |
| group+inverse_product | 3 | completing | 1n | 25 | 3.2% (1.4%) |
| group+inverse_product | 3 | completing | 2n | 25 | 12.6% (9.6%) |
| group+inverse_product | 3 | completing | 4n | 25 | 43.6% (41.0%) |
| group+inverse_product | 3 | merging | 1n | 25 | 3.4% (1.6%) |
| group+inverse_product | 3 | merging | 2n | 25 | 15.6% (12.8%) |
| group+inverse_product | 3 | merging | 4n | 25 | 70.6% (70.0%) |
| group+inverse_product | 3 | uniform | 1n | 25 | 0.2% (0.2%) |
| group+inverse_product | 3 | uniform | 2n | 25 | 3.0% (1.4%) |
| group+inverse_product | 3 | uniform | 4n | 25 | 3.2% (2.0%) |
| group+inverse_product | 4 | completing | 1n | 7 | 1.4% (1.4%) |
| group+inverse_product | 4 | completing | 2n | 7 | 10.0% (12.1%) |
| group+inverse_product | 4 | completing | 4n | 7 | 47.9% (50.7%) |
| group+inverse_product | 4 | merging | 1n | 7 | 0.7% (0.7%) |
| group+inverse_product | 4 | merging | 2n | 7 | 24.3% (22.9%) |
| group+inverse_product | 4 | merging | 4n | 7 | 96.4% (97.9%) |
| group+inverse_product | 4 | uniform | 1n | 7 | 0.0% (0.0%) |
| group+inverse_product | 4 | uniform | 2n | 7 | 0.0% (0.0%) |
| group+inverse_product | 4 | uniform | 4n | 7 | 2.1% (2.1%) |
| group+inverse_product | >6 | completing | 1n | 1 | 100.0% (0.0%) |
| group+inverse_product | >6 | completing | 2n | 1 | 100.0% (5.0%) |
| group+inverse_product | >6 | completing | 4n | 1 | 100.0% (5.0%) |
| group+inverse_product | >6 | merging | 1n | 1 | 100.0% (0.0%) |
| group+inverse_product | >6 | merging | 2n | 1 | 100.0% (0.0%) |
| group+inverse_product | >6 | merging | 4n | 1 | 100.0% (100.0%) |
| group+inverse_product | >6 | uniform | 1n | 1 | 100.0% (0.0%) |
| group+inverse_product | >6 | uniform | 2n | 1 | 100.0% (0.0%) |
| group+inverse_product | >6 | uniform | 4n | 1 | 100.0% (0.0%) |
| left_group+right_inverse | 1 | completing | 1n | 60 | 22.3% (14.2%) |
| left_group+right_inverse | 1 | completing | 2n | 60 | 36.7% (29.7%) |
| left_group+right_inverse | 1 | completing | 4n | 60 | 64.2% (57.7%) |
| left_group+right_inverse | 1 | merging | 1n | 60 | 23.3% (15.9%) |
| left_group+right_inverse | 1 | merging | 2n | 60 | 36.2% (26.9%) |
| left_group+right_inverse | 1 | merging | 4n | 60 | 62.7% (59.3%) |
| left_group+right_inverse | 1 | uniform | 1n | 60 | 9.2% (3.8%) |
| left_group+right_inverse | 1 | uniform | 2n | 60 | 9.4% (3.6%) |
| left_group+right_inverse | 1 | uniform | 4n | 60 | 13.2% (6.3%) |
| left_group+right_inverse | 2 | completing | 1n | 60 | 14.8% (4.2%) |
| left_group+right_inverse | 2 | completing | 2n | 60 | 36.3% (25.2%) |
| left_group+right_inverse | 2 | completing | 4n | 60 | 74.5% (73.8%) |
| left_group+right_inverse | 2 | merging | 1n | 60 | 15.4% (5.1%) |
| left_group+right_inverse | 2 | merging | 2n | 60 | 37.9% (26.2%) |
| left_group+right_inverse | 2 | merging | 4n | 60 | 80.0% (84.2%) |
| left_group+right_inverse | 2 | uniform | 1n | 60 | 4.7% (0.2%) |
| left_group+right_inverse | 2 | uniform | 2n | 60 | 6.0% (1.0%) |
| left_group+right_inverse | 2 | uniform | 4n | 60 | 8.8% (1.8%) |
| left_group+right_inverse | 3 | completing | 1n | 60 | 31.4% (3.6%) |
| left_group+right_inverse | 3 | completing | 2n | 60 | 53.2% (33.8%) |
| left_group+right_inverse | 3 | completing | 4n | 60 | 83.0% (83.5%) |
| left_group+right_inverse | 3 | merging | 1n | 60 | 31.8% (2.3%) |
| left_group+right_inverse | 3 | merging | 2n | 60 | 54.8% (35.9%) |
| left_group+right_inverse | 3 | merging | 4n | 60 | 93.0% (96.8%) |
| left_group+right_inverse | 3 | uniform | 1n | 60 | 19.7% (0.3%) |
| left_group+right_inverse | 3 | uniform | 2n | 60 | 21.8% (1.4%) |
| left_group+right_inverse | 3 | uniform | 4n | 60 | 23.5% (1.8%) |
| left_group+right_inverse | 4 | completing | 1n | 60 | 52.5% (4.4%) |
| left_group+right_inverse | 4 | completing | 2n | 60 | 76.8% (52.1%) |
| left_group+right_inverse | 4 | completing | 4n | 60 | 90.9% (82.8%) |
| left_group+right_inverse | 4 | merging | 1n | 60 | 52.5% (5.7%) |
| left_group+right_inverse | 4 | merging | 2n | 60 | 80.1% (56.0%) |
| left_group+right_inverse | 4 | merging | 4n | 60 | 99.0% (95.7%) |
| left_group+right_inverse | 4 | uniform | 1n | 60 | 27.8% (0.5%) |
| left_group+right_inverse | 4 | uniform | 2n | 60 | 30.4% (1.0%) |
| left_group+right_inverse | 4 | uniform | 4n | 60 | 33.6% (2.2%) |
| left_group+right_inverse | 5 | completing | 1n | 19 | 65.5% (4.7%) |
| left_group+right_inverse | 5 | completing | 2n | 19 | 84.2% (55.3%) |
| left_group+right_inverse | 5 | completing | 4n | 19 | 96.6% (64.7%) |
| left_group+right_inverse | 5 | merging | 1n | 19 | 67.4% (2.9%) |
| left_group+right_inverse | 5 | merging | 2n | 19 | 88.7% (62.9%) |
| left_group+right_inverse | 5 | merging | 4n | 19 | 100.0% (74.5%) |
| left_group+right_inverse | 5 | uniform | 1n | 19 | 46.3% (0.0%) |
| left_group+right_inverse | 5 | uniform | 2n | 19 | 48.9% (2.4%) |
| left_group+right_inverse | 5 | uniform | 4n | 19 | 48.2% (2.4%) |
| left_group+right_inverse | 6 | completing | 1n | 11 | 62.7% (0.0%) |
| left_group+right_inverse | 6 | completing | 2n | 11 | 66.4% (11.8%) |
| left_group+right_inverse | 6 | completing | 4n | 11 | 71.4% (10.9%) |
| left_group+right_inverse | 6 | merging | 1n | 11 | 65.5% (0.0%) |
| left_group+right_inverse | 6 | merging | 2n | 11 | 79.1% (9.1%) |
| left_group+right_inverse | 6 | merging | 4n | 11 | 84.5% (28.6%) |
| left_group+right_inverse | 6 | uniform | 1n | 11 | 55.0% (0.0%) |
| left_group+right_inverse | 6 | uniform | 2n | 11 | 55.5% (0.0%) |
| left_group+right_inverse | 6 | uniform | 4n | 11 | 55.9% (0.0%) |
| left_group+right_inverse | >6 | completing | 1n | 5 | 8.0% (0.0%) |
| left_group+right_inverse | >6 | completing | 2n | 5 | 19.0% (0.0%) |
| left_group+right_inverse | >6 | completing | 4n | 5 | 19.0% (0.0%) |
| left_group+right_inverse | >6 | merging | 1n | 5 | 18.0% (0.0%) |
| left_group+right_inverse | >6 | merging | 2n | 5 | 48.0% (2.0%) |
| left_group+right_inverse | >6 | merging | 4n | 5 | 47.0% (8.0%) |
| left_group+right_inverse | >6 | uniform | 1n | 5 | 0.0% (0.0%) |
| left_group+right_inverse | >6 | uniform | 2n | 5 | 1.0% (0.0%) |
| left_group+right_inverse | >6 | uniform | 4n | 5 | 1.0% (0.0%) |
| left_group+right_inverse+right_identity | 1 | completing | 1n | 60 | 70.9% (14.2%) |
| left_group+right_inverse+right_identity | 1 | completing | 2n | 60 | 74.6% (29.7%) |
| left_group+right_inverse+right_identity | 1 | completing | 4n | 60 | 83.5% (57.7%) |
| left_group+right_inverse+right_identity | 1 | merging | 1n | 60 | 71.4% (15.9%) |
| left_group+right_inverse+right_identity | 1 | merging | 2n | 60 | 77.2% (26.9%) |
| left_group+right_inverse+right_identity | 1 | merging | 4n | 60 | 85.9% (59.3%) |
| left_group+right_inverse+right_identity | 1 | uniform | 1n | 60 | 67.2% (3.8%) |
| left_group+right_inverse+right_identity | 1 | uniform | 2n | 60 | 68.0% (3.6%) |
| left_group+right_inverse+right_identity | 1 | uniform | 4n | 60 | 70.2% (6.3%) |
| left_group+right_inverse+right_identity | 2 | completing | 1n | 60 | 59.2% (4.2%) |
| left_group+right_inverse+right_identity | 2 | completing | 2n | 60 | 68.7% (25.2%) |
| left_group+right_inverse+right_identity | 2 | completing | 4n | 60 | 77.3% (73.8%) |
| left_group+right_inverse+right_identity | 2 | merging | 1n | 60 | 59.4% (5.1%) |
| left_group+right_inverse+right_identity | 2 | merging | 2n | 60 | 69.5% (26.2%) |
| left_group+right_inverse+right_identity | 2 | merging | 4n | 60 | 83.6% (84.2%) |
| left_group+right_inverse+right_identity | 2 | uniform | 1n | 60 | 51.7% (0.2%) |
| left_group+right_inverse+right_identity | 2 | uniform | 2n | 60 | 53.6% (1.0%) |
| left_group+right_inverse+right_identity | 2 | uniform | 4n | 60 | 55.8% (1.8%) |
| left_group+right_inverse+right_identity | 3 | completing | 1n | 60 | 55.6% (3.6%) |
| left_group+right_inverse+right_identity | 3 | completing | 2n | 60 | 65.1% (33.8%) |
| left_group+right_inverse+right_identity | 3 | completing | 4n | 60 | 74.6% (83.5%) |
| left_group+right_inverse+right_identity | 3 | merging | 1n | 60 | 55.8% (2.3%) |
| left_group+right_inverse+right_identity | 3 | merging | 2n | 60 | 69.9% (35.9%) |
| left_group+right_inverse+right_identity | 3 | merging | 4n | 60 | 89.7% (96.8%) |
| left_group+right_inverse+right_identity | 3 | uniform | 1n | 60 | 44.8% (0.3%) |
| left_group+right_inverse+right_identity | 3 | uniform | 2n | 60 | 47.7% (1.4%) |
| left_group+right_inverse+right_identity | 3 | uniform | 4n | 60 | 49.6% (1.8%) |
| left_group+right_inverse+right_identity | 4 | completing | 1n | 60 | 72.6% (4.4%) |
| left_group+right_inverse+right_identity | 4 | completing | 2n | 60 | 82.0% (52.1%) |
| left_group+right_inverse+right_identity | 4 | completing | 4n | 60 | 91.2% (82.8%) |
| left_group+right_inverse+right_identity | 4 | merging | 1n | 60 | 71.8% (5.7%) |
| left_group+right_inverse+right_identity | 4 | merging | 2n | 60 | 90.4% (56.0%) |
| left_group+right_inverse+right_identity | 4 | merging | 4n | 60 | 99.1% (95.7%) |
| left_group+right_inverse+right_identity | 4 | uniform | 1n | 60 | 52.4% (0.5%) |
| left_group+right_inverse+right_identity | 4 | uniform | 2n | 60 | 57.7% (1.0%) |
| left_group+right_inverse+right_identity | 4 | uniform | 4n | 60 | 60.4% (2.2%) |
| left_group+right_inverse+right_identity | 5 | completing | 1n | 19 | 77.1% (4.7%) |
| left_group+right_inverse+right_identity | 5 | completing | 2n | 19 | 82.1% (55.3%) |
| left_group+right_inverse+right_identity | 5 | completing | 4n | 19 | 91.3% (64.7%) |
| left_group+right_inverse+right_identity | 5 | merging | 1n | 19 | 79.2% (2.9%) |
| left_group+right_inverse+right_identity | 5 | merging | 2n | 19 | 87.1% (62.9%) |
| left_group+right_inverse+right_identity | 5 | merging | 4n | 19 | 100.0% (74.5%) |
| left_group+right_inverse+right_identity | 5 | uniform | 1n | 19 | 62.4% (0.0%) |
| left_group+right_inverse+right_identity | 5 | uniform | 2n | 19 | 63.7% (2.4%) |
| left_group+right_inverse+right_identity | 5 | uniform | 4n | 19 | 66.1% (2.4%) |
| left_group+right_inverse+right_identity | 6 | completing | 1n | 11 | 93.2% (0.0%) |
| left_group+right_inverse+right_identity | 6 | completing | 2n | 11 | 95.9% (11.8%) |
| left_group+right_inverse+right_identity | 6 | completing | 4n | 11 | 96.4% (10.9%) |
| left_group+right_inverse+right_identity | 6 | merging | 1n | 11 | 92.3% (0.0%) |
| left_group+right_inverse+right_identity | 6 | merging | 2n | 11 | 99.5% (9.1%) |
| left_group+right_inverse+right_identity | 6 | merging | 4n | 11 | 100.0% (28.6%) |
| left_group+right_inverse+right_identity | 6 | uniform | 1n | 11 | 90.9% (0.0%) |
| left_group+right_inverse+right_identity | 6 | uniform | 2n | 11 | 90.9% (0.0%) |
| left_group+right_inverse+right_identity | 6 | uniform | 4n | 11 | 91.8% (0.0%) |
| left_group+right_inverse+right_identity | >6 | completing | 1n | 5 | 100.0% (0.0%) |
| left_group+right_inverse+right_identity | >6 | completing | 2n | 5 | 100.0% (0.0%) |
| left_group+right_inverse+right_identity | >6 | completing | 4n | 5 | 100.0% (0.0%) |
| left_group+right_inverse+right_identity | >6 | merging | 1n | 5 | 100.0% (0.0%) |
| left_group+right_inverse+right_identity | >6 | merging | 2n | 5 | 100.0% (2.0%) |
| left_group+right_inverse+right_identity | >6 | merging | 4n | 5 | 100.0% (8.0%) |
| left_group+right_inverse+right_identity | >6 | uniform | 1n | 5 | 100.0% (0.0%) |
| left_group+right_inverse+right_identity | >6 | uniform | 2n | 5 | 100.0% (0.0%) |
| left_group+right_inverse+right_identity | >6 | uniform | 4n | 5 | 100.0% (0.0%) |

## Needed-set shifts

| derived theory | old → new | goals |
|---|---:|---:|
| exponent_two+commutes | 1 → 1 | 60 |
| exponent_two+commutes | 2 → 0 | 1 |
| exponent_two+commutes | 2 → 2 | 59 |
| exponent_two+commutes | 3 → 0 | 1 |
| exponent_two+commutes | 3 → 3 | 59 |
| exponent_two+commutes | 4 → 0 | 6 |
| exponent_two+commutes | 4 → 4 | 54 |
| exponent_two+commutes | 5 → 0 | 1 |
| exponent_two+commutes | 5 → 1 | 1 |
| exponent_two+commutes | 5 → 2 | 1 |
| exponent_two+commutes | 5 → 3 | 1 |
| exponent_two+commutes | 5 → 5 | 35 |
| exponent_two+commutes | 6 → 0 | 5 |
| exponent_two+commutes | 6 → 3 | 3 |
| exponent_two+commutes | 6 → 6 | 11 |
| exponent_two+commutes | 7 → 0 | 3 |
| exponent_two+commutes | 7 → 7 | 1 |
| exponent_two+commutes | 8 → 0 | 2 |
| group+inverse_product | 1 → 1 | 60 |
| group+inverse_product | 2 → 2 | 60 |
| group+inverse_product | 3 → 3 | 25 |
| group+inverse_product | 4 → 4 | 7 |
| group+inverse_product | 8 → 0 | 1 |
| left_group+right_inverse | 1 → 0 | 3 |
| left_group+right_inverse | 1 → 1 | 57 |
| left_group+right_inverse | 2 → 0 | 2 |
| left_group+right_inverse | 2 → 1 | 3 |
| left_group+right_inverse | 2 → 2 | 55 |
| left_group+right_inverse | 3 → 0 | 10 |
| left_group+right_inverse | 3 → 1 | 14 |
| left_group+right_inverse | 3 → 3 | 36 |
| left_group+right_inverse | 4 → 0 | 12 |
| left_group+right_inverse | 4 → 1 | 24 |
| left_group+right_inverse | 4 → 2 | 1 |
| left_group+right_inverse | 4 → 4 | 23 |
| left_group+right_inverse | 5 → 0 | 7 |
| left_group+right_inverse | 5 → 1 | 6 |
| left_group+right_inverse | 5 → 5 | 6 |
| left_group+right_inverse | 6 → 0 | 6 |
| left_group+right_inverse | 6 → 2 | 1 |
| left_group+right_inverse | 6 → 6 | 4 |
| left_group+right_inverse | 7 → 3 | 1 |
| left_group+right_inverse | 7 → 7 | 3 |
| left_group+right_inverse | 8 → 8 | 1 |
| left_group+right_inverse+right_identity | 1 → 0 | 39 |
| left_group+right_inverse+right_identity | 1 → 1 | 21 |
| left_group+right_inverse+right_identity | 2 → 0 | 29 |
| left_group+right_inverse+right_identity | 2 → 1 | 18 |
| left_group+right_inverse+right_identity | 2 → 2 | 13 |
| left_group+right_inverse+right_identity | 3 → 0 | 24 |
| left_group+right_inverse+right_identity | 3 → 1 | 18 |
| left_group+right_inverse+right_identity | 3 → 2 | 7 |
| left_group+right_inverse+right_identity | 3 → 3 | 11 |
| left_group+right_inverse+right_identity | 4 → 0 | 23 |
| left_group+right_inverse+right_identity | 4 → 1 | 27 |
| left_group+right_inverse+right_identity | 4 → 2 | 4 |
| left_group+right_inverse+right_identity | 4 → 3 | 3 |
| left_group+right_inverse+right_identity | 4 → 4 | 3 |
| left_group+right_inverse+right_identity | 5 → 0 | 9 |
| left_group+right_inverse+right_identity | 5 → 1 | 7 |
| left_group+right_inverse+right_identity | 5 → 5 | 3 |
| left_group+right_inverse+right_identity | 6 → 0 | 10 |
| left_group+right_inverse+right_identity | 6 → 2 | 1 |
| left_group+right_inverse+right_identity | 7 → 0 | 4 |
| left_group+right_inverse+right_identity | 8 → 0 | 1 |

## Zero-move and named goals

| derived theory | goals true at start | named goal | named result |
|---|---:|---|---|
| exponent_two+commutes | 19 | exponent_two_commutes | exponent_two_commutes: 0 moves |
| group+inverse_product | 1 | inverse_product | inverse_product: 0 moves |
| left_group+right_inverse | 40 | left_group_right_inverse | left_group_right_inverse: 0 moves |
| left_group+right_inverse+right_identity | 139 | left_group_right_inverse | left_group_right_inverse: 0 moves |

## Run receipt

- Wall time: 1035.1 seconds, using four worker processes.
- Rows evaluated: 1007; needed sets unavailable: 0.
- Nothing was cut and the run was not relaunched.
