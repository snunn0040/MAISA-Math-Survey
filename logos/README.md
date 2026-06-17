# Logos

Drop your logo image files here (PNG or SVG). The one-pager looks for these
filenames by default (configurable at the top of `math_onepager.qmd`):

| File                  | Where it appears        |
|-----------------------|-------------------------|
| `logo_top_left.png`   | Top-left of each page   |
| `logo_top_right.png`  | Top-right of each page  |
| `logo_footer.png`     | Bottom-right footer     |

Missing files are skipped automatically — the page still renders without them,
just with an empty slot. To use different names or formats (e.g. `.svg`),
update the `logo_top_left` / `logo_top_right` / `logo_footer` paths in the
BRAND CONFIG block of `math_onepager.qmd`.
