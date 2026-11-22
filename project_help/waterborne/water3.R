# Demonstration: extracting tables from the CDC WBDOSS 2021 report with pdftools
# install.packages(c("pdftools", "dplyr"))   # dplyr only used for tidying
library(pdftools)

pdf_file <- "2021_Annual_Waterborne_Disease_Surveillance_Report.pdf"

# ---------------------------------------------------------------------------
# 1. Orientation: page count, metadata, one character string per page
# ---------------------------------------------------------------------------
pdf_info(pdf_file)[c("pages", "created", "version")]
pages <- pdf_text(pdf_file)          # character vector, length = number of pages
length(pages)

# Which pages hold which tables?  (captions look like "Table 8. ...")
cap <- lapply(seq_along(pages), function(i) {
  m <- regmatches(pages[i], gregexpr("Table [0-9]+\\.", pages[i]))[[1]]
  if (length(m)) data.frame(page = i, table = sub("\\.$", "", m)) else NULL
})
table_index <- do.call(rbind, cap)
table_index    # note: a caption can sit on the page *before* the table body

# ---------------------------------------------------------------------------
# 2. Easy case: simple "N (%)" tables, parsed line by line with regexes
#    (Tables 1, 3, 5, 6, 7, 10-13: one row per line, numeric columns)
# ---------------------------------------------------------------------------
page_lines <- function(p) strsplit(pages[p], "\n")[[1]]

# tokens such as "32 (62)" or a bare "0" -> count and (optional) percent
parse_np_row <- function(line) {
  line <- gsub("[†‡§*]", "", line)           # footnote marks
  m <- regmatches(line, regexec("^(.+?)\\s+(\\d+(?:\\s*\\(\\d+\\))?(?:\\s+\\d+(?:\\s*\\(\\d+\\))?)*)\\s*$", line,
                                perl = TRUE))[[1]]
  if (length(m) == 0) return(NULL)
  tok <- regmatches(m[3], gregexpr("\\d+(?:\\s*\\(\\d+\\))?", m[3], perl = TRUE))[[1]]
  n   <- as.integer(sub("\\s*\\(.*$", "", tok))
  pct <- suppressWarnings(as.integer(sub("^.*\\((\\d+)\\).*$", "\\1", ifelse(grepl("\\(", tok), tok, NA))))
  list(label = trimws(m[2]), n = n, pct = pct)
}

# Take the lines from a header pattern through the "Total" row and parse them
read_np_table <- function(page, header_pat, col_names) {
  ln <- page_lines(page)
  h     <- grep(header_pat, ln)[1]
  start <- if (is.na(h)) 1 else h + 1          # fall back to top of page if header not matched
  end   <- grep("^\\s*Total", ln)
  end   <- end[end >= start][1]
  if (is.na(end)) stop("no 'Total' row found on page ", page, "; inspect page_lines(", page, ")")
  rows  <- Filter(Negate(is.null), lapply(ln[start:end], parse_np_row))
  rows  <- Filter(function(r) any(!is.na(r$pct)), rows)   # real rows always have a "(%)"
  out <- do.call(rbind, lapply(rows, function(r) {
    vals <- as.vector(rbind(r$n, r$pct))           # n1, pct1, n2, pct2, ...
    names(vals) <- as.vector(t(outer(col_names, c("_n", "_pct"), paste0)))
    data.frame(label = r$label, as.list(vals)[seq_len(2 * length(col_names))],
               check.names = FALSE)
  }))
  rownames(out) <- NULL
  out
}

# Table 1 (printed on page 3): outbreaks / cases / hospitalizations / deaths by water exposure
tab1 <- read_np_table(3, "^\\s*(Water\\s+)?Exposure", c("outbreaks", "cases", "hosp", "deaths"))
tab1

# Table 3 (page 6): outbreaks and cases by setting
tab3 <- read_np_table(6, "^Setting\\s+N", c("outbreaks", "cases"))
tab3

# Sanity check against the printed totals -- always worth doing
stopifnot(sum(tab3$outbreaks_n[tab3$label != "Total"]) == 52,
          sum(tab3$cases_n[tab3$label != "Total"])     == 511)

# ---------------------------------------------------------------------------
# 3. Harder case: wide tables with wrapped cells (Tables 8, 9, 14, 16)
#    Line-based parsing breaks because one record spans several text lines.
#    pdf_data() returns every word with x/y coordinates, so we can rebuild
#    the grid geometrically.
# ---------------------------------------------------------------------------
w <- pdf_data(pdf_file)[[11]]        # list element i = page i; columns: width height x y space text
head(w)

# Strategy (checked against the geometry poppler reports for pages 11-12):
#  * header words give each column a centre x (headers/data are centred, not
#    left-aligned, so we assign words to the NEAREST column centre)
#  * every record has a single-line first-column word (the jurisdiction); its y
#    is the record's anchor, and cells are vertically centred on it
#  * text lines in each column are grouped into runs (consecutive lines closer
#    than `line_gap`), and each run goes to the anchor nearest the run's middle
#    -- this keeps tall wrapped cells (e.g. 5-line etiology) with the right row.
# col_labels: named list, one entry per column, each a list of c(prefix, k) =
#   "k-th header word (left to right) starting with prefix". Prefix matching is
#   needed because poppler may split long header words ("Hospitalizati"/"ons").
read_wide_table <- function(page, col_labels, line_gap = 14) {
  w <- pdf_data(pdf_file)[[page]]
  w$cx <- w$x + w$width / 2

  # --- header band: anchored on the first column's header word
  jy  <- min(w$y[startsWith(w$text, col_labels[[1]][[1]][1])])
  hdr <- w[w$y >= jy - 10 & w$y <= jy + 15, ]
  centre <- vapply(col_labels, function(spec) {
    mean(vapply(spec, function(p) {
      i <- which(startsWith(hdr$text, p[1]))
      i <- i[order(hdr$x[i])][as.integer(p[2])]
      if (is.na(i)) stop("header word not found: '", p[1], "' (#", p[2], ") on page ", page)
      hdr$cx[i]
    }, numeric(1)))
  }, numeric(1))

  body <- w[w$y > max(hdr$y), ]
  nearest <- function(x, ref) max.col(-abs(outer(x, ref, "-")), ties.method = "first")
  body$col <- nearest(body$cx, centre)

  # --- stop at prose/footnote that starts in the first column ("The ...", "*...")
  stop_y <- body$y[body$col == 1 & grepl("^(The|\\*)", body$text)]
  if (length(stop_y)) body <- body[body$y < min(stop_y), ]

  anchors <- sort(unique(body$y[body$col == 1]))
  body$row <- NA_integer_
  for (cc in seq_along(centre)) {
    sel <- body$col == cc
    ys  <- sort(unique(body$y[sel]))
    run <- cumsum(c(1, diff(ys) > line_gap))
    mid <- tapply(ys, run, function(v) mean(range(v)))
    run_row <- nearest(as.numeric(mid), anchors)
    body$row[sel] <- run_row[run[match(body$y[sel], ys)]]
  }

  body <- body[order(body$row, body$col, body$y, body$x), ]
  cells <- tapply(body$text, list(factor(body$row, seq_along(anchors)),
                                  factor(body$col, seq_along(centre))),
                  paste, collapse = " ")
  out <- as.data.frame(cells, stringsAsFactors = FALSE)
  names(out) <- names(col_labels)
  out[] <- lapply(out, function(x) trimws(gsub("\\s+", " ", x)))
  rownames(out) <- NULL
  out
}

# Table 8 (treated recreational water outbreaks) spans pages 11-12.
h <- function(prefix, k = 1) c(prefix, k)
cols8 <- list(Jurisdiction = list(h("Jurisdiction")),
              Month        = list(h("Month")),
              Status       = list(h("Status")),
              Etiology     = list(h("Etiology", 2)),   # "Etiology" appears twice in the header
              Cases        = list(h("Cases")),
              Hosp         = list(h("Hospitalizati")), # split by poppler: "Hospitalizati" + "ons"
              Deaths       = list(h("Deaths")),
              Venue        = list(h("Water", 1), h("Venue(s)")),
              Setting      = list(h("Water", 2), h("Setting")))

# Inspect the geometry first -- header positions vary by page and layout:
subset(pdf_data(pdf_file)[[11]], y < 120)[, c("x", "y", "text")]

tab8 <- do.call(rbind, lapply(11:12, function(p) read_wide_table(p, cols8)))
num <- c("Cases", "Hosp", "Deaths")
tab8[num] <- lapply(tab8[num], as.integer)
# poppler splits some long words across lines ("Subdivision/Neig" + "hborhood");
# in the pasted cell text this leaves a stray space -- repair the known cases:
tab8$Setting <- sub("/(Neig|Mun|Cond|U) ", "/\\1", tab8$Setting)
tab8        # EXPECT 22 rows; if not, check the header prefixes in cols8 and `line_gap`

# ---------------------------------------------------------------------------
# 4. Alternative for gnarly tables: tabulapdf (successor of tabulizer).
#    Needs Java, but handles ruled/wrapped cells with its own lattice/stream
#    detection, and pairs well with pdftools for locating the right pages.
# ---------------------------------------------------------------------------
# install.packages("tabulapdf")
# library(tabulapdf)
# t14 <- extract_tables(pdf_file, pages = 20:21, guess = TRUE, output = "data.frame")

