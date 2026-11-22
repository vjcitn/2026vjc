# Extracting tables from RAND "The Science of Gun Policy" (4th ed., 2024; RRA243-9)
# with pdftools.  512 PDF pages; page numbers below are PDF page indices (the printed
# folios differ because of roman-numeral front matter).
#
# Layout facts this script is built around (checked against the file):
#  * Captions: "TABLE 2.3" on the first page, "Table 2.3—Continued" afterwards.
#  * Text tables sit on ordinary portrait pages (2.1-2.4, 18.1, A.1-A.10, ...).
#  * Tables S.1, 2.5 and 21.1 are on pages rotated 90 degrees; running-head text and the
#    page number sit in the margin and must be filtered out by x position.
#  * Column headers are often CENTRED over their data, so left-edge detection from
#    headers is unreliable; the script lets you give either column left edges
#    (left-aligned tables) or column centres (centred tables).
#  * Cells wrap, so one record can span several text lines.
#
# install.packages("pdftools")
library(pdftools)

pdf_file <- "scienceOfGunPolicy.pdf"   # adjust path

# ---------------------------------------------------------------------------
# 0. Helpers
# ---------------------------------------------------------------------------
.cache <- new.env()
page_words <- function(p) {                       # one data.frame of words per page
  if (is.null(.cache$all)) .cache$all <- pdf_data(pdf_file)
  w <- .cache$all[[p]]
  w$x2 <- w$x + w$width
  w$yc <- w$y + w$height / 2                      # vertical centre
  w$cx <- w$x + w$width / 2                       # horizontal centre
  w
}

# Group words into text lines.  Superscripts (footnote letters) sit ~3 pt off the
# baseline, so the tolerance must be bigger than that but smaller than line pitch (~8+).
add_lines <- function(w, tol = 4) {
  w <- w[order(w$yc, w$x), ]
  w$line <- cumsum(c(1, diff(w$yc) > tol))
  w
}

line_text <- function(w) tapply(w$text[order(w$line, w$x)], w$line[order(w$line, w$x)],
                                paste, collapse = " ")

# ---------------------------------------------------------------------------
# 1. Table index: which PDF pages hold which table?
# ---------------------------------------------------------------------------
table_index <- function() {
  txt <- pdf_text(pdf_file)
  pat <- "^\\s*(TABLE|Table)\\s+([A-Z]\\.[0-9]+|[0-9]+\\.[0-9]+)(\u2014Continued)?\\s*$"
  out <- lapply(seq_along(txt), function(p) {
    ln <- strsplit(txt[p], "\n")[[1]]
    m  <- regmatches(ln, regexec(pat, ln, perl = TRUE))
    m  <- Filter(length, m)
    if (!length(m)) return(NULL)
    data.frame(table = vapply(m, `[`, "", 3), pdf_page = p,
               continued = nzchar(vapply(m, `[`, "", 4)))
  })
  do.call(rbind, out)
}
idx <- table_index()
aggregate(list(pdf_pages = idx$pdf_page), list(table = idx$table),
          function(x) paste(x, collapse = ", "))               # one row per table

# ---------------------------------------------------------------------------
# 2. Generic geometric extractor
#    table_id      "A.3": which caption on the page to start below
#    header_first  regex for the FIRST word of the (last) header line you anchor on
#    header_span   c(lines_above, lines_below) of that anchor line that belong to the header
#    edges         left-aligned tables: column left edges, in points RELATIVE to the table's
#                  leftmost word (odd/even pages have different margins)
#    centres       centred tables: absolute x centres of the columns (nearest centre wins)
#    record        "gap"    rows are separated by extra vertical space (blank line between)
#                  "anchor" a new row starts on each line where column `anchor_col` is non-empty
#    x_min/x_max   discard margin text (rotated pages: page number at left, running head at right)
# ---------------------------------------------------------------------------
read_table <- function(p, table_id, header_first, header_span = c(0, 0),
                       edges = NULL, centres = NULL,
                       record = c("gap", "anchor"), anchor_col = NULL,
                       stop_pat = "^(NOTE|SOURCE|Note|Source):?$",
                       y_max = 650, x_min = -Inf, x_max = Inf,
                       gap_factor = 1.35, col_names = NULL) {
  record <- match.arg(record)
  w <- add_lines(page_words(p))
  w <- w[w$x > x_min & w$x < x_max, ]

  # the caption line for this table, then everything below it
  lt  <- line_text(w)
  cap <- grep(paste0("^(TABLE|Table) ", gsub("\\.", "\\\\.", table_id), "($|\u2014)"), lt)
  if (!length(cap)) stop("caption for table ", table_id, " not found on page ", p)
  cap_y <- min(w$yc[w$line == as.integer(names(lt)[cap[1]])])
  w <- w[w$yc > cap_y & w$y < y_max, ]

  lines    <- sort(unique(w$line))
  leftmost <- vapply(lines, function(l) { s <- w[w$line == l, ]; s$text[which.min(s$x)] }, "")
  i0 <- which(grepl(header_first, leftmost))[1]
  if (is.na(i0)) stop("header line '", header_first, "' not found on page ", p)
  hdr_ids  <- lines[(i0 - header_span[1]):(i0 + header_span[2])]
  rest     <- i0 + header_span[2] + 1
  body_ids <- if (rest <= length(lines)) lines[rest:length(lines)] else integer(0)

  for (l in body_ids) {                            # stop at NOTE:/SOURCE:/footnote line
    first <- leftmost[match(l, lines)]
    if (grepl(stop_pat, first)) { body_ids <- body_ids[body_ids < l]; break }
  }
  w <- w[w$line %in% c(hdr_ids, body_ids), ]

  # --- columns
  if (!is.null(edges)) {
    e <- min(w$x) + edges
    w$col <- findInterval(w$x, e - 0.5)
    ncol  <- length(e)
  } else {
    w$col <- max.col(-abs(outer(w$cx, centres, "-")), ties.method = "first")
    ncol  <- length(centres)
  }
  w <- w[w$col >= 1, ]

  cell <- function(ids, cc) {
    s <- w[w$line %in% ids & w$col == cc, ]
    s <- s[order(s$line, s$x), ]
    paste(s$text, collapse = " ")
  }

  # --- records (a record = one or more text lines)
  ym <- tapply(w$yc, w$line, mean)
  ym <- ym[as.character(body_ids)]
  recs <- list(); cur <- integer(0)
  if (record == "gap") {
    thr <- gap_factor * min(diff(ym))
    for (i in seq_along(body_ids)) {
      if (length(cur) && ym[i] - ym[cur[length(cur)]] > thr) { recs[[length(recs) + 1]] <- cur; cur <- integer(0) }
      cur <- c(cur, i)
    }
  } else {
    for (i in seq_along(body_ids)) {
      has <- any(w$line == body_ids[i] & w$col == anchor_col)
      if (has && length(cur)) { recs[[length(recs) + 1]] <- cur; cur <- integer(0) }
      cur <- c(cur, i)
    }
  }
  if (length(cur)) recs[[length(recs) + 1]] <- cur

  body <- t(vapply(recs, function(r) vapply(seq_len(ncol), function(cc) cell(body_ids[r], cc), ""),
                   character(ncol)))
  if (ncol == 1) body <- t(body)
  hdr <- vapply(seq_len(ncol), function(cc) cell(hdr_ids, cc), "")
  out <- as.data.frame(body, stringsAsFactors = FALSE)
  names(out) <- if (is.null(col_names)) make.names(hdr, unique = TRUE) else col_names
  out[] <- lapply(out, function(x) trimws(gsub("\\s+", " ", x)))
  attr(out, "header") <- hdr
  out
}

# Footnote letters ride along as lone lowercase words after ")" -- drop them
strip_fn <- function(x) sub("\\)\\s+[a-f]$", ")", x)

# ---------------------------------------------------------------------------
# 3. Table A.3 (p. 492): study / policy / outcome / IRR [95% CI]   -> numeric
#    left-aligned columns, rows separated by space, wrapped cells
# ---------------------------------------------------------------------------
a3 <- read_table(492, "A.3", "^Study$", edges = c(0, 131, 228, 322),
                 col_names = c("study", "policy", "outcome", "irr_ci"))
m <- regmatches(a3$irr_ci, regexec("^([0-9.]+) \\[([0-9.]+)[\u2013-]([0-9.]+)\\]$", a3$irr_ci))
a3[c("irr", "ci_lo", "ci_hi")] <- lapply(2:4, function(i) as.numeric(vapply(m, function(z) z[i], "")))
a3$ci_excludes_1 <- a3$ci_lo > 1 | a3$ci_hi < 1
stopifnot(nrow(a3) == 18, !anyNA(a3$irr))
a3

# ---------------------------------------------------------------------------
# 4. Table 18.1 (p. 397): two text columns; footnote letters after the year
# ---------------------------------------------------------------------------
t181 <- read_table(397, "18.1", "^Study$", edges = c(0, 121),
                   stop_pat = "^(NOTE|SOURCE|a|b)$",
                   col_names = c("study", "significant_effect"))
t181$study <- strip_fn(t181$study)
t181

# ---------------------------------------------------------------------------
# 5. Table A.6 (p. 501): old-vs-new conclusions.  Rows are NOT separated by space and the
#    first column wraps, so anchor each record on the "No. of Studies" cell.
#    (A.4-A.10 share this structure but each page has slightly different column offsets:
#     re-measure `edges` per page -- see suggest_edges() below.)
# ---------------------------------------------------------------------------
a6 <- read_table(501, "A.6", "^Policy$", header_span = c(2, 0),
                 edges = c(0, 113, 136, 252, 276), record = "anchor", anchor_col = 2,
                 col_names = c("policy", "n_2022", "conclusion_2022", "n_2024", "conclusion_2024"))
a6$n_2022 <- as.integer(a6$n_2022); a6$n_2024 <- as.integer(a6$n_2024)
a6

# Helper for new tables: frequent left-x positions of words in the table band.  Column
# left edges show up as x values shared by many lines; subtract the smallest x to get `edges`.
suggest_edges <- function(p, table_id, min_lines = 4) {
  w <- add_lines(page_words(p)); lt <- line_text(w)
  cap <- grep(paste0("^(TABLE|Table) ", gsub("\\.", "\\\\.", table_id)), lt)[1]
  w <- w[w$yc > min(w$yc[w$line == as.integer(names(lt)[cap])]) & w$y < 650, ]
  tab <- tapply(w$line, round(w$x), function(l) length(unique(l)))
  pk  <- as.numeric(names(tab)[tab >= min_lines])
  data.frame(x = pk, rel = pk - min(w$x), n_lines = as.integer(tab[tab >= min_lines]))
}
suggest_edges(501, "A.6")

# ---------------------------------------------------------------------------
# 6. Table 2.3 (pp. 69-71): 182 included studies in two side-by-side "No. / Study" blocks.
#    Plain text lines are enough here; reshape to long.
# ---------------------------------------------------------------------------
txt <- pdf_text(pdf_file)
rows <- unlist(lapply(69:71, function(p) strsplit(txt[p], "\n")[[1]]))
m <- regmatches(rows, regexec("^\\s*(\\d+)\\s+(.+?)(?:\\s{2,}(\\d+)\\s+(.+?))?\\s*$", rows, perl = TRUE))
m <- Filter(function(z) length(z) && nzchar(z[3]), m)                 # drop page numbers etc.
left  <- do.call(rbind, lapply(m, function(z) data.frame(no = as.integer(z[2]), study = z[3])))
right <- do.call(rbind, lapply(Filter(function(z) nzchar(z[4]), m),
                               function(z) data.frame(no = as.integer(z[4]), study = z[5])))
t23 <- rbind(left, right)
t23 <- t23[order(t23$no), ]
stopifnot(identical(t23$no, 1:182))                                    # every study 1..182 once
head(t23)

# ---------------------------------------------------------------------------
# 7. Table 2.5 (pp. 73-75): studies by policy x outcome.  Rotated pages, CENTRED headers,
#    cells list study numbers (-> link to Table 2.3).  Use column centres, anchor on Total.
#    If pdf_data() on a rotated page returns coordinates in the unrotated frame, the x
#    values below won't line up: check range(page_words(73)$x) -- expect ~72..620.
# ---------------------------------------------------------------------------
centres25 <- c(96, 185, 257, 319, 375, 424, 474, 524, 571, 617)
outcomes  <- c("policy", "suicide", "violent_crime", "unintentional", "mass_shootings",
               "police_shootings", "defensive_gun_use", "hunting_recreation", "gun_industry", "total")
t25 <- do.call(rbind, lapply(73:75, function(p)
  read_table(p, "2.5", "^Policy$", header_span = c(2, 0), centres = centres25,
             record = "anchor", anchor_col = 10, x_min = 70, x_max = 640, col_names = outcomes)))
t25$total <- as.integer(sub(" [a-z]$", "", t25$total))                # drop footnote letter
# explode "5, 20, 62" cells into (policy, outcome, study_no) rows and join to Table 2.3
long25 <- do.call(rbind, lapply(outcomes[2:9], function(o) {
  s <- strsplit(t25[[o]], ",\\s*")
  data.frame(policy = rep(t25$policy, lengths(s)), outcome = o,
             no = as.integer(unlist(s[lengths(s) > 0 & nzchar(t25[[o]])])))
}))
long25 <- merge(long25, t23, by = "no")
head(long25)

# ---------------------------------------------------------------------------
# 8. Table S.1 (pp. 10-12) and 21.1 (pp. 454-456): evidence-rating grids.  NOT fully parsed.
#    Rotated pages; column headers are vertical text; cells are symbols from a dingbat
#    font that poppler returns as stray letters: "\u00ea" = decrease, "\u00e9" = increase,
#    S/M/L = strength (supportive/moderate/limited), I = inconclusive, small trailing
#    letters = footnote superscripts.  The glyph/letter tokens come out as separate words
#    (e.g. "\u00ea" then "M,"), so a robust grid needs per-page column clustering.  What this
#    gives you is a tidy dump with decoded symbols for your own clustering / checking.
# ---------------------------------------------------------------------------
decode_glyph <- function(x) {
  x <- gsub("\u00ea", "\u2193", x)    # decrease
  x <- gsub("\u00e9", "\u2191", x)    # increase
  x
}
dump_grid_words <- function(p, x_min = 135, y_min = 180) {
  w <- page_words(p)
  w <- w[w$x >= x_min & w$y >= y_min & w$height < 12, ]
  w$symbol <- decode_glyph(w$text)
  w[order(w$yc, w$x), c("x", "y", "cx", "yc", "text", "symbol")]
}
# write.csv(dump_grid_words(10), "table_S1_p10_words.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 9. Save results
# ---------------------------------------------------------------------------
# write.csv(a3,   "table_A3_irr.csv",       row.names = FALSE)
# write.csv(t181, "table_18_1.csv",         row.names = FALSE)
# write.csv(a6,   "table_A6_changes.csv",   row.names = FALSE)
# write.csv(t23,  "table_2_3_studies.csv",  row.names = FALSE)
# write.csv(t25,  "table_2_5_matrix.csv",   row.names = FALSE)
