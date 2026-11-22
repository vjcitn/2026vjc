# BibTeX for the CDC "2021 Annual Waterborne Disease Surveillance Report" (WBDOSS summary)
#
# What this document does about citations (checked against the file):
#  * Numbered (Vancouver-style) in-text citations: "(1)", "(2)".
#  * A two-entry "References" list on PDF page 25:
#      1. Collier SA, Deng L, Adam EA, et al. <title>. Emerging Infectious Diseases. 2021;27(1):140-149. doi:...
#      2. Information about Public Water Systems. 2021 11/10/2021 [cited 2021 11/22]; Available from: <URL>
#    (entry 2's URL is split across two lines: "https://www.epa.g" / "ov/dwreginfo/...").
#  * Most of the real "sources" are systems, datasets and forms named in the Methods/Conclusions
#    with URLs -- these become @misc.  Their URLs below were taken from the PDF's hyperlink
#    annotations (the visible text of some is cut by line breaks), not retyped.
#  * Page 26 gives a "Suggested Citation" for the report itself -> @techreport.
#
# install.packages("pdftools")   # httr2 only for check_urls()
library(pdftools)

pdf_file <- "3c9121b4-2021_Annual_Waterborne_Disease_Surveillance_Report.pdf"   # adjust path
ACCESSED <- NA        # set to e.g. "2026-11-22" once YOU have checked the links; emitted as urldate

# ---------------------------------------------------------------------------
# 1. Parse the numbered reference list (page 25)
# ---------------------------------------------------------------------------
ref_entries <- function(page = 25) {
  ln <- trimws(strsplit(pdf_text(pdf_file)[page], "\n")[[1]])
  ln <- ln[nzchar(ln)]
  i  <- which(ln == "References")
  ents <- list()
  for (l in ln[-seq_len(i)]) {
    m <- regmatches(l, regexec("^([0-9]+)\\.\\s+(.*)$", l))[[1]]
    if (length(m)) { ents[[m[2]]] <- m[3]; cur <- m[2]; next }
    prev <- ents[[cur]]
    # a line that ends mid-URL is glued on without a space (fixes "epa.g" + "ov/...")
    ents[[cur]] <- if (grepl("http", prev) && !endsWith(prev, ".")) paste0(prev, l) else paste(prev, l)
  }
  unlist(ents)                                  # named by reference number
}
refs_raw <- ref_entries()

# Vancouver author list "Collier SA, Deng L, Adam EA, et al" -> BibTeX "Collier, S. A. and ... and others"
vancouver_authors <- function(s) {
  others <- grepl(",\\s*et al$", s)
  s  <- sub(",\\s*et al$", "", s)
  au <- trimws(strsplit(s, ",")[[1]])
  one <- function(a) {
    tok <- strsplit(a, " ")[[1]]
    fam <- paste(tok[-length(tok)], collapse = " "); ini <- tok[length(tok)]   # "Collier" "SA"
    sprintf("%s, %s", fam, paste0(strsplit(ini, "")[[1]], ".", collapse = " "))
  }
  paste(c(vapply(au, one, ""), if (others) "others"), collapse = " and ")
}

# Entry type 1: journal article "Authors. Title. Journal. YYYY;vol(issue):pages. doi:..."
parse_vancouver_article <- function(e, num) {
  m <- regmatches(e, regexec(paste0("^(.*?(?:et al)?)\\.\\s+(.*?)\\.\\s+(.*?)\\.\\s+((?:19|20)[0-9]{2});",
                                    "([0-9]+)\\(([^)]*)\\):([0-9-]+)\\.\\s*(?:doi:(\\S+?)\\.?)?$"),
                                    e, perl = TRUE))[[1]]
  if (!length(m)) stop("reference ", num, " does not look like a Vancouver journal article:\n", e)
  list(type = "article", num = num,
       author = vancouver_authors(m[2]), title = m[3], journal = m[4], year = m[5],
       volume = m[6], number = m[7], pages = sub("-", "--", m[8]), doi = m[9])
}

# Entry type 2: web page "Title. YYYY MM/DD/YYYY [cited YYYY MM/DD]; Available from: URL"
parse_vancouver_web <- function(e, num) {
  m <- regmatches(e, regexec(paste0("^(.*?)\\.\\s+((?:19|20)[0-9]{2})\\s+([0-9]{2}/[0-9]{2}/(?:19|20)[0-9]{2})\\s+",
                                    "\\[cited\\s+((?:19|20)[0-9]{2})\\s+([0-9]{2}/[0-9]{2})\\];\\s+Available from:\\s+(\\S+?)\\.?$"),
                                    e, perl = TRUE))[[1]]
  if (!length(m)) stop("reference ", num, " does not look like a Vancouver web page:\n", e)
  upd <- as.Date(m[4], "%m/%d/%Y"); cit <- as.Date(paste(m[5], m[6], sep = "/"), "%Y/%m/%d")
  list(type = "misc", num = num, title = m[2], year = m[3], url = m[7],
       # The list gives no author; the host (epa.gov) identifies the publisher.  Recorded as inferred.
       author = if (grepl("epa\\.gov", m[7])) "{U.S. Environmental Protection Agency}" else NA,
       note = sprintf("Page last updated %s", format(upd)), urldate = format(cit),
       annote = "Author inferred from URL host; not stated in the reference list.")
}

refs <- list(
  Collier2021 = parse_vancouver_article(refs_raw[["1"]], 1),
  EPA2021PWS  = parse_vancouver_web(refs_raw[["2"]], 2)
)
# Map the report's numeric citations to BibTeX keys
numeric_keys <- setNames(names(refs), vapply(refs, function(r) as.character(r$num), ""))

# ---------------------------------------------------------------------------
# 2. Curated @misc / @techreport entries.  Every URL below appears in the PDF itself: the
#    first five as visible text (pp. 1 and 25), the two forms as hyperlink targets on p. 1.
# ---------------------------------------------------------------------------
cdc <- "{Centers for Disease Control and Prevention}"
curated <- list(
  Lawinger2023WBDOSS = list(type = "techreport",
    author = "Lawinger, H. and Hlavsa, M. and Miko, S. and Kunz, J. and Thuneibat, M. and Gerdes, M. and Gleason, M. and Roberts, V.",
    title = "Waterborne Disease and Outbreak Surveillance System ({WBDOSS}) Summary Report, {United States}, 2021",
    institution = "U.S. Department of Health and Human Services, CDC", address = "Atlanta, Georgia", year = "2023",
    annote = "Citation text from the report's own 'Suggested Citation' (p. 26)."),
  CDCWBDOSS = list(type = "misc", author = cdc,
    title = "Waterborne Disease and Outbreak Surveillance System ({WBDOSS})",
    url = "https://www.cdc.gov/healthywater/surveillance/index.html",
    note = "National surveillance for waterborne disease outbreaks since 1971"),
  CDCNORS = list(type = "misc", author = cdc, title = "National Outbreak Reporting System ({NORS})",
    url = "https://www.cdc.gov/nors/index.html",
    note = "Web-based platform used by jurisdictions to report outbreaks since 2009"),
  CDCNORSDashboard = list(type = "misc", author = cdc, title = "{NORS} Dashboard",
    url = "https://www.cdc.gov/nors/data/dashboard/index.html",
    note = "2021 data in the report: outbreaks reported through NORS as of 19 January 2023; later pulls may differ"),
  CDCNORSForm5212 = list(type = "misc", author = cdc,
    title = "National Outbreak Reporting System reporting form, {CDC} 52.12 (waterborne; used 2017--January 2023)",
    url = "https://www.cdc.gov/healthywater/statistics/wbdoss/nors/forms_archive/CDC_5212_02_2017.pdf"),
  CDCNORSForm5214 = list(type = "misc", author = cdc,
    title = "National Outbreak Reporting System reporting form, {CDC} 52.14 (all modes of transmission; released January 2023)",
    url = "https://www.cdc.gov/nors/pdf/NORS-Form-CDC-52.14_508.pdf"),
  CDCLegionellaWMP = list(type = "misc", author = cdc, title = "Legionella Water Management Program Toolkit",
    url = "https://www.cdc.gov/legionella/wmp/toolkit/index.html"),
  CDCMAHC = list(type = "misc", author = cdc, title = "Model Aquatic Health Code ({MAHC})",
    url = "https://www.cdc.gov/mahc")
)
all_refs <- c(refs, curated)

# ---------------------------------------------------------------------------
# 3. BibTeX writer
# ---------------------------------------------------------------------------
bib_esc <- function(x) gsub("([&%$#_])", "\\\\\\1", x)       # protects & % $ # _ in text fields
to_bibtex <- function(refs) {
  one <- function(key, r) {
    doi <- if (!is.null(r$doi) && !is.na(r$doi)) r$doi else NULL
    f <- c(author = r$author, title = paste0("{", r$title, "}"), journal = r$journal,
           volume = r$volume, number = r$number, pages = r$pages,
           institution = r$institution, address = r$address, year = r$year,
           doi = doi, url = if (!is.null(doi) && is.null(r$url)) paste0("https://doi.org/", doi) else r$url,
           urldate = if (!is.null(r$url) || !is.null(doi)) (if (is.null(r$urldate)) ACCESSED else r$urldate),
           note = r$note, annote = r$annote)
    f <- f[!vapply(f, function(x) is.null(x) || is.na(x) || !nzchar(x), NA)]
    esc <- ifelse(names(f) %in% c("url", "doi", "urldate", "title", "author"), f, bib_esc(f))
    paste0("@", r$type, "{", key, ",\n", paste(sprintf("  %s = {%s}", names(f), esc), collapse = ",\n"), "\n}")
  }
  paste(Map(one, names(refs), refs), collapse = "\n\n")
}
bib <- to_bibtex(all_refs)
cat(bib, "\n")
# writeLines(bib, "cdc_wbdoss_2021.bib")

# ---------------------------------------------------------------------------
# 4. Where does the report cite by number?  (review the hits; don't trust blindly)
#    Pattern: a word, then "(N)" or "(N, M)", then punctuation -- this excludes table cells
#    like "Other 1 (2)" where the number follows a digit.
# ---------------------------------------------------------------------------
find_numeric_citations <- function(max_ref = length(refs_raw)) {
  txt <- pdf_text(pdf_file)
  out <- lapply(seq_along(txt), function(p) {
    flat <- gsub("\\s+", " ", txt[p])
    m <- gregexpr("[A-Za-z]{2,}\\.? ?\\(([0-9]+(?:, ?[0-9]+)*)\\)[.,;]", flat, perl = TRUE)[[1]]
    if (m[1] == -1) return(NULL)
    hit  <- regmatches(flat, list(m))[[1]]
    nums <- lapply(strsplit(gsub("^.*\\(([0-9, ]+)\\)[.,;]$", "\\1", hit), ",\\s*"), as.integer)
    keep <- vapply(nums, function(n) all(n >= 1 & n <= max_ref), NA)
    if (!any(keep)) return(NULL)
    data.frame(page = p, cited = hit[keep], numbers = vapply(nums[keep], paste, "", collapse = ","),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}
find_numeric_citations()

# ---------------------------------------------------------------------------
# 5. Optional: check the URLs from your own machine (they may have moved: CDC reorganised
#    its site, and an old path can redirect or return a stub).  Not run here.
# ---------------------------------------------------------------------------
check_urls <- function(refs) {
  library(httr2)
  urls <- Filter(Negate(is.null), lapply(refs, `[[`, "url"))
  data.frame(key = names(urls), url = unlist(urls), status = vapply(urls, function(u) {
    tryCatch(resp_status(req_perform(req_method(request(u), "HEAD"))), error = function(e) NA_integer_)
  }, 1L), row.names = NULL)
}
# check_urls(all_refs)
