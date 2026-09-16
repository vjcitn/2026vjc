-- Wraps any Div with class "keep" in a LaTeX minipage so its content
-- (headings, code blocks, answer-space rules) cannot be split across a
-- page break by the PDF engine. No-op for non-PDF output formats.
function Div(el)
  if el.classes:includes("keep") and quarto.doc.is_format("pdf") then
    table.insert(el.content, 1, pandoc.RawBlock('latex', '\\begin{minipage}{\\linewidth}'))
    table.insert(el.content, pandoc.RawBlock('latex', '\\end{minipage}'))
    return el.content
  end
end
