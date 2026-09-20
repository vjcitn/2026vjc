return {
  ["git-rev"] = function(args, kwargs, meta)
    local handle = io.popen("git rev-parse --short HEAD 2>/dev/null")
    local result = handle:read("*a")
    handle:close()
    result = result:gsub("%s+", "")
    if result == "" then
      result = "unknown"
    end
    return pandoc.Inlines{pandoc.Str(result)}
  end
}
