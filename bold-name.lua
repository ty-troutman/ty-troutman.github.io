local patterns = {
  "Troutman[^,.]*,%s*T%.?%s*D%.?",
  "T%.?%s*D%.?%s*Troutman",
  "Troutman,%s*TD",
  "TD%s*Troutman"
}

function Div(el)
  if not el.classes:includes("csl-entry") then
    return nil
  end

  local text = pandoc.utils.stringify(el.content)

  for _, pat in ipairs(patterns) do
    text = text:gsub(pat, function(m)
      return "<strong>" .. m .. "</strong>"
    end)
  end

  return pandoc.Div({ pandoc.RawBlock("html", text) }, el.attr)
end

function Header(h)
  if h.level == 1 then
    h.content:insert(1, pandoc.Str("[LUA ACTIVE] "))
    return h
  end
end