-- SPDX-License-Identifier: GPL-3.0-or-later

local argKeywordTranslation = {
  ["eval"] = "eval",
  ["echo"] = "exports",
  ["include"] = "exports",
  ["purl"] = "tangle",
  ["cache"] = "cache"
}
  
local argValueTranslations = {
  ["TRUE"] = "yes",
  ["FALSE"] = "no",
}

local argKeyValueTranslations = {
  ["echo-FALSE"] = "results",
  ["echo-TRUE"] = "both",
  ["include-FALSE"] = "none",
  ["include-TRUE"] = "both",
}

local function reformatHeaderArgs(headerArgs)
  local out = ""
  for header in string.gmatch(headerArgs..",", "%s*([^,][^,]-)%s*,") do
    local hKey = string.match(header, "^([^=][^=]-)%s*=")
    local hVal = string.match(header, "=%s*(.-)%s*$")
    local key = argKeywordTranslation[hKey]
    local val
    if hVal ~= nil then
      val = argKeyValueTranslations[hKey.."-"..hVal]
    end
    if val == nil then
      val = argValueTranslations[hVal]
    end
    if key ~= nil and val ~= nil then
      out = out.." :"..key.." "..val
    end
  end
  return out
end

local function generateSrcInfo(body)
  local beginTag = "#+BEGIN_SRC"
  local endTag = "#+END_SRC"
  if ("{" == string.sub(body, 1, 1)) then
    local firstLine = string.match(body, "^([^\n]+)")
    local lang = string.match(firstLine, "{%s*([^,]-)%s*[,}]")
    if lang == nil then
      lang = ""
      local beginTag = "#+BEGIN_EXAMPLE"
      local endTag = "#+END_EXAMPLE"
    elseif lang == "r" then
      lang = " R"
    else
      lang = " "..lang
    end
    local argsRaw = string.match(firstLine, "{[^,]+(.-)%s*}")
    local args
    if argsRaw ~= nil and argsRaw ~= "" then
      args = reformatHeaderArgs(argsRaw)
    else
      args = argsRaw 
    end
    body = string.gsub(body, "^[^\n]+\n", "")
    return "\n"..beginTag..lang..args.."\n"..body.."\n"..endTag.."\n"
  end
  return "\n#+BEGIN_EXAMPLE\n"..body.."\n#+END_EXAMPLE\n"
end

return {
  {
    Code = function(elem)
      return elem
    end
  },
  {
    CodeBlock = function(elem)
      return pandoc.RawBlock("Org", generateSrcInfo(elem.text))
    end
  }
}
