-- Compute the minimal number of pages that can host the given elements, given
-- their heights, some space to add between elements, and the height of pages.
--
function minimalNumberOfPages(eltHeights, interEltSpace, pageHeight)
  local numberOfPages = 1
  local curPageHeight = 0

  for _, eltHeight in ipairs(eltHeights) do
    local nextPageHeight = (curPageHeight == 0) and eltHeight or (curPageHeight + interEltSpace + eltHeight)

    if nextPageHeight <= pageHeight then
      curPageHeight = nextPageHeight
    else
      numberOfPages = numberOfPages + 1
      curPageHeight = eltHeight
    end
  end

  return numberOfPages
end

-- Auxiliary function to `balancedPages`. Given an array of element heights, a space
-- to add between each elements, a number of pages, and a first and last element to
-- consider in the list, compute the most balanced distribution of elements.
--
function balancedPagesAux(eltHeights, interEltSpace, numberOfPages, startEltIdx, endEltIdx)
  if endEltIdx < startEltIdx then
    return math.huge

  elseif numberOfPages <= 0 then
    return {cost = math.huge, elts = {}}

  elseif numberOfPages == 1 then
    local cost = 0
    for eltIdx = startEltIdx, endEltIdx do
      cost = cost + ((eltIdx == startEltIdx) and 0 or interEltSpace) + eltHeights[eltIdx]
    end
    return {cost = cost ^ 2, elts = {endEltIdx - startEltIdx + 1}}

  else
    local bestCost = math.huge
    local bestElts = {}
    for cutEltIdx = startEltIdx, (endEltIdx - (numberOfPages - 1)) do
      local before = balancedPagesAux(eltHeights, interEltSpace, 1, startEltIdx, cutEltIdx)
      local after = balancedPagesAux(eltHeights, interEltSpace, numberOfPages - 1, cutEltIdx + 1, endEltIdx)

      local thisCost = before.cost + after.cost
      if thisCost < bestCost then
        bestCost = thisCost
	bestElts = {table.unpack(before.elts), table.unpack(after.elts)}
      end
    end
    return {cost = bestCost, elts = bestElts}
  end
end

-- Compute the most balanced distribution of elements given their heights, some space to add
-- between elements, and the number of pages to span.
--
function balancedPages(eltHeights, interEltSpace, numberOfPages)
  local result = balancedPagesAux(eltHeights, interEltSpace, numberOfPages, 1, #eltHeights)
  return result.elts
end
