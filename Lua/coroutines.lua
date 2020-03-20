function permgen (a, n, callback)
  if n == 0 then
    callback(a)
  else
    for i=1,n do
      a[n], a[i] = a[i], a[n]       -- swap ith and last elements
      permgen(a, n - 1, callback)   -- permute the rest
      a[n], a[i] = a[i], a[n]       -- restore
    end
  end
end

function perm (a)
  -- local co = coroutine.create(function () permgen(a, #a, coroutine.yield) end)
  -- return function ()   -- iterator
  --   local _, res = coroutine.resume(co)
  --   return res
  -- end
  return coroutine.wrap(function () permgen(a, #a, coroutine.yield) end)
end

function printArray (a)
  io.write("[")
  for i,v in ipairs(a) do
    io.write(i == 1 and "" or " ", v)
  end
  io.write("]\n")
end

do
  -- permgen({1, 2, 3}, 3, printArray)
  for p in perm{1, 2, 3} do
    printArray(p)
  end
end
