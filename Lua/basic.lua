do
  local input = ""

  -- repeat-until is eqv. to do-while
  repeat
    io.write("> ")
    input = io.read()
  until true

  -- nil and false are the only non-true values
  local n = tonumber(input) or 5
  n = n > 0 and n or -n   -- equivalent to ternary if n > 0 ? n : n

  local table = {}

  -- numeric for is exactly like a built-in range()
  for i=1,n do   -- 1-based indexing (!)
    table[i] = i
  end

  -- varargs
  function foo (i, ...)
    local n = select('#', ...)
    if n > 0 then
      local arg = {...}
      for i,v in ipairs(arg) do   -- generic for uses iterators
        print(string.format("%d: %s", i, tostring(v)))
      end
      return arg[i]
    else
      error("n = " .. n)
    end
  end

  -- multiple returns, unpacking and exception handling with protected calls
  status, ret = pcall(function() return foo(1, unpack(table)) end)
  print(ret)

end
