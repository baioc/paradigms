Account = {
  balance = 0,
  withdraw = function (self, v)
    if v > self.balance then
      error("insufficient funds")
    else
      self.balance = self.balance - v
    end
  end,
  deposit = function (self, v)
    self.balance = self.balance + v
  end
}

--[[
function Account.withdraw (v)
  Account.balance = Account.balance - v
end
Account.withdraw(100.00)
print(Account.balance)
--]]

--[[
function Account.withdraw (self, v)
  self.balance = self.balance - v
end
a = {balance=0, withdraw=Account.withdraw}
a.withdraw(a, 100.00)
print(a.balance)
--]]

--[[
function Account:withdraw (self, v)
  if v > self.balance then
    error("insufficient funds")
  else
    self.balance = self.balance - v
  end
end

a = {balance=0, withdraw=Account.withdraw}
a:withdraw(100.00)
print(a.balance)
--]]

--[[
function Account:new (o)
  o = o or {}   -- create object if user does not provide one
  setmetatable(o, self)
  self.__index = self
  return o
end

a = Account:new{balance = 0}   -- named arguments
a:deposit(100.00)

SpecialAccount = Account:new()
s = SpecialAccount:new{limit=1000}

function SpecialAccount:withdraw (v)
  if v - self.balance > self:getLimit() then
    error("insufficient funds")
  else
    self.balance = self.balance - v
  end
end

function SpecialAccount:getLimit ()
  return self.limit or 0
end

s:withdraw(1000)
print(s.balance)
--]]

-- [[ Multiple Inheritance, see https://www.lua.org/pil/16.3.html
local function search (k, plist)
  for i,superclass in ipairs(plist) do
    local v = superclass[k]   -- try ith superclass
    if v then return v end
  end
end

function createClass (constructor, ...)
  local c = {}   -- new class
  local arg = {...}

  -- class will search for each method in the list of its
  -- parents (`arg' is the list of parents)
  setmetatable(c, {__index = function (_, k)
    return search(k, arg)
  end})

  -- prepare c to be the metatable of its instances
  c.__index = c

  -- define a new constructor for this new class
  function c:new (o)
    o = constructor(o)
    setmetatable(o, c)
    return o
  end

  -- return new class
  return c
end

-- Privacy: https://www.lua.org/pil/16.4.html
function newNamed (arg)
  local self = {name = arg.name}
  local getName = function ()
    return self.name
  end
  local setName = function (name)
    self.name = name
  end
  return {   -- self.name is now "private"
    getName = getName,
    setName = setName
  }
end
Named = {}

NamedAccount = createClass(newNamed, Account, Named)
account = NamedAccount:new{name = "Paul"}
print(account.getName())   --> Paul
--]]
