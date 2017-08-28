-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
-- contributed by Mike Pall
-- requires LGMP "A GMP package for Lua 5.1"
-- with matrix optimization, courtesy of Wim Couwenberg

local g, aux = {}, {}
require"c-gmp"(g, aux)
local add, mul, div = g.mpz_add, g.mpz_mul_si, g.mpz_tdiv_q
local init, get = g.mpz_init_set_d, g.mpz_get_d

local u, v, w

local function produce(n1, n2, d, k)
  mul(n1, 2*k-1, u)
  add(n2, n2, v)
  mul(n1, k-1, w)
  add(u, v, n1)
  mul(n2, k+2, u)
  add(w, u, n2)
  mul(d, 2*k+1, d)
end

local function extract(n1, n2, d, y)
  mul(d, -10*y, u)
  mul(n1, 10, n1)
  add(n1, u, n1)
  mul(n2, 10, n2)
  add(n2, u, n2)
end

local function digit(n1, n2, d)
  local y = get(div(n1, d, u))
  if y == get(div(n2, d, v)) then return y end
end

-- Generate successive digits of PI.
local function pidigits(N)
  local write = io.write
  local k = 1
  local n1, n2, d = init(4), init(3), init(1)
  u, v, w = init(0), init(0), init(0)
  local i = 0
  while i < N do
    local y = digit(n1, n2, d)
    if y then
      write(y)
      i = i + 1; if i % 10 == 0 then write("\t:", i, "\n") end
      extract(n1, n2, d, y)
    else
      produce(n1, n2, d, k)
      k = k + 1
    end
  end
  if i % 10 ~= 0 then write(string.rep(" ", 10 - N % 10), "\t:", N, "\n") end
end

local N = tonumber(arg and arg[1]) or 27
pidigits(N)
