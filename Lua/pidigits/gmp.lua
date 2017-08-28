
--
--  This file is part of the lgmp package for Lua 5.1.
--
--  Author: Wim Couwenberg
--  Date  : 2007/7/28
--
--  The lgmp package is distributed under the MIT license.  See the "COPYRIGHT"
--  file that came with the distribution of lgmp for license details.
--

setfenv(1,{
    require = require,
    type = type,
    getmetatable = getmetatable,
    assert = assert,
    error = error,
    match = string.match,
    print = print
})

local _M = {}

local randmeta = {}
local zmeta = {}
local fmeta = {}

local prv = {}
local aux = {
    randmeta = randmeta,
    zmeta = zmeta,
    fmeta = fmeta
}
-- fills prv with type creation functions, 
-- uses aux to get metadata for various types
require "_gmp" (prv, aux)

randmeta.__index = randmeta
zmeta.__index = zmeta
fmeta.__index = fmeta

local function isrand(obj)
	return type(obj) == "userdata" and getmetatable(obj) == randmeta
end

local function isz(obj)
	return type(obj) == "userdata" and getmetatable(obj) == zmeta
end

local function isf(obj)
	return type(obj) == "userdata" and getmetatable(obj) == fmeta
end

local function ntype(obj)
	local t = type(obj)
	if t == "number" then
		if obj%1 == 0 then
			if obj >= 0 and obj <= prv.ULONG_MAX then
				return "u"
			elseif obj >= prv.LONG_MIN and obj < 0 then
				return "s"
			elseif obj >= -prv.ULONG_MAX and obj < prv.LONG_MIN then
				return "n"
			end
		end
		return "d"
	elseif t == "userdata" then
		local m = getmetatable(obj)
		if m == zmeta then
			return "z"
		elseif m == fmeta then
			return "f"
		end
	end
	return "?"
end

local function checkrand(obj)
	assert(isrand(obj), "gmp random state expected")
end

local function checku(obj)
	assert(type(obj) == "number" and obj >= 0 and obj <= prv.ULONG_MAX and obj%1 == 0, "unsigned integer expected")
end

local function checkn(obj)
	assert(type(obj) == "number" and obj >= 0 and obj <= prv.LONG_MAX and obj%1 == 0, "non-negative intger expected")
end

local function checkbase(obj)
	assert(type(obj) == "number" and obj >= 2 and obj <= 62 and obj%1 == 0, "gmp number base must be an integer between 2 and 62")
end

local function checkz(obj)
	assert(isz(obj), "gmp integer expected")
end

local function checkzopt(obj)
	assert(obj == nil or isz(obj), "gmp integer expected")
end

local function checkf(obj)
	assert(isf(obj), "gmp floating point expected")
end

local function checkfopt(obj)
	assert(obj == nil or isf(obj), "gmp floating point expected")
end

local dtoz = prv.mpz_init_set_d
local dtof = prv.mpf_init_set_d

-- note: most C init functions pull up _G.*meta than call setmetatable.

function _M.z(value, base)
	if value == nil then
		return prv.mpz_init()
	elseif type(value) == "number" then
		return dtoz(value)
	elseif type(value) == "string" then
		if base == nil then 
			base = 0 
		elseif base ~= 0 then 
			checkbase(base) 
		end
		local res = prv.mpz_init_set_str(value, base)
		if res then return res end
		error("not a valid integer constant in base " .. base .. ": " .. value)
	elseif isz(value) then
		return prv.mpz_init_set(value)
	elseif isf(value) then
		local res = prv.mpz_init()
		prv.mpz_set_f(res, value)
		return res
	else
		error("cannot initialize gmp integer from " .. type(value))
	end
end

function _M.f(value, base)
	if value == nil then
		return prv.mpf_init()
	elseif type(value) == "number" then
		return dtof(value)
	elseif type(value) == "string" then
		if base == nil then
			base = 10 
		elseif type(base) == "number" and base < 0 then 
			checkbase(-base) 
		else 
			checkbase(base) 
		end
		local res = prv.mpf_init_set_str(value, base)
		if res then return res end
		error("not a valid floating point constant in base " .. base .. ": " .. value)
	elseif isf(value) then
		return prv.mpf_init_set(value)
	elseif isz(value) then
		local res = prv.mpf_init()
		prv.mpf_set_z(res, value)
		return res
	else
		error("cannot initialize gmp floating point from " .. type(value))
	end
end

function zmeta:__tostring()
	checkz(self)
	return prv.mpz_get_str(10, self)
end

function zmeta:__concat(other)
	if isz(self) then
		return zmeta.get_str(self) .. other
	else
		return self .. zmeta.get_str(other)
	end
end

function zmeta:__add(other)
	if isz(self) then
		return zmeta.add(self, other)
	else
		return zmeta.add(other, self)
	end
end

function zmeta:__sub(other)
	if isz(self) then
		return zmeta.sub(self, other)
	else
		return zmeta.rsub(other, self)
	end
end

function zmeta:__mul(other)
	if isz(self) then
		return zmeta.mul(self, other)
	else
		return zmeta.mul(other, self)
	end
end

function zmeta:__div(other)
	if isz(self) then
		return zmeta.fdiv_q(self, other)
	elseif type(other) == "number" then
		return zmeta.fdiv_q(dtoz(self), other)
	else
		error("unsupported type")
	end
end

function zmeta:__mod(other)
	if isz(self) then
		return zmeta.fdiv_r(self, other)
	elseif type(self) == "number" then
		return zmeta.fdiv_r(dtoz(self), other)
	else
		error("unsupported type")
	end
end

function zmeta:__pow(exp)
	checkz(self)
	checku(exp)
	return prv.mpz_pow_ui(self, exp)
end

function zmeta:__unm()
	checkz(self)
	return prv.mpz_neg(self)
end

function zmeta:__lt(other)
	checkz(self)
	checkz(other)
	return prv.mpz_cmp(self, other) < 0
end

function zmeta:__le(other)
	checkz(self)
	checkz(other)
	return prv.mpz_cmp(self, other) <= 0
end

function zmeta:__eq(other)
	checkz(self)
	checkz(other)
	return prv.mpz_cmp(self, other) == 0
end

function zmeta:__gc()
	checkz(self)
	return prv.mpz_clear(self)
end

function zmeta:abs(res)
	checkz(self)
	checkzopt(res)
	return prv.mpz_abs(self, res)
end

function zmeta:add(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_add(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_add_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpz_sub_ui(self, -a, res)
	elseif t == "z" then
		return prv.mpz_add(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:addmul(a1, a2)
	checkz(self)
	checkz(a1)
	local t = ntype(a2)
	if t == "d" then
		prv.mpz_addmul(self, a1, dtoz(a2))
	elseif t == "u" then
		prv.mpz_addmul_ui(self, a1, a2)
	elseif t == "s" or t == "n" then
		prv.mpz_submul_ui(self, a1, -a2)
	elseif t == "z" then
		prv.mpz_addmul(self, a1, a2)
	else
		error("unsupported type")
	end
end

function zmeta:AND(a, res)
	checkz(self)
	checkzopt(res)
	if type(a) == "number" then
		return prv.mpz_and(self, dtoz(a), res)
	elseif isz(a) then
		return prv.mpz_and(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:OR(a, res)
	checkz(self)
	checkzopt(res)
	if type(a) == "number" then
		return prv.mpz_or(self, dtoz(a), res)
	elseif isz(a) then
		return prv.mpz_or(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:XOR(a, res)
	checkz(self)
	checkzopt(res)
	if type(a) == "number" then
		return prv.mpz_xor(self, dtoz(a), res)
	elseif isz(a) then
		return prv.mpz_xor(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:bin(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)

	return prv.mpz_bin_ui(self, a, res)
end

function _M.bin(a1, a2, res)
	checku(a2)
	checkzopt(res)
	local t = ntype(a1)
	if t == "d" then
		return prv.mpz_bin_ui(dtoz(a1), a2, res)
	elseif t == "u" then
		return prv.mpz_bin_uiui(a1, a2, res)
	elseif t == "s" or t == "n" then
		if a2 <= prv.ULONG_MAX + a1 then
			res = prv.mpz_bin_uiui(a2 - a1 - 1, a2, res)
			if a2%2 ~= 0 then
				prv.mpz_neg(res, res)
			end
			return res
		else
			return prv.mpz_bin_ui(dtoz(a1), a2, res)
		end
	elseif t == "z" then
		return prv.mpz_bin_ui(a1, a2, res)
	else
		error("unsupported type")
	end
end

function zmeta:cdiv_q(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_cdiv_q(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_cdiv_q_ui(self, a, res)
	elseif t == "s" or t == "n" then
		local r2
		res, r2 = prv.mpz_fdiv_q_ui(self, -a, res)
		return prv.mpz_neg(res, res), r2
	elseif t == "z" then
		return prv.mpz_cdiv_q(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:cdiv_q_2exp(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_cdiv_q_2exp(self, a, res)
end

function zmeta:cdiv_qr(a, r1, r2)
	checkz(self)
	checkzopt(r1)
	checkzopt(r2)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_cdiv_qr(self, dtoz(a), r1, r2)
	elseif t == "u" then
		return prv.mpz_cdiv_qr_ui(self, a, r1, r2)
	elseif t == "s" or t == "n" then
		local r3
		r1, r2, r3 = prv.mpz_fdiv_qr_ui(self, -a, r1, r2)
		return prv.mpz_neg(r1, r1), r2, r3
	elseif t == "z" then
		return prv.mpz_cdiv_qr(self, a, r1, r2)
	else
		error("unsupported type")
	end
end

function zmeta:cdiv_r(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_cdiv_r(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_cdiv_r_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpz_fdiv_r_ui(self, -a, res)
	elseif t == "z" then
		return prv.mpz_cdiv_r(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:cdiv_r_2exp(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_cdiv_r_2exp(self, a, res)
end

function zmeta:cdiv(a)
	checkz(self)
	local t = ntype(a)
	if t == "u" then
		return prv.mpz_cdiv_ui(self, a)
	elseif t == "s" or t == "n" then
		return prv.mpz_cdiv_ui(self, -a)
	else
		error("unsupported type")
	end
end

function zmeta:clrbit(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_clrbit(self, a, res)
end

function zmeta:cmp(a)
	checkz(self)
	local t = ntype(a)
	if t == "d" or t == "n" then
		return prv.mpz_cmp_d(self, a)
	elseif t == "u" then
		return prv.mpz_cmp_ui(self, a)
	elseif t == "s" then
		return prv.mpz_cmp_si(self, a)
	elseif t == "z" then
		return prv.mpz_cmp(self, a)
	else
		error("unsupported type")
	end
end

function zmeta:cmpabs(a)
	checkz(self)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_cmpabs_d(self, a)
	elseif t == "u" then
		return prv.mpz_cmpabs_ui(self, a)
	elseif t == "s" or t == "n" then
		return prv.mpz_cmpabs_ui(self, -a)
	elseif t == "z" then
		return prv.mpz_cmpabs(self, a)
	else
		error("unsupported type")
	end
end

function zmeta:com(res)
	checkz(self)
	checkzopt(res)
	return prv.mpz_com(self, res)
end

function zmeta:combit(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_combit(self, a, res)
end

function zmeta:congruent(a1, a2)
	checkz(self)
	local t1, t2 = ntype(a1), ntype(a2)
	if t1 == "z" and t2 == "z" then
		return prv.mpz_congruent_p(self, a1, a2) ~= 0
	elseif t1 == "u" and t2 == "u" then
		return prv.mpz_congruent_ui_p(self, a1, a2) ~= 0
	else
		error("unsupported type")
	end
end

function zmeta:congruent_2exp(a1, a2)
	checkz(self)
	checkz(a1)
	checku(a2)
	return prv.mpz_congruent_2exp_p(self, a1, a2) ~= 0
end

function zmeta:divexact(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_divexact(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_divexact_ui(self, a, res)
	elseif t == "s" or t == "n" then
		res = prv.mpz_divexact_ui(self, -a, res)
		return prv.mpz_neg(res, res)
	elseif t == "z" then
		return prv.mpz_divexact(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:divisible(a)
	checkz(self)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_divisible_p(self, dtoz(a)) ~= 0
	elseif t == "u" then
		return prv.mpz_divisible_ui_p(self, a) ~= 0
	elseif t == "s" or t == "n" then
		return prv.mpz_divisible_ui_p(self, -a) ~= 0
	elseif t == "z" then
		return prv.mpz_divisible_p(self, a) ~= 0
	else
		error("unsupported type")
	end
end

function zmeta:divisible_2exp(a)
	checkz(self)
	checku(a)
	return prv.mpz_divisible_2exp_p(self, a) ~= 0
end

function zmeta:export()
    checkz(self)
    return prv.mpz_export(self)
end

function _M.importz(s)
    return prv.mpz_import(s)
end

function _M.fac(a, res)
	checku(a)
	checkzopt(res)
	return prv.mpz_fac_ui(a, res)
end

function zmeta:fdiv_q(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_fdiv_q(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_fdiv_q_ui(self, a, res)
	elseif t == "s" or t == "n" then
		local r2
		res, r2 = prv.mpz_cdiv_q_ui(self, -a, res)
		return prv.mpz_neg(res, res), r2
	elseif t == "z" then
		return prv.mpz_fdiv_q(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:fdiv_q_2exp(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_fdiv_q_2exp(self, a, res)
end

function zmeta:fdiv_qr(a, r1, r2)
	checkz(self)
	checkzopt(r1)
	checkzopt(r2)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_fdiv_qr(self, dtoz(a), r1, r2)
	elseif t == "u" then
		return prv.mpz_fdiv_qr_ui(self, a, r1, r2)
	elseif t == "s" or t == "n" then
		local r3
		r1, r2, r3 = prv.mpz_cdiv_qr_ui(self, -a, r1, r2)
		return prv.mpz_neg(r1, r1), r2, r3
	elseif t == "z" then
		return prv.mpz_fdiv_qr(self, a, r1, r2)
	else
		error("unsupported type")
	end
end

function zmeta:fdiv_r(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_fdiv_r(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_fdiv_r_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpz_cdiv_r_ui(self, -a, res)
	elseif t == "z" then
		return prv.mpz_fdiv_r(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:fdiv_r_2exp(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_fdiv_r_2exp(self, a, res)
end

function zmeta:fdiv(a)
	checkz(self)
	local t = ntype(a)
	if t == "u" then
		return prv.mpz_fdiv_ui(self, a)
	elseif t == "s" or t == "n" then
		return prv.mpz_fdiv_ui(self, -a)
	else
		error("unsupported type")
	end
end

function _M.fib(a, res)
	checku(a)
	checkzopt(res)
	return prv.mpz_fib_ui(a, res)
end

function _M.fib2(a, r1, r2)
	checku(a)
	checkzopt(r1)
	checkzopt(r2)
	return prv.mpz_fib2_ui(a, r1, r2)
end

function zmeta:fits_sint()
	checkz(self)
	return prv.mpz_fits_sint_p(self) ~= 0
end

function zmeta:fits_slong()
	checkz(self)
	return prv.mpz_fits_slong_p(self) ~= 0
end

function zmeta:fits_sshort()
	checkz(self)
	return prv.mpz_fits_sshort_p(self) ~= 0
end

function zmeta:fits_uint()
	checkz(self)
	return prv.mpz_fits_uint_p(self) ~= 0
end

function zmeta:fits_ulong()
	checkz(self)
	return prv.mpz_fits_ulong_p(self) ~= 0
end

function zmeta:fits_ushort()
	checkz(self)
	return prv.mpz_fits_ushort_p(self) ~= 0
end

function zmeta:gcd(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_gcd(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_gcd_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpz_gcd_ui(self, -a, res)
	elseif t == "z" then
		return prv.mpz_gcd(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:gcdext(a, r1, r2, r3)
	checkz(self)
	checkz(a)
	checkzopt(r1)
	checkzopt(r2)
	checkzopt(r3)
	return prv.mpz_gcdext(self, a, r1, r2, r3)
end

function zmeta:get_d()
	checkz(self)
	return prv.mpz_get_d(self)
end

function zmeta:get_d_2exp()
	checkz(self)
	return prv.mpz_get_d_2exp(self)
end

function zmeta:get_str(base)
	checkz(self)
	if base == nil then base = 10 else checkbase(base) end
	return prv.mpz_get_str(base, self)
end

function zmeta:hamdist(a)
	checkz(self)
	checkz(a)
	return prv.mpz_hamdist(self, a)
end

function zmeta:invert(a, res)
	checkz(self)
	checkz(a)
	checkzopt(res)
	local r2
	res, r2 = prv.mpz_invert(self, a, res)
	if r2 ~= 0 then
		return res
	end
end

function zmeta:ior(a, res)
	checkz(self)
	checkzopt(res)
	if type(a) == "number" then
		return prv.mpz_ior(self, dtoz(a), res)
	elseif isz(a) then
		return prv.mpz_ior(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:kronecker(a)
	checkz(self)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_kronecker(self, dtoz(a))
	elseif t == "u" then
		return prv.mpz_kronecker_ui(self, a)
	elseif t == "s" then
		return prv.mpz_kronecker_si(self, a)
	elseif t == "n" then
		return prv.mpz_kronecker(self, dtoz(a))
	elseif t == "z" then
		return prv.mpz_kronecker(self, a)
	else
		error("unsupported type")
	end
end

function zmeta:rkronecker(a)
	checkz(self)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_kronecker(dtoz(a), self)
	elseif t == "u" then
		return prv.mpz_ui_kronecker(a, self)
	elseif t == "s" then
		return prv.mpz_si_kronecker(a, self)
	elseif t == "n" then
		return prv.mpz_kronecker(dtoz(a), self)
	elseif t == "z" then
		return prv.mpz_kronecker(a, self)
	else
		error("unsupported type")
	end
end

zmeta.jacobi = zmeta.kronecker
zmeta.rjacobi = zmeta.rkronecker

zmeta.legendre = zmeta.kronecker
zmeta.rlegendre = zmeta.rkronecker

function zmeta:lcm(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_lcm(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_lcm_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpz_lcm_ui(self, -a, res)
	elseif t == "z" then
		return prv.mpz_lcm(self, a, res)
	else
		error("unsupported type")
	end
end

function _M.lucnum(a, res)
	checku(a)
	checkzopt(res)
	return prv.mpz_lucnum_ui(a, res)
end

function _M.lucnum2(a, r1, r2)
	checku(a)
	checkzopt(r1)
	checkzopt(r2)
	return prv.mpz_lucnum2_ui(a, r1, r2)
end

function zmeta:mod(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_mod(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_fdiv_r_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpz_cdiv_r_ui(self, -a, res)
	elseif t == "z" then
		return prv.mpz_mod(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:mul(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_mul(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_mul_ui(self, a, res)
	elseif t == "s" then
		return prv.mpz_mul_si(self, a, res)
	elseif t == "n" then
		res = prv.mpz_mul_ui(self, -a, res)
		return prv.mpz_neg(res, res)
	elseif t == "z" then
		return prv.mpz_mul(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:mul_2exp(a, res)
	checkz(self)
	checku(a)
	checkzopt(self)
	return prv.mpz_mul_2exp(self, a, res)
end

function zmeta:neg(res)
	checkz(self)
	checkzopt(res)
	return prv.mpz_neg(self, res)
end

function zmeta:nextprime(res)
	checkz(self)
	checkzopt(res)
	return prv.mpz_nextprime(self, res)
end

function zmeta:perfect_power()
	checkz(self)
	return prv.mpz_perfect_power_p(self) ~= 0
end

function zmeta:perfect_square()
	checkz(self)
	return prv.mpz_perfect_square_p(self) ~= 0
end

function zmeta:popcount()
	checkz(self)
	return prv.mpz_popcount(self)
end

function zmeta:pow(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_pow_ui(self, a, res)
end

function zmeta:powm(a1, a2, res)
	checkz(self)
	checkz(a2)
	checkzopt(res)
	local t = ntype(a1)
	if t == "d" then
		return prv.mpz_powm(self, dtoz(a1), a2, res)
	elseif t == "u" then
		return prv.mpz_powm_ui(self, a1, a2, res)
	elseif t == "s" or t == "n" then
		return prv.mpz_powm(self, dtoz(a1), a2, res)
	elseif t == "z" then
		return prv.mpz_powm(self, a1, a2, res)
	else
		error("unsupported type")
	end
end

function zmeta:probab_prime(a)
	checkz(self)
	if a == nil then a = 10 else ckechn(a) end
	local res = prv.mpz_probab_prime_p(self, a)
	return res ~= 0 and res
end

function zmeta:remove(a, res)
	checkz(self)
	checkzopt(res)
	if type(a) == "number" then
		return prv.mpz_remove(self, dtoz(a), res)
	elseif isz(a) then
		return prv.mpz_rempve(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:root(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	local r2
	res, r2 = prv.mpz_root(self, a, res)
	return res, r2 ~= 0
end

function zmeta:rootrem(a, r1, r2)
	checkz(self)
	checku(a)
	checkzopt(r1)
	checkzopt(r2)
	return prv.mpz_rootrem(self, a, r1, r2)
end

function zmeta:scan0(a)
	checkz(self)
	if a == nil then a = 0 else checku(a) end
	return prv.mpz_scan0(self, a)
end

function zmeta:scan1(a)
	checkz(self)
	if a == nil then a = 0 else checku(a) end
	return prv.mpz_scan1(self, a)
end

function zmeta:set(a, base)
	checkz(self)
	if type(a) == "number" then
		prv.mpz_set_d(self, a)
	elseif type(a) == "string" then
		if base == nil then 
			base = 0 
		elseif base ~= 0 then 
			checkbase(base) 
		end
		if prv.mpz_set_str(self, a, base) ~= 0 then
			error("not a valid integer constant in base " .. base .. ": " .. a)
		end
	elseif isz(a) then
		prv.mpz_set(self, a)
	elseif isf(a) then
		prv.mpz_set_f(self, a)
	else
		error("unsupported type")
	end
end

function zmeta:setbit(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_setbit(self, a, res)
end

function zmeta:sgn()
	checkz(self)
	return prv.mpz_sgn(self)
end

function zmeta:sizeinbase(a)
	checkz(self)
	if a == nil then a = 10 else checkbase(a) end
	return prv.mpz_sizeinbase(self, a)
end

function zmeta:sqrt(res)
	checkz(self)
	assert(prv.mpz_sgn(self) >= 0, "taking square of negative number")
	checkzopt(res)
	return prv.mpz_sqrt(self, res)
end

function zmeta:sqrtrem(r1, r2)
	checkz(self)
	assert(prv.mpz_sgn(self) >= 0, "taking square of negative number")
	checkzopt(r1)
	checkzopt(r2)
	return prv.mpz_sqrtrem(self, r1, r2)
end

function zmeta:sub(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_sub(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_sub_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpz_add_ui(self, -a, res)
	elseif t == "z" then
		return prv.mpz_sub(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:rsub(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a1)
	if t == "d" then
		return prv.mpz_sub(dtoz(a), self, res)
	elseif t == "u" then
		return prv.mpz_ui_sub(a, self, res)
	elseif t == "s" or t == "n" then
		res = prv.mpz_add_ui(self, -a, res)
		return prv.mpz_neg(res, res)
	elseif t == "z" then
		return prv.mpz_sub(a, self, res)
	else
		error("unsupported type")
	end
end

function zmeta:submul(a1, a2)
	checkz(self)
	checkz(a1)
	local t = ntype(a2)
	if t == "d" then
		prv.mpz_submul(self, a1, dtoz(a2))
	elseif t == "u" then
		prv.mpz_submul_ui(self, a1, a2)
	elseif t == "s" or t == "n" then
		prv.mpz_addmul_ui(self, a1, -a2)
	elseif t == "z" then
		prv.mpz_submul(self, a1, a2)
	else
		error("unsupported type")
	end
end

function zmeta:tdiv_q(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_tdiv_q(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_tdiv_q_ui(self, a, res)
	elseif t == "s" or t == "n" then
		local r2
		res, r2 = prv.mpz_tdiv_q_ui(self, -a, res)
		return prv.mpz_neg(res, res), r2
	elseif t == "z" then
		return prv.mpz_tdiv_q(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:tdiv_q_2exp(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_tdiv_q_2exp(self, a, res)
end

function zmeta:tdiv_qr(a, r1, r2)
	checkz(self)
	checkzopt(r1)
	checkzopt(r2)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_tdiv_qr(self, dtoz(a), r1, r2)
	elseif t == "u" then
		return prv.mpz_tdiv_qr_ui(self, a, r1, r2)
	elseif t == "s" or t == "n" then
		local r3
		r1, r2, r3 = prv.mpz_tdiv_qr_ui(self, -a, r1, r2)
		return prv.mpz_neg(r1, r1), r2, r3
	elseif t == "z" then
		return prv.mpz_tdiv_qr(self, a, r1, r2)
	else
		error("unsupported type")
	end
end

function zmeta:tdiv_r(a, res)
	checkz(self)
	checkzopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpz_tdiv_r(self, dtoz(a), res)
	elseif t == "u" then
		return prv.mpz_tdiv_r_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpz_tdiv_r_ui(self, -a, res)
	elseif t == "z" then
		return prv.mpz_tdiv_r(self, a, res)
	else
		error("unsupported type")
	end
end

function zmeta:tdiv_r_2exp(a, res)
	checkz(self)
	checku(a)
	checkzopt(res)
	return prv.tdiv_r_2exp(self, a, res)
end

function zmeta:tdiv(a)
	checkz(self)
	local t = ntype(a)
	if t == "u" then
		return prv.mpz_tdiv_ui(self, a)
	elseif t == "s" or t == "n" then
		return prv.mpz_tdiv_ui(self, -a)
	else
		error("unsupported type")
	end
end

function zmeta:tstbit(a)
	checkz(self)
	checku(a)
	return prv.mpz_tstbit(self, a)
end

function zmeta:lshift(a)
    checkz(self)
    prv.mpz_lshift(self,a)
    return self
end

function zmeta:rshift(a)
    checkz(self)
    prv.mpz_rshift(self,a)
    return self
end

function _M.pow(a1, a2, res)
	checku(a2)
	checkzopt(res)
	local t = ntype(a1)
	if t == "d" then
		return prv.mpz_pow_ui(dtoz(a1), a2, res)
	elseif t == "u" then
		return prv.mpz_ui_pow_ui(a1, a2, res)
	elseif t == "s" or t == "n" then
		res = prv.mpz_ui_pow_ui(-a1, a2, res)
		if a2%2 ~= 0 then
			prv.mpz_neg(res, res)
		end
		return res
	elseif t == "z" then
		return prv.mpz_pow_ui(a1, a2, res)
	else
		error("unsupported type")
	end
	return prv.mpz_ui_pow_ui(a1, a2, res)
end

function zmeta:xor(a, res)
    return self:XOR(a,res)
end

function fmeta:abs(res)
	checkf(self)
	checkfopt(res)
	return prv.mpf_abs(self, res)
end

function fmeta:add(a, res)
	checkf(self)
	checkfopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpf_add(self, dtof(a), res)
	elseif t == "u" then
		return prv.mpf_add_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpf_sub_ui(self, -a, res)
	elseif t == "f" then
		return prv.mpf_add(self, a, res)
	else
		error("unsupported type")
	end
end

function fmeta:ceil(res)
	checkf(self)
	checkfopt(res)
	return prv.mpf_ceil(self, res)
end

function fmeta:cmp(a)
	checkf(self)
	local t = ntype(a)
	if t == "d" then
		return prv.mpf_cmp_d(self, a)
	elseif t == "u" then
		return prv.mpf_cmp_ui(self, a)
	elseif t == "s" then
		return prv.mpf_cmp_si(self, a)
	elseif t == "n" then
		return prv.mpf_cmp(self, dtof(a))
	elseif t == "f" then
		return prv.mpf_cmp(self, a)
	else
		error("unsupported type")
	end
end

function fmeta:div(a, res)
	checkf(self)
	checkfopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpf_div(self, dtof(a), res)
	elseif t == "u" then
		return prv.mpf_div_ui(self, a, res)
	elseif t == "s" or t == "n" then
		res = prv.mpf_div_ui(self, -a, res)
		return prv.mpf_neg(res, res);
	elseif t == "f" then
		return prv.mpf_div(self, a, res)
	else
		error("unsupported type")
	end
end

function fmeta:eq(a1, a2)
	checkf(self)
	checkf(a1)
	checku(a2)
	return prv.mpf_eq(self, a1, a2) ~= 0
end

function fmeta:fits_sint()
	checkf(self)
	return prv.mpf_fits_sint_p(self) ~= 0
end

function fmeta:fits_sshort()
	checkf(self)
	return prv.mpf_fits_sshort_p(self) ~= 0
end

function fmeta:fits_uint()
	checkf(self)
	return prv.mpf_fits_uint_p(self) ~= 0
end

function fmeta:fits_ulong()
	checkf(self)
	return prv.mpf_fits_ulong_p(self) ~= 0
end

function fmeta:fits_ushort()
	checkf(self)
	return prv.mpf_fits_ushort_p(self) ~= 0
end

function fmeta:floor(res)
	checkf(self)
	checkfopt(res)
	return prv.mpf_floor(self, res)
end

function fmeta:get_d()
	checkf(self)
	return prv.mpf_get_d(self)
end

function fmeta:get_d_2exp()
	checkf(self)
	return prv.mpf_get_d_2exp(self)
end

_M.get_default_prec = prv.mpf_get_default_prec

function fmeta:get_prec()
	checkf(self)
	return prv.mpf_get_prec(self)
end

function fmeta:get_str(base, size)
	checkf(self)
	if base == nil then base = 10 else checkbase(base) end
	if size == nil then size = 0 else checkn(size) end
	return prv.mpf_get_str(base, size, self)
end

function fmeta:integer()
	checkf(self)
	return prv.mpf_integer_p(self) ~= 0
end

function fmeta:mul(a, res)
	checkf(self)
	checkfopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpf_mul(self, dtof(a), res)
	elseif t == "u" then
		return prv.mpf_mul_ui(self, a, res)
	elseif t == "s" or t == "n" then
		res = prv.mpf_mul_ui(self, -a, res)
		return prv.mpf_neg(res, res)
	elseif t == "f" then
		return prv.mpf_mul(self, a, res)
	else
		error("unsupported type")
	end
end

function fmeta:mul_2exp(a, res)
	checkf(self)
	checku(a)
	checkfopt(res)
	return prv.mpf_mul_2exp(self, a, res)
end

function fmeta:neg(res)
	checkf(self)
	checkfopt(res)
	return prv.mpf_neg(self, res)
end

function fmeta:pow(a, res)
	checkf(self)
	checku(a)
	checkfopt(res)
	return prv.mpf_pow_ui(self, a, res)
end

function fmeta:reldiff(a, res)
	checkf(self)
	checkf(a)
	checkfopt(res)
	return prv.mpf_reldiff(self, a, res)
end

function _M.set_default_prec(a)
	checku(a)
	prv.mpf_set_default_prec(a)
end

function fmeta:set_prec(a)
	checkf(self)
	checku(a)
	prv.mpf_set_prec(self, a)
end

function fmeta:set(a, base)
	checkf(self)
	if type(a) == "number" then
		prv.mpf_set_d(self, a)
	elseif type(a) == "string" then
		if base == nil then 
			base = 10 
		elseif type(base) == "number" and base < 0 then
			checkbase(-base)
		else
			checkbase(base)
		end
		if prv.mpf_set_str(self, a, base) ~= 0 then
			error("not a valid floating point constant in base " .. base .. ": " .. a)
		end
	elseif isf(a) then
		prv.mpf_set(self, a)
	elseif isz(a) then
		prv.mpf_set_z(self, a)
	else
		error("unsupported type")
	end
end

function fmeta:sgn()
	checkf(self)
	return prv.mpf_sgn(self)
end

function fmeta:sqrt(res)
	checkf(self)
	checkfopt(res)
	return prv.mpf_sqrt(self, res)
end

function _M.sqrt(a, res)
	checku(a)
	checkfopt(res)
	return prv.mpf_sqrt_ui(a, res)
end

function fmeta:sub(a, res)
	checkf(self)
	checkfopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpf_sub(self, dtof(a), res)
	elseif t == "u" then
		return prv.mpf_sub_ui(self, a, res)
	elseif t == "s" or t == "n" then
		return prv.mpf_add_ui(self, -a, res)
	elseif t == "f" then
		return prv.mpf_sub(self, a, res)
	else
		error("unsupported type")
	end
end

function fmeta:trunc(res)
	checkf(self)
	checkfopt(res)
	return prv.mpf_trunc(self, res)
end

function fmeta:rdiv(a, res)
	checkf(self)
	checkfopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpf_div(dtof(a), self, res)
	elseif t == "u" then
		return prv.mpf_ui_div(a, self, res)
	elseif t == "s" or t == "n" then
		res = prv.mpf_ui_div(-a, self, res)
		return prv.mpf_neg(res, res)
	elseif t == "f" then
		return prv.mpf_div(a, self, res)
	else
		error("unsupported type")
	end
end

function fmeta:rsub(a, res)
	checkf(self)
	checkfopt(res)
	local t = ntype(a)
	if t == "d" then
		return prv.mpf_sub(dtof(a), self, res)
	elseif t == "u" then
		return prv.mpf_ui_sub(a, self, res)
	elseif t == "s" or t == "n" then
		res = prv.mpf_add_ui(self, -a, res)
		return prv.mpf_neg(res, res)
	elseif t == "f" then
		return prv.mpf_sub(a, self, res)
	else
		error("unsupported type")
	end
end

function fmeta:__add(a)
	if isf(self) then
		return fmeta.add(self, a)
	else
		return fmeta.add(a, self)
	end
end

function fmeta:__sub(a)
	if isf(self) then
		return fmeta.sub(self, a)
	else
		return fmeta.rsub(a, self)
	end
end

function fmeta:__mul(a)
	if isf(self) then
		return fmeta.mul(self, a)
	else
		return fmeta.mul(a, self)
	end
end

function fmeta:__div(a)
	if isf(self) then
		return fmeta.div(self, a)
	else
		return fmeta.rdiv(a, self)
	end
end

function fmeta:__unm()
	checkf(self)
	return prv.mpf_neg(self)
end

function fmeta:__pow(a)
	checkf(self)
	checku(a)
	return prv.mpf_pow_ui(self, a)
end

function fmeta:__lt(a)
	checkf(self)
	checkf(a)
	return prv.mpf_cmp(self, a) < 0
end

function fmeta:__le(a)
	checkf(self)
	checkf(a)
	return prv.mpf_cmp(self, a) <= 0
end

function fmeta:__eq(a)
	checkf(self)
	checkf(a)
	return prv.mpf_cmp(self, a) == 0
end

function fmeta:__tostring()
	return fmeta.format(self, ".g")
end

function fmeta:__concat(other)
	if isf(self) then
		return fmeta.__tostring(self) .. other
	else
		return self .. fmeta.__tostring(other)
	end
end

function fmeta:__gc()
	checkf(self)
	return prv.mpf_clear(self)
end

function _M.rand(a)
	if a == nil then
		return prv.gmp_randinit_default()
	else
		checkrand(a)
		return prv.gmp_randinit_set(a)
	end
end

function randmeta:seed(a)
	checkrand(self)
	local t = ntype(a)
	if a == "u" then
		prv.gmp_randseed_ui(self, a)
	elseif a == "z" then
		prv.gmp_randseed(self, a)
	else
		error("unsupported type")
	end
end

function randmeta:zbits(a, res)
	checkrand(self)
	checku(a)
	checkzopt(res)
	return prv.mpz_urandomb(self, a, res)
end

function randmeta:z(a, res)
	checkrand(self)
	checkz(a)
	checkzopt(res)
	return prv.mpz_urandomm(self, a, res)
end

function randmeta:fbits(a, res)
	checkrand(self)
	checku(a)
	checkfopt(res)
	return prv.mpf_urandomb(self, a, res)
end

function randmeta:__gc()
	checkrand(self)
	prv.gmp_randclear(self)
end

function randmeta:__tostring()
	checkrand(self)
	return "gmp random state"
end

function zmeta:format(fmt, p)
	checkz(self)
	assert(type(fmt) == "string", "gmp integer format string expected")
	local fw, prec, conv = 
	      match(fmt, "^%%?([0#+ ]?%d*)(%.?%*?%d*)Z?([dioxX])$")

	if not conv or prec ~= "" and prec ~= ".*" and not match(prec, "^%.%d*$") then
		error("invalid format string for gmp integer: " .. fmt)
	end

	if prec == ".*" then
		checkn(p)
	else
		assert(p == nil, "precision incorrectly specified")
	end

	return prv.mpz_asprintf("%" .. fw .. prec .. "Z" .. conv, self, p)
end

function fmeta:format(fmt, p)
	checkf(self)
	assert(type(fmt) == "string", "gmp floating point format string expected")
	local fw, prec, conv = 
	      match(fmt, "^%%?([0#+ ]?%d*)(%.?%*?%d*)F?([aAeEfgG])$")

	if not conv or prec ~= "" and prec ~= ".*" and not match(prec, "^%.%d*$") then
		error("invalid format string for gmp floating point: " .. fmt)
	end

	if prec == ".*" then
		checkn(p)
	else
		assert(p == nil, "precision incorrectly specified")
	end

	return prv.mpf_asprintf("%" .. fw .. prec .. "F" .. conv, self, p)
end

_M.version = prv.version

_M.zero = prv.mpz_init()

return _M
