
-- see python solution

h = 100
v = 20
g = 9.81

--
-- y = h + v sin(a) t - g/2 t^2
-- t = x/(vcos(a))
-- y(t) = h + v sin(a) x/cos(a) - g/2 x^2/(v^2 cos(a)^2)
-- GOAL: max{y(t) : y(t) > 0}

-- let z = tan(a)
-- y = h + vx z + g/2 x^2/v^2 (z^2+1)
--   = (h-g/2 x^2/v^2) + x z - g/2 x^2/v^2 z^2

-- largest possible y achieved when
--     z0 = v^2/(gx)
-- largest possible x achieved when delta = 0
--     x0 = sqrt((1 + 2gh/v^2) v^4/g^2)

-- integrate(y(z) (2pi x) dx, 0, x0)
