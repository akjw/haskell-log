--1) no. i cannot have a type of a, which is polymorphic; type signature must have concrete type or type class

--2) no. 1.0 is Fractional

--3) yes. Fractional has an instance of Float

--4) yes. RealFrac has an instance of Float

--5) yes. Although there isn't an Ord method being implemented here so there's no difference anyway

--6) yes. see 5)

--7) no. sigmund must return Int, but a is fully polymorphic

--8) no. see 7)

--9) yes. Int has an instance of Ord

--10) yes. sort takes type class constrained variable that must have instance of Ord

--11) no. mySort expects argument of type [Char], not Ord
