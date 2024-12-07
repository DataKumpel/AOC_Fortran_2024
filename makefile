COMPILER = gfortran
FLAGS    = -g -std=f2018 -Wall
DAY_01   = ./build/day_01.x
DAY_02   = ./build/day_02.x
DAY_03   = ./build/day_03.x
DAY_04   = ./build/day_04.x
DAY_05   = ./build/day_05.x


all:
	day_01 day_02 day_03 day_04 day_05


day_01:
	$(COMPILER) day_01.f90 -o $(DAY_01) $(FLAGS)
	$(DAY_01)

day_02:
	$(COMPILER) day_02.f90 -o $(DAY_02) $(FLAGS)
	$(DAY_02)

day_03:
	$(COMPILER) day_03.f90 -o $(DAY_03) $(FLAGS)
	$(DAY_03)

day_04:
	$(COMPILER) day_04.f90 -o $(DAY_04) $(FLAGS)
	$(DAY_04)

day_05:
	$(COMPILER) day_05.f90 -o $(DAY_05) $(FLAGS)
	$(DAY_05)
