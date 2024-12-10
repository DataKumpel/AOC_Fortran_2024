COMPILER = gfortran
FLAGS    = -g -std=f2018 -Wall
DAY_01   = ./build/day_01.x
DAY_02   = ./build/day_02.x
DAY_03   = ./build/day_03.x
DAY_04   = ./build/day_04.x
DAY_05   = ./build/day_05.x
DAY_06   = ./build/day_06.x
DAY_07   = ./build/day_07.x


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

day_06:
	$(COMPILER) day_06.f90 -o $(DAY_06) $(FLAGS)
	$(DAY_06)

day_07:
	$(COMPILER) day_07.f90 -o $(DAY_07) $(FLAGS)
	$(DAY_07)
