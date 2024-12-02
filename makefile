COMPILER = gfortran
FLAGS    = -g -std=f2018 -Wall
DAY_01   = ./build/day_01.x
DAY_02   = ./build/day_02.x


all:
	$(COMPILER) day_01.f90 -o $(DAY_01) $(FLAGS)


day_01:
	$(COMPILER) day_01.f90 -o $(DAY_01) $(FLAGS)
	$(DAY_01)

day_02:
	$(COMPILER) day_02.f90 -o $(DAY_02) $(FLAGS)
	$(DAY_02)

