COMPILER = gfortran
FLAGS    = -g -std=f2018 -Wall
DAY_01   = ./build/day_01.x


all:
	$(COMPILER) day_01.f90 -o $(DAY_01) $(FLAGS)


day_01:
	$(COMPILER) day_01.f90 -o $(DAY_01) $(FLAGS)
	$(DAY_01)
