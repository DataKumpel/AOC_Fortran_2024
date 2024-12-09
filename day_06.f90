PROGRAM main

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), PARAMETER  :: ex_file = "inputs/day_06_example.txt"
    CHARACTER(LEN=*), PARAMETER  :: in_file = "inputs/day_06_inputs.txt"
    CHARACTER(LEN=32)            :: file
    LOGICAL                      :: test
    
    CHARACTER, DIMENSION(:, :), ALLOCATABLE :: map
    INTEGER                                 :: start_x, start_y

    INTERFACE
        SUBROUTINE init_map(filename, map, start_x, start_y)
            CHARACTER(LEN=*), INTENT(IN)                           :: filename
            CHARACTER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: map
            INTEGER, INTENT(OUT)                                   :: start_x, start_y
        END SUBROUTINE init_map

        SUBROUTINE print_map(map)
            CHARACTER, DIMENSION(:, :), INTENT(IN) :: map
        END SUBROUTINE print_map

        SUBROUTINE simulate_movement(map, start_x, start_y)
            CHARACTER, DIMENSION(:, :), INTENT(INOUT) :: map
            INTEGER, INTENT(IN)                       :: start_x, start_y
        END SUBROUTINE simulate_movement

        SUBROUTINE count_visited_tiles(map)
            CHARACTER, DIMENSION(:, :), INTENT(IN) :: map
        END SUBROUTINE count_visited_tiles

        SUBROUTINE check_loop(map, start_x, start_y)
            CHARACTER, DIMENSION(:, :), INTENT(INOUT) :: map
            INTEGER, INTENT(IN)                       :: start_x, start_y
        END SUBROUTINE check_loop
    END INTERFACE
    !=============================================================================================!
    
    test = .FALSE.

    IF(test)       file = ex_file
    IF(.NOT. test) file = in_file

    CALL init_map(TRIM(file), map, start_x, start_y)
    WRITE(*, *) "START:", start_x, start_y
    !CALL print_map(map)
    
    WRITE(*, *) "-----------------------------------"

    CALL simulate_movement(map, start_x, start_y)
    !CALL print_map(map)
    CALL count_visited_tiles(map)

    CALL check_loop(map, start_x, start_y)

    IF(ALLOCATED(map)) DEALLOCATE(map)

END PROGRAM main


SUBROUTINE init_map(filename, map, start_x, start_y)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN)                           :: filename
    CHARACTER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: map
    INTEGER, INTENT(OUT)                                   :: start_x, start_y

    INTEGER :: f_unit
    INTEGER :: io_stat
    INTEGER :: size_x, size_y
    INTEGER :: x, y
    CHARACTER :: chr
    CHARACTER(LEN=256) :: line
    !=============================================================================================!
    
    OPEN(file=filename, action="read", status="old", newunit=f_unit)
    
    size_x = 0
    DO
        READ(f_unit, "(A)", advance="no", iostat=io_stat) chr
        IF(io_stat /= 0) EXIT
        size_x = size_x + 1
    ENDDO

    REWIND(f_unit)
    
    size_y = 0
    DO
        READ(f_unit, "(A)", iostat=io_stat) chr
        IF(io_stat /= 0) EXIT
        size_y = size_y + 1
    ENDDO

    ALLOCATE(map(size_y, size_x))

    REWIND(f_unit)
    
    y = 1
    DO
        READ(f_unit, "(A)", iostat=io_stat) line
        IF(io_stat /= 0) EXIT

        DO x = 1, LEN_TRIM(line)
            map(y, x) = line(x:x)

            IF(map(y, x) == "^") THEN
                start_x = x
                start_y = y
            ENDIF
        ENDDO
        
        y = y + 1
    ENDDO

    CLOSE(f_unit)

END SUBROUTINE init_map


SUBROUTINE simulate_movement(map, start_x, start_y)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER, DIMENSION(:, :), INTENT(INOUT) :: map
    INTEGER, INTENT(IN)                       :: start_x, start_y
    
    INTEGER :: size_x, size_y
    INTEGER :: pos_x, pos_y
    INTEGER :: next_pos_x, next_pos_y
    INTEGER :: dir
    INTEGER, DIMENSION(4, 2) :: directions
    !=============================================================================================!
    
    size_y = SIZE(map, 1)
    size_x = SIZE(map, 2)

    pos_y = start_y
    pos_x = start_x
    
    directions(1, :) = [-1,  0] ! UP
    directions(2, :) = [ 0,  1] ! RIGHT
    directions(3, :) = [ 1,  0] ! DOWN
    directions(4, :) = [ 0, -1] ! LEFT
    
    dir = 1

    DO
        IF(pos_y > size_y .OR. pos_y < 1) EXIT
        IF(pos_x > size_x .OR. pos_x < 1) EXIT

        map(pos_y, pos_x) = "X"
        
        next_pos_y = pos_y + directions(dir, 1)
        next_pos_x = pos_x + directions(dir, 2)

        IF(      next_pos_y <= size_y .AND. next_pos_y >= 1 &
          &.AND. next_pos_x <= size_x .AND. next_pos_x >= 1) THEN
            IF(map(next_pos_y, next_pos_x) == "#") THEN
                dir = dir + 1
                IF(dir == 5) dir = 1
                CYCLE
            ENDIF
        ENDIF

        pos_x = next_pos_x
        pos_y = next_pos_y
    ENDDO

END SUBROUTINE simulate_movement


SUBROUTINE print_map(map)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER, DIMENSION(:, :), INTENT(IN) :: map

    INTEGER :: i
    !=============================================================================================!

    DO i = 1, SIZE(map, 1)
        WRITE(*, *) map(i, :)
    ENDDO

END SUBROUTINE print_map


SUBROUTINE count_visited_tiles(map)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER, DIMENSION(:, :), INTENT(IN) :: map

    INTEGER :: num_visited_tiles
    INTEGER :: i, j
    !=============================================================================================!
    
    num_visited_tiles = 0

    DO i = 1, SIZE(map, 1)
        DO j = 1, SIZE(map, 2)
            IF(map(i, j) == "X") num_visited_tiles = num_visited_tiles + 1
        ENDDO
    ENDDO

    WRITE(*, *) "NUMBER OF VISITED TILES:", num_visited_tiles

END SUBROUTINE count_visited_tiles


SUBROUTINE clear_map(map)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER, DIMENSION(:, :), INTENT(INOUT) :: map

    INTEGER :: i, j
    !=============================================================================================!
    
    DO i = 1, SIZE(map, 1)
        DO j = 1, SIZE(map, 2)
            IF(map(i, j) /= "#") map(i, j) = "."
        ENDDO
    ENDDO

END SUBROUTINE clear_map


SUBROUTINE check_loop(map, start_x, start_y)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER, DIMENSION(:, :), INTENT(INOUT) :: map
    INTEGER, INTENT(IN)                       :: start_x, start_y

    INTEGER :: i, j, k
    INTEGER :: loop_count
    INTEGER :: pos_x, pos_y
    INTEGER :: next_x, next_y
    INTEGER :: dir
    INTEGER :: num_obstacles
    INTEGER, DIMENSION(4, 2) :: directions
    INTEGER, DIMENSION(:, :), ALLOCATABLE :: known_obstacles
    LOGICAL                  :: is_loop

    INTERFACE
        SUBROUTINE clear_map(map)
            CHARACTER, DIMENSION(:, :), INTENT(INOUT) :: map
        END SUBROUTINE clear_map

        SUBROUTINE print_map(map)
            CHARACTER, DIMENSION(:, :), INTENT(IN) :: map
        END SUBROUTINE print_map
    END INTERFACE
    !=============================================================================================!
    
    loop_count = 0

    directions(1, :) = [-1,  0] ! UP
    directions(2, :) = [ 0,  1] ! RIGHT
    directions(3, :) = [ 1,  0] ! DOWN
    directions(4, :) = [ 0, -1] ! LEFT

    ALLOCATE(known_obstacles(SIZE(map, 1) * SIZE(map, 2), 4))

    DO i = 1, SIZE(map, 1)
        DO j = 1, SIZE(map, 2)
            IF(map(i, j) == "#") CYCLE
            IF(i == start_y .AND. j == start_x) CYCLE
            
            map(i, j) = "O"
            
            pos_x = start_x
            pos_y = start_y
            dir   = 1

            num_obstacles = 0

            is_loop = .FALSE.

            MOVEMENT: DO
                IF(      pos_y > SIZE(map, 1) .OR. pos_y < 1 &
                   &.OR. pos_x > SIZE(map, 2) .OR. pos_x < 1) THEN 
                    is_loop = .FALSE.
                    EXIT MOVEMENT
                ENDIF
                
                IF(map(pos_y, pos_x) /= "+") THEN
                    SELECT CASE(dir)
                        CASE(1)
                            map(pos_y, pos_x) = "|"
                        CASE(2)
                            map(pos_y, pos_x) = "-"
                        CASE(3)
                            map(pos_y, pos_x) = "|"
                        CASE(4)
                            map(pos_y, pos_x) = "-"
                    END SELECT
                ENDIF
                
                next_y = pos_y + directions(dir, 1)
                next_x = pos_x + directions(dir, 2)

                IF(    next_y <= SIZE(map, 1) .AND. next_y >= 1 &
                &.AND. next_x <= SIZE(map, 2) .AND. next_x >= 1) THEN
                    IF(   map(next_y, next_x) == "#" &
                    &.OR. map(next_y, next_x) == "O") THEN
                        dir = dir + 1
                        IF(dir == 5) dir = 1
                        map(pos_y, pos_x) = "+"
                        
                        DO k = 1, num_obstacles
                            IF(    known_obstacles(k, 1) == next_x &
                            &.AND. known_obstacles(k, 2) == next_y &
                            &.AND. known_obstacles(k, 3) == pos_x  &
                            &.AND. known_obstacles(k, 4) == pos_y) THEN
                                is_loop = .TRUE.
                                !CALL print_map(map)
                                !WRITE(*, *) "------------------------------"
                                EXIT MOVEMENT
                            ENDIF
                        ENDDO

                        num_obstacles = num_obstacles + 1
                        known_obstacles(num_obstacles, :) = [next_x, next_y, pos_x, pos_y]

                        CYCLE MOVEMENT
                    ENDIF
                ENDIF

                pos_x = next_x
                pos_y = next_y

            ENDDO MOVEMENT

            IF(is_loop) loop_count = loop_count + 1

            CALL clear_map(map)

        ENDDO
    ENDDO

    WRITE(*, *) "NUMBER OF POSSIBLE LOOPS:", loop_count

END SUBROUTINE check_loop
