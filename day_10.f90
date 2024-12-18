MODULE trailhead_class
    TYPE trailhead
        INTEGER :: pos_x
        INTEGER :: pos_y
        INTEGER :: score
        INTEGER :: rating
    END TYPE trailhead

    TYPE landmark
        INTEGER :: pos_x
        INTEGER :: pos_y
        INTEGER :: height
    END TYPE landmark
CONTAINS
    SUBROUTINE find_trailheads(topo_map, trailheads)
        IMPLICIT NONE
        !==========================================================================================!
        INTEGER, DIMENSION(:, :), INTENT(IN)                      :: topo_map
        TYPE(trailhead), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: trailheads
        
        INTEGER :: i, j
        INTEGER :: index
        INTEGER :: num_trailheads
        !==========================================================================================!

        num_trailheads = 0
        DO i = 1, SIZE(topo_map, 1)
            DO j = 1, SIZE(topo_map, 2)
                IF(topo_map(i, j) == 0) num_trailheads = num_trailheads + 1
            ENDDO
        ENDDO

        ALLOCATE(trailheads(num_trailheads))

        index = 1
        DO i = 1, SIZE(topo_map, 1)
            DO j = 1, SIZE(topo_map, 2)
                IF(topo_map(i, j) == 0) THEN
                    trailheads(index) = trailhead(j, i, 0, 0)
                    index = index + 1
                ENDIF
            ENDDO
        ENDDO
    END SUBROUTINE find_trailheads

    SUBROUTINE calc_trailhead_score(topo_map, th)
        IMPLICIT NONE
        !==========================================================================================!
        INTEGER, DIMENSION(:, :), INTENT(IN) :: topo_map
        TYPE(trailhead), INTENT(INOUT)       :: th
        
        TYPE(landmark), DIMENSION(512) :: buffer
        TYPE(landmark), DIMENSION(512) :: buffer_swap
        
        INTEGER :: buffer_size
        INTEGER :: i, j, index, step
        INTEGER :: diff_up, diff_down, diff_left, diff_right
        INTEGER :: x, y, h, max_x, max_y
        !==========================================================================================!

        buffer(1) = landmark(th%pos_x, th%pos_y, 0)
        buffer_size      = 1

        max_y = SIZE(topo_map, 1)
        max_x = SIZE(topo_map, 2)

        DO step = 1, 9
            index = 1

            DO i = 1, buffer_size
                x = buffer(i)%pos_x
                y = buffer(i)%pos_y
                h = buffer(i)%height

                diff_up    = -1
                diff_down  = -1
                diff_left  = -1
                diff_right = -1
                
                IF(y >     1) diff_up    = topo_map(y - 1, x    ) - h
                IF(y < max_y) diff_down  = topo_map(y + 1, x    ) - h
                IF(x >     1) diff_left  = topo_map(y    , x - 1) - h
                IF(x < max_x) diff_right = topo_map(y    , x + 1) - h
                
                IF(diff_up == 1) THEN
                    buffer_swap(index) = landmark(x, y - 1, topo_map(y - 1, x))
                    index = index + 1
                ENDIF

                IF(diff_down == 1) THEN
                    buffer_swap(index) = landmark(x, y + 1, topo_map(y + 1, x))
                    index = index + 1
                ENDIF

                IF(diff_left == 1) THEN
                    buffer_swap(index) = landmark(x - 1, y, topo_map(y, x - 1))
                    index = index + 1
                ENDIF

                IF(diff_right == 1) THEN
                    buffer_swap(index) = landmark(x + 1, y, topo_map(y, x + 1))
                    index = index + 1
                ENDIF
            ENDDO

            buffer_size = index - 1
            buffer(:buffer_size) = buffer_swap(:buffer_size)
        ENDDO

        th%rating = buffer_size

        ! Filter multiples:
        index = 0
        CHECK: DO i = 1, buffer_size
            DO j = 1, index
                IF(buffer(i)%pos_x == buffer_swap(j)%pos_x .AND. &
                  &buffer(i)%pos_y == buffer_swap(j)%pos_y) CYCLE CHECK
            ENDDO
            
            index = index + 1
            buffer_swap(index) = buffer(i)
        ENDDO CHECK

        th%score = index

    END SUBROUTINE calc_trailhead_score
END MODULE


PROGRAM main
    USE trailhead_class
    IMPLICIT NONE
    !==============================================================================================!
    CHARACTER(LEN=*), PARAMETER :: ex_file = "inputs/day_10_example.txt"
    CHARACTER(LEN=*), PARAMETER :: in_file = "inputs/day_10_inputs.txt"
    CHARACTER(LEN=32)           :: file
    LOGICAL                     :: test = .FALSE.

    INTEGER, DIMENSION(:, :), ALLOCATABLE      :: topo_map
    TYPE(trailhead), DIMENSION(:), ALLOCATABLE :: trailheads

    INTEGER :: i
    INTEGER :: score_sum, rating_sum

    INTERFACE
        SUBROUTINE read_topographic_map(filename, topo_map)
            CHARACTER(LEN=*), INTENT(IN)                         :: filename
            INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: topo_map
        END SUBROUTINE read_topographic_map
    END INTERFACE
    !==============================================================================================!

    IF(.NOT. test) file = in_file
    IF(      test) file = ex_file

    CALL read_topographic_map(TRIM(file), topo_map)
    CALL find_trailheads(topo_map, trailheads)
    
    score_sum  = 0
    rating_sum = 0
    DO i = 1, SIZE(trailheads)
        CALL calc_trailhead_score(topo_map, trailheads(i))
        score_sum  = score_sum  + trailheads(i)%score
        rating_sum = rating_sum + trailheads(i)%rating

        WRITE(*, *) "TRAILHEAD:", trailheads(i)
        WRITE(*, *) REPEAT("-", 80)
    ENDDO

    WRITE(*, *) "SUM OF SCORES :", score_sum
    WRITE(*, *) "SUM OF RATINGS:", rating_sum

END PROGRAM main


SUBROUTINE read_topographic_map(filename, topo_map)
    IMPLICIT NONE
    !==============================================================================================!
    CHARACTER(LEN=*), INTENT(IN)                         :: filename
    INTEGER, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: topo_map

    INTEGER            :: size_x, size_y
    INTEGER            :: f_unit, io_stat
    INTEGER            :: i, j
    CHARACTER          :: chr
    CHARACTER(LEN=256) :: line
    !==============================================================================================!

    OPEN(file=filename, action="read", status="old", newunit=f_unit)

    ! Determine size of the topographic map:
    size_x = 0
    size_y = 0
    DO
        READ(f_unit, "(A)", advance="no", iostat=io_stat) chr
        IF(io_stat /= 0) EXIT
        size_x = size_x + 1
    ENDDO

    REWIND(f_unit)

    DO
        READ(f_unit, "(A)", iostat=io_stat) chr
        IF(io_stat /= 0) EXIT
        size_y = size_y + 1
    ENDDO

    ! Allocate map data and read in values:
    ALLOCATE(topo_map(size_y, size_x))
    REWIND(f_unit)
    DO i = 1, size_y
        READ(f_unit, "(A)") line
        DO j = 1, size_x
            READ(line(j:j), "(I1)") topo_map(i, j)
        ENDDO
        !WRITE(*, *) topo_map(i, :)
    ENDDO

    CLOSE(f_unit)
END SUBROUTINE read_topographic_map