MODULE fileblock_class
    TYPE fileblock
        INTEGER :: id  ! -1 means empty block
        INTEGER :: start_index
        INTEGER :: length
    END TYPE fileblock
END MODULE fileblock_class


PROGRAM main

    USE fileblock_class
    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), PARAMETER :: ex_file = "inputs/day_09_example.txt"
    CHARACTER(LEN=*), PARAMETER :: in_file = "inputs/day_09_inputs.txt"
    CHARACTER(LEN=32)           :: file
    LOGICAL                     :: test = .FALSE.

    INTEGER, DIMENSION(:), ALLOCATABLE :: diskmap
    INTEGER, DIMENSION(:), ALLOCATABLE :: filesystem
    TYPE(fileblock), DIMENSION(:), ALLOCATABLE :: file_blocks
    TYPE(fileblock), DIMENSION(:), ALLOCATABLE :: empty_blocks

    INTERFACE
        SUBROUTINE read_diskmap(filename, diskmap)
            CHARACTER(LEN=*), INTENT(IN)                      :: filename
            INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: diskmap
        END SUBROUTINE read_diskmap

        SUBROUTINE diskmap_to_filesystem(diskmap, filesystem)
            INTEGER, DIMENSION(:), INTENT(IN)                          :: diskmap
            INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: filesystem
        END SUBROUTINE diskmap_to_filesystem

        SUBROUTINE print_filesystem(filesystem)
            INTEGER, DIMENSION(:), INTENT(IN) :: filesystem
        END SUBROUTINE print_filesystem

        SUBROUTINE defrag_simple(filesystem)
            INTEGER, DIMENSION(:), INTENT(INOUT) :: filesystem
        END SUBROUTINE defrag_simple

        SUBROUTINE calc_checksum(filesystem)
            INTEGER, DIMENSION(:), INTENT(IN) :: filesystem
        END SUBROUTINE calc_checksum

        SUBROUTINE diskmap_to_blocks(diskmap, file_blocks, empty_blocks)
            USE fileblock_class
            INTEGER, DIMENSION(:), INTENT(IN) :: diskmap
            TYPE(fileblock), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: file_blocks
            TYPE(fileblock), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: empty_blocks
        END SUBROUTINE diskmap_to_blocks

        SUBROUTINE blocks_to_filesystem(file_blocks, filesystem)
            USE fileblock_class
            TYPE(fileblock), DIMENSION(:), INTENT(IN)         :: file_blocks
            INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: filesystem
        END SUBROUTINE blocks_to_filesystem

        SUBROUTINE defrag_blocks(file_blocks, empty_blocks)
            USE fileblock_class
            TYPE(fileblock), DIMENSION(:), INTENT(INOUT) :: file_blocks
            TYPE(fileblock), DIMENSION(:), INTENT(INOUT) :: empty_blocks
        END SUBROUTINE defrag_blocks
    END INTERFACE
    !=============================================================================================!

    IF(      test) file = ex_file
    IF(.NOT. test) file = in_file

    CALL read_diskmap(TRIM(file), diskmap)
    CALL diskmap_to_filesystem(diskmap, filesystem)
    !CALL print_filesystem(filesystem)
    CALL defrag_simple(filesystem)
    !CALL print_filesystem(filesystem)
    CALL calc_checksum(filesystem)

    DEALLOCATE(filesystem)

    !----- PART II -----!
    CALL diskmap_to_blocks(diskmap, file_blocks, empty_blocks)
    CALL defrag_blocks(file_blocks, empty_blocks)
    CALL blocks_to_filesystem(file_blocks, filesystem)
    !CALL print_filesystem(filesystem)
    CALL calc_checksum(filesystem)

END PROGRAM main


SUBROUTINE print_filesystem(filesystem)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, DIMENSION(:), INTENT(IN) :: filesystem
    
    INTEGER :: i
    !=============================================================================================!

    DO i = 1, SIZE(filesystem)
        IF(filesystem(i) == -1) THEN
            WRITE(*, "(A)", advance="no") "."
        ELSE
            WRITE(*, "(I0)", advance="no") filesystem(i)
        ENDIF
    ENDDO
    WRITE(*, *) " "

END SUBROUTINE print_filesystem


SUBROUTINE read_diskmap(filename, diskmap)

    IMPLICIT NONE
    !=============================================================================================!
    CHARACTER(LEN=*), INTENT(IN)                      :: filename
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: diskmap

    INTEGER :: f_unit, io_stat
    INTEGER :: number
    INTEGER :: size_diskmap
    INTEGER :: i
    !=============================================================================================!

    OPEN(file=filename, action="read", status="old", newunit=f_unit)

    size_diskmap = 0
    DO
        READ(f_unit, "(I1)", advance="no", iostat=io_stat) number
        IF(io_stat /= 0) EXIT
        size_diskmap = size_diskmap + 1
    ENDDO

    ALLOCATE(diskmap(size_diskmap))

    REWIND(f_unit)

    DO i = 1, size_diskmap
        READ(f_unit, "(I1)", advance="no") diskmap(i)
    ENDDO

    CLOSE(f_unit)

END SUBROUTINE read_diskmap


SUBROUTINE diskmap_to_filesystem(diskmap, filesystem)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, DIMENSION(:), INTENT(IN)                 :: diskmap
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: filesystem

    INTEGER :: i, j
    INTEGER :: index, f_index
    !=============================================================================================!

    ALLOCATE(filesystem(SUM(diskmap)))

    index   = 1
    f_index = 0
    DO i = 1, SIZE(diskmap)
        DO j = 1, diskmap(i)
            IF(MODULO(i, 2) == 1) THEN
                filesystem(index) = f_index
            ELSE
                filesystem(index) = -1
            ENDIF
            index = index + 1
        ENDDO
        IF(MODULO(i, 2) == 0) f_index = f_index + 1
    ENDDO

END SUBROUTINE diskmap_to_filesystem


SUBROUTINE defrag_simple(filesystem)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, DIMENSION(:), INTENT(INOUT) :: filesystem

    INTEGER :: front, back

    INTERFACE
        SUBROUTINE print_filesystem(filesystem)
            INTEGER, DIMENSION(:), INTENT(IN) :: filesystem
        END SUBROUTINE print_filesystem
    END INTERFACE
    !=============================================================================================!

    DO back = SIZE(filesystem), 1, -1
        IF(filesystem(back) == -1) CYCLE
        DO front = 1, back
            IF(filesystem(front) /= -1) CYCLE
            filesystem(front) = filesystem(back)
            filesystem(back) = -1
            EXIT
        ENDDO
        !CALL print_filesystem(filesystem)
    ENDDO

END SUBROUTINE defrag_simple


SUBROUTINE calc_checksum(filesystem)

    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, DIMENSION(:), INTENT(IN) :: filesystem

    INTEGER(KIND=16) :: checksum
    INTEGER :: i
    !=============================================================================================!

    checksum = 0
    DO i = 1, SIZE(filesystem)
        IF(filesystem(i) == -1) CYCLE
        checksum = checksum + (i - 1) * filesystem(i)
        !WRITE(*, *) "(DEBUG) INDEX:", i, "CHKSUM:", checksum
    ENDDO

    WRITE(*, *) "Checksum:", checksum

END SUBROUTINE calc_checksum


SUBROUTINE diskmap_to_blocks(diskmap, file_blocks, empty_blocks)

    USE fileblock_class
    IMPLICIT NONE
    !=============================================================================================!
    INTEGER, DIMENSION(:), INTENT(IN) :: diskmap
    TYPE(fileblock), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: file_blocks
    TYPE(fileblock), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: empty_blocks
    
    INTEGER :: i, fb_index, eb_index, fs_index
    INTEGER :: num_fb, num_eb
    !=============================================================================================!

    num_fb = 0
    num_eb = 0
    DO i = 1, SIZE(diskmap)
        IF(MODULO(i, 2) == 1) num_fb = num_fb + 1
        IF(MODULO(i, 2) /= 1) num_eb = num_eb + 1
    ENDDO

    ALLOCATE(file_blocks(num_fb))
    ALLOCATE(empty_blocks(num_eb))

    fb_index = 1
    fs_index = 1
    eb_index = 1
    DO i = 1, SIZE(diskmap)
        IF(MODULO(i, 2) == 1) THEN
            file_blocks(fb_index)%id          = fb_index - 1
            file_blocks(fb_index)%start_index = fs_index
            file_blocks(fb_index)%length      = diskmap(i)
            !WRITE(*, *) file_blocks(fb_index)
            fb_index = fb_index + 1
        ELSE
            empty_blocks(eb_index)%id          = -1
            empty_blocks(eb_index)%start_index = fs_index
            empty_blocks(eb_index)%length      = diskmap(i)
            eb_index = eb_index + 1
        ENDIF

        fs_index = fs_index + diskmap(i)
    ENDDO

END SUBROUTINE diskmap_to_blocks


SUBROUTINE blocks_to_filesystem(file_blocks, filesystem)

    USE fileblock_class
    IMPLICIT NONE
    !=============================================================================================!
    TYPE(fileblock), DIMENSION(:), INTENT(IN)         :: file_blocks
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: filesystem

    INTEGER :: fs_length
    INTEGER :: i, j
    INTEGER :: block_start, block_end
    !=============================================================================================!
    
    fs_length = 0
    DO i = 1, SIZE(file_blocks)
        block_end = file_blocks(i)%start_index + file_blocks(i)%length
        IF(block_end > fs_length) fs_length = block_end
    ENDDO

    ALLOCATE(filesystem(fs_length))

    filesystem(:) = -1

    DO i = 1, SIZE(file_blocks)
        block_start = file_blocks(i)%start_index
        block_end   = block_start + file_blocks(i)%length - 1
        DO j = block_start, block_end
            filesystem(j) = file_blocks(i)%id
        ENDDO
    ENDDO

END SUBROUTINE blocks_to_filesystem


SUBROUTINE defrag_blocks(file_blocks, empty_blocks)

    USE fileblock_class
    IMPLICIT NONE
    !=============================================================================================!
    TYPE(fileblock), DIMENSION(:), INTENT(INOUT) :: file_blocks
    TYPE(fileblock), DIMENSION(:), INTENT(INOUT) :: empty_blocks

    INTEGER :: back, front
    !=============================================================================================!

    DO back = SIZE(file_blocks), 1, -1
        DO front = 1, SIZE(empty_blocks)
            IF(file_blocks(back)%length > empty_blocks(front)%length) CYCLE
            IF(file_blocks(back)%start_index < empty_blocks(front)%start_index) EXIT

            file_blocks(back)%start_index   = empty_blocks(front)%start_index
            empty_blocks(front)%start_index = empty_blocks(front)%start_index + file_blocks(back)%length
            empty_blocks(front)%length      = empty_blocks(front)%length - file_blocks(back)%length

            EXIT
        ENDDO
    ENDDO

    ! DO i = 1, SIZE(file_blocks)
    !     WRITE(*, *) file_blocks(i)
    ! ENDDO

END SUBROUTINE defrag_blocks