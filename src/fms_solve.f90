!!
! fms_solve.f90: A module used to analyze and solve mazes
!
! Created: Friday, February 5, 2016
! Author: Jessy Williams
! Contact: jessy@jessywilliams.com
!!

module fms_solve

    use fms_msg
    use fms_types
    implicit none

    ! Cardinal Direction Constants
    integer, parameter   :: NORTH = 1
    integer, parameter   :: EAST  = 2
    integer, parameter   :: SOUTH = 3
    integer, parameter   :: WEST  = 4

contains


    !!
    ! parse:
    !   parses the dimensions and layout of a maze file
    !
    ! Parameters:
    !   - path(string): the file path of the maze file to parse
    !
    ! Preconditions:
    !   - the path points to a file which exists and can be read
    !   - the passed file conforms to the specified format
    !
    ! Return:
    !   - a populated maze_t which includes the maze dimensions, start position,
    !     and layout
    !
    ! Errors:
    !   - ERR_NODIM: if the passed maze file doesn't contain dimensions
    !   - ERR_IO: if the maze file's dimensions do not conform to its specified
    !     dimensions
    !   - ERR_START: if the maze does not have exactly one start position
    !   - ERR_EXIT: if the maze does not have exactly one exit position
    !!

    function parse (path)

        type(maze_t)            :: parse
        character(*),intent(in) :: path

        character(len=:), allocatable :: line
        integer                       :: r_status
        integer                       :: startCount,exitCount
        integer                       :: x,y

        ! Set counters to 0
        startCount = 0
        exitCount  = 0

        ! Open maze file
        open(1, file=path, status='old', access='sequential', &
            action='read')

        ! Get dimensions
        read (1,*,iostat=r_status)  parse%rows, parse%cols
        if(r_status /= 0) call throw(ERR_NODIM)
        allocate(parse%grid(parse%rows,parse%cols))
        allocate(character(len=parse%cols)::line)


        ! Read maze file line by line
        do y=1,parse%rows
            read (1,*,iostat=r_status) line

            ! Check for inconsistent lines
            if(r_status/=0) call throw(ERR_IO)

            ! Commit lines to 2d character array
            do x=1,parse%cols
                ! print*,y,x,line(x:x)
                parse%grid(y,x)=line(x:x)

                ! Get start positional information, keep count
                if(line(x:x) == MAZE_START) then
                    startCount = startCount+1;
                    parse%startY = y
                    parse%startX = x
                end if

                ! Keep count of exits found
                if(line(x:x) == MAZE_EXIT) exitCount = exitCount+1;

            enddo
        enddo

        ! Verify number of start/ exit points
        if(startCount /= 1) call throw(ERR_START)
        if(exitCount /= 1) call throw(ERR_EXIT)


    end function parse


    !!
    ! solve:
    !   uses a depth-first pathfinding algorithm to attempt to traverse the maze
    !   from the starting position to the end position
    !
    ! Parameters:
    !   - maze(maze_t): the maze to solve
    !
    ! Preconditions:
    !   - the passed maze type contains is initialized and populated correctly
    !
    ! Return:
    !   - a logical value which is .true. if the maze was solved, else .false.
    !!

    function solve (maze)

        type(maze_t)  :: maze
        type(path_t)  :: current, next
        type(stack_t) :: stack
        logical :: found, solve
        integer :: i

        ! Initialize stack
        call init(stack,maze%rows,maze%cols)

        ! Push 
        call push(stack,path_t(maze%startY,maze%startX,MAZE_START))

        ! Begin pathfinding loop
        do 
            found = .false.

            ! Get current path from stack
            current = top(stack)

            ! Look for exit
            do i=NORTH,WEST
                next = nav(i,current,maze)
                if (next%s == MAZE_EXIT) then
                    maze%grid(current%y,current%x) = MAZE_VISITED
                    found = .true.
                    solve = .true.
                    exit
                endif
            end do
            if(found) exit

            ! Else look for empty
            do i=NORTH,WEST
                next = nav(i,current,maze)
                if (next%s==MAZE_EMPTY) then
                    call push(stack, &
                        path_t(next%y,next%x,maze%grid(next%y,next%x)))
                    maze%grid(current%y,current%x) = MAZE_VISITED
                    found = .true.
                    exit
                endif
            end do
            if(found) cycle

            ! Else look for visited
            do i=NORTH,WEST
                next = nav(i,current,maze)
                if (next%s==MAZE_VISITED) then
                    current = pop(stack)
                    maze%grid(current%y,current%x) = MAZE_EXAUSTED
                    found = .true.
                    exit
                endif
            end do
            if(found) cycle
            
            ! Else maze cannot be solved
            solve = .false.
            exit

        end do

            maze%grid(maze%startY,maze%startX)=MAZE_START

    end function solve


    !!
    ! nav:
    !   will create a path_t type based off of a relatively adjacent one
    !
    ! Parameters:
    !   - direction(integer): an enumerated value corresponding to a cardinal 
    !     direction to move to
    !   - current(path_t): the point of reference to move from
    !   - maze(maze_t): the maze file containing the 2D path_t array to source
    !     from
    !
    ! Preconditions:
    !   - the passed maze type contains is initialized and populated correctly
    !
    ! Return:
    !   - a maze_t corresponding to the adjacent path_t
    !!

    pure function nav(direction,current,maze)
        
        integer,intent(in)       :: direction
        type(path_t),intent(in)  :: current
        type(maze_t),intent(in)  :: maze
        type(path_t) :: nav


        nav = current
        nav%s = MAZE_UNKNOWN

        select case (direction)

            case (NORTH)
                if (nav%y > 1 .and. nav%y <= maze%rows) then
                    nav%y=nav%y-1
                    nav%s = maze%grid(nav%y,nav%x)
                endif

            case (EAST)
                if (nav%x < maze%cols .and. nav%x >= 1) then
                    nav%x=nav%x+1
                    nav%s = maze%grid(nav%y,nav%x)
                endif

            case (SOUTH)
                if (nav%y >= 1 .and. nav%y < maze%rows) then
                    nav%y=nav%y+1
                    nav%s = maze%grid(nav%y,nav%x)
                endif

            case (WEST)
                if (nav%x <= maze%cols .and. nav%x > 1) then
                    nav%x=nav%x-1
                    nav%s = maze%grid(nav%y,nav%x)
                endif

        end select

    end function nav


end module fms_solve
