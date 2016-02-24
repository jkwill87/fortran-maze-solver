!!
! fms_msg.f90: A module used to handle user input and process cli output
!
! Created: Friday, February 5, 2016
! Author: Jessy Williams
! Contact: jessy@jessywilliams.com
!!

module fms_msg

    use fms_types
    implicit none

    ! Error codes
    integer, parameter :: OK = 0
    integer, parameter :: ERR_IO = 1
    integer, parameter :: ERR_NODIM = 2
    integer, parameter :: ERR_NOSOL = 3
    integer, parameter :: ERR_START = 4
    integer, parameter :: ERR_EXIT = 5

    ! Colour codes
    character(len=*),parameter:: C_RED    = achar(27)//'[31m'
    character(len=*),parameter:: C_BLUE   = achar(27)//'[34m'
    character(len=*),parameter:: C_YELLOW = achar(27)//'[33m'
    character(len=*),parameter:: C_GREEN  = achar(27)//'[32m'
    character(len=*),parameter:: C_RESET  = achar(27)//'[0m'

    ! Maze tile constants
    character, parameter :: MAZE_EMPTY    = '.'
    character, parameter :: MAZE_WALL     = '*'
    character, parameter :: MAZE_START    = 'o'
    character, parameter :: MAZE_EXIT     = 'e'
    character, parameter :: MAZE_VISITED  = 'x'
    character, parameter :: MAZE_EXAUSTED = '-'
    character, parameter :: MAZE_UNKNOWN  = '?'

    integer, parameter :: R_BUFFER = 1024

contains


    !!
    ! get_path:
    !   gets the maze file path from arg[1] if available, otherwise from stdin;
    !   either way, verifies that the path is accessible prior to processing
    !
    ! Parameters:
    !   - arg (string): a string to be used as the path (optional)
    !
    ! Return:
    !   - the file path as a string
    !
    ! Errors:
    !   - will throw ERR_IO if the file cannot be found
    !!

   function get_path (arg)

        character(*),optional,intent(in) :: arg
        character(len=R_BUFFER)          :: get_path

        logical :: r_status


        ! Get file path
        if(present(arg) .and. len_trim(arg)/=0 ) then
            get_path = arg
            print *,'Mazefile: ',trim(get_path)
        else
            print *, "please enter the path of the maze to be solved:"
            read '(a)',get_path
        endif

        ! verify mzae file exists
        inquire(file=get_path, exist=r_status)
        if(.not. r_status) call throw(ERR_IO)

    end function get_path


    !!
    ! mapout:
    !   prints a maze to stdout; can be called prior to solving to display the 
    !   maze as is, or after solving to include the solution the solution
    !
    ! Parameters:
    !   - maze (maze_t): the structure containing the maze to print
    !   - colour (logical): a flag, which when true prints to stdout using 
    !     terminal escape sequences to print with colour
    !
    ! Preconditions:
    !   - the maze has been properly initialized
    !   - the dimensions specified in the maze match those of the array
    !
    ! Postconditions:
    !   - the formatted maze layout is sent to stdout
    !!

    subroutine mapout (maze,could_solve)

        type(maze_t),intent(in) :: maze
        logical,optional,intent(in) :: could_solve
        integer:: i,j

#ifndef _NO_COLOUR

        ! Iterate through maze array
        do i=1,maze%rows
            do j=1,maze%cols

                ! Print to stdout using colour terminal escape sequences
                    select case (maze%grid(i,j))
                        case(MAZE_START)
                            write(unit=*, fmt="(a11)", advance="no") &
                                C_RED//MAZE_START//C_RESET
                        case(MAZE_EXIT)
                            write(unit=*, fmt="(a11)", advance="no") &
                                C_RED//MAZE_EXIT//C_RESET
                        case(MAZE_EMPTY)
                            write(unit=*, fmt="(a2)", advance="no") &
                                MAZE_EMPTY
                        case(MAZE_EXAUSTED)
                            write(unit=*, fmt="(a11)", advance="no") &
                                C_GREEN//MAZE_EXAUSTED//C_RESET
                        case(MAZE_VISITED)
                            write(unit=*, fmt="(a11)", advance="no") &
                                C_RED//MAZE_VISITED//C_RESET
                        case(MAZE_WALL)
                            write(unit=*, fmt="(a11)", advance="no") &
                                C_BLUE//MAZE_WALL//C_RESET
                        case default
                            write(unit=*, fmt="(a2)", advance="no") &
                                maze%grid(i,j)
                    end select
            end do

            print *, 
        end do

        ! Print Status (if provided)
        if(present(could_solve)) then
            if(could_solve) then
                print *, C_YELLOW//"Maze Was Solved :D"//C_RESET
            else
                print *, C_YELLOW//"Maze Could Not Be Solved :("//C_RESET
            endif
        end if


        ! Print Legend
        print *, 
        print *, "LEGEND:"
        print *, '['//C_BLUE//MAZE_WALL//C_RESET//']: Wall'
        print *, '['//MAZE_EMPTY//']: Untraversed Passage'
        print *, '['//C_RED//MAZE_START//C_RESET//']: Maze Entrance'
        print *, '['//C_RED//MAZE_EXIT//C_RESET//']: Maze Exit'
        print *, '['//C_RED//MAZE_VISITED//C_RESET//']: Solution Path'
        print *, '['//C_GREEN//MAZE_EXAUSTED//C_RESET//']: Backtracked Path'
#else

    ! Print to stdout w/o using colour terminal escape sequences
    do i=1,maze%rows
        do j=1,maze%cols
            write(unit=*, fmt="(a2)", advance="no") maze%grid(i,j)
        end do
        print *, 
    end do

    ! Print Status (if provided)
    if(present(could_solve)) then
        if(could_solve) then
            print *, "Maze Was Solved :D"
        else
            print *, "Maze Could Not Be Solved :("
        endif
    end if

    ! Print Legend
    print *, 
    print *, "LEGEND:"
    print *, '['//MAZE_WALL//"]: Wall"
    print *, '['//MAZE_EMPTY//']: Untraversed Passage'
    print *, '['//MAZE_START//']: Maze Entrance'
    print *, '['//MAZE_EXIT//']: Maze Exit'
    print *, '['//MAZE_VISITED//']: Solution Path'
    print *, '['//MAZE_EXAUSTED//']: Backtracked Path'
#endif

    end subroutine mapout


    !!
    ! throw:
    !   provides a mechanism for handling and reporting irreconcilable runtime
    !   errors
    !
    ! Parameters:
    !   - error_code (integer): the enumerated code corresponding to the thrown
    !     error
    !
    ! Postconditions:
    !   - a description of the error message is printed to stdout
    !   - program operation is terminated
    !!

    subroutine throw (error_code)

        integer,intent(in) :: error_code

        print *, "Error:"

        select case (error_code)

            case (ERR_IO)
                print *, &
                    "Could not open maze file for reading."

            case (ERR_NODIM)
                print *, &
                    "Could not source dimensions from file."

            case (ERR_NOSOL)
                print *, &
                    "Provided maze file could not be solved."

            case (ERR_START)
                print *, &
                    "Maze requires exactly one start point."

            case (ERR_EXIT)
                print *, &
                    "Maze requires exactly one exit point."

        end select

        print *, "Exiting."
        stop 1
        
    end subroutine throw

end module fms_msg
