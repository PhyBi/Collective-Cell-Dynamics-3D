! Brief: This module defines the types of the basic objects

module types
    implicit none
    private
    public :: dp, vector, bead, cell, simbox
    
    integer, parameter :: dp = selected_real_kind(15)
    
    type vector
        real(dp), dimension(2) :: comp ! 2D components : x, y
    end type vector
    
    type bead
        type(vector) :: r ! position
        type(vector) :: v  ! velocity
        type(vector) :: n_cap ! motility axis / polarity (unit vector)
        type(vector) :: f_intra, f_inter ! intra and intercellular forces on the bead
        real(dp) :: m  ! mass
    end type bead
    
    type cell(n)
        integer, len :: n ! length type parameter containing #beads per cell
        type(bead), dimension(n) :: beads
        real(dp) :: k_spring ! spring constant between neighboring beads
        real(dp) :: P ! internal pressure*l0
        real(dp) :: l0 ! spring rest length between neighboring beads
        
        contains
        
        procedure :: cm => get_cm_of_cell ! function to output centre of mass of cell
        procedure :: set_f_intra ! subroutine to compute f_intra
    end type cell

    type simbox
        real(dp) :: length ! square box size (x and y edges)
        real(dp) :: shear ! applied using affine-transformation + Lees-Edwards boundary
    end type simbox
    
    interface
        ! The following procedure(s) shall be defined in separate file(s)
        ! Note the use of `import` below
        subroutine set_f_intra(this)
            import :: cell
            class(cell(n=*)), intent(inout) :: this
        end subroutine set_f_intra
    end interface
    
    contains
    
    pure function get_cm_of_cell(this) result(cm)
        class(cell(n=*)), intent(in) :: this
        type(vector) :: cm
        
        cm%comp(1) = sum(this%beads%r%comp(1)) / this%n
        cm%comp(2) = sum(this%beads%r%comp(2)) / this%n
    end function get_cm_of_cell
end module types
