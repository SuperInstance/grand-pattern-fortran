!> Grand Pattern Fibonacci Dual-Direction Architecture
!> Core type definitions module
module gp_types
  implicit none
  private

  integer, parameter, public :: DP = selected_real_kind(15, 307)
  integer, parameter, public :: EMBED_DIM = 8
  integer, parameter, public :: MAX_ENTRIES = 1024

  !> Embedding vector of dimension EMBED_DIM
  type, public :: Embedding
    real(DP), dimension(EMBED_DIM) :: data = 0.0_DP
  end type Embedding

  !> Tick record: timestamp + sensor_id + embedding
  type, public :: Tick
    real(DP) :: timestamp = 0.0_DP
    integer  :: sensor_id = 0
    type(Embedding) :: emb
    real(DP) :: strength = 1.0_DP
  end type Tick

  !> Vibe: position, velocity, acceleration on embedding manifold
  type, public :: Vibe
    type(Embedding) :: position
    type(Embedding) :: velocity
    type(Embedding) :: acceleration
    real(DP)        :: strength = 1.0_DP
  end type Vibe

  !> Database of ticks (perception or prediction)
  type, public :: TickDB
    type(Tick), dimension(MAX_ENTRIES) :: entries
    integer :: count = 0
  contains
    procedure :: push => tickdb_push
    procedure :: last => tickdb_last
    procedure :: clear => tickdb_clear
  end type TickDB

  !> Room: a node in the cellular graph
  type, public :: Room
    integer :: id = 0
    type(TickDB) :: perception_db  ! Z_in
    type(TickDB) :: prediction_db  ! Z_out
    type(Vibe)   :: vibe
  end type Room

  !> Edge between rooms (algorithm connection)
  type, public :: Edge
    integer :: from_id = 0
    integer :: to_id = 0
    real(DP) :: weight = 1.0_DP
  end type Edge

  !> Cellular Graph: rooms as nodes, algorithms as edges
  type, public :: CellularGraph
    type(Room), dimension(:), allocatable :: rooms
    type(Edge), dimension(MAX_ENTRIES) :: edges
    integer :: room_count = 0
    integer :: edge_count = 0
  end type CellularGraph

  !> GC report
  type, public :: GCReport
    integer :: merged = 0
    integer :: decayed = 0
    integer :: pruned = 0
  end type GCReport

contains

  subroutine tickdb_push(db, entry)
    class(TickDB), intent(inout) :: db
    type(Tick), intent(in) :: entry
    if (db%count < MAX_ENTRIES) then
      db%count = db%count + 1
      db%entries(db%count) = entry
    end if
  end subroutine tickdb_push

  function tickdb_last(db) result(t)
    class(TickDB), intent(in) :: db
    type(Tick) :: t
    if (db%count > 0) then
      t = db%entries(db%count)
    end if
  end function tickdb_last

  subroutine tickdb_clear(db)
    class(TickDB), intent(inout) :: db
    db%count = 0
  end subroutine tickdb_clear

end module gp_types
