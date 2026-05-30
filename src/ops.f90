!> Grand Pattern Fibonacci Dual-Direction Architecture
!> Core operations module
module gp_ops
  use gp_types
  implicit none
  private

  public :: cosine_similarity
  public :: cosine_distance
  public :: embedding_add
  public :: embedding_scale
  public :: embedding_zero
  public :: embedding_lerp
  public :: tick_room
  public :: predict
  public :: balance_check
  public :: compute_vibe
  public :: merge_similar
  public :: decay
  public :: prune
  public :: gc
  public :: murmur
  public :: correlate
  public :: add_room
  public :: add_edge
  public :: propagate_tick
  public :: surprise_check

contains

  ! ---- Embedding operations ----

  function cosine_similarity(a, b) result(sim)
    type(Embedding), intent(in) :: a, b
    real(DP) :: sim, dotp, na, nb
    dotp = dot_product(a%data, b%data)
    na = norm2(a%data)
    nb = norm2(b%data)
    if (na < 1.0e-12_DP .or. nb < 1.0e-12_DP) then
      sim = 0.0_DP
    else
      sim = dotp / (na * nb)
    end if
  end function cosine_similarity

  function cosine_distance(a, b) result(dist)
    type(Embedding), intent(in) :: a, b
    real(DP) :: dist
    dist = 1.0_DP - cosine_similarity(a, b)
  end function cosine_distance

  function embedding_add(a, b) result(c)
    type(Embedding), intent(in) :: a, b
    type(Embedding) :: c
    c%data = a%data + b%data
  end function embedding_add

  function embedding_scale(a, s) result(c)
    type(Embedding), intent(in) :: a
    real(DP), intent(in) :: s
    type(Embedding) :: c
    c%data = a%data * s
    c%strength = a%strength
  end function embedding_scale

  function embedding_zero() result(c)
    type(Embedding) :: c
    c%data = 0.0_DP
  end function embedding_zero

  function embedding_lerp(a, b, t) result(c)
    type(Embedding), intent(in) :: a, b
    real(DP), intent(in) :: t
    type(Embedding) :: c
    c%data = a%data * (1.0_DP - t) + b%data * t
  end function embedding_lerp

  ! ---- Core architecture functions ----

  !> Process a tick: update perception DB, generate prediction, compute error
  subroutine tick_room(room, reading, timestamp, sensor_id, threshold, error_out, is_surprise)
    type(Room), intent(inout) :: room
    type(Embedding), intent(in) :: reading
    real(DP), intent(in) :: timestamp
    integer, intent(in) :: sensor_id
    real(DP), intent(in) :: threshold
    real(DP), intent(out) :: error_out
    logical, intent(out) :: is_surprise

    type(Tick) :: perception_tick, prediction_tick
    type(Embedding) :: predicted

    ! 1. Store perception in Z_in
    perception_tick%timestamp = timestamp
    perception_tick%sensor_id = sensor_id
    perception_tick%emb = reading
    perception_tick%strength = 1.0_DP
    call room%perception_db%push(perception_tick)

    ! 2. Generate prediction from current vibe
    predicted = predict(room)
    prediction_tick%timestamp = timestamp
    prediction_tick%sensor_id = sensor_id
    prediction_tick%emb = predicted
    prediction_tick%strength = 1.0_DP
    call room%prediction_db%push(prediction_tick)

    ! 3. Compute prediction error
    error_out = cosine_distance(reading, predicted)

    ! 4. Surprise check
    is_surprise = (error_out > threshold)

    ! 5. Update vibe with new data
    call compute_vibe(room)
  end subroutine tick_room

  !> Predict next embedding from current vibe trajectory
  function predict(room) result(pred)
    type(Room), intent(in) :: room
    type(Embedding) :: pred

    ! Prediction = position + velocity + 0.5 * acceleration
    pred%data = room%vibe%position%data + &
                room%vibe%velocity%data + &
                0.5_DP * room%vibe%acceleration%data
  end function predict

  !> Verify double-entry bookkeeping: |Z_in.count - Z_out.count| == 0
  function balance_check(room) result(balanced)
    type(Room), intent(in) :: room
    logical :: balanced
    balanced = (room%perception_db%count == room%prediction_db%count)
  end function balance_check

  !> Compute vibe from DB history
  subroutine compute_vibe(room)
    type(Room), intent(inout) :: room
    integer :: n
    type(Embedding) :: centroid, diff, diff2

    n = room%perception_db%count

    if (n == 0) then
      room%vibe%position = embedding_zero()
      room%vibe%velocity = embedding_zero()
      room%vibe%acceleration = embedding_zero()
      room%vibe%strength = 0.0_DP
      return
    end if

    ! Position = centroid of recent entries
    if (n >= 1) then
      centroid = room%perception_db%entries(n)%emb
    end if

    if (n >= 2) then
      diff%data = room%perception_db%entries(n)%emb%data - &
                  room%perception_db%entries(n-1)%emb%data
    else
      diff = embedding_zero()
    end if

    if (n >= 3) then
      diff2%data = diff%data - &
                   (room%perception_db%entries(n-1)%emb%data - &
                    room%perception_db%entries(n-2)%emb%data)
    else
      diff2 = embedding_zero()
    end if

    room%vibe%position = centroid
    room%vibe%velocity = diff
    room%vibe%acceleration = diff2
    room%vibe%strength = real(n, DP)
  end subroutine compute_vibe

  !> Merge embeddings within cosine similarity threshold
  function merge_similar(db, threshold) result(merged_count)
    type(TickDB), intent(inout) :: db
    real(DP), intent(in) :: threshold
    integer :: merged_count
    logical, dimension(MAX_ENTRIES) :: alive
    integer :: i, j, n

    alive = .false.
    merged_count = 0
    n = db%count

    do i = 1, n
      alive(i) = .true.
    end do

    do i = 1, n
      if (.not. alive(i)) cycle
      do j = i + 1, n
        if (.not. alive(j)) cycle
        if (cosine_similarity(db%entries(i)%emb, db%entries(j)%emb) > threshold) then
          ! Average the embeddings
          db%entries(i)%emb%data = 0.5_DP * (db%entries(i)%emb%data + db%entries(j)%emb%data)
          db%entries(i)%strength = db%entries(i)%strength + db%entries(j)%strength
          alive(j) = .false.
          merged_count = merged_count + 1
        end if
      end do
    end do

    ! Compact
    call compact_db(db, alive)
  end function merge_similar

  !> Exponential decay on all embedding strengths
  subroutine decay(db, rate)
    type(TickDB), intent(inout) :: db
    real(DP), intent(in) :: rate
    integer :: i
    do i = 1, db%count
      db%entries(i)%strength = db%entries(i)%strength * rate
    end do
  end subroutine decay

  !> Remove embeddings below minimum strength
  function prune(db, min_strength) result(pruned_count)
    type(TickDB), intent(inout) :: db
    real(DP), intent(in) :: min_strength
    integer :: pruned_count
    logical, dimension(MAX_ENTRIES) :: alive
    integer :: i

    alive = .false.
    pruned_count = 0

    do i = 1, db%count
      if (db%entries(i)%strength >= min_strength) then
        alive(i) = .true.
      else
        pruned_count = pruned_count + 1
      end if
    end do

    call compact_db(db, alive)
  end function prune

  !> Full GC cycle: merge → decay → prune
  function gc(room, merge_threshold, decay_rate, min_strength) result(report)
    type(Room), intent(inout) :: room
    real(DP), intent(in) :: merge_threshold, decay_rate, min_strength
    type(GCReport) :: report

    ! Phase 1: Merge similar
    report%merged = merge_similar(room%perception_db, merge_threshold)
    report%merged = report%merged + merge_similar(room%prediction_db, merge_threshold)

    ! Phase 2: Decay
    call decay(room%perception_db, decay_rate)
    call decay(room%prediction_db, decay_rate)
    report%decayed = room%perception_db%count + room%prediction_db%count

    ! Phase 3: Prune weak
    report%pruned = prune(room%perception_db, min_strength)
    report%pruned = report%pruned + prune(room%prediction_db, min_strength)

    ! Rebalance: ensure DBs stay equal length after GC
    call rebalance(room)
  end function gc

  !> Send vibe summary from one room to another (murmur/gossip)
  subroutine murmur(from, to, influence)
    type(Room), intent(in) :: from
    type(Room), intent(inout) :: to
    real(DP), intent(in) :: influence

    ! Blend from's vibe position into to's vibe position
    to%vibe%position%data = to%vibe%position%data * (1.0_DP - influence) + &
                            from%vibe%position%data * influence
  end subroutine murmur

  !> Cosine similarity of vibe positions between two rooms
  function correlate(room_a, room_b) result(sim)
    type(Room), intent(in) :: room_a, room_b
    real(DP) :: sim
    sim = cosine_similarity(room_a%vibe%position, room_b%vibe%position)
  end function correlate

  !> Check if error exceeds threshold (surprise)
  function surprise_check(error, threshold) result(is_surprise)
    real(DP), intent(in) :: error, threshold
    logical :: is_surprise
    is_surprise = (error > threshold)
  end function surprise_check

  ! ---- Graph operations ----

  subroutine add_room(graph, room_id)
    type(CellularGraph), intent(inout) :: graph
    integer, intent(in) :: room_id
    type(Room), dimension(:), allocatable :: tmp
    integer :: n

    if (.not. allocated(graph%rooms)) then
      allocate(graph%rooms(0))
    end if

    n = size(graph%rooms) + 1
    allocate(tmp(n))
    tmp(1:n-1) = graph%rooms
    call move_alloc(tmp, graph%rooms)

    graph%rooms(n)%id = room_id
    graph%room_count = n
  end subroutine add_room

  subroutine add_edge(graph, from_id, to_id, weight)
    type(CellularGraph), intent(inout) :: graph
    integer, intent(in) :: from_id, to_id
    real(DP), intent(in), optional :: weight

    if (graph%edge_count < MAX_ENTRIES) then
      graph%edge_count = graph%edge_count + 1
      graph%edges(graph%edge_count)%from_id = from_id
      graph%edges(graph%edge_count)%to_id = to_id
      if (present(weight)) then
        graph%edges(graph%edge_count)%weight = weight
      end if
    end if
  end subroutine add_edge

  !> Propagate a tick through all edges from a room
  subroutine propagate_tick(graph, from_room, reading, timestamp, sensor_id, &
                            threshold, murmur_influence)
    type(CellularGraph), intent(inout) :: graph
    type(Room), intent(in) :: from_room
    type(Embedding), intent(in) :: reading
    real(DP), intent(in) :: timestamp, threshold, murmur_influence
    integer, intent(in) :: sensor_id
    integer :: i, to_idx

    do i = 1, graph%edge_count
      if (graph%edges(i)%from_id == from_room%id) then
        ! Find target room and murmur to it
        to_idx = find_room_idx(graph, graph%edges(i)%to_id)
        if (to_idx > 0) then
          call murmur(from_room, graph%rooms(to_idx), murmur_influence)
        end if
      end if
    end do
  end subroutine propagate_tick

  ! ---- Internal helpers ----

  subroutine compact_db(db, alive)
    type(TickDB), intent(inout) :: db
    logical, dimension(MAX_ENTRIES), intent(in) :: alive
    type(Tick), dimension(MAX_ENTRIES) :: tmp
    integer :: i, j

    j = 0
    do i = 1, db%count
      if (alive(i)) then
        j = j + 1
        tmp(j) = db%entries(i)
      end if
    end do

    db%entries(1:j) = tmp(1:j)
    db%count = j
  end subroutine compact_db

  subroutine rebalance(room)
    type(Room), intent(inout) :: room
    ! Trim the larger DB to match the smaller one (from the front)
    integer :: target

    target = min(room%perception_db%count, room%prediction_db%count)
    room%perception_db%count = target
    room%prediction_db%count = target
  end subroutine rebalance

  function find_room_idx(graph, room_id) result(idx)
    type(CellularGraph), intent(in) :: graph
    integer, intent(in) :: room_id
    integer :: idx, i

    idx = 0
    do i = 1, graph%room_count
      if (graph%rooms(i)%id == room_id) then
        idx = i
        return
      end if
    end do
  end function find_room_idx

end module gp_ops
