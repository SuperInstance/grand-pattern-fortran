!> Grand Pattern Fibonacci Dual-Direction Architecture
!> Comprehensive test suite
program test_gp
  use gp_types
  use gp_ops
  implicit none

  integer :: pass_count, fail_count, total
  pass_count = 0
  fail_count = 0
  total = 0

  call test_tick_updates_perception_db()
  call test_predict_generates_embedding()
  call test_balance_check_passes()
  call test_balance_check_fails()
  call test_vibe_computation()
  call test_merge_reduces_count()
  call test_decay_reduces_strengths()
  call test_prune_removes_weak()
  call test_full_gc_cycle()
  call test_cross_room_correlation()
  call test_murmur_between_rooms()
  call test_graph_construction()
  call test_tick_propagation()

  write(*,'(A)') ''
  write(*,'(A,I0,A,I0,A)') 'Results: ', pass_count, ' passed, ', fail_count, ' failed'
  if (fail_count > 0) then
    write(*,'(A)') 'FAIL: Some tests failed'
    stop 1
  else
    write(*,'(A)') 'ALL TESTS PASSED'
  end if

contains

  subroutine assert(cond, test_name)
    logical, intent(in) :: cond
    character(len=*), intent(in) :: test_name
    total = total + 1
    if (cond) then
      pass_count = pass_count + 1
      write(*,'(A,A)') '  PASS: ', test_name
    else
      fail_count = fail_count + 1
      write(*,'(A,A)') '  FAIL: ', test_name
    end if
  end subroutine assert

  subroutine test_tick_updates_perception_db()
    type(Room) :: r
    type(Embedding) :: reading
    real(DP) :: err
    logical :: surprise

    r%id = 1
    reading%data = [1.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]

    call tick_room(r, reading, 1.0_DP, 42, 0.5_DP, err, surprise)

    call assert(r%perception_db%count == 1, 'tick updates perception DB count')
    call assert(r%perception_db%entries(1)%emb%data(1) == 1.0_DP, &
                'tick stores correct embedding')
    call assert(r%perception_db%entries(1)%sensor_id == 42, &
                'tick stores correct sensor_id')
  end subroutine test_tick_updates_perception_db

  subroutine test_predict_generates_embedding()
    type(Room) :: r
    type(Embedding) :: pred

    r%id = 1
    r%vibe%position%data = [1.0_DP, 2.0_DP, 3.0_DP, 4.0_DP, 5.0_DP, 6.0_DP, 7.0_DP, 8.0_DP]
    r%vibe%velocity%data = [0.1_DP, 0.2_DP, 0.3_DP, 0.4_DP, 0.5_DP, 0.6_DP, 0.7_DP, 0.8_DP]
    r%vibe%acceleration%data = [0.01_DP, 0.02_DP, 0.03_DP, 0.04_DP, 0.05_DP, 0.06_DP, 0.07_DP, 0.08_DP]

    pred = predict(r)

    call assert(pred%data(1) > 0.0_DP, 'predict generates non-zero embedding')
    ! position + velocity + 0.5*acceleration
    call assert(abs(pred%data(1) - (1.0_DP + 0.1_DP + 0.005_DP)) < 1.0e-10_DP, &
                'predict computes correct value')
  end subroutine test_predict_generates_embedding

  subroutine test_balance_check_passes()
    type(Room) :: r
    type(Tick) :: t

    r%id = 1
    t%emb%data = 1.0_DP
    t%strength = 1.0_DP

    call r%perception_db%push(t)
    call r%prediction_db%push(t)
    call r%perception_db%push(t)
    call r%prediction_db%push(t)

    call assert(balance_check(r), 'balance check passes when equal')
  end subroutine test_balance_check_passes

  subroutine test_balance_check_fails()
    type(Room) :: r
    type(Tick) :: t

    r%id = 1
    t%emb%data = 1.0_DP
    t%strength = 1.0_DP

    call r%perception_db%push(t)
    call r%perception_db%push(t)
    call r%prediction_db%push(t)

    call assert(.not. balance_check(r), 'balance check fails when unequal')
  end subroutine test_balance_check_fails

  subroutine test_vibe_computation()
    type(Room) :: r
    type(Tick) :: t
    integer :: i

    r%id = 1
    ! Push 3 entries with different embeddings
    do i = 1, 3
      t%timestamp = real(i, DP)
      t%emb%data = real(i, DP)
      t%strength = 1.0_DP
      call r%perception_db%push(t)
    end do

    call compute_vibe(r)

    call assert(r%vibe%position%data(1) == 3.0_DP, 'vibe position is last entry')
    call assert(r%vibe%velocity%data(1) == 1.0_DP, 'vibe velocity is diff')
    call assert(r%vibe%acceleration%data(1) == 0.0_DP, 'vibe acceleration is diff-of-diff')
    call assert(r%vibe%strength == 3.0_DP, 'vibe strength is entry count')
  end subroutine test_vibe_computation

  subroutine test_merge_reduces_count()
    type(TickDB) :: db
    integer :: merged
    type(Tick) :: t

    ! Two identical embeddings should merge
    t%emb%data = [1.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]
    t%strength = 1.0_DP
    call db%push(t)
    call db%push(t)
    call db%push(t)

    merged = merge_similar(db, 0.99_DP)

    call assert(merged > 0, 'merge found similar entries')
    call assert(db%count < 3, 'merge reduced count')
  end subroutine test_merge_reduces_count

  subroutine test_decay_reduces_strengths()
    type(TickDB) :: db
    type(Tick) :: t

    t%emb%data = 1.0_DP
    t%strength = 1.0_DP
    call db%push(t)
    call db%push(t)

    call decay(db, 0.9_DP)

    call assert(db%entries(1)%strength < 1.0_DP, 'decay reduces strength')
    call assert(abs(db%entries(1)%strength - 0.9_DP) < 1.0e-10_DP, &
                'decay reduces by correct amount')
  end subroutine test_decay_reduces_strengths

  subroutine test_prune_removes_weak()
    type(TickDB) :: db
    integer :: pruned
    type(Tick) :: t

    t%emb%data = 1.0_DP
    t%strength = 1.0_DP
    call db%push(t)
    t%strength = 0.01_DP
    call db%push(t)
    t%strength = 0.5_DP
    call db%push(t)

    pruned = prune(db, 0.1_DP)

    call assert(pruned == 1, 'prune removes exactly 1 weak entry')
    call assert(db%count == 2, 'prune leaves 2 entries')
  end subroutine test_prune_removes_weak

  subroutine test_full_gc_cycle()
    type(Room) :: r
    type(GCReport) :: report
    type(Embedding) :: reading
    real(DP) :: err
    logical :: surprise
    integer :: i

    r%id = 1
    ! Add some data
    do i = 1, 5
      reading%data = real(i, DP)
      call tick_room(r, reading, real(i, DP), 1, 0.5_DP, err, surprise)
    end do

    report = gc(r, 0.99_DP, 0.8_DP, 0.5_DP)

    call assert(balance_check(r), 'GC maintains balance')
    call assert(report%merged >= 0, 'GC returns merge count')
    call assert(report%pruned >= 0, 'GC returns prune count')
  end subroutine test_full_gc_cycle

  subroutine test_cross_room_correlation()
    type(Room) :: a, b

    a%id = 1
    b%id = 2

    a%vibe%position%data = [1.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]
    b%vibe%position%data = [1.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]

    call assert(abs(correlate(a, b) - 1.0_DP) < 1.0e-10_DP, &
                'identical vibes correlate at 1.0')

    b%vibe%position%data = [0.0_DP, 1.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]
    call assert(abs(correlate(a, b)) < 1.0e-10_DP, &
                'orthogonal vibes correlate at 0.0')
  end subroutine test_cross_room_correlation

  subroutine test_murmur_between_rooms()
    type(Room) :: a, b

    a%id = 1
    b%id = 2
    a%vibe%position%data = [1.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]
    b%vibe%position%data = [0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]

    call murmur(a, b, 0.5_DP)

    call assert(abs(b%vibe%position%data(1) - 0.5_DP) < 1.0e-10_DP, &
                'murmur blends vibe position')
  end subroutine test_murmur_between_rooms

  subroutine test_graph_construction()
    type(CellularGraph) :: graph

    call add_room(graph, 1)
    call add_room(graph, 2)
    call add_room(graph, 3)

    call assert(graph%room_count == 3, 'graph has 3 rooms')

    call add_edge(graph, 1, 2, 1.0_DP)
    call add_edge(graph, 2, 3, 0.5_DP)

    call assert(graph%edge_count == 2, 'graph has 2 edges')
    call assert(graph%edges(1)%from_id == 1, 'edge 1 from correct')
    call assert(graph%edges(2)%to_id == 3, 'edge 2 to correct')
  end subroutine test_graph_construction

  subroutine test_tick_propagation()
    type(CellularGraph) :: graph
    type(Embedding) :: reading
    real(DP) :: err
    logical :: surprise

    call add_room(graph, 1)
    call add_room(graph, 2)
    call add_edge(graph, 1, 2, 1.0_DP)

    ! Setup initial vibes
    graph%rooms(1)%vibe%position%data = [1.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, &
                                          0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]
    graph%rooms(2)%vibe%position%data = [0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, &
                                          0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]

    reading%data = [2.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP, 0.0_DP]

    call propagate_tick(graph, graph%rooms(1), reading, 1.0_DP, 1, 0.5_DP, 0.5_DP)

    ! Room 2's vibe should have been influenced by murmur
    call assert(graph%rooms(2)%vibe%position%data(1) > 0.0_DP, &
                'tick propagation influences connected room via murmur')
  end subroutine test_tick_propagation

end program test_gp
