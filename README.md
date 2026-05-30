# Grand Pattern Fibonacci Dual-Direction Architecture - Fortran

Fortran 2018 implementation of the Grand Pattern cellular graph system.

## Architecture

The core system is a cellular graph where each cell (room) maintains:
- **Perception DB (Z_in)**: incoming sensor embeddings
- **Prediction DB (Z_out)**: predicted future embeddings
- **JEPA mapping**: cross-DB comparison computing prediction error (surprise)
- **Double-entry bookkeeping**: every tick updates BOTH databases, must balance
- **Vibe**: (position, velocity, acceleration) tuple on the embedding manifold
- **GC**: 3-phase (merge similar → decay old → prune weak)
- **Cellular graph**: rooms as nodes, algorithms as edges, murmur as gossip protocol

## Building

```bash
make
```

## Testing

```bash
make test
```

## Project Structure

- `src/types.f90` - Core type definitions (Embedding, Tick, Vibe, Room, CellularGraph)
- `src/ops.f90` - Core operations (tick, predict, balance_check, compute_vibe, gc, murmur, correlate)
- `tests/test_gp.f90` - Comprehensive test suite (13 tests)

## Requirements

- Fortran 2018 compatible compiler (gfortran ≥ 9.0)
- No external dependencies
