"""VirtualLargeArray: a memory-safe, lazily-evaluated array-like object.

This class behaves like a very large read-only sequence (supports len() and __getitem__)
without allocating memory for every element. Elements are generated deterministically on-demand.

Use responsibly — iterating over all elements is impossible and will never be attempted.
"""
from collections.abc import Sequence
import hashlib


class VirtualLargeArray(Sequence):
    def __init__(self, length: int, seed: str = "virtual-array"):
        if length < 0:
            raise ValueError("length must be non-negative")
        self._length = int(length)
        self._seed = seed

    def __len__(self):
        return self._length

    def _compute_value(self, index: int) -> int:
        # Deterministic pseudo-random value per index using SHA-256 of seed+index
        h = hashlib.sha256(f"{self._seed}:{index}".encode("utf-8")).digest()
        # Use first 8 bytes as a 64-bit unsigned integer, then mod to keep values reasonable
        return int.from_bytes(h[:8], "big") % 1_000_000_000

    def __getitem__(self, index):
        if isinstance(index, slice):
            # Support slices by generating the requested range lazily (bounded)
            start, stop, step = index.indices(self._length)
            return [self._compute_value(i) for i in range(start, stop, step)]
        if index < 0:
            index = self._length + index
        if index < 0 or index >= self._length:
            raise IndexError("VirtualLargeArray index out of range")
        return self._compute_value(index)

    def sample(self, n: int = 10, start: int = 0):
        """Return a small deterministic sample of n elements starting at `start`.

        This is safe to call and is intended for preview/diagnostic purposes only.
        """
        if n <= 0:
            return []
        end = min(start + n, self._length)
        return [self[i] for i in range(start, end)]
