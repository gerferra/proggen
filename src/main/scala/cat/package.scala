package object cat {
  type Pair[+A, +B] = (A, B) {
    // don't know what I'm doing :) if I use = got an error about covariant type on invariant position
    type _1 <: A
    type _2 <: B
  }
}