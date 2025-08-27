#pragma once

#include "core/IL.hpp"

namespace bitty::IL {

// Run whole-program IL optimizations:
//  - Inline functions that are called exactly once (conservatively: single return at tail, non-recursive).
//  - DAG-based CSE inside blocks (pure expressions only), with safe hoisting.
// The pass mutates the Program in place.
void optimizeProgram(Program& p);

} // namespace bitty::IL