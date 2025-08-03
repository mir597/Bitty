#pragma once

#include "core/IL.hpp"

#if defined(__GNUC__) || defined(__clang__)
#define BITTY_CP_ALWAYS_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define BITTY_CP_ALWAYS_INLINE __forceinline
#else
#define BITTY_CP_ALWAYS_INLINE inline
#endif

namespace bitty::IL {

// Constant propagation + replacement pass
void propagateConstants(Program& program);

}  // namespace bitty::IL
