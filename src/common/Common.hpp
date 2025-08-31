#pragma once

#include <memory>

template <class T, class NodeT>
std::unique_ptr<T> create(NodeT&& node) {
  auto st = std::make_unique<T>();
  st->node = std::move(node);
  return st;
}

[[noreturn]] inline void unreachable() {
#if defined(_MSC_VER)
  __assume(false);
#elif defined(__GNUC__)
  __builtin_unreachable();
#else
  // fallback
  assert(false && "unreachable");
  std::abort();
#endif
}