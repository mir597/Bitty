#pragma once

#include <cassert>
#include <sstream>
#include <string>
#include <vector>

namespace bitty::Ty {

enum class Tag {
  Unknown,  // Bottom
  Any,      // Top
  Nil,      // nil
  Bool,
  Int,
  Float,
  Number,  // Int ∪ Float
  String,
};

inline const char* toString(Tag t) {
  switch (t) {
    case Tag::Unknown:
      return "Unknown";
    case Tag::Any:
      return "Any";
    case Tag::Nil:
      return "Nil";
    case Tag::Bool:
      return "Bool";
    case Tag::Int:
      return "Int";
    case Tag::Float:
      return "Float";
    case Tag::Number:
      return "Number";
    case Tag::String:
      return "String";
  }
  return "Unknown";
}

inline Tag join(Tag a, Tag b) {
  if (a == b) {
    return a;
  } else if (a == Tag::Unknown) {
    return b;
  } else if (b == Tag::Unknown) {
    return a;
  } else if (a == Tag::Any || b == Tag::Any) {
    return Tag::Any;
  } else if (a == Tag::Number && (b == Tag::Int || b == Tag::Float)) {
    return Tag::Number;
  } else if (b == Tag::Number && (a == Tag::Int || a == Tag::Float)) {
    return Tag::Number;
  } else if ((a == Tag::Int && b == Tag::Float) ||
             (a == Tag::Float && b == Tag::Int)) {
    return Tag::Number;
  }

  return Tag::Any;
}

struct Arena {
  struct Node {
    int parent;
    Tag tag;
  };
  std::vector<Node> nodes;

  int newVar(Tag init = Tag::Unknown) {
    int id = (int)nodes.size();
    nodes.push_back(Node{id, init});
    return id;
  }

  int find(int x) {
    int& p = nodes[x].parent;
    if (p == x) {
      return x;
    }
    p = find(p);
    return p;
  }

  Tag get(int x) {
    return nodes[find(x)].tag;
  }

  int unify(int a, int b) {
    a = find(a);
    b = find(b);
    if (a == b) {
      return a;
    }

    nodes[b].parent = a;
    nodes[a].tag = join(nodes[a].tag, nodes[b].tag);
    return a;
  }

  int bind(int v, Tag t) {
    int r = find(v);
    nodes[r].tag = join(nodes[r].tag, t);
    return r;
  }

  std::string toString() {
    std::string result;
    std::stringstream ss;
    ss << "[";
    for (size_t i = 0; i < nodes.size(); i++) {
      auto& node = nodes[i];
      ss << i << "{";
      ss << "p: " << node.parent << ", ";
      ss << "t: " << Ty::toString(node.tag);
      ss << "}, ";
    }
    ss << "]" << std::endl;
    return ss.str();
  }
};

}  // namespace bitty::Ty
