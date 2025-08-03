#pragma once
#include "core/IL.hpp"

namespace bitty::IL {

// 프로그램 전체에 대해 타입을 추론하고
// IL의 expression/stmt/function 타입 필드를 제자리 갱신한다.
void inferTypes(Program& program);

}  // namespace bitty::IL
