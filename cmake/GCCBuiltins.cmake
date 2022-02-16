# Compiler features

check_c_source_compiles(
    "unsigned int x = 11; int main() { return __builtin_clz(x); }"
    HAVE__BUILTIN_CLZ)
check_c_source_compiles(
    "unsigned int x = 11; int main() { return __builtin_popcount(x); }"
    HAVE__BUILTIN_POPCOUNT)
check_c_source_compiles(
    "int i=0; int main() { return __builtin_expect(i, 0) ? 0 : 1; }"
    HAVE___BUILTIN_EXPECT)
check_c_source_compiles(
    "int i=0; struct {int x[10];} st; int main() { return __builtin_choose_expr(1, i, st); }"
    HAVE___BUILTIN_CHOOSE_EXPR)
check_c_source_compiles(
    "int i=0; int main() { return __builtin_types_compatible_p(typeof(i), int); }"
    HAVE___BUILTIN_TYPES_COMPATIBLE_P)
check_c_source_compiles(
    "__thread int i=0; int main() { return 0; }"
    HAVE___THREAD)
check_c_source_compiles(
    "volatile int i=0; int main() { return 0; }"
    HAVE_VOLATILE)
check_c_source_compiles(
    "static inline foo() { return 0; } int main() { return foo(); }"
    HAVE_INLINE)
check_c_source_compiles(
    "int main() { void *p = &&lbl; goto *p; lbl: return 0; }"
    O_LABEL_ADDRESSES)
check_c_source_runs(
    "extern int __attribute__((weak)) foo(); int main() { return &foo == 0 ? 0 : 1; }"
    HAVE_WEAK_ATTRIBUTE)

function(check_visibility)
  set(CMAKE_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS} -Werror)
  check_c_source_compiles(
      "int foo __attribute__((visibility (\"hidden\"))) = 1; int main() { return 0; }"
      HAVE_VISIBILITY_ATTRIBUTE)
  if(HAVE_VISIBILITY_ATTRIBUTE)
    set(HAVE_VISIBILITY_ATTRIBUTE 1 PARENT_SCOPE)
  endif()
endfunction()
check_visibility()

# Builtin functions that lead to conflicts

check_c_source_compiles(
    "int main() { char *s = alloca(10); return s!=0; }"
    HAVE_ALLOCA)
check_c_source_compiles(
    "#include <math.h>\nint main() { double x; return signbit(x); }"
    HAVE_SIGNBIT)
