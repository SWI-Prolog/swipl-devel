# Set target options for Android

# Minizip needs USE_FILE32API set in 32-bit architectures
if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  target_compile_definitions(libswipl PRIVATE USE_FILE32API=1)
endif()
