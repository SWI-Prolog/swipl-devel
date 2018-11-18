# Set target options for Android

message(STATUS "Crosscompiling for Android: ${CMAKE_ANDROID_ARCH}")

# Minizip needs USE_FILE32API set in 32-bit architectures
if($(CMAKE_ANDROID_ARCH) MATCHES "arm$|mips$|x86$")
   target_compile_definitions(libswipl PRIVATE USE_FILE32API=1)
endif()
