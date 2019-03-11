# Set target options for Android

# Minizip needs USE_FILE32API on Android
target_compile_definitions(libswipl PRIVATE USE_FILE32API=1)
