if(APPLE)

message("-- Using Macports packages from /opt/local and /usr/local")
set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH}
    /opt/local/lib /usr/local/lib)
set(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH}
    /opt/local/include /usr/local/include)

# Prefer sem_open() over deprecated sem_init()
set(USE_SEM_OPEN 1)
set(SO_PATH DYLD_LIBRARY_PATH)

endif(APPLE)
