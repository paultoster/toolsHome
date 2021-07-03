cmake_minimum_required (VERSION 2.6)

# Maps to a solution file (.sln). The solution will 
# have all targets (exe, lib, dll) as projects (.vcproj)
project ("allg_c") 

# cmake_policy(SET CMP0015 NEW)
# cmake_policy(SET CMP0043 NEW)


# Turn on the ability to create folders to organize projects (.vcproj)
# It creates "CMakePredefinedTargets" folder by default and adds CMake
# defined projects like INSTALL.vcproj and ZERO_CHECK.vcproj
# set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Set compiler flags and options. 
# Here it is setting the Visual Studio warning level to 4
# set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")

# Command to output information to the console
# Useful for displaying errors, warnings, and debugging
# message ("cxx Flags: " ${CMAKE_CXX_FLAGS})

# Set Properties->General->Configuration Type to Application(.exe)
# Creates main.exe with the listed sources (main.cxx)
# Adds sources to the Solution Explorer
# add_executable (${PROJECT_NAME} main.cpp)

# include_directories ("${PROJECT_SOURCE_DIR}/src")

add_library(${PROJECT_NAME} STATIC
            src/amem.c
            src/amem.h
            src/amsg.c
            src/amsg.h
            src/anum.c
            src/anum.h
            src/apar.c
            src/apar.h
            src/artf.c
            src/artf.h
            src/astr.c
            src/astr.h
            src/astrs.c
            src/astrs.h
            src/asys.c
            src/asys.h
            src/avar.c
            src/avar.h
            src/numeric.c
            src/numeric.h
            src/definer.h
            src/system.h
)

install(TARGETS ${PROJECT_NAME} DESTINATION ${PROJECT_SOURCE_DIR}/lib)
install(FILES 
        src/amem.h
        src/amsg.h
        src/anum.h
        src/apar.h
        src/artf.h
        src/astr.h
        src/astrs.h
        src/asys.h
        src/avar.h
        src/numeric.h
        src/definer.h
        src/system.h
        DESTINATION ${PROJECT_SOURCE_DIR}/include)

# Sub-directories where more CMakeLists.txt exist
# add_subdirectory(src)

# target_link_libraries(${PROJECT_NAME}
#                       PUBLIC "sub1"
#                       )
# Turn on CMake testing capabilities
# enable_testing()

# Add test cases
# add_test(AppTest1 ${PROJECT_BINARY_DIR}/bin/app 100)
# add_test(AppTest2 ${PROJECT_BINARY_DIR}/bin/app 200)
# add_test(AppTest3 ${PROJECT_BINARY_DIR}/bin/app 300)
