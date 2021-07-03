cmake_minimum_required (VERSION 2.6)

# Maps to a solution file (.sln). The solution will 
# have all targets (exe, lib, dll) as projects (.vcproj)
project ("tire_model_static_calc") 

# Turn on the ability to create folders to organize projects (.vcproj)
# It creates "CMakePredefinedTargets" folder by default and adds CMake
# defined projects like INSTALL.vcproj and ZERO_CHECK.vcproj
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Set compiler flags and options. 
# Here it is setting the Visual Studio warning level to 4
# set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")

# Command to output information to the console
# Useful for displaying errors, warnings, and debugging
message ("cxx Flags: " ${CMAKE_CXX_FLAGS})

# Set Properties->General->Configuration Type to Application(.exe)
# Creates main.exe with the listed sources (main.cxx)
# Adds sources to the Solution Explorer
# add_executable (${PROJECT_NAME} main.cpp)

set(MAIN_SOURCE_DIR "${PROJECT_SOURCE_DIR}/src")

set(ALL_CPP_DIR "D:/toolsHome/tools_tb/c_cpp/allg_cpp")
set(ALL_CPP_SOURCE_DIR "${ALL_CPP_DIR}/src")

set(PY_INCLUDE_DIR "C:/Python39/include")
set(PY_LIB_DIR "C:/Python39/libs")

include_directories (
     ${PY_INCLUDE_DIR}
     ${MAIN_SOURCE_DIR}
     ${ALL_CPP_SOURCE_DIR}
)
#     ${ALL_CPP_SOURCE_DIR}

link_directories (
     ${PY_LIB_DIR}
)

add_library(${PROJECT_NAME} SHARED
               src/py_interface_definition.cpp
               src/py_interface_definition.h
               src/py_interface.cpp
               src/py_interface.h
               src/mod_tire_static_calc.cpp
               src/mod_tire_static_calc.h
               src/run_mod.cpp
               src/run_mod.h
               )

target_link_libraries(${PROJECT_NAME} PUBLIC allg_cpp)
# target_link_libraries(${PROJECT_NAME} PUBLIC)

# Sub-directories where more CMakeLists.txt exist
add_subdirectory("${ALL_CPP_DIR}" build)

# target_link_libraries(${PROJECT_NAME}
#                       PUBLIC "sub1"
#                       )
# Turn on CMake testing capabilities
# enable_testing()

# Add test cases
# add_test(AppTest1 ${PROJECT_BINARY_DIR}/bin/app 100)
# add_test(AppTest2 ${PROJECT_BINARY_DIR}/bin/app 200)
# add_test(AppTest3 ${PROJECT_BINARY_DIR}/bin/app 300)




set(CMAKE_VS_SDK_INCLUDE_DIRECTORIES "${PY_INCLUDE_DIR}" "$(IncludePath)")
set(CMAKE_VS_SDK_LIBRARY_DIRECTORIES "${PY_LIB_DIR}" "$(LibraryPath)")

SET_TARGET_PROPERTIES(${PROJECT_NAME}   PROPERTIES 
                      SUFFIX ".pyd"
                      RUNTIME_OUTPUT_DIRECTORY_DEBUG "${PROJECT_SOURCE_DIR}/work"
                      RUNTIME_OUTPUT_DIRECTORY_RELEASE "${PROJECT_SOURCE_DIR}/work")

