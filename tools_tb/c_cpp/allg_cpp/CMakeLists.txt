cmake_minimum_required (VERSION 2.6)

# Maps to a solution file (.sln). The solution will 
# have all targets (exe, lib, dll) as projects (.vcproj)
project ("allg_cpp") 

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
            src/f2c.h
            src/SlfDef.h
            src/SlfBase.h
			      src/SlfFkt.h
			      src/SlfFktUnitTable.h
			      src/SlfFkt.cpp
            src/SlfLog.h
            src/SlfLog.cpp
            src/SlfMessage.h
            src/SlfModCtrl.h
            src/SlfModCtrl.cpp
            src/SlfModIntegr.cpp
            src/SlfModelBase.h
            src/SlfModState.h
            src/SlfModStruct.h
            src/SlfModVar.h
            src/SlfModVar.cpp            
            src/SlfNum.cpp
            src/SlfNum.h
            src/SlfNumDecSol.cpp
            src/SlfNumDecSol.h
            src/SlfPar.cpp
            src/SlfParRead.cpp
            src/SlfPar.h
            src/SlfParVar.cpp
            src/SlfParVar.h
            src/SlfParVarVal.cpp
            src/SlfParVarVal.h
            src/SlfPrint.h
            src/SlfPrint.cpp
            src/SlfSolveODE.h
            src/SlfSolveODE_ERK.cpp
            src/SlfSolveODE_ERK.h
            src/SlfSolveODE_GEAR.cpp
            src/SlfSolveODE_GEAR.h
            src/SlfSolveODE_RAD5.cpp
            src/SlfSolveODE_RAD5.h
            src/SlfStr.cpp
            src/SlfStr.h
            src/SlfStrM.cpp
            src/SlfStrM.h
            src/SlfStrRegEx.cpp
            src/SlfStrRegEx.h
            src/SlfStrV.cpp
            src/SlfStrV.h
            src/SlfSys.h
            src/SlfSys.cpp
            src/SlfVec.h
)

install(TARGETS ${PROJECT_NAME} DESTINATION ${PROJECT_SOURCE_DIR}/lib)
install(FILES
            src/f2c.h 
            src/SlfDef.h
            src/SlfBase.h
			      src/SlfFkt.h
            src/SlfLog.h
            src/SlfMessage.h
            src/SlfModCtrl.h
            src/SlfModelBase.h
            src/SlfModState.h
            src/SlfModStruct.h
            src/SlfModVar.h
            src/SlfNum.h
            src/SlfNumDecSol.h
            src/SlfPar.h
            src/SlfParVar.h
            src/SlfParVarVal.h
            src/SlfPrint.h
            src/SlfSolveODE.h
            src/SlfSolveODE_ERK.h
            src/SlfSolveODE_GEAR.h
            src/SlfSolveODE_RAD.h
            src/SlfSolveODE_RAD_int.h
            src/SlfStr.h
            src/SlfStrRegEx.h
            src/SlfStrM.h
            src/SlfStrV.h
            src/SlfSys.h
            src/SlfVec.h
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
