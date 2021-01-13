cmake_minimum_required(VERSION 3.10)
PROJECT(ExifDemo)


SET(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -rdynamic")

if(NOT TOOLCHAIN_DIR)
        #SET(TOOLCHAIN_DIR "/home/pc/allwinner/t7linux-auto/out/sun8iw17p1/linux/common/buildroot/host/usr/bin")
        SET(TOOLCHAIN_DIR "/opt/gcc-linaro-5.3.1-2016.05-x86_64_arm-linux-gnueabi/bin")
endif()

set(CMAKE_CXX_COMPILER ${TOOLCHAIN_DIR}/arm-linux-gnueabi-g++)
set(CMAKE_C_COMPILER   ${TOOLCHAIN_DIR}/arm-linux-gnueabi-gcc)


include_directories(./)

AUX_SOURCE_DIRECTORY(./ SRC_LIST)


if(CMAKE_COMPILER_IS_GNUCXX)
    add_compile_options(-std=c++11 -std=gnu++11 -fstack-protector-all)
  #  add_compile_options(-std=c++11 -std=gnu++11 -g -fstack-protector-all -funwind-tables -ffunction-sections -fsanitize=address -fno-omit-frame-pointer -rdynamic)
    message(STATUS "optional:-std=c++11")   
endif()


ADD_EXECUTABLE(ExifDemo_T7 ${SRC_LIST})
