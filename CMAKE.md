# Native build

```{bash}
mkdir build
cd build
cmake ..
make -j 8
```

# Cross build

## 64 bit Windows from Linux

```{bash}
mkdir win64
cd win64
cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win64.cmake ..
```

## 32 bit Windows from Linux

```{bash}
mkdir win32
cd win32
cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win32.cmake ..
```

# Issues

  - Build documention
    - online plain text manual?
  - Create installers
  - Install pkg-config files
