# Script utilities for Conda builds.   We use Python for the scripting
# such that we can share the same scripts over all operating systems.

import subprocess

def install_component(c):
    if subprocess.run([ "cmake", f"-DCMAKE_INSTALL_COMPONENT={c}",
                        "-P", "cmake_install.cmake"
                      ],
                      cwd="build").returncode != 0:
        exit(1)

