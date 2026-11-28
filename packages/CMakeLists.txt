# All the packages in ${SWIPL_PACKAGE_LIST} inherit from here.

list(PREPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Each package must do the following "include" (but see
# https://github.com/SWI-Prolog/swipl-devel/pull/1598#issuecomment-7243907656):

# include(PrologPackage)

foreach(package ${SWIPL_PACKAGE_LIST})
  swipl_package_component(${package}
			  CMAKE_INSTALL_DEFAULT_COMPONENT_NAME)
  add_subdirectory(${package})
endforeach(package)
