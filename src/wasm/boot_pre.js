// Emscripten pre-js to mount the actual
// filesystem as NODEFS.
Module.onRuntimeInitialized = () => {
  FS.mkdir('/swipl-devel');
  FS.mount(NODEFS, { root: '..' }, '/swipl-devel');
};
