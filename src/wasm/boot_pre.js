// Emscripten pre-js to mount the actual
// filesystem as NODEFS.
Module.onRuntimeInitialized = () => {
  FS.mkdir('/working');
  FS.mount(NODEFS, { root: '..' }, '/working');
};
