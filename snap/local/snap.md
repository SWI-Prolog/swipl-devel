# Snap

Snap that runs the latest version of swipl and swipl-win.

The snap is strictly contained, so it has limited access to the host
system via the following plugs:

| Plug              | Description               |
| ----------------- | ------------------------- |
| home              | $HOME (no hidden folders) |
| network           | network access            |
| x11               | I/O and graphics output   |
| desktop           | common desktop elements   |
| desktop-legacy    | legacy desktop elements   |
| removable-media   | removable media           |
| opengl            | opengl/gpu hardware       |
| audio-playback    | audio playback devices    |
| audio-record      | audio record devices      |

If other connections are needed they can be added from this
[list](https://snapcraft.io/docs/supported-interfaces).

## Installation
```sh
snap install swi-prolog
```

The shared Qt5 content snap is used for the Qt ui, and to set the proper paths, dependencies and variables like opengl, pulseaudo, that swipl expects to find.

For this reason it needs to be installed and setup as follows the first time:
```sh
snap install kde-frameworks-5-core18
snap connect swi-prolog:kde-frameworks-5-plug kde-frameworks-5-core18:kde-frameworks-5-core18-slot
```

If the snap did not add the automated aliases you can add them as follows:
```sh
snap alias swi-prolog.swipl swipl
snap alias swi-prolog.swipl-win swipl-win
```
