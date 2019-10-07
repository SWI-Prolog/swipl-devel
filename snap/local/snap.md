# Snap

Snap that runs the latest version of swipl and swipl-win.

The snap is strictly contained, so it has limited access to the host system via the following plugs:

| Plug              | Description               | Auto-connected |
| ----------------- | ------------------------- | -------------- |
| home              | $HOME (no hidden folders) | yes            |
| network           | network access            | yes            |
| x11               | I/O and graphics output   | yes            |
| desktop           | common desktop elements   | yes            |
| desktop-legacy    | legacy desktop elements   | yes            |
| removable-media   | removable media           | no             |
| opengl            | opengl/gpu hardware       | yes            |
| audio-playback    | audio playback devices    | yes            |
| audio-record      | audio record devices      | no             |

If other connections are needed they can be added from this [list](https://snapcraft.io/docs/supported-interfaces).

## Installation
```sh
snap install swi-prolog
```

The shared Qt5 content snap is used for the Qt ui, and to set the proper paths, dependencies and variables like opengl, pulseaudo, that swipl expects to find.

It is autoinstalled, but is has to be connected the first time.

Once it's released on the store this part can be automated.
```sh
snap connect swi-prolog:kde-frameworks-5-plug kde-frameworks-5-core18:kde-frameworks-5-core18-slot
```

Most interfaces are autoconnected, exept for the removable-media and audio-record.

If you need these functionalities you can enable them via:
```sh
snap connect swi-prolog:removable-media
snap connect swi-prolog:audio-record
```

If the snap did not add the automated aliases you can add them as follows:
```sh
snap alias swi-prolog.swipl swipl
snap alias swi-prolog.swipl-win swipl-win
```

## Paths

$HOME

This environment variable is re-written by snapd so that each snap appears to have a dedicated home directory that is a subdirectory of the real home directory.

The location of the config and data folders is different from a classic install.

| Description    | Description  |
| -------------- | ------------ |
| Config         | /home/$USER/snap/swi-prolog/current/.config/swi-prolog
| Data           | /home/$USER/snap/swi-prolog/current/.local/share/swi-prolog

This is set automatically at build time, but it might be useful to know if you need access to these folders.