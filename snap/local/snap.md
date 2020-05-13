# Snap

Snap that runs the latest version of swipl and swipl-win.

The snap is strictly contained, so it has limited access to the host
system via the following plugs:

| Plug              | Description               | Autoconnected |
| ----------------- | ------------------------- | ------------- |
| home              | $HOME (no hidden folders) | Yes           |
| network           | network access            | Yes           |
| x11               | I/O and graphics output   | Yes           |
| desktop           | common desktop elements   | Yes           |
| desktop-legacy    | legacy desktop elements   | Yes           |
| removable-media   | removable media           | No            |
| opengl            | opengl/gpu hardware       | Yes           |
| audio-playback    | audio playback devices    | Yes           |
| audio-record      | audio record devices      | No            |

To connect the removable-media (read/write files on removable storage devices) run the following command:
```sh
snap connect swi-prolog:removable-media
```

To connect the audio-record (allows audio recording via supported services) run the following command:
```sh
snap connect swi-prolog:audio-record
```

If other connections are needed they can be added from this
[list](https://snapcraft.io/docs/supported-interfaces) in future releases.

## Installation

```sh
snap install swi-prolog
```

If the snap did not add the automated aliases you can add them as follows:
```sh
snap alias swi-prolog.swipl swipl
snap alias swi-prolog.swipl-win swipl-win
```

## Usage

The config folders are located in a directory specific to the snap:
```
~/.config -> ~/snap/swi-prolog/current/.config
~/.local -> ~/snap/swi-prolog/current/.local
```

The rest of the non hidden folders in $HOME are visible as normal

## Font issues

There is an issue with fontconfig, which is   used by the GUI tools, and
snap. You are a victim of if running   `?-  emacs.` results in the error
below:


```
?- emacs.
[PCE fatal: @helvetica_roman_12/font: Xopen failed on @display/display
	in:	<No exception goal>
]
	[13] M @helvetica_roman_12/font ->_xopen(@display/display)
	[12] M @helvetica_roman_12/font ->_xopen(@display/display)
...
```

This can be resolved by cleaning the   fontconfig global cache. Doing so
has no consequences except for the   first application to use fontconfig
starting slow.

```
sudo rm /var/cache/fontconfig/*
```
