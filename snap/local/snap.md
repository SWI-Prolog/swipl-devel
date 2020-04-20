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

If the snap did not add the automated aliases you can add them as follows:
```sh
snap alias swi-prolog.swipl swipl
snap alias swi-prolog.swipl-win swipl-win
```

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
