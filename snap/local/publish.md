# Publishing the snap

## Building

   SNAPCRAFT_BUILD_ENVIRONMENT=multipass snapcraft clean swi-prolog
   SNAPCRAFT_BUILD_ENVIRONMENT=multipass snapcraft

## Local testing

   snap install --devmode swi-prolog_<version>.snap

## Publishing

   snapcraft login		   (Ubuntu one credentials)
   snapcraft upload swi-prolog_<version>.snap
   snapcraft list-revisions swi-prolog

   snapcraft release swi-prolog 1 edge,candidate,beta
OR
   snapcraft release swi-prolog 1 stable

Channels is a list of `stable`, `candidate`, `beta` or `edge`

