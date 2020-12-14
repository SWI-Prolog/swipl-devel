# Publishing the snap

## Building

   snapcraft clean swi-prolog
   snapcraft

## Local testing

   snap install --devmode swi-prolog_<version>.snap

## Publishing

   snapcraft login		   (Ubuntu one credentials)
   snapcraft upload swi-prolog_<version>.snap
   snapcraft list-revisions swi-prolog

   snapcraft release swi-prolog 1 edge
OR
   snapcraft release swi-prolog 1 stable,candidate,beta

Channels is a list of `stable`, `candidate`, `beta` or `edge`

