# Publishing the snap

## Building

   snapcraft clean swi-prolog
   snapcraft

## Local testing

   snap install --devmode swi-prolog_<version>.snap

## Publishing

   snapcraft login		   (Ubuntu one credentials)
   snapcraft push swi-prolog_<version>.snap
   snapcraft list-revisions swi-prolog
   snapcraft release swi-prolog 1 beta,edge

Channels is a list of `stable`, `candidate`, `beta` or `edge`

