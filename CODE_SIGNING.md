# Code Signing Policy

Official Windows installers and binaries published by the SWI-Prolog
project are signed via [SignPath Foundation](https://signpath.org/),
which provides free code signing for open-source projects.

## What is signed

The Windows installer (`swipl-*.exe`) produced by the
`release-windows.yml` GitHub Actions workflow.

## How it is signed

- Builds run on GitHub Actions from the `SWI-Prolog/swipl-devel`
  repository.
- The workflow submits the artifact to SignPath; a project maintainer
  must explicitly approve each signing request before a signature is
  issued.
- The signed artifact is then attached to the corresponding GitHub
  release.

## Verification

Signed files carry an Authenticode signature issued to the SWI-Prolog
project. Right-click the file in Windows Explorer and choose
*Properties → Digital Signatures* to inspect it.

## Reporting issues

If you encounter an installer that is unsigned, has an unexpected
signer, or fails signature verification, please report it to
bugs@swi-prolog.org.
