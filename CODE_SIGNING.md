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
project and a trusted timestamp, so they remain verifiable after the
signing certificate expires.

On **Windows**, right-click the installer in Explorer and choose
*Properties → Digital Signatures*, or from a command prompt:

    signtool verify /pa /v swipl-*.x64.exe

On **Linux** or **macOS**, install `osslsigncode` and run:

    osslsigncode verify swipl-*.x64.exe

A valid signature reports SWI-Prolog as the signer, a chain that
terminates in a public root CA, and a matching message digest.

## Reporting issues

If you encounter an installer that is unsigned, has an unexpected
signer, or fails signature verification, please report it to
bugs@swi-prolog.org.
