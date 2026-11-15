# Build Windows installer using GitHub Actions

The workflow `.github/workflows/release-windows.yml` cross-builds the
Windows installer in the `swipl-mingw-build` container and has the
result Authenticode-signed by [SignPath](https://signpath.io), which we
use under their free plan for open source projects.

## Automated

Push a release tag (V*)

## By hand

    gh workflow run release-windows.yml -f ref=master
    gh run watch
    gh run download -n swipl-<version>-1.x64.exe

Downloads ``swipl-<version>-1.x64.exe{,sha256}``. The run also leaves an
artifact `unsigned-installer`, which is what was submitted for signing;
a bare `gh run download` fetches both.

## Signing

Signing is part of the run: the installer is uploaded as the
`unsigned-installer` artifact, a signing request is submitted, and the
job blocks until SignPath returns the signed file. Should the policy
require an approver, do so at <https://app.signpath.io>; the job waits
up to five hours. The organisation, project and policy are spelled out
in the workflow.

Verify the download on Linux with

    osslsigncode verify swipl-<version>-1.x64.exe

and on Windows with `Get-AuthenticodeSignature` or the *Digital
Signatures* tab of the file properties.

### One-time setup

  - Add the predefined *GitHub.com* trusted build system to the SignPath
    organisation and link it to the project.
  - Install the SignPath GitHub App on the repository and grant it
    access. SignPath uses it to verify the artifact's origin.
  - Store an API token of a user with submitter rights on the signing
    policy as the repository secret `SIGNPATH_API_TOKEN`.

### Testing changes to the workflow

Signing only happens from a branch the signing policy accepts. Dispatch
the workflow definition from that branch, choosing separately which
sources it builds:

    gh workflow run release-windows.yml --ref <branch> -f ref=master

Nothing is published to a GitHub release unless the run was triggered by
a `V*` tag.
