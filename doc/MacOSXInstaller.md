# Building the signed, notarized macOS .pkg installer

This document covers the end-to-end process for producing the macOS
`.pkg` installer for SWI-Prolog --- the same one users download
from the website and run from Finder.  It is targeted at the person
responsible for cutting a Mac release and intended to be reproducible
when certificates expire, the build machine is reinstalled, or a
new maintainer takes over.

The high-level flow is:

  1. Two single-architecture builds (`arm64`, `x86_64`) are produced.
  2. The x86_64 binaries are imported into the `arm64` build tree
     and `lipo`'d into universal Mach-O files.
  3. `ninja pkg` runs the `cmake/macos_app_fixup/CMakeLists.txt`
     custom target, which:
     - installs into a staging prefix (default `~/cmake/$BUILDDIR`),
     - runs `scripts/macosx_bundle_fixup.sh` to bundle MacPorts /
       Homebrew dylibs into the framework, rewrite install names,
       and `codesign` every Mach-O with the Developer ID Application
       cert + hardened-runtime + entitlements,
     - `pkgbuild` wraps the staging tree into a component pkg,
     - `productbuild` signs the outer pkg with the Developer ID
       Installer cert,
     - `xcrun notarytool submit --wait` uploads to Apple and blocks
       until the notarization verdict comes back,
     - `xcrun stapler staple` attaches the notarization ticket so
       Gatekeeper accepts the pkg offline.

Two scripts drive the flow:

  - `scripts/make-distribution` --- contains the
    `build_macos_gcc_universal()` shell function that orchestrates
    the above.
  - `scripts/macosx_bundle_fixup.sh` --- the per-binary fixup +
    code-signing pass, invoked from CMake's install hook.

The build machine needs an Apple Developer account, a few signing
artefacts in its login keychain, and one environment variable
(`$APPLEID`) before the function will run.


## One-time setup

These steps only need to be done once per macOS user account that
will produce releases.  They are also the steps to redo when the
certificates expire (see [Renewing certificates](#renewing-certificates)
below).

### 1. Apple Developer account

  - Active Apple Developer Program membership ($99/yr) tied to your
    Apple ID.  Renew at https://developer.apple.com/account before
    the membership lapses, otherwise newly issued certs are not
    recognised by Apple's notarization service.
  - Note your Team ID --- a 10-character alphanumeric string, e.g.
    `ACDDGQR4VW`, shown under "Membership Details".

### 2. Generate a Certificate Signing Request

In Keychain Access on the build Mac:

  - menu *Keychain Access* → *Certificate Assistant* → *Request a
    Certificate From a Certificate Authority…*
  - email = your Apple ID email, common name = your name,
    "CA Email Address" blank
  - "Saved to disk"
  - this creates `CertificateSigningRequest.certSigningRequest`
    on disk and pairs a fresh private key into your login keychain

### 3. Issue both Developer ID certificates

At https://developer.apple.com/account → *Certificates, Identifiers
& Profiles* → *Certificates* → **+** :

  - Pick **Developer ID Application**, upload the CSR, download the
    `.cer`, double-click to import into the login keychain.  This is
    used by `codesign` to sign the `.app`, `.framework`, `.dylib`s
    and `.so`s.
  - Repeat for **Developer ID Installer**.  This is used by
    `productbuild --sign` to sign the outer `.pkg`.

Verify both certs landed in the keychain and have a matching private
key (they each expand into one entry in Keychain Access showing
cert + key):

    security find-identity -v -p codesigning    # expect Application
    security find-identity -v -p basic          # expect both

Both identity strings should be of the form
`Developer ID Application: <Your Name> (<TeamID>)` and
`Developer ID Installer: <Your Name> (<TeamID>)` --- the same Team ID
on both lines.  The build function looks them up by these prefixes,
not by Apple ID.

### 4. Loosen the Installer key's ACL

Out of the box the Installer private key has the default access
control list, which makes the Security Server prompt for explicit
permission every time a tool wants to use it --- and refuse outright
when the requesting process is not in the user's GUI Aqua session.
This breaks `productbuild` when called from an ssh shell, a cron
job, or via a tool that does not have access to the window server.

Open **Keychain Access**, find `Developer ID Installer: <Your Name>
(<TeamID>)` under **My Certificates** (or **Keys**), expand it, and
double-click the **private key**:

  - go to the *Access Control* tab,
  - select **Allow all applications to access this item**,
  - **Save Changes** (prompts for the login password).

The Developer ID Application key usually already has a wider ACL
because you have used `codesign` with it before; if not, do the
same.

For non-GUI sessions also widen the keychain partition list so
the Security Server accepts requests from outside the Aqua session:

    security set-key-partition-list -S apple-tool:,apple:,unsigned: -s \
        ~/Library/Keychains/login.keychain-db

(prompts for the login password).  Without `unsigned:` ssh / Claude
Code / similar will still see `errSecInteractionNotAllowed`.

### 5. Notarytool keychain profile

`notarytool` accepts an *app-specific* password (not your Apple ID
password).  Generate one at https://account.apple.com → *Sign-In and
Security* → *App-Specific Passwords* → **+**, label it `swipl-notary`.
Apple shows the password (format `abcd-efgh-ijkl-mnop`) once --- copy
it including the hyphens.

Then store it in the login keychain under a profile name:

    xcrun notarytool store-credentials swipl-notary \
        --apple-id   jan@swi-prolog.org \
        --team-id    ACDDGQR4VW \
        --password   'abcd-efgh-ijkl-mnop'

The profile name `swipl-notary` is the default expected by
`build_macos_gcc_universal()`; if you use a different name, export
`$NOTARYTOOL_PROFILE`.  Verify the profile works:

    xcrun notarytool history --keychain-profile swipl-notary
    # No submission history    --> credentials are valid


## Producing a release pkg

With the one-time setup in place:

    cd ~/src/swipl-devel
    export APPLEID=jan@swi-prolog.org      # required guard
    . scripts/make-distribution
    build_macos_gcc_universal build.fat-gcc

This runs both single-arch builds (PGO, GCC from MacPorts), merges
them via `scripts/macos-import-x86_64.sh`, reconfigures the merged
tree with the cache variables

    SWIPL_CODESIGN_IDENTITY    = Developer ID Application: ...
    SWIPL_PKGBUILD_IDENTITY    = Developer ID Installer: ...
    SWIPL_NOTARYTOOL_PROFILE   = swipl-notary

(values resolved automatically from the keychain), then runs
`ninja pkg`.  Expect 10--20 minutes wall time; notarization is the
slow step and blocks until Apple replies.

The result lands at

    build.fat-gcc/swipl-<version>-fat.pkg

(`fat` because the .pkg ships both arm64 and x86_64 slices; for
non-universal builds the suffix is the build host's architecture.)

### Keychain password handling

The build needs the login-keychain unlocked for three separate
phases (codesign during the fixup pass, productbuild after pkgbuild,
and a final codesign on the wrapped framework).  A long arch+PGO
build can exceed the keychain idle-lock timeout between unlock and
first use, and an Aqua-less session (ssh, cron, CI) cannot get a
Security Server prompt at all.

To handle both, `build_macos_gcc_universal` prompts once at the
start (`stty -echo` so nothing is echoed), exports the password as
`$KEYCHAIN_PASSWORD`, and:

  - `security unlock-keychain -p` the login keychain;
  - `security set-key-partition-list -S apple-tool:,apple:,unsigned:
    -s -k` to allow apple-signed (productbuild) and unsigned-caller
    (shell-launched) tools to use the private keys without
    confirmation;
  - exports `$KEYCHAIN_PASSWORD` so the install hook's
    `cmake -E env` propagates it down to
    `scripts/macosx_bundle_fixup.sh`, which then runs
    `security unlock-keychain -p` non-interactively if the keychain
    re-locks during the build.

Set the password ahead of time to skip the prompt entirely:

    export KEYCHAIN_PASSWORD='your-login-password'
    build_macos_gcc_universal build.fat-gcc

`scripts/macosx_bundle_fixup.sh` no longer re-locks the keychain
at the end of its codesign pass, since productbuild needs the
Installer key immediately afterwards.  The OS auto-locks on its
own idle timer.


## Verifying the result

After a successful run, four commands confirm the pkg is fully
signed, hardened-runtime, and notarized:

    PKG=build.installer/swipl-10.1.7-arm64.pkg

    # Gatekeeper verdict (what Installer.app sees)
    spctl --assess --type install -vvv "$PKG"
    # accepted     source=Notarized Developer ID

    # Productbuild signature chain
    pkgutil --check-signature "$PKG"
    # Status: signed by a developer certificate ...

    # Notarization ticket stapled (so Gatekeeper works offline)
    stapler validate -v "$PKG"
    # The validate action worked!

After installing onto a Mac, also spot-check the bundles:

    codesign --verify --deep --strict --verbose=2 \
        /Applications/swipl-win.app
    codesign --verify --deep --strict --verbose=2 \
        /Library/Frameworks/swipl.framework
    codesign -dvvv --entitlements - \
        /Applications/swipl-win.app/Contents/MacOS/swipl-win
    # expect: flags=0x10000(runtime) and the entitlements dict


## Renewing certificates

Developer ID certificates expire after five years.  Anything that
was signed and timestamped against Apple's TSA stays valid forever,
but new builds need fresh certs.  When you redo this:

  - **Apple Developer Account membership**: renew first.  Apple does
    not let you issue new certs while the membership is lapsed.
  - **Team ID**: identical across renewals --- so the cert *identity
    strings* (`Developer ID Application: Your Name (TEAMID)`) are
    byte-identical to the old ones.  No code changes needed.
  - **CSR**: easiest to generate a fresh one via Keychain Access
    Certificate Assistant.  You may also reuse the old private key
    (the matching keychain entry from the previous cert) by using
    its CSR --- but with macOS routinely changing keychain behaviour
    between releases, starting fresh is less hassle.
  - **Step 3** (issue both certs) and **Step 4** (loosen the
    Installer key ACL + partition list) must be redone.
  - **App-specific password**: probably still valid unless Apple
    revoked it during membership renewal.  Test with
    `xcrun notarytool history --keychain-profile swipl-notary` ---
    if it returns history (or "No submission history"), the
    credentials are still good.  Otherwise generate a new password
    and re-run `notarytool store-credentials`.
  - **Old keychain entries**: in Keychain Access, delete the expired
    cert and (old) private key so `security find-identity` returns
    just the new entries.

After renewal, no source changes are required --- the build function
re-discovers the active identities via `security find-identity` each
run.


## Troubleshooting

### "ninja pkg" fails with "syntax error near unexpected token `('"

CMake didn't quote the cert identity properly when emitting the
ninja command.  The custom target needs `VERBATIM`; this is the
default in current `cmake/macos_app_fixup/CMakeLists.txt` but watch
out if you fork the target.

### "Interaction is not allowed with the Security Server"

The Installer (or Application) private key has a restrictive ACL
and the calling process is outside the Aqua session.  See Step 4
above --- the partition list needs `unsigned:` and the ACL on the
private key needs to allow all applications.

### "code object is not signed at all" during framework signing

A non-Mach-O file is sitting next to the framework's main binary,
breaking codesign's bundle-signing pass.  Historically this was a
stray `swipl.home` file installed into `swipl.framework/Versions/A/`
by `src/CMakeLists.txt`; it is now gated on `NOT BUILD_MACOS_BUNDLE`.
If a new file shows up there, either move it under `Resources/` or
gate its install similarly.

### Notarization comes back "Invalid"

`xcrun notarytool log <submission-id> --keychain-profile swipl-notary`
returns JSON listing the offending paths and the reason.  Common
patterns:

  - *"The binary is not signed with a valid Developer ID certificate"*
    on a bundled third-party dylib: the bundle fixup pass picked it
    up but did not re-sign it after install-name rewriting.  Check
    the `--name` filter and the `fixup_files()` loop in
    `scripts/macosx_bundle_fixup.sh`.
  - *"The signature does not include a secure timestamp"*: the sign
    pass ran offline (Apple's TSA was unreachable).  Re-run with
    network connectivity.

### Installer.app says "not compatible with this version of macOS"

This message is misleading; it covers several distinct conditions.
The most common cause on macOS 11+ is an unsigned or unnotarized
package being rejected by Gatekeeper without an explicit signing
diagnostic --- run `spctl --assess --type install -vvv <pkg>` to
see the real verdict.

The historic `<volume-check>/<allowed-os-versions>` block in
`desktop/distribution.xml.in` could also cause this on modern macOS
with sealed system volumes; it has been removed.

### "The package is attempting to install content to the system volume"

A file in the staging tree has a path that maps to a SIP-protected
location (e.g. `/Readme.html`).  The custom target strips known
DragNDrop-only files; if you add new top-level resources in
`cmake/port/Darwin.cmake`'s deploy function, also strip them from
the pkg staging in `cmake/macos_app_fixup/CMakeLists.txt`.
