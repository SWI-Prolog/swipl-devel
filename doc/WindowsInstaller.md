# Build Windows installer using GitHub Actions

## Automated

Push a release tag (V*)

## By hand

    gh workflow run release-windows.yml -f ref=master
    gh run watch
    gh run download

Downloads ``swipl-<version>-1.x64.exe{,sha256}``
