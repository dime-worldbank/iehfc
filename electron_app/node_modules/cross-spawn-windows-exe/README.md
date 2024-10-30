# `cross-spawn-windows-exe`

> Cross-platform support for running Windows executables via Node.js.

[![CI](https://github.com/malept/cross-spawn-windows-exe/workflows/CI/badge.svg)](https://github.com/malept/cross-spawn-windows-exe/actions?query=workflow%3ACI)
[![codecov](https://codecov.io/gh/malept/cross-spawn-windows-exe/branch/main/graph/badge.svg)](https://codecov.io/gh/malept/cross-spawn-windows-exe)

Utilizes [`@malept/cross-spawn-promise`](https://npm.im/@malept/cross-spawn-promise) (and by
extension, [`cross-spawn`](https://npm.im/cross-spawn)) to execute Windows executables regardless
of platform.

For all platforms, Node 10 or above is required.

On non-Windows, non-WSL host systems, the following dependencies are required:

- .NET executables: [Mono](https://www.mono-project.com/)
- All other Windows executables: [Wine](https://www.winehq.org/)

## Usage

Using the `cross-spawn-windows-exe` API is similar in terms of function signature to `spawn` in
`@malept/cross-spawn-promise`.

### Running a .NET executable

```javascript
// Note: top-level await exists in Node >= 14.8.0. In earlier versions of Node, please wrap in an
// async function.

const { spawnDotNet } = require("cross-spawn-windows-exe");

await spawnDotNet("./hellodotnet.exe", ["--arg1"]);
```

### Running a Windows executable

```javascript
// Note: top-level await exists in Node >= 14.8.0. In earlier versions of Node, please wrap in an
// async function.

const { spawnExe } = require("cross-spawn-windows-exe");

await spawnExe("./hellowindows.exe", ["--arg1"]);
```

### Normalizing Paths

Executables generally can't handle UNIX-style paths that Windows Subsystem for Linux (WSL) passes
in, since it's a Linux environment. This module provides a function to convert those paths from
UNIX-style to Windows-style (via `wslpath`, which should be installed by default on every WSL
distribution), if the host system is determined to be WSL. For non-WSL environments, this is a
no-op.

```javascript
// Note: top-level await exists in Node >= 14.8.0. In earlier versions of Node, please wrap in an
// async function.

const { normalizePath, spawnExe } = require("cross-spawn-windows-exe");

const normalizedPath = await normalizePath("/tmp/foo");
await spawnExe("./openfile.exe", ["--filename", normalizedPath]);
```

## Legal

This module is licensed under the [Apache License, version 2.0](https://www.apache.org/licenses/LICENSE-2.0).
See `LICENSE` for details.
