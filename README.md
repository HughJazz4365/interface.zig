add a dependency to zig.zon:
```
zig fetch --save git+https://github.com/HughJazz4365/interface.zig
```
add to your build.zig file:
```zig
const exe_mod = ...
exe_mod.addImport(
    "interface",
    b.dependency("interface",.{
        .target = target,
        .optimize = optimize,
    }).module("interface"),
);
```
import:
```zig
const Interface = @import("interface").Interface;
```
see the example for more details
