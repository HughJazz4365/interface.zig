const std = @import("std");
const Interface = @import("interface").Interface;

pub fn main() !void {
    //has to be a mutable pointer
    var count_printer: CountPrinter = .{};
    const count_impl = Printer.implement(&count_printer);

    //doesnt have to be a mutable pointer
    const multi_printer: MultiPrinter = .{ .num = 4 };
    const multi_impl = Printer.implement(&multi_printer);

    try print(count_impl);
    try print(multi_impl);
    try print(count_impl);
}
//you can accept interface Implementation types as function args
fn print(printer: Printer.Implementation) !void {
    try printer.print(printer._, .{"message"});
}
//will print the message and the number of times it printed so far
const CountPrinter = struct {
    counter: u32 = 0,
    pub fn print(self: *CountPrinter, msg: []const u8) error{TooMuch}!void {
        if (self.counter > 0) return error.TooMuch;
        defer self.counter += 1;
        std.debug.print("msg: {s}| times printed so far: {}\n", .{ msg, self.counter });
    }
};
//will print the message 'num' times
const MultiPrinter = struct {
    num: u32 = 3,
    pub fn print(self: MultiPrinter, msg: []const u8) void {
        for (0..self.num) |i| {
            std.debug.print("{}:msg: {s}\n", .{ i, msg });
        }
    }
};
//interface that describes the print function
const Printer = Interface(.{
    .print = fn ([]const u8) error{ TooMuch, NotEnough }!void,
});
