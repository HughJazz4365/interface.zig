//TODO: better error messages
const std = @import("std");

pub const ptr_field_name = "_";
pub const self_ptr_field: StructField = .{
    .name = ptr_field_name,
    .type = *anyopaque,
    .default_value_ptr = emptyVoidPtr(*anyopaque),
    .is_comptime = false,
    .alignment = @alignOf(*anyopaque),
};

///returns a type with a single implement function and an Implementation struct
///Implementation struct returned from implement function contains:
///1. pointers to all of the fields of the given struct described in the interface
///2. pointers to wrapper functions of given struct`s functions described in the interface
///3. void pointer to the passed struct(a field named '_')
///Example:
///const impl = SomeInterface.implement(some_struct);
///impl.doStuff(impl._,.{arg1, arg2});
///- is equivalent to:
///some_struct.doStuff(arg1, arg2);
pub fn Interface(comptime interface: anytype) type {
    compAssert(@typeInfo(@TypeOf(interface)) == .@"struct", "interface must be a struct");
    const fields, const functions = extractFieldsFunctions(interface);
    return struct {
        pub fn implement(source: anytype) Implementation {
            const T, const is_const = getSourceType(@TypeOf(source));

            var result: Implementation = .{};
            @field(result, ptr_field_name) = @as(*anyopaque, @ptrCast(@alignCast(@constCast(source))));

            matchFields(
                Implementation,
                &result,
                fields,
                T,
                source,
            );
            matchFuncs(
                Implementation,
                &result,
                functions,
                T,
                is_const,
            );
            return result;
        }

        pub const Implementation = ImplementationType(fields, functions);
    };
}

pub fn ImplementationType(comptime fields: []const NT, comptime functions: []const Func) type {
    var struct_fields: []const StructField = &.{self_ptr_field};
    for (functions) |function| {
        struct_fields = struct_fields ++ &[_]StructField{function.toStructField()};
    }
    for (fields) |field| {
        struct_fields = struct_fields ++ &[_]StructField{NTtoStructField(field)};
    }
    return @Type(.{ .@"struct" = .{
        .layout = .auto,
        .fields = struct_fields,
        .decls = &.{},
        .is_tuple = false,
    } });
}

//assigns all the field pointers to the implementation struct fields
pub fn matchFields(I: type, impl_ptr: *I, fields: []const NT, T: type, source: *const T) void {
    const struct_fields = @typeInfo(T).@"struct".fields;
    for (fields) |i_field| {
        const full_field_name = @typeName(T) ++ "." ++ i_field[0];
        for (struct_fields) |s_field| {
            if (!sliceEql(u8, i_field[0], s_field.name)) continue;
            if (s_field.type == i_field[1]) break;
        } else @compileError("missing field " ++ full_field_name);
        @field(impl_ptr, i_field[0]) = @field(source, i_field[0]);
    }
}

//assigns all the function pointers to the implementation struct fields
pub fn matchFuncs(I: type, impl_ptr: *I, comptime functions: []const Func, T: type, comptime is_const: bool) void {
    const decls = @typeInfo(T).@"struct".decls;
    inline for (functions) |i_field| {
        const full_function_name = @typeName(T) ++ "." ++ i_field.name;
        inline for (decls) |decl| {
            if (comptime !sliceEql(u8, i_field.name, decl.name)) continue;
            const F = @TypeOf(@field(T, i_field.name));
            const self_type, const can_error = (comptime i_field.matchToFn(F, T)) catch |err|
                switch (err) {
                    error.Rtype => @compileError("return type mismatch in " ++ full_function_name),
                    error.Args => @compileError("argument type mismatch in " ++ full_function_name),
                    error.ErrorType => @compileError("error mismatch in " ++ full_function_name),
                };
            if (self_type == .ptr and is_const) @compileError("function " ++ full_function_name ++ " requires mutable pointer, but source is immutable");
            @field(impl_ptr, i_field.name) = &(i_field.getFunctionPtr(self_type, T, can_error).f);
            break;
        } else @compileError("missing function " ++ full_function_name);
    }
}

//extract all the information about interface fields and functions
pub fn extractFieldsFunctions(comptime interface: anytype) Tuple(&.{ []const NT, []const Func }) {
    var fields: []const NT = &.{};
    var functions: []const Func = &.{};

    for (@typeInfo(@TypeOf(interface)).@"struct".fields) |f| {
        const interface_field_type = @field(interface, f.name);
        compAssert(@TypeOf(interface_field_type) == type, "interface field can only be a type");
        switch (@typeInfo(interface_field_type)) {
            .@"fn" => |func| functions = functions ++ &[_]Func{.fromFn(f.name, func)},
            else => fields = fields ++ &[_]NT{.{ f.name, interface_field_type }},
        }
    }

    return .{ fields, functions };
}

pub fn getSourceType(comptime T: type) Tuple(&.{ type, bool }) {
    const tinfo = @typeInfo(T);
    const wrong_type_msg = "source must be a pointer to a struct";
    switch (tinfo) {
        .pointer => |ptr| {
            const ChildT = ptr.child;
            compAssert(@typeInfo(ChildT) == .@"struct", wrong_type_msg);
            return .{ ChildT, ptr.is_const };
        },
        else => @compileError(wrong_type_msg),
    }
}
pub const Func = struct {
    name: Str,
    rtype: type,
    args: []const type,
    pub fn fromFn(comptime name: Str, comptime f: Type.Fn) Func {
        return .{
            .name = name,
            .rtype = f.return_type orelse void,
            .args = typesFromParams(f.params),
        };
    }
    pub fn toFn(comptime self: Func) type {
        return fn (*anyopaque, Tuple(self.args)) self.rtype;
    }
    pub fn toStructField(comptime self: Func) StructField {
        const Fptr = *const self.toFn();
        return .{
            .name = self.name,
            .type = Fptr,
            .default_value_ptr = emptyVoidPtr(Fptr),
            .is_comptime = false,
            .alignment = @alignOf(Fptr),
        };
    }
    ///T - the type F belongs to(for checking methods)
    pub fn matchToFn(self: Func, F: type, T: type) error{ Rtype, Args, ErrorType }!Tuple(&.{ SelfType, bool }) {
        const new_func = fromFn(self.name, @typeInfo(F).@"fn");

        //get an error union
        const comp_result = try compareReturnTypesCheckErrors(self.rtype, new_func.rtype);
        if (!comp_result.rtypes_match) return error.Rtype; //return null;
        if (new_func.args.len == 0 and self.args.len == 0) return .none;

        var new_args = new_func.args;
        const first = new_args[0];
        const self_type: SelfType = blk: {
            if (first == T) break :blk .value;
            break :blk switch (@typeInfo(first)) {
                .pointer => |p| if (p.child == T) (if (p.is_const) .constptr else .ptr) else .none,
                else => .none,
            };
        };
        if (self_type != .none) new_args = new_args[1..];
        return .{ if (sliceEql(type, new_args, self.args)) self_type else return error.Args, comp_result.can_error };
    }

    fn getFunctionPtr(comptime self: Func, comptime self_type: SelfType, comptime T: type, comptime can_error: bool) type {
        const tuple = Tuple(self.args);
        //if self type is none function doest need to accept void_ptr as argument
        //but that could break everything since that depends on source type
        return struct {
            pub fn f(void_ptr: *anyopaque, args: tuple) self.rtype {
                switch (self_type) {
                    .none => {
                        if (can_error) {
                            try @call(.auto, @field(T, self.name), args);
                        } else @call(.auto, @field(T, self.name), args);
                    },
                    else => {
                        const t_ptr: *T = @ptrCast(@alignCast(void_ptr));
                        var new_args: Tuple(&[_]type{if (self_type == .value) T else *T} ++ self.args) = undefined;
                        new_args[0] = if (self_type == .value) t_ptr.* else t_ptr;
                        inline for (0..self.args.len) |i| new_args[i + 1] = args[i];
                        if (can_error) {
                            try @call(.auto, @field(T, self.name), new_args);
                        } else @call(.auto, @field(T, self.name), new_args);
                    }
                }
            }
        };
    }
};
///separates error sets and payload types
///checks if interface field return type`s error set is a superset of source function return type`s error set
///and compares payload types
fn compareReturnTypesCheckErrors(interface_rtype: type, source_rtype: type) error{ErrorType}!struct { rtypes_match: bool, can_error: bool } {
    const interface_error_union: ?Type.ErrorUnion = switch (@typeInfo(interface_rtype)) {
        .error_union => |e| e,
        else => null,
    };
    const interface_rtype_payload = if (interface_error_union) |e| e.payload else interface_rtype;
    const source_rtype_payload: type, const can_error: bool = sep: {
        switch (@typeInfo(source_rtype)) {
            .error_union => |e| {
                //error if interface func cant return an error but source function can
                if (interface_error_union) |se| {
                    if (isErrorSuperset(se.error_set, e.error_set)) {
                        break :sep .{ e.payload, true };
                    } else return error.ErrorType;
                } else return error.ErrorType;
            },
            else => break :sep .{ source_rtype, false },
        }
    };
    return .{
        .rtypes_match = interface_rtype_payload == source_rtype_payload,
        .can_error = can_error,
    };
}
fn isErrorSuperset(comptime Superset: type, comptime Subset: type) bool {
    const superset = @typeInfo(Superset).error_set orelse return true;
    const subset = @typeInfo(Subset).error_set orelse return false;
    inline for (subset) |err| {
        inline for (superset) |s_err| {
            if (sliceEql(u8, err.name, s_err.name)) break;
        } else return false;
    }
    return true;
}
const SelfType = enum { none, value, constptr, ptr };
pub fn NTtoStructField(comptime nt: NT) StructField {
    return .{
        .name = nt[0],
        .type = nt[1],
        .default_value_ptr = emptyVoidPtr(nt[1]),
        .is_comptime = false,
        .alignment = @alignOf(nt[1]),
    };
}
pub const NT = Tuple(&.{ Str, type });

//zig utils
fn typesFromParams(params: []const Type.Fn.Param) []const type {
    var args: []const type = &.{}; //&.{anytype}
    for (params) |param| {
        args = args ++ &[_]type{param.type orelse void};
    }
    return args;
}
pub fn compAssert(comptime condition: bool, comptime msg: Str) void {
    if (!condition) @compileError(msg);
}
fn sliceEql(comptime T: type, a: []const T, b: []const T) bool {
    return a.len == b.len and std.mem.eql(T, a, b);
}
fn emptyVoidPtr(comptime T: type) *const anyopaque {
    const t: T = undefined;
    return @ptrCast(&t);
}

const Type = std.builtin.Type;
const StructField = Type.StructField;
const Str = [:0]const u8;
const Tuple = std.meta.Tuple;
