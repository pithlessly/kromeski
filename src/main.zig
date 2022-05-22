const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const LambdaHeap = struct {
    const NodeRef = enum(u32) { _ };
    const PortRef = enum(u32) { nul, _ };
    const Node = extern struct {
        ports: [3]PortRef align(16),
        kind: Kind,
        const Kind = enum(u32) { fun, app, dup };
    };

    comptime {
        assert(@sizeOf(Node) == 4 * @sizeOf(u32));
        assert(@alignOf(Node) == 4 * @alignOf(u32));
    }

    const Elem = union {
        used: Node,
        unused: UnusedRef,
        const UnusedRef = enum(u32) {
            nul = 0,
            _,
            fn new(n: usize) UnusedRef {
                assert(n != 0);
                return @intToEnum(UnusedRef, @intCast(u32, n));
            }
        };
    };

    elems: []Elem,
    next_unused: Elem.UnusedRef,
    alloc: Allocator,
    warp: std.ArrayListUnmanaged(PortRef),
    exit: std.ArrayListUnmanaged(u2),

    const initial_size = 512;

    fn init(alloc: Allocator) !LambdaHeap {
        const elems = try alloc.alloc(Elem, initial_size);
        elems[0] = undefined; // don't actually use null
        var i: u32 = 0;
        while (i < elems.len) : (i += 1)
            elems[i] = .{ .unused = Elem.UnusedRef.new(i + 1) };
        return LambdaHeap{
            .elems = elems,
            .next_unused = Elem.UnusedRef.new(1),
            .alloc = alloc,
            .warp = std.ArrayListUnmanaged(PortRef){},
            .exit = std.ArrayListUnmanaged(u2){},
        };
    }

    fn deinit(self: *LambdaHeap) void {
        self.alloc.free(self.elems);
        self.warp.deinit(self.alloc);
        self.exit.deinit(self.alloc);
    }

    fn dumpGraph(self: LambdaHeap, start: NodeRef, out: anytype) !void {
        var visited = try std.DynamicBitSet.initEmpty(self.alloc, self.elems.len);
        defer visited.deinit();
        var toVisit = std.ArrayList(NodeRef).init(self.alloc);
        try toVisit.append(start);
        defer toVisit.deinit();
        try out.writeAll("digraph {\n");
        try out.writeAll("  concentrate = true\n");
        while (toVisit.popOrNull()) |node| {
            const idx = @enumToInt(node);
            if (visited.isSet(idx)) continue;
            visited.set(idx);
            const data = self.nodePtr(node).*;
            var shape: []const u8 = "";
            if (start == node) shape = ",shape=doublecircle";
            try out.print("  s{} [label={s}{s}]\n", .{ idx, @tagName(data.kind), shape });
            for (data.ports) |port, i| {
                const arrow_style = switch (i) {
                    0 => " [arrowhead=dot]",
                    1 => " [arrowhead=none]",
                    2 => " [arrowhead=obox]",
                    else => unreachable,
                };
                if (port == .nul) {
                    try out.print("  dangling -> s{}{s}\n  dangling [style=invisible]\n", .{
                        idx,
                        arrow_style,
                    });
                    continue;
                }
                const node2 = getNode(port);
                const idx2 = @enumToInt(node2);
                try toVisit.append(node2);
                if (i != 0 and @enumToInt(getPort(node, @intCast(u2, i))) < @enumToInt(port))
                    // it would be redundant to draw these edges
                    {} // continue;
                try out.print("  s{} -> s{}{s}\n", .{ idx2, idx, arrow_style });
            }
        }
        try out.writeAll("}\n");
    }

    // precondition: the heap is already filled (there are no unused elements)
    fn expand(self: *LambdaHeap) !void {
        const old_size = self.elems.len;
        const new_size = old_size + old_size / 2 + 8;
        const new_elems = try self.alloc.reallocAtLeast(self.elems, new_size);
        for (new_elems[old_size..]) |*elem, i|
            elem.* = .{ .unused = Elem.UnusedRef.new(old_size + i + 1) };
        self.elems = new_elems;
        self.next_unused = Elem.UnusedRef.new(old_size);
    }

    fn newNode(self: *LambdaHeap, kind: Node.Kind) !NodeRef {
        if (self.next_unused == .nul)
            try self.expand();
        const idx = @enumToInt(self.next_unused);
        assert(idx != 0);
        const node_ref = @intToEnum(NodeRef, idx * 4);
        const elem = &self.elems[idx];
        self.next_unused = elem.unused;
        elem.* = .{ .used = Node{
            .ports = .{ getPort(node_ref, 0), getPort(node_ref, 1), getPort(node_ref, 2) },
            .kind = kind,
        } };
        return node_ref;
    }

    fn freeNode(self: *LambdaHeap, node: NodeRef) void {
        const idx = @divExact(@enumToInt(node), 4);
        const elem = &self.elems[idx];
        _ = elem.used; // for debug check
        elem.* = .{ .unused = self.next_unused };
        self.next_unused = Elem.UnusedRef.new(idx);
    }

    fn nodePtr(self: LambdaHeap, node: NodeRef) *Node {
        return &self.elems[@enumToInt(node) / 4].used;
    }

    fn portPtr(self: LambdaHeap, port: PortRef) *PortRef {
        const idx = @enumToInt(port);
        assert(idx % 4 < 3);
        if (std.debug.runtime_safety) {
            return &self.elems[idx / 4].used.ports[idx % 4];
        } else {
            comptime {
                assert(@sizeOf(Elem) == @sizeOf(Node));
            }
            const ports = @ptrCast([*]u32, self.elems);
            return @ptrCast(*PortRef, &ports[port]);
        }
    }

    fn getPort(node: NodeRef, idx: u2) PortRef {
        assert(idx < 3);
        return @intToEnum(PortRef, @enumToInt(node) | idx);
    }

    fn getNode(port: PortRef) NodeRef {
        return @intToEnum(NodeRef, @enumToInt(port) >> 2 << 2);
    }

    fn portSlot(ref: PortRef) u2 {
        const idx = @truncate(u2, @enumToInt(ref));
        assert(idx < 3);
        return idx;
    }

    fn link(self: LambdaHeap, port_a: PortRef, port_b: PortRef) void {
        self.portPtr(port_a).* = port_b;
        self.portPtr(port_b).* = port_a;
    }

    fn reduce(self: *LambdaHeap, start: NodeRef) !void {
        const alloc = self.alloc;
        var warp = self.warp;
        var exit = self.exit;
        assert(warp.items.len == 0);
        assert(exit.items.len == 0);
        var next = self.portPtr(getPort(start, 0)).*;
        while (next != .nul or warp.items.len > 0) {
            if (next == .nul)
                next = warp.pop();
            const prev = self.portPtr(next).*;
            if (portSlot(next) == 0) {
                if (portSlot(prev) == 0 and prev != .nul) {
                    const back = self.portPtr(getPort(getNode(prev), exit.pop())).*;
                    try self.rewrite(getNode(prev), getNode(next));
                    next = self.portPtr(back).*;
                } else {
                    try warp.append(alloc, getPort(getNode(next), 2));
                    next = self.portPtr(getPort(getNode(next), 1)).*;
                }
            } else {
                try exit.append(alloc, portSlot(next));
                next = self.portPtr(getPort(getNode(next), 0)).*;
            }
        }
        assert(self.exit.items.len == 0);
        self.warp = warp;
        self.exit = exit;
    }

    fn rewrite(self: *LambdaHeap, x: NodeRef, y: NodeRef) !void {
        const xk = self.nodePtr(x).kind;
        const yk = self.nodePtr(y).kind;
        if (xk == yk) {
            self.link(self.portPtr(getPort(x, 1)).*, self.portPtr(getPort(y, 1)).*);
            self.link(self.portPtr(getPort(x, 2)).*, self.portPtr(getPort(y, 2)).*);
            self.freeNode(x);
            self.freeNode(y);
        } else {
            const a = try self.newNode(xk);
            const b = try self.newNode(yk);
            self.link(getPort(b, 0), self.portPtr(getPort(x, 1)).*);
            self.link(getPort(y, 0), self.portPtr(getPort(x, 2)).*);
            self.link(getPort(a, 0), self.portPtr(getPort(y, 1)).*);
            self.link(getPort(x, 0), self.portPtr(getPort(y, 2)).*);
            self.link(getPort(a, 1), getPort(b, 1));
            self.link(getPort(a, 2), getPort(y, 1));
            self.link(getPort(x, 1), getPort(b, 2));
            self.link(getPort(x, 2), getPort(y, 2));
        }
    }

    const Combinator = enum {
        s,
        k,
        zero,
        i,
        succ,
    };

    fn createCombinator(self: *LambdaHeap, comptime comb: Combinator) !PortRef {
        switch (comb) {
            .i => {
                // the I combinator consists of a single node arranged like this:
                //  x aa aa 1
                const fun = try self.newNode(.fun);
                self.link(getPort(fun, 1), getPort(fun, 2));
                return getPort(fun, 0);
            },
            .k => {
                // the K combinator consists of three nodes arranged like this:
                //  x bb aa 1  (fun1)
                // aa cc bb 1  (fun2)
                // cc dd dd 0  (discard variable of fun2)
                const fun1 = try self.newNode(.fun);
                const fun2 = try self.newNode(.fun);
                const discard = try self.newNode(.app);
                self.link(getPort(fun1, 1), getPort(fun2, 2));
                self.link(getPort(fun1, 2), getPort(fun2, 0));
                self.link(getPort(fun2, 1), getPort(discard, 0));
                self.link(getPort(discard, 1), getPort(discard, 2));
                return getPort(fun1, 0);
            },
            .zero => {
                // the K combinator consists of three nodes aranged like this:
                //  x bb aa 1  (fun1)
                // bb cc cc 0  (discard variable of fun1)
                // aa dd dd 1  (fun2)
            },
            .s => {
                // the S combinator consists of seven nodes arranged like this:
                //     x aa bb 1 (fun1: param = aa)
                //    bb cc dd 1 (fun2: param = cc)
                //    dd ee hh 1 (fun3: param = ee)
                //    ee ff gg 2 (dup param of fun3 -> f and g)
                //    jj ii hh 1 (app1: apply app2 to app3)
                //    aa gg jj 1 (app2: apply fun1 param to copy of fun3 param)
                //    cc ff ii 1 (app3: apply fun2 param to copy of fun3 param)
                const fun1 = try self.newNode(.fun);
                const fun2 = try self.newNode(.fun);
                const fun3 = try self.newNode(.fun);
                const dup3 = try self.newNode(.dup);
                const app1 = try self.newNode(.app);
                const app2 = try self.newNode(.app);
                const app3 = try self.newNode(.app);

                self.link(getPort(fun1, 1), getPort(app2, 0)); // aa
                self.link(getPort(fun1, 2), getPort(fun2, 0)); // bb
                self.link(getPort(fun2, 1), getPort(app3, 0)); // cc
                self.link(getPort(fun2, 2), getPort(fun3, 0)); // dd
                self.link(getPort(fun3, 1), getPort(dup3, 0)); // ee
                self.link(getPort(dup3, 1), getPort(app3, 1)); // ff
                self.link(getPort(dup3, 2), getPort(app2, 1)); // gg
                self.link(getPort(fun3, 2), getPort(app1, 2)); // hh
                self.link(getPort(app1, 1), getPort(app3, 2)); // ii
                self.link(getPort(app1, 0), getPort(app2, 2)); // jj

                return getPort(fun1, 0);
            },
            .succ => {
                // the SUCC combinator consists of seven nodes arranged like this:
                //     x aa bb 1 (fun1: param = aa)
                //    bb cc dd 1 (fun2: param = cc)
                //    dd ee ff 1 (fun3: param = ee)
                //    cc gg hh 2 (dup param of fun2 -> g and h)
                //    gg ii ff 1 (app1: apply copy of fun2 param to app2)
                //    jj ee ii 1 (app2: apply app3 to fun3 param)
                //    aa hh jj 1 (app3: apply fun1 param to copy of fun2 param)
                const fun1 = try self.newNode(.fun);
                const fun2 = try self.newNode(.fun);
                const fun3 = try self.newNode(.fun);
                const dup2 = try self.newNode(.dup);
                const app1 = try self.newNode(.app);
                const app2 = try self.newNode(.app);
                const app3 = try self.newNode(.app);

                self.link(getPort(fun1, 1), getPort(app3, 0)); // aa
                self.link(getPort(fun1, 2), getPort(fun2, 0)); // bb
                self.link(getPort(fun2, 1), getPort(dup2, 0)); // cc
                self.link(getPort(fun2, 2), getPort(fun3, 0)); // dd
                self.link(getPort(fun3, 1), getPort(app2, 1)); // ee
                self.link(getPort(fun3, 2), getPort(app1, 2)); // ff
                self.link(getPort(dup2, 1), getPort(app1, 0)); // gg
                self.link(getPort(dup2, 2), getPort(app3, 1)); // hh
                self.link(getPort(app1, 1), getPort(app2, 2)); // ii
                self.link(getPort(app2, 0), getPort(app3, 2)); // jj

                return getPort(fun1, 0);
            }
        }
    }
};

const n_combinators = 3;

const CLTerm = enum(u32) { none = 0, s = 1, k = 2, i = 3, _ };
const CLDict = struct {
    const term_offset = n_combinators + 1;
    const Application = struct {
        better_equivalent: CLTerm,
        lc_equivalent: LambdaHeap.NodeRef,
    };
    const ApplyCache = std.AutoArrayHashMap(struct { func: CLTerm, arg: CLTerm }, CLTerm);
    cells: std.ArrayList(Application),
    apply_cache: ApplyCache,

    pub fn init(alloc: Allocator) CLDict {
        return .{
            .cells = std.ArrayList(Application).init(alloc),
            .apply_cache = ApplyCache.init(alloc),
        };
    }

    pub fn deinit(self: *CLDict) void {
        self.cells.deinit();
        self.apply_cache.deinit();
    }

    // pub fn makeApplyCell(self: *CLDict) !CLTerm {
    //     const cells = &self.cells;
    //     const lc = @intToEnum(LambdaTerm, 0); // TODO actually compute this
    //     const term = @intToEnum(CLTerm, cells.items.len + term_offset);
    //     try cells.append(.{ .better_equivalent = .none, .lc_equivalent = lc });
    //     return term;
    // }
};

// this can return none if this application has been removed from the cache.
// fn clApply(dict: *CLDict, func: CLTerm, arg: CLTerm) !CLTerm {
//     assert(func != .none and arg != .none);
//     assert(func != .i); // don't even bother
//     const entry = try dict.apply_cache.getOrPut(.{ .func = func, .arg = arg });
//     if (entry.found_existing)
//         return entry.value_ptr.*
//     else {
//         const term = try dict.makeApplyCell();
//         entry.value_ptr.* = term;
//         return term;
//     }
// }

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var heap = try LambdaHeap.init(alloc);
    defer heap.deinit();
    const combo = try heap.createCombinator(.succ);
    heap.portPtr(combo).* = .nul;
    try heap.dumpGraph(LambdaHeap.getNode(combo), std.io.getStdOut().writer());

    var dict = CLDict.init(alloc);
    defer dict.deinit();
    // std.debug.print("{}\n", .{std.meta.trait.hasUniqueRepresentation(struct { func: CLTerm, arg: CLTerm })});
}

test "compilation" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(CLDict);
    std.testing.refAllDecls(LambdaHeap);
    std.testing.refAllDecls(LambdaHeap.Node);
    std.testing.refAllDecls(LambdaHeap.Elem);
    assert(@sizeOf(LambdaHeap.Node) == 4 * @sizeOf(u32));
}
