const std = @import("std");
// const BitReader = std.io.BitReader(.little, std.fs.File.Reader);
// const BitWriter = std.io.BitWriter(.little, std.fs.File.Writer);
// const Reader = std.io.Reader();
const ArrayList = std.ArrayList;

const Word = union(enum) {
    pure: PureWord,
    parent: ParentWord,
    source: SourceWord,
};

const PureWord = struct {
    name: u8,
    prob:f64,
};

pub fn BitWriter(comptime endian: std.builtin.Endian) type {
    return struct {
        const Self = @This();
        writer: *std.Io.Writer,
        buf: u8 = 0,
        bits: u4 = 0, // number of valid bits currently in `buf` (0..8)
        pub fn init(writer: *std.Io.Writer) Self {
            return .{ .writer = writer };
        }
        /// Writes the low `num` bits of `value` to the stream.
        pub fn writeBits(self: *Self, value: anytype, num: u16) !void {
            if (num == 0) return;
            const value_size = @max(@bitSizeOf(@TypeOf(value)), 8);
            const UValue = @Int(.unsigned, @max(@bitSizeOf(@TypeOf(value)), 8));
            const Log2 = std.math.Log2Int(UValue);
            var v: UValue = @intCast(value);
            var remaining: u16 = num;
            while (remaining > 0) {
            // if (num == 64) {
            // std.debug.print("remaining: {any}, v: {any}\n", .{remaining, v});
            // }
                const space: u4 = 8 - self.bits;
                const take: u4 = @intCast(@min(@as(u16, space), remaining));
                if (take == 8) {
                    // the special case of take==8, (fill an entire byte) is the same regardless of endianness, endianness changes where the
                    // bits are placed in self.buf, but if we're filling the whole buffer anyways it makes no difference.
                    // Put 111 into the buffer: .little: 0000111 , .big: 1110000; Put 10101010: .little: 10101010, .big: 10101010
                    const chunk: u8 = @truncate(v);
                    self.buf = chunk;
                    self.bits = 8;
                // if (num == 64) {
                // std.debug.print("self.bits: {any}, self.buf: {any}\n", .{self.bits, self.buf});
                // }
                    try self.flushByte();
                    remaining -= 8;
                    if (remaining == 0) {
                        break;
                    } else {
                        comptime var take_log2: Log2 = @intCast(0);
                        comptime if (value_size > 8) {
                            take_log2 = @intCast(8);
                        } else {
                            take_log2 = @intCast(0);
                        };
                        if (take_log2 == 0) {
                            unreachable;
                        } else {
                        v >>= take_log2;
                        }
                        continue;
                    }
                }
                const take_log2: Log2 = @intCast(take);
                switch (endian) {
                    .little => {
                        const mask: UValue = (@as(UValue, 1) << take_log2) - 1;
                        const chunk: u8 = @truncate(v & mask);
                        self.buf |= chunk << @intCast(self.bits);
                        v >>= take_log2;
                    },
                    .big => {
                        const shift: Log2 = @intCast(remaining - take);
                        const mask: UValue = (@as(UValue, 1) << take_log2) - 1;
                        const chunk: u8 = @truncate((v >> shift) & mask);
                        self.buf |= chunk << @intCast(space - take);
                    },
                }
                self.bits += take;
                remaining -= take;
                if (self.bits == 8) try self.flushByte();
            }
        }
        /// Convenience for writing a single bit (e.g. one step of a Huffman code).
        pub fn writeBit(self: *Self, bit: bool) !void {
            try self.writeBits(@as(u1, @intFromBool(bit)), 1);
        }
        fn flushByte(self: *Self) !void {
            try self.writer.writeByte(self.buf);
            self.buf = 0;
            self.bits = 0;
        }
        /// Zero-pads and flushes any partial byte still buffered. Call this
        /// once at the very end (equivalent to the old `flushBits`).
        pub fn flushBits(self: *Self) !void {
            if (self.bits != 0) try self.flushByte();
        }
    };
}

pub fn BitReader(comptime endian: std.builtin.Endian) type {
    return struct {
        const Self = @This();
        reader: *std.Io.Reader,
        buf: u8 = 0,
        bits: u4 = 0, // number of valid bits currently in `buf` (0..8)
        pub fn init(reader: *std.Io.Reader) !Self {
            return .{ .reader = reader};
        }
        pub fn readBits(self: *Self, comptime T: type, num: u16) !T {
            var value: T = 0;
            if (num == 0) return value;
            const Log2 = std.math.Log2Int(u8);
            const Log2Val = std.math.Log2Int(T);
            var remaining: u16 = num;
            while (remaining > 0) {
                if (self.bits == 0) try self.loadByte();
                const available = self.bits;
                const take: u4 = @intCast(@min(@as(i64, remaining), available));
                if (take == 8) {
                    const app_val: T = @intCast(self.buf);
                    const offset:Log2Val = @intCast(num - remaining);
                    value |= app_val << offset;
                    self.bits = 0;
                    self.buf = 0;
                    remaining -= 8;
                    continue;
                }
                const take_log2: Log2 = @intCast(take);
                const offset:Log2Val = @intCast(num - remaining);
                switch (endian) {
                    .little => {
                        const mask: u8 = (@as(u8, 1) << take_log2) - 1;
                        const app_val: T = @intCast(self.buf & mask);
                        value |= app_val << offset;
                        self.buf = self.buf >> take_log2;
                    },
                    .big => {
                        const mask: u8 = (@as(u8, 1) << take_log2) - 1;
                        const app_val: T = @intCast((self.buf >> take_log2) & mask);
                        value |= app_val << offset;
                        self.buf = self.buf >> take_log2;
                    },
                }
                self.bits -= take;
                remaining -= take;
            }
            return value;
        }
        fn loadByte(self: *Self) !void {
            self.buf = try self.reader.takeByte();
            self.bits = 8;
        }
    };
}

pub const CodingError = error{
    CodeTooLong
};

pub fn recursePure(self: *const PureWord, words_done: *[256]bool, codes: *[256][32]bool, code_length: *[256]u32,
    running_code: *[32]bool, running_code_length: u32) CodingError!void {
    if (running_code_length > 32) {
        return CodingError.CodeTooLong;
    }
    words_done[self.name] = true;
    for (0..running_code_length) |item| {
        codes[self.name][item] = running_code[item];
    }
    code_length[self.name] = running_code_length;
}

const ParentWord = struct {
    child0: *const Word,
    child1: *const Word,
    name: u8
};

pub fn recurseParent(self: *const ParentWord, words_done: *[256]bool, codes: *[256][32]bool, code_length: *[256]u32,
    running_code: *[32]bool, running_code_length: u32) CodingError!void {
    var code0 = std.mem.zeroes([32]bool);
    var code0_length: u32 = running_code_length;
    for (0..running_code_length) |item| {
        code0[item] = running_code[item];
    }
    if (running_code_length == 32) {
        return CodingError.CodeTooLong;
    }
    code0[running_code_length] = false;
    code0_length += 1;
    try recurseWord(self.child0, words_done, codes, code_length, &code0, code0_length);
    var code1 = std.mem.zeroes([32]bool);
    var code1_length: u32 = running_code_length;
    for (0..running_code_length) |item| {
        code1[item] = running_code[item];
    }
    code1[running_code_length] = true;
    code1_length += 1;
    try recurseWord(self.child1, words_done, codes, code_length, &code1, code1_length);
}

const SourceWord = struct {
    child0: *const Word,
    child1: *const Word,
    name: u8
};

pub fn recurseSource(self: *const SourceWord, words_done: *[256]bool, codes: *[256][32]bool, code_length: *[256]u32,
    running_code: *[32]bool, running_code_length: u32) CodingError!void {
    var code0 = std.mem.zeroes([32]bool);
    var code0_length: u32 = running_code_length;
    for (0..running_code_length) |item| {
        code0[item] = running_code[item];
    }
    code0[running_code_length] = false;
    code0_length += 1;
    try recurseWord(self.child0, words_done, codes, code_length, &code0, code0_length);
    var code1 = std.mem.zeroes([32]bool);
    var code1_length: u32 = running_code_length;
    
    for (0..running_code_length) |item| {
        code1[item] = running_code[item];
    }
    code1[running_code_length] = true;
    code1_length += 1;
    try recurseWord(self.child1, words_done, codes, code_length, &code1, code1_length);
}

pub fn recurseWord(word: *const Word, words_done: *[256]bool, codes: *[256][32]bool, code_length: *[256]u32,
    running_code: *[32]bool, running_code_length: u32) !void {
    const actualWord = word.*;
    switch (actualWord) {
        .pure => |w| try recursePure(&w, words_done, codes, code_length, running_code, running_code_length),
        .parent => |w| try recurseParent(&w, words_done, codes, code_length, running_code, running_code_length),
        .source => |w| try recurseSource(&w, words_done, codes, code_length, running_code, running_code_length),
    }
}

const NoneFoundError = error{NoneFound};

fn findMin(arr: []f64) !usize {
    const size = arr.len;
    var curr:f64 = 100;
    var curr_ind:usize = 0;
    var index:usize = 0;
    var elem:f64 = curr;
    while (index < size) : (index += 1) {
        elem = arr[index];
        if (elem < curr and elem > 0) {
            curr = elem;
            curr_ind = index;
        }
    }
    if (curr == 100) {
        return NoneFoundError.NoneFound;
    }
    return curr_ind;
}

fn constructHuffman(probs:[256]f64, alloc: anytype) !*const SourceWord {
    var wordStructs: ArrayList(*Word) = .empty;
    var wordProbs: ArrayList(f64) = .empty;
    var nchars: u64 = 0;
    for (0..256, probs) |char, prob| {
        if (prob > 0.0) {
            const pureWord = try alloc.create(PureWord);
            const char_u8: u8 = @intCast(char);
            pureWord.* = .{.name = char_u8, .prob = prob};
            const resultWord = try alloc.create(Word);
            resultWord.* = .{.pure = pureWord.*};
            try wordStructs.append(alloc, resultWord);
            nchars += 1;
            try wordProbs.append(alloc, prob);
        }
    }
    var nwords: usize = nchars;
    while (nwords > 2) {
        const minInd = try findMin(wordProbs.items);
        const minWord = wordStructs.orderedRemove(minInd);
        const minProb = wordProbs.items[minInd];
        _ = wordProbs.orderedRemove(minInd);
        // wordProbs.items[minInd] = -1;
        const minInd2 = try findMin(wordProbs.items);
        const minWord2 = wordStructs.orderedRemove(minInd2);
        const minProb2 = wordProbs.items[minInd2];
        _ = wordProbs.orderedRemove(minInd2);
        // wordProbs.items[minInd2] = -1;
        const parentWord = try alloc.create(ParentWord);
        parentWord.* = .{.child0 = &minWord.*, .child1 = &minWord2.*, .name = 0};
        const resultWord = try alloc.create(Word);
        resultWord.* = .{.parent = parentWord.*};
        try wordStructs.append(alloc,resultWord);
        try wordProbs.append(alloc, minProb + minProb2);
        nwords -= 1;
    }
    const minInd = try findMin(wordProbs.items);
    const minWord = wordStructs.orderedRemove(minInd);
    _ = wordProbs.orderedRemove(minInd);
    const minInd2 = try findMin(wordProbs.items);
    const minWord2 = wordStructs.orderedRemove(minInd2);
    wordProbs.items[minInd2] = -1;
    const source = try alloc.create(SourceWord);
    source.* = .{.child0 = &minWord.*, .child1 = &minWord2.*, .name = 1};
    return source;
}
const NotFoundError = error{NotFound};
const TooSmallError = error{TooSmall};

fn appendChars(buf_writer:anytype, chars:[32]bool, length: u32) !void {
    for (0..length) |item| {
        const char = chars[item];
        try buf_writer.writeBits(@as(u1, @intFromBool(char)), 1);
    }
    return;
}

fn appendCharCount(buf_writer:anytype, char:u8, count:u64) !void {
    try buf_writer.writeBits(char, 8);
    try buf_writer.writeBits(count, 64);
}

fn writeHeader(counts:[256]u64, fileLength:u64, buf_writer:anytype) !void {
    // Length of file
    try buf_writer.writeBits(@as(u64, fileLength), 64);
    var nchars: u64 = 0;
    for (counts) |count| {
        if (count > 0) nchars += 1;
    }
    // Number of characters (Unnecessary?):
    try buf_writer.writeBits(@as(u64, nchars), 64);
    // Character frequencies:
    for (0.., counts) |word, count| {
        if (count > 0) {
            const char_u8:u8 = @intCast(word);
            try appendCharCount(buf_writer, char_u8, count);
        }
    }
    // for (words.items, counts.items) |word, count| {
    //     try appendCharCount(buf_writer, word, count);
    // }
}

fn encode(reader: anytype, codes:[256][32]bool, code_length:[256]u32,
    buf_writer:anytype) !void {
    // var buffer: [4096]u8 = undefined;
    while (true) {
        const chunk = reader.take(1) catch |err| switch (err) {
            error.EndOfStream => break,
            error.ReadFailed => |e| return e,
        };
        for (chunk) |char| {
            const code = codes[char];
            const length = code_length[char];
            try appendChars(buf_writer, code, length);
        }
    }
    return;
}

fn readHeader(bit_reader:anytype, counts:*[256]u64) !u64 {
    const fileLength = try bit_reader.*.readBits(u64, 64);
    const num_words = try bit_reader.*.readBits(u64, 64);
    for (0..num_words) |_| {
        const char = try bit_reader.*.readBits(u8, 8);
        const count = try bit_reader.*.readBits(u64, 64);
        counts[char] = count;
    }
    return fileLength;
}

fn delvePure(word: *const PureWord) u8 {
    return word.name;
}

fn delveParent(word: *const ParentWord, value_raw: u1, bit_reader: anytype) !u8 {
    if (value_raw == 0) {
        const new_word: Word = word.child0.*;
        return delve(&new_word, bit_reader);
    } else {
        const new_word: Word = word.child1.*;
        return delve(&new_word, bit_reader);
    }
}

fn delveSource(word: *const SourceWord, value_raw: u1, bit_reader: anytype) !u8 {
    if (value_raw == 0) {
        const new_word: Word = word.child0.*;
        return delve(&new_word, bit_reader);
    } else {
        const new_word: Word = word.child1.*;
        return delve(&new_word, bit_reader);
    }
}

fn delve(word: *const Word, bit_reader: anytype) std.Io.Reader.Error!u8 {
    const actualWord = word.*;
    switch (actualWord) {
        .pure =>  |w| return delvePure(&w,),
        .parent => {},
        .source => {},
    }
    const value_raw = bit_reader.*.readBits(u1, 1) catch |err| {
        if (err == error.EndOfStream) {
            switch (actualWord) {
                .pure =>  unreachable,
                .parent => return 1,
                .source => return 1,
            }
        }
        return err;
    };
    switch (actualWord) {
        .pure =>  |w| return delvePure(&w),
        .parent => |w| return delveParent(&w, value_raw, bit_reader),
        .source => |w| return delveSource(&w, value_raw, bit_reader),
    }
}

fn readData(bit_reader:anytype, source:*const SourceWord, fileLength:u64, w:anytype) !void {
    var read_chars: u64 = 0;
    var current_node: Word = .{.source = source.*};
    while (read_chars < fileLength) {
        const char = try delve(&current_node, bit_reader);
        try w.writeByte(char);
        read_chars += 1;
    }
    return;
}

pub fn huffmanDecode(init: std.process.Init, input:[]const u8, output:[]const u8) !void {
    var arena = std.heap.ArenaAllocator.init(init.gpa);
    defer arena.deinit();
    const alloc = arena.allocator();
    const io = init.io;

    const file = try std.Io.Dir.cwd().openFile(io, input, .{.mode = .read_only });
    defer file.close(io);

    var buffer:[1024]u8 = undefined;
    var reader: std.Io.File.Reader = file.reader(io, &buffer);
    const r = &reader.interface;

    var bit_reader = try BitReader(.little).init(r);

    // var chars = std.mem.zeroes([256]u8);
    var counts = std.mem.zeroes([256]u64);
    const fileLength = try readHeader(&bit_reader, &counts);
    
    var counts_sum: f64 = 0.0;
    var i: usize = 0;
    while (i < 256) : (i += 1) {
        counts_sum += @as(f64, @floatFromInt( counts[i]));
    }
    var probs = std.mem.zeroes([256]f64);
    i = 0;
    while (i < 256) : (i += 1) {
        const prob = @as(f64, @floatFromInt( counts[i])) / counts_sum;
        probs[i] = prob;
    }

    const source = try constructHuffman(probs, alloc);
    // var words: ArrayList(u8) = .empty;ArrayList(u8) = .empty;
    var words = std.mem.zeroes([256]bool);
    var codes = std.mem.zeroes([256][32]bool);
    var code_length = std.mem.zeroes([256]u32);
    var running_code = std.mem.zeroes([32]bool);
    // var codes: ArrayList(ArrayList(bool)) = .empty;
    try recurseSource(source, &words, &codes, &code_length, &running_code, 0);
    
    const out_file = try std.Io.Dir.cwd().createFile(io, output, .{});
    defer out_file.close(io);

    var writebuffer: [1024]u8 = undefined;
    var writer = out_file.writer(io, &writebuffer);
    var w = &writer.interface;
    try readData(&bit_reader, source, fileLength, w);
    try w.flush();
}

pub fn huffmanEncode(init: std.process.Init, input:[]const u8, output:[]const u8) !void {
    var arena = std.heap.ArenaAllocator.init(init.gpa);
    defer arena.deinit();
    const alloc = arena.allocator();
    const io = init.io;
    const file = try std.Io.Dir.cwd().openFile(io, input, .{.mode = .read_only });
    defer file.close(io);

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    
    var counts = std.mem.zeroes([256]u64);
    // var counts_test: ArrayList(u8 = undefined;
    var nchars: usize = 0;

    // File reading stuff
    var fileLength: u64 = 0;
    var buffer: [8192]u8 = undefined;
    var reader: std.Io.File.Reader = file.reader(io, &buffer);
    const r = &reader.interface;
    // var buf_reader = std.Io.bufferedReader(file.reader());
    // var reader = buf_reader.reader();
    // This loop is reading "invisible" characters, take it out or not?
    while (true) {
        const char = r.takeByte() catch |err| switch (err) {
            error.EndOfStream => break,
            error.ReadFailed => |e| return e,
        };
        fileLength += 1;
        nchars += 1;
        counts[char] += 1;
    }
    if (nchars < 2) {
        return TooSmallError.TooSmall;
    }
    var counts_sum: f64 = 0.0;
    for (0..256) |i| {
        counts_sum += @as(f64, @floatFromInt( counts[i]));
    }
    var probs = std.mem.zeroes([256]f64);
    for (0..256) |i| {
        const prob = @as(f64, @floatFromInt(counts[i])) / counts_sum;
        probs[i] = prob;
    }
    const source = try constructHuffman(probs, alloc);
    var words = std.mem.zeroes([256]bool);
    var codes = std.mem.zeroes([256][32]bool);
    var code_length = std.mem.zeroes([256]u32);
    var running_code = std.mem.zeroes([32]bool);
    // var codes: ArrayList(ArrayList(bool)) = .empty;
    try recurseSource(source, &words, &codes, &code_length, &running_code, 0);
    const out_file = try std.Io.Dir.cwd().createFile(io, output, .{});
    defer out_file.close(io);

    var out_buf: [1024]u8 = undefined;
    var out_file_writer: std.Io.File.Writer = .init(out_file, io, &out_buf);
    const w = &out_file_writer.interface;
    defer w.flush() catch {};

    var bit_writer: BitWriter(.little) = .init(w);

    try writeHeader(counts, fileLength, &bit_writer);
    try reader.seekTo(0);
    try encode(r, codes, code_length, &bit_writer);
    try bit_writer.flushBits();
    try w.flush();}

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    var args_it = try std.process.Args.iterateAllocator(init.minimal.args, gpa);
    defer args_it.deinit();

    _ = args_it.next();
    const first_arg = args_it.next() orelse {
        std.debug.print("Give a command --test, --compress, --decompress", .{});
        return;
    };
    if (std.mem.eql(u8, first_arg, "--test")) {
        std.debug.print(" ENCODING \n", .{});
        try huffmanEncode(init, "foo.txt", "foo-comp.huff");
        std.debug.print(" DECODING \n", .{});
        try huffmanDecode(init, "foo-comp.huff", "foo-decomp.txt");
    } else if (std.mem.eql(u8, first_arg, "--compress")) {
        const input = args_it.next() orelse {
            std.debug.print("Not enough parameters for compression: ./main --compress <input> <output>", .{});
            return;
        };
        const output = args_it.next() orelse {
            std.debug.print("Not enough parameters for compression: ./main --compress <input> <output>", .{});
            return;
        };
        try huffmanEncode(init, input, output);
    } else if (std.mem.eql(u8, first_arg, "--decompress")) {
        const input = args_it.next() orelse {
            std.debug.print("Not enough parameters for decompression: ./main --decompress <input> <output>", .{});
            return;
        };
        const output = args_it.next() orelse {
            std.debug.print("Not enough parameters for decompression: ./main --decompress <input> <output>", .{});
            return;
        };
        try huffmanDecode(init, input, output);
    } else {
        std.debug.print("Unrecognized option: {s}", .{first_arg});
    }
}
